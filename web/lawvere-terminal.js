import { Terminal } from 'https://cdn.jsdelivr.net/npm/xterm@5.3.0/+esm';
import { FitAddon } from 'https://cdn.jsdelivr.net/npm/xterm-addon-fit@0.8.0/+esm';
import { WASI, OpenFile, File, ConsoleStdout } from 'https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/+esm';
import { sampleFiles } from './samples.js';

/**
 * Lawvere terminal — xterm.js controller + WASM REPL bootstrap.
 * Parchment / iron-gall colour theme to match the surrounding page.
 */
class LawvereTerminal {
  constructor() {
    this.term = new Terminal({
      cursorBlink: true,
      fontSize: 14,
      fontFamily: '"JetBrains Mono", "Iosevka", Menlo, Monaco, "Courier New", monospace',
      theme: {
        background:     '#fffdf5',
        foreground:     '#2b2118',
        cursor:         '#8a1c1c',
        cursorAccent:   '#fffdf5',
        selectionBackground: 'rgba(138, 28, 28, 0.22)',
        black:          '#2b2118',
        red:            '#8a1c1c',
        green:          '#3a6b35',
        yellow:         '#a06a1a',
        blue:           '#1f4e79',
        magenta:        '#6b2a6e',
        cyan:           '#2d6e7e',
        white:          '#faf6ec',
        brightBlack:    '#4a3c2f',
        brightRed:      '#b22222',
        brightGreen:    '#5c8a4a',
        brightYellow:   '#c89033',
        brightBlue:     '#3a6ea5',
        brightMagenta:  '#8a4a8d',
        brightCyan:     '#4f8a99',
        brightWhite:    '#fffdf5'
      },
      allowProposedApi: true
    });

    this.fitAddon = new FitAddon();
    this.term.loadAddon(this.fitAddon);

    this.inputBuffer = '';
    this.resolveInput = null;
    this.isInitialized = false;
  }

  initialize() {
    if (this.isInitialized) return;

    const terminalElement = document.getElementById('terminal');
    if (!terminalElement) throw new Error('Terminal element not found');

    this.term.open(terminalElement);
    this.fitAddon.fit();

    window.addEventListener('resize', () => this.fitAddon.fit());

    this.term.onData(data => this.handleInput(data));
    this.isInitialized = true;
  }

  handleInput(data) {
    const code = data.charCodeAt(0);

    // Enter
    if (code === 13) {
      if (this.resolveInput) {
        const input = this.inputBuffer;
        this.inputBuffer = '';
        this.term.write('\r\n');
        const r = this.resolveInput;
        this.resolveInput = null;
        r(input);
      }
      return;
    }

    // Backspace / Ctrl+H
    if (code === 127 || code === 8) {
      if (this.inputBuffer.length > 0) {
        this.inputBuffer = this.inputBuffer.slice(0, -1);
        this.term.write('\b \b');
      }
      return;
    }

    // Ctrl+C — abort current input
    if (code === 3) {
      this.term.write('^C\r\n');
      if (this.resolveInput) {
        this.inputBuffer = '';
        const r = this.resolveInput;
        this.resolveInput = null;
        r('');
      }
      return;
    }

    // Ctrl+D — EOF (treated as :q)
    if (code === 4) {
      this.term.write('^D\r\n');
      if (this.resolveInput && this.inputBuffer.length === 0) {
        const r = this.resolveInput;
        this.resolveInput = null;
        r(':q');
      }
      return;
    }

    // Tab — insert two spaces (no completion yet)
    if (code === 9) {
      this.inputBuffer += '  ';
      this.term.write('  ');
      return;
    }

    // Printable
    if (code >= 32 && code < 127) {
      this.inputBuffer += data;
      this.term.write(data);
      return;
    }
  }

  printLine(text) {
    const lines = text.split('\n');
    for (let i = 0; i < lines.length; i++) {
      if (i > 0) this.term.write('\r\n');
      this.term.write(lines[i]);
    }
    this.term.write('\r\n');
  }

  async readLine(prompt) {
    return new Promise(resolve => {
      this.term.write(prompt);
      this.resolveInput = resolve;
    });
  }
}

const lawvereTerm = new LawvereTerminal();

// WASM-side FFI exports
window.terminal_initialize = () => lawvereTerm.initialize();
window.terminal_printLine   = (jsVal) => lawvereTerm.printLine(String(jsVal));
window.terminal_readLine    = (jsVal) => lawvereTerm.readLine(String(jsVal));

window.terminal_loadFile = async (jsVal) => {
  const filename = String(jsVal).trim();

  if (filename) {
    if (Object.prototype.hasOwnProperty.call(sampleFiles, filename)) {
      return sampleFiles[filename];
    }
    const available = Object.keys(sampleFiles).sort().join(', ');
    throw new Error(
      `File not found: ${filename}\n` +
      `Available bundled files: ${available}\n` +
      `Use ':l' with no arguments to pick a local file.`
    );
  }

  // No filename: file picker
  return new Promise((resolve, reject) => {
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = '.law,.md,.txt';
    input.style.display = 'none';
    document.body.appendChild(input);
    input.addEventListener('change', async () => {
      document.body.removeChild(input);
      const file = input.files[0];
      if (file) resolve(await file.text());
      else reject(new Error('No file selected'));
    });
    input.addEventListener('cancel', () => {
      document.body.removeChild(input);
      reject(new Error('File selection cancelled'));
    });
    input.click();
  });
};

function showError(message, details) {
  const loadingEl = document.getElementById('loading');
  loadingEl.className = 'error';
  loadingEl.innerHTML =
    `<div class="error-title">Failed to load the Lawvere interpreter</div>` +
    `<div>${message}</div>` +
    (details ? `<pre style="margin-top: 0.8rem; text-align: left; white-space: pre-wrap;">${details}</pre>` : '');
}

async function initLawvere() {
  const loadingEl = document.getElementById('loading');
  const terminalContainer = document.getElementById('terminal-container');

  try {
    loadingEl.innerHTML = `<span class="quill"></span>Fetching WebAssembly module&hellip;`;
    const response = await fetch('lawvere.wasm');
    if (!response.ok) {
      throw new Error(`Failed to fetch lawvere.wasm: ${response.status} ${response.statusText}`);
    }
    const wasmBytes = await response.arrayBuffer();

    loadingEl.innerHTML = `<span class="quill"></span>Initialising the interpreter&hellip;`;

    // post-link.mjs-generated JSFFI glue
    const jsffiModule = await import('./lawvere.js');

    // knot-tying: exports are filled after instantiation
    const __exports = {};
    const jsffi = jsffiModule.default(__exports);

    const fds = [
      new OpenFile(new File([])),                                            // stdin
      ConsoleStdout.lineBuffered(msg => console.log(`[lawvere] ${msg}`)),    // stdout
      ConsoleStdout.lineBuffered(msg => console.warn(`[lawvere] ${msg}`)),   // stderr
    ];
    const wasi = new WASI([], [], fds);

    const { instance } = await WebAssembly.instantiate(wasmBytes, {
      ghc_wasm_jsffi: jsffi,
      wasi_snapshot_preview1: wasi.wasiImport,
    });
    Object.assign(__exports, instance.exports);

    wasi.initialize(instance);

    loadingEl.classList.add('hidden');
    terminalContainer.style.display = 'block';

    instance.exports.hs_init();
    instance.exports.hs_start();
  } catch (error) {
    console.error('Lawvere initialisation error:', error);
    showError(error.message || 'An unknown error occurred', error.stack);
  }
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initLawvere);
} else {
  initLawvere();
}
