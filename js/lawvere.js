function clone(e) {
  if (null === e || "object" != typeof e || "isActiveClone" in e) return e;
  if (e instanceof Date) var n = new e.constructor();
  else n = e.constructor();
  for (var t in e)
    Object.prototype.hasOwnProperty.call(e, t) &&
      ((e.isActiveClone = null), (n[t] = clone(e[t])), delete e.isActiveClone);
  return n;
}

var tops = {};

let identity = (x) => x;

let mkConst = function (v) { return function (_) { return v; }; };

let comp = function (f1, f2) { return function (x) { return f2(f1(x)); }; };

let top = (i) => { return function (x) { return tops[i](x); }; };

let proj = (i) => function (x) { return x[i]; };

let inj = (i) => function (x) { return { tag: i, val: x }; };

let distr = (l) =>
  function (r) {
    let new_r = clone(r);
    new_r[l] = r[l].val;
    return { tag: r[l].tag, val: new_r };
  };

let cone = (c) =>
  function (x) {
    return Object.fromEntries(Object.entries(c).map(([k, f]) => [k, f(x)]));
  };

let cocone = (c) =>
  function (x) {
    return c[x.tag](x.val);
  };

let toLawBool = function (b){
    return { tag: b ? "true" : "false", val: {} }
};

let binOp = (op, f, g) => function(x) {
    switch(op) {
        case "plus": return f(x) + g(x);
        case "mult": return f(x) * g(x);
        case "minus": return f(x) - g(x);
        case "equal": return toLawBool(f(x) === g(x));
        case "less_than": return toLawBool(f(x) < g(x));
        case "less_than_equal": return toLawBool(f(x) <= g(x));
        default:
            console.log("Missing binOp!", op);
    }
  };

tops["plus"] = (x) => x["1"] + x["2"];
tops["print"] = (x) => {
  console.log("PRINT", x);
  return {};
};
tops["incr"] = (x) => x + 1;
tops["app"] = (x) => x["1"](x["2"]);
