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

// Category
let identity = (x) => x;
let comp = function (f1, f2) { return function (x) { return f2(f1(x)); }; };

// Sums
let inj = (i) => function (x) { return { tag: i, val: x }; };
let cocone = (c) =>
  function (x) {
    return c[x.tag](x.val);
  };

// Products
let proj = (i) => function (x) { return x[i]; };
let cone = (c) =>
  function (x) {
    return Object.fromEntries(Object.entries(c).map(([k, f]) => [k, f(x)]));
  };

// Distribution
let distr = (l) =>
  function (r) {
    let new_r = clone(r);
    new_r[l] = r[l].val;
    return { tag: r[l].tag, val: new_r };
  };

let mkConst = function (v) { return function (_) { return v; }; };

// Recursive definitions
let top = (i) => { return function (x) { return tops[i](x); }; };

// Primitive functions

let toLawBool = function (b){
    return { tag: b ? "true" : "false", val: {} }
};

let prim = function (p) {
  switch(p) {
    case "plus": return (x) => x["1"] + x["2"];
    case "mult": return (x) => x["1"] * x["2"];
    case "minus": return (x) => x["1"] - x["2"];
    case "equal": return (x) => toLawBool(x["1"] === x["2"]);
    case "less_than": return (x) => toLawBool(x["1"] < x["2"]);
    case "less_than_equal": return (x) => toLawBool(x["1"] <= x["2"]);
    case "greater_than": return (x) => toLawBool(x["1"] > x["2"]);
    case "greater_than_equal": return (x) => toLawBool(x["1"] >= x["2"]);
    case "app": return (x) => x["1"](x["2"]);
    case "identity": return (x) => x;
    case "incr": return (x) => x + 1;
    case "abs": return (x) => Math.abs(x);
    case "show": return (x) => "" + x;
    case "concat": return (x) => x["1"] + x["2"];
    default:
      console.log("Missing prim!", p);
  }
};


