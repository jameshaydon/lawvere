(function () {
  function clone(e) {
    if (null === e || "object" != typeof e || "isActiveClone" in e) return e;
    if (e instanceof Date) var n = new e.constructor();
    else n = e.constructor();
    for (var t in e)
      Object.prototype.hasOwnProperty.call(e, t) &&
        ((e.isActiveClone = null),
        (n[t] = clone(e[t])),
        delete e.isActiveClone);
    return n;
  }
  var tops = {};
  let identity = (x) => x;
  let mkConst = function (v) {
    return function (_) {
      return v;
    };
  };
  let comp = function (f1, f2) {
    return function (x) {
      return f2(f1(x));
    };
  };
  let top = (i) => {
    return function (x) {
      return tops[i](x);
    };
  };
  let proj = (i) =>
    function (x) {
      return x[i];
    };
  let inj = (i) =>
    function (x) {
      return { tag: i, val: x };
    };
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
  tops["plus"] = (x) => x._1 + x._2;
  tops["print"] = (x) => {
    console.log("PRINT", x);
    return {};
  };
  tops["incr"] = (x) => x + 1;
  tops["app"] = (x) => x._1(x._2);
  tops["uniq"] = cone({});
  tops["map"] = comp(
    comp(comp(identity, cone({ f: proj("_1"), xs: proj("_2") })), distr("xs")),
    cocone({
      empty: comp(comp(identity, top("uniq")), inj("empty")),
      cons: comp(
        comp(
          identity,
          cone({
            head: comp(
              comp(
                identity,
                cone({
                  _1: proj("f"),
                  _2: comp(comp(identity, proj("xs")), proj("head")),
                })
              ),
              top("app")
            ),
            tail: comp(
              comp(
                identity,
                cone({
                  _1: proj("f"),
                  _2: comp(comp(identity, proj("xs")), proj("tail")),
                })
              ),
              top("map")
            ),
          })
        ),
        inj("cons")
      ),
    })
  );
  tops["length"] = cocone({
    empty: mkConst(0),
    cons: comp(
      comp(
        identity,
        cone({
          _1: mkConst(1),
          _2: comp(comp(identity, proj("tail")), top("length")),
        })
      ),
      top("plus")
    ),
  });
  tops["mapLength"] = comp(
    comp(identity, cone({ _1: mkConst(top("length")), _2: identity })),
    top("map")
  );
  tops["mapPrint"] = comp(
    comp(identity, cone({ _1: mkConst(top("print")), _2: identity })),
    top("map")
  );
  tops["cons1"] = comp(
    comp(identity, cone({ head: mkConst(1), tail: identity })),
    inj("cons")
  );
  tops["cons2"] = comp(
    comp(identity, cone({ head: mkConst(2), tail: identity })),
    inj("cons")
  );
  tops["list3"] = comp(
    comp(comp(comp(identity, inj("empty")), top("cons1")), top("cons2")),
    top("cons1")
  );
  tops["list2"] = comp(
    comp(comp(identity, inj("empty")), top("cons2")),
    top("cons1")
  );
  tops["listOfLists"] = comp(
    comp(
      comp(
        comp(
          comp(identity, inj("empty")),
          cone({ head: top("list3"), tail: identity })
        ),
        inj("cons")
      ),
      cone({ head: top("list2"), tail: identity })
    ),
    inj("cons")
  );
  tops["main"] = comp(comp(identity, top("listOfLists")), top("mapLength"));
  return tops;
})();
