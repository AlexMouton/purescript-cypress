exports.should0Fn = function should0Fn(a, b) { return b.should(a); }
exports.should1Fn = function should1Fn(a, b, c) { return c.should(a, b); }
exports.should2Fn = function should2Fn(a, b, c, d) { return d.should(a, b, c); }
