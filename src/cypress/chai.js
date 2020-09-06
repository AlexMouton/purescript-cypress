const promisify  = require('cypress-promise')
exports.should0Fn = function should0Fn(a, b, cy) { console.log('Should0', a, b, cy); return promisify(cy.wrap(b).should(a)); }
exports.should1Fn = function should1Fn(a, b, c, cy) { console.log('Should1', a, b, c, cy); return promisify(cy.wrap(c).should(a, b)); }
exports.should2Fn = function should2Fn(a, b, c, d, cy) { console.log('Should2', a, b, c, d, cy); return promisify(cy.wrap(d).should(a, b, c)); }
