const promisify  = require('cypress-promise');

// exports.andFn = function andFn(a, cy) {
//   return promisify(cy.and(a));
// }

// exports.asFn = function asFn(a, cy) {
//   return promisify(cy.as(a));
// }

// exports.backFn = function backFn(cy) {
//   return promisify(cy.back());
// }

exports.blurFn = function blurFn(isJust, fromJust, opt, el, cy) {
  const options = isJust(opt) ? fromJust(opt) : undefined;
  return promisify(cy.wrap(a, {log: false}).blur(options));
}

function checkPrime(val, opt, el, cy) {
  return promisify(cy.wrap(el, {log: false}).check(val, opt));
}

exports.checkValsFn = function checkVals(isJust, fromJust, val, opt, el, cy) {
  const val = isJust(val) ? fromJust(val) : undefined;
  const options = isJust(opt) ? fromJust(opt) : undefined;
  return checkPrime(val, opt, el, cy);
}

exports.checkFn = function checkFn(isJust, fromJust, opt, el, cy) {
  const options = isJust(opt) ? fromJust(opt) : undefined;
  return checkPrime(undefined, opt, el, cy);
}


// TODO: Selector, options
exports.childrenFn = function childrenFn(isJust, fromJust, str, opt, el, cy) {
  const options = isJust(opt) ? fromJust(opt) : undefined;
  const selector = isJust(str) ? fromJust(str) : undefined;

  return promisify(cy.wrap(b, {log: false}).children(selector, options));
}


// TODO:  options
exports.clearFn = function clearFn(a, cy) {
  return promisify(cy.wrap(a, {log: false}).clear());
}

// root
exports.clearCookieFn = function clearCookieFn(isJust, fromJust, s, opt, cy) {
  const options = isJust(opt) ? fromJust(opt) : undefined;
  return promisify(cy.clearCookie(s, options));
}

// root
// TODO: options
exports.clearCookiesFn = function clearCookiesFn(isJust, fromJust, opt, cy) {
  const options = isJust(opt) ? fromJust(opt) : undefined;
  return promisify(cy.clearCookies(options));
}

// root
// TODO: keys, options, clear all
exports.clearLocalStorageFn = function clearLocalStorageFn(isJust, fromJust, s, opt, cy) {
  const options = isJust(opt) ? fromJust(opt) : undefined;
  return promisify(cy.clearLocalStorage(s, options));
}


// TODO: position
exports.clickFn = function clickFn(isJust, fromJust, opt, el, cy) {
  const options = isJust(opt) ? fromJust(opt) : undefined;
  return promisify(cy.wrap(el, {log: false}).click(options));
}

exports.clickXyFn = function clickFn(isJust, fromJust, x, y, opt, el, cy) {
  const options = isJust(opt) ? fromJust(opt) : undefined;
  return promisify(cy.wrap(el, {log: false}).click(x, y, options));
}

exports.clickPosFn = function clickFn(isJust, fromJust, pos, opt, el, cy) {
  const options = isJust(opt) ? fromJust(opt) : undefined;
  return promisify(cy.wrap(el, {log: false}).click(pos, options));
}


// root
// TODO: now, options, fn names
exports.clockFn = function clockFn(cy) {
  return promisify(cy.clock());
}

// TODO: options
exports.closestFn = function closestFn(a, b, cy) {
  return promisify(cy.wrap(b, {log: false}).closest(a));
}

// both
const containsArgs = (isJust, fromJust, props) => {
  if(isJust(props.selector)) {
    const selector = fromJust(props.selector);
    const options = isJust(props.options) ? fromJust(props.options) : undefined;
    return [selector, props.content, options];
  }
  const options = isJust(props.options) ? fromJust(props.options) : undefined;
  return [props.content, options];
};

exports.containsFn = (isJust, fromJust, props, cy) => {
  return promisify(cy.contains(...containsArgs(isJust, fromJust, props)));
}

exports.containsqFn = (isJust, fromJust, props, q, cy) => {
  return promisify(q.contains(...containsArgs(isJust, fromJust, props)));
}

exports.dblclickFn = function dblclickFn(a, cy) {
  return promisify(cy.wrap(a, {log: false}).dblclick());
}

// both
exports.debugFn = function debugFn(a, cy) {
  return promisify(cy.debug(a));
}

// root
exports.documentFn = function documentFn(cy) {
  return promisify(cy.document());
}

// exports.eachFn = function eachFn(a, cy) {
//   return promisify(cy.each(a));
// }

// exports.endFn = function endFn(cy) {
//   return promisify(cy.end());
// }

// exports.eqFn = function eqFn(a, cy) {
//   return promisify(cy.eq(a));
// }

// root
exports.execFn = function execFn(a, cy) {
  return promisify(cy.exec(a));
}

exports.filterFn = function filterFn(a, b, cy) {
  return promisify(cy.wrap(b, {log: false}).filter(a));
}

exports.findFn = function findFn(a, cy) {
  return promisify(cy.find(a));
}

exports.firstFn = function firstFn(a, cy) {
  return promisify(cy.wrap(a, {log: false}).first());
}

// root
exports.fixtureFn = function fixtureFn(a, cy) {
  return promisify(cy.fixture(a));
}

exports.focusFn = function focusFn(a, cy) {
  return promisify(cy.focus(a));
}

// root
exports.focusedFn = function focusedFn(a, cy) {
  return promisify(cy.focused(a));
}

// root
exports.getFn = function getFn(isJust, fromJust, actionString, props, cy) {
  const action = actionString(props.action);
  const options = isJust(props.options) ? fromJust(props.options) : undefined;
  const res = cy.get(action, options)
  return promisify(res)
    .then( function(a) {
      console.log(a);
      return a;
    });
}

// root
exports.getCookieFn = function getCookieFn(a, cy) {
  return promisify(cy.getCookie(a));
}

// root
exports.getCookiesFn = function getCookiesFn(cy) {
  return promisify(cy.getCookies());
}

// root
exports.goFn = function goFn(a, cy) {
  return promisify(cy.go(a));
}

// root
exports.hashFn = function hashFn(cy) {
  return promisify(cy.hash());
}

// exports.hoverFn = function hoverFn(a, cy) {
//   return promisify(cy.hover(a));
// }

// exports.invokeFn = function invokeFn(a, cy) {
//   return promisify(cy.invoke(a));
// }

// exports.itsFn = function itsFn(a, cy) {
//   return promisify(cy.its(a));
// }

// TODO: options
exports.lastFn = function lastFn(a, b, cy) {
  return promisify(cy.wrap(b, {log: false}).last());
}

// root
exports.locationFn = function locationFn(a, cy) {
  return promisify(cy.location(a));
}

// root
exports.logFn = function logFn(a, cy) {
  return promisify(cy.log(a));
}

exports.nextFn = function nextFn(a, cy) {
  return promisify(cy.wrap(a, {log: false }).next());
}

exports.nextAllFn = function nextAllFn(a, cy) {
  return promisify(cy.wrap(a, {log: false}).nextAll());
}

exports.nextUntilFn = function nextUntilFn(a, b, cy) {
  return promisify(cy.wrap(b).nextUntil(a));
}

exports.notFn = function notFn(a, cy) {
  return promisify(cy.not(a));
}

exports.parentFn = function parentFn(a, cy) {
  return promisify(cy.parent(a));
}

exports.parentsFn = function parentsFn(a, cy) {
  return promisify(cy.parents(a));
}

exports.parentsUntilFn = function parentsUntilFn(a, cy) {
  return promisify(cy.parentsUntil(a));
}

// both
exports.pauseFn = function pauseFn(a, cy) {
  return promisify(cy.pause(a));
}

exports.prevFn = function prevFn(a, cy) {
  return promisify(cy.prev(a));
}

exports.prevAllFn = function prevAllFn(a, cy) {
  return promisify(cy.prevAll(a));
}

exports.prevUntilFn = function prevUntilFn(a, cy) {
  return promisify(cy.prevUntil(a));
}

// root
exports.readFileFn = function readFileFn(a, cy) {
  return promisify(cy.readFile(a));
}

// root
exports.reloadFn = function reloadFn(cy) {
  return promisify(cy.reload());
}

// exports.requestFn = function requestFn(a, cy) {
//   return promisify(cy.request(a));
// }

exports.rightclickFn = function rightclickFn(cy) {
  return promisify(cy.rightclick());
}

// exports.rootFn = function rootFn(cy) {
//   return promisify(cy.root());
// }

// exports.routeFn = function routeFn(a, cy) {
//   return promisify(cy.route(a));
// }

// both
exports.screenshotFn = function screenshotFn(cy) {
  return promisify(cy.screenshot());
}

exports.scrollIntoViewFn = function scrollIntoViewFn(a, cy) {
  return promisify(cy.scrollIntoView(a));
}

// exports.scrollToFn = function scrollToFn(a, cy) {
//   return promisify(cy.scrollTo(a));
// }

exports.selectFn = function selectFn(values, cy) {
  return promisify(cy.select(values)); }

// exports.serverFn = function serverFn(a, cy) {
//   return promisify(cy.server(a));
// }

// root
exports.setCookieFn = function setCookieFn(a, b, cy) {
  return promisify(cy.setCookie(a, b));
}

exports.siblingsFn = function siblingsFn(a, cy) {
  return promisify(cy.siblings(a));
}

// exports.spreadFn = function spreadFn(a, cy) {
//   return promisify(cy.spread(a));
// }

// exports.spyFn = function spyFn(a, cy) {
//   return promisify(cy.spy(a));
// }

// exports.stubFn = function stubFn(a, cy) {
//   return promisify(cy.stub(a));
// }

// form
exports.submitFn = function submitFn(a, cy) {
  return promisify(cy.submit(a));
}

// exports.taskFn = function taskFn(a, cy) {
//   return promisify(cy.task(a));
// }

// exports.thenFn = function thenFn(f, a) {
//   return promisify(a.then(f));
// }

// root
exports.tickFn = function tickFn(a, cy) {
  return promisify(cy.tick(a));
}

// root
exports.titleFn = function titleFn(cy) {
  return promisify(cy.title());
}

exports.triggerFn = function triggerFn(a, cy) {
  return promisify(cy.trigger(a));
}

exports.typeFn = function typeFn(a, b, cy) {
  return promisify(cy.wrap(b, {log: false}).type(a));
}

exports.uncheckFn = function uncheckFn(a, cy) {
  return promisify(cy.uncheck(a));
}

// root
exports.urlFn = function urlFn(cy) {
  return promisify(cy.url());
}

// root
exports.viewportFn = function viewportFn(a, b, cy) {
  return promisify(cy.viewport(a, b));
}

// root
exports.visitFn = function visitFn(url, cy) {
  return promisify(cy.visit(url))
    // .then( function(a) { debugger; return a; } )
}

// root
exports.waitFn = function waitFn(a, cy) {
  return promisify(cy.wait(a));
}

// root
exports.windowFn = function windowFn(cy) {
  return promisify(cy.window());
}

exports.withinFn = function withinFn(a, cy) {
  return promisify(cy.within(a));
}

exports.wrapFn = function wrapFn(a, cy) {
  return promisify(cy.wrap(a));
}
// exports.writeFileFn = function writeFileFn(a, cy) {
//   return promisify(cy.writeFile(a));
// }

exports.xpathFn = function xpathFn(isJust, fromJust, s, opts, cy) {
  const options = isJust(opts) ? fromJust(opts) : undefined;
  return promisify(cy.xpath(s, opts));
}

exports.attachFileFn = function attachFileFn(isJust, fromJust, a, b, opts) {
  const options = isJust(opts) ? fromJust(opts) : undefined;
  return promisify(b.attachFile(a, options));
}
