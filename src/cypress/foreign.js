exports.andFn = function andFn(a, cy) { return cy.and(a);  }
exports.asFn = function asFn(a, cy) { return cy.as(a);  }
exports.blurFn = function blurFn(a, cy) { return cy.blur(a);  }
exports.checkFn = function checkFn(a, cy) { return cy.check(a);  }
exports.childrenFn = function childrenFn(a, cy) { return cy.children(a);  }
exports.clearFn = function clearFn(cy) { return cy.clear();  }
// root
exports.clearCookieFn = function clearCookieFn(a, cy) { return cy.clearCookie(a);  }
// root
exports.clearCookiesFn = function clearCookiesFn(a, cy) { return cy.clearCookies(a);  }
// root
exports.clearLocalStorageFn = function clearLocalStorageFn(a, cy) { return cy.clearLocalStorage(a);  }
exports.clickFn = function clickFn(cy) { return cy.click();  }
// root
exports.clockFn = function clockFn(cy) { return cy.clock();  }
exports.closestFn = function closestFn(a, cy) { return cy.closest(a);  }

// both
const containsArgs = (isJust, fromJust, props) => {
  if(isJust(props.selector)) {
    const selector = fromJust(props.selector);
    const options = isJust(props.options) ? fromJust(props.options) : undefined;
    return [selector, props.content, options];
  }
  else {
    const options = isJust(props.options) ? fromJust(props.options) : undefined;
    return [props.content, options];
  }
};

exports.containsFn = (isJust, fromJust, props, cy) => {
  return cy.contains(...containsArgs(isJust, fromJust, props))
}

exports.containsqFn = (isJust, fromJust, props, q, cy) => {
  return q.contains(...containsArgs(isJust, fromJust, props))
}

exports.dblclickFn = function dblclickFn(cy) { return cy.dblclick();  }
// both
exports.debugFn = function debugFn(a, cy) { return cy.debug(a);  }
// root
exports.documentFn = function documentFn(cy) { return cy.document();  }
// exports.eachFn = function eachFn(a, cy) { return cy.each(a);  }
exports.endFn = function endFn(cy) { return cy.end();  }
exports.eqFn = function eqFn(a, cy) { return cy.eq(a);  }
// root
exports.execFn = function execFn(a, cy) { return cy.exec(a);  }
exports.filterFn = function filterFn(a, cy) { return cy.filter(a);  }
exports.findFn = function findFn(a, cy) { return cy.find(a);  }
exports.firstFn = function firstFn(a) { return a.first();  }
// root
exports.fixtureFn = function fixtureFn(a, cy) { return cy.fixture(a);  }
exports.focusFn = function focusFn(a, cy) { return cy.focus(a);  }
// root
exports.focusedFn = function focusedFn(a, cy) { return cy.focused(a);  }
// root
exports.getFn = function getFn(isJust, fromJust, actionString, props, cy) {
  const action = actionString(props.action);
  const options = isJust(props.options) ? fromJust(props.options) : undefined;
  return cy.get(action, options);
}

// root
exports.getCookieFn = function getCookieFn(a, cy) { return cy.getCookie(a);  }
// root
exports.getCookiesFn = function getCookiesFn(cy) { return cy.getCookies();  }
// root
exports.goFn = function goFn(a, cy) { return cy.go(a);  }
// root
exports.hashFn = function hashFn(cy) { return cy.hash();  }
// exports.hoverFn = function hoverFn(a, cy) { return cy.hover(a);  }
// exports.invokeFn = function invokeFn(a, cy) { return cy.invoke(a);  }
// exports.itsFn = function itsFn(a, cy) { return cy.its(a);  }
exports.lastFn = function lastFn(a, cy) { return cy.last(a);  }
// root
exports.locationFn = function locationFn(a, cy) { return cy.location(a);  }
// root
exports.logFn = function logFn(a, cy) { return cy.log(a);  }
exports.nextFn = function nextFn(cy) { return cy.next();  }
exports.nextAllFn = function nextAllFn(a, cy) { return cy.nextAll(a);  }
exports.nextUntilFn = function nextUntilFn(a, cy) { return cy.nextUntil(a);  }
exports.notFn = function notFn(a, cy) { return cy.not(a);  }
exports.parentFn = function parentFn(a, cy) { return cy.parent(a);  }
exports.parentsFn = function parentsFn(a, cy) { return cy.parents(a);  }
exports.parentsUntilFn = function parentsUntilFn(a, cy) { return cy.parentsUntil(a);  }
// both
exports.pauseFn = function pauseFn(a, cy) { return cy.pause(a);  }
exports.prevFn = function prevFn(a, cy) { return cy.prev(a);  }
exports.prevAllFn = function prevAllFn(a, cy) { return cy.prevAll(a);  }
exports.prevUntilFn = function prevUntilFn(a, cy) { return cy.prevUntil(a);  }
// root
exports.readFileFn = function readFileFn(a, cy) { return cy.readFile(a);  }
// root
exports.reloadFn = function reloadFn(cy) { return cy.reload();  }
// exports.requestFn = function requestFn(a, cy) { return cy.request(a);  }
exports.rightclickFn = function rightclickFn(cy) { return cy.rightclick();  }
// exports.rootFn = function rootFn(cy) { return cy.root();  }
// exports.routeFn = function routeFn(a, cy) { return cy.route(a);  }
// both
exports.screenshotFn = function screenshotFn(cy) { return cy.screenshot();  }
exports.scrollIntoViewFn = function scrollIntoViewFn(a, cy) { return cy.scrollIntoView(a);  }
// exports.scrollToFn = function scrollToFn(a, cy) { return cy.scrollTo(a);  }
exports.selectFn = function selectFn(values, cy) { return cy.select(values); }
// exports.serverFn = function serverFn(a, cy) { return cy.server(a);  }
// root
exports.setCookieFn = function setCookieFn(a, b, cy) { return cy.setCookie(a, b);  }

exports.should0Fn = function should0Fn(a, b) { return b.should(a); }
exports.should1Fn = function should1Fn(a, b, c) { return c.should(a, b); }
exports.should2Fn = function should2Fn(a, b, c, d) { return d.should(a, b, c); }

exports.siblingsFn = function siblingsFn(a, cy) { return cy.siblings(a);  }
// exports.spreadFn = function spreadFn(a, cy) { return cy.spread(a);  }
// exports.spyFn = function spyFn(a, cy) { return cy.spy(a);  }
// exports.stubFn = function stubFn(a, cy) { return cy.stub(a);  }
// form
exports.submitFn = function submitFn(a, cy) { return cy.submit(a);  }
// exports.taskFn = function taskFn(a, cy) { return cy.task(a);  }
exports.thenFn = function thenFn(f, a) { return a.then(f);  }
// root
exports.tickFn = function tickFn(a, cy) { return cy.tick(a);  }
// root
exports.titleFn = function titleFn(cy) { return cy.title();  }
exports.triggerFn = function triggerFn(a, cy) { return cy.trigger(a);  }
exports.typeFn = function typeFn(a, cy) { return cy.type(a);  }
exports.uncheckFn = function uncheckFn(a, cy) { return cy.uncheck(a);  }
// root
exports.urlFn = function urlFn(cy) { return cy.url();  }
// root
exports.viewportFn = function viewportFn(a, b, cy) { return cy.viewport(a, b);  }
// root
exports.visitFn = function visitFn(url, cy) { return cy.visit(url); }
// root
exports.waitFn = function waitFn(a, cy) { return cy.wait(a);  }
// root
exports.windowFn = function windowFn(cy) { return cy.window();  }
exports.withinFn = function withinFn(a, cy) { return cy.within(a);  }
exports.wrapFn = function wrapFn(a, cy) { return cy.wrap(a);  }
// exports.writeFileFn = function writeFileFn(a, cy) { return cy.writeFile(a);  }

exports.xpathFn = function xpathFn(a, cy) { return cy.xpath(a); }
