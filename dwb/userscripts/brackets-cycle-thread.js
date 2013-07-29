//!javascript


var nextPatterns = "next,more,>,\u2192,\xbb,\u226b,>>";
var previousPatterns = "prev,previous,back,<,\u2190,\xab,\u226a,<<";

var domUtils = function()
{
    var DomUtils = {
        makeXPath: function(elementArray) {
            var i, xpath;
            xpath = [];
            for (i in elementArray) {
                xpath.push("//" + elementArray[i], "//xhtml:" + elementArray[i]);
            }
            return xpath.join(" | ");
        },
        evaluateXPath : function(xpath, resultType) {
            var namespaceResolver;
            namespaceResolver = function(namespace) {
                if (namespace === "xhtml") {
                    return "http://www.w3.org/1999/xhtml";
                } else {
                    return null;
                }
            };
            return document.evaluate(xpath, document.documentElement, namespaceResolver, resultType, null);
        },
        simulateClick : function(element, modifiers) {
            var event, eventSequence, mouseEvent, _i, _len, _results;
            modifiers || (modifiers = {});
            eventSequence = ["mouseover", "mousedown", "mouseup", "click"];
            _results = [];
            for (_i = 0, _len = eventSequence.length; _i < _len; _i++) {
                event = eventSequence[_i];
                mouseEvent = document.createEvent("MouseEvents");
                mouseEvent.initMouseEvent(event, true, true, window, 1, 0, 0, 0, 0, 
                        modifiers.ctrlKey, false, false, modifiers.metaKey, 0, null);
                _results.push(element.dispatchEvent(mouseEvent));
            }
            return _results;
        },
        followLink : function(linkElement) 
        {
            if (linkElement.nodeName.toLowerCase() === "link") 
            {
                return window.location.href = linkElement.href;
            } 
            else 
            {
                linkElement.scrollIntoView();
                linkElement.focus();
                return this.simulateClick(linkElement);
            }
        }
    };
};
var findAndFollowRelInjectable = function() {
    var value = arguments[0];

    var element, elements, relTags, tag, _i, _j, _len, _len1;
    relTags = ["link", "a", "area"];
    for (_i = 0, _len = relTags.length; _i < _len; _i++) {
      tag = relTags[_i];
      elements = document.getElementsByTagName(tag);
      for (_j = 0, _len1 = elements.length; _j < _len1; _j++) {
        element = elements[_j];
        if (element.hasAttribute("rel") && element.rel === value) {
          DomUtils.followLink(element);
          return true;
        }
      }
    }
};
function findAndFollowLinkInjectable()
{
    var linkStrings = arguments[0];
    var boundingClientRect, candidateLink, candidateLinks, computedStyle, exactWordRegex, i, link, linkMatches, linkString, links, linksXPath, _i, _j, _k, _l, _len, _len1, _len2, _len3, _m, _ref;
    linksXPath = DomUtils.makeXPath(["a", "*[@onclick or @role='link' or contains(@class, 'button')]"]);
    links = DomUtils.evaluateXPath(linksXPath, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE);
    candidateLinks = [];
    for (i = _i = _ref = links.snapshotLength - 1; _i >= 0; i = _i += -1) 
    {
        link = links.snapshotItem(i);
        boundingClientRect = link.getBoundingClientRect();
        if (boundingClientRect.width === 0 || boundingClientRect.height === 0) 
        {
            continue;
        }
        computedStyle = window.getComputedStyle(link, null);
        if (computedStyle.getPropertyValue("visibility") !== "visible" || computedStyle.getPropertyValue("display") === "none") 
        {
            continue;
        }
        linkMatches = false;
        for (_j = 0, _len = linkStrings.length; _j < _len; _j++) 
        {
            linkString = linkStrings[_j];
            if (link.innerText.toLowerCase().indexOf(linkString) !== -1) {
                linkMatches = true;
                break;
            }
        }
        if (!linkMatches) 
        {
            continue;
        }
        candidateLinks.push(link);
    }
    if (candidateLinks.length === 0) 
    {
        return;
    }

    for (_k = 0, _len1 = candidateLinks.length; _k < _len1; _k++) {
        link = candidateLinks[_k];
        link.wordCount = link.innerText.trim().split(/\s+/).length;
    }
    candidateLinks.forEach(function(a, i) {
        return a.originalIndex = i;
    });
    candidateLinks = candidateLinks.sort(function(a, b) {
        if (a.wordCount === b.wordCount) {
            return a.originalIndex - b.originalIndex;
        } 
        else {
            return a.wordCount - b.wordCount;
        }
    }).filter(function(a) {
        return a.wordCount <= candidateLinks[0].wordCount + 1;
    });
    for (_l = 0, _len2 = linkStrings.length; _l < _len2; _l++) {
        linkString = linkStrings[_l];
        exactWordRegex = /\b/.test(linkString[0]) || /\b/.test(linkString[linkString.length - 1]) ? new RegExp("\\b" + linkString + "\\b", "i") : new RegExp(linkString, "i");
        for (_m = 0, _len3 = candidateLinks.length; _m < _len3; _m++) {
            candidateLink = candidateLinks[_m];
            if (exactWordRegex.test(candidateLink.innerText)) {
                DomUtils.followLink(candidateLink);
                return true;
            }
        }
    }
    return false;
}
function constructInjectable(inject)
{
    var script = util.getBody(domUtils) + " " + util.getBody(inject);
    return script;
}
function findAndFollowRel(dir)
{
    return JSON.parse(tabs.current.inject(constructInjectable(findAndFollowRelInjectable), dir, 1));
}
function findAndFollowLink(string)
{
    return JSON.parse(tabs.current.inject(constructInjectable(findAndFollowLinkInjectable), string, 1));
}

function back()
{
    var previousStrings;
    previousStrings = previousPatterns.split(",").filter(function(s) {
        return s.length;
    });
    return findAndFollowRel("prev") || findAndFollowLink(previousStrings);
}
function forward()
{
    var previousStrings;
    previousStrings = nextPatterns.split(",").filter(function(s) {
        return s.length;
    });
    return findAndFollowRel("next") || findAndFollowLink(previousStrings);
}

bind("]]", forward);
bind("[[", back);

