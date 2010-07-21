/*
    (c) 2009 by Leon Winter
    (c) 2009 by Hannes Schueller

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE.
*/

function clearfocus() {
    if(document.activeElement && document.activeElement.blur)
        document.activeElement.blur();
}

function v(e, y) {
    t = e.nodeName.toLowerCase();
    if((t == 'input' && /^(text|password|checkbox|radio)$/.test(e.type))
    || /^(select|textarea)$/.test(t)
    || e.contentEditable == 'true')
        console.log('insertmode_'+(y=='focus'?'on':'off'));
}

if(document.activeElement)
    v(document.activeElement,'focus');

m=['focus','blur'];

if (document.getElementsByTagName("body")[0] !== null && typeof(document.getElementsByTagName("body")[0]) == "object") {
    for(i in m)
        document.getElementsByTagName('body')[0].addEventListener(m[i], function(x) {
            v(x.target,x.type);
        }, true);

    document.getElementsByTagName("body")[0].appendChild(document.createElement("style"));
    document.styleSheets[0].addRule('.hinting_mode_hint', 'color: #000; background: #ff0;');
    document.styleSheets[0].addRule('.hinting_mode_hint_focus', 'color: #000; background: #8f0;');
}

self.onunload = function() {
    v(document.activeElement, '');
};

function show_hints(inputText) {
    if (document.getElementsByTagName("body")[0] !== null && typeof(document.getElementsByTagName("body")[0]) == "object") {
        var height = window.innerHeight;
        var width = window.innerWidth;
        var scrollX = document.defaultView.scrollX;
        var scrollY = document.defaultView.scrollY;
        /* prefixing html: will result in namespace error */
        var hinttags;
        if (typeof(inputText) == "undefined" || inputText == "") {
            hinttags = "//*[@onclick or @onmouseover or @onmousedown or @onmouseup or @oncommand or @class='lk' or @role='link' or @href] | //input[not(@type='hidden')] | //a | //area | //iframe | //textarea | //button | //select";
        } else {
            /* only elements which match the text entered so far */
            hinttags = "//*[(@onclick or @onmouseover or @onmousedown or @onmouseup or @oncommand or @class='lk' or @role='link' or @href) and contains(., '" + inputText + "')] | //input[not(@type='hidden') and contains(., '" + inputText + "')] | //a[contains(., '" + inputText + "')] | //area[contains(., '" + inputText + "')] | //iframe[contains(@name, '" + inputText + "')] | //textarea[contains(., '" + inputText + "')] | //button[contains(@value, '" + inputText + "')] | //select[contains(., '" + inputText + "')]";
        }

        /* iterator type isn't suitable here, because: "DOMException NVALID_STATE_ERR: The document has been mutated since the result was returned." */
        var r = document.evaluate(hinttags, document,
            function(p) {
                return 'http://www.w3.org/1999/xhtml';
            }, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
        div = document.createElement("div");
        /* due to the different XPath result type, we will need two counter variables */
        j = 1;
        var i;
        a = [];
        colors = [];
        for (i = 0; i < r.snapshotLength; i++)
        {
            var elem = r.snapshotItem(i);
            rect = elem.getBoundingClientRect();
            if (!rect || rect.top > height || rect.bottom < 0 || rect.left > width || rect.right < 0 || !(elem.getClientRects()[0]))
                continue;
            var computedStyle = document.defaultView.getComputedStyle(elem, null);
            if (computedStyle.getPropertyValue("visibility") != "visible" || computedStyle.getPropertyValue("display") == "none")
                continue;
            var leftpos = Math.max((rect.left + scrollX), scrollX);
            var toppos = Math.max((rect.top + scrollY), scrollY);
            a.push(elem);
            /* making this block DOM compliant */
            var hint = document.createElement("span");
            hint.setAttribute("class", "hinting_mode_hint");
            hint.setAttribute("id", "vimprobablehint" + j);
            hint.style.position = "absolute";
            hint.style.left = leftpos + "px";
            hint.style.top =  toppos + "px";
            hint.style.background = "red";
            hint.style.color = "#fff";
            hint.style.font = "bold 10px monospace";
            hint.style.zIndex = "99";
            var text = document.createTextNode(j);
            hint.appendChild(text);
            div.appendChild(hint);
            /* remember site-defined colour of this element */
            colors[j] = elem.style.color;
            /* make the link black to ensure it's readable */
            elem.style.color = "#000";
            j++;
        }
        i = 0;
        while (typeof(a[i]) != "undefined") {
            a[i].className += " hinting_mode_hint";
            i++;
        }
        document.getElementsByTagName("body")[0].appendChild(div);
        clearfocus();
        h = null;
        if (i == 1) {
            /* just one hinted element - might as well follow it */
            return fire(1);
        }
    }
}
function fire(n)
{
    if (typeof(a[n - 1]) != "undefined") {
        el = a[n - 1];
        tag = el.nodeName.toLowerCase();
        clear();
        if(tag == "iframe" || tag == "frame" || tag == "textarea" || tag == "input" && (el.type == "text" || el.type == "password" || el.type == "checkbox" || el.type == "radio")) {
            el.focus();
            if (tag == "textarea" || tag == "input")
                console.log('insertmode_on');
        } else {
            if (el.onclick) {
                var evObj = document.createEvent('MouseEvents');
                evObj.initMouseEvent('click', true, true, window, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, null);
                el.dispatchEvent(evObj);
            } else if (el.href) {
                if (el.href.match(/^javascript:/)) {
                    var evObj = document.createEvent('MouseEvents');
                    evObj.initMouseEvent('click', true, true, window, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, null);
                    el.dispatchEvent(evObj);
                } else {
                    /* send signal to open link */
                    return "open;" + el.href;
                }
            } else {
                var evObj = document.createEvent('MouseEvents');
                evObj.initMouseEvent('click', true, true, window, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, null);
                el.dispatchEvent(evObj);
            }
        }
    }
}
function cleanup()
{
    for(e in a) {
        if (typeof(a[e].className) != "undefined") {
            a[e].className = a[e].className.replace(/hinting_mode_hint/,'');
            /* reset to site-defined colour */
            a[e].style.color = colors[e];
        }
    }
    div.parentNode.removeChild(div);
    window.onkeyup = null;
}
function clear()
{
    cleanup();
    console.log("hintmode_off")
}

function update_hints(n) 
{
    if(h != null)
        h.className = h.className.replace("_focus","");
    if (j - 1 < n * 10 && typeof(a[n - 1]) != "undefined") {
        /* return signal to follow the link */
        return "fire;" + n;
    } else
        if (typeof(a[n - 1]) != "undefined")
            (h = a[n - 1]).className = a[n - 1].className.replace("hinting_mode_hint", "hinting_mode_hint_focus");
}

function focus_input()
{
    if (document.getElementsByTagName("body")[0] !== null && typeof(document.getElementsByTagName("body")[0]) == "object") {
        /* prefixing html: will result in namespace error */
        var hinttags = "//input[@type='text'] | //input[@type='password'] | //textarea";
        var r = document.evaluate(hinttags, document,
            function(p) {
                return 'http://www.w3.org/1999/xhtml';
            }, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
        var i;
        var j = 0;
        var first = null;
        for (i = 0; i < r.snapshotLength; i++) {
            var elem = r.snapshotItem(i);
            if (i == 0) {
                first = elem;
            }
            if (j == 1) {
                elem.focus();
                var tag = elem.nodeName.toLowerCase();
                if (tag == "textarea" || tag == "input")
                    console.log('insertmode_on');
                break;
            } else {
                if (elem == document.activeElement)
                    j = 1;
            }
        }
        if (j == 0) {
            /* no appropriate field found focused - focus the first one */
            if (first !== null) {
            	first.focus();
                var tag = elem.nodeName.toLowerCase();
                if (tag == "textarea" || tag == "input")
                    console.log('insertmode_on');
            }
        }
    }
}
