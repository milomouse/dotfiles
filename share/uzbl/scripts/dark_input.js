(function() {
    window.addEventListener('load', function() {
        css = 'input {color: #9f9f9f !important; background-color: #232323 !important;} textarea {color: white !important; background-color: black !important;}';
        var heads = document.getElementsByTagName("head");
        if (heads.length > 0) {
            var node = document.createElement("style");
            node.type = "text/css";
            node.appendChild(document.createTextNode(css));
            heads[0].appendChild(node);
        }
    }, false);
})();
