settings.defaultSearchEngine = 'd';

api.unmap('ob');
api.unmap('sb');

// api.map('W', '<Alt-s>');

api.map('_', 't');
api.map('t', 'T');
api.map('T', '_');
api.unmap('_');

api.map('gT', 'E');
api.map('gt', 'R');
api.map('gp', 'E');
api.map('gn', 'R');
api.map('H', 'S');
api.map('L', 'D');
api.map('K', 'u');
api.map('J', 'd');
api.map('gd', 'x');
api.map('u', 'X');
api.map('P', 'cc');

api.unmap('d');

api.cmap('<Ctrl-j>', '<Tab>');
api.cmap('<Ctrl-k>', '<Shift-Tab>');

api.aceVimMap('jk', '<Esc>', 'insert');

settings.tabsThreshold = 0;
settings.tabsMRUOrder = false;

// set theme
settings.theme = `
.sk_theme {
    font-family: Input Sans Condensed, Charcoal, sans-serif;
    font-size: 10pt;
    background: #7F7F7F;
    color: #313131;
    font-weight: bold;
}
.sk_theme tbody {
    color: #fff;
}
.sk_theme input {
    color: #d0d0d0;
}
.sk_theme .url {
    color: #3E3E4C;
    font-weight: normal;
    font-style: italic;
}
.sk_theme .annotation {
    color: #7B7FCC;
}
.sk_theme .omnibar_highlight {
    color: #D0D0D0;
}
.sk_theme .omnibar_timestamp {
    color: #5A5A5A;
}
.sk_theme .omnibar_visitcount {
    color: #4A4A4A;
}
.sk_theme #sk_omnibarSearchResult ul li:nth-child(odd) {
    background: #797979;
}
.sk_theme #sk_omnibarSearchResult ul li.focused {
    background: #313131;
    color: #7F7F7F;
}
#sk_status, #sk_find {
    font-size: 20pt;
}`;
// click `Save` button to make above settings to take effect.</ctrl-i></ctrl-y>
