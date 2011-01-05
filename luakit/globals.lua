-- Global variables for luakit
globals = {
    homepage            = "https://bbs.archlinux.org/search.php?action=show_new",
    scroll_step         = 40,
    zoom_step           = 0.1,
    max_cmd_history     = 100,
    max_srch_history    = 100,
    http_proxy          = "127.0.0.1:8118",
    download_dir        = luakit.get_special_dir("DOWNLOAD") or (os.getenv("HOME") .. "/down"),
    default_window_size = "1280x800",
   -- default_window_size = "800x600",
}

-- Make useragent
local arch = string.match(({luakit.spawn_sync("uname -sm")})[2], "([^\n]*)")
local lkv  = string.format("luakit/%s", luakit.version)
local wkv  = string.format("WebKitGTK+/%d.%d.%d", luakit.webkit_major_version, luakit.webkit_minor_version, luakit.webkit_micro_version)
local awkv = string.format("AppleWebKit/%s.%s+", luakit.webkit_user_agent_major_version, luakit.webkit_user_agent_minor_version)
globals.useragent = string.format("Mozilla/5.0 (%s) %s %s %s", arch, awkv, wkv, lkv)

-- Search common locations for a ca file which is used for ssl connection validation.
local ca_files = {luakit.data_dir .. "/ca-certificates.crt",
    "/etc/certs/ca-certificates.crt", "/etc/ssl/certs/ca-certificates.crt",}
for _, ca_file in ipairs(ca_files) do
    if os.exists(ca_file) then
        globals.ca_file = ca_file
        break
    end
end

-- Change to stop navigation sites with invalid or expired ssl certificates
globals.ssl_strict = false

-- Search engines
search_engines = {
    lua   = "http://www.luakit.org/search/index/luakit?q={0}",
    bbs   = "https://bbs.archlinux.org/search.php?action=search&keywords={0}&author=&forum=-1&search_in=all&sort_by=0&sort_dir=DESC&show_as=topics",
    aur   = "http://aur.archlinux.org/packages.php?O=0&K={0}",
    abug  = "https://bugs.archlinux.org/index.php?string={0}&project=1&type%5B%5D=&sev%5B%5D=&pri%5B%5D=&due%5B%5D=&reported%5B%5D=&cat%5B%5D=&status%5B%5D=open&percent%5B%5D=&opened=&dev=&closed=&duedatefrom=&duedateto=&changedfrom=&changedto=&openedfrom=&openedto=&closedfrom=&closedto=&do=index",
    hub   = "https://github.com/search?q={0}&type=Everything&repo=&langOverride=&start_value=1",
    wiki  = "http://en.wikipedia.org/wiki/Special:Search?search={0}",
    g     = "http://www.google.com/search?q={0}",
    gs    = "https://www.google.com/search?q={0}",
    gi    = "http://www.google.com/images?q={0}&um=1&ie=UTF-8&source=og&sa=N&hl=en&tab=wi",
    gc    = "http://www.google.com/codesearch?as_q={0}&btnG=Search+Code&hl=en&as_package=&as_lang=&as_filename=&as_class=&as_function=&as_license=&as_case=",
    sf    = "http://sourceforge.net/search/?words={0}",
    slash = "http://slashdot.org/search.pl?query={0}",
    negg  = "http://www.newegg.com/Product/ProductList.aspx?Submit=ENE&DEPA=0&Order=BESTMATCH&Description={0}",
    tiger = "http://www.tigerdirect.com/applications/SearchTools/search.asp?keywords={0}",
    im    = "http://www.imdb.com/find?s=all&q={0}",
    yout  = "http://www.youtube.com/results?search_query={0}",
    dart  = "http://browse.deviantart.com/?qh=&section=&global=1&q={0}",
    last  = "http://www.last.fm/music/?q={0}",
    ama   = "http://www.amazon.com/s/ref=nb_ss_gw?url=search-alias%3Dall&field-keywords={0}",
    ebay  = "http://shop.ebay.com/?_from=R40&_trksid=p3907.m570.l1313&_nkw={0}&_sacat=See-All-Categories",
    kt    = "http://www.kickasstorrents.com/search/{0}/",
    bt    = "http://btjunkie.org/search?q={0}",
    pt    = "http://thepiratebay.org/search/{0}",
}

-- Set google-ssl (gs) as fallback search engine
search_engines.default = search_engines.gs

-- Fake the cookie policy enum here
cookie_policy = { always = 0, never = 1, no_third_party = 2 }

-- Per-domain webview properties
domain_props = {
    ["all"] = {
        ["enable-scripts"]          = true,
        ["enable-plugins"]          = false,
        ["enable-private-browsing"] = true,
        ["user-stylesheet-uri"]     = luakit.data_dir .. "/styles/mouse.css",
        ["accept-policy"]           = cookie_policy.no_third_party,
    },
    ["youtube.com"] = {
        ["enable-scripts"] = true,
        ["enable-plugins"] = true,
        ["enable-private-browsing"] = false,
        ["accept-policy"]  = cookie_policy.no_third_party,
    },
    ["kickasstorrents.com"] = {
        ["enable-scripts"] = true,
        ["enable-plugins"] = true,
        ["enable-private-browsing"] = true,
        ["accept-policy"]  = cookie_policy.no_third_party,
    },
    ["bbs.archlinux.org"] = {
        ["enable-private-browsing"] = false,
        ["accept-policy"]           = cookie_policy.no_third_party,
    },
    ["akiba-online.com"] = {
        ["enable-scripts"] = true,
        ["enable-plugins"] = true,
        ["enable-private-browsing"] = false,
        ["accept-policy"]  = cookie_policy.no_third_party,
    },
}

-- vim: et:sw=4:ts=8:sts=4:tw=80
