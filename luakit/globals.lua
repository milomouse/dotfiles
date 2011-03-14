-- Global variables for luakit
globals = {
    homepage            = "https://bbs.archlinux.org/search.php?action=show_new",
    scroll_step         = 40,
    zoom_step           = 0.1,
    max_cmd_history     = 100,
    max_srch_history    = 100,
    http_proxy          = "127.0.0.1:8118",
    default_window_size = "1280x800",

 -- Disables loading of hostnames from /etc/hosts (for large host files)
    load_etc_hosts      = false,
 -- Disables checking if a filepath exists in search_open function
 -- check_filepath      = false,
}

-- Make useragent
local arch = string.match(({luakit.spawn_sync("uname -sm")})[2], "([^\n]*)")
local lkv  = string.format("luakit/%s", luakit.version)
local wkv  = string.format("WebKitGTK+/%d.%d.%d", luakit.webkit_major_version, luakit.webkit_minor_version, luakit.webkit_micro_version)
local awkv = string.format("AppleWebKit/%s.%s+", luakit.webkit_user_agent_major_version, luakit.webkit_user_agent_minor_version)
globals.useragent = string.format("Mozilla/5.0 (%s) %s %s %s", arch, awkv, wkv, lkv)

-- Search common locations for a ca file which is used for ssl connection validation.
local ca_files = {
    -- $XDG_DATA_HOME/luakit/ca-certificates.crt
    luakit.data_dir .. "/ca-certificates.crt",
    "/etc/certs/ca-certificates.crt",
    "/etc/ssl/certs/ca-certificates.crt",
}
-- Use the first ca-file found
for _, ca_file in ipairs(ca_files) do
    if os.exists(ca_file) then
        soup.set_property("ssl-ca-file", ca_file)
        break
    end
end

-- Change to stop navigation sites with invalid or expired ssl certificates
soup.set_property("ssl-strict", false)

-- Set cookie acceptance policy
cookie_policy = { always = 0, never = 1, no_third_party = 2 }
soup.set_property("accept-policy", cookie_policy.no_third_party)

-- List of search engines. Each item must contain a single %s which is
-- replaced by URI encoded search terms. All other occurances of the percent
-- character (%) may need to be escaped by placing another % before or after
-- it to avoid collisions with lua's string.format characters.
-- See: http://www.lua.org/manual/5.1/manual.html#pdf-string.format
search_engines = {
    luakit   = "http://luakit.org/search/index/luakit?q=%s",
    aur      = "http://aur.archlinux.org/packages.php?O=0&K=%s",
    abbs     = "https://bbs.archlinux.org/search.php?action=search&keywords=%s&author=&forum=-1&search_in=all&sort_by=0&sort_dir=DESC&show_as=topics",
    apkg     = "http://www.archlinux.org/packages?sort=&arch=x86_64&repo=&q=%s&maintainer=&last_update=&flagged=&limit=50",
    awiki    = "https://wiki.archlinux.org/index.php?title=Special%%3ASearch&search=%s&go=Go",
    abugs    = "https://bugs.archlinux.org/index.php?string=%s&project=1&type%%5B%%5D=&sev%%5B%%5D=&pri%%5B%%5D=&due%%5B%%5D=&reported%%5B%%5D=&cat%%5B%%5D=&status%%5B%%5D=open&percent%%5B%%5D=&opened=&dev=&closed=&duedatefrom=&duedateto=&changedfrom=&changedto=&openedfrom=&openedto=&closedfrom=&closedto=&do=index",
    github   = "https://github.com/search?q=%s&type=Everything&repo=&langOverride=&start_value=1",
    wiki     = "https://en.wikipedia.org/wiki/Special:Search?search=%s",
    fact     = "http://www.factbites.com/topics/%s",
    duck     = "https://duckduckgo.com/?q=%s",
    google   = "https://www.google.com/search?q=%s",
    newegg   = "http://www.newegg.com/Product/ProductList.aspx?Submit=ENE&DEPA=0&Order=BESTMATCH&Description=%s",
    tiger    = "http://www.tigerdirect.com/applications/SearchTools/search.asp?keywords=%s",
    imdb     = "http://www.imdb.com/find?s=all&q=%s",
    youtube  = "http://www.youtube.com/results?search_query=%s",
    lastfm   = "http://www.last.fm/music/?q=%s",
    postrock = "http://www.postrockxchange.com/?s=%s",
    amazon   = "http://www.amazon.com/s/ref=nb_ss_gw?url=search-alias%%3Dall&field-keywords=%s",
    ebay     = "http://shop.ebay.com/?_from=R40&_trksid=p3907.m570.l1313&_nkw=%s&_sacat=See-All-Categories",
    kt       = "http://www.kickasstorrents.com/search/%s/",
    bt       = "http://btjunkie.org/search?q=%s",
    pt       = "http://thepiratebay.org/search/%s",
}

-- Set google as fallback search engine
search_engines.default = search_engines.ddg
-- Use this instead to disable auto-searching
--search_engines.default = "%s"

-- Per-domain webview properties
-- See http://webkitgtk.org/reference/webkitgtk-WebKitWebSettings.html
domain_props = {
    ["all"] = {
        ["enable-scripts"] = true,
        ["enable-plugins"] = false,
        ["enable-private-browsing"] = true,
       -- ["user-stylesheet-uri"] = "file://" .. luakit.data_dir .. "/styles/mouse.css",
    },
    ["cybernations.net"] = {
        ["enable-plugins"] = true,
        ["enable-private-browsing"] = false,
    },
    ["nationstates.net"] = {
        ["enable-plugins"] = true,
        ["enable-private-browsing"] = false,
    },
    ["ars-regendi.com"] = {
        ["enable-plugins"] = true,
        ["enable-private-browsing"] = false,
    },
    ["bbs.archlinux.org"] = {
        ["enable-private-browsing"] = false,
    },
    ["bugs.archlinux.org"] = {
        ["enable-private-browsing"] = false,
    },
    ["aur.archlinux.org"] = {
        ["enable-private-browsing"] = false,
    },
    ["github.com"] = {
        ["enable-plugins"] = true,
        ["enable-private-browsing"] = false,
    },
    ["en.wikipedia.org"] = {
        ["enable-private-browsing"] = false,
    },
    ["amazon.com"] = {
        ["enable-plugins"] = true,
        ["enable-private-browsing"] = false,
    },
    ["newegg.com"] = {
        ["enable-private-browsing"] = false,
    },
    ["tigerdirect.com"] = {
        ["enable-private-browsing"] = false,
    },
    ["ebay.com"] = {
        ["enable-plugins"] = true,
        ["enable-private-browsing"] = false,
    },
    ["shop.ebay.com"] = {
        ["enable-plugins"] = true,
        ["enable-private-browsing"] = false,
    },
    ["my.ebay.com"] = {
        ["enable-plugins"] = true,
        ["enable-private-browsing"] = false,
    },
    ["google.com"] = {
        ["enable-scripts"] = false,
    },
    ["youtube.com"] = {
        ["enable-plugins"] = true,
    },
    ["dailymotion.com"] = {
        ["enable-plugins"] = true,
    },
    ["yahoo.com"] = {
        ["enable-scripts"] = false,
    },
    ["imdb.com"] = {
        ["enable-scripts"] = false,
    },
    ["kickasstorrents.com"] = {
        ["enable-plugins"] = true,
    },
    ["last.fm"] = {
        ["enable-plugins"] = true,
        ["enable-private-browsing"] = false,
    },
    ["pandora.com"] = {
        ["enable-plugins"] = true,
        ["enable-private-browsing"] = false,
    },
}

-- vim: et:sw=4:ts=8:sts=4:tw=80
