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
    bbs         = "https://bbs.archlinux.org/search.php?action=search&keywords={0}&author=&forum=-1&search_in=all&sort_by=0&sort_dir=DESC&show_as=topics",
    aur         = "http://aur.archlinux.org/packages.php?O=0&K={0}",
    apkg        = "http://www.archlinux.org/packages?sort=&arch=x86_64&repo=&q={0}&maintainer=&last_update=&flagged=&limit=50",
    awiki       = "https://wiki.archlinux.org/index.php?title=Special%3ASearch&search={0}&go=Go",
    archbugs    = "https://bugs.archlinux.org/index.php?string={0}&project=1&type%5B%5D=&sev%5B%5D=&pri%5B%5D=&due%5B%5D=&reported%5B%5D=&cat%5B%5D=&status%5B%5D=open&percent%5B%5D=&opened=&dev=&closed=&duedatefrom=&duedateto=&changedfrom=&changedto=&openedfrom=&openedto=&closedfrom=&closedto=&do=index",
    github      = "https://github.com/search?q={0}&type=Everything&repo=&langOverride=&start_value=1",
    wikipedia   = "http://en.wikipedia.org/wiki/Special:Search?search={0}",
    factbites   = "http://www.factbites.com/topics/{0}",
    ddg         = "https://duckduckgo.com/?q={0}",
    google      = "http://www.google.com/search?q={0}",
    googlessl   = "https://www.google.com/search?q={0}",
    images      = "http://www.google.com/images?q={0}&um=1&ie=UTF-8&source=og&sa=N&hl=en&tab=wi",
    newegg      = "http://www.newegg.com/Product/ProductList.aspx?Submit=ENE&DEPA=0&Order=BESTMATCH&Description={0}",
    tigerdirect = "http://www.tigerdirect.com/applications/SearchTools/search.asp?keywords={0}",
    imdb        = "http://www.imdb.com/find?s=all&q={0}",
    youtube     = "http://www.youtube.com/results?search_query={0}",
    lastfm      = "http://www.last.fm/music/?q={0}",
    amazon      = "http://www.amazon.com/s/ref=nb_ss_gw?url=search-alias%3Dall&field-keywords={0}",
    ebay        = "http://shop.ebay.com/?_from=R40&_trksid=p3907.m570.l1313&_nkw={0}&_sacat=See-All-Categories",
    postrock    = "http://www.postrockxchange.com/?s={0}",
    kickass     = "http://www.kickasstorrents.com/search/{0}/",
    btjunkie    = "http://btjunkie.org/search?q={0}",
    piratebay   = "http://thepiratebay.org/search/{0}",
}

-- Set fallback search engine
search_engines.default = search_engines.ddg
-- Use this instead to disable auto-searching
--search_engines.default = "{0}"

-- Fake the cookie policy enum here
cookie_policy = { always = 0, never = 1, no_third_party = 2 }

-- Per-domain webview properties
-- See http://webkitgtk.org/reference/webkitgtk-WebKitWebSettings.html
domain_props = {
    ["all"] = {
        ["enable-scripts"] = true,
        ["enable-plugins"] = false,
        ["enable-private-browsing"] = true,
       -- ["user-stylesheet-uri"] = "file://" .. luakit.data_dir .. "/styles/mouse.css",
        ["accept-policy"] = cookie_policy.no_third_party,
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
        ["accept-policy"] = cookie_policy.never,
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
        ["accept-policy"] = cookie_policy.never,
    },
    ["imdb.com"] = {
        ["enable-scripts"] = false,
        ["accept-policy"] = cookie_policy.never,
    },
    ["btjunkie.org"] = {
        ["accept-policy"] = cookie_policy.never,
    },
    ["kickasstorrents.com"] = {
        ["enable-plugins"] = true,
    },
    ["thepiratebay.org"] = {
        ["accept-policy"] = cookie_policy.never,
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
