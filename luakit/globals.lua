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

--[[
  List of search engines. Each item must contain a single %s which is
  replaced by URI encoded search terms. All other occurances of the percent
  character (%) may need to be escaped by placing another % before or after
  it to avoid collisions with lua's string.format characters.
  See: http://www.lua.org/manual/5.1/manual.html#pdf-string.format
  Personal Note:
    some searches such as !lisp or !perl are excluded because it's just one
    character extra to type.. while certain sites i'd rather redirect through
    duckduckgo due to referrers, etc. privoxy also helps. --]]
search_engines = {
    ddg      = "https://duckduckgo.com/?q=%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    ducky    = "https://duckduckgo.com/?q=%%21ducky%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    images   = "https://duckduckgo.com/?q=%%21googleimages%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    bimages  = "https://duckduckgo.com/?q=%%21bingimages%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    map      = "https://duckduckgo.com/?q=%%21googlemaps%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    bmap     = "https://duckduckgo.com/?q=%%21bingmaps%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    arch     = "https://duckduckgo.com/?q=%%21archlinux%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    awiki    = "https://duckduckgo.com/?q=%%21archwiki%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    apkg     = "https://duckduckgo.com/?q=%%21archpkg%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    aur      = "https://duckduckgo.com/?q=%%21archaur%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    stack    = "https://duckduckgo.com/?q=%%21stackoverflow%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    git      = "https://duckduckgo.com/?q=%%21github%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    wiki     = "https://duckduckgo.com/?q=%%21wikipedia%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    newegg   = "https://duckduckgo.com/?q=%%21newegg%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    imdb     = "https://duckduckgo.com/?q=%%21imdb%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    youtube  = "https://duckduckgo.com/?q=%%21youtube%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    motion   = "https://duckduckgo.com/?q=%%21dailymotion%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    lastfm   = "https://duckduckgo.com/?q=%%21lastfm%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    amazon   = "https://duckduckgo.com/?q=%%21amazon%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    ebay     = "https://duckduckgo.com/?q=%%21ebay%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    list     = "https://duckduckgo.com/?q=%%21craigslist%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    kt       = "https://duckduckgo.com/?q=%%21kickasstorrents%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    pt       = "https://duckduckgo.com/?q=%%21thepiratebay%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    bt       = "https://duckduckgo.com/?q=%%21btjunkie%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    dt       = "https://duckduckgo.com/?q=%%21demonoid%%20%s&kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kp=1&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1",
    postrock = "http://www.postrockxchange.com/?s=%s",
    tiger    = "http://www.tigerdirect.com/applications/SearchTools/search.asp?keywords=%s",
    luakit   = "http://luakit.org/search/index/luakit?q=%s",
}

-- Set duckduckgo as fallback search engine
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
        --["user-stylesheet-uri"] = "file://" .. luakit.data_dir .. "/styles/everymouse.css",
    },
    ["bbs.archlinux.org"] = {
        ["enable-private-browsing"] = false,
        ["user-stylesheet-uri"] = "file://" .. luakit.data_dir .. "/styles/archmouse.css",
    },
    ["bugs.archlinux.org"] = {
        ["enable-private-browsing"] = false,
        ["user-stylesheet-uri"] = "file://" .. luakit.data_dir .. "/styles/archmouse.css",
    },
    ["aur.archlinux.org"] = {
        ["enable-private-browsing"] = false,
        ["user-stylesheet-uri"] = "file://" .. luakit.data_dir .. "/styles/archmouse.css",
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
    ["nationstates.net"] = {
        ["enable-plugins"] = true,
        ["enable-private-browsing"] = false,
    },
}

-- vim: et:sw=4:ts=8:sts=4:tw=80
