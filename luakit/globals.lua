-- Global variables for luakit
globals = {
    homepage            = "https://bbs.archlinux.org/search.php?action=show_new",
    scroll_step         = 40,
    scroll_step_more    = 400,
    scroll_step_micro   = 4,
    zoom_step           = 0.1,
    max_cmd_history     = 100,
    max_srch_history    = 100,
    http_proxy          = "127.0.0.1:8118",
    default_window_size = "1280x800",

 -- Disables loading of hostnames from /etc/hosts (for large host files)
    load_etc_hosts      = false,
 -- Disables checking if a filepath exists in search_open function
    check_filepath      = true,
}

-- Make useragent
local _, arch = luakit.spawn_sync("uname -sm")
-- Only use the luakit version if in date format (reduces identifiability)
local lkv = string.match(luakit.version, "^(%d+.%d+.%d+)")
globals.useragent = string.format("Mozilla/5.0 (%s) AppleWebKit/%s+ (KHTML, like Gecko) WebKitGTK+/%s luakit%s",
    string.sub(arch, 1, -2), luakit.webkit_user_agent_version,
    luakit.webkit_version, (lkv and ("/" .. lkv)) or "")
--For those websites who fuck with user experiences or forbid access due to unrecognized user string:
--globals.useragent = string.format("Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_5_6; en-us) AppleWebKit/528.16 (KHTML, like Gecko) Version/4.0 Safari/528.16", arch, awkv, wkv, lkv)
--globals.useragent = string.format("Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_8; en-us) AppleWebKit/537.13+ (KHTML, like Gecko) Version/5.1.7 Safari/534.57.2", arch, awkv, wkv, lkv)

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
        soup.ssl_ca_file = ca_file
        break
    end
end

-- Change to stop navigation sites with invalid or expired ssl certificates
soup.ssl_strict = false

-- Set cookie acceptance policy
cookie_policy = { always = 0, never = 1, no_third_party = 2 }
soup.accept_policy = cookie_policy.no_third_party

-- List of search engines. Each item must contain a single %s which is
-- replaced by URI encoded search terms. All other occurances of the percent
-- character (%) may need to be escaped by placing another % before or after
-- it to avoid collisions with lua's string.format characters.
-- See: http://www.lua.org/manual/5.1/manual.html#pdf-string.format
search_engines = {
  ddg        = "https://duckduckgo.com/?kd=1&k1=-1&ke=-1&ka=s&kb=d&kf=fw&kh=1&kk=-1&ko=s&kr=b&kt=n&kv=1&kw=n&kx=e&ky=-1&kp=1&q=%s",
  postrock   = "http://www.postrockxchange.com/?s=%s",
  tiger      = "http://www.tigerdirect.com/applications/SearchTools/search.asp?keywords=%s",
  luakit     = "http://luakit.org/search/index/luakit?q=%s",
  craigslist = "http://asheville.craigslist.org/search/?query=%s&catAbb=sss",
}

-- Set duckduckgo as fallback search engine
search_engines.default = search_engines.ddg
-- Use this instead to disable auto-searching
--search_engines.default = "%s"

-- Per-domain webview properties
-- See http://webkitgtk.org/reference/webkitgtk/stable/WebKitWebSettings.html
domain_props = {
    ["all"] = {
        ["enable-private-browsing"] = false,
    },
    ["google.com"] = {
        ["enable-private-browsing"] = true,
    },
   --[[ ["all"] = {
        enable_scripts          = false,
        enable_plugins          = false,
        enable_private_browsing = false,
        user_stylesheet_uri     = "",
    },
    ["youtube.com"] = {
        enable_scripts = true,
        enable_plugins = true,
    },
    ["bbs.archlinux.org"] = {
        user_stylesheet_uri     = "file://" .. luakit.data_dir .. "/styles/dark.css",
        enable_private_browsing = true,
    }, ]] --]]
}

-- vim: et:sw=4:ts=8:sts=4:tw=80
