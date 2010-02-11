-- $Id: statusd_df.lua 60 2006-11-14 11:19:29Z tibi $

local defaults = {
   template = "%avail/%availp",
   fslist = { "/", "/home" },
   separator = " }} ",
   update_interval = 5000, -- 1 second
}
                
local settings = table.join(statusd.get_config("df"), defaults)

local df_timer = statusd.create_timer()

function math.round(num, idp)
   local mult = 10^(idp or 0)
   return math.floor(num  * mult + 0.5) / mult
end

local function guess_mem_unit(amount)
   amount = tonumber(amount)
   if (amount < 1024) then
      return amount .. "k"
   elseif (amount >= 1024) and (amount < 1048576) then
      return math.round((amount / 1024), 0) .. "M"
   elseif (amount > 1048576) then
      return math.round((amount / 1048576), 1) .. "G"
   end
end

local function get_df()
   local df_table = {}
   local f = io.popen('df -k', 'r')
   if (f == nil) then return nil end
   f:read("*line") -- skip header line
   local s = f:read("*a")
   f:close()
   local i = 0
   while (i < string.len(s)) do
      local j, fsname, fssize, fsused, fsavail, fsusedp, mpoint
      i, j, fsname, fssize, fsused, fsavail, fsusedp, mpoint
	 = string.find(s, "(/%S+)%s+(%d+)%s+(%d+)%s+(%d+)%s+(%d+)%%?%s(%S+)\n",
		       i)
      if (i == nil) then return nil end
      df_table[mpoint] = { mpoint=mpoint,
	                   fs=fsname,
	                   size=guess_mem_unit(tonumber(fssize)),
			   used=guess_mem_unit(tonumber(fsused)),
			   avail=guess_mem_unit(tonumber(fsavail)),
			   usedp=tonumber(fsusedp),
			   availp=((100 - tonumber(fsusedp)) .. "%") }
      i = j+1
   end
   return df_table
end

local function update_df()
   local t = get_df()
   if (t == nil) then return nil end
   local df_str = ""
   for i=1, #settings.fslist do
      local s = string.gsub(settings.template, "%%(%w+)", "critical",
			    function (arg)
			       if (t[settings.fslist[i]] ~= nil) then
				  return t[settings.fslist[i]][arg]
			       end
			       return nil
			    end)
      df_str = df_str .. settings.separator .. s
   end
   df_str = string.sub(df_str, #settings.separator + 1)
   statusd.inform("df", df_str)
   df_timer:set(settings.update_interval, update_df)
end

update_df()

-- EOF
