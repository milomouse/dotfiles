-- file: ${XDG_CONFIG_HOME}/nvim/lua/statusbar.lua

local lualine = require('lualine')

-- colors
local colors = {
  bg       = '#1E1E1E',
  fg       = '#8D8D8D',
  red      = '#C2003B',
  green    = '#00BB74',
  yellow   = '#A4B000',
  orange   = '#BB7400',
  blue     = '#005CF4',
  magenta  = '#9798E5',
  indigo   = '#6B0EFF',
  cyan     = '#00A4BB',
  white    = '#9E9DA5',
  gray     = '#5F5F5F',
}

-- pre-setup
local conditions = {
  buffer_not_empty = function()
    return vim.fn.empty(vim.fn.expand('%:t')) ~= 1
  end,
  hide_in_width = function()
    return vim.fn.winwidth(0) > 80
  end,
  check_git_workspace = function()
    local filepath = vim.fn.expand('%:p:h')
    local gitdir = vim.fn.finddir('.git', filepath .. ';')
    return gitdir and #gitdir > 0 and #gitdir < #filepath
  end,
}

-- redefine and clear sections
local config = {
  options = {
    component_separators = '',
    section_separators = '',
    theme = {
      -- we are going to use lualine_c an lualine_x as left and
      -- right section. both are highlighted by c theme.
      -- so we are just setting default looks to statusline.
      normal = { c = { fg = colors.fg, bg = colors.bg } },
      inactive = { c = { fg = colors.fg, bg = colors.bg } },
    },
  },
  sections = {
    -- these are to remove the defaults
    lualine_a = {},
    lualine_b = {},
    lualine_y = {},
    lualine_z = {},
    -- these will be filled later
    lualine_c = {},
    lualine_x = {},
  },
  inactive_sections = {
    -- these are to remove the defaults
    lualine_a = {},
    lualine_b = {},
    lualine_y = {},
    lualine_z = {},
    lualine_c = {},
    lualine_x = {},
  },
  -- top bar
  tabline = {
    lualine_a = {
      function()
        return ' '
      end,
    },
    lualine_b = {},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {
      {
        'buffers',
        show_filename_only = true,
        hide_filename_extension = false,
        show_modified_status = true,
        mode = 4,
        icons_enabled = false,
        use_mode_colors = false,
        buffers_color = {
          inactive = 'lualine_c_normal',
          active = 'lualine_z_normal',
        },
      }
    },
  },
}

-- inserts a component in lualine_c at left section
local function ins_left(component)
  table.insert(config.sections.lualine_c, component)
end

-- inserts a component in lualine_x at right section
local function ins_right(component)
  table.insert(config.sections.lualine_x, component)
end

-- start of left section

-- symbol at far left
ins_left {
  function()
    return '▊'
  end,
  color = { fg = colors.gray},
  padding = { left = 0, right = 1 },
}

-- change neovim mode icon colors
-- ins_left {
--   function()
--     return ''
--   end,
--   color = function()
--     -- auto change color according to mode
--     local mode_color = {
--       ['N '] = colors.red,
--       ['I '] = colors.magenta,
--       v = colors.yellow,
--       [''] = colors.yellow,
--       V = colors.yellow,
--       c = colors.white,
--       no = colors.indigo,
--       s = colors.indigo,
--       S = colors.indigo,
--       ic = colors.yellow,
--       cv = colors.blue,
--       ce = colors.blue,
--       t = colors.blue,
--       R = colors.green,
--       Rv = colors.green,
--       r = colors.cyan,
--       rm = colors.cyan,
--       ['r?'] = colors.cyan,
--       ['!'] = colors.red,
--       ['␖'] = colors.blue,
--       ['␓'] = colors.orange,
--     }
--     return { fg = mode_color[vim.fn.mode()] }
--   end,
--   padding = { right = 1 },
-- }

-- change neovim mode display
local mode_map = {
  icons_enabled = true,
  ['NORMAL']    = 'N ',
  ['O-PENDING'] = 'N?',
  ['INSERT']    = 'I ',
  ['VISUAL']    = 'V ',
  ['V-BLOCK']   = 'VB',
  ['V-LINE']    = 'VL',
  ['V-REPLACE'] = 'VR',
  ['REPLACE']   = 'R ',
  ['COMMAND']   = 'C ',
  ['EX']        = 'EX',
  ['S-BLOCK']   = 'SB',
  ['S-LINE']    = 'SL',
  ['SELECT']    = 'S ',
  ['CONFIRM']   = 'Y?',
  ['MORE']      = 'M ',
  ['SHELL']     = 'SH',
  ['TERMINAL']  = 'T ',
}
ins_left {
    'mode', fmt = function(s) return mode_map[s] or s end
}

-- position within file
ins_left { 'progress', color = { fg = colors.gray } }
ins_left { 'location', color = { fg = colors.gray } }
-- ins_left { 'selectioncount', color = { fg = colors.gray } }
-- ins_left { 'searchcount', color = { fg = colors.gray } }

-- filename
ins_left {
  'filename',
  path = 3,
  symbols = { modified = ' ', readonly = ' ', unnamed = '󰮍 ', newfile = '󰎔 ' },
  cond = conditions.buffer_not_empty,
  color = { fg = colors.white, gui = 'bold' },
}

-- git branch
ins_left {
  'branch',
  icon = '',
  color = { fg = colors.indigo, gui = 'bold' },
}

-- git diff
ins_left {
  'diff',
  symbols = { added = ' ', modified = ' ', removed = ' ' },
  diff_color = {
    added = { fg = colors.cyan },
    modified = { fg = colors.blue },
    removed = { fg = colors.magenta },
  },
  cond = conditions.hide_in_width,
}

-- start of middle section
ins_left {
  function()
    return '%='
  end,
}

-- start of right section

-- spell mode
ins_right {
  color = { fg = colors.green, gui = 'bold' },
  'mode',
  fmt = function(mode) return vim.go.spell == true and '󰓆' end
}

-- paste mode [for when default doesn't work correctly]
ins_right {
  color = { fg = colors.blue, gui = 'bold' },
  'mode',
  fmt = function(mode) return vim.go.paste == true and '' end
}

-- trailing whitespaces
ins_right {
  color = { fg = colors.cyan, gui = 'bold' },
  function()
    local space = vim.fn.search([[\s\+$]], 'nwc')
    return space ~= 0 and "·· "..space or ""
  end
}

-- mixed indentation [both tabs and spaces]
ins_right {
  color = { fg = colors.blue, gui = 'bold' },
  function()
    local space_pat = [[\v^ +]]
    local tab_pat = [[\v^\t+]]
    local space_indent = vim.fn.search(space_pat, 'nwc')
    local tab_indent = vim.fn.search(tab_pat, 'nwc')
    local mixed = (space_indent > 0 and tab_indent > 0)
    local mixed_same_line
    if not mixed then
      mixed_same_line = vim.fn.search([[\v^(\t+ | +\t)]], 'nwc')
      mixed = mixed_same_line > 0
    end
    if not mixed then return '' end
    if mixed_same_line ~= nil and mixed_same_line > 0 then
       return '│· '..mixed_same_line
    end
    local space_indent_cnt = vim.fn.searchcount({pattern=space_pat, max_count=1e3}).total
    local tab_indent_cnt =  vim.fn.searchcount({pattern=tab_pat, max_count=1e3}).total
    if space_indent_cnt > tab_indent_cnt then
      return '│· '..tab_indent
    else
      return '│· '..space_indent
    end
  end
}

-- language server diagnostics
ins_right {
  'diagnostics',
  sources = { 'nvim_diagnostic' },
  symbols = { error = ' ', warn = ' ', info = ' ' },
  colored = true,
  diagnostics_color = {
    error = { fg = colors.red },
    warn = { fg = colors.yellow },
    info = { fg = colors.blue },
  },
}

-- language server protocol (LSP)
ins_right {
  function()
    local msg = ''-- display when no LSP available
    local buf_ft = vim.api.nvim_buf_get_option(0, 'filetype')
    local clients = vim.lsp.get_active_clients()
    if next(clients) == nil then
      return msg
    end
    for _, client in ipairs(clients) do
      local filetypes = client.config.filetypes
      if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
        return client.name
      end
    end
    return msg
  end,
  icons_enabled = true,
  -- icon = 'LSP:',-- prepend before active LSP
  color = { fg = colors.white },
}

-- filetype
ins_right {
  'filetype',
  fmt = string.upper,
  icons_enabled = true,
  icon_only = false,
  colored = false,
  color = { fg = colors.gray, gui = 'bold' },
}

-- file encoding
ins_right {
  'o:encoding',
  fmt = string.upper,
  cond = conditions.hide_in_width,
  icons_enabled = false,
  color = { fg = colors.gray, gui = 'bold' },
}

-- file format
ins_right {
  'fileformat',
  fmt = string.upper,
  icons_enabled = false,
  color = { fg = colors.gray, gui = 'bold' },
}

-- initialize the configured setup
lualine.setup(config)
