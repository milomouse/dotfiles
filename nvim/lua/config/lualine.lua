-- file: ${XDG_CONFIG_HOME}/nvim/lua/config/lualine.lua

-- initialize lualine
local lualine = require('lualine')

-- color variables
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
    gray2    = '#121212',
    gray3    = '#262626',
    gray4    = '#444444',
    gray5    = '#585858',
}


-- mode names
local mode_map = {
    ['NORMAL']      = 'N ',-- normal
    ['n']           = 'N ',
    ['O-PENDING']   = 'N?',-- o-pending
    ['no']          = 'N?',
    ['nov']         = 'N?',
    ['noV']         = 'N?',
    ['no\22']       = 'N?',
    ['niI']         = 'N-',-- (other?)
    ['niR']         = 'N-',
    ['niV']         = 'N-',
    ['nt']          = 'N-',
    ['ntT']         = 'N-',
    ['INSERT']      = 'I ',--insert
    ['i']           = 'I ',
    ['ic']          = 'I ',
    ['ix']          = 'I ',
    ['VISUAL']      = 'V ',-- visual
    ['v']           = 'V ',
    ['vs']          = 'V ',
    ['V-LINE']      = 'VL',-- v-line
    ['V']           = 'VL',
    ['Vs']          = 'VL',
    ['V-BLOCK']     = 'VB',-- v-block
    ['\22s']        = 'VB',
    ['\22']         = 'VB',
    ['^V']          = 'VB',
    ['SELECT']      = 'S ',-- select
    ['s']           = 'S ',
    ['S-LINE']      = 'SL',-- s-line
    ['S']           = 'SL',
    ['S-BLOCK']     = 'SB',-- s-block
    ['\19']         = 'SB',
    ['multi']       = 'SM',
    ['V-REPLACE']   = 'VR',-- v-replace
    ['Rv']          = 'VR',
    ['Rvc']         = 'VR',
    ['Rvx']         = 'VR',
    ['REPLACE']     = 'R ',-- replace
    ['r']           = 'R ',
    ['R']           = 'R ',
    ['Rc']          = 'R ',
    ['Rx']          = 'R ',
    ['CONFIRM']     = 'Y?',-- confirm
    ['r?']          = 'Y?',
    ['MORE']        = 'M-',-- more
    ['rm']          = 'M-',
    ['COMMAND']     = 'C ',-- command
    ['c']           = 'C ',
    ['EX']          = 'EX',-- EX
    ['cv']          = 'EX',
    ['ce']          = 'EX',
    ['TERMINAL']    = 'T ',-- terminal
    ['t']           = 'T ',
    ['SHELL']       = 'SH',-- shell
    ['!']           = 'SH',
}


-- mode colors
local mode_color = {
    ['n']       = colors.gray,-- normal
    ['no']      = colors.gray,-- o-pending
    ['nov']     = colors.gray,
    ['noV']     = colors.gray,
    ['no\22']   = colors.gray,
    ['niI']     = colors.fg,-- (other?)
    ['niR']     = colors.fg,
    ['niV']     = colors.fg,
    ['nt']      = colors.fg,
    ['ntT']     = colors.fg,
    ['i']       = colors.red,--insert
    ['ic']      = colors.red,
    ['ix']      = colors.red,
    ['v']       = colors.yellow,--visual
    ['vs']      = colors.yellow,
    ['V']       = colors.yellow,-- v-line
    ['Vs']      = colors.yellow,
    ['\22s']    = colors.yellow,-- v-block
    ['\22']     = colors.yellow,
    ['^V']      = colors.yellow,
    ['s']       = colors.orange,-- select
    ['S']       = colors.orange,-- s-line
    ['\19']     = colors.orange,-- s-block
    ['multi']   = colors.orange,
    ['Rv']      = colors.cyan,-- v-replace
    ['Rvc']     = colors.cyan,
    ['Rvx']     = colors.cyan,
    ['r']       = colors.green,-- replace
    ['R']       = colors.green,
    ['Rc']      = colors.green,
    ['Rx']      = colors.green,
    ['r?']      = colors.magenta,-- confirm
    ['rm']      = colors.magenta,-- more
    ['c']       = colors.indigo,-- command
    ['cv']      = colors.indigo,-- EX
    ['ce']      = colors.indigo,
    ['t']       = colors.blue,-- terminal
    ['!']       = colors.blue,-- shell
    ['␖']       = colors.red,-- misc.
    ['␓']       = colors.red,
    ['']        = colors.yellow,
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
--
-- +-------------------------------------------------+
-- | A | B | C                             X | Y | Z |
-- +-------------------------------------------------+
--
local config = {
    options = {
        component_separators = '',
        section_separators = '',
        theme = {
            -- we are going to use lualine_c as left section, with lualine_x as right section.
            -- both are highlighted by c theme (unless manually overridden).
            -- these settings form the default look of the statusline.
            normal = { c = { fg = colors.fg, bg = colors.bg } },
            inactive = { c = { fg = colors.fg, bg = colors.bg } },
            -- buffer colors (can be used elsewhere too)
            normal_buf = { z = { fg = colors.white, bg = colors.gray2 } },
            inactive_buf = { z = { fg = colors.gray4, bg = colors.bg } },
            -- -- tab colors (can be used elsewhere too)
            -- normal_tab = { z = { fg = colors.white, bg = colors.gray2 } },
            -- inactive_tab = { z = { fg = colors.gray4, bg = colors.gray2 } },
        },
    },
    sections = {
        -- these are to remove the defaults
        lualine_a = {},
        lualine_b = {},
        lualine_c = {},-- this will be filled later
        lualine_x = {},-- ^
        lualine_y = {},
        lualine_z = {},
    },
    inactive_sections = {
        -- these are to remove the defaults (empty when not focused)
        lualine_a = {},
        lualine_b = {},
        lualine_c = {
            {
                function()
                    return '▊ 󰒲'-- inactive indicator
                end,
                color = { fg = colors.gray3 },
                padding = { left = 0, right = 1 },
            },
            {
                'filename',-- at least show filename, if available, to distinguish between inactive windows
                path = 5,
                file_status = true,
                newfile_status = true,
                symbols = { modified = '  ', readonly = '  ', unnamed = ' 󰮍 ', newfile = ' - ' },--' 󰎔 '
                cond = conditions.buffer_not_empty,
                color = { fg = colors.gray4 },
            },
        },
        lualine_x = {},
        lualine_y = {},
        lualine_z = {},
    },
    -- top bar
    tabline = {
        lualine_a = {
            {
                function()
                    -- return ' '
                    -- return ' '
                    return ' '
                end,
            },
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
                    active = 'lualine_z_normal_buf',
                    inactive = 'lualine_z_inactive_buf',
                },
            },

            -- -- wait until we install one.nvim
            -- {
            --     function()
            --         return '▊'
            --     end,
            --     color = { fg = colors.gray },
            --     padding = { left = 0, right = 0 }
            -- },
            -- {
            --     'tabs',
            --     padding = { left = 1, right = 1 },
            --     tab_max_length = 6,
            --     mode = 0,
            --     use_mode_colors = false,
            --     tabs_color = {
            --         active = 'lualine_z_tnormal',
            --         inactive = 'lualine_z_tinactive',
            --     },
            --     show_modified_status = true,
            --     symbols = {
            --         modified = '!',
            --     },
            -- }

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


-- -- start of left section

-- symbol at far left
ins_left
{
    function()
        return '▊'
    end,
    color = { fg = colors.gray },
    padding = { left = 0, right = 0 },
}

-- truncated mode_name with custom mode_color
ins_left
{
    'mode',
    fmt = function(s) return mode_map[s] or s end,
    color = function()
        return { fg = mode_color[vim.fn.mode()], bg = colors.bg, gui = 'bold' }
    end,
    padding = { left = 2, right = 2 },
}


-- position within file
ins_left { 'progress', color = { fg = colors.gray } }
ins_left { 'location', color = { fg = colors.gray } }
ins_left { 'selectioncount', color = { fg = colors.magenta } }
ins_left { 'searchcount', color = { fg = colors.indigo } }


-- filename
ins_left
{
    'filename',
    path = 4,
    file_status = true,
    newfile_status = true,
    symbols = { modified = '  ', readonly = '  ', unnamed = ' 󰮍 ', newfile = ' - ' },--' 󰎔 '
    cond = conditions.buffer_not_empty,
    color = { fg = colors.white },
}


-- git branch
ins_left {
    'branch',
    icon = '',--󰊤 (GitHub)
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


-- -- start of middle section
ins_left {
    function()
        return '%='
    end,
}


-- -- start of right section

-- spell mode (does not show symbol when using :setlocal spell)
ins_right {
    color = { fg = colors.green, gui = 'bold' },
    'mode',
    fmt = function(mode) return vim.go.spell == true and '󰓆' end
}


-- paste mode
ins_right {
    color = { fg = colors.yellow, gui = 'bold' },
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
        info = { fg = colors.cyan },
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
    fmt = string.upper,
    icons_enabled = true,
    icon = '󰨮',
    color = { fg = colors.cyan, gui = 'bold' },
}


-- filetype [icon]
ins_right {
    'filetype',
    fmt = string.upper,
    icons_enabled = true,
    icon_only = true,
    colored = false,
    color = { fg = colors.fg, gui = 'bold' },
    padding = { left = 1, right = 0 },
}

-- filetype [label]
ins_right {
    'filetype',
    fmt = string.upper,
    icons_enabled = false,
    icon_only = false,
    colored = false,
    color = { fg = colors.gray5, gui = 'bold' },
    padding = { left = 0, right = 1 },
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
