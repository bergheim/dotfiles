vim.opt.tabstop = 4 -- Number of spaces a <Tab> in the file counts for
vim.opt.softtabstop = 4 -- Number of spaces a <Tab> in the file counts for
vim.opt.shiftwidth = 4 -- Number of spaces to use for each step of (auto)indent
vim.opt.expandtab = true -- Use spaces instead of tabs

vim.o.background = "dark"
vim.cmd([[colorscheme everforest]])

function set_transparent_background()
    vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

    vim.api.nvim_set_hl(0, "SignColumn", { bg = "none" })
    vim.api.nvim_set_hl(0, "StatusLine", { bg = "none" })
    vim.api.nvim_set_hl(0, "StatusLineNC", { bg = "none" }) -- Non-current windows
    vim.api.nvim_set_hl(0, "WinSeparator", { bg = "none" }) -- Window separators
    vim.api.nvim_set_hl(0, "VertSplit", { bg = "none" }) -- Vertical split lines
    vim.api.nvim_set_hl(0, "Folded", { bg = "none" }) -- Folded lines
    vim.api.nvim_set_hl(0, "EndOfBuffer", { bg = "none" }) -- Empty lines at end of buffer
    vim.api.nvim_set_hl(0, "TabLineFill", { bg = "none" }) -- Tabline background
    vim.api.nvim_set_hl(0, "TabLine", { bg = "none" }) -- Active tab background
    vim.api.nvim_set_hl(0, "TabLineSel", { bg = "none" }) -- Inactive tab background

    vim.api.nvim_set_hl(0, "bg_dim", { bg = "none" }) -- Inactive tab background
    vim.api.nvim_set_hl(0, "bg0", { bg = "none" }) -- Inactive tab background
    vim.api.nvim_set_hl(0, "bg1", { bg = "none" }) -- Inactive tab background
end

function dark_light_theme()
    local handle = io.popen("gsettings get org.gnome.desktop.interface color-scheme")
    local output = handle:read("*a")
    handle:close()
    output = output:gsub("'", "")

    if string.find(output, "dark") then
        vim.o.background = "dark"
        vim.cmd.colorscheme("carbonfox")
        vim.cmd([[hi CursorLine guibg=#252525]])
    else
        vim.o.background = "light"
        vim.cmd.colorscheme("everforest")
    end
    -- set_transparent_background()
end

dark_light_theme()
-- set_transparent_background()

local function get_current_background_color()
    local hl = vim.api.nvim_get_hl_by_name("Normal", true)
    if hl.background then
        -- Convert the color from decimal to hex
        local bg_color = string.format("#%06x", hl.background)
        return bg_color
    else
        return "NONE"
    end
end
