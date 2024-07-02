vim.o.background = "dark"
vim.cmd([[colorscheme everforest]])

function set_transparent_background()
    vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
end

function dark_light_theme()
    local handle = io.popen("gsettings get org.gnome.desktop.interface color-scheme")
    local output = handle:read("*a")
    handle:close()
    output = output:gsub("'", "")

    if string.find(output, "dark") then
        vim.o.background = "dark"
        -- vim.cmd.colorscheme 'carbonfox'
        vim.cmd([[hi CursorLine guibg=#252525]])
    else
        vim.o.background = "light"
        -- vim.cmd.colorscheme 'dayfox'
    end
    set_transparent_background()
end

dark_light_theme()
set_transparent_background()

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
