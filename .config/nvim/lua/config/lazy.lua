-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    spec = {
        { import = "plugins" },
        {
            "folke/zen-mode.nvim",
            opts = {
                -- window = {
                --     width = 10,
                --     height = 1,
                -- },
                plugins = {
                    alacritty = {
                        enabled = true,
                        font = "20", -- font size
                    },
                },
            },
        },
        {
            "declancm/maximize.nvim",
            config = function()
                vim.keymap.set("n", "<leader>wm", "<cmd>:Maximize<cr>", { desc = "Maximize window" })
            end,
        },
        {
            "typicode/bg.nvim",
        },
    },

    -- Configure any other settings here. See the documentation for more details.
    -- colorscheme that will be used when installing plugins.
    install = { colorscheme = { "habamax" } },
    -- automatically check for plugin updates
    checker = { enabled = false },
})
