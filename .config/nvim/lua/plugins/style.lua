return {
    -- {
    --     'nvim-lualine/lualine.nvim',
    --     dependencies = { 'nvim-tree/nvim-web-devicons' },
    --     opts = {}
    -- },
    -- {
    --     "stevearc/dressing.nvim",
    --     opts = {},
    -- },
    -- show error/warnings in the scrollbar
    {
        "petertriho/nvim-scrollbar",
        opts = {
            marks = {
                Error = {
                    -- text = { "-", "x" },
                    color = "Red",
                },
            },
        },
    },
    {
        "brenoprata10/nvim-highlight-colors",
        opts = {},
    },
    {
        "nvimdev/dashboard-nvim",
        event = "VimEnter",
        config = function()
            require("dashboard").setup({
                -- config
            })
        end,
        dependencies = { { "nvim-tree/nvim-web-devicons" } },
    },
    -- {
    --     "hiphish/rainbow-delimiters.nvim",
    --     config = function()
    --         require("rainbow-delimiters.setup").setup()
    --     end,
    -- },
    -- themes
    { "catppuccin/nvim", name = "catppuccin", priority = 1000 },
    { "ellisonleao/gruvbox.nvim", priority = 1000, config = true, opts = {} },
    { "EdenEast/nightfox.nvim", priority = 1000 },
    {
        "sainnhe/everforest",
        -- config = function()
        --     require("everforest").setup({
        --         background = "hard",
        --     })
        -- end,
    },
    { "NLKNguyen/papercolor-theme", priority = 1000 },
    { "vimpostor/vim-prism", priority = 1000 },
    {
        "folke/tokyonight.nvim",
        lazy = false,
        priority = 1000,
        opts = {},
    },
}
