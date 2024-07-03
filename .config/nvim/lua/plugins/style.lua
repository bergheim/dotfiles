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
    -- themes
    { "catppuccin/nvim", name = "catppuccin", priority = 1000 },
    { "ellisonleao/gruvbox.nvim", priority = 1000, config = true, opts = {} },
    { "EdenEast/nightfox.nvim", priority = 1000 },
    { "sainnhe/everforest", priority = 1000 },
    { "NLKNguyen/papercolor-theme", priority = 1000 },
    { "vimpostor/vim-prism", priority = 1000 },
}
