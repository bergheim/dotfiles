return {

    "tpope/vim-sleuth", -- Detect tabstop and shiftwidth automatically
    "farmergreg/vim-lastplace",
    { "numToStr/Comment.nvim", opts = {} },
    {
        "kylechui/nvim-surround",
        version = "*", -- Use for stability; omit to use `main` branch for the latest features
        event = "VeryLazy",
        config = function()
            require("nvim-surround").setup({
                -- Configuration here, or leave empty to use defaults
            })
        end,
    },
    {
        "folke/flash.nvim",
        event = "VeryLazy",
        ---@type Flash.Config
        opts = {
            -- jump = {
            --     autojump = true,
            -- },
        },
        -- stylua: ignore
        keys = {
            { "s", mode = { "n", "x", "o" }, function() require("flash").jump() end, desc = "Flash" },
            { "S", mode = { "n", "x", "o" }, function() require("flash").treesitter() end, desc = "Flash Treesitter" },
            { "r", mode = "o", function() require("flash").remote() end, desc = "Remote Flash" },
            { "R", mode = { "o", "x" }, function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
            { "<c-s>", mode = { "c" }, function() require("flash").toggle() end, desc = "Toggle Flash Search" },
        },
    },
    -- {
    --     "ggandor/leap.nvim",
    --     opts = {},
    --     config = function()
    --         require("leap").opts.equivalence_classes = { " \t\r\n", "([{", ")]}", "'\"`" }
    --         require("leap").create_default_mappings()
    --     end,
    -- },
    {
        "windwp/nvim-autopairs",
        event = "InsertEnter",
        config = true,
        opts = {
            check_ts = true,
        },
    },
    -- Highlight todo, notes, etc in comments
    {
        "folke/todo-comments.nvim",
        event = "VimEnter",
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = { signs = false },
        vim.keymap.set("n", "<leader>st", vim.cmd.TodoTelescope, { desc = "Search TODOs" }),
    },
}
