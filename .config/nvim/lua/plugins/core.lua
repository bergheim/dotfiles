return {

    "tpope/vim-sleuth", -- Detect tabstop and shiftwidth automatically
    "farmergreg/vim-lastplace",
    -- good comment (gc) support
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
    -- Session management. This saves your session in the background,
    -- keeping track of open buffers, window arrangement, and more.
    -- You can restore sessions when returning through the dashboard.
    {
        "folke/persistence.nvim",
        event = "BufReadPre",
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = { options = vim.opt.sessionoptions:get() },
        -- stylua: ignore
        keys = {
            { "<leader>qs", function() require("persistence").load() end, desc = "Restore Session" },
            { "<leader>ql", function() require("persistence").load({ last = true }) end, desc = "Restore Last Session" },
            { "<leader>qd", function() require("persistence").stop() end, desc = "Don't Save Current Session" },
        },
    },
    {
        "ahmedkhalf/project.nvim",
        config = function()
            require("project_nvim").setup({
                require("telescope").load_extension("projects"),
                vim.keymap.set("n", "<leader>pp", function()
                    require("telescope").extensions.projects.projects({})
                end, { desc = "Select project" }),
            })
        end,
    },
}
