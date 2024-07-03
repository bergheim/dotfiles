return {
    {

        "tpope/vim-fugitive",
        config = function()
            vim.keymap.set("n", "<leader>gs", "<cmd>Git add %<cr>", { desc = "Add current file to Git" })
            -- vim.keymap.set("n", "<leader>gg", vim.cmd.Git, { desc = "Git status" })
        end,
    },
    {
        "lewis6991/gitsigns.nvim",
        opts = {
            signs = {
                add = { text = "+" },
                change = { text = "~" },
                delete = { text = "_" },
                topdelete = { text = "‾" },
                changedelete = { text = "~" },
            },
        },
    },

    {
        "NeogitOrg/neogit",
        dependencies = {
            "nvim-lua/plenary.nvim", -- required
            "sindrets/diffview.nvim", -- optional - Diff integration

            -- Only one of these is needed, not both.
            "nvim-telescope/telescope.nvim", -- optional
            -- "ibhagwan/fzf-lua", -- optional
        },
        config = true,
        keys = {
            { "<leader>gg", "<cmd>Neogit<cr>", desc = "Neogit" },
        },
    },
}
