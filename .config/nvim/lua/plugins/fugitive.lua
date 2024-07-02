return {
    "tpope/vim-fugitive",
    config = function()
        vim.keymap.set("n", "<leader>gs", "<cmd>Git add %<cr>", { desc = "Add current file to Git" })
        vim.keymap.set("n", "<leader>gg", vim.cmd.Git, { desc = "Git status" })
    end,
}
