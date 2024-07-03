return {
    {
        "folke/which-key.nvim",
        event = "VimEnter", -- Sets the loading event to 'VimEnter'
        config = function() -- This is the function that runs, AFTER loading
            require("which-key").setup()

            require("which-key").register({
                ["<leader>c"] = { name = "Code", _ = "which_key_ignore" },
                ["<leader>d"] = { name = "Document", _ = "which_key_ignore" },
                -- ["<leader>r"] = { name = "[R]ename", _ = "which_key_ignore" },
                ["<leader>s"] = { name = "Search", _ = "which_key_ignore" },
                ["<leader>h"] = { name = "Help", _ = "which_key_ignore" },
                ["<leader>a"] = { name = "Apps", _ = "which_key_ignore" },
                ["<leader>b"] = { name = "Buffers", _ = "which_key_ignore" },
                ["<leader>f"] = { name = "Files", _ = "which_key_ignore" },
                ["<leader>p"] = { name = "Project", _ = "which_key_ignore" },
                ["<leader>w"] = { name = "Workspace", _ = "which_key_ignore" },
                ["<leader>t"] = { name = "Toggle", _ = "which_key_ignore" },
                ["<leader>g"] = { name = "Git", _ = "which_key_ignore" },
            })
            -- visual mode
            require("which-key").register({
                ["<leader>gh"] = { "hunk" },
            }, { mode = "v" })
        end,
    },
}
