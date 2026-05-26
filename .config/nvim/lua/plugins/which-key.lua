return {
    {
        "folke/which-key.nvim",
        event = "VimEnter", -- Sets the loading event to 'VimEnter'
        config = function() -- This is the function that runs, AFTER loading
            require("which-key").setup()

            require("which-key").add({
                { "<leader>a", group = "Apps" },
                { "<leader>b", group = "Buffers" },
                { "<leader>c", group = "Code" },
                { "<leader>d", group = "Document" },
                { "<leader>f", group = "Files" },
                { "<leader>g", group = "Git" },
                { "<leader>h", group = "Help" },
                { "<leader>p", group = "Project" },
                { "<leader>q", group = "Session" },
                -- { "<leader>r", group = "Rename" },
                { "<leader>s", group = "Search" },
                { "<leader>t", group = "Toggle" },
                { "<leader>w", group = "Workspace" },
                -- visual mode
                { "<leader>gh", desc = "hunk", mode = "v" },
            })
        end,
    },
}
