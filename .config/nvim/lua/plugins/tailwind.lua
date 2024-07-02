-- tailwind-tools.lua
return {
    {
        "luckasRanarison/tailwind-tools.nvim",
        dependencies = { "nvim-treesitter/nvim-treesitter" },
        opts = {
            filetypes = { "heex", "html", "css", "javascript", "typescript" },
        },

        {
            "laytan/tailwind-sorter.nvim",
            dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-lua/plenary.nvim" },
            build = "cd formatter && npm ci && npm run build",
            config = true,
            config = function()
                require("tailwind-sorter").setup({
                    on_save_enabled = true,
                })
            end,
        },
    },
}
