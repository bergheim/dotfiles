-- tailwind-tools.lua
return {
    {
        "luckasRanarison/tailwind-tools.nvim",
        dependencies = { "nvim-treesitter/nvim-treesitter" },
        opts = {
            -- filetypes = { "heex", "html", "css", "javascript", "typescript" },
        },
    },
    {
        "laytan/tailwind-sorter.nvim",
        dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-lua/plenary.nvim" },
        build = "cd formatter && npm ci && npm run build",
        config = function()
            require("tailwind-sorter").setup({
                -- if this is enable, your undo history will be a bit fubar
                on_save_enabled = false,
            })
        end,
        keys = {
            -- { "<leader>qs", function() require("persistence").load() end, desc = "Restore Session" },
            { "<leader>bf", "<cmd>:TailwindSort<CR>", desc = "Sort TailwindCSS classes" },
        },
    },
}
