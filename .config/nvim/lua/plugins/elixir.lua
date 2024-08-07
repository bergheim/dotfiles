return {
    {
        "elixir-tools/elixir-tools.nvim",
        version = "*",
        event = { "BufReadPre", "BufNewFile" },
        config = function()
            local elixir = require("elixir")
            local elixirls = require("elixir.elixirls")

            elixir.setup({
                nextls = { enable = false },
                credo = {},
                elixirls = {
                    enable = true,
                    settings = elixirls.settings({
                        dialyzerEnabled = true,
                        fetchDeps = false,
                        enableTestLenses = false,
                        suggestSpecs = false,
                    }),
                    on_attach = function(client, bufnr)
                        vim.keymap.set("n", "<space>fp", ":ElixirFromPipe<cr>", { buffer = true, noremap = true })
                        vim.keymap.set("n", "<space>tp", ":ElixirToPipe<cr>", { buffer = true, noremap = true })
                        vim.keymap.set("v", "<space>em", ":ElixirExpandMacro<cr>", { buffer = true, noremap = true })
                    end,
                },
            })
        end,
        dependencies = {
            "nvim-lua/plenary.nvim",
        },
    },
    {
        "emmanueltouzery/elixir-extras.nvim",
        keys = {
            -- TODO should use localleader here
            {
                "<leader>sE",
                function()
                    require("elixir-extras").elixir_view_docs({ include_mix_libs = true })
                end,
                desc = "Search Elixir Docs",
            },
            {
                "<leader>cE",
                function()
                    require("elixir-extras").module_complete()
                end,
                desc = "Complete module",
            },
        },
    },
}
