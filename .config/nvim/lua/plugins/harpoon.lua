return {
    {
        "ThePrimeagen/harpoon",
        branch = "harpoon2",
        dependencies = { "nvim-lua/plenary.nvim" },
        config = function()
            local harpoon = require("harpoon")
            harpoon:setup({})
            -- basic telescope configuration
            local conf = require("telescope.config").values
            local function toggle_telescope(harpoon_files)
                local file_paths = {}
                for _, item in ipairs(harpoon_files.items) do
                    table.insert(file_paths, item.value)
                end

                require("telescope.pickers")
                    .new({}, {
                        prompt_title = "Harpoon",
                        finder = require("telescope.finders").new_table({
                            results = file_paths,
                        }),
                        previewer = conf.file_previewer({}),
                        sorter = conf.generic_sorter({}),
                    })
                    :find()
            end

            vim.keymap.set("n", "<C-S-e>", function()
                toggle_telescope(harpoon:list())
            end, { desc = "Open harpoon window" })

            vim.keymap.set("n", "<C-e>", function()
                harpoon.ui:toggle_quick_menu(harpoon:list())
            end)

            vim.keymap.set("n", "<leader>ah", function()
                harpoon:list():add()
            end, { desc = "Add current file to harpoon" })

            vim.keymap.set("n", "<leader>aH", function()
                harpoon:list():remove()
            end, { desc = "Remove current file from harpoon" })

            vim.keymap.set("n", "<M-1>", function()
                harpoon:list():select(1)
            end)
            vim.keymap.set("n", "<M-2>", function()
                harpoon:list():select(2)
            end)
            vim.keymap.set("n", "<M-3>", function()
                harpoon:list():select(3)
            end)
            vim.keymap.set("n", "<M-4>", function()
                harpoon:list():select(4)
            end)

            -- Toggle previous & next buffers stored within Harpoon list
            vim.keymap.set("n", "<C-S-k>", function()
                harpoon:list():prev()
            end)
            vim.keymap.set("n", "<C-S-j>", function()
                harpoon:list():next()
            end)
        end,
    },
}
