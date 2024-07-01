vim.g.mapleader = " "
vim.g.maplocalleader = ","
vim.keymap.set("n", "<leader>fb", vim.cmd.Ex)
vim.keymap.set("n", "<leader>fs", vim.cmd.write, { desc = "Save current file" })

vim.keymap.set("n", "<leader>qq", vim.cmd.quitall, {})

-- window navigation and management
vim.keymap.set("n", "<leader>ws", vim.cmd.split, { desc = "Split window horizontally" })
vim.keymap.set("n", "<M-\\>", vim.cmd.vsplit, { desc = "Split window vertically" })

vim.keymap.set("n", "<leader>wv", vim.cmd.vsplit, { desc = "Split window vertically" })
-- vim.keymap.set("n", "<M-]>", vim.cmd.vsplit, { desc = "Split window vertically" })
vim.keymap.set("n", "<M-BS>", vim.cmd.close, { desc = "Delete current window" })
vim.keymap.set("n", "<leader>wd", vim.cmd.close, { desc = "Delete current window" })

-- Diagnostic keymaps
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Go to previous [D]iagnostic message" })
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Go to next [D]iagnostic message" })
vim.keymap.set("n", "<leader>ce", vim.diagnostic.open_float, { desc = "Show diagnostic [E]rror messages" })
vim.keymap.set("n", "<leader>cq", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })

--  Use CTRL+<hjkl> to switch between windows
--
--  See `:help wincmd` for a list of all window commands
vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Move focus to the left window" })
vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Move focus to the right window" })
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" })
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" })

vim.keymap.set('n', '<M-h>', '<C-w>h', { desc = 'Move to left window' })
vim.keymap.set('n', '<M-j>', '<C-w>j', { desc = 'Move to window below' })
vim.keymap.set('n', '<M-k>', '<C-w>k', { desc = 'Move to window above' })
vim.keymap.set('n', '<M-l>', '<C-w>l', { desc = 'Move to right window' })
