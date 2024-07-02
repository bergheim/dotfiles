require("bergheim.remap")

vim.g.have_nerd_font = true

vim.opt.number = true
vim.opt.relativenumber = true

vim.opt.tabstop = 4 -- Number of spaces a <Tab> in the file counts for
vim.opt.shiftwidth = 4 -- Number of spaces to use for each step of (auto)indent
vim.opt.expandtab = true -- Use spaces instead of tabs

vim.opt.mouse = 'a'
vim.opt.showmode = false

-- Sync clipboard between OS and Neovim.
vim.opt.clipboard = "unnamedplus"

-- Enable break indent
vim.opt.breakindent = true

-- Save undo history
vim.opt.undofile = true

vim.opt.ignorecase = true
vim.opt.smartcase = true
-- Keep signcolumn on by default
vim.opt.signcolumn = "yes"

-- Decrease update time
vim.opt.updatetime = 250

-- Decrease mapped sequence wait time
-- Displays which-key popup sooner
vim.opt.timeoutlen = 300

-- Configure how new splits should be opened
vim.opt.splitright = true
vim.opt.splitbelow = true

-- Sets how neovim will display certain whitespace characters in the editor.
--  See `:help 'list'`
--  and `:help 'listchars'`
vim.opt.list = true
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }

-- Preview substitutions live, as you type!
vim.opt.inccommand = "split"

-- Show which line your cursor is on
vim.opt.cursorline = true

-- Minimal number of screen lines to keep above and below the cursor.
vim.opt.scrolloff = 10
-- Set highlight on search, but clear on pressing <Esc> in normal mode
vim.opt.hlsearch = true
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- Highlight when yanking (copying) text
--  Try it with `yap` in normal mode
--  See `:help vim.highlight.on_yank()`
vim.api.nvim_create_autocmd("TextYankPost", {
    desc = "Highlight when yanking (copying) text",
    group = vim.api.nvim_create_augroup("kickstart-highlight-yank", { clear = true }),
    callback = function()
        vim.highlight.on_yank()
    end,
})

-- vim.lsp.set_log_level("debug")

local function copy_current_file()
    local current_file = vim.fn.expand("%:p")
    if current_file == "" then
        print("No file to copy")
        return
    end

    local new_file = vim.fn.input("Copy to: ", current_file, "file")
    if new_file == "" then
        print("Copy cancelled")
        return
    end

    vim.fn.writefile(vim.fn.readfile(current_file), new_file)
    print("File copied to " .. new_file)
end

-- Keybinding to copy the current file
vim.keymap.set("n", "<leader>fc", copy_current_file, { desc = "Copy current file" })

local function delete_current_file()
  local file = vim.fn.expand("%")
  if file == "" then
    print("No file to delete")
    return
  end
  
  -- Ask for confirmation
  local response = vim.fn.confirm("Are you sure you want to delete " .. file .. "?", "&Yes\n&No", 2)
  
  if response == 1 then
    vim.fn.delete(file)
    vim.cmd("bdelete!")
    print("Deleted " .. file)
  else
    print("Deletion cancelled")
  end
end

-- Bind the function to <leader>fD
vim.keymap.set("n", "<leader>fD", delete_current_file, { desc = "Delete current file" })
