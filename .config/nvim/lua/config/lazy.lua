-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
end
vim.opt.rtp:prepend(lazypath)

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Setup lazy.nvim
require("lazy").setup({
    spec = {
        -- import your plugins
        { import = "plugins" },
        { "editorconfig/editorconfig-vim" },
        "tpope/vim-sleuth", -- Detect tabstop and shiftwidth automatically
        {
            "nvim-telescope/telescope-file-browser.nvim",
            dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
            config = function()
                -- open file_browser with the path of the current buffer
                vim.keymap.set("n", "<space>ff", ":Telescope file_browser path=%:p:h select_buffer=true<CR>")
            end,
        },
        { "numToStr/Comment.nvim", opts = {} },

        {
            "kylechui/nvim-surround",
            version = "*", -- Use for stability; omit to use `main` branch for the latest features
            event = "VeryLazy",
            config = function()
                require("nvim-surround").setup({
                    -- Configuration here, or leave empty to use defaults
                })
            end,
        },
        -- themes
        { "catppuccin/nvim", name = "catppuccin", priority = 1000 },
        { "ellisonleao/gruvbox.nvim", priority = 1000, config = true, opts = {} },
        { "EdenEast/nightfox.nvim", priority = 1000 },
        { "sainnhe/everforest", priority = 1000 },
        { "NLKNguyen/papercolor-theme", priority = 1000 },
        { "vimpostor/vim-prism", priority = 1000 },

        -- Here is a more advanced example where we pass configuration
        -- options to `gitsigns.nvim`. This is equivalent to the following Lua:
        --    require('gitsigns').setup({ ... })
        --
        -- See `:help gitsigns` to understand what the configuration keys do
        { -- Adds git related signs to the gutter, as well as utilities for managing changes
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

        { -- Useful plugin to show you pending keybinds.
            "folke/which-key.nvim",
            event = "VimEnter", -- Sets the loading event to 'VimEnter'
            config = function() -- This is the function that runs, AFTER loading
                require("which-key").setup()

                -- Document existing key chains
                require("which-key").register({
                    ["<leader>c"] = { name = "Code", _ = "which_key_ignore" },
                    ["<leader>d"] = { name = "[D]ocument", _ = "which_key_ignore" },
                    -- ["<leader>r"] = { name = "[R]ename", _ = "which_key_ignore" },
                    ["<leader>s"] = { name = "Search", _ = "which_key_ignore" },
                    ["<leader>w"] = { name = "[W]orkspace", _ = "which_key_ignore" },
                    ["<leader>t"] = { name = "Toggle", _ = "which_key_ignore" },
                    ["<leader>g"] = { name = "Git", _ = "which_key_ignore" },
                })
                -- visual mode
                require("which-key").register({
                    ["<leader>gh"] = { "hunk" },
                }, { mode = "v" })
            end,
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

        -- {
        --    "hrsh7th/nvim-cmp",
        --    dependencies = {
        --       -- install different completion source
        --       "hrsh7th/cmp-nvim-lsp",
        --       "hrsh7th/cmp-buffer",
        --       "hrsh7th/cmp-path",
        --    },
        --    config = function()
        --       local cmp = require("cmp")
        --       cmp.setup({
        --             -- add different completion source
        --             sources = cmp.config.sources({
        --                   { name = "nvim_lsp" },
        --                   { name = "buffer" },
        --                   { name = "path" },
        --             }),
        --             -- using default mapping preset
        --             mapping = cmp.mapping.preset.insert({
        --                   ["<C-Space>"] = cmp.mapping.complete(),
        --                   ["<CR>"] = cmp.mapping.confirm({ select = true }),
        --             }),
        --             snippet = {
        --                -- you must specify a snippet engine
        --                expand = function(args)
        --                   -- using neovim v0.10 native snippet feature
        --                   -- you can also use other snippet engines
        --                   vim.snippet.expand(args.body)
        --                end,
        --             },
        --       })
        --    end,
        -- },

        { -- Autocompletion
            "hrsh7th/nvim-cmp",
            event = "InsertEnter",
            dependencies = {
                -- Snippet Engine & its associated nvim-cmp source
                {
                    "L3MON4D3/LuaSnip",
                    build = (function()
                        -- Build Step is needed for regex support in snippets.
                        -- This step is not supported in many windows environments.
                        -- Remove the below condition to re-enable on windows.
                        if vim.fn.has("win32") == 1 or vim.fn.executable("make") == 0 then
                            return
                        end
                        return "make install_jsregexp"
                    end)(),
                    dependencies = {
                        -- `friendly-snippets` contains a variety of premade snippets.
                        --    See the README about individual language/framework/plugin snippets:
                        --    https://github.com/rafamadriz/friendly-snippets
                        -- {
                        --   'rafamadriz/friendly-snippets',
                        --   config = function()
                        --     require('luasnip.loaders.from_vscode').lazy_load()
                        --   end,
                        -- },
                    },
                },
                "saadparwaiz1/cmp_luasnip",

                -- Adds other completion capabilities.
                --  nvim-cmp does not ship with all sources by default. They are split
                --  into multiple repos for maintenance purposes.
                "hrsh7th/cmp-nvim-lsp",
                "hrsh7th/cmp-buffer",
                "hrsh7th/cmp-path",
            },
            config = function()
                -- See `:help cmp`
                local cmp = require("cmp")
                local luasnip = require("luasnip")
                luasnip.config.setup({})

                cmp.setup({
                    snippet = {
                        expand = function(args)
                            luasnip.lsp_expand(args.body)
                            -- using neovim v0.10 native snippet feature
                            -- you can also use other snippet engines
                            -- vim.snippet.expand(args.body)
                        end,
                    },
                    completion = { completeopt = "menu,menuone,noinsert" },

                    -- For an understanding of why these mappings were
                    -- chosen, you will need to read `:help ins-completion`
                    --
                    -- No, but seriously. Please read `:help ins-completion`, it is really good!
                    mapping = cmp.mapping.preset.insert({
                        ["<C-n>"] = cmp.mapping.select_next_item(),
                        ["<C-p>"] = cmp.mapping.select_prev_item(),
                        ["<C-j>"] = cmp.mapping.select_next_item(),
                        ["<C-k>"] = cmp.mapping.select_prev_item(),

                        -- Scroll the documentation window [b]ack / [f]orward
                        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
                        ["<C-f>"] = cmp.mapping.scroll_docs(4),

                        -- Accept ([y]es) the completion.
                        --  This will auto-import if your LSP supports it.
                        --  This will expand snippets if the LSP sent a snippet.
                        -- ['<C-y>'] = cmp.mapping.confirm { select = true },

                        -- If you prefer more traditional completion keymaps,
                        -- you can uncomment the following lines
                        ["<CR>"] = cmp.mapping.confirm({ select = true }),
                        --['<Tab>'] = cmp.mapping.select_next_item(),
                        --['<S-Tab>'] = cmp.mapping.select_prev_item(),

                        -- Manually trigger a completion from nvim-cmp.
                        --  Generally you don't need this, because nvim-cmp will display
                        --  completions whenever it has completion options available.
                        ["<C-Space>"] = cmp.mapping.complete({}),

                        -- Think of <c-l> as moving to the right of your snippet expansion.
                        --  So if you have a snippet that's like:
                        --  function $name($args)
                        --    $body
                        --  end
                        --
                        -- <c-l> will move you to the right of each of the expansion locations.
                        -- <c-h> is similar, except moving you backwards.
                        ["<C-l>"] = cmp.mapping(function()
                            if luasnip.expand_or_locally_jumpable() then
                                luasnip.expand_or_jump()
                            end
                        end, { "i", "s" }),
                        ["<C-h>"] = cmp.mapping(function()
                            if luasnip.locally_jumpable(-1) then
                                luasnip.jump(-1)
                            end
                        end, { "i", "s" }),

                        -- For more advanced Luasnip keymaps (e.g. selecting choice nodes, expansion) see:
                        --    https://github.com/L3MON4D3/LuaSnip?tab=readme-ov-file#keymaps
                    }),
                    sources = {
                        { name = "nvim_lsp" },
                        { name = "luasnip" }, -- snippets
                        { name = "buffer" },
                        { name = "path" },
                    },
                })
            end,
        },

        { -- LSP Configuration & Plugins
            "neovim/nvim-lspconfig",
            dependencies = {
                -- Automatically install LSPs and related tools to stdpath for Neovim
                { "williamboman/mason.nvim", config = true }, -- NOTE: Must be loaded before dependants
                "williamboman/mason-lspconfig.nvim",
                "WhoIsSethDaniel/mason-tool-installer.nvim",

                -- Useful status updates for LSP.
                -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
                { "j-hui/fidget.nvim", opts = {} },

                -- `neodev` configures Lua LSP for your Neovim config, runtime and plugins
                -- used for completion, annotations and signatures of Neovim apis
                { "folke/neodev.nvim", opts = {} },
            },
            config = function()
                vim.api.nvim_create_autocmd("LspAttach", {
                    group = vim.api.nvim_create_augroup("kickstart-lsp-attach", { clear = true }),
                    callback = function(event)
                        -- NOTE: Remember that Lua is a real programming language, and as such it is possible
                        -- to define small helper and utility functions so you don't have to repeat yourself.
                        --
                        -- In this case, we create a function that lets us more easily define mappings specific
                        -- for LSP related items. It sets the mode, buffer and description for us each time.
                        local map = function(keys, func, desc)
                            vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
                        end

                        -- Jump to the definition of the word under your cursor.
                        --  This is where a variable was first declared, or where a function is defined, etc.
                        --  To jump back, press <C-t>.
                        map("gd", require("telescope.builtin").lsp_definitions, "[G]oto [D]efinition")

                        -- Find references for the word under your cursor.
                        map("gr", require("telescope.builtin").lsp_references, "[G]oto [R]eferences")

                        -- Jump to the implementation of the word under your cursor.
                        --  Useful when your language has ways of declaring types without an actual implementation.
                        map("gI", require("telescope.builtin").lsp_implementations, "[G]oto [I]mplementation")

                        -- Jump to the type of the word under your cursor.
                        --  Useful when you're not sure what type a variable is and you want to see
                        --  the definition of its *type*, not where it was *defined*.
                        map("<leader>cD", require("telescope.builtin").lsp_type_definitions, "Type [D]efinition")

                        -- Fuzzy find all the symbols in your current document.
                        --  Symbols are things like variables, functions, types, etc.
                        map("<leader>ds", require("telescope.builtin").lsp_document_symbols, "[D]ocument [S]ymbols")
                        map("<leader>si", require("telescope.builtin").lsp_document_symbols, "Symbols")

                        -- Fuzzy find all the symbols in your current workspace.
                        --  Similar to document symbols, except searches over your entire project.
                        map("<leader>sI", require("telescope.builtin").lsp_dynamic_workspace_symbols, "Project symbols")

                        -- Rename the variable under your cursor.
                        --  Most Language Servers support renaming across files, etc.
                        map("<leader>cR", vim.lsp.buf.rename, "Rename")

                        -- Execute a code action, usually your cursor needs to be on top of an error
                        -- or a suggestion from your LSP for this to activate.
                        map("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction")

                        -- Opens a popup that displays documentation about the word under your cursor
                        --  See `:help K` for why this keymap.
                        map("K", vim.lsp.buf.hover, "Hover Documentation")

                        -- WARN: This is not Goto Definition, this is Goto Declaration.
                        --  For example, in C this would take you to the header.
                        map("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")

                        -- The following two autocommands are used to highlight references of the
                        -- word under your cursor when your cursor rests there for a little while.
                        --    See `:help CursorHold` for information about when this is executed
                        --
                        -- When you move your cursor, the highlights will be cleared (the second autocommand).
                        local client = vim.lsp.get_client_by_id(event.data.client_id)
                        if client and client.server_capabilities.documentHighlightProvider then
                            local highlight_augroup =
                                vim.api.nvim_create_augroup("kickstart-lsp-highlight", { clear = false })
                            vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
                                buffer = event.buf,
                                group = highlight_augroup,
                                callback = vim.lsp.buf.document_highlight,
                            })

                            vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
                                buffer = event.buf,
                                group = highlight_augroup,
                                callback = vim.lsp.buf.clear_references,
                            })

                            vim.api.nvim_create_autocmd("LspDetach", {
                                group = vim.api.nvim_create_augroup("kickstart-lsp-detach", { clear = true }),
                                callback = function(event2)
                                    vim.lsp.buf.clear_references()
                                    vim.api.nvim_clear_autocmds({
                                        group = "kickstart-lsp-highlight",
                                        buffer = event2.buf,
                                    })
                                end,
                            })
                        end

                        -- The following autocommand is used to enable inlay hints in your
                        -- code, if the language server you are using supports them
                        --
                        -- This may be unwanted, since they displace some of your code
                        if client and client.server_capabilities.inlayHintProvider and vim.lsp.inlay_hint then
                            map("<leader>th", function()
                                vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
                            end, "[T]oggle Inlay [H]ints")
                        end
                    end,
                })

                -- LSP servers and clients are able to communicate to each other what features they support.
                --  By default, Neovim doesn't support everything that is in the LSP specification.
                --  When you add nvim-cmp, luasnip, etc. Neovim now has *more* capabilities.
                --  So, we create new capabilities with nvim cmp, and then broadcast that to the servers.
                local capabilities = vim.lsp.protocol.make_client_capabilities()
                capabilities =
                    vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())

                -- Enable the following language servers
                --  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
                --
                --  Add any additional override configuration in the following tables. Available keys are:
                --  - cmd (table): Override the default command used to start the server
                --  - filetypes (table): Override the default list of associated filetypes for the server
                --  - capabilities (table): Override fields in capabilities. Can be used to disable certain LSP features.
                --  - settings (table): Override the default settings passed when initializing the server.
                --        For example, to see the options for `lua_ls`, you could go to: https://luals.github.io/wiki/settings/
                local servers = {
                    -- See `:help lspconfig-all` for a list of all the pre-configured LSPs
                    --
                    -- Some languages (like typescript) have entire language plugins that can be useful:
                    --    https://github.com/pmizio/typescript-tools.nvim
                    --
                    -- But for many setups, the LSP (`tsserver`) will work just fine
                    -- tsserver = {},
                    --

                    elixirls = {
                        cmd = { "elixir-ls" },
                        filetypes = { "elixir" },
                        capabilities = {
                            textDocument = {
                                completion = {
                                    completionItem = {
                                        snippetSupport = true,
                                    },
                                },
                            },
                        },
                    },

                    html = { filetypes = { "html", "twig", "hbs", "heex", "templ" } },

                    tailwindcss = {
                        init_options = {
                            userLanguages = {
                                elixir = "html-eex",
                                eelixir = "html-eex",
                                heex = "html-eex",
                            },
                        },
                        -- opts = {
                        --    settings = {
                        --       tailwindCSS = {
                        --          classAttributes = { 'class' , 'className' , 'classList' },
                        --          experimental = {
                        --             classRegex = {
                        --                'class[:]\\s*"([^"]*)"',
                        --             },
                        --          },
                        --       }
                        --    }
                        -- }
                    },

                    lua_ls = {
                        -- cmd = {...},
                        -- filetypes = { ...},
                        -- capabilities = {},
                        settings = {
                            Lua = {
                                completion = {
                                    callSnippet = "Replace",
                                },
                                -- You can toggle below to ignore Lua_LS's noisy `missing-fields` warnings
                                -- diagnostics = { disable = { 'missing-fields' } },
                            },
                        },
                    },
                }

                require("mason").setup()

                local ensure_installed = vim.tbl_keys(servers or {})
                vim.list_extend(ensure_installed, {
                    -- TODO: unpin this once the latest version > 0.0.18 works on .heex files again
                    { "tailwindcss-language-server", version = "0.0.16" },
                    "prettierd",
                    "stylua", -- Used to format Lua code
                })
                require("mason-tool-installer").setup({
                    ensure_installed = ensure_installed,
                    -- auto_update = true,
                })

                require("mason-lspconfig").setup({
                    handlers = {
                        function(server_name)
                            local server = servers[server_name] or {}
                            -- This handles overriding only values explicitly passed
                            -- by the server configuration above. Useful when disabling
                            -- certain features of an LSP (for example, turning off formatting for tsserver)
                            server.capabilities =
                                vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
                            require("lspconfig")[server_name].setup(server)
                        end,
                    },
                })
            end,
        },

        -- error listing, diagnostics..
        {
            "folke/trouble.nvim",
            opts = {
                warn_no_results = false,
                open_no_results = true,
            },
            cmd = "Trouble",
            keys = {
                {
                    "<leader>xx",
                    "<cmd>Trouble diagnostics toggle<cr>",
                    desc = "Diagnostics (Trouble)",
                },
                {
                    "<leader>xX",
                    "<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
                    desc = "Buffer Diagnostics (Trouble)",
                },
                {
                    "<leader>cs",
                    "<cmd>Trouble symbols toggle focus=false<cr>",
                    desc = "Symbols (Trouble)",
                },
                {
                    "<leader>cl",
                    "<cmd>Trouble lsp toggle focus=false win.position=right<cr>",
                    desc = "LSP Definitions / references / ... (Trouble)",
                },
                {
                    "<leader>xL",
                    "<cmd>Trouble loclist toggle<cr>",
                    desc = "Location List (Trouble)",
                },
                {
                    "<leader>xQ",
                    "<cmd>Trouble qflist toggle<cr>",
                    desc = "Quickfix List (Trouble)",
                },
            },
        },

        { -- Autoformat
            "stevearc/conform.nvim",
            lazy = false,
            keys = {
                {
                    "<leader>cf",
                    function()
                        require("conform").format({ async = true, lsp_fallback = true })
                    end,
                    mode = "",
                    desc = "[F]ormat buffer",
                },
            },
            opts = {
                notify_on_error = false,
                format_on_save = function(bufnr)
                    -- Disable "format_on_save lsp_fallback" for languages that don't
                    -- have a well standardized coding style. You can add additional
                    -- languages here or re-enable it for the disabled ones.
                    local disable_filetypes = { c = true, cpp = true }
                    return {
                        timeout_ms = 500,
                        lsp_fallback = not disable_filetypes[vim.bo[bufnr].filetype],
                    }
                end,
                formatters_by_ft = {
                   bash = { "shfmt" },
                   sh = { "shfmt" },
                    lua = { "stylua" },
                    python = { "isort", "black" },
                    javascript = { { "prettierd", "prettier" } },
                    typescript = { { "prettierd", "prettier" } },
                    javascriptreact = { { "prettierd", "prettier" } },
                    typescriptreact = { { "prettierd", "prettier" } },
                    vue = { { "prettierd", "prettier" } },
                    css = { { "prettierd", "prettier" } },
                    scss = { { "prettierd", "prettier" } },
                    less = { { "prettierd", "prettier" } },
                    html = { { "prettierd", "prettier" } },
                    json = { { "prettierd", "prettier" } },
                    jsonc = { { "prettierd", "prettier" } },
                    yaml = { { "prettierd", "prettier" } },
                    markdown = { { "prettierd", "prettier" } },
                    rust = { "rustfmt" },
                },
            },
        },

        -- Highlight todo, notes, etc in comments
        {
            "folke/todo-comments.nvim",
            event = "VimEnter",
            dependencies = { "nvim-lua/plenary.nvim" },
            opts = { signs = false },
            vim.keymap.set("n", "<leader>st", vim.cmd.TodoTelescope, { desc = "Search TODOs" }),
        },

        -- file navigator
        {
            "stevearc/oil.nvim",
            opts = {},
            -- Optional dependencies
            dependencies = { "nvim-tree/nvim-web-devicons" },
            config = function()
                require("oil").setup({
                    -- Oil will take over directory buffers (e.g. `vim .` or `:e src/`)
                    -- Set to false if you want some other plugin (e.g. netrw) to open when you edit directories.
                    default_file_explorer = true,
                    -- Id is automatically added at the beginning, and name at the end
                    -- See :help oil-columns
                    columns = {
                        "icon",
                        -- "permissions",
                        -- "size",
                        -- "mtime",
                    },

                    vim.keymap.set("n", "<leader>fb", vim.cmd.Oil, { desc = "Find file" }),
                    keymaps = {
                        ["g?"] = "actions.show_help",
                        ["<CR>"] = "actions.select",
                        ["<C-s>"] = {
                            "actions.select",
                            opts = { vertical = true },
                            desc = "Open the entry in a vertical split",
                        },
                        ["<C-h>"] = {
                            "actions.select",
                            opts = { horizontal = true },
                            desc = "Open the entry in a horizontal split",
                        },
                        ["<C-t>"] = { "actions.select", opts = { tab = true }, desc = "Open the entry in new tab" },
                        ["<C-p>"] = "actions.preview",
                        ["<C-c>"] = "actions.close",
                        ["<C-l>"] = "actions.refresh",
                        ["-"] = "actions.parent",
                        ["H"] = "actions.parent",
                        ["_"] = "actions.open_cwd",
                        ["`"] = "actions.cd",
                        ["~"] = { "actions.cd", opts = { scope = "tab" }, desc = ":tcd to the current oil directory" },
                        ["gs"] = "actions.change_sort",
                        ["gx"] = "actions.open_external",
                        ["g."] = "actions.toggle_hidden",
                        ["g\\"] = "actions.toggle_trash",
                    },
                })
            end,
        },

        {
            "zbirenbaum/copilot.lua",
            cmd = "Copilot",
            event = "InsertEnter",
            config = function()
                require("copilot").setup({
                    panel = {
                        enabled = true,
                        auto_refresh = false,
                        keymap = {
                            jump_prev = "[[",
                            jump_next = "]]",
                            accept = "<CR>",
                            refresh = "gr",
                            open = "<M-CR>",
                        },
                        layout = {
                            position = "bottom", -- | top | left | right
                            ratio = 0.4,
                        },
                    },
                    suggestion = {
                        enabled = true,
                        auto_trigger = true,
                        hide_during_completion = true,
                        debounce = 75,
                        keymap = {
                            accept = "<M-l>",
                            accept_word = false,
                            accept_line = false,
                            next = "<M-]>",
                            prev = "<M-[>",
                            dismiss = "<C-]>",
                        },
                    },
                    filetypes = {
                        yaml = false,
                        markdown = false,
                        help = false,
                        gitcommit = false,
                        gitrebase = false,
                        hgcommit = false,
                        svn = false,
                        cvs = false,
                        ["."] = false,
                    },
                    copilot_node_command = "node", -- Node.js version must be > 18.x
                    server_opts_overrides = {},
                })
            end,
        },

        -- {
        --    "zbirenbaum/copilot-cmp",
        --    config = function ()
        --       require("copilot_cmp").setup()
        --    end
        -- },

        { -- Collection of various small independent plugins/modules
            "echasnovski/mini.nvim",
            config = function()
                -- Better Around/Inside textobjects
                --
                -- Examples:
                --  - va)  - [V]isually select [A]round [)]paren
                --  - yinq - [Y]ank [I]nside [N]ext [']quote
                --  - ci'  - [C]hange [I]nside [']quote
                require("mini.ai").setup({ n_lines = 500 })

                -- Add/delete/replace surroundings (brackets, quotes, etc.)
                --
                -- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
                -- - sd'   - [S]urround [D]elete [']quotes
                -- - sr)'  - [S]urround [R]eplace [)] [']
                require("mini.surround").setup()

                -- Simple and easy statusline.
                --  You could remove this setup call if you don't like it,
                --  and try some other statusline plugin
                local statusline = require("mini.statusline")
                -- set use_icons to true if you have a Nerd Font
                statusline.setup({ use_icons = vim.g.have_nerd_font })

                -- You can configure sections in the statusline by overriding their
                -- default behavior. For example, here we set the section for
                -- cursor location to LINE:COLUMN
                ---@diagnostic disable-next-line: duplicate-set-field
                statusline.section_location = function()
                    return "%2l:%-2v"
                end

                -- ... and there is more!
                --  Check out: https://github.com/echasnovski/mini.nvim
            end,
        },

        -- {
        --    "mfussenegger/nvim-lint",
        --    event = {
        --       "BufReadPre",
        --       "BufNewFile",
        --    },
        --    config = function()
        --       local lint = require("lint")

        --       lint.linters_by_ft = {
        --          -- FIXME: this does not work. See https://github.com/LazyVim/LazyVim/issues/3808 for a relevant discussion
        --          elixir = { "credo "},
        --          javascript = { "eslint_d" },
        --          typescript = { "eslint_d" },
        --          javascriptreact = { "eslint_d" },
        --          typescriptreact = { "eslint_d" },
        --          svelte = { "eslint_d" },
        --       }

        --       local lint_augroup = vim.api.nvim_create_augroup("lint", { clear = true })

        --       vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "InsertLeave" }, {
        --             group = lint_augroup,
        --             callback = function()
        --                lint.try_lint()
        --             end,
        --       })

        --       vim.keymap.set("n", "<leader>ll", function()
        --                         lint.try_lint()
        --       end, { desc = "Trigger linting for current file" })
        --    end,
        -- },

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
                        enable = false,
                        settings = elixirls.settings({
                            dialyzerEnabled = false,
                            enableTestLenses = false,
                        }),
                        on_attach = function(client, bufnr)
                            vim.keymap.set("n", "<space>fp", ":ElixirFromPipe<cr>", { buffer = true, noremap = true })
                            vim.keymap.set("n", "<space>tp", ":ElixirToPipe<cr>", { buffer = true, noremap = true })
                            vim.keymap.set(
                                "v",
                                "<space>em",
                                ":ElixirExpandMacro<cr>",
                                { buffer = true, noremap = true }
                            )
                        end,
                    },
                })
            end,
            dependencies = {
                "nvim-lua/plenary.nvim",
            },
        },
    },

    -- Configure any other settings here. See the documentation for more details.
    -- colorscheme that will be used when installing plugins.
    install = { colorscheme = { "habamax" } },
    -- automatically check for plugin updates
    checker = { enabled = true },
})
