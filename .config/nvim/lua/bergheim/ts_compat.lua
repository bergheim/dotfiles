-- Compatibility shim: nvim-treesitter `master` branch on Neovim 0.12.
--
-- master registers its query handlers with the legacy `all = false` option
-- (lua/nvim-treesitter/query_predicates.lua), which 0.12 removed. Handlers now
-- always receive `match[capture_id]` as a *list* of TSNodes, but master's
-- handlers still treat it as a single node, so handing it to get_node_text()
-- dies inside vim.treesitter.get_range():
--
--   Decoration provider "start" (ns=nvim.treesitter.highlighter):
--   .../treesitter.lua:197: attempt to call method 'range' (a nil value)
--
-- Triggered by #downcase! (bash, ruby injections), #set-lang-from-info-string!
-- (markdown code blocks) and #set-lang-from-mimetype! (html script tags).
--
-- Delete this file once the config moves to nvim-treesitter's `main` branch.

local M = {}

local html_script_type_languages = {
    ["importmap"] = "json",
    ["module"] = "javascript",
    ["application/ecmascript"] = "javascript",
    ["text/ecmascript"] = "javascript",
}

local non_filetype_match_injection_language_aliases = {
    ex = "elixir",
    pl = "perl",
    sh = "bash",
    uxn = "uxntal",
    ts = "typescript",
}

-- 0.12 passes a list of nodes per capture; take the first, which is what
-- Neovim's own builtin directives (#gsub!, #trim!) do.
local function unwrap(nodes)
    if type(nodes) == "table" then
        return nodes[1]
    end
    return nodes
end

local function node_at(match, id)
    return unwrap(match[id])
end

-- Same removal, second victim: nvim-treesitter/query.lua:251 asks for
-- `{ all = false }` matches, which 0.12 ignores, so every capture in a prepared
-- match is a list of nodes rather than a node. Textobjects motions (]] , [[ ,
-- af/if, ...) then die in move.lua with the same 'range' error. Re-implement the
-- iterator, unwrapping as we go; otherwise identical to the original.
local function patch_iter_prepared_matches()
    local ts_query = require("nvim-treesitter.query")
    local tsrange = require("nvim-treesitter.tsrange")

    local function split(to_split)
        local t = {}
        for str in string.gmatch(to_split, "([^.]+)") do
            table.insert(t, str)
        end
        return t
    end

    ts_query.iter_prepared_matches = function(query, qnode, bufnr, start_row, end_row)
        local matches = query:iter_matches(qnode, bufnr, start_row, end_row)

        return function()
            local pattern, match, metadata = matches()
            if pattern == nil then
                return
            end

            local prepared_match = {}

            for id, nodes in pairs(match) do
                local name = query.captures[id] -- name of the capture in the query
                if name ~= nil then
                    ts_query.insert_to_path(prepared_match, split(name .. ".node"), unwrap(nodes))
                    ts_query.insert_to_path(prepared_match, split(name .. ".metadata"), metadata[id])
                end
            end

            local preds = query.info.patterns[pattern]
            if preds then
                for _, pred in pairs(preds) do
                    if pred[1] == "set!" and type(pred[2]) == "string" then
                        ts_query.insert_to_path(prepared_match, split(pred[2]), pred[3])
                    end
                    if pred[1] == "make-range!" and type(pred[2]) == "string" and #pred == 4 then
                        ts_query.insert_to_path(
                            prepared_match,
                            split(pred[2] .. ".node"),
                            tsrange.TSRange.from_nodes(bufnr, unwrap(match[pred[3]]), unwrap(match[pred[4]]))
                        )
                    end
                end
            end

            return prepared_match
        end
    end
end

function M.setup()
    -- On 0.11 master works as designed; leave it alone.
    if vim.fn.has("nvim-0.12") == 0 then
        return
    end

    -- make sure master has registered its handlers before we override them
    require("nvim-treesitter.query_predicates")

    patch_iter_prepared_matches()

    local query = require("vim.treesitter.query")
    local get_node_text = vim.treesitter.get_node_text
    local force = { force = true }

    query.add_directive("downcase!", function(match, _, bufnr, pred, metadata)
        local id = pred[2]
        local node = node_at(match, id)
        if not node then
            return
        end

        local text = get_node_text(node, bufnr, { metadata = metadata[id] }) or ""
        metadata[id] = metadata[id] or {}
        metadata[id].text = text:lower()
    end, force)

    query.add_directive("set-lang-from-mimetype!", function(match, _, bufnr, pred, metadata)
        local node = node_at(match, pred[2])
        if not node then
            return
        end

        local mimetype = get_node_text(node, bufnr)
        local configured = html_script_type_languages[mimetype]
        if configured then
            metadata["injection.language"] = configured
        else
            local parts = vim.split(mimetype, "/", {})
            metadata["injection.language"] = parts[#parts]
        end
    end, force)

    query.add_directive("set-lang-from-info-string!", function(match, _, bufnr, pred, metadata)
        local node = node_at(match, pred[2])
        if not node then
            return
        end

        local alias = get_node_text(node, bufnr):lower()
        metadata["injection.language"] = vim.filetype.match({ filename = "a." .. alias })
            or non_filetype_match_injection_language_aliases[alias]
            or alias
    end, force)

    query.add_predicate("nth?", function(match, _, _, pred)
        local node = node_at(match, pred[2])
        local n = tonumber(pred[3])
        local parent = node and n and node:parent()
        if parent and parent:named_child_count() > n then
            return parent:named_child(n) == node
        end
        return false
    end, force)

    query.add_predicate("is?", function(match, _, bufnr, pred)
        local node = node_at(match, pred[2])
        if not node then
            return true
        end

        -- required lazily to avoid a circular dependency
        local _, _, kind = require("nvim-treesitter.locals").find_definition(node, bufnr)
        return vim.tbl_contains({ unpack(pred, 3) }, kind)
    end, force)

    query.add_predicate("kind-eq?", function(match, _, _, pred)
        local node = node_at(match, pred[2])
        if not node then
            return true
        end
        return vim.tbl_contains({ unpack(pred, 3) }, node:type())
    end, force)
end

return M
