local function map(mode, lhs, rhs, opts)
    local options = {
        noremap = true
    }
    if opts then options = vim.tbl_extend('force', options, opts) end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

map('', '<leader>c', '"+y') -- Copy to clipboard in normal, visual, select and operator modes
map('i', '<C-u>', '<C-g>u<C-u>') -- Make <C-u> undo-friendly
map('i', '<C-w>', '<C-g>u<C-w>') -- Make <C-w> undo-friendly

map('n', '<C-l>', '<cmd>noh<CR>') -- Clear highlights
map('n', '<leader>o', 'm`o<Esc>``') -- Insert a newline in normal mode
map('n', '<leader>n', '<cmd>NERDTreeToggle<CR>') -- Insert a newline in normal mode

-- Buffer navigation
map('n', 'bn', '<cmd>bn<CR>') -- Insert a newline in normal mode
map('n', 'bp', '<cmd>bp<CR>') -- Insert a newline in normal mode

-- LSP mappings
map('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>')
map('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>')
map('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>')
map('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>')
map('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>')
map('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>')
map('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>')
map('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>')
map('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>')
map('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>')
map('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>')
map('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>')
map('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>')
map('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>')
map('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>')
map('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>')
map('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>')

-- Completion mappings
map('n', '<C-d>', 'cmp.mapping.scroll_docs(-4)')
map('n', '<C-f>', 'cmp.mapping.scroll_docs(4)')
map('n', '<C-Space>', 'cmp.mapping.complete()')
map('n', '<C-e>', 'cmp.mapping.close()')
map('n', '<CR>', 'cmp.mapping.confirm({ select = true })')
