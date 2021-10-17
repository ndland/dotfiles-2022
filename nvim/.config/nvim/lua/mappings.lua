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
map('n', 'bn', '<cmd>bn<CR>') -- Insert a newline in normal mode
map('n', 'bp', '<cmd>bp<CR>') -- Insert a newline in normal mode

