local opts = { noremap = true, silent = true }

local keymap = vim.api.nvim_set_keymap

-- Remap ',' as leader
keymap("", ",", "<Nop>", opts)
vim.g.mapleader =","
vim.g.maplocalleader =","

keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- Press 'jj' to escape in insert mode
keymap("i", "jj", "<ESC>", opts)

keymap('n', '<leader>f', "<cmd>lua require'telescope.builtin'.find_files({ find_command = {'rg', '--files', '--hidden', '-g', '!.git' }})<cr>", opts)
keymap('n', '<leader>b', "<cmd>lua require'telescope.builtin'.buffers({ show_all_buffers = true })<cr>", opts)
keymap('n', '<leader>g', "<cmd>lua require'telescope.builtin'.live_grep<cr>", opts)
keymap('n', '<leader>p', "<cmd>lua require'telescope.builtin'.projects<cr>", opts)
keymap('n', '<leader>g', "<cmd>lua require'telescope.builtin'.git_status()<cr>", opts)
keymap('n', '<leader>k', "<cmd>lua require'telescope.builtin'.keymaps()<cr>", opts)
keymap('n', '<leader>?', ":TodoTelescope<cr>", opts)
keymap("n", "<C-g>", "<cmd>lua _LAZYGIT_TOGGLE()<CR>", opts)
keymap("n", "<leader>t", ":NvimTreeToggle<CR>", opts)
keymap("n", "<leader>l", ":Format<CR>", opts)
