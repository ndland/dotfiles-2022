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

keymap('n', '<leader>ff', "<cmd>lua require'telescope.builtin'.find_files({ find_command = {'rg', '--files', '--hidden', '-g', '!.git' }})<cr>", opts)
keymap('n', '<leader>fb', "<cmd>lua require'telescope.builtin'.buffers({ show_all_buffers = true })<cr>", opts)
keymap('n', '<leader>fr', "<cmd>Telescope live_grep<cr>", opts)
keymap('n', '<leader>fm', "<cmd>Telescope marks<cr>", opts)
keymap('n', '<leader>fp', "<cmd>Telescope projects<cr>", opts)
keymap('n', '<leader>fg', "<cmd>lua require'telescope.builtin'.git_status()<cr>", opts)
keymap('n', '<leader>fk', "<cmd>lua require'telescope.builtin'.keymaps()<cr>", opts)
keymap('n', '<leader>f?', "<cmd> TodoTelescope<cr>", opts)
keymap("n", "<leader>g", "<cmd>lua _LAZYGIT_TOGGLE()<CR>", opts)
keymap("n", "<leader>t", ":NvimTreeToggle<CR>", opts)
keymap("n", "<leader>l", ":Format<CR>", opts)
