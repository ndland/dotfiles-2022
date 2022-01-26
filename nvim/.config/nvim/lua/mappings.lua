local map = vim.api.nvim_set_keymap

map('n', '<leader>t', ':NvimTreeToggle<CR>', {})

map('n', '<leader>fo', ':Neoformat<CR>', {})

-- FZF mappings
map('n', '<leader>b', ':Buffers<CR>', {})
map('n', '<leader>fi', ':Files<CR>', {})
