local status_ok, which_key = pcall(require, "which-key")
if not status_ok then
  return
end

which_key.setup({})

local opts = {
  mode = "n",
  prefix = ",",
  buffer = nil,
  silent = true,
  noremap = true,
  nowait = false,
}

which_key.register({
  f = {
    name = "file",
    b = { "<cmd>lua require('telescope.builtin').buffers({ show_all_buffers = true })<cr>", "Find Buffer" },
    f = {
      "<cmd>lua require('telescope.builtin').find_files({ find_command = {'rg', '--files', '--hidden', '-g', '!.git' }})<cr>",
      "Find File",
    },
    g = { "<cmd>lua require('telescope.builtin').git_status()<cr>", "Git Status" },
    k = { "<cmd>lua require('telescope.builtin').keymaps()<cr>", "Keymaps" },
    l = { "<cmd>lua vim.lsp.buf.format()<cr>", "Format" },
    o = { "<cmd>lua require('telescope.builtin').oldfiles()<cr>", "Old Files" },
    p = { "<cmd>Telescope projects<cr>", "Projects" },
    r = { "<cmd>lua require('telescope.builtin').live_grep()<cr>", "Ripgrep" },
    t = { "<cmd>TodoTelescope<cr>", "Todos" },
  },
  g = {
    name = "Git",
    s = { "<cmd>lua _LAZYGIT_TOGGLE()<cr>", "Status" },
  },
  t = {
    name = "Test",
    F = { "<cmd>lua require('neotest').run.run({vim.fn.expand('%'), strategy = 'dap'})<cr>", "Debug File" },
    L = { "<cmd>lua require('neotest').run.run_last({ strategy = 'dap' })<cr>", "Debug Last" },
    N = { "<cmd>lua require('neotest').run.run({strategy = 'dap'})<cr>", "Debug Nearest" },
    S = { "<cmd>lua require('neotest').run.stop()<cr>", "Stop" },
    a = { "<cmd>lua require('neotest').run.attach()<cr>", "Attach" },
    f = { "<cmd>lua require('neotest').run.run(vim.fn.expand('%'))<cr>", "Run File" },
    l = { "<cmd>lua require('neotest').run.run_last()<cr>", "Run Last" },
    n = { "<cmd>lua require('neotest').run.run()<cr>", "Run Nearest" },
    o = { "<cmd>lua require('neotest').output.open({ enter = true })<cr>", "Output" },
    s = { "<cmd>lua require('neotest').summary.toggle()<cr>", "Summary" },
  },
}, opts)
