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
    f = {
      "<cmd>lua require('telescope.builtin').find_files({ find_command = {'rg', '--files', '--hidden', '-g', '!.git' }})<cr>",
      "Find File",
    },
    b = { "<cmd>lua require('telescope.builtin').buffers({ show_all_buffers = true })<cr>", "Find Buffer" },
    g = { "<cmd>lua require('telescope.builtin').git_status()<cr>", "Git Status" },
    p = { "<cmd>Telescope projects<cr>", "Projects" },
    t = { "<cmd>TodoTelescope<cr>", "Todos" },
    k = { "<cmd>lua require('telescope.builtin').keymaps()<cr>", "Keymaps" },
    l = { "<cmd>lua vim.lsp.buf.format()<cr>", "Format" },
    r = { "<cmd>lua require('telescope.builtin').live_grep()<cr>", "Ripgrep" },
  },
  g = {
    name = "Git",
    s = { "<cmd>lua _LAZYGIT_TOGGLE()<cr>", "Status" },
  },
  t = {
    name = "Test",
    a = { "<cmd>lua require('neotest').run.attach()<cr>", "Attach" },
    f = { "<cmd>lua require('neotest').run.run(vim.fn.expand('%'))<cr>", "Run File" },
    F = { "<cmd>lua require('neotest').run.run({vim.fn.expand('%'), strategy = 'dap'})<cr>", "Debug File" },
    l = { "<cmd>lua require('neotest').run.run_last()<cr>", "Run Last" },
    L = { "<cmd>lua require('neotest').run.run_last({ strategy = 'dap' })<cr>", "Debug Last" },
    n = { "<cmd>lua require('neotest').run.run()<cr>", "Run Nearest" },
    N = { "<cmd>lua require('neotest').run.run({strategy = 'dap'})<cr>", "Debug Nearest" },
    o = { "<cmd>lua require('neotest').output.open({ enter = true })<cr>", "Output" },
    S = { "<cmd>lua require('neotest').run.stop()<cr>", "Stop" },
    s = { "<cmd>lua require('neotest').summary.toggle()<cr>", "Summary" },
  },
}, opts)
