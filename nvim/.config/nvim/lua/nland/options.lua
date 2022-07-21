local options = {
  autoindent = true,
  autochdir = true,
  backup = false,
  clipboard = "unnamedplus",
  completeopt = { "menuone", "noselect" },
  expandtab = true,
  number = true,
  numberwidth = 4,
  relativenumber = true,
  shiftwidth = 2,
  smartindent = true,
  swapfile = false,
  tabstop = 2,
  undofile = true,
  updatetime = 300,
  writebackup = false,
}

for k, v in pairs(options) do
  vim.opt[k] = v
end
