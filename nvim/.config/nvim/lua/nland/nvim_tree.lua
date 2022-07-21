-- examples for your init.lua
local status_ok, nvim_tree = pcall(require, "nvim-tree")
if not status_ok then
  return
end

-- OR setup with some options
nvim_tree.setup({
  sort_by = "case_sensitive",
  view = {
    adaptive_size = true,
    mappings = {
      list = {
        { key = "u", action = "dir_up" },
      },
    },
  },
  renderer = {
    group_empty = true,
  },
  respect_buf_cwd = true,
  filters = {
    dotfiles = false,
  },
})
