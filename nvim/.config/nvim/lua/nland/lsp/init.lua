local status_ok, lspconfig = pcall(require, "lspconfig")
if not status_ok then
  return
end

lspconfig.jsonls.setup({
  settings = {
    json = {
      schemas = require("schemastore").json.schemas(),
      validate = { enable = true },
    },
  },
})

require("nland.lsp.lsp-installer")
require("nland.lsp.lsp-saga")
require("nland.lsp.handlers")
