local cmp = require 'cmp'

cmp.setup({
    snippet = {
        expand = function(args)
            vim.fn["UltiSnips#Anon"](args.body)
        end
    },
    sources = {
        {
            name = 'nvim_lsp'
        }, {
            name = 'buffer'
        }, {
            name = 'path'
        }
    }

})
