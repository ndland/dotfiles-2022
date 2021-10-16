local cmp = require 'cmp'

cmp.setup({
    snippet = {
        expand = function(args)
            vim.fn["UltiSnips#Anon"](args.body)
        end
    },
    mapping = {...},
    sources = {
        {
            name = 'nvim_lsp'
        }, {
            name = 'ultisnips'
        }, {
            name = 'buffer'
        }, {
            name = 'path'
        }
    }

})
