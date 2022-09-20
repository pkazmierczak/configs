"----------------------------------------------
" Plugin management
" Download vim-plug from the URL below and follow the installation
" instructions:
" https://github.com/junegunn/vim-plug
"----------------------------------------------
call plug#begin('~/.vim/plugged')

" Dependencies
Plug 'tpope/vim-rhubarb'           " Depenency for tpope/fugitive
Plug 'MunifTanjim/nui.nvim'        " dependency for neo-tree

" LSP
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/nvim-cmp'
Plug 'kyazdani42/nvim-web-devicons'

" Telescope
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

" General
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'enricobacis/vim-airline-clock'
Plug 'nvim-neo-tree/neo-tree.nvim'
Plug 'mhinz/vim-signify'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'        " for bracket mappings
Plug 'rbgrouleff/bclose.vim'
Plug 'kshenoy/vim-signature'       " show marks in the gutter
Plug 'ruanyl/vim-gh-line'          " copies gh url of the current line
Plug 'APZelos/blamer.nvim'

" Language support
Plug 'fatih/vim-go', { 'do': ':silent :GoUpdateBinaries' }
Plug 'fatih/vim-hclfmt'
Plug 'jvirtanen/vim-hcl'
Plug 'hashivim/vim-terraform'

" Colorschemes
Plug 'iCyMind/NeoSolarized'
Plug 'sainnhe/gruvbox-material'
call plug#end()

"----------------------------------------------
" General settings
"----------------------------------------------
set autoindent                    " take indent for new line from previous line
set smartindent                   " enable smart indentation
set autoread                      " reload file if the file changes on the disk
set autowrite                     " write when switching buffers
set autowriteall                  " write on :quit
set clipboard=unnamedplus
set colorcolumn=101               " highlight the 80th column as an indicator
set cursorline                    " highlight the current line for the cursor
set encoding=utf-8
set expandtab                     " expands tabs to spaces
set nospell                       " disable spelling
set noswapfile                    " disable swapfile usage
set wrap
set noerrorbells                  " No bells!
set novisualbell                  " I said, no bells!
set number                        " show number ruler
set relativenumber                " show relative numbers in the ruler
set ruler
set formatoptions=tcqronj         " set vims text formatting options
set softtabstop=2
set tabstop=2
set title                         " let vim set the terminal title
set updatetime=100                " redraw the status bar often
set so=5
set completeopt=menu,menuone,noselect

" neovim specific settings
if has('nvim')
    if has('mac')
        let g:python_host_prog = '/usr/local/python2'
        let g:python3_host_prog = '/usr/local/bin/python3'
    else
        let g:python_host_prog = '/usr/bin/python2'
        let g:python3_host_prog = '/usr/bin/python3'
    endif
endif

" Enable mouse if possible
if has('mouse')
    set mouse=a
endif

" Allow vim to set a custom font or color for a word
syntax enable

" Set the leader button
let mapleader = ','

" Autosave buffers before leaving them
autocmd BufLeave * silent! :wa

" Remove trailing white spaces on save
autocmd BufWritePre * :%s/\s\+$//e

" Enable blamer
let g:blamer_enabled = 1
let g:blamer_relative_time = 1
let g:blamer_template = '<committer> wrote that underwhelming piece of code <committer-time>'

"----------------------------------------------
" Colors
"----------------------------------------------

set background=light
set termguicolors
colo NeoSolarized

function! Switch_background()
    if &background ==? 'dark'
        set background=light
        colo NeoSolarized
        let g:airline_theme='solarized'
        AirlineRefresh
    else
        set background=dark
        let g:gruvbox_material_background = 'soft'
        colo gruvbox-material
        let g:airline_theme='gruvbox_material'
        AirlineRefresh
    endif
endfunction
command! SwitchBackground call Switch_background()


" Toggle background with <leader>bg
map <leader>bg :SwitchBackground<cr>

"----------------------------------------------
" Searching
"----------------------------------------------
set incsearch                     " move to match as you type the search query
set hlsearch                      " disable search result highlighting

if has('nvim')
    set inccommand=split          " enables interactive search and replace
endif

" Clear search highlights
map <leader>c :nohlsearch<cr>

"----------------------------------------------
" Navigation
"----------------------------------------------
" Disable arrow keys
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" ... but skip the quickfix when navigating
augroup qf
    autocmd!
    autocmd FileType qf set nobuflisted
augroup END

" Fix some common typos
cnoreabbrev W! w!
cnoreabbrev Q! q!
cnoreabbrev Qall! qall!
cnoreabbrev Wq wq
cnoreabbrev Wa wa
cnoreabbrev wQ wq
cnoreabbrev WQ wq
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Qall qall

"----------------------------------------------
" Splits
"----------------------------------------------
" Create horizontal splits below the current window
set splitbelow
set splitright

" Creating splits
nnoremap <leader>v :vsplit<cr>
nnoremap <leader>h :split<cr>

" Closing splits
nnoremap <leader>q :close<cr>

" Closing buffers
nnoremap q :bp\|bd #<CR>
nnoremap Q :bd!<cr>

" Plugin: LSP {{{
lua <<EOF
require'lspconfig'.bashls.setup { }
require'lspconfig'.dockerls.setup { }
util = require "lspconfig/util"
require'lspconfig'.gopls.setup {
    cmd = {"gopls", "serve"},
    filetypes = {"go", "gomod"},
    root_dir = util.root_pattern("go.work", "go.mod", ".git"),
    settings = {
      gopls = {
        analyses = {
          unusedparams = true,
        },
        staticcheck = true,
      },
    },
}
require'lspconfig'.hls.setup { }
require'lspconfig'.pylsp.setup {
    filetypes = {"python"},
    enable = true,
    settings = {
        configurationSources = {"flake8"},
        plugins = {
            flake8 = {
                enabled = true,
                maxLineLength = 120,
            },
            pycodestyle = { enabled = false }
        }
    },
    on_attach = on_attach
}
require'lspconfig'.jsonls.setup { }
require'lspconfig'.yamlls.setup { }
EOF

nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> gi    <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR>
nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>
nnoremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<CR>
" }}}
"

" Plugin: nvim-cmp {{{
lua <<EOF
  local cmp = require'cmp'

  cmp.setup({
    mapping = {
      ['<Up>'] = cmp.mapping.select_prev_item(),
      ['<Down>'] = cmp.mapping.select_next_item(),
      ['<C-d>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.close(),
      ['<CR>'] = cmp.mapping.confirm({ select = true }),
    },
    sources = {
      { name = 'nvim_lsp' },

    }
  })
EOF

"----------------------------------------------
" Plugin: tpope/vim-fugitive
"----------------------------------------------
nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>gs <cmd>Telescope git_status<cr>
nnoremap <leader>gc :Git commit<cr>
nnoremap <leader>gp :Git push<cr>

"----------------------------------------------
" Plugin: bling/vim-airline
"----------------------------------------------
" Show status bar by default.
set laststatus=2

let g:airline#extensions#tabline#enabled = 0
let g:airline#extensions#tabline#show_tabs = 0

let g:airline_powerline_fonts = 1
let g:airline_theme='solarized'
let g:airline#extensions#clock#format = '%a %d %b | %H:%M'

"----------------------------------------------
" Plugin: 'nvim-telescope/telescope.nvim'
"----------------------------------------------
nnoremap <c-p> <cmd>Telescope find_files<cr>
nnoremap <leader>r <cmd>Telescope oldfiles<cr>
nnoremap <leader>; <cmd>Telescope live_grep<cr>
nnoremap <leader>f :Neotree reveal<cr>
nnoremap <F8> <cmd>Telescope diagnostics<cr>
nnoremap <F9> <cmd>Telescope lsp_document_symbols<cr>
nnoremap <F5> <cmd>Telescope buffers<cr>

"----------------------------------------------
" Language: Golang
"----------------------------------------------
au FileType go set noexpandtab
au FileType go set shiftwidth=4
au FileType go set softtabstop=4
au FileType go set tabstop=4

" Mappings
au FileType go nmap <F10> :GoTest -short<cr>

" Run goimports when running gofmt
let g:go_fmt_command = "goimports"

" Enable syntax highlighting per default
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_highlight_extra_types = 1

" Show the progress when running :GoCoverage
let g:go_echo_command_info = 1

" Show type information
let g:go_auto_type_info = 0

" Highlight variable uses
let g:go_auto_sameids = 0

" Fix for location list when vim-go is used together with Syntastic
let g:go_list_type = "quickfix"

" Add the failing test name to the output of :GoTest
let g:go_test_show_name = 1

" Set whether the JSON tags should be snakecase or camelcase.
let g:go_addtags_transform = "snakecase"

" Use gopls
let g:go_def_mode='gopls'
let g:go_info_mode='gopls'

"----------------------------------------------
" Language: Python
"----------------------------------------------
au FileType python set expandtab
au FileType python set shiftwidth=4
au FileType python set softtabstop=4
au FileType python set tabstop=4

au FileType python nmap <leader>i :%!isort -<cr>
autocmd Filetype python setlocal omnifunc=v:lua.vim.lsp.omnifunc

"----------------------------------------------
" Language: terraform
"----------------------------------------------
let g:terraform_align=1
let g:terraform_fmt_on_save=1

"----------------------------------------------
" Language: gitcommit
"----------------------------------------------
au FileType gitcommit setlocal spell
au FileType gitcommit setlocal textwidth=80

