"----------------------------------------------
" Plugin management
" Download vim-plug from the URL below and follow the installation
" instructions:
" https://github.com/junegunn/vim-plug
"----------------------------------------------
call plug#begin('~/.vim/plugged')

" Dependencies
Plug 'tpope/vim-rhubarb'           " Depenency for tpope/fugitive

" LSP
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/completion-nvim'

" General
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'enricobacis/vim-airline-clock'
Plug 'editorconfig/editorconfig-vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'mhinz/vim-signify'
Plug 'mileszs/ack.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'        " for bracket mappings
Plug 'francoiscabrol/ranger.vim'
Plug 'rbgrouleff/bclose.vim'
Plug 'kshenoy/vim-signature'       " show marks in the gutter

" Language support
Plug 'buoto/gotests-vim'                       " gotests
Plug 'psf/black', { 'commit': 'ce14fa8b497bae2b50ec48b3bd7022573a59cdb1' }
Plug 'hashivim/vim-terraform'

" Colorschemes
Plug 'iCyMind/NeoSolarized'
Plug 'jonathanfilip/vim-lucius'
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
set completeopt-=preview          " remove the horrendous preview window
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

"----------------------------------------------
" Colors
"----------------------------------------------

set background=dark
set termguicolors
let g:gruvbox_material_background = 'soft'
colo gruvbox-material

" Toggle background with <leader>bg
map <leader>bg :let &background = (&background == "dark"? "light" : "dark")<cr>

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
autocmd BufEnter * :lua require'completion'.on_attach()

lua <<EOF
require'lspconfig'.bashls.setup { }
require'lspconfig'.ccls.setup { }
require'lspconfig'.dockerls.setup { }
require'lspconfig'.gopls.setup { }
require'lspconfig'.hls.setup { }
require'lspconfig'.html.setup { }
require'lspconfig'.jsonls.setup { }
require'lspconfig'.solargraph.setup { }
require'lspconfig'.vimls.setup { }
require'lspconfig'.yamlls.setup { }
EOF

nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR>
nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>
nnoremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<CR>
" }}}

"----------------------------------------------
" Plugin: tpope/vim-fugitive
"----------------------------------------------
nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gp :Git push<cr>

"----------------------------------------------
" Plugin: bling/vim-airline
"----------------------------------------------
" Show status bar by default.
set laststatus=2

" Enable top tabline.
let g:airline#extensions#tabline#enabled = 1

" Disable showing tabs in the tabline. This will ensure that the buffers are
" what is shown in the tabline at all times.
let g:airline#extensions#tabline#show_tabs = 0

" Enable powerline fonts.
let g:airline_powerline_fonts = 1

" Explicitly define some symbols that did not work well for me in Linux.
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_symbols.branch = ''
let g:airline_symbols.maxlinenr = ''
let g:airline_theme='gruvbox_material'
let g:airline#extensions#clock#format = '%a %d %b | %H:%M'

"----------------------------------------------
" Plugin: 'junegunn/fzf.vim'
"----------------------------------------------
nnoremap <c-p> :FZF<cr>
nnoremap <c-m> :History<cr>

"----------------------------------------------
" Plugin: mileszs/ack.vim
"----------------------------------------------
" Open ack
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif
nnoremap <leader>; :Ack<Space>


"----------------------------------------------
" Language: Python
"----------------------------------------------
au FileType python set expandtab
au FileType python set shiftwidth=4
au FileType python set softtabstop=4
au FileType python set tabstop=4

autocmd BufWritePre *.py execute ':Black'
au FileType python nmap <leader>i :%!isort -<cr>

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

