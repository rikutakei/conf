""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" My .vimrc file "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"Make sure you have downloaded and installed Shougo's neobundle.vim:
"$ curl	https://raw.githubusercontent.com/Shougo/neobundle.vim/master/bin/install.sh > install.sh
" $ sh ./install.sh

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Pre-install tweak:

"Turn on Vim's enhancements and improvements:
if &compatible
  set nocompatible
endif

"Include the bundle directory in the runtime path, so plugins are visible to
"Vim
set runtimepath+=~/.vim/bundle/neobundle.vim/

"Automatically set what kind of 'make' to use for compiling vimproc.vim:
let g:make = 'gmake'
" If the system is GNU, use make and not gmake
if system('uname -o') =~ '^GNU/'
	let g:make = 'make'
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Neobundle package management:

"Start neobundle package manager:
call neobundle#begin(expand('~/.vim/bundle/'))

"Manage Neobundle first:
NeoBundleFetch 'Shougo/neobundle.vim'

"Manage other plugins (i.e. your custom plugins):
NeoBundle 'godlygeek/tabular'
NeoBundle 'junegunn/vim-easy-align'
NeoBundle 'Shougo/context_filetype.vim'
NeoBundle 'Shougo/neocomplete.vim'
NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimproc.vim', {'build': {'unix': g:make}}
NeoBundle 'tpope/vim-commentary'
NeoBundle 'tpope/vim-surround'

call neobundle#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General Vim settings:

filetype plugin on "Enable the plugin that recognises filetype syntax

let mapleader = " " " Change the default leader key to space key

" Map some key combinations to make saving and quitting easier:
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>

" Map some key combinations to make navigation between buffers easier:
nnoremap <Leader>j :bn<CR>
nnoremap <Leader>k :bp<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Neocomplete settings:

let g:acp_enableAtStartup = 0 " Disable AutoComplPop.
let g:neocomplete#enable_at_startup = 1 " Use neocomplete.
let g:neocomplete#enable_smart_case = 1 " Use smartcase.
let g:neocomplete#max_list = 4 " Set the maximum number of options shown in the popup menu
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scheme' : $HOME.'/.gosh_completions'
    \ }

" Define keyword.
if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'

" Recommended key-mappings.

" <CR>: close popup and save completion without returning.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return pumvisible() ? "\<C-y>" : "\<CR>"
endfunction

" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

" <C-h>, <BS>: close popup and delete backward char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"

" Close popup by <Space>.
"inoremap <expr><Space> pumvisible() ? "\<C-y>" : "\<Space>"

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" For smart TAB completion - this function will show the popup menu with
" recommended words.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" :
        \ <SID>check_back_space() ? "\<TAB>" :
        \ neocomplete#start_manual_complete()
function! s:check_back_space() 
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Neosnippet settings:

" Plugin key-mappings.
imap <C-k> <Plug>(neosnippet_expand_or_jump)
smap <C-k> <Plug>(neosnippet_expand_or_jump)
xmap <C-k> <Plug>(neosnippet_expand_target)

" SuperTab like snippets' behavior.
map <expr><TAB>
	\ pumvisible() ? "\<C-n>" :
	\ neosnippet#expandable_or_jumpable() ?
	\    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
map <expr><TAB> neosnippet#expandable_or_jumpable() ?
	\ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

" For conceal markers.
if has('conceal')
	set conceallevel=2 concealcursor=niv
endif

" Enable snipMate compatibility feature.
let g:neosnippet#enable_snipmate_compatibility = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


