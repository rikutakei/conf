"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"							My .vimrc file								   "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"Make sure you have downloaded and installed Shougo's neobundle.vim:
"
"$ curl https://raw.githubusercontent.com/Shougo/neobundle.vim/master/bin/install.sh > install.sh
"$ sh ./install.sh
"
"install.sh is already in my git repo, so just run that shell script to
"install neobundle

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Pre-install tweak:

"Turn on Vim's enhancements and improvements:
if &compatible
	set nocompatible
endif

"Include the neobundle directory in the runtime path, so plugins are visible to
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
NeoBundle 'Shougo/context_filetype.vim' " TODO: checkout precious.vim and/or quickrun
NeoBundle 'Shougo/neocomplete.vim'
NeoBundle 'Shougo/neosnippet.vim' "TODO: R snippets?
NeoBundle 'Shougo/neosnippet-snippets' "TODO: have a look at ulti snips
NeoBundle 'Shougo/unite.vim' "TODO: look at how to actually work this
NeoBundle 'Shougo/vimproc.vim', {'build': {'unix': g:make}} "TODO: try remap some commands
NeoBundle 'tpope/vim-commentary'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'jonathanfilip/vim-lucius' "you'll have to symlink or move the lucius.vim file into ~/.vim/colors/ directory for this to work

call neobundle#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General Vim settings:

" Enable the plugin that recognises filetype syntax:
filetype plugin indent on

" Tweak colorscheme for Vim:
syntax enable
let g:solarized_termcolors=16
let g:solarized_contrast="high"
let g:solarized_visibility="normal"
set background=light
colorscheme default

" Extra variables for toggling colorschemes and background:
let g:colschemelist=['default', 'solarized', 'lucius'] " List of colorscheme I want to cycle through
let g:mybg=['dark', 'light']                           " Background can be dark or light
let g:currentcolscheme=0                               " Counter to keep track of which colorscheme to use fromt the list
let g:currentbg=1                                      " Counter for toggling background (0 or 1)

" General use interface settings:
set number                                         " turn on line number
set incsearch                                      " Turn on incremental search
set hlsearch                                       " Turn on search highlighting
set smartcase                                      " Turn on smart case
set showmatch                                      " Show matching brackets/paranthese
set wildmenu                                       " Show list of matches
set wildmode=full
set scrolloff=8                                    " Minimum lines to keep above/below the line with cursor
set list                                           " Display unprintable characters
set listchars=tab:›\ ,trail:•,extends:❯,precedes:❮ " Use these characters for unprintable characters
set backspace=indent,eol,start                     " Allow backspace from where you pressed insert
set visualbell t_vb=                               " Remove visual and/or sound notification for errors
set showmode                                       " Display which mode you are in
set virtualedit=block                              " Allow you to move cursor to position with no characters (e.g past eol)
                                                   " set gdefault                                     " If you want to make the g flag default for substitution, uncomment this line
set splitbelow                                     " Horizontal split will split the window below
set splitright                                     " Vertical split will split the window to the right
set ruler                                          " Display the whereabouts you are in the file
set history=1000                                   " Set the max number of history to remember
set matchtime=3                                    " Highlight the matching paranthesis for n/10 seconds
set laststatus=2                                   " Always have statusline
set showbreak=↪\                                   " Use this symbol to show where the line is wrapped
set nowrap                                         " No text wrapping by default
set notimeout                                      " Don't time out for key codes and/or mappings
set ttimeout                                       " Together with the line above, this will set it to time out for key codes, but not mappings
set ttimeoutlen=10                                 " Set time out length to 10 milliseconds

" General text/comment format settings:
set autoindent
set linebreak                  " When wrapping lines, break at spaces and tabs only
" set breakat=" ^I!@*-+;:,./?" " Default setting for breakat is good enough, but it's here if you want to change it
set textwidth=80               " Set maximum characters per line in a file (only for comments)
set tabstop=4                  " An indentation every four columns
set softtabstop=4              " let backsapce delete indent
set shiftwidth=4               " Use indents of four spaces
set nojoinspaces               " prevents inserting two spaces after punctuation on a join (J)
set formatoptions=croq1j       " Set default text and/or comment format options (see :h fo-table for explanation of each flag):

" Spell checking and dictionary:
set dictionary=/usr/share/dict/words       " Set the dictionary directory
set spellfile=~/.vim/custom-dictionary.add " Set the file to put your custom words in

" Turn off the annoying "show-the-special-symbol-in-Vim-screen" feature for
" LaTeX:
let g:tex_conceal = ""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key Mappings:

" Change the default leader key (\) to space key:
let mapleader = " "

" Mappings to (re)load .vimrc file:
nnoremap    gr     :so ~/.vimrc<CR>
nnoremap <Leader>r :so ~/.vimrc<CR>

" Mappings to make moving around wrapped text easier:
nnoremap j gj
nnoremap k gk

" Mappings to make saving and quitting easier:
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>

" Mappings to make navigation between buffers easier:
nnoremap <Leader>j :bn<CR>
nnoremap <Leader>k :bp<CR>

" Mappings for splitting windows:
nnoremap <silent>  <C-w>-   <C-w>s
nnoremap <silent>  <C-w>\|  <C-w>v
nnoremap <silent> <Leader>= <C-w>=

" Mappings for resizing windows:
" nnoremap <silent> <C-J> :exe "resize -5" <CR>
" nnoremap <silent> <C-K> :exe "resize +5" <CR>
" nnoremap <silent> <C-H> :exe "vertical resize -5" <CR>
" nnoremap <silent> <C-L> :exe "vertical resize +5" <CR>

" Mappings for system clipboard yank:
nnoremap <Leader>y "+y
vnoremap <Leader>y "+y
nnoremap <Leader>p :set paste<CR>"+p<CR>:set nopaste<CR>
nnoremap <Leader>P :set paste<CR>"+P<CR>:set nopaste<CR>

" Mappings for shifting text in visual mode without leaving visual mode:
vnoremap < <gv
vnoremap > >gv
vnoremap = =gv

" Mapping to join line without moving cursor position:
nnoremap J mzJ`z

" Mapping to split a line at the current cursor position:
nnoremap S i<CR><ESC>^mwgk:silent! s/\v +$//<CR><BS>`w

" Mapping for using dot command on the selection:
vnoremap . :normal .<CR>

" Mapping for toggling background colour:
nnoremap <silent> cob :call Togglebg()<CR>

" Mapping for toggling colorscheme (default or solarized):
nnoremap <silent> col :call ToggleColScheme()<CR>

" Mapping for toggling search highlighting (only in normal mode):
nnoremap <silent> <BS> :set hlsearch!<CR>

" Mapping for toggling spell checking (only in normal mode):
cnoremap <silent>  :set hlsearch!<CR>

" If you want more key mapping ideas, see :h map-which-keys for a list of key
" sequences not used by Vim.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" My functions:
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Function to toggle background colour:
function! Togglebg()
	:exec "set background=" . (g:mybg[g:currentbg])
	let g:currentbg=-(g:currentbg-1)
endfunction

" Function to toggle colourscheme in the list:
function! ToggleColScheme()
	:exec "colorscheme" (g:colschemelist[g:currentcolscheme])
	if g:currentcolscheme < (len(g:colschemelist) - 1)
		let g:currentcolscheme=g:currentcolscheme+1
	else
		let g:currentcolscheme=0
	endif
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Neocomplete settings:

let g:acp_enableAtStartup = 0           " Disable AutoComplPop.
let g:neocomplete#enable_at_startup = 1 " Use neocomplete.
let g:neocomplete#enable_smart_case = 1 " Use smartcase.
let g:neocomplete#max_list = 4          " Set the maximum number of options shown in the popup menu
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

"Define the pattern to store/match for auto-completion (the default setting is
"'\h\w*', which mean start of the word to the end of the word).
"'\h[\w*[-_]\=]*' means from the start of a word to the end of a word that
"contains none or one - or _ characters:
let g:neocomplete#keyword_patterns['default'] = '\h[\w*[-_]\=]*'

" Recommended key-mappings from the manual:

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
autocmd FileType css           setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript    setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python        setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml           setlocal omnifunc=xmlcomplete#CompleteTags

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
imap <expr><TAB>
			\ pumvisible() ? "\<C-n>" :
			\ neosnippet#expandable_or_jumpable() ?
			\    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
			\ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

" For conceal markers.
if has('conceal')
	set conceallevel=2 concealcursor=niv
endif

" Enable snipMate compatibility feature.
let g:neosnippet#enable_snipmate_compatibility = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Easy align settings:

" Turn off foldmethod temporarily when using easy align:
let g:easy_align_bypass_fold=1

" Mappings for interactive aligning:
nmap  ga   <Plug>(EasyAlign)
vmap <CR>  <Plug>(EasyAlign)

" Mappings for filter aligning:
vmap <C-a> <Plug>(EasyAlign)<C-f>g/

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Tabular settings:

" Automatically tabularize text in insert mode when | is encountered:
" TODO: need to put these into language specific files for loading
inoremap <Bar> <Bar><Esc>:call <SID>align('<Bar>')<CR>a
inoremap & &<Esc>:call <SID>align('&')<CR>a

" Function to generalise Tim Pope's gist so any character passed to the function
" is used to automatically create tables (you will have to use a similar mapping
" as above):
function! s:align(char)
	" let tmp = substitute(a:char, '"', '', 'g')
	let p = '^.*'.a:char.'.*'.a:char.'.*$'
	if exists(':Tabularize') && getline('.') =~# '^.*'.a:char && (getline(line('.')-1) =~# p || getline(line('.')+1) =~# p)
		let column = strlen(substitute(getline('.')[0:col('.')],'[^'.a:char.']','','g'))
		let position = strlen(matchstr(getline('.')[0:col('.')],'.*'.a:char.'.*\zs.*'))
		exec "Tabularize/".a:char."/l1"
		normal! 0
		call search(repeat('[^'.a:char.']*'.a:char,column).'.\{-\}'.repeat('.',position),'ce',line('.'))
	endif
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Surround.vim settings:


" Automatic surrounding brackets:
imap ( <Plug>Isurround)
imap { <Plug>Isurround}
imap [ <Plug>Isurround]
imap " <Plug>Isurround"
imap ` <Plug>Isurround`

" Span three lines when you press enter straight after making brackets:
imap (<CR> <Plug>ISurround)
imap {<CR> <Plug>ISurround}
imap [<CR> <Plug>ISurround]

" TODO: add a function to skip the closing braces

" Add a function so that the <BS> within an empty three-line-spanning braces are
" put into a single line:
function! s:SmartBackSpace()
	let a = ''
	let cl = line('.')   " current line number
	let cp = 0           " count previous lines
	let ca = 0           " count lines after the current line
	let gl = getline(cl) " get the current line content
	let la = gl          " content of the non-whitespace line after the 'current' line
	let lp = gl          " content of the non-whitespace line before the 'current' line
	"If the current line contains a non-blank character before the cursor, use normal deletion
	if !(gl =~ '^\s*$')
		return "\<C-h>"
	endif
	"Find the next line with non-blank chars before/after current line
	while ((match(lp, '^.*(\zs\s*$') < 0) || !(lp =~ '\S$'))
		let cp = cp+1
		let lp = getline(cl-cp)
	endwhile
	while ((match(la, '^\s*).*$') < 0) || !(la =~ '\S$'))
		let ca = ca+1
		let la = getline(cl+ca)
	endwhile
	"Delete the blank spaces in between the braces
	" TODO: add conditional for when you're not in between braces, but in
	" between lines of blank spaces
	let l = lp.la
	if match(l, '^.*(\zs\s\n+\ze).*$')
		let a = call(function('DeleteBetweenBraces'), [l, cl, cp, ca])
		return a
	endif
endfunction

" Just a helping function to 'delete' a line
function! DeleteLine()
	return "\<C-u>"
endfunction

" Function to delete the blanklines between the innermost braces
" TODO: add a general brace_char variable to identify what kind of brackets
" you're in between
" TODO: undo for the smart backspace isn't working - ned to look into that
function! DeleteBetweenBraces(l, cl, cp, ca)
		let tmp_line = substitute(a:l, '^.*(\zs\s*\n*)\ze.*$','','g')
		call setline(a:cl-a:cp, tmp_line)
		let cursor_pos = [bufnr('%')-1, a:cl+a:ca, 0, 0]
		call setpos('.', cursor_pos)
		let x = a:cp+a:ca
		let a = ''
		let tmp = DeleteLine()
		while x > 0
			let a = a.tmp
			let x = x-1
		endwhile
		return a
endfunction

inoremap <BS> <C-R>=<SID>SmartBackSpace()<CR>
