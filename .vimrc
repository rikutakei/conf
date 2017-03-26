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
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'godlygeek/tabular'
NeoBundle 'honza/vim-snippets'
NeoBundle 'jalvesaq/nvim-r'
NeoBundle 'jonathanfilip/vim-lucius' " you'll have to symlink or move the lucius.vim file into ~/.vim/colors/ directory for this to work
NeoBundle 'jpalardy/vim-slime'
NeoBundle 'junegunn/vim-easy-align'
NeoBundle 'majutsushi/tagbar'
NeoBundle 'mbbill/undotree'
NeoBundle 'osyo-manga/unite-quickfix'
NeoBundle 'rking/ag.vim'             " You'll have to install silversearcher-ag from command line
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'shanzi/autoHEADER'
NeoBundle 'Shougo/context_filetype.vim'
NeoBundle 'Shougo/neocomplete.vim'
NeoBundle 'Shougo/neoinclude.vim'
NeoBundle 'Shougo/neomru.vim'
NeoBundle 'Shougo/neoyank.vim'
NeoBundle 'Shougo/unite-outline'
NeoBundle 'Shougo/unite.vim'         " You may have to update it to the latest (possibly unstable) version of Vim to stop this freezing your vim
NeoBundle 'Shougo/vimproc.vim', {'build': {'unix': g:make}}
NeoBundle 'SirVer/ultisnips'
NeoBundle 'termoshtt/unite-bibtex'   " You need to install pybtex from the command line for this to work
NeoBundle 'thinca/vim-unite-history'
NeoBundle 'tpope/vim-commentary'
NeoBundle 'kana/vim-submode'
NeoBundle 'tpope/vim-dispatch'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tsukkee/unite-help'
NeoBundle 'tsukkee/unite-tag'
NeoBundle 'ujihisa/unite-colorscheme'
NeoBundle 'xolox/vim-easytags'       " You need to install exuberant-ctags
NeoBundle 'xolox/vim-misc'

call neobundle#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General Vim settings:

" Enable the plugin that recognises filetype syntax:
filetype plugin indent on
set omnifunc=syntaxcomplete#Complete

" Tweak colorscheme for Vim:
syntax enable
let g:solarized_termcolors=16
let g:solarized_contrast="high"
let g:solarized_visibility="normal"
set background=dark
colorscheme solarized

" Extra variables for toggling colorschemes and background:
let g:mybg=['dark', 'light'] " Background can be dark or light
let g:currentbg=0            " Counter for toggling background (0 or 1)

" General use interface settings:
set backspace=indent,eol,start " Allow backspace from where you pressed insert
set hidden                     " Allow switching to different buffer/files without saving
set history=1000               " Set the max number of history to remember
set incsearch                  " Turn on incremental search
set laststatus=2               " Always have statusline
set list                       " Display unprintable characters
set listchars=tab:›\ ,trail:•,extends:❯,precedes:❮ " Use these characters for unprintable characters
set matchtime=3                " Highlight the matching paranthesis for n/10 seconds
set nohlsearch                 " Turn on search highlighting
set notimeout                  " Don't time out for key codes and/or mappings
set nowrap                     " No text wrapping by default
set number                     " turn on line number
set pumheight=5                " Set how many words are shown in the popup menu for any completion
set ruler                      " Display the whereabouts you are in the file
set scrolloff=8                " Minimum lines to keep above/below the line with cursor
set showbreak=↪\               " Use this symbol to show where the line is wrapped
set showmatch                  " Show matching brackets/paranthese
set showmode                   " Display which mode you are in
set smartcase                  " Turn on smart case
set splitbelow                 " Horizontal split will split the window below
set splitright                 " Vertical split will split the window to the right
set ttimeout                   " Together with the line above, this will set it to time out for key codes, but not mappings
set ttimeoutlen=10             " Set time out length to 10 milliseconds
set virtualedit=block          " Allow you to move cursor to position with no characters (e.g past eol)
" set gdefault                 " If you want to make the g flag default for substitution, uncomment this line
set visualbell t_vb=           " Remove visual and/or sound notification for errors
set wildmenu                   " Show list of matches
set wildmode=full

" General text/comment format settings:
set autoindent
" set breakat=" ^I!@*-+;:,./?" " Default setting for breakat is good enough, but it's here if you want to change it
set formatoptions=croq1j       " Set default text and/or comment format options (see :h fo-table for explanation of each flag):
set linebreak                  " When wrapping lines, break at spaces and tabs only
set nojoinspaces               " prevents inserting two spaces after punctuation on a join (J)
set shiftwidth=4               " Use indents of four spaces
set softtabstop=4              " let backsapce delete indent
set tabstop=4                  " An indentation every four columns
set nodigraph                  " remove the digraph functionality with C-k

" Change some highlighting colours:
:hi IncSearch  term=reverse   cterm=reverse   ctermfg=1
:hi SpellBad   term=undercurl cterm=undercurl gui=undercurl ctermfg=5
:hi SpellCap   term=undercurl cterm=undercurl gui=undercurl ctermfg=5
:hi SpellRare  term=undercurl cterm=undercurl gui=undercurl ctermfg=5
:hi SpellLocal term=undercurl cterm=undercurl gui=undercurl ctermfg=5

" Spell checking and dictionary:
set dictionary=/usr/share/dict/words       " Set the dictionary directory
set spellfile=~/.vim/custom-dictionary.add " Set the file to put your custom words in

" Turn off the annoying "show-the-special-symbol-in-Vim-screen" feature for
" LaTeX:
let g:tex_conceal = ""

" Make sure all of the tex files are recognised as 'tex', not 'plaintex':
let g:tex_flavor = "latex"

" Use Ag as default grep method:
if executable('ag')
	" Use ag over grep
	set grepprg=ag\ --nogroup\ --nocolor
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key Mappings:

" Change the default leader key (\) to space key:
let mapleader = " "
let maplocalleader = "\\"

" Mapping to toggle text wrapping:
nnoremap <silent> <expr> gr &wrap == 1 ? ":set nowrap\<CR>" : ":set wrap\<CR>"

" Mappings to (re)load .vimrc file:
nnoremap <Leader>l :so ~/.vimrc<CR>

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
nnoremap <silent> <C-w>H <C-w><10
nnoremap <silent> <C-w>L <C-w>>10
nnoremap <silent> <C-w>J <C-w>+10
nnoremap <silent> <C-w>K <C-w>-10

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

" Mapping for creating folds:
vnoremap f :fold<CR>

" Mapping for toggling background colour:
nnoremap <silent> cob :call <SID>Togglebg()<CR>

" Mapping for toggling search highlighting (only in normal mode):
nnoremap <silent> <BS> :set hlsearch!<CR>

" Mapping for toggling spell checking (only in normal mode):
nnoremap <silent> <expr> <Leader>s &spell == 1 ? ":set nospell\<CR>" : ":set spell\<CR>"

" Mapping to move between incorrectly spelled words:
call submode#enter_with('spell_check', 'n', '', '<leader>n', ']s')
call submode#map('spell_check', 'n', '', 'n', ']s')
call submode#map('spell_check', 'n', '', 'N', '[s')
call submode#map('spell_check', 'n', '', 'j', 'j')
call submode#map('spell_check', 'n', '', 'k', 'k')
call submode#map('spell_check', 'n', '', '<C-f>', '<C-f>')
call submode#map('spell_check', 'n', '', '<C-b>', '<C-b>')

" Mapping for deleting blank lines between two lines of text
nnoremap <expr> dd (getline('.') =~ '^\s*$') ? "i\<C-r>=SmartBackSpace()\<CR>\<ESC>" : "dd"
inoremap <expr> <BS> pumvisible() ? neocomplete#smart_close_popup()."\<C-h>" : "\<C-R>=SmartBackSpace()\<CR>"

" Mapping for checking what regex is being picked up (only in between single
" quotation marks):
nnoremap <F5> yi':let @/ = @"<CR>

"Mappings to skip closing brackets when it is typed in insert mode
inoremap ) <C-r>=<SID>SkipBracket(')')<CR>
inoremap ] <C-r>=<SID>SkipBracket(']')<CR>
inoremap } <C-r>=<SID>SkipBracket('}')<CR>
inoremap > <C-r>=<SID>SkipBracket('>')<CR>

"Mapings to quickly make the document:
nnoremap <Leader>m :make<CR>

" If you want more key mapping ideas, see :h map-which-keys for a list of key
" sequences not used by Vim.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Autocommands:

" Autocommand to run make when I save a tex file:
autocmd BufWritePost *.tex :Make!

"Autocommand to put the cursor at the position where I was working the last
"time:
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Unite settings:

let g:unite_source_history_yank_enable=1
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#custom#profile('default', 'context', {
			\'winheight' : 10,
			\})

" Set the directory of the reference file:
let g:unite_bibtex_bib_files=["~/Documents/References/BibTeX/MSc.bib"]

"Mappings for unite:
nnoremap <silent> <C-g>b :<C-u>Unite -buffer-name=buffer      -start-insert            buffer:-<cr>
nnoremap <silent> <C-g>c :<C-u>Unite -buffer-name=colorscheme colorscheme<cr>
nnoremap <silent> <C-g>h :<C-u>Unite -buffer-name=help        -start-insert            -direction=dynamicbottom help:!<cr>
nnoremap <silent> <C-g>m :<C-u>Unite -buffer-name=mru         -start-insert            file_mru<cr>
nnoremap <silent> <C-g>r :<C-u>Unite -buffer-name=references  -start-insert            -direction=dynamicbottom bibtex<cr>
nnoremap <silent> <C-g>s :<C-u>Unite -buffer-name=syntastic   -direction=dynamicbottom -no-quit                 -toggle   -winheight=5 location_list<cr>
nnoremap <silent> <C-g>t :<C-u>Unite -buffer-name=outline     -direction=aboveleft     -toggle                  -vertical -winwidth=30 outline:!<cr>
nnoremap <silent> <C-p>  :<C-u>Unite -buffer-name=files       -start-insert            file_rec/async:!         buffer<cr>
nnoremap <silent> <C-y>  :<C-u>Unite -buffer-name=yank        -start-insert            history/yank             -default-action=append<cr>

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
"'\h\w*')
let g:neocomplete#keyword_patterns['default'] = '\h\w*-\w*'

" Recommended key-mappings from the manual:

" <CR>: completion:
inoremap <expr><CR> pumvisible() ? "\<C-y>" : "\<CR>"

" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

" <C-h>, <BS>: close popup and delete backward char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"

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
" ultisnippet settings:

" Unset all the default values:
let g:UltiSnipsExpandTrigger="<NOP>"
let g:UltiSnipsListSnippets="<NOP>"
let g:UltiSnipsJumpForwardTrigger="<NOP>"
let g:UltiSnipsJumpBackwardTrigger="<NOP>"

" Manually remap the keys to the ultisnip completion and jumping command:
inoremap <silent> <C-k> <C-R>=UltiSnips#ExpandSnippetOrJump()<CR>
snoremap <silent> <C-k> <ESC>:call UltiSnips#JumpForwards()<CR>
xnoremap <silent> <C-k> :call UltiSnips#SaveLastVisualSelection()<CR>gvs
inoremap <silent> <C-j> <C-R>=UltiSnips#JumpBackwards()<CR>
snoremap <silent> <C-j> <ESC>:call UltiSnips#JumpBackwards()<CR>

" Mapping and settings for editing personal snippet file:
let g:UltiSnipsSnippetsDir="~/.vim/mydir/mysnips/UltiSnips"
set runtimepath+=~/.vim/mydir/mysnips/
nnoremap <silent> <C-g>x :UltiSnipsEdit<CR>

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
" Fugitive settings:

" TODO: add some mappings for fugitive commands like gstatus/gdiff/gwrite/gread


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Gitgutter settings:

" Mappings to skip to hunks of changes:
nnoremap <silent> <C-n> :GitGutterNextHunk<CR>
nnoremap <silent> <C-k> :GitGutterPrevHunk<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Tabular settings:

" Mapping to align symbols that aren't covered in easy-align:
vmap <Tab>% :Tabularize /%<CR>

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

" Create a dictionary for matching braces:
let g:open_brackets = {  "{" : "}", "<" : ">",  "(" : ")",  "[" : "]", '"' : '"',  "`" : "`" }
let g:close_brackets = { "}" : "{", ">" : "<", ")" : "(", "]" : "[",  '"' : '"',  "`" : "`" }

" Function to add new pairs into the dictionary of matching pairs
" Note: char1 must be the open bracket, and char2 is the closing bracket
function! AddPairs(char1, char2)
	let g:open_brackets[a:char1] = a:char2
	let g:close_brackets[a:char2] = a:char1
	return
endfunction

" Function to remove pairs out of the dictionary of matching pairs
" Note: char1 must be the open bracket, and char2 is the closing bracket
function! RemovePairs(char1, char2)
	if has_key(g:open_brackets, a:char1) && has_key(g:close_brackets, a:char2)
		unlet g:open_brackets[a:char1] = a:char2
		unlet g:close_brackets[a:char2] = a:char1
	else
		echo "The pairs are not in the list of matching pairs."
	endif
	return
endfunction

" Automatic surrounding brackets (see Tim Pop's surround source code):
imap ( <Plug>Isurround)
imap { <Plug>Isurround}
imap [ <Plug>Isurround]

" Span three lines when you press enter straight after making brackets:
" (see Tim Pope's surround source code)
imap (<CR> <Plug>ISurround)
imap {<CR> <Plug>ISurround}
imap [<CR> <Plug>ISurround]

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NERDTree settings:

" Open/close NERDTree:
nnoremap <C-e> :NERDTreeToggle<CR>

" Show dot files by default (Toggle by pressing I in the menu):
let g:NERDTreeShowHidden=1

"TODO:add bookmark shortcut

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Nvim-R settings:

" if has("gui_running")
"     inoremap <C-TAB> <C-x><C-o>
" else
"     inoremap <C-TAB> <C-x><C-o>
" endif
vmap <C-m> <Plug>RDSendSelection
nmap <C-m> <Plug>RDSendLine

" Split window in tmux when starting R:
let R_in_buffer = 0
let R_applescript = 0
let R_tmux_split = 1

" Object browser config:
let R_objbr_place = "script,right"
let R_objbr_opendf = 1    " Show data.frames elements
let R_objbr_openlist = 0  " Show lists elements
let R_objbr_allnames = 0  " Show .GlobalEnv hidden objects
let R_objbr_labelerr = 1  " Warn if label is not a valid text

" " Completion setup:
" let R_show_args = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Syntastic settings:

" Show the sytax errors in quick fix list when file is opened:
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_loc_list_height= 5

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Undotree settings:

" Show/close UndoTree:
nnoremap <C-u> :UndotreeToggle<CR>

" Set persistent undo (have to make undodir in your home directory):
if has("persistent_undo")
	set undodir=~/.vim/mydir/undodir/
	set undofile
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim-slime settings:

" Make it compatible with tmux:
let g:slime_target="tmux"
let g:slime_default_config={"socket_name" : "default", "target_pane" : ":1.1"}

" Options to make it send lines to different panes:
let g:slime_vars={
			\"up":    {"socket_name" : "default", "target_pane" : "0"},
			\"down":  {"socket_name" : "default", "target_pane" : "1"},
			\"right": {"socket_name" : "default", "target_pane" : "2"},
			\"left":  {"socket_name" : "default", "target_pane" : "1"}
			\}

" Mappings:
xmap <C-c><C-c> <Plug>SlimeConfig<Plug>SlimeRegionSend
nmap <expr> <C-c><C-c> <SID>TmuxSend(slime_vars["down"], getline('.')."\r")
nmap <expr> <C-c><C-r> <SID>TmuxSend(slime_vars["right"], getline('.')."\r")

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" tagbar settings:

" Mapping to toggle tagbar:
nnoremap <C-g><C-t> :TagbarToggle<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" My functions:
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Function to toggle background colour:
function! s:Togglebg()
	:exec "set background=" . (g:mybg[g:currentbg])
	let g:currentbg=-(g:currentbg-1)
endfunction

" Function to skip the specified closing bracket:
function! s:SkipBracket(char) abort
	let cp = getpos('.')
	let str = getline('.')[cp[2]-1:]
	if (match(str, a:char) >= 0)
		let cp[2] = cp[2]+match(str, a:char)+1
		call setpos('.', cp)
	else
		call setpos('.', cp)
		return a:char
	endif
	return ""
endfunction

" Function that deletes any blank lines between lines of text
function! SmartBackSpace()
	let a = ''
	let comm = escape(GetComm(), '\-*.^$+?')
	let open = GetOpen(comm)
	let close = GetClose(comm)
	let cl = line('.')   " current line number
	let cp = 0           " count previous lines
	let ca = 0           " count lines after the current line
	let gl = getline(cl) " get the current line content
	let la = gl          " content of the non-whitespace line after the 'current' line
	let lp = gl          " content of the non-whitespace line before the 'current' line

	"If the current line contains a non-blank character before the cursor, use
	"normal deletion
	if !(gl =~ '^\s*$')
		if !(gl =~ '^\s*['.comm.']\s*$')
			return "\<C-h>"
		endif
	endif
	"Find the next line with non-blank chars before/after current line
	while lp =~ '\%^'
		if lp =~ '^.*\zs['.open.']\ze\s*$'
			break
		elseif lp =~ '\zs\S\ze\s*$'
			if lp =~ '^\s*['.comm.']\s*$'
				let cp = cp+1
				let lp = getline(cl-cp)
			else
				break
			endif
		elseif lp =~ '^\s*$'
			let cp = cp+1
			let lp = getline(cl-cp)
		else
			break
		endif
	endwhile
	while (cl+ca) <= line('$')
		if la =~ '^\s*\zs['.close.']\ze.*$'
			break
		elseif la =~ '\zs\S\ze\s*$'
			if la =~ '^\s*['.comm.']\s*$'
				let ca = ca+1
				let la = getline(cl+ca)
			else
				break
			endif
		elseif la =~ '^\s*$'
			let ca = ca+1
			let la = getline(cl+ca)
		else
			break
		endif
	endwhile
	"Delete the blank spaces in between the braces
	let l = lp.la
	let regex1 = '^.*\zs['.open.']\ze\s*$'
	let regex2 = '^\s*\zs['.close.']\ze.*$'
	let char1 = matchstr(lp, regex1)
	let char2 = matchstr(la, regex2)
	if has_key(g:open_brackets, char1)
		if has_key(g:close_brackets, char2)
			let a = call(function('DeleteBetweenBraces'), [cl, cp-1, ca, comm])
			return a
		else
			let a = call(function('DeleteBetweenBraces'), [cl, cp-2, ca, comm])
			return a
		endif
	else
		let a = call(function('DeleteBetweenBraces'), [cl, cp-1, ca, comm])
		return a
	endif
endfunction

" Function to delete the blanklines between the innermost braces
function! DeleteBetweenBraces(cl, cp, ca, comm)
	let a = SetCursor(a:ca)
	let x = a:cp+a:ca
	let tmp = "\<C-u>"
	while x >= 0
		let line = getline((a:cl+a:ca)-x)
		let a = a.tmp
		if (line =~ '^\s*['.a:comm.']\s*$') || (line =~ '^\s\+.*$')
			let a = a.tmp
			if line =~ '^\s['.a:comm.']'
				let a = a.tmp
			endif
		endif
		let x = x-1
	endwhile
	return a
endfunction

" Function to set cursor at the position where you want to start deleting
function! SetCursor(ca)
	let t = ''
	let line = getline(a:ca)
	let c = a:ca
	while c > 0
		let t = t."\<C-g>\<C-j>"
		let c = c-1
	endwhile
	return t.'\<ESC>I'
endfunction

" Function to get the possible comment characters for the current filetype
function! GetComm()
	let tmp = []
	let tmp = add(tmp, matchstr(&commentstring, '\zs.*\ze%s'))
	let x = split(matchstr(split(&comments, ','), 'm'), ':')
	if !empty(split(matchstr(split(&comments, ','), 'm'), ':'))
		let tmp = add(tmp, split(matchstr(split(&comments, ','), 'm'), ':')[1])
	endif
	let comm = join(tmp, '|')
	return comm
endfunction

" Function to construct a string of opening brackets
function! GetOpen(comm) abort
	let open = join(keys(g:open_brackets), '')
	let tmp = split(a:comm, '\zs')
	for x in tmp
		let open = substitute(open, x, '', 'g')
	endfor
	let open = escape(open, "*\[")
	return open
endfunction

" Same as GetOpen(), but for closing brackets
function! GetClose(comm) abort
	let close = join(keys(g:close_brackets), '')
	let tmp = split(a:comm, '\zs')
	for x in tmp
		let close = substitute(close, x, '', 'g')
	endfor
	let close = escape(close, "*\]")
	return close
endfunction

function! s:TmuxSend(config, text)
  let l:prefix = "tmux -L " . shellescape(a:config["socket_name"])
  " use STDIN unless configured to use a file
  if !exists("g:slime_paste_file")
    call system(l:prefix . " load-buffer -", a:text)
  else
    call s:WritePasteFile(a:text)
    call system(l:prefix . " load-buffer " . g:slime_paste_file)
  end
  call system(l:prefix . " paste-buffer -d -t " . shellescape(a:config["target_pane"]))
endfunction

function! s:TmuxPaneNames(A,L,P)
  let format = '#{pane_id} #{session_name}:#{window_index}.#{pane_index} #{window_name}#{?window_active, (active),}'
  return system("tmux -L " . shellescape(b:slime_config['socket_name']) . " list-panes -a -F " . shellescape(format))
endfunction

function! s:TmuxConfig() abort
  if !exists("b:slime_config")
    let b:slime_config = {"socket_name": "default", "target_pane": ":"}
  end

  let b:slime_config["socket_name"] = input("tmux socket name: ", b:slime_config["socket_name"])
  let b:slime_config["target_pane"] = input("tmux target pane: ", b:slime_config["target_pane"], "custom,<SNR>" . s:SID() . "_TmuxPaneNames")
  if b:slime_config["target_pane"] =~ '\s\+'
    let b:slime_config["target_pane"] = split(b:slime_config["target_pane"])[0]
  endif
endfunction
