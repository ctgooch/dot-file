" ============================
" Some contributions by geoff mcnamara
" Some contributions from Matt Spong
" Some taken from Sven Guckes guckes@vim.org (guckes@math.fu-berlin.de)
" Troy Gooch Current owner
" NOTES:
"  * Comments start with  a double quote mark
"  * One settings is set for a dark background - you may have a light
"    colored background that requires a different setting 
"      - see [ set background=dark ] below
"  * This script also tries to use templates for some scripting (files
"    ending in .sh for example) - see [ autocmd BufNewFile ] below
" ===========================
" $Id: .vimrc,v 1.60 2005/07/14 20:30:36 geoffm Exp $
" Last update
" this stuff below can bust things so I took it out
"set mousehide
"set mousefocus
"set mouse=a
"set mousem=popup
" Movement
" fx (goes forward to the next "x")
" Fx (goes backward to the previous "x")
" tx  (goes forward to the char before the next "x")
" 2tx (goes to the char just before the second "x")
" dG deletes all lines from cursor to EOF
" :g/^#/s/foo/bar/g <-finds every line starting with "#" and replaces all
"           foo with bar


"===== set up a menu and map to F4 ==========
if has("menu")
  source $VIMRUNTIME/menu.vim
  set wildmenu
  "set cpo-=<
  set wcm=<C-Z>
  map <F4> :menu <C-Z>
endif

noremap <F6> :so `$VIMRUNTIME/tools/vimspell.sh %`<CR><CR> "highlights errors
noremap <F7> :syntax clear SpellErrors<CR> " unhighlights errors

if has("syntax")
  function Swapcolor()
    if exists("g:syntax_on")
      syntax off
      set nohlsearch
    else
      syntax on
      set hlsearch
    endif
  endfunction
endif

"===== set stuff =============
set fileformat=unix
set nosecure      " allows running of extended macros (some are below)
set nocompatible  " turns off vi compatibility
set nosecure
set autowrite
set autoread
set noerrorbells  " thank goodness!
set visualbell
set t_vb=         " this sets the terminal visual bell off! (that is-quiet mode)
set tabstop=2     " so it will fit on the screen (list mode can help here too)
set shiftwidth=2  " same reason as above
set backspace=2   " matches what is going on above
set smarttab      " this generally works well for me

set nosmartindent " helps when pasting text
"set ai            " sets autoindenting on " bad news when pasting text
"set expandtab     " essential! - trashes tabs and replaces withspaces
set laststatus=2  " always show laststatus line
set ruler         " I want to see where I am over in the bottom right corner
"                    report: show a report when N lines were changed.
"                    report=0 thus means "show all changes"!
set report=0
set showmode      " is this html, perl, C++, script blah blah blah
" set list        " shows tabs and end of line symbols
" set nu          " set numberred lines
" set verbose=9 
set showmatch
set showcmd
set suffixes=.bak,~,.o,.h,.info
set gfn=-adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*  
" blink matching parens!
set showmatch
"  Allow jump commands for left/right motion to wrap to previous/next
"  line when cursor is on first/last character in the line:
set whichwrap=<,>,h,l,[,]
set wrapmargin=0
set wrapscan       " continue searching after hitting bottom
set comments=b:#,:%,fb:-,n:>,n:)
" 
set incsearch " jumps to search word as you type - can be annoying
set hidden        " allow hidden buffers
set showcmd       " always show command
set iskeyword=@,48-57,_,192-255,-,.,:,/,@-@
"   laststatus:  show status line?  Yes, always!
"   laststatus:  Even for only one buffer.
set laststatus=2

set statusline=%t       "tail of the filename
set statusline+=[%{strlen(&fenc)?&fenc:'none'}, "file encoding
set statusline+=%{&ff}] "file format
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%y      "filetype
set statusline+=%=      "left/right separator
set statusline+=%c,     "cursor column
set statusline+=%l/%L   "cursor line/total lines
set statusline+=\ %P    "percent through file
set statusline=%t       "tail of the filename
set statusline+=[%{strlen(&fenc)?&fenc:'none'}, "file encoding
set statusline+=%{&ff}] "file format
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%y      "filetype
set statusline+=%=      "left/right separator
set statusline+=%c,     "cursor column
set statusline+=%l/%L   "cursor line/total lines
set statusline+=\ %P    "percent through file
"       wildchar  the char used for "expansion" on the command line
"                 default value is "<C-E>" but I prefer the tab key:
set wildchar=<TAB>
" If you prefer backspace and delete in Insert mode to have the old behavior,
" put this line in your vimrc:
" inoremap   
" And you may also want to add these, to fix the values for <BS> and <Del>:
"   set t_kb=^H
"   set t_kD=^?
" (Enter ^H with CTRL-V CTRL-H and ^? with CTRL-V CTRL-? or <Del>.)
" If the value for t_kb is correct, but the t_kD value is not, use the ":fixdel"
" command.  It will set t_kD according to the value of t_kb.  This is useful if
" you are using several different terminals.    |:fixdel|
" When ^H is not recognized as <BS> or <Del>, it is used like a backspace.
set t_kb= " fixes the backspace issue
set t_kD=[3~ " fixes the delete key issue
""""""""""""""""""""""""""""""""""""""
" force using hjkl$
" :noremap <Up>       :echoerr "Use k instead!"<CR>$
" :noremap <Down>     :echoerr "Use j instead!"<CR>$
" :noremap <Left>     :echoerr "Use l instead!"<CR>$
":noremap <Right>    :echoerr "Use h instead!"<CR>$,
"""""""""""""""""""""""""""""
" To start, we must first tell vim where our dictionary is located.  
" This is done via the 'dictionary'  option.  Below is an example.
" Your location may vary.  See :help 'dictionary'
" for hints as to where you should look.
"
" :set dictionary-=/usr/share/dict/words dictionary+=/usr/share/dict/words
"
" Now, to use this list we have to enter insert mode completion.  
" This is done by hitting CTRL-X
" while in insert mode.  Next, you have to specify what you want to complete.
" For dictionaries use CTRL-K.  Once in this mode the keys CTRL-N 
" and CTRL-P will cycle through the matches.  So, to complete the 
" word "acknowledgeable" I would do the following in insert mode:
"
" acknow<CTRL-X><CTRL-K><CTRL-N>
"
" It can be cumbersome to type CTRL-X CTRL-K for many different completions.
" So, vim gives us a shortcut.  While in insert mode CTRL-N and CTRL-P 
" will cycle through a predetermined set of completion sources. 
" By default, dictionary completion is not a part of this set.  This set is
" defined by the 'complete' option.  Therefore, we must add dictionary 
" to this as shown below:
"
" :set complete-=k complete+=k
"
" Now, while in insert mode we can type the following to complete 
" our example:
"
" acknow<CTRL-N><CTRL-N>
""""""""""""""""""""""""""""""""""""""
set dictionary-=/usr/share/dict/words dictionary+=/usr/share/dict/words
set complete-=k complete+=k

if has("folding") " {{{
  set foldmethod=marker " uses the default {{{ to mark VIM folds 
endif

" use the z commands to control folds zc=closes them zo=opens them " }}}

" set ic " set ignore case change with set noic
"}}}
"======= abbreviations ==========
"Note: <C-R> means Ctrl-R inserts contents of a numbered or named register.
"iab MYRULER  123456789-123456789-123456789-123456789-123456789-123456789-123456789-123456789-
iab _ruler  123456789-123456789-123456789-123456789-123456789-123456789-123456789-123456789-
"iab MYDATE <C-R>=strftime("%b %d %Y")
"iab MYDTIME <C-R>=strftime("%Y%m%d-%T")
" insert mode: "_date "
iab _date <C-R>=strftime("%b %d %Y ") 
iab _dtime <C-R>=strftime("%b %d %Y %H:%M ") 
" stooopid keyboard :-) - I always miss type the word the
iab hte <C-R>the 

"========= maps ===============
" make _G grep for the word under the cursor in all 
" .c, .cc, .h, etc., files in the current directory and then 
" make :cn, :cp, :cl, etc... use the list grep returns
map _G :let efsave=&ef<Bar>let &ef=tempname()<Bar>exe ':!grep -n -w "<cword>" *.[cChH] *.cc *.cpp *.hpp *.txt >'.&ef<CR>:cf<CR>:exe ":!rm ".&ef<CR>:let &ef=efsave<Bar>unlet efsave<CR><CR>:cc<CR> 

" This runs ispell against the file
map _S :w<CR>:!aspell -c %<CR>                
map spell :w<CR>:!aspell %<CR>                

" Use this when you are doing mail - it turns on word wrap at 75 chars
map _W :set textwidth=75<CR>:set wrap<CR>:set wrapmargin=1

" Insert the date and time right where the cursor is now
map _D <Esc>mm<Esc>i<CR><Up><Esc>:r!date +"\%b \%d, \%Y \%H:\%M \%p"<CR>:j<CR>`m:j<CR>
" or
:map <F2> a<C-R>=strftime("%c")<CR><Esc>
" or
" give current date
iab DATE <C-R>=strftime("%a %b %d %T %Z %Y")<CR>

" Looks for the string Last update and changes the date-needs _date 
"map _L  1G/Last update:\s*/e+1<CR>C_date<ESC>
"map _L  1G/Last update\s*/e<CR>C_date<ESC>

" Turn off smartindent - usefull when pasting external lines 
map _N  :set nosmartindent<CR>
map _I  :set smartindent<CR>  " to turn smart indent back on

" use shell with ctrl-z
map <C-Z> :shell

" convert text2html
map ,h :runtime! syntax/2html.vim 


" Because my pinky is slow to release the shift key - this only works sometimes
"   I have no idea why...
map :W :w

" To cause a quick save invoke the next lines - no chance to stop it though
map :Q :q<CR>
map _Q :wq!<CR>              " this writes regardless and quits 
"map :wq :wq<CR>
"map :WQ :wq<CR>

" map F8 to switch on and off syntax highlighting
map <F8> :call Swapcolor()<CR>

" set up split windows 
map <F9> :split<CR><c-w>w

map <C-N> :n<CR>    " next window/session/file-must be rw files
map <C-P> :rew<CR>  " previous window/session/file-must be rw files

map _T :.,$ s/^<Tab>*//<CR>:%s/^ *//<CR> " strips leading tabs out
                               " from here to end of buffer

map _C :% s/<Tab>/  /g <CR>      " change all tabs to 2 spaces

"cmap <C-W> s/^ *//<CR>         " get rid of begining white spaces
                                 " after you put in an address

"map <C-Home> :1<CR> " hit the top of the file
"map <C-End> :$<CR>  " Ahh - bottom of file
" 980527 I often reformat a paragraph to fit some textwidth -
" and I use the following mapping to adjust it to the
" current position of the cursor:
  map #tw :set textwidth=<C-R>=col(".")<C-M>
" Disable the suspend for ^Z.
" I use Vim under "screen" where a suspend would lose the
" connection to the " terminal - which is what I want to avoid.
map <C-Z> :shell

"=== autocmds ===========
if has("autocmd")
  autocmd!         
  "autocmd BufNewFile   *.sh 0r~/vimrc-files/vimrc-sh.tem
  autocmd BufWritePost *.sh !chmod +x %

  autocmd BufNewFile   *.html 0r~/vimrc-files/vimrc-html.tem
  "  or
  "au BufWinLeave *.html,*.pg mkview
  "au BufNewFile   *.html      0r ~/.vim/template.html
  "au BufNewFile   *.html      set ts=2
  "au BufWinEnter  *.html,*.pg silent loadview

  autocmd BufNewFile   *.php 0r~/vimrc-files/vimrc-php.tem

  " automatically read tags file for C/C++ files
  " au BufNewFile,BufRead *.cc,*.c,*.h,*.cpp,*.hpp so tags.vim

  " make { automatically add a matching } for C/C++ files
  au BufNewFile,BufRead *.cc,*.c,*.h,*.cpp,*.hpp inoremap { {<CR><CR>}<ESC>ki<TAB>

  " make /* add a closing */ for C/C++ files
  "au BufNewFile,BufRead *.cc,*.c,*.h,*.cpp,*.hpp inoremap /* <ESC>0i/*<CR><CR><ESC>0i*/<ESC>k0i<TAB>

  " we want C-style indenting only for C/C++ files        
  au BufNewFile,BufRead *.cc,*.c,*.h,*.cpp,*.hpp set cindent
endif

"======= play with colors =================
" t_Co = number of colors 
"if &t_Co > 1
if has("syntax")
  set background=dark
  syntax on
  " set hlsearch
  set nohlsearch
  hi! Comment  term=NONE cterm=NONE
  hi! Comment  ctermfg=cyan ctermbg=black guifg=blue guibg=black
endif

if has("gui_running") 
  set background=light
  "set background=dark
  syntax on
  set hlsearch
  " set nohlsearch
endif

if $TERM=="xterm"
  set background=light
  syntax on
  " set nohlsearch
endif

"if &diff
"  setup for diff mode
"else
"  setup for non-diff mode
"endif

"========= set color scheme for gui ====================
" set up my cheesy color scheme (GUI only for now, I suppose
" I should set up some colors for term at some point)

"highlight Comment gui=italic guifg=gray60
"highlight Comment term=bold ctermfg=Cyan guifg=#80a0ff gui=bold
highlight Comment term=bold ctermfg=Cyan guifg=#00a0f0 gui=bold
highlight String  guifg=DarkBlue
highlight Statement gui=bold guifg=Blue
highlight Conditional gui=bold guifg=Blue
highlight Number guifg=DarkCyan
highlight String guifg=DarkCyan
highlight Operator gui=NONE guifg=Blue
highlight Type gui=NONE guifg=Blue
highlight Structure guifg=Blue
highlight StorageClass guifg=Blue
highlight Paren guifg=DarkGray
highlight Function guifg=Blue
highlight Constant guifg=Blue
highlight Normal guibg=White guifg=Black
highlight Tag guifg=#008888 gui=NONE
highlight Folded term=bold ctermfg=Red ctermbg=black guifg=Red guibg=#DDDDDD
highlight Visual guifg=Yellow guibg=Black

" 001010  Do the Color Test!
map ,CT :sp $VIMRUNTIME/syntax/colortest.vim<cr>:so %<cr>

execute pathogen#infect()
"#################################################################
"
filetype plugin  indent on
