# Lines configured by zsh-newuser-install
HISTFILE=~/.histfiles/zshhistory
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/riku/.zshrc'

autoload -Uz compinit
compinit

# End of lines added by compinstall

# My personal configuration:

# Not so complicated settings:
autoload -Uz colors && colors
# Set some color variables (mainly for prompt):
BLACK="%{$fg[black]%}"
GREEN="%{$fg[green]%}"
BLUE="%{$fg[blue]%}"
CYAN="%{$fg[cyan]%}"
RED="%{$fg[red]%}"
YELLOW="%{$fg[yellow]%}"
MAGENTA="%{$fg[magenta]%}"
WHITE="%{$fg[white]%}"
NO_COLOR="%{$reset_color%}"

# Set special character variables:
SEGMENT_SEP="\ue0b0"
BRANCH="\ue0a0"
DETACHED="\u27a6"
PLUSMINUS="\u00b1"

# Aliases:
alias ls='ls --color=auto'

# Prompt settings:
prompt off
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn
precmd() { vcs_info }
setopt prompt_subst
zstyle ':vcs_info:git*' formats "%b"
zstyle ':vcs_info:git*' actionformats "%b (%a)"

# Make git prompt:
prompt_git() {
	local ref color
	is_dirty() {
		test -n "$(git status --porcelain --ignore-submodules)"
	}
	ref="$vcs_info_msg_0_"
	if [[ -n "$ref" ]]; then
		if is_dirty; then
			color=$YELLOW
			ref="${ref} $PLUSMINUS"
		else
			color=$GREEN
			ref="${ref} "
		fi
		if [[ "${ref/.../}" == "$ref" ]]; then
			ref="$BRANCH $ref"
		else
			ref="$DETACHED ${ref/.../}"
		fi
		echo $color${ref}$NO_COLOR
	fi
}
PROMPT=`echo "$BLACK%K{blue}%n@%m%k$BLUE%K{yellow}$SEGMENT_SEP%k$BLACK%K{yellow}%1~%k$YELLOW$SEGMENT_SEP$NO_COLOR "`
RPROMPT='${RED}%(?..%?) `prompt_git`$NO_COLOR'

# Prevent duplication in history:
setopt HIST_IGNORE_DUPS

# History search with patterns matching current line:
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "${key[Up]}"   ]] && bindkey "${key[Up]}" up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey "${key[Down]}" down-line-or-beginning-search

eval $(thefuck --alias)
eval `dircolors ~/.dircolors`

export TERM=xterm-256color

