# To make Homebrew's completion available in zsh, you must get the
# Homebrew-managed zsh site-functions on you FPATH before initialising
# zsh's completion facility.
# The following code block needs to be evaluated before
# `compinit`. If you are using oh-my-zsh.sh, then compinit
# will be called. That means the following code block
# needs to be evaluated before calling oh-my-zsh.sh
# After that, force rebuild `zcompdump`, as follows:
# rm -f ~/.zcompdump; compinit
# Additionally, if you receive `zsh compinit: insecure directories`
# warnings when attempting to load these completions ´, you may need
# to run this:
# chmod go-w "$(brew --prefix)/share"
if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
fi

# Path to your oh-my-zsh installation.
export ZSH="/Users/sunrise/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="gentoo_cyan" #"blinks-customised" # #"aussiegeek" #"kphoen" #"robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=("robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
#
# After adding a plugin to the list, evaluate
# autoload -U compinit && compinit
plugins=(git docker docker-compose asdf kubectl mix)

source $ZSH/oh-my-zsh.sh

# User configuration

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

export PATH="/usr/local/sbin:/usr/local/bin:$PATH"

# export MANPATH="/usr/local/man:$MANPATH"
export MANPATH="/usr/local/opt/erlang/lib/erlang/man:$MANPATH"

export PATH="/usr/local/opt/sqlite/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/sqlite/lib"
export CPPFLAGS="-I/usr/local/opt/sqlite/include"

export PATH="/usr/local/opt/libpq/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/libpq/lib"
export CPPFLAGS="-I/usr/local/opt/libpq/include"



# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias ls="gls --color=auto"
alias e=emacs
alias ff="find . -print | grep -i ${1}"
alias bu="cd && amado/ && ./mybrew && cd"
alias fw="grep -rHn ${1} ."

# completion for kubectl
# TODO check first if kubectl exists
source <(kubectl completion zsh)

# azure cli
# requirements
# `brew install az`
# az completion
# reference [azure clie dev](https://raw.githubusercontent.com/Azure/azure-cli/dev/az.completion)
#
_python_argcomplete() {
  local IFS=$'\013'
  local SUPPRESS_SPACE=0
  if compopt +o nospace 2> /dev/null; then
    SUPPRESS_SPACE=1
  fi
  COMPREPLY=( $(IFS="$IFS" \
                   COMP_LINE="$COMP_LINE" \
                   COMP_POINT="$COMP_POINT" \
                   COMP_TYPE="$COMP_TYPE" \
                   _ARGCOMPLETE_COMP_WORDBREAKS="$COMP_WORDBREAKS" \
                   _ARGCOMPLETE=1 \
                   _ARGCOMPLETE_SUPPRESS_SPACE=$SUPPRESS_SPACE \
                   "$1" 8>&1 9>&2 1>/dev/null 2>/dev/null) )
  if [[ $? != 0 ]]; then
    unset COMPREPLY
  elif [[ $SUPPRESS_SPACE == 1 ]] && [[ "$COMPREPLY" =~ [=/:]$ ]]; then
    compopt -o nospace
  fi
}
complete -o nospace -F _python_argcomplete "az"


## Key bindings and auto-completion
## after evaluting `/usr/local/opt/fzf/install` the following
## is automatically generated
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

## Dir Color Solarized
## git clone https://github.com/seebi/dircolors-solarized.git
eval $(gdircolors ~/.dircolors-solarized/dircolors.256dark)

source <(kubectl completion zsh)

. /usr/local/opt/asdf/asdf.sh

# Local Variables:
# coding: utf-8
# mode: shell-script
# indent-tabs-mode: nil
# sh-basic-offset: 2
# sh-indent-comment: t
# tabs-width: 2
# End:
