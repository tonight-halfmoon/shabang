function prompt_char {
  if [ $UID -eq 0 ]; then echo "#"; else echo "%{%F{green}\u30A2\u30DE\u30C9%}%%"; fi
}

PROMPT='%(!.%{$fg_bold[red]%}.%{$fg_bold[magenta]%}%n@)%{$fg_bold[green]%}%m %{$fg_bold[cyan]%}%(!.%1~.%~) %{$fg_bold[magenta]%}
$(git_prompt_info)$(prompt_char)%{$reset_color%} '

ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX=") "
