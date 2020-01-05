function prompt_char {
  if [ $UID -eq 0 ]; then echo "#"; else echo $; fi
}

function amado {
  amado_str=\\u30A2\\u30DE\\u30C9
  echo "%{%F{green}%}${amado_str}"
}

PROMPT='%(!.%{$fg_bold[red]%}.%{$fg_bold[magenta]%}%n@)%{$fg_bold[green]%}%m %{$fg_bold[cyan]%}%(!.%1~.%~) %{$fg_bold[magenta]%}
$(amado) $(git_prompt_info)$(prompt_char)%{$reset_color%} '

ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX=") "

# Local Variables:
# mode: shell-script
# coding: utf-8
# indent-tabs-mode: nil
# sh-basic-offset: 2
# sh-indent-comment: t
# tabs-width: 2
# End:
