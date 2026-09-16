function ccat
    # it2profile is iTerm2-only; elsewhere just use bat's default theme
    if not command -q it2profile
        bat $argv
        return
    end
    switch (it2profile -g)
      case Default
        bat --theme=Dracula $argv
      case Monokai
        bat --theme="Monokai Extended" $argv
      case light
        bat --theme="Solarized (light)" $argv
      case '*'
        bat $argv
    end
end
