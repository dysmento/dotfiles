function ccat
    switch (it2profile -g)
      case Default
        bat --theme=Dracula $argv
      case Monokai
        bat --theme="Monokai Extended" $argv
      case light
        bat --theme="Solarized (light)" $argv
    end
end
