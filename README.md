Packages
========
Dotfile packages reside in the `package` folder. To install a package run:

``` sh
./install package/<package-name>
```

agnosticrc/base
---------------
The **agnosticrc/base** package is a common set of scripts that are agnostic of
the shell of the user. At this time, agnotistrc only supports bash and zsh.

Local rc scripts may be added to `${HOME}/.local/agnosticrc/agnosticrc.d/`.

The **agnosticrc/base** package creates the following files:
``` text
$HOME/
├── .config/
│   └── agnosticrc/
│       ├── agnosticrc.d/
│       │   ├── 00_env.sh
│       │   ├── 01_alias.sh
│       │   ├── 01_completion.sh
│       │   ├── 01_opts.sh
│       │   └── 01_path.sh
│       └── agnosticrc
├── .local/
│   └── agnosticrc/
│       └── agnosticrc.d/
│           └── .gitkeep
├── .bashrc
└── .zshrc
```

**To install run:**
``` sh
/.install package/agnosticrc/base
```

agnosticrc/default_emacs
------------------------
The **agnosticrc/default_emacs** package sets the EDITOR and VISUAL environment
variables to `emacs`.

The **agnosticrc/default_emacs** package creates the following files:
``` text
$HOME/
└── .config/
    └── agnosticrc/
        └── agnosticrc.d/
            └── 05_editor.sh
```

**To install run:**
``` sh
/.install package/agnosticrc/default_emacs
```

agnosticrc/default_vim
----------------------
The **agnosticrc/default_vim** package creates the following files:
``` text
$HOME/
└── .config/
    └── agnosticrc/
        └── agnosticrc.d/
            └── 05_editor.sh
```

**To install run:**
``` sh
/.install package/agnosticrc/defunct_vim
```

agnosticrc/starship
-------------------
The **agnosticrc/starship** package installs the
[starship cross shell prompt](https://starship.rs). 

The **agnosticrc/starship** package creates the following files:
``` text
$HOME/
└── .config/
    ├── agnosticrc/
    │   └── agnosticrc.d/
    │       └── 02_starship.sh
    └── starship.toml
```

**To install run:**
``` sh
/.install package/agnosticrc/starship
```

kporter
-------
The **kporter*** package configures misc personal dotfiles. 

The **kporter** package creates the following files:
``` text
$HOME/
├── .config/
│   └── i3/
│       └── config
├── .doom.d/
│   ├── config.el
│   ├── init.el
│   └── packages.el
├── .gitconfig
└── .gitignore
```

**To install run:**
``` sh
/.install package/kporter
```

tmux
----
The **tmux** package configures tmux.

The **tmux** package creates the following files:
``` text
$HOME/
└── .tmux.conf

```

**To install run:**
``` sh
/.install package/tmux
```

Acknowledgements
================
[dotbot]: https://github.com/anishathalye/dotbot
[template]: https://github.com/anishathalye/dotfiles_template

