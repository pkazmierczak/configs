# Emacs config

Requires **Emacs >= 29**.

Ports the spirit of `../nvim` to Emacs: if Emacs core (or eglot/flymake,
built in since 29) can do it, core does it. Packages only fill what's left,
and vim muscle memory comes back via evil.

| Job | Handled by |
| --- | --- |
| Package management | straight.el (+ `use-package`, built into Emacs 29) |
| Vim keybindings | evil + evil-collection, leader key via general.el |
| LSP client, diagnostics | eglot + flymake (core) |
| Syntax highlighting | tree-sitter major modes (core): `go-ts-mode`, `rust-ts-mode` |
| Minibuffer completion (files/buffers/grep/symbols) | vertico + orderless + marginalia + consult |
| In-buffer completion popup | corfu + cape |
| File browsing | dired (core) |
| Statusline | ~30 lines of Elisp in `lisp/init-ui.el` |
| Theme | modus-themes (core; light: modus-operandi, dark: modus-vivendi) |
| Git | magit (status/blame/log) + diff-hl (gutter/hunks) |
| Prose | org-mode, kept close to vanilla (see `lisp/init-org.el`) |

## Layout

```
early-init.el                 GC/UI tuning that must run before the frame is drawn
init.el                       requires the modules below, in order
lisp/init-straight.el         package manager bootstrap
lisp/init-defaults.el         core editor options
lisp/init-ui.el                theme, mode-line, which-key
lisp/init-completion.el        vertico/orderless/marginalia/consult/corfu/cape
lisp/init-evil.el              vim keybindings
lisp/init-editor.el            editing behaviour, recentf, TODO highlighting, yank pulse
lisp/init-eglot.el             LSP client config (which servers, diagnostics, format-on-save)
lisp/init-go.el                Go (go-ts-mode + gopls settings)
lisp/init-rust.el              Rust (rust-ts-mode + rust-analyzer settings)
lisp/init-git.el               magit, diff-hl, blamer
lisp/init-org.el               org-mode
lisp/init-tools.el             browse-at-remote, test runner
lisp/init-keybindings.el       leader-key map tying the above together
straight/versions/default.el   plugin lockfile — commit this
```

## External tools

Emacs does not install these; your package manager does.

```sh
brew install ripgrep lua-language-server
go install golang.org/x/tools/gopls@latest
rustup component add rust-analyzer clippy
```

Tree-sitter grammars are installed on demand:

```elisp
M-x treesit-install-language-grammar RET go RET
M-x treesit-install-language-grammar RET rust RET
```

Add a language server by adding a major-mode hook to `eglot-ensure` in
`lisp/init-eglot.el` (and, if you need per-server settings, a
`cfg-eglot-configure` call — see `lisp/init-go.el`/`lisp/init-rust.el` for the
pattern). Check status with `M-x eglot-events-buffer`.

## Maintenance

```elisp
M-x straight-pull-all             ; update everything
M-x straight-freeze-versions      ; write straight/versions/default.el
M-x straight-thaw-versions        ; reinstall exactly what the lockfile says
M-x straight-remove-unused-repos
```

## Keymaps

`SPC` is the leader (works in normal/visual/motion evil states). Press it and
wait — which-key lists everything.

### Core evil/Emacs defaults worth remembering (not defined in this config)

| Key | Action |
| --- | --- |
| `i` / `a` / `o` / `v` / `V` | insert/append/open-line/visual/visual-line, as in vim |
| `dd` `yy` `p` `u` `C-r` | delete/yank/paste line, undo/redo |
| `cs"'` `ds"` `ysiw)` | evil-surround: change/delete/add surrounding pair |
| `gcc` / `gc` (visual) | comment line / comment selection (evil-nerd-commenter) |
| `jk` | leave insert state (evil-escape), in addition to plain `<Esc>` |
| `M-x` | run any command (already fuzzy-completed via vertico) |

### This config

| Key | Action |
| --- | --- |
| `q` / `Q` | kill buffer / kill buffer unconditionally |
| `SPC v` / `SPC q` | vertical split / close window |
| `SPC SPC` | recent files |
| `SPC f` / `SPC b` / `SPC /` | find file in project / switch buffer / grep (consult-ripgrep) |
| `SPC s/` | grep, restricted to the current file's extension |
| `SPC '` | resume last vertico/consult session |
| `C-n` | file explorer (dired) |
| `SPC s…` | search: `h` info manuals, `k` keybindings, `c` commands, `w` word under cursor, `t` TODOs, `s` spelling |
| `gd` / `SPC D` / `SPC r` / `SPC a` / `SPC e` | definition, type definition, rename, code action, line diagnostic |
| `grr` / `gri` / `gO` | references, implementations, document symbols (imenu) |
| `SPC ds` / `SPC ws` / `SPC wd` | document symbols / workspace symbols (consult-eglot) / workspace diagnostics |
| `SPC dd` / `SPC we` | buffer diagnostics / project diagnostics |
| `]c` `[c` / `SPC ph` | next/previous git hunk, preview hunk (diff-hl) |
| `SPC g…` | git: `s` stage hunk, `r` revert hunk, `d` diff file, `b` blame, `l` file log, `f` project files, `c` log, `B` browse-at-remote |
| `SPC t…` | toggle: `b` light/dark theme, `s` spell-check, `w` soft wrap, `f` format-on-save, `h` inlay hints (Emacs ≥ 30) |
| `<f10>` | run tests for the current major mode |
| `TAB` / `S-TAB` (insert state) | corfu completion menu next/previous candidate |

The nvim config's mini.pick "query syntax" table doesn't apply here —
consult uses [orderless](https://github.com/oantolin/orderless) instead, so
plain space-separated fuzzy terms already match out of order; there is no
separate exact/regex mode to switch.
