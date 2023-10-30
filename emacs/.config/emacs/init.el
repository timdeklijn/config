;; ==========================================
;; ███████ ███    ███  █████   ██████ ███████ 
;; ██      ████  ████ ██   ██ ██      ██      
;; █████   ██ ████ ██ ███████ ██      ███████ 
;; ██      ██  ██  ██ ██   ██ ██           ██ 
;; ███████ ██      ██ ██   ██  ██████ ███████ 
;; ==========================================
;; Tim de Klijn

;; =============================================================================
;; Setup
;; =============================================================================
(setq user-full-name "Tim de Klijn")

;; Write generated elisp code to the 'custom-file'
(setq custom-file (make-temp-file "emacs-custom.el"))

;; =============================================================================
;; Use-Package
;; =============================================================================
(require 'package) ;; Emacs builtin

;; set package.el repositories
(setq package-archives
'(
   ("org" . "https://orgmode.org/elpa/")
   ("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
))
;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

;; initialize built-in package management
(package-initialize)

;; update packages list if we are on a new install
(unless package-archive-contents
  (package-refresh-contents))

;; Make sure that ':ensure t' is set for all packages
(eval-and-compile
  (setq use-package-always-ensure t
	use-package-expand-minimally t))

;; =============================================================================
;; Environment
;; =============================================================================
;; Set MacOS path correctly, add GOPATH to path as well
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

;; Make use of direnv from whithin Emacs. Make use of project (folders) scoped
;; environment variables within Emacs.
(use-package direnv
  :config (direnv-mode))

;; Make sure the compilation mode can handle ANSI color codes to see colors: for
;; example passing/failing tests.
(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

(setq compilation-scroll-output t)

;; =============================================================================
;; Basic Configuration
;; =============================================================================
;; do not type 'yes' or 'no' when prompted
(defalias 'yes-or-no-p #'y-or-n-p)

;; some defaults
(menu-bar-mode -1)
(delete-selection-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode)
(global-hl-line-mode -1)		;; highlight the cursor line
(global-auto-revert-mode t)	;; update buffer when file is updated

;; Some performence tweeks:
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; No more startup screen
(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      make-backup-files nil)

;; =============================================================================
;; Looks
;; =============================================================================
;; Make the titlebar on mac the same color as the background.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; NOTE: Try to keep this for over a week: 2023-10-30
;; Color theme: Solarized, with my settings.
(use-package solarized-theme
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-use-less-bold nil)
  (setq solarized-use-more-italic nil)
  (setq solarized-emphasize-indicators t)
  (setq solarized-scale-org-headlines t)
  (setq solarized-scale-markdown-headlines t)
  (load-theme 'solarized-selenized-black t))

;; NOTE: Try to keep this for over a week: 2023-10-30
;; Set Emacs font: family, size and weight.
(set-face-attribute 'default nil
		    :font "UbuntuMono Nerd Font Mono"
		    :height 260)

;; Highlight the folowing:
;; TODO:, FIXME:, NOTE:, etc.
(use-package hl-todo
  :config (global-hl-todo-mode))

;; =============================================================================
;; Modeline
;; =============================================================================
;; Hide modes from modeline
(use-package minions
 :config (minions-mode 1))

;; =============================================================================
;; Search and Completion
;; =============================================================================
(use-package counsel)
(use-package swiper)
(use-package ivy
  :bind (("C-s" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("<f6>" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> v" . counsel-describe-variable)
	 ("<f1> o" . counsel-describe-symbol)
	 ("<f1> l" . counsel-find-library)
	 ("<f2> i" . counsel-info-lookup-symbol)
	 ("<f2> u" . counsel-unicode-char)
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 ("C-c k" . counsel-rg)
	 ("C-x l" . counsel-locate))
  :config
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
	ivy-re-builders-alist
	'((t . ivy--regex-plus)))
  (ivy-mode 1))

;; Completion, get completions from LSP (eglot)
;; TODO: try out corfu or something smaller/faster
(use-package company
  :bind ("C-;" . company-complete)
  :config (setq company-idle-delay nil))
(add-hook 'after-init-hook 'global-company-mode)

;; =============================================================================
;; Project
;; =============================================================================
;; project manager, builtin package
(use-package project
  :commands (project-root))

;; =============================================================================
;; Magit
;; =============================================================================
;; git client
;; TODO: how do I checkout branches?
(use-package magit
  :bind ("C-c g" . magit-file-dispatch))

;; =============================================================================
;; vterm
;; =============================================================================
;; terminal emulator
(use-package vterm)

;; =============================================================================
;; Language Servers
;; =============================================================================
;; eglot is used as an lsp client for programming languages
(use-package eglot)

;; Add lsp servers to list, should be started when eglot runs for a specific
;; language
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       ;; '(python-mode . ("pyright" "--stdio"))
	       '(python-mode . ("pylsp" "--stdio"))
	       '(yaml-mode . ("yaml-language-server" "--stdio"))))
;; TODO: rust, go, json, markdown

;; =============================================================================
;; Custom Keybinds
;; =============================================================================
;; Not all commands have keybinds. Also, some custom commands may also
;; be assigned to a custom keybind

;; -----------------------------------------------------------------------------
;; Open file shortcuts
;; -----------------------------------------------------------------------------

;; Open config file
(global-set-key (kbd "C-c o o")
		(lambda () (interactive)(find-file "~/dotfiles/emacs/.config/emacs/init.el")))

;; ns notes
(global-set-key (kbd "C-c o n")
		(lambda () (interactive)(find-file "~/TimDocs/notes/ns.org")))

;; htc notes
(global-set-key (kbd "C-c o h")
		(lambda () (interactive)(find-file "~/TimDocs/notes/htc.org")))

;; Open todo file, this is also used for agenda/calendar
(global-set-key (kbd "C-c o t")
		(lambda() (interactive)(find-file "~/TimDocs/notes/todo.org")))

;; -----------------------------------------------------------------------------
;; Custom keybinds
;; -----------------------------------------------------------------------------

;; Shortcut to Emacs org-agenda
(global-set-key (kbd "C-c a") 'org-agenda)
;; Rerun last compilation command
(global-set-key (kbd "C-c r r") 'recompile)
(global-set-key (kbd "C-c f") 'project-find-file)
(global-set-key (kbd "C-c e f") 'eglot-format-buffer)
(global-set-key (kbd "C-c e r n") 'eglot-rename)

;; =============================================================================
;; Treesitter
;; =============================================================================
;; activate tree-sitter for relevant languages
(add-hook 'python-mode-hook #'tree-sitter-mode)
(add-hook 'go-mode-hook #'tree-sitter-mode)
(add-hook 'rust-mode-hook #'tree-sitter-mode)
(add-hook 'org-mode-hook #'tree-sitter-mode)
(add-hook 'yaml-mode-hook #'tree-sitter-mode)
(add-hook 'json-mode-hook #'tree-sitter-mode)
(add-hook 'markdown-mode-hook #'tree-sitter-mode)
(add-hook 'zig-mode-hook #'tree-sitter-mode)

;; Download tree-sitter grammars
(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

;; =============================================================================
;; Org Mode
;; =============================================================================
;; Create fancy headline symbols to org header lines.
(use-package org-superstar
  :config
  (setq org-superstar-special-todo-items 1))

;; Org mode that includes fancy headers symbols.
(use-package org
  :after org-superstar
  :init
  ;; add fancy looking header symbols
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  ;; make sure each line is 100 chars long when filling paragraph
  (add-hook 'org-mode-hook (lambda () (set-fill-column 100))))

(eval-after-load "org"
  '(require 'ox-md nil t))

;; Add my todo file as an agenda file to see an overview of my todo's in the
;; Emacs calendar.
(setq org-agenda-files (list "~/TimDocs/notes/todo.org"))

;; What languages should work within an org code block
(org-babel-do-load-languages 'org-babel-load-languages
			     '((shell . t)))

;; =============================================================================
;; Go
;; =============================================================================
;; Simple go mode does not add LSP by default, this is done later.
;; TODO: go-mode has quite some nice functionality that I need to look into.
;; TODO: add auto import functionality
(use-package go-mode)

;; =============================================================================
;; Python
;; =============================================================================
;; Python mode is mostly loaded for simple syntax highlighting. The LSP takes
;; care of most of the IDE like features. We add the debugger config in this
;; mode as well.

;; TODO: experiment with python-lsp-server and its functions
(use-package python-mode
  :config (setq truncate-lines 0))

(setq-default python-indent-offset 4)

;; TODO: function to run 'black' and 'isort' on current buffer
;; TODO: function to run 'pylint' and 'mypy' on current buffer
;; TODO: function to run 'pylint' and 'mypy' on whole project

;; =============================================================================
;; Rust
;; =============================================================================
;; Rust highlighting and other stuff.
(use-package rust-mode
  :custom (setq rust-format-on-save 1))

;; Make sure there is no weird indenting
(add-hook 'rust-mode-hook '(lambda () (setq indent-tabs-mode nil)))

;; =============================================================================
;; terraform
;; =============================================================================
(use-package terraform-mode
  :custom
  (terraform-indent-level 2)
  (terraform-format-on-save 1)
  :config
  (defun my-terraform-mode-init ()
    (outline-minor-mode 1))
  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

;; =============================================================================
;; Markdown
;; =============================================================================
;; Package to support markdown mode. This is a huge package with a lot of
;; functionality.
;; TODO: Dive into this package and learn more about what can be done with
;;       it.
;; TODO: Maybe add either a Markdown Language server or a Markdown linter.
(use-package markdown-mode)

;; =============================================================================
;; Configuration modes
;; =============================================================================
;; TODO: linters or formatters for both json and yaml
(use-package json-mode)
(add-hook 'json-mode-hook (lambda () (setq tab-width 2)))
(use-package yaml-mode)
