;; My emacs configuration (v3).
;; TODO:
;;   - Split over multiple files
;;   - switch all 'straight-use-package' declarations to 'use-package'

;; Setup straight package manager:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install 'use-package' to use the 'use-package' macro together with straight.
(straight-use-package 'use-package)
;; Now I do not need to type ':straight t' everytime I use 'use-package'.
(setq straight-use-package-by-default 1)

;; NOTE: not sure what this is used for:
(setq user-full-name "Tim de Klijn")

;; Write generated elisp code to 'custom-file' in the emacs config directory
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Do not type 'yes' or 'no' when prompted
(defalias 'yes-or-no-p #'y-or-n-p)

;; Simple customizations
(menu-bar-mode -1)
(delete-selection-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode)
(global-auto-revert-mode t)           ;; update buffer when file is updated
(pixel-scroll-precision-mode 1)       ;; smooth scrolling

;; scroll before the cursor is at the bottom or top of the page.
(setq scroll-margin 4)

;; Stop making backup files in the working directory. simply move them to a
;; specified folder
(setq backup-directory-alist '((".*" . "~/.config/emacs/backup_files")))

;; Some performence tweaks:
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq package-install-upgrade-built-in t)

;; No more startup screen
(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      make-backup-files nil)

;; Configure color theme --------------------------------------------------------
(use-package color-theme-sanityinc-tomorrow
  :config (load-theme 'sanityinc-tomorrow-night t))

;; Fonts -----------------------------------------------------------------------
(defun tim-process-font-choice (choice)
  "if I figure out how to use a hashmap I do not need this
function. for now return a size for a specific 'choice'."
  (cond
    ((string-equal choice "small") 17)
    ((string-equal choice "medium") 21)
    ((string-equal choice "large") 24)
    (t (message "unknown size, choose: 'small', 'medium' or 'large'"))))

(defun tim-set-font (size)
  "set font for frame. Takes a number as input, this is the font
size in pixels."
  (set-frame-font (format "%s-%f" tim-font-name size)))

(defun tim-change-font-size ()
  "ask the user to input a choice out of 'small', 'medium' or
'large'. Process the input to a number and change the font size.
- Ask user for input
- Extract the choice from the list
- Convert choice to number in pixels
- Set the font"
  (interactive)
  (let ((choice (completing-read-multiple "Select: " '("small" "medium" "large"))))
    (tim-set-font (tim-process-font-choice (car choice)))))

;; Start-up font settings
(defvar tim-font-name "ComicShannsMono Nerd Font" "Font name")
(defvar tim-initial-font (tim-process-font-choice "medium") "Font Size medium value")
(setq-default line-spacing 0.3)

;; Set font on startup
(tim-set-font tim-initial-font)

;; Compilation Mode -------------------------------------------------------------
;;
;; Make sure the compilation mode can handle ANSI color codes to see
;; colors: for example passing/failing tests.
(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)
;; Scroll along with text appearing in the compilation buffer
(setq compilation-scroll-output t)

;; Dired ------------------------------------------------------------------------
(straight-use-package 'all-the-icons-dired)
(add-hook 'dired-mode-hook
  (lambda ()
    ;; Simplify Dired mode by hiding file details
    (dired-hide-details-mode)
    ;; Introduce pretty icons into Dired
    (all-the-icons-dired-mode)))

;; Mode Line --------------------------------------------------------------------
;;
;; Just use the doom-modeline. It looks nice, and works.
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Indent guides ----------------------------------------------------------------
;; Show indent markers.

(straight-use-package 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character 062) ;; ascii code for character
(setq highlight-indent-guides-auto-character-face-perc 80)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; Environment ------------------------------------------------------------------

;; If we are on a mac, we need to set the PATH and GOPATH environment
;; differently. This is because Emacs does not inherit the PATH from
;; the shell on a mac. On linux this is not a problem.
(straight-use-package 'exec-path-from-shell)
;; (if (memq window-system '(mac ns x))
;;     (require 'exec-path-from-shell)
;;   (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
;;   (exec-path-from-shell-initialize))

(setq exec-path-from-shell-variables '("PATH" "GOPATH"))
(exec-path-from-shell-initialize)

;; Use direnv to set environment variables for a specific directory in
;; a .envrc file
(use-package direnv
  :config (direnv-mode))

;; Search -----------------------------------------------------------------------
;;
;; Better minibuffer behaviour when searching for anything.
;;
;; TODO: add Embark to the mix.
(straight-use-package 'vertico)
(straight-use-package 'orderless)
(straight-use-package 'savehist)
(straight-use-package 'marginalia)
(straight-use-package 'consult)

(setq vertico-cycle t)
(vertico-mode)
(setq completion-styles '(basic substring partial-completion flex))
(savehist-mode)
(marginalia-mode)

(setq consult-project-root-function #'projectile-project-root)
;; https://github.com/minad/consult <- for keybinds
;; Set consult keymaps
(keymap-global-set "C-x b" 'consult-buffer)
(keymap-global-set "M-g o" 'consult-outline)
(keymap-global-set "M-s r" 'consult-ripgrep)
(keymap-global-set "M-s l" 'consult-line)
(keymap-global-set "M-g i" 'consult-imenu)
(keymap-global-set "M-g g" 'consult-goto-line)
(keymap-global-set "M-y" 'consult-yank-pop)

;; Codeium ----------------------------------------------------------------------
;;
;; Codeium is a free alternative for copilot.
(straight-use-package '(codeium :type git :host github :repo "Exafunction/codeium.el"))
(add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
(setq use-dialog-box nil)
(setq codeium-mode-line-enable
  (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
(add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)

;; Treesitter -------------------------------------------------------------------
;;
;; Use treesitter for better highlighting. At some point in the future
;; I want to investigate how to use treesitter for smarter selections
;; etc.

;; TODO: Add rust
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (zig "git@github.com:GrayJack/tree-sitter-zig.git")))

;; The following modes are getting a treesitter revamp:
(setq major-mode-remap-alist
  '((yaml-mode . yaml-ts-mode)
    (bash-mode . bash-ts-mode)
    (js2-mode . js-ts-mode)
    (typescript-mode . typescript-ts-mode)
    (json-mode . json-ts-mode)
    (css-mode . css-ts-mode)
    (python-mode . python-ts-mode)
    (rust-mode . rust-ts-mode)
;;    (go-mode . go-ts-mode)
    (dockerfile-mode . dockerfile-ts-mode)))

;; Terminal ---------------------------------------------------------------------
;;
;; Use EAT to have the best of both worlds with regards to a terminal
;; After the first install run: 'M-x eat-compile-terminfo' to have
;; normal keybinds
(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))
(straight-use-package 'all-the-icons)
(global-set-key (kbd "C-c t") 'eat)

;; Projectile -------------------------------------------------------------------
;;
;; Projectile is a project management tool. It is used to quickly
;; navigate projects.

(straight-use-package 'projectile)
(projectile-mode +1)
;; unmap project.el keybindings, THIS DOES NOT WORK
(global-unset-key (kbd "C-x p"))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Which-key --------------------------------------------------------------------
;;
;; Which-key is used to show keybindings when a prefix key is pressed.

(straight-use-package 'which-key)
(which-key-mode)

;; Magit ------------------------------------------------------------------------
;;
;; Magit is a git porcelain for emacs. It is used to interact with git
;; repositories.

(straight-use-package 'magit)

;; Elisp ------------------------------------------------------------------------

;; This might be required for copilot to not give warnings.
(setq lisp-indent-offset 2)

;; Org-mode ---------------------------------------------------------------------
;;
;; Org-mode is used for note taking, todo's, agenda's and much more.

(straight-use-package 'org)
(straight-use-package 'org-superstar)

(defvar org-columns 100)  ;; Set the column width for org-mode

(defun tim-org-setup ()
  "Setup org-mode with nice bullets and a line width."
  (org-superstar-mode 1)
  (org-indent-mode 1)
  (set-fill-column org-columns))
(add-hook 'org-mode-hook #'tim-org-setup)

;; Use markdown export for org-mode
(eval-after-load "org"
  '(require 'ox-md nil t))

;; Add my todo file as an agenda file to see an overview of my todo's in the
;; Emacs calendar.
(setq org-agenda-files (list "~/Dropbox/notes/todo.org"))

;; What languages should work within an org code block:
(org-babel-do-load-languages 'org-babel-load-languages
  '((shell . t)))

(setq org-src-fontify-natively t)

;; Markdown ---------------------------------------------------------------------
;;
;; For now this mode is only used because of the syntax
;; highlighting. At some point I may want to learn more about this
;; mode since it looks very interesting

(straight-use-package 'markdown-mode)
(defvar markdown-columns 100)  ;; Set the column width for org-mode

(defun tim-markdown-setup ()
  (set-fill-column markdown-columns))
(add-hook 'markdown-mode-hook #'tim-markdown-setup)

;; Eglot ------------------------------------------------------------------------
;;
(straight-use-package 'eglot)
(setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
    '(python-mode . ("pyright-langserver" "--stdio"))
    '(yaml-mode . ("yaml-language-server" "--stdio"))))

;; Corfu ------------------------------------------------------------------------
;; Autocomplete

(straight-use-package 'corfu)
(straight-use-package 'cape)
(global-corfu-mode)

;; Add Completion functions to corfu
(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-elisp-block)

;; Trigger completion manually using C-c i
(global-set-key (kbd "C-c i") 'completion-at-point)

;; Python -----------------------------------------------------------------------
(straight-use-package 'python-mode)
(defun tim-python-mode-hook ()
  ;; eglot should start when python mode starts.
  (eglot-ensure))
(add-hook 'python-mode-hook 'tim-python-mode-hook)

;; Yaml -------------------------------------------------------------------------
;;
;; Yaml mode for syntax highlighting.
;;
;; TODO: how to work with indenting?
(straight-use-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)

;; Json -------------------------------------------------------------------------
;;
;; Json mode for syntax highlighting.
(use-package json-mode)

;; Zig --------------------------------------------------------------------------
(use-package zig-mode)

;; Rust -------------------------------------------------------------------------
(use-package rust-mode)

;; Go ---------------------------------------------------------------------------
;;
;; TODO: I think we can get away with simply using 'go-ts-mode' + 'eglot'
(straight-use-package 'go-mode)
(defun tim-go-mode-hook ()
  (setq tab-width 2 indent-tabs-mode 1)
  (setq gofmt-command "goimports")
  (eglot-ensure)
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'tim-go-mode-hook)

;; Terraform --------------------------------------------------------------------
;;
;; Install 'terraform-ls': https://www.hashicorp.com/official-packaging-guide

(straight-use-package 'terraform-mode)
;; (setq terraform-indent-level 2) <- not for ns

(defun tim-terraform-mode-init ()
  ;; not sure I want to use this. It will change a lot of existing
  ;; terraform files
  (terraform-format-on-save-mode 1)
  ;; TODO: figure out keybinding. It looks nice
  (outline-minor-mode 1)
  ;; Start language server when opening terraform mode.
  (eglot-ensure))

(add-hook 'terraform-mode-hook 'tim-terraform-mode-init)

;; Keybindings ------------------------------------------------------------------
;;
;; Some custom keybinds.

;; Open todo file, this is also used for agenda/calendar
(global-set-key (kbd "C-c o t")
		(lambda() (interactive)(find-file "~/Dropbox/notes/todo.org")))

;; Open emacs init file
(global-set-key (kbd "C-c o o")
		(lambda() (interactive)(find-file "~/.config/emacs/init.el")))
(put 'dired-find-alternate-file 'disabled nil)
