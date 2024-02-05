;; My emacs configuration (v3).
;; TODO:
;;   - Split over multiple files

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

;; NOTE: not sure what this is used for:
(setq user-full-name "Tim de Klijn")

;; Write generated elisp code to the 'custom-file'
(setq custom-file (make-temp-file "emacs-custom.el"))

;; Do not type 'yes' or 'no' when prompted
(defalias 'yes-or-no-p #'y-or-n-p)

;; Simple customizations
(menu-bar-mode -1)
(delete-selection-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode)
(global-hl-line-mode -1)	      ;; highlight the cursor line
(global-auto-revert-mode t)           ;; update buffer when file is updated
(global-display-line-numbers-mode -1) ;; show line numbers
(pixel-scroll-precision-mode 1)       ;; smooth scrolling

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

;; Font: https://ifonts.xyz/comic-code-complete-font-family.html
;; Specify font and theme
(set-face-attribute 'default nil
  :family "Comic Code"
  :height 180)

(setq-default line-spacing 0.2)

;; Change faces from:
;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
(custom-theme-set-faces
  'user
  '(markdown-inline-code-face ((t (:inherit default :foreground "light green")))))

;; Make sure the compilation mode can handle ANSI color codes to see colors: for
;; example passing/failing tests.
(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)
(setq compilation-scroll-output t)

;; Clean up the modeline by hiding minor modes
(straight-use-package
 '(minions
   :type git
   :host github
   :repo "tarsius/minions"))
(require 'minions)
(minions-mode 1)

;; Configure emacs Spacemacs themes ---------------------------------------------
(straight-use-package 'modus-themes)
;; In all of the following, WEIGHT is a symbol such as `semibold',
;; `light', `bold', or anything mentioned in `modus-themes-weights'.
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui nil
      modus-themes-custom-auto-reload t
      modus-themes-disable-other-themes t
      modus-themes-prompts '(italic bold)
      modus-themes-completions
      '((matches . (extrabold))
        (selection . (semibold italic text-also)))
      modus-themes-org-blocks nil
      modus-themes-headings
      '((1 . (variable-pitch 1.5))
        (2 . (1.3))
        (agenda-date . (1.3))
        (agenda-structure . (variable-pitch light 1.8))
        (t . (1.1))))
(load-theme 'modus-vivendi-tinted t)

;; Environment ------------------------------------------------------------------

;; If we are on a mac, we need to set the PATH and GOPATH environment
;; differently. This is because Emacs does not inherit the PATH from
;; the shell on a mac. On linux this is not a problem.
(straight-use-package 'exec-path-from-shell)
(if (memq window-system '(mac ns x))
    (require 'exec-path-from-shell)
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

;; Use direnv to set environment variables for a specific directory in
;; a .envrc file
(straight-use-package 'direnv)
(direnv-mode)

;; Search -----------------------------------------------------------------------
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
;; https://github.com/minad/consult <- for keybinds
(setq consult-project-root-function #'projectile-project-root)

;; Copilot ----------------------------------------------------------------------
;;
;; One of the reasons to not use a package manager. I need copilot in
;; my life and now I can manually add it without any package manager
;; complaining it is not in ELPA/MELPA.
;; https://emacsredux.com/blog/2023/03/12/install-a-package-from-a-vcs-repository/
;; run this function once

(straight-use-package
 '(dash :type: git
	:host github
	:repo "magnars/dash.el"))

(straight-use-package
 '(s :type git
     :host github
     :repo "magnars/s.el"))

(straight-use-package
 '(editorconfig-emacs :type git
		      :host github
		      :repo "editorconfig/editorconfig-emacs"))

(straight-use-package
 '(copilot :type git
	   :host github
	   :repo "copilot-emacs/copilot.el"))

;; TODO: copilot is broken or something.
;; (require 'copilot)
;; (add-hook 'prog-mode-hook 'copilot-mode)

;; TODO: think about triggering copilot with a keybind and not always
;; have suggestions appear
;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; Treesitter -------------------------------------------------------------------
;;
;; Use treesitter for better highlighting. At some point in the future I want to
;; investigate how to use treesitter for smarter selections etc.

;; TODO: Add rust
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (docker "https://github.com/camdencheek/tree-sitter-dockerfile")
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
    (rust-mode . rust-ts-mode)))

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

(straight-use-package 'projectile)
(projectile-mode +1)
(global-unset-key (kbd "C-x p")) ;; unmap project.el keybindings
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

(defun my-org-setup ()
  "Setup org-mode with nice bullets and a line width."
  (org-superstar-mode 1)
  (set-fill-column org-columns))
(add-hook 'org-mode-hook #'my-org-setup)

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

(defun my-markdown-setup ()
  "Setup org-mode with nice bullets and a line width."
  (set-fill-column markdown-columns))
(add-hook 'markdown-mode-hook #'my-markdown-setup)

;; Eglot ------------------------------------------------------------------------
;;
(straight-use-package 'eglot)
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
;;
;; TODO: Add some basic python functionality
(straight-use-package 'python-mode)

;; Yaml -------------------------------------------------------------------------
;;
;; Yaml mode for syntax highlighting.
;;
;; TODO: how to work with indenting?
(straight-use-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Rust -------------------------------------------------------------------------
;;
;; TODO: Add some basic rust functionality

;; Json -------------------------------------------------------------------------
;;
;; Json mode for syntax highlighting.
(straight-use-package 'json-mode)

;; Zig --------------------------------------------------------------------------
(straight-use-package 'zig-mode)

;; Keybindings ------------------------------------------------------------------
;;
;; Some custom keybinds.

;; Open todo file, this is also used for agenda/calendar
(global-set-key (kbd "C-c o t")
		(lambda() (interactive)(find-file "~/Dropbox/notes/todo.org")))

;; Open emacs init file
(global-set-key (kbd "C-c o o")
		(lambda() (interactive)(find-file "~/.config/emacs/init.el")))
