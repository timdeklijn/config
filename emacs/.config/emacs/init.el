;; My emacs configuration (v3).
;; 
;; For this one I want to really utilize all packages that I
;; install. I want to start writing my own utilities when I want emacs
;; to do something for me. Lastly, I want to not use a package
;; manager, but a simply bash script to clone all the repos with
;; packages I want. This because I want to use non-melpa packages like
;; copilot and I do not feel like using straight or anything.

;; TODO:
;;   - Split over multiple files

;; location to save packages to
(defvar my-packages-dir "~/.config/emacs/packages/")
;; Load all packages into path so they can be 'required'.
(let ((default-directory my-packages-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; NOTE: not sure what this is used for:
(setq user-full-name "Tim de Klijn")

;; Write generated elisp code to the 'custom-file'
(setq custom-file (make-temp-file "emacs-custom.el"))

;; Byte compile everything for a nice speedup. Put this in a function
;; because there always will be some warnings which I do not want when
;; I reload my config.
(defun my-byte-compile ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.config/emacs/") 0))
(my-byte-compile)

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

;; No more startup screen
(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      make-backup-files nil)

;; Specify font and theme
(set-face-attribute 'default nil
		    :font "BlexMono Nerd Font Mono"
		    :height 180)
(setq-default line-spacing 0.3)

;; Make sure the compilation mode can handle ANSI color codes to see colors: for
;; example passing/failing tests.
(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

(setq compilation-scroll-output t)

;; Clean up the modeline by hiding minor modes
(require 'minions)
(minions-mode 1)

;; Configure emacs Doom themes --------------------------------------------------
(require 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t
      doom-themes-padded-modeline t)
(load-theme 'doom-one t)

;; Environment ------------------------------------------------------------------

;; If we are on a mac, we need to set the PATH and GOPATH environment
;; differently. This is because Emacs does not inherit the PATH from
;; the shell on a mac. On linux this is not a problem.
(if (memq window-system '(mac ns x))
    (require 'exec-path-from-shell)
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

;; Use direnv to set environment variables for a specific directory in
;; a .envrc file
(require 'direnv)
(direnv-mode)

;; Search -----------------------------------------------------------------------
;;
;; NOTES:
;;   - Dependencies: compat <- Required by vertico
(require 'vertico)
(require 'marginalia)
(require 'orderless)

;; Vertico is a (minibuffer) completion framework.
(vertico-mode)
;; Use orderless to have fuzzy like matching in vertico buggers.
(setq completion-styles '(basic substring partial-completion flex))

;; Save commands used to be on top when searching again.
(savehist-mode)

;; Marginalia shows the docstrings of the functions or other
;; information while searching in a vertico buffer.
(marginalia-mode)

;; Copilot ----------------------------------------------------------------------
;;
;; One of the reasons to not use a package manager. I need copilot in
;; my life and now I can manually add it without any package manager
;; complaining it is not in ELPA/MELPA.

(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)

;; TODO: think about triggering copilot with a keybind and not always
;; have suggestions appear
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; Treesitter -------------------------------------------------------------------
;;
;; Use treesitter for better highlighting. At some point in the future I want to
;; investigate how to use treesitter for smarter selections etc.

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

(require 'eat)
(global-set-key (kbd "C-c t") 'eat)

;; Projectile -------------------------------------------------------------------
;;
;; Projectile is a project management tool. It is used to quickly

(require 'projectile)
(projectile-mode +1)
(global-unset-key (kbd "C-x p")) ;; unmap project.el keybindings
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Which-key --------------------------------------------------------------------
;;
;; Which-key is used to show keybindings when a prefix key is pressed.

(require 'which-key)
(which-key-mode)

;; Magit ------------------------------------------------------------------------
;;
;; Magit is a git porcelain for emacs. It is used to interact with git
;; repositories.

;; NOTE: magit installation from source is weird. Frist, I need to run
;; 'make' in the magit repo. Then I could not get 'transient', a
;; dependency of magit, updated using a simple git clone. In the end I
;; needed to run `install-package transient' to get it working. Not
;; great, need to find a better solution for that.
(require 'transient)
(require 'magit)

;; Elisp ------------------------------------------------------------------------

;; This might be required for copilot to not give warnings.
(setq lisp-indent-offset 2)

;; Org-mode ---------------------------------------------------------------------
;;
;; Org-mode is used for note taking, todo's, agenda's and much more.

(require 'org)
(require 'org-superstar)  ;; Better org-mode bullets
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

;; Markdown ---------------------------------------------------------------------
;;
;; For now this mode is only used because of the syntax
;; highlighting. At some point I may want to learn more about this
;; mode since it looks very interesting

(require 'markdown-mode)

;; Eglot ------------------------------------------------------------------------
;;
;; TODO: Get Eglot to work
(require 'eglot)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
    '(python-mode . ("pyright-langserver" "--stdio"))
    '(yaml-mode . ("yaml-language-server" "--stdio"))))

;; Corfu ------------------------------------------------------------------------
;;

(require 'corfu)
(require 'cape)
(global-corfu-mode)

;; Add Completion functions to corfu
(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-elisp-block)

;; Trigger completion manually using C-c i::
(global-set-key (kbd "C-c i") 'completion-at-point)

;; Python -----------------------------------------------------------------------
;;
;; TODO: Add some basic python functionality

;; Yaml -------------------------------------------------------------------------
;;
;; Yaml mode for syntax highlighting.
;;
;; TODO: how to work with indenting?
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Rust -------------------------------------------------------------------------
;;
;; TODO: Add some basic rust functionality

;; Json -------------------------------------------------------------------------
;;
;; Json mode for syntax highlighting.
(require 'json-mode)

;; Keybindings ------------------------------------------------------------------
;;
;; Some custom keybinds.

;; Open todo file, this is also used for agenda/calendar
(global-set-key (kbd "C-c o t")
		(lambda() (interactive)(find-file "~/Dropbox/notes/todo.org")))

;; Open emacs init file
(global-set-key (kbd "C-c o o")
		(lambda() (interactive)(find-file "~/.config/emacs/init.el")))
