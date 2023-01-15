(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)

(set-frame-parameter nil 'alpha-background 100)
(add-to-list 'default-frame-alist '(alpha-background . 100))

(defun toggle-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha-background (frame-parameter nil 'alpha-background)))
    (set-frame-parameter nil 'alpha-background
     (if (eql (cond ((numberp alpha-background) alpha-background)
                    ((numberp (cdr alpha-background)) (cdr alpha-background))
                    )
              100)
         '75 '100))))

(use-package writeroom-mode
  :ensure t)

(use-package xah-fly-keys
  :ensure t
  :config
  (xah-fly-keys-set-layout "qwertz"))

(use-package meow
  :ensure t
  :custom
    (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  :config
    (meow-motion-overwrite-define-key
      '("j" . meow-next)
      '("k" . meow-prev)
      '("<escape>" . ignore))
    (meow-leader-define-key
      ;; SPC j/k will run the original command in MOTION state.
      '("j" . "H-j")
      '("k" . "H-k")
      ;; Use SPC (0-9) for digit arguments.
      '("1" . meow-digit-argument)
      '("2" . meow-digit-argument)
      '("3" . meow-digit-argument)
      '("4" . meow-digit-argument)
      '("5" . meow-digit-argument)
      '("6" . meow-digit-argument)
      '("7" . meow-digit-argument)
      '("8" . meow-digit-argument)
      '("9" . meow-digit-argument)
      '("0" . meow-digit-argument)
      '("ß" . meow-keypad-describe-key)
      '("´" . meow-cheatsheet))
    (meow-normal-define-key
      '("0" . meow-expand-0)
      '("9" . meow-expand-9)
      '("8" . meow-expand-8)
      '("7" . meow-expand-7)
      '("6" . meow-expand-6)
      '("5" . meow-expand-5)
      '("4" . meow-expand-4)
      '("3" . meow-expand-3)
      '("2" . meow-expand-2)
      '("1" . meow-expand-1)
      '("ß" . negative-argument)
      '("ö" . meow-reverse)
      '("," . meow-inner-of-thing)
      '("." . meow-bounds-of-thing)
      '("ü" . meow-beginning-of-thing)
      '("+" . meow-end-of-thing)
      '("a" . meow-append)
      '("A" . meow-open-below)
      '("b" . meow-back-word)
      '("B" . meow-back-symbol)
      '("c" . meow-change)
      '("d" . meow-delete)
      '("D" . meow-backward-delete)
      '("e" . meow-next-word)
      '("E" . meow-next-symbol)
      '("f" . meow-find)
      '("g" . meow-cancel-selection)
      '("G" . meow-grab)
      '("h" . meow-left)
      '("H" . meow-left-expand)
      '("i" . meow-insert)
      '("I" . meow-open-above)
      '("j" . meow-next)
      '("J" . meow-next-expand)
      '("k" . meow-prev)
      '("K" . meow-prev-expand)
      '("l" . meow-right)
      '("L" . meow-right-expand)
      '("m" . meow-join)
      '("n" . meow-search)
      '("o" . meow-block)
      '("O" . meow-to-block)
      '("p" . meow-yank)
      '("q" . meow-quit)
      '("Q" . meow-goto-line)
      '("r" . meow-replace)
      '("R" . meow-swap-grab)
      '("s" . meow-kill)
      '("t" . meow-till)
      '("u" . meow-undo)
      '("U" . meow-undo-in-selection)
      '("v" . meow-visit)
      '("w" . meow-mark-word)
      '("W" . meow-mark-symbol)
      '("x" . meow-line)
      '("X" . meow-goto-line)
      '("z" . meow-save)
      '("Z" . meow-sync-grab)
      '("y" . meow-pop-selection)
      '("ä" . repeat)
      '("<escape>" . ignore))
    (meow-global-mode 0))


(use-package elfeed
  :ensure t
  :config
  (setq elfeed-show-entry-switch 'display-buffer))

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org))

(use-package tex
  :ensure auctex
  :hook (LaTex-mode . (lambda ()
                        (push (list 'output-pdf "Zathura")
                              TeX-view-program-selection))))

(use-package avy
  :ensure t
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line))

(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  (aw-minibuffer-flag t)
  (aw-dispatch-always t)
  (aw-dispatch-alist
   '(
     (?x aw-delete-window "delete")
     (?m aw-swap-window "swap")
     (?M aw-move-window "move")
     (?n aw-flip-window "aw-flip-window")
     (?u aw-switch-buffer-other-window "switch buffer other window")
     (?f aw-split-window-fair "split fair")
     (?v aw-split-window-vert "split vertical")
     (?r aw-split-window-horz "split horizontal (to the _r_ight)")
     (?o aw-delete-other-windows "delete other windows")
     (?? aw-show-dispatch-help "help"))
   "List of actions for `aw-dispatch-default`.")
  :bind ("M-o" . ace-window))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package consult
  :ensure t
  :defer t)

(use-package consult-dir
  :ensure t
  :after consult
  :custom
  (consult-dir-default-command 'project-find-file)
  :config
  (recentf-mode)
  :general
  (:keymaps 'insert
            "C-x C-d" 'consult-dir)
  (:keymaps 'vertico-map
            "C-x C-d" 'consult-dir
            "C-x C-j" 'consult-dir-jump-file))

(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :custom
  (corfu-cycle t)
  :general
  (general-def 'insert "C-n" 'completion-at-point)
  (:keymaps 'corfu-map
            [remap completion-at-point] 'corfu-next
            [remap evil-complete-next] 'corfu-next
            [remap evil-complete-previous] 'corfu-previous
            "C-n"  'corfu-next
            "C-p"  'corfu-previous))

;; (use-package embark)
;; (use-package embark-consult)

(use-package helpful
  :general
  ([remap describe-function] 'helpful-function)
  ([remap describe-symbol] 'helpful-command)
  ([remap describe-variable] 'helpful-variable)
  ([remap describe-key] 'helpful-key))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.1))



(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package wgrep
  :defer 2
  :ensure t)

(use-package magit
  :defer 2
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package eglot
  :ensure t
  :ghook
  ('go-mode-hook #'eglot-ensure)
  ('nix-mode-hook #'eglot-ensure))

(use-package consult-eglot
  :ensure t
  :after eglot)

(use-package go-mode
  :ensure t
  :mode
  ("\\.go\\'" . go-mode)
  :interpreter
  ("go" . go-mode)
  )

(use-package nix-mode
  :ensure t
  :mode
  ("\\.nix\\'" . nix-mode)
  :interpreter
  ("rnix-lsp" . nix-mode))

(defun load-org-tempo ()
  (require 'org-tempo))

(use-package org
  :ensure t
  :mode
  ("\\.org\\'" . org-mode)
  :interpreter
  ("org" . org-mode)
  :custom
  (org-ellipsis " ▼")
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-confirm-babel-evaluate nil)
  (org-structure-template-alist
   '(
     ;; custom
     ("sh" . "src shell")
     ("el" . "src emacs-lisp")
     ;; default
     ("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse")))
  :gfhook
  #'visual-line-mode
  #'load-org-tempo
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t))))

(setq ring-bell-function 'ignore)   ; Disable alarm bell
(setq inhibit-startup-message t)    ; Remove startup message
(setq initial-scratch-message "Ready for work? Use C-h for help.")
(setq cursor-type 'hbar)

(menu-bar-mode -1)      ; Disable the menu bar
(tool-bar-mode -1)      ; Disable the toolbar
(scroll-bar-mode -1)    ; Disable visible scrollbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Give some breathing room

(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t))

;; Display the cursor column in modeline
(column-number-mode t)

(dolist (mode '(prog-mode-hook
                text-mode-hook))
  (add-hook mode #'(lambda()
                    (set-fill-column 78)
                    (auto-fill-mode)
                    (display-fill-column-indicator-mode))))

;; Enable line numbers
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :ensure t
  :ghook
  'prog-mode-hook)

(setq epg-pinentry-mode 'loopback)

;; (use-package password-store) ; auth-source-pass

;; (use-package hercules)

(use-package esup
  :ensure t
  :custom
  ;; Work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  ;; Ref: https://github.com/jschaf/esup/issues/54#issuecomment-651247749
  esup-depth 0)
