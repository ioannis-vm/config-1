;;   __
;;   \ \/\   /\/\/\     John Vouvakis Manousakis
;;    \ \ \ / /    \    emacs configuration file
;; /\_/ /\ V / /\/\ \
;; \___/  \_/\/    \/
;;

;; ===================================
;; MELPA Package Support
;; ===================================

;; Enables basic packaging support
(require 'package)

; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; Installs packages
;; myPackages contains a list of package names
(defvar myPackages
  '(use-package
    elpy                            ;; Emacs Lisp Python Environment
    py-autopep8                     ;; Run autopep8 on save
    blacken                         ;; Black formatting on save
    projectile                      ;; Manage and navigate projects
    magit                           ;; Git integration
    auctex                          ;; Integrated environment for *TeX*
    markdown-mode                   ;; markdown mode
    adaptive-wrap                   ;; Smart line-wrapping with wrap-prefix
    ir-black-theme                  ;; A nice theme
    irony                           ;; C/C++ minor mode powered by libclang
    irony-eldoc                     ;; irony-mode support for eldoc-mode
    flycheck                        ;; On-the-fly syntax checking
    flycheck-irony                  ;; Flycheck: C/C++ support via Irony
    company-irony                   ;; company-mode completion back-end for irony-mode
    rtags                           ;; A front-end for rtags
    company-rtags
    haskell-mode                    ;; Work with haskell files
    rainbow-mode		    ;; Colorize color names in buffers
    nov                             ;; epub reader. Oh, yeah!
    yaml-mode                       ;; Syntax highlighting for yaml files
    json-mode                       ;; Major mode for editing JSON files
    org-ref                         ;; citations in org-mode
    bibtex-utils                    ;; Provides utilities for extending BibTeX mode
    elfeed                          ;; an Emacs Atom/RSS feed reader
    )
  )

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)

;; ===================================
;; Basic Look
;; ===================================

;;font
(add-to-list 'default-frame-alist
             '(font . "Ubuntu Mono-10"))
;; theme
(load-theme 'ir-black t)
;; transparency
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

(add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
		;; font
		(add-to-list 'default-frame-alist
			     '(font . "Ubuntu Mono-10"))
		;; theme
                ;; (load-theme 'ir-black t)
		;; transparency
		(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
		)))
(setq inhibit-startup-message t)    ;; Hide the startup message
(tool-bar-mode -1)                  ;; Hide toolbar
(menu-bar-mode -1)                  ;; Hide menu bar
(scroll-bar-mode -1)                ;; Hide scrollbar
(show-paren-mode 1)                 ;; Show parenthesis matching

;; ===================================
;; +Transparency
;; ===================================


(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
		    ((numberp (cdr alpha)) (cdr alpha))
		    ;; Also handle undocumented (<active> <inactive>) form.
		    ((numberp (cadr alpha)) (cadr alpha)))
	      100)
	 '(93 . 93) '(100 . 100)))))
(global-set-key (kbd "C-c C-t") 'toggle-transparency)

;; ===================================
;; General Behavior
;; ===================================

;; backup files
(setq
 make-backup-files t
 vc-make-backup-files t
 backup-directory-alist
 '(("." . ".~"))
 version-control t
 kept-new-versions 1000
 delete-old-versions t)
;; backup on every save
(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook  'force-backup-of-buffer)

;; boost pointer movement speed
(setq auto-window-vscroll nil)

;; replace highlighted text when typing over it
(delete-selection-mode 1)

;; add a space after line numbers
(setq linum-format "%d ")

;; spell check
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

;; ===================================
;; Dired
;; ===================================

(put 'dired-find-alternate-file 'disabled nil)
;; move files quickly from split view by pressing C
(setq dired-dwim-target t)

(defun open-terminal-in-workdir ()
  (interactive)
  (call-process-shell-command
   (concat "alacritty") nil 0))
(global-set-key (kbd "C-c t") 'open-terminal-in-workdir)

(defun open-filemanager-in-workdir ()
  (interactive)
  (call-process-shell-command
   (concat "thunar") nil 0))
(global-set-key (kbd "C-c f") 'open-filemanager-in-workdir)
(setq
 dired-kill-when-opening-new-dired-buffer t
 )

;; ===================================
;; Ibuffer
;; ===================================

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; ===================================
;; Python
;; ===================================

(elpy-enable)
(setq elpy-rpc-virtualenv-path 'current)
(load "~/.emacs_workon_home")

;;(require 'py-autopep8)
;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; this has been causing some issues..

(add-hook 'elpy-mode-hook 'visual-line-mode)
(add-hook 'elpy-mode-hook 'adaptive-wrap-prefix-mode)

;; syntax checking
(add-hook 'elpy-mode-hook 'flycheck-mode)
(add-hook 'elpy-mode-hook 'flycheck-mode)

;; display line numbers
(add-hook 'elpy-mode-hook 'linum-mode)

;; display color tags with their color
(add-hook 'elpy-mode-hook 'rainbow-mode)

;; always scroll to the bottom of the python shell buffer
(add-hook 'inferior-python-mode-hook
          (lambda ()
            (setq comint-move-point-for-output t)))

;; enable code folding
(add-hook 'elpy-mode-hook 'hs-minor-mode)

;; electric pair mode
(add-hook 'elpy-mode-hook 'electric-pair-mode)


;; ===================================
;; C++
;; ===================================

;; https://syamajala.github.io/c-ide.html
(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

;; source code completion
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; company mode with irony
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-backends (delete 'company-semantic company-backends))
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-irony))

;; tab completion with no delay
(setq company-idle-delay 0)
(define-key c-mode-map [(tab)] 'company-complete)
(define-key c++-mode-map [(tab)] 'company-complete)

;; syntax checking
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

;; line numbers
(add-hook 'c++-mode-hook 'linum-mode)
(add-hook 'c-mode-hook 'linum-mode)

;; ===================================
;; Org mode
;; ===================================

;; enable adaptive line wrapping
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'adaptive-wrap-prefix-mode)
(require 'org-ref)
(setq org-ref-default-citation-link "citep")
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
(setq org-support-shift-select t)
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))
(setq org-babel-python-command "~/anaconda3/envs/computing/bin/python")

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(when (string= system-name "Precision-5530")
  (load "~/.emacs_gtd"))

;; word count
(add-hook 'org-mode-hook #'wc-mode)
;; instant spell checking
(add-hook 'org-mode-hook 'flyspell-mode)

;; ===================================
;; Markdown mode
;; ===================================

(add-to-list 'auto-mode-alist '("\\.rmd\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'adaptive-wrap-prefix-mode)
(add-hook 'markdown-mode-hook 'linum-mode)

;; ===================================
;; LaTeX
;; ===================================

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'adaptive-wrap-prefix-mode)
(add-hook 'LaTeX-mode-hook 'linum-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) 
(setq TeX-view-program-selection '((output-pdf "Zathura")))
(setq TeX-save-query nil)
(setq tex-fontify-script nil)
(setq font-latex-fontify-script nil)
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout)")))
;; electric pair mode
(add-hook 'LaTeX-mode-hook 'electric-pair-mode)
;; instant spell checking
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; BibLaTeX settings
;; bibtex-mode
(setq bibtex-dialect 'biblatex)

;; ===================================
;; Epub
;; ===================================

;; https://depp.brause.cc/nov.el/
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq nov-text-width 80)
(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
			   :height 1.3)
  (setq line-spacing 0.3))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

;; ===================================
;; yaml files
;; ===================================
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; mu4e location
(when (string= system-name "Precision-5530")
  (load "~/.emacs_mu4e"))

;; ===================================
;; elfeed
;; ===================================

(setq elfeed-feeds
      '(
	("https://www.engineeringvillage.com/rss/feed.url?queryID=M5162486e180a99060f7M7ffd127001&SYSTEM_PT=t" nonstructural components)
	)
      )

;; ===================================
;; end
;; ===================================
