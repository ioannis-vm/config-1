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
  '(elpy                            ;; Emacs Lisp Python Environment
    py-autopep8                     ;; Run autopep8 on save
    blacken                         ;; Black formatting on save
    magit                           ;; Git integration
    auctex                          ;; Integrated environment for *TeX*
    markdown-mode                   ;; markdown mode
    adaptive-wrap                   ;; Smart line-wrapping with wrap-prefix
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
(load-theme 'material t)            ;; Load material theme
(setq inhibit-startup-message t)    ;; Hide the startup message
(tool-bar-mode -1)                  ;; Hide toolbar
(global-linum-mode t)               ;; Enable line numbers globally


;; ===================================
;; Dired
;; ===================================

(put 'dired-find-alternate-file 'disabled nil)

;; ===================================
;; Python
;; ===================================

(elpy-enable)
(setq elpy-rpc-virtualenv-path 'current)
(setenv "WORKON_HOME" "~/anaconda3/envs")

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(add-hook 'elpy-mode-hook #'visual-line-mode)
(add-hook 'elpy-mode-hook #'adaptive-wrap-prefix-mode)

;; ===================================
;; Org mode
;; ===================================

;; enable adaptive line wrapping
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'adaptive-wrap-prefix-mode)

;; ===================================
;; Markdown mode
;; ===================================

(add-to-list 'auto-mode-alist '("\\.rmd\\'" . markdown-mode))
(add-hook 'markdown-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook #'adaptive-wrap-prefix-mode)






;; User-Defined init.el ends here

