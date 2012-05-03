;; Shortcuts

(global-set-key (kbd "C-x C-M-\\") 'align-regexp)

(fset 'reverse-return
   "\C-a\C-o")



;; git
(add-to-list 'load-path "/usr/local/Cellar/git/1.7.10/share/git-core/contrib/emacs")
(add-to-list 'vc-handled-backends 'GIT)
(autoload 'git-status "git" "Entry point into git-status mode." t)
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

;keybindindings for git
(global-set-key "\C-xgs" 'git-status)

;; linum
(global-linum-mode 1)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Flymake
(require 'flymake)

;;;; Show error messages
(defun my-flymake-show-help () 
  (interactive)
  (when (get-char-property (point) 'flymake-overlay) 
    (let ((help (get-char-property (point) 'help-echo))) 
      (if help (message "%s" help)))))

;;;; Flymake Ruby
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	  (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

;; Haml mode
(add-to-list 'load-path "~/.emacs.d/plugins/haml-mode")
(require 'haml-mode)

;; Sass mode
(add-to-list 'load-path "~/.emacs.d/plugins/sass-mode")
(require 'sass-mode)

;; Scss mode
(add-to-list 'load-path "~/.emacs.d/plugins/scss-mode")
(require 'scss-mode)

;; Feature mode
(add-to-list 'load-path "~/.emacs.d/plugins/cucumber.el")
;; optional configurations
;; default language if .feature doesn't have "# language: fi"
;(setq feature-default-language "fi")
;; point to cucumber languages.yml or gherkin i18n.yml to use
;; exactly the same localization your cucumber uses
;(setq feature-default-i18n-file "/path/to/gherkin/gem/i18n.yml")
;; and load it
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; Ido mode
(require 'ido)
(ido-mode)

;; YASnippet
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)

;; Autogen custom vars
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ido-case-fold t)
 '(ido-enable-flex-matching t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
