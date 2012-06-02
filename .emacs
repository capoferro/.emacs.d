(add-to-list 'load-path "~/.emacs.d/plugins")

;; Scheme

;;; The basic settings
(setq scheme-program-name "scheme48"
      scheme-mit-dialect nil)

;;; Tell emacs about your special interpreters. This is from the #!
;;; line.
(add-to-list 'interpreter-mode-alist '("scsh" . scheme-mode))

;;; You want quack. Really.
(require 'quack)

;;; Can you please explain what I `really want' from Quack?

(setq quack-fontify-style 'emacs
      quack-default-program "scheme48"
      quack-newline-behavior 'newline)

;;; Examples for quick documentation access. Quack does similar stuff.
;;; (So does <http://twb.ath.cx/~twb/canon/scheme-lookup/>,
;;; without the extra baggage of Quack.)

(defun s48-doc ()
  "Browse the Scheme48 documentation."
  (interactive)
  (browse-url "file:///usr/share/doc/scheme48/html/s48manual.html"))

(defun scsh-doc ()
  "Browse the scsh documentation."
  (interactive)
  (browse-url "file:///usr/share/doc/scsh-doc/scsh-manual/html/man-Z-H-1.html"))

;;; Tell emacs about the indentation of some not-so-well-known
;;; procedures.

;; If you are running scheme48 download scheme48.el from 
;; http://www.emacswiki.org/cgi-bin/wiki/Scheme48Mode
;; And add this provide 
(require 'scheme48)

;; gauche
(put 'with-error-handler 'scheme-indent-function 1)     ; 'defun)
(put 'with-exception-handler 'scheme-indent-function 1)
(put 'with-exit-exception-handler 'scheme-indent-function 1)
(put 'with-exit-exception-handler* 'scheme-indent-function 2)
(put 'my-with-exception-handler 'scheme-indent-function 2)
(put 'for-debug 'scheme-indent-function 'defun)
(put 'test-expected 'scheme-indent-function 'defun)
(put 'call-with-input-string 'scheme-indent-function 1)
(put 'with-port-locking 'scheme-indent-function 1)

;; A customized indentation function for receive.
;; It is adapted from lisp-indent-specform.

;; This will indent RECEIVE as follows:
;; (receive params
;;          producer
;;   receiver)
;; instead of the `put' form above, which will indent as follows:
;; (receive params
;;     producer
;;   receiver)

;; Note: This could be made into a generalized function
;;   if it would be useful for other scheme functions.

(defun scheme-indent-receive (state indent-point normal-indent)
  (let ((containing-form-start (nth 1 state))
        (i 0)
        containing-form-column)
    ;; <snip documentation>
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case ()
                    (progn
                      (setq i (1+ i))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (cond ((= i 0)
           (+ containing-form-column (* 2 lisp-body-indent)))
          ((= i 1) normal-indent)
          (t (+ containing-form-column lisp-body-indent)))))

;; tell emacs to use this function for indenting receive
(put 'receive 'scheme-indent-function 'scheme-indent-receive)


;; Shortcuts

(global-set-key (kbd "C-x C-M-\\") 'align-regexp)

(fset 'reverse-return
   "\C-a\C-o")


;; Coffeescript
(require 'coffee-mode)

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
 '(ido-enable-flex-matching t)
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
