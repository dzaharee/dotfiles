;; Emacs config & auto installing of packages

;; package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; packages we want
(defvar package-list '(evil                    ;; vimmy emacs
                       dash                    ;; required for clojure-mode
                       pkg-info                ;; "
                       color-theme-solarized   ;; color theme
                       smex                    ;; Smart M-x
                       ido-ubiquitous          ;; smart-completion everywhere
                       clojure-mode            ;; clojure support
                       clojure-test-mode       ;; clojure test support
                       better-defaults         ;; minimal emacs default "fixing"
                       cider                   ;; Clojure IDE and Repl for emacs
                       paredit                 ;; manage s-expressions
                       ))
(dolist (p package-list)
  (when (not (package-installed-p p))
    (package-install p)))

;; evil
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

;; Don't create backup files, and move auto-save files
(setq make-backup-files nil)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "M-}") 'paredit-forward-barf-sexp))
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp))
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "M-{") 'paredit-backward-barf-sexp))

;; solarized color theme
(load-theme 'solarized-light t)

;; fix colors we don't care for
(set-face-attribute 'font-lock-builtin-face t :foreground "blue")
(set-face-attribute 'font-lock-constant-face t :foreground "green")
(set-face-attribute 'font-lock-keyword-face t :foreground "red")
(set-face-attribute 'font-lock-preprocessor-face t :foreground "brightmagenta")
(set-face-attribute 'show-paren-match t :foreground "white" :background "brightblue")

;; show TODO: and FIXME: as a different color
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(TODO\\|FIXME\\):" 1 font-lock-warning-face t)))))

;; misc clojure stuff
(add-hook 'clojure-mode-hook 'paredit-mode)     ; paredit w/ clojure
(add-hook 'cider-repl-mode-hook 'paredit-mode)  ; paredit in the repl
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)   ; eldoc in repl
(setq nrepl-hide-special-buffers t)
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 4000)
(setq cider-repl-history-file "~/.emacs.d/repl-history")    ; save repl history
(setq cider-popup-stacktraces nil)  ; stop error buffer popping up when not in repl
(setq cider-repl-use-clojure-font-lock t)   ; use clojure-mode for coloring cider (also gets use our custom stuff)
(setq cider-repl-use-pretty-printing t)     ; pretty print by default

;; add more syntax highlighting to clojure-mode
(defface clojure-special-chars
  '((t (:foreground "red")))
  "Used for clojure special characters `~@#'%")
(defface clojure-delimiter-chars
  '((t (:foreground "yellow")))
  "Used for clojure delimiters [](){}")
(add-hook 'clojure-mode-hook (lambda () (font-lock-add-keywords nil '(("[`~@#'%]" . 'clojure-special-chars)
                                                                      ("[][(){}]" . 'clojure-delimiter-chars)))))

;; smex
(global-set-key  (kbd "M-x") 'smex)
(global-set-key  (kbd "M-X") 'smex-major-mode-commands)

;; scroll one line at a time, with a margin
(setq scroll-conservatively 10000)
(setq scroll-margin 3)

;; auto indent new lines
(define-key global-map (kbd "RET") 'newline-and-indent)

;; show matching parens
(show-paren-mode 1)

;; trailing whitespace
(setq-default show-trailing-whitespace t)

;; make tabs work how we like (width 4 w/ spaces)
(setq default-tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))
(setq-default indent-tabs-mode nil)
(defun better-tab ()
  "When at column 0, indent the line, otherwise do a normal tab"
  (interactive)
  (if (= 0 (current-column))
         (funcall indent-line-function)
         (tab-to-tab-stop)))
(global-set-key (kbd "TAB") 'better-tab)
