;; emacs 24.3.50
(custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))

;; all backups goto ~/.backups instead in the current directory
(setq backup-directory-alist (quote (("." . "~/.backups"))))

;; Setting English Font
(set-face-attribute
'default nil :font "DejaVu Sans Mono")
 
;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "Microsoft YaHei" :size 12)))
;; ibus-mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/ibus-el/"))
(require 'ibus)
;; Turn on ibus-mode automatically after loading .emacs
(add-hook 'after-init-hook 'ibus-mode-on)
;; Choose your key to toggle input status:
(ibus-define-common-key ?\C-\s nil)
(global-set-key (kbd "C-\\") 'ibus-toggle) ;;通过Ctrl+\切换输入法
(setq ibus-cursor-color '("red" "blue"))
(custom-set-variables '(ibus-python-shell-command-name "python2"))

;; gnu-elpa marmalade melpa
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; emmet-mode 1.0.5 on marmalade
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook 'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
(setq emmet-move-cursor-between-quotes t)
(global-set-key (kbd "C-<tab>") 'emmet-expand-line)

;; yasnippet 0.6.1 on marmalade
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
;; git submodule add git://github.com/AndreaCrotti/yasnippet-snippets.git elpa/yasnippet-0.6.1/yasnippet-snippets
(yas/load-directory "~/.emacs.d/elpa/yasnippet-0.6.1/yasnippet-snippets")

;;auto-complete 1.4 on marmalade
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-compete-1.4/dict")
(ac-config-default)
(global-set-key "\M-/" 'auto-complete)  ;; 补全的快捷键，用于需要提前补全
;; 选择菜单项的快捷键
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; graphviz dot-mode
;;(load-file "~/.emacs.d/graphviz-dot-mode.el")

;; c-style comment for asm-mode 
(add-hook 'asm-mode-hook 
      (lambda () (setq comment-start "/* " comment-end " */")))

;; always show line numbers    
(global-linum-mode t) 
(setq linum-format "%d")  ;set format

;;paredit-mode for scheme
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

;;;;;;;;;;;;
;; Scheme 
;; sudo apt-get install racket
;;;;;;;;;;;;
(require 'cmuscheme)
(setq scheme-program-name "racket")         ;; 如果用 Petite 就改成 "petite"

;; bypass the interactive question and start the default interpreter
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))

(defun scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (find "*scheme*"
               (mapcar (lambda (w) (buffer-name (window-buffer w)))
                       (window-list))
               :test 'equal))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))

(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))

(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))

(add-hook 'scheme-mode-hook
  (lambda ()
    (paredit-mode 1)
    (define-key scheme-mode-map (kbd "<f5>") 'scheme-send-last-sexp-split-window)
    (define-key scheme-mode-map (kbd "<f6>") 'scheme-send-definition-split-window)))

;;pareface for scheme
(require 'parenface)
(set-face-foreground 'paren-face "DimGray")

;;sbcl+slime for common lisp
;;sudo apt-get install sbcl
;;git submodule add git://github.com/nablaone/slime lisp/slime/
(add-to-list 'load-path "~/.emacs.d/lisp/slime/")  ; your SLIME directory
(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(require 'slime)
(slime-setup)

;;org-jekyll publish to github pages
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(require 'org-jekyll)
(setq org-src-fontify-natively t)

(require 'ecb)

;;cedet
;;cedet setup - https://github.com/alexott/emacs-configs/blob/master/rc/emacs-rc-cedet.el
(require 'cedet) 
    (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode  
                                      global-semanticdb-minor-mode  
                                      global-semantic-idle-summary-mode  
                                      global-semantic-mru-bookmark-mode
				      global-semantic-highlight-func-mode
    ))  
    (semantic-mode 1)  
    (global-semantic-show-parser-state-mode 1)  

(global-ede-mode 1)   ; Enable the Project management system

;;; 快捷键
(defun my-cedet-hook()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)

  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key (kbd "M-n") 'semantic-ia-complete-symbol-menu)

  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key (kbd "M-/") 'semantic-complete-analyze-inline)

  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-cd" 'semantic-ia-fast-jump)
  (local-set-key "\C-cr" 'semantic-symref-symbol)
  (local-set-key "\C-cR" 'semantic-symref)

  ;;; c/c++ setting
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)

  ;;; work with auto-complete
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic))

(add-hook 'c-mode-common-hook 'my-cedet-hook)

;;(require 'xcscope)
(add-hook 'c-mode-common-hook '(lambda() (require 'xcscope)))
(cscope-setup) ;;打开cscope-minor-mode,才能显示cscope菜单与使用C-c s绑定的快捷键
(setq cscope-do-not-update-database t)
(define-key global-map [(control f3)]  'cscope-set-initial-directory)
(define-key global-map [(control f4)]  'cscope-unset-initial-directory)
(define-key global-map [(control f5)]  'cscope-find-this-symbol)
(define-key global-map [(control f6)]  'cscope-find-global-definition)
(define-key global-map [(control f7)]  'cscope-find-global-definition-no-prompting)
(define-key global-map [(control f8)]  'cscope-pop-mark)
(define-key global-map [(control f9)]  'cscope-history-forward-line)
(define-key global-map [(control f10)] 'cscope-history-forward-file)
(define-key global-map [(control f11)] 'cscope-history-backward-line)
(define-key global-map [(control f12)] 'cscope-history-backward-file)
(define-key global-map [(meta f9)]  'cscope-display-buffer)
(define-key global-map [(meta f10)] 'cscope-display-buffer-toggle)


