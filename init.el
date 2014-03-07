;; emacs 24.3.50
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(setq default-frame-alist
      '((height . 40)(width . 120)))

;; all backups goto ~/.backups instead in the current directory
(setq backup-directory-alist (quote (("." . "~/.backups"))))

;; Setting English Font
;; M-x describe-font
(set-default-font "-bitstream-Menlo-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
 
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
;; 创建子模块　git submodule add git://github.com/AndreaCrotti/yasnippet-snippets.git elpa/yasnippet-0.6.1/yasnippet-snippets
;; 下载子模块　git submodule update --init --recursive
;; No submodules errors,you can add to .gitmodules by manualy
(yas/load-directory "~/.emacs.d/elpa/yasnippet-0.6.1/yasnippet-snippets")
(yas/load-directory "~/.emacs.d/elpa/yasnippet-0.6.1/snippets")
(yas/load-directory "~/.emacs.d/lisp/snippets") ;;自定义的模板保存路径

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

;; c-style setup http://arttecher.com/blog/2013/09/20/linux-kernel-coding-style/
(defun linux-c-mode ()
"C mode with adjusted defaults for use with the Linux kernel."
(interactive)
(c-mode)
(c-set-style "K&R")
(setq tab-width 8)
(setq indent-tabs-mode t)
(setq c-basic-offset 8))

(add-hook 'c-mode-hook 'turn-on-auto-fill)
(add-hook 'c++-mode-hook 'turn-on-auto-fill)
;;http://docs.huihoo.com/homepage/shredderyin/emacs_fill.html
(setq adaptive-fill-regexp "[ \t]+\\|[ \t]*\\([0-9]+\\.\\|\\*+\\)[ \t]*")
(setq adaptive-fill-first-line-regexp "^\\* *$")

(add-to-list 'auto-mode-alist '("\.c$" . linux-c-mode))
(show-paren-mode t)

;;C mode 括号自动补全半边
;;http://ann77.emacser.com/Emacs/EmacsAutoInsertBrace.html
;;http://forum.ubuntu.org.cn/viewtopic.php?f=68&t=363635
(add-hook 'c-mode-hook 'hs-minor-mode)
(defun my-c-mode-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist  '(
;;    (?\' _ "\'")  ;单引号
;;    (?\" _ "\"")　;双引号
;;    (?\( _ ")")   
;;    (?\[ _ "]")
    (?{ > _ \n ?} >)))
  (setq skeleton-pair t)
;;  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe))
;;  (local-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
;;  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;;  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe))
(add-hook 'c-mode-hook 'my-c-mode-auto-pair)

;;c-flow
(require 'cflow-mode)
(defvar cmd nil nil)
(defvar cflow-buf nil nil)
(defvar cflow-buf-name nil nil)
 
(defun yyc/cflow-function (function-name)
  "Get call graph of inputed function. "
 ; (interactive "sFunction name:\n")
 ; (interactive (list (car (senator-jump-interactive "Function name: " nil nil nil))))
  (interactive (list (car (getfunc))))
  (setq cmd (format "cflow  -b --main=%s %s" function-name buffer-file-name))
  (setq cflow-buf-name (format "**cflow-%s:%s**"
                               (file-name-nondirectory buffer-file-name)
                               function-name))
  (setq cflow-buf (get-buffer-create cflow-buf-name))
  (set-buffer cflow-buf)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert (shell-command-to-string cmd))
  (pop-to-buffer cflow-buf)
  (goto-char (point-min))
  (cflow-mode)
  )

(defun getfunc()
  (interactive)
  (beginning-of-line)
  (re-search-forward "[A-Za-z0-9_]*\(" nil t)
  (list (buffer-substring (match-beginning 0)(- (match-end 0) 1))))

;; wrap text
;; printf();
;; like this
;; for() { printf();} by define
(defun wrap-text (b e txt)
  "simple wrapper"
  (interactive "r\nMEnter text to wrap with: ")
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (insert txt)
    (insert " {\n")
    (goto-char (point-max))
    (insert "\n}")
    (setq e2 (point)))
  (indent-region b e2 nil)
  (search-backward ") {"))
 
(global-set-key (kbd "C-x M-w") 'wrap-text)


;; ALWAYS SHOW LINE NUMBERS    
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
(add-to-list 'load-path "~/.emacs.d/elpa/slime-20140302.1712")  ; your SLIME directory
(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(require 'slime)
(slime-setup)

;;org-jekyll publish to github pages
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(require 'org-jekyll)
(setq org-src-fontify-natively t)

;; install from melpa forked by alexott
;; solve the version depend on cedet
;; M-x ecb-activate 
(require 'ecb)
(setq ecb-tip-of-the-day nil)
(setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1) ;;设置可以使用鼠标点击各个窗口的东东

;;cedet build-in
;;cedet setup
;;- https://github.com/alexott/emacs-configs/blob/master/rc/emacs-rc-cedet.el
;;- http://emacser.com/built-in-cedet.htm
;;- http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
(require 'cedet) 
    (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode  
                                      global-semanticdb-minor-mode  
                                      global-semantic-idle-summary-mode  
                                      global-semantic-mru-bookmark-mode
				      global-semantic-highlight-func-mode
				      global-semantic-idle-local-symbol-highlight-mode
				      ;;global-semantic-highlight-edits-mode 高亮编缉痕迹
				     
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

(add-hook 'c-mode-hook 'my-cedet-hook) ;;使用 c-mode-common-hook 指针补全失效？

;;insall from melpa repository
;;add -q option to cscope-indexer shell script 反向创建索引，提高速度
;;sudo cscope-indexer -r 创建索引
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (base16-railscasts)))
 '(custom-safe-themes
   (quote
    ("f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
