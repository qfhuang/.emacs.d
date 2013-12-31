 ;;; Emacs org-mode support for blogging with Jekyll.
 ;;;
 ;;; To use, just put this file somewhere in your emacs load path and
 ;;; (require 'org-jekyll)
 ;;;
 ;;; An article showing its use can be found at:
 ;;;    - http://www.gorgnegre.com/linux/using-emacs-orgmode-to-blog-with-jekyll.html
 ;;;
 ;;; Adapted from
 ;;;    - http://orgmode.org/worg/org-tutorials/org-jekyll.html
 ;;;    - https://github.com/metajack/jekyll/blob/master/emacs/jekyll.el
 ;;;
 ;;; Gorg Negre 2012-07-05
 
 (provide 'org-jekyll)
 
 ;; Define our org project to be exported. Run "M-x org-export X mvm" to
 ;; export.
 (setq org-publish-project-alist
       '(
 
    ("org-mvm"
           :base-directory "~/qfhuang.github.com/_orgposts" ;; Path to your org files.
           :base-extension "org"
           :publishing-directory "~/qfhuang.github.com/_posts" ;; Path to your Jekyll project.
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 6
           :html-extension "html"
           :body-only t ;; Only export section between &lt;body&gt; &lt;/body&gt; tags
           :section-numbers nil
           :with-toc nil
 
           :author "Your Name"
           :email "user@example.cat"
     )
 
     ("org-static-mvm"
           :base-directory "~/qfhuang.github.com/"
           :base-extension "css\\|js\\|png\\|jpg\\|ico\\|gif\\|pdf\\|mp3\\|flac\\|ogg\\|swf\\|php\\|markdown\\|md\\|html\\|htm\\|sh\\|xml\\|gz\\|bz2\\|vcf\\|zip\\|txt\\|tex\\|otf\\|ttf\\|eot\\|rb\\|yml\\|htaccess\\|gitignore"
           :publishing-directory "~/qfhuang.github.com/"
           :recursive t
           :publishing-function org-publish-attachment)
 
     ("mvm" :components ("org-mvm" "org-static-mvm"))
 
 ))

 ;; Improve our blogging experience with Org-Jekyll. This code sets four
 ;; functions with corresponding key bindings:
 ;;
 ;; C-c j n - Create new draft
 ;; C-c j P - Post current draft
 ;; C-c j d - Show all drafts
 ;; C-c j p - Show all posts
 ;;
 ;; Once a draft has been posted (i.e., moved from the _drafts
 ;; directory to _post with the required date prefix in the filename), we
 ;; then need to html-export it to the jekyll rootdir (with org-publish).
 
  (global-set-key (kbd "C-c j n") 'jekyll-draft-post)
  (global-set-key (kbd "C-c j P") 'jekyll-publish-post)
  (global-set-key (kbd "C-c j p") (lambda ()
     (interactive)
     (find-file "~/qfhuang.github.com/_orgposts/")))
  (global-set-key (kbd "C-c j d") (lambda ()
     (interactive)
     (find-file "~/qfhuang.github.com/_drafts/")))
 
 (defvar jekyll-directory "/home/kevin/qfhuang.github.com/"
   "Path to Jekyll blog.")
 (defvar jekyll-drafts-dir "_drafts/"
   "Relative path to drafts directory.")
 (defvar jekyll-posts-dir "_orgposts/"
   "Relative path to posts directory.")
 (defvar jekyll-post-ext ".org"
   "File extension of Jekyll posts.")
 (defvar jekyll-post-template
   "#+STARTUP: showall\n#+STARTUP: hidestars\n#+OPTIONS: H:2 num:nil tags:nil toc:nil timestamps:t\n#+BEGIN_HTML\n---\nlayout: post\ntitle: %s\nexcerpt: \ncategories:\n  -  \ntags:\n  -  \npublished: true\n---\n{%% include JB/setup %%}\n#+END_HTML\n** "
   "Default template for Jekyll posts. %s will be replace by the post title.")
 
 (defun jekyll-make-slug (s)
   "Turn a string into a slug."
   (replace-regexp-in-string
    " " "-" (downcase
             (replace-regexp-in-string
              "[^A-Za-z0-9 ]" "" s))))
 
 (defun jekyll-yaml-escape (s)
   "Escape a string for YAML."
   (if (or (string-match ":" s)
           (string-match "\"" s))
       (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"")
     s))
 
 (defun jekyll-draft-post (title)
   "Create a new Jekyll blog post."
   (interactive "sPost Title: ")
   (let ((draft-file (concat jekyll-directory jekyll-drafts-dir
                             (jekyll-make-slug title)
                             jekyll-post-ext)))
     (if (file-exists-p draft-file)
         (find-file draft-file)
       (find-file draft-file)
       (insert (format jekyll-post-template (jekyll-yaml-escape title))))))
 
 (defun jekyll-publish-post ()
   "Move a draft post to the posts directory, and rename it so that it
 contains the date."
   (interactive)
   (cond
    ((not (equal
           (file-name-directory (buffer-file-name (current-buffer)))
           (concat jekyll-directory jekyll-drafts-dir)))
     (message "This is not a draft post.")
     (insert (file-name-directory (buffer-file-name (current-buffer))) "\n"
             (concat jekyll-directory jekyll-drafts-dir)))
    ((buffer-modified-p)
     (message "Can't publish post; buffer has modifications."))
    (t
     (let ((filename
            (concat jekyll-directory jekyll-posts-dir
                    (format-time-string "%Y-%m-%d-")
                    (file-name-nondirectory
                     (buffer-file-name (current-buffer)))))
           (old-point (point)))
       (rename-file (buffer-file-name (current-buffer))
                    filename)
       (kill-buffer nil)
       (find-file filename)
       (set-window-point (selected-window) old-point)
       (org-publish "org-mvm")))))

;; screenshot in org-mode
;; modified by gift.young@gmail.com
 ;; based on [http://praktikanten.brueckenschlaeger.org/2010/11/28/screenshots-in-org-mode]
(defun my-screenshot ()
 "Take a screenshot into a unique-named file in the current buffer file
 directory and insert a link to this file."
 (interactive)
 (setq filename
 (concat (make-temp-name
 (concat (file-name-directory (buffer-file-name)) "../images/" ) ) ".png"))
 (if (file-accessible-directory-p (concat (file-name-directory
(buffer-file-name)) "../images/"))
 nil
 (make-directory "../images"))
 (call-process-shell-command "scrot" nil nil nil nil "-s" (concat
"\"" filename "\"" ))
 (insert (concat "[[" "../images/" (file-name-nondirectory filename) "]]"))
 (org-display-inline-images)
 )

 (global-set-key (kbd "s-s") 'my-screenshot)


 
