;;; anything-project.el -- finding any resource of a project

;; Copyright (c) 2009 by KAYAC Inc.

;; Author: IMAKADO <ken.imakado@gmail.com>
;; blog: http://d.hatena.ne.jp/IMAKADO (japanese)
;; Prefix: ap:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;; anything-project.el is pure emacs lisp version of anything-find-project-resources.el.
;; many ideas from
;; http://trac.codecheck.in/share/browser/lang/elisp/anything-find-project-resources/trunk/anything-find-project-resources.el
;; and
;; http://blog.jrock.us/articles/eproject.POD

;;; Installation:

;; drop this file into a directory in your `load-path',
;; and put these lines into your .emacs file.

;; (require 'anything-project)
;; (global-set-key (kbd "C-c C-f") 'anything-project)

;; type C-c C-f to invoke anything with project files.
;; project root directory is automatically detected by anything-project.el

;; clear cache, If `anything-project' function is called with prefix arg (C-u M-x anything-project)


;;; Configuration:
;; you can add new project rule by `ap:add-project' function
;; keywords :look-for, :include-regexp and :exclude-regexp can be regexp or list of regexp
;; below are few samples

;; (ap:add-project
;;  :name 'perl
;;  :look-for '("Makefile.PL" "Build.PL") ; or
;;  :include-regexp '("\\.pm$" "\\.t$" "\\.pl$" "\\.PL$") ;or
;;  )

;; (ap:add-project
;;  :name 'perl
;;  :look-for '("Makefile.PL" "Build.PL")
;;  :include-regexp '("\\.pm$" "\\.t$" "\\.pl$" "\\.PL$")
;;  :exclude-regexp "/tmp" ; can be regexp or list of regexp
;;  )


(require 'anything)

(defvar ap:default-directory-filter-regexps nil)

(defvar ap:default-filter-regexps
  '("\\.pm$" "\\.t$" "\\.pl$" "\\.PL$"))

;; almost copied from `anything-find-resource--project-root-files'.
(defvar ap:default-project-root-files
    '("build.xml" "prj.el" ".project" "pom.xml"
      "Makefile" "configure" "Rakefile" "Info.plist"
      "NAnt.build" "xpi" "Makefile.SH" ".git"
      ))

;; internal
(defvar ap:projects nil)
(defvar ap:root-directory "")
(defvar ap:--cache nil)

(defun ap:mk-list (a)
  (if (listp a) a (list a)))

(defun* ap:add-project (&key name look-for (include-regexp ".*") (exclude-regexp nil) (exclude-directory-regexp nil))
  (assert (not (null look-for)))
  (assert (and (not (null name))
               (symbolp name)))
  (setq ap:projects (assq-delete-all name ap:projects))
  (add-to-list 'ap:projects
               `(,name . ((,:look-for . ,(ap:mk-list look-for))
                          (,:include-regexp . ,(ap:mk-list include-regexp))
                          (,:exclude-regexp . ,(ap:mk-list exclude-regexp))))))

;; (ap:get-project-data 'perl :look-for)
(defun ap:get-project-data (name type)
  (let ((alist (assoc-default name ap:projects)))
    (when alist
      (assoc-default type alist))))

(defun ap:get-project-keys ()
  (let* ((keys (loop for alist in ap:projects
                     collect (first alist)))
         (keys (delete 'default keys)))
    (add-to-list 'keys 'default t)))

(defun ap:root-directory-p (root-files files)
  (some
   (lambda (file)
     (find file
           root-files
           :test 'string=))
   files))

(defun ap:current-directory ()
  (file-name-directory
   (expand-file-name
    (or (buffer-file-name)
        default-directory))))

(defun* ap:root-detector (current-dir &optional (project-key :default))
  (let* ((current-dir (expand-file-name current-dir))
         (files (ap:get-project-data project-key :look-for)))
    (ap:root-directory-p files (directory-files current-dir))))


(defvar ap:get-root-directory-limit 10)
(defun ap:get-root-directory-aux (key)
  (let ((cur-dir (ap:current-directory)))
    (ignore-errors
      (loop with count = 0
            until (ap:root-detector cur-dir key)
            if (= count ap:get-root-directory-limit)
            do (return nil)
            else
            do (progn (incf count)
                      (setq cur-dir (expand-file-name (concat cur-dir "../"))))
            finally return cur-dir))))

(defun ap:get-root-directory ()
  (let ((project-keys (ap:get-project-keys)))
    (loop for key in project-keys
          for ret = (values (ap:get-root-directory-aux key) key)
          until (car ret)
          finally return ret)))

(defsubst ap:any-match (regexp-or-regexps file-name)
  (when regexp-or-regexps
    (let ((regexps (if (listp regexp-or-regexps) regexp-or-regexps (list regexp-or-regexps))))
      (some
       (lambda (re)
         (string-match re file-name))
       regexps))))

(defun* ap:directory-files-recursively (regexp &optional directory type (dir-filter-regexp nil))
  (let* ((directory (or directory default-directory))
         (predfunc (case type
                     (dir 'file-directory-p)
                     (file 'file-regular-p)
                     (otherwise 'identity)))
         (files (directory-files directory t "^[^.]" t)))
    (loop for file in files
          when (and (funcall predfunc file)
                    (ap:any-match regexp (file-name-nondirectory file)))
          collect file into ret
          when (and (file-directory-p file)
                    (not (ap:any-match dir-filter-regexp file)))
          nconc (ap:directory-files-recursively regexp file type dir-filter-regexp) into ret
          finally return  ret)))

(defun ap:truncate-file-name (root-dir files)
  (let* ((root-dir (replace-regexp-in-string "/$" "" root-dir))
         (re (concat "^" root-dir "\\(.*\\)$")))
    (let* ((truncate (lambda (f)
                       (if (string-match re f)
                         (match-string-no-properties 1 f)
                         f))))
      (mapcar truncate files))))

(defun ap:get-project-files (&optional clear-cache)
  (destructuring-bind
      (root-dir key)
      (ap:get-root-directory)
    (when (and root-dir key)
      ;; clear cache if command invoked with prefix(C-u).
      (when clear-cache
        (setq ap:--cache
              (delete-if (lambda (ls) (equal root-dir ls))
                         ap:--cache
                         :key 'car)))
      (ap:get-project-files-aux root-dir key))))

(defun ap:get-project-files-aux (root-dir key)
  (lexical-let ((root-dir root-dir)
                (key key))
    (setq ap:root-directory root-dir)
    (ap:cache-get-or-set
     root-dir
     (lambda ()
       (let ((include-regexp (ap:get-project-data key :include-regexp))
             (exclude-regexp (ap:get-project-data key :exclude-regexp)))
         (let* ((files (ap:directory-files-recursively include-regexp root-dir 'identity exclude-regexp))
                (files (ap:truncate-file-name root-dir files)))
           files))))))

(defun ap:cache-get-or-set (root-dir get-files-fn)
  (let ((cache (assoc-default root-dir ap:--cache)))
    (if cache
        cache ; cache hit!!
      (let ((files (funcall get-files-fn)))
        (when files
          (add-to-list 'ap:--cache
                       `(,root-dir . ,files))
          files)))))

(defun ap:expand-file (file)
  (let ((root-dir (replace-regexp-in-string "/$" "" ap:root-directory)))
    (concat root-dir file)))




(if (fboundp 'anything-run-after-quit)
    (defalias 'ap:anything-run-after-quit 'anything-run-after-quit)
  (defun ap:anything-run-after-quit (function &rest args)
    "Perform an action after quitting `anything'.
The action is to call FUNCTION with arguments ARGS."
    (setq anything-quit t)
    (apply 'run-with-idle-timer 0 nil function args)
    (anything-exit-minibuffer)))

(defun ap:project-files-init-msg ()
  (message "Buffer is not project file. buffer: %s" (ap:current-directory)))

(defun ap:project-files-init (&optional cache-clear)
  (let ((files (ap:get-project-files cache-clear))
        (cands-buf (anything-candidate-buffer 'local)))
    (cond
     (files
      (with-current-buffer cands-buf
        (insert (mapconcat 'identity files "\n"))))
     (t
      (ap:anything-run-after-quit
       'ap:project-files-init-msg)))))

;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Commands
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; copied from anything-config.el
(defun ap:anything-c-open-dired (file)
  "Opens a dired buffer in FILE's directory.  If FILE is a
directory, open this directory."
  (if (file-directory-p file)
      (dired file)
    (dired (file-name-directory file))
    (dired-goto-file file)))


(defvar anything-c-source-project
  `(
    ((name . ,(format "Project files. root directory: %s" (or (car-safe (ap:get-root-directory)) "")))
     (init . (lambda ()
               (ap:project-files-init cache-clear)))
     (candidates-in-buffer)
     (action . (("Find file" .
                 (lambda (c)
                   (find-file (ap:expand-file c))))
                ("Find file other window" .
                 (lambda (c)
                   (find-file-other-window (ap:expand-file c))))
                ("Find file other frame" .
                 (lambda (c)
                   (find-file-other-frame (ap:expand-file c))))
                ("Open dired in file's directory" .
                 (lambda (c)
                   (ap:anything-c-open-dired (ap:expand-file c))))
                ))
     )))

(defun anything-project (&optional cache-clear)
  (interactive "P")
  (anything anything-c-source-project 
            nil "Project files: "))



;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Default Project
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(ap:add-project
 :name 'perl
 :look-for '("Makefile.PL" "Build.PL")
 :include-regexp '("\\.pm$" "\\.t$" "\\.pl$" "\\.PL$")
 )

(ap:add-project
 :name 'default
 :look-for ap:default-project-root-files
 )

(ap:add-project
 :name 'symfony
 :look-for '("symfony")
 )


(provide 'anything-project)
