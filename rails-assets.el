;;; rails-assets.el - find assets quickly

;; Copyright (C) 2012 Matt Briggs

;; Author: Matt Briggs <matt@mattbriggs.net>
;; Keywords: navigation rails
;; Version: 1


;;; Dependancies: railway.el


;;; Commentary:

;; The goal of this project is to provide easy ways to get to javascript and css files in a rails project

;;; interactive

(defun ra/find-javascript ()
  (interactive)
  (let ((js (ido-completing-read "Javascript: " (ra/js-names))))
    (find-file (ra/find-path js))))

(defun ra/find-stylesheet ()
  (interactive)
  (let ((stylesheet (ido-completing-read "Stylesheet: " (ra/stylesheet-names))))
    (find-file (ra/find-path stylesheet))))

(defun ra/clear-caches ()
  (interactive)
  (setq ra/js-alist nil)
  (setq ra/stylehseet-alist nil))


;;; helpers

(defvar ra/js-suffix  "js\\|coffee")
(defvar ra/stylesheet-suffix  "css\\|sass\\|scss")

(defun ra/suffix (type)
  (let ((suffix (if (eq type 'js) ra/js-suffix ra/stylesheet-suffix)))
    (concat ".\\(" suffix "\\)$")))

(defun ra/asset-dir (additional)
  (concat (railway-root) "app/assets/" additional))

(defun ra/find-path (name alist)
  (cdr (assoc name alist)))

;;; files

(defun ra/js-names ()
  (mapcar 'car (ra/js-files)))

(defun ra/stylesheet-names ()
  (mapcar 'car (ra/stylesheet-files)))

(defun ra/js-files ()
  (or ra/js-alist
      (setq ra/js-alist (mapcar 'ra/name-and-path (ra/read-js)))))

(defun ra/stylesheet-files ()
  (or ra/stylehseet-alist
      (setq ra/js-alist (mapcar 'ra/name-and-path (ra/read-stylesheets)))))

(defvar ra/js-alist nil)
(defvar ra/stylesheet-alist nil)

(defun ra/read-js ()
  (all-files-under-dir-recursively (ra/asset-dir "javascripts")))

(defun ra/read-stylesheets ()
  (all-files-under-dir-recursively (ra/asset-dir "stylesheets")))

;;; parsing

(defun ra/name-and-path (file-name)
  `(,(ra/name-from file-name) . ,file-name))

(defun ra/name-from (file-name)
  (let* ((no-js (replace-regexp-in-string ".\\(js\\|coffee\\)$" "" file-name))
         (no-suffix (replace-regexp-in-string ".\\(css\\|css.scss\\|css.sass\\)$" "" no-js)))
    (replace-regexp-in-string (ra/asset-dir "\\(javascripts\\|stylesheets\\)/") "" no-suffix)))
