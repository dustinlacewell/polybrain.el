;;; polybrain.el --- Polymode support for Org-brain -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Dustin Lacewell

;; Author: Dustin Lacewell <dlacewell@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26") (polymode "0") (org-brain "0") (org "0"))
;; Keywords: polymode, org, org-brain
;; URL: http://github.com/dustinlacewell/polybrain.el

;;; Commentary:

;; This package lets you edit your org-brain entries from org-brain-visualize

;;; Code:
(require 'cl-lib)
(require 'polymode)
(require 'org-brain)

;;;###autoload
(defgroup polybrain nil
  "Configuration options for polybrain."
  :group 'org)

;; ;;;###autoload
;; (defcustom polybrain-file (expand-file-name "~/org/bookmarks.org")
;;   "Define polybrain file to store your bookmarks in."
;;   :group 'polybrain
;;   :type 'string)

(defun polybrain-visualize-setup ()
  (unless poly-brain-mode (poly-brain-mode))
  (setq-local buffer-read-only nil))

(add-hook 'org-brain-visualize-text-hook 'polybrain-visualize-setup)

(defun polybrain-save ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (search-forward "--- Entry")
    (end-of-line)
    (forward-char)
    (call-interactively 'set-mark-command)
    (end-of-buffer)
    (call-interactively 'kill-ring-save)
    (deactivate-mark t)
    (find-file (org-brain-entry-path org-brain--vis-entry))
    (beginning-of-buffer)
    (goto-char
     (or (save-excursion
           (when (re-search-forward org-brain-keyword-regex nil t)
             (end-of-line)
             (forward-char)
             (point)))
         (point-min)))
    (yank)
    (call-interactively 'set-mark-command)
    (end-of-buffer)
    (call-interactively 'kill-region)
    (save-buffer)
    (switch-to-buffer (other-buffer (current-buffer) 1))))

(defvar polybrain-last-brain-point nil)
(defvar polybrain-last-org-point nil)

(defun polybrain-switch ()
  (interactive)
  (let ((p (point)))
    (if (search-forward "--- Entry" nil t)
        (progn
          (setq polybrain-last-brain-point p)
          (if polybrain-last-org-point
              (goto-char polybrain-last-org-point)
            (end-of-line) (forward-char)))
      (setq polybrain-last-org-point p)
      (if polybrain-last-brain-point
          (goto-char polybrain-last-brain-point)
        (beginning-of-buffer)))))

(define-hostmode poly-brain-hostmode
  :mode 'org-brain-visualize-mode)

(define-innermode poly-brain-org-innermode
  :mode 'org-mode
  :head-matcher "--- Entry.*"
  :tail-matcher "\'"
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-brain-mode
  :hostmode 'poly-brain-hostmode
  :innermodes '(poly-brain-org-innermode))

(provide 'polybrain)

;;; polybrain.el ends here
