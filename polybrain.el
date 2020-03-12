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

(defun my/org-brain-visualize-setup ()
  (unless poly-brain-mode (poly-brain-mode))
  (setq-local buffer-read-only nil))

(add-hook 'org-brain-visualize-text-hook 'my/org-brain-visualize-setup)

(defun my/org-brain-save ()
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

(defvar my/poly-brain-last-brain-point nil)
(defvar my/poly-brain-last-org-point nil)

(defun my/polymode-next-chunk ()
  (interactive)
  (let ((p (point)))
    (if (search-forward "--- Entry" nil t)
        (progn
          (setq my/poly-brain-last-brain-point p)
          (if my/poly-brain-last-org-point
              (goto-char my/poly-brain-last-org-point)
            (end-of-line) (forward-char)))
      (setq my/poly-brain-last-org-point p)
      (if my/poly-brain-last-brain-point
          (goto-char my/poly-brain-last-brain-point)
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
