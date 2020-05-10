;;; polybrain.el --- Polymode support for Org-brain -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Dustin Lacewell

;; Author: Dustin Lacewell <dlacewell@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26") (polymode "0") (org-brain "0.93"))
;; Keywords: polymode, org, org-brain
;; URL: http://github.com/dustinlacewell/polybrain.el

;;; Commentary:

;; This package lets you edit your org-brain entries from org-brain-visualize

;;; Code:
(require 'polymode)
(require 'org-brain)

;;;###autoload
(defgroup polybrain nil
  "Configuration options for polybrain."
  :group 'org)

;;;###autoload
(defcustom polybrain-save-on-switch t
  "Automatically save the current entry
anytime (polybrain-switch) is called, or the current entry is
changed."
  :group 'polybrain
  :type 'string)

;;;###autoload
(defcustom polybrain-set-title-on-save t
  "Automatically set the title of the curernt entry to its name,
  whenever a save happens."
  :group 'polybrain
  :type 'boolean)

(defun polybrain-visualize-setup ()
  (unless poly-brain-mode (poly-brain-mode)))

(add-hook 'org-brain-visualize-text-hook 'polybrain-visualize-setup)

(defun polybrain--get-point-min-max ()
  "Get `org-mode' min position and `org-brain-visualize' max position."
  (save-excursion
    (goto-char org-brain--vis-entry-text-marker)
    (previous-line)
    (let ((max (point)))
      (next-line)
      (next-line)
      (list (point) max))))

(defun polybrain--entry-contents ()
  "Get text content of the visualized entry."
  (save-excursion
    (goto-char org-brain--vis-entry-text-marker)
    (end-of-line)
    ;; (forward-char)
    (buffer-substring (point) (point-max))))

(defun polybrain--save-contents (content)
  "Replace the entry's text with CONTENT."
  (find-file (org-brain-entry-path org-brain--vis-entry))
  (seq-let (entry-min entry-max) (org-brain-text-positions org-brain--vis-entry)
    (goto-char entry-min)
    (delete-region entry-min entry-max)
    (insert content)
    (unless (looking-at-p "\n")
      (insert "\n\n"))
    (save-buffer)
    (switch-to-buffer (other-buffer (current-buffer) 1))))

(defun polybrain-save ()
  "Save entry text content to the entry's file."
  (interactive)
  (polybrain--save-contents (polybrain--entry-contents)))

(defvar polybrain--last-brain-point nil)
(defvar polybrain--last-org-point nil)

(defun polybrain-switch ()
  "Move `point' between `org-brain-visualize' and `org-mode' content."
  (interactive)
  (seq-let (org-min brain-max) (polybrain--get-point-min-max)
    (let ((p (point)))
      (if (< p org-brain--vis-entry-text-marker)
          (progn
            ;; In case the text has been changed outside org-brain-visualize
            (revert-buffer)
            (setq polybrain--last-brain-point p)
            (if polybrain--last-org-point
                (goto-char (max org-min polybrain--last-org-point))
              (goto-char org-brain--vis-entry-text-marker)
              (end-of-line) (forward-char)))
        (when polybrain-save-on-switch (polybrain-save))
        (setq polybrain--last-org-point p)
        (if polybrain--last-brain-point
            (goto-char (floor (min brain-max polybrain--last-brain-point)))
          (beginning-of-buffer))))))

(defun polybrain-top ()
  (interactive)
  (when polybrain-save-on-switch
    (polybrain-save))
  (seq-let (org-min brain-max) (polybrain--get-point-min-max)
    (unless (< (point) brain-max)
      (setq polybrain--last-org-point (point))
      (goto-char (or polybrain--last-brain-point 0)))))

(defun polybrain-bottom ()
  (interactive)
  (when polybrain-save-on-switch
    (polybrain-save))
  (seq-let (org-min brain-max) (polybrain--get-point-min-max)
    (unless (> (point) org-min)
      (setq polybrain--last-brain-point (point))
      (goto-char (or polybrain--last-org-point org-min)))))

(defun polybrain-top-then (cont &optional preserve-point)
  (if preserve-point
      (save-excursion
        (polybrain-top)
        (call-interactively cont))
    (polybrain-top)
    (call-interactively cont)))

(define-hostmode poly-brain-hostmode
  :mode 'org-brain-visualize-mode)

(defun poly-brain--set-writable () (setq-local buffer-read-only nil))

(define-innermode poly-brain-org-innermode
  :mode 'org-mode
  :head-matcher "^[─-]\\{3\\} Entry [─-]+"
  :tail-matcher "\\'"
  :head-mode 'host
  :tail-mode 'host
  :init-functions '(poly-brain--set-writable))

(define-polymode poly-brain-mode
  :hostmode 'poly-brain-hostmode
  :innermodes '(poly-brain-org-innermode))

(defun poly-brain-set-read-only ()
  (setq-local polymode-move-these-vars-from-old-buffer
              (delq 'buffer-read-only polymode-move-these-vars-from-old-buffer)))

(add-hook 'poly-brain-mode-hook 'poly-brain-set-read-only)

(defun poly-brain-org-set-read-only ()
  (setq-local buffer-read-only nil))

(add-hook 'poly-brain-org-innermode-hook 'poly-brain-org-set-read-only)

(provide 'polybrain)

;;; polybrain.el ends here
