;;-------------------------------------------------------------------
;; keyboard for emacs running in OS X
;;-------------------------------------------------------------------
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'ctrl)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/el-get"))

;;-------------------------------------------------------------------
;; configuration about boot
;;-------------------------------------------------------------------
(setq *spell-check-support-enabled* nil)
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/test"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/color-theme-6.6.0"))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/popup"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/auto-complete"))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/emacs-dirtree"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/eproject"))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/autopair"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/yasnippet"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/js2-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/js2-refactor"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/dash"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/s"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/multiple-cursors"))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/nodejs-mode"))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/slime"))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/cedet"))


(add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/el-get"))
(require 'el-get)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/buffer-move"))
(require 'buffer-move)
(global-set-key (kbd "<M-S-up>")     'buf-move-up)
(global-set-key (kbd "<M-S-down>")   'buf-move-down)
(global-set-key (kbd "<M-S-left>")   'buf-move-left)
(global-set-key (kbd "<M-S-right>")  'buf-move-right)


(desktop-save-mode 1);; save edit status last time
(setq ring-bell-function 'ignore);; close error tip sound
(setq make-backup-files nil);;disable auto backup

(require 'utils)
(require 'unicad)

;;set shell PATH
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (string-rtrim (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (and *is-a-mac* window-system)
  (set-exec-path-from-shell-PATH))

;;-------------------------------------------------------------------
;; configuration about view
;;-------------------------------------------------------------------
;; color-theme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-select)
(color-theme-comidia)

(tool-bar-mode 0) ;;hide tool bar

;;绑定全屏快捷键
(global-set-key [f12] 'ns-toggle-fullscreen)

;; show file path on title bar
(setq frame-title-format  
      '("%S" (buffer-file-name "%f"  
			       (dired-directory dired-directory "%b"))))

;; switch with frames, need to debug
(global-set-key (kbd "M-`") 'ns-next-frame)

;; bind key for move between windows
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)  
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; bind key for beginning or end of buffer
(global-set-key [67108924] 'beginning-of-buffer) ;;C-<
(global-set-key [67108926] 'end-of-buffer) ;;C->

;; scroll speed
(setq scroll-margin 3
      scroll-conservatively 10000)
(setq mouse-wheel-progressive-speed nil);; close mouse speed up  
(setq scroll-margin 3  
      scroll-conservatively 10000)  ;; scroll screen smoothly

;; show line num
(require 'linum)
(global-linum-mode)

(show-paren-mode t);; show parentheses matched
(global-font-lock-mode t);; syntax highlight  

(mouse-avoidance-mode 'animate) ;; hide mouse when it cover the cursor
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key [C-M-s] 'isearch-forward-regexp)
(global-set-key [C-M-r] 'isearch-backward-regexp)
;;-------------------------------------------------------------------
;; configuration some lib for edit
;;-------------------------------------------------------------------

;; auto-complete,depend popup
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories (expand-file-name "~/.emacs.d/site-lisp/auto-complete/dict"))
(require 'auto-complete-config)
(ac-config-default)

;; autopair
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;;yasnippet
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (expand-file-name "~/.emacs.d/snippets"))
(yas-global-mode 1)

;; dir tree
(require 'dirtree)
(global-set-key (kbd "C-x d") 'dirtree)

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)) ;;js2-mode instead later

;;js2-refactor
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-x r") ;; eg. extract function with `C-c C-m ef`.

;;eproject, need to debug
(require 'eproject)

;;nodejs-mode, need to debug
(require 'nodejs-repl)

;;c#
;;(load-file (expand-file-name "~/.emacs.d/site-lisp/csharp-mode-0.8.5.el"))
;;(require 'csharp-mode)

;;common lisp
(require 'slime)
(setq inferior-lisp-program "/usr/local/Cellar/clisp/2.49/bin/clisp")
(slime-setup '(slime-fancy))

;;cedet ide plugin 4 c/c++
(load-file (expand-file-name "~/.emacs.d/site-lisp/cedet/cedet-devel-load.el"))
(require 'cedet)
(semantic-load-enable-minimum-features)
(semantic-load-enable-code-helpers)
;;(semantic-load-enable-guady-code-helpers)
(semantic-load-enable-excessive-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)

(defconst cedet-user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public"
        "../.." "../../include" "../../inc" "../../common" "../../public"))
(require 'semantic-c nil 'noerror)
(let ((include-dirs cedet-user-include-dirs))
  (when (eq system-type 'windows-nt)
    (setq include-dirs (append include-dirs cedet-win32-include-dirs)))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        include-dirs))
;;-------------------------------------------------------------------
;; configuration about edit
;;-------------------------------------------------------------------
;;(global-set-key (kbd "RET") 'newline-and-indent)

;;format whole buffer
(defun indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format successfully"))
(global-set-key "f" 'indent-whole) ;; bind to C-x f

(setq x-select-enable-clipboard t);; enable external clipboard
(global-set-key [67108910] (quote set-mark-command));; bind mark set command to C-.

(setq kill-ring-max 200);; kill ring

;; set new method of kill a whole line 
(defadvice kill-ring-save (before slickcopy activate compile)
  ;;"When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
(defadvice kill-region (before slickcut activate compile)
  ;;"When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; search words selected
(defun wcy-define-key-in-transient-mode (global-p key cmd-mark-active  cmd-mark-no-active)
  (funcall (if global-p 'global-set-key 'local-set-key)
           key
           `(lambda ()
              (interactive)
              (if mark-active
                  (call-interactively ',cmd-mark-active)
                (call-interactively ',cmd-mark-no-active)))))
(defun wcy-isearch-forward-on-selection (&optional regexp-p no-recursive-edit)
  (interactive "P\np")
  (let ((text (buffer-substring (point) (mark))))
    (goto-char (min (point) (mark)))
    (setq mark-active nil)
    (isearch-mode t (not (null regexp-p)) nil (not no-recursive-edit))
    (isearch-process-search-string text text)))
(wcy-define-key-in-transient-mode t (kbd "C-s")
                                  'wcy-isearch-forward-on-selection
                                  'isearch-forward)

;;kill buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(defun kill-all-buffers ()
  "kill all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; open line previous or next with C-o and M-o 
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "C-o") 'open-next-line)
;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "M-o") 'open-previous-line)
;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

;;move line to next or previous with C-up and C-down
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))
(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))
(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))
(global-set-key (kbd "C-<up>") 'move-line-up)
(global-set-key (kbd "C-<down>") 'move-line-down)

;; back to indentation or beginning
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

;;rename file and buffer
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;;chinese font setting
(when (display-graphic-p)
  (setq fonts
        (cond ((eq system-type 'darwin)     '("Monaco"     "STHeiti"))
              ((eq system-type 'gnu/linux)  '("Menlo"     "WenQuanYi Zen Hei"))
              ((eq system-type 'windows-nt) '("Consolas"  "Microsoft Yahei"))))

  (setq face-font-rescale-alist '(("STHeiti" . 1.0) ("Microsoft Yahei" . 1.0) ("WenQuanYi Zen Hei" . 1.0)))
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (car fonts) 14))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family (car (cdr fonts))))))


;;use C-x C-v RET to update buffer from disk



(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))


(defun scroll-up-half ()
  (interactive)
  (scroll-up (window-half-height)))

(defun scroll-down-half ()         
  (interactive)                    
  (scroll-down (window-half-height)))

(global-set-key (kbd "C-S-v") 'scroll-up-command)
(global-set-key (kbd "M-S-v") 'scroll-down-command)
(global-set-key (kbd "C-v") 'scroll-up-half)
(global-set-key (kbd "M-v") 'scroll-down-half)


(defun my-horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window)
                            (- cur mid)))))
(global-set-key (kbd "C-S-l") 'my-horizontal-recenter)

;;use C-x C-v RET to update buffer from disk
;;auto revert buffers
(autoload 'auto-revert-mode "autorevert" nil t)
(autoload 'turn-on-auto-revert-mode "autorevert" nil nil)
(autoload 'global-auto-revert-mode "autorevert" nil t)
(global-auto-revert-mode 1)


(require 'tramp)
(cond
 ((eq system-type 'windows-nt)
  (setq tramp-default-method "plink"
        tramp-password-end-of-line "\r\n"))
 ((eq system-type 'gnu/linux)
  (setq tramp-default-method "ssh")))
(setq tramp-default-user "root"
	 tramp-default-host "114.215.87.216")
(setq password-cache-expiry 360000)
