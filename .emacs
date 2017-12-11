(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(make-backup-files nil)
 '(package-selected-packages (quote (ac-dcd d-mode use-package highlight-parentheses))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "#55FF55"))))
 '(cursor ((((class color) (background dark)) (:background "#00AA00")) (((class color) (background light)) (:background "#999999")) (t nil))))

(put 'erase-buffer 'disabled nil)

;(windmove-default-keybindings 'meta)
;(global-set-key (kbd "\e <up>") 'windmove-up)
;(global-set-key (kbd "\e <down>") 'windmove-down)
;(global-set-key (kbd "\e <left>") 'windmove-left)
;(global-set-key (kbd "\e <right>") 'windmove-right)
;(global-set-key (kbd "\C-x <kp-add>") 'balance-windows)

(when (eq window-system 'w32)
  (custom-set-faces
   '(default ((t
               (:background "black" :foreground "#55FF55")
               )))
   '(cursor ((((class color)
               (background dark))
              (:background "#00AA00"))
             (((class color)
               (background light))
              (:background "#999999"))
             (t ())
             )))
  )

;(global-hl-line-mode t)

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package)
    (package-install 'use-package))
;初めてuse-packageを使う方は
;以下のコマンドを実行します。
;
;M-x package-install use-package

'(use-package beacon
  :ensure t
  :pin melpa
  :config
  (beacon-mode 1)
  )

(use-package highlight-parentheses
  :ensure t
  :pin melpa
  :init
  (setq hl-paren-background-colors '("orangered4"))
  (setq hl-paren-colors '("black"))
  :config
  (define-globalized-minor-mode global-highlight-parentheses-mode highlight-parentheses-mode
    (lambda nil (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t)
  )

(use-package whitespace
  :ensure t
  :init
  '(setq whitespace-style '(face spaces tabs trailing lines space-before-tab newline indentation space-after-tab space-mark tab-mark newline-mark))
  (if (eq window-system 'w32)
      (setq whitespace-style '(face spaces tabs trailing lines space-before-tab newline indentation space-after-tab space-mark tab-mark))
    (setq whitespace-style '(face spaces tabs trailing lines space-before-tab newline indentation space-after-tab tab-mark)))
  '(setq global-whitespace-mode t)
  :config
  (global-whitespace-mode t)
  (whitespace-mode)
  :bind
  (([f12] . whitespace-mode)
   ([(shift f12)] . global-whitespace-mode)))

(setq-default tab-width 4 indent-tabs-mode nil)

'(when (and (not window-system)
           (string-match "^xterm" (getenv "TERM")))
  (require 'xterm-title)
  (xterm-title-mode 1))

(setq inhibit-startup-screen t)

;(progn (shell) (delete-other-windows))

(use-package d-mode :ensure t)
'(add-hook 'd-mode-hook
          (lambda ()
            (setq-local flycheck-checker 'd-dmd)
            (local-set-key  (kbd "C-c C-p") 'flycheck-previous-error)
            (local-set-key  (kbd "C-c C-n") 'flycheck-next-error)))
'(add-hook 'c-mode-hook
          (lambda ()
            (setq-local flycheck-checker 'd-dmd)
            (local-set-key  (kbd "C-c C-p") 'flycheck-previous-error)
            (local-set-key  (kbd "C-c C-n") 'flycheck-next-error)))
'(add-hook 'c++-mode-hook
          (lambda ()
            (setq-local flycheck-checker 'd-dmd)
            (local-set-key  (kbd "C-c C-p") 'flycheck-previous-error)
            (local-set-key  (kbd "C-c C-n") 'flycheck-next-error)))

;(use-package flycheck :ensure t)

;(load-file "~/flycheck.el")

;(add-to-list 'exec-path (expand-file-name "/cygpath/c/D/dmd2/windows/bin"))
;(add-to-list 'exec-path (expand-file-name "/cygdrive/c/Users/Public/D/dmd2/windows/bin"))
(add-to-list 'exec-path (expand-file-name "~/dmd2/linux/bin64"))
(add-to-list 'exec-path "/c/D/dmd2/windows/bin")


;(global-flycheck-mode)

;(use-package flycheck-d-unittest :ensure t)
;(require 'flycheck-d-unittest)
;(setup-flycheck-d-unittest)

(global-set-key [C-right] 'forward-sexp)
(global-set-key [C-left]  'backward-sexp)

(use-package ac-dcd :ensure t)
(add-hook 'd-mode-hook
          (lambda ()
            (c-set-style "bsd")     ;;; (a)
            (setq c-basic-offset 4) ;;; (b)
            ;; 演算式が複数行にまたがるときのオフセット
            (c-set-offset 'statement-cont 'c-lineup-math) ;;; (c)
            ;; 行末のスペースやタブに色づけして警告する。
            (setq show-trailing-whitespace t)
            (auto-complete-mode t)
            (when (featurep 'yasnippet) (yas-minor-mode-on))
            (ac-dcd-maybe-start-server)
            (ac-dcd-add-imports)
            (add-to-list 'ac-sources 'ac-source-dcd)
            (define-key d-mode-map (kbd "C-c ?") 'ac-dcd-show-ddoc-with-buffer)
            (define-key d-mode-map (kbd "C-c .") 'ac-dcd-goto-definition)
            (define-key d-mode-map (kbd "C-c ,") 'ac-dcd-goto-def-pop-marker)
            (define-key d-mode-map (kbd "C-c s") 'ac-dcd-search-symbol)

            (when (featurep 'popwin)
              (add-to-list 'popwin:special-display-config
                           `(,ac-dcd-error-buffer-name :noselect t))
              (add-to-list 'popwin:special-display-config
                           `(,ac-dcd-document-buffer-name :position right :width 80))
              (add-to-list 'popwin:special-display-config
                           `(,ac-dcd-search-symbol-buffer-name :position bottom :width 5)))))
'(add-hook 'd-mode-hook ;; https://masutaka.net/chalow/2009-07-16-1.html
          (lambda ()
            (c-set-style "bsd")                            ;;; (a)
            (setq c-basic-offset 4)                        ;;; (b)
            ;; 演算式が複数行にまたがるときのオフセット
            (c-set-offset 'statement-cont 'c-lineup-math)  ;;; (c)
            ;; 行末のスペースやタブに色づけして警告する。
            (setq show-trailing-whitespace t)
            ))            ;;; (d)
