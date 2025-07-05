;;; dukemhk-mode.el -- Major mode for editing EDuke32 .mhk files

;; This file is NOT part of Emacs.

;; Syntax Highlighting Only

;; Author: LeoD, by search&replace-editing:
;; dukecon-mode by Philipp Kutin, based on a tutorial by Scott Andrew Borton
;; Created: 2012-11-16
;; Keywords: Duke3D EDuke32 mhk major-mode
;; Last updated: 2025-06-01 (EDuke32 svn8644 - r10373 -> r10619)
;; -> .../source/build/src/mhk.cpp : legaltokens []



;; Copyright (C) 2012-2025 LeoD
;; Copyright (C) 2007-2015 Philipp Kutin
;; Copyright (C) 2000,2003 Scott Andrew Borton

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Installation:
;;
;; Have the directory which contains this file in your load-path,
;;  for example: (add-to-list 'load-path "~/.emacs.d/site-lisp")
;; Add the following to your ~/.emacs file:
;;  (require 'dukemhk-mode)
;;  (setq auto-mode-alist (cons  '("\\.mhk\\'" . dukemhk-mode) auto-mode-alist))


;;; Code:
(defvar dukemhk-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.mhk\\'" . dukemhk-mode))




(defconst dukemhk-keywords-1 ; objects
  '(
     sprite    light
   ))

(defconst dukemhk-keywords-2 ; commands, short version where available
  '(
     notmd     away1     away2     nomdanim
     angoff    pitch     roll
     mdxoff    mdyoff    mdzoff
     mdposxoff mdposyoff mdposzoff
   ))

(defconst dukemhk-keywords-2a ; commands: redundant ones, long versions,mhkreset
  '(
     notmd2    notmd3    nomd2anim nomd3anim
     angleoff
     mdpivxoff mdpivyoff mdpivzoff mdpivotxoff    mdpivotyoff    mdpivotzoff
                                   mdpositionxoff mdpositionyoff mdpositionzoff
     mhkreset
   ))

(defconst dukemhk-keywords-3 ; BloodEX / Fresh Supply objects
  '(
     spriteid  wallid    xsectorid
   ))

(defconst dukemhk-keywords-4 ; BloodEX / Fresh Supply commands
  '(
     clearcstat novoxel  remove x y
   ))


;;; Font lock
(defgroup dukemhk nil
  "Major mode for editing Duke3D/Eduke[32] mhk files."
  :group 'languages
)

(defvar dukemhk-object-keywords  'dukemhk-object-keywords)
(defvar dukemhk-command-keywords 'dukemhk-command-keywords)
(defvar dukemhk-cmd-kw           'dukemhk-cmd-kw)
(defvar dukemhk-obj-ex-keywords  'dukemhk-obj-ex-keywords)
(defvar dukemhk-cmd-ex-keywords  'dukemhk-cmd-ex-keywords)

(defface dukemhk-object-keywords  '((t (:inherit font-lock-type-face)))
  "*Object  keywords for Dukemhk Mode." :group 'dukemhk :group 'faces)
(defface dukemhk-command-keywords '((t (:inherit font-lock-keyword-face)))
  "*Command keywords for Dukemhk Mode." :group 'dukemhk :group 'faces)
(defface dukemhk-cmd-kw           '((t (:inherit font-lock-warning-face)))
  "*Command keywords for Dukemhk Mode." :group 'dukemhk :group 'faces)
(defface dukemhk-obj-ex-keywords  '((t (:inherit font-lock-variable-name-face)))
  "*Object  keywords for Dukemhk Mode." :group 'dukemhk :group 'faces)
(defface dukemhk-cmd-ex-keywords '((t (:inherit font-lock-constant-face)))
  "*Command keywords for Dukemhk Mode." :group 'dukemhk :group 'faces)

(defconst dukemhk-font-lock-keywords-1
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukemhk-keywords-1) t) "\\_>") .  dukemhk-object-keywords) )
  "Object primitives highlighted in DUKEMHK mode."
)

(defconst dukemhk-font-lock-keywords-2
  (append dukemhk-font-lock-keywords-1
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukemhk-keywords-2) t) "\\_>") . dukemhk-command-keywords) ) )
  "Command primitives highlighted in DUKEMHK mode."
)

(defconst dukemhk-font-lock-keywords-2a
  (append dukemhk-font-lock-keywords-2
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukemhk-keywords-2a) t) "\\_>") . dukemhk-cmd-kw) ) )
  "Command primitives highlighted in DUKEMHK mode."
)

(defconst dukemhk-font-lock-keywords-3
  (append dukemhk-font-lock-keywords-2a
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukemhk-keywords-3) t) "\\_>") . dukemhk-obj-ex-keywords) ) )
  "Object primitives highlighted in DUKEMHK mode."
)

(defconst dukemhk-font-lock-keywords-4
  (append dukemhk-font-lock-keywords-3
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukemhk-keywords-4) t) "\\_>") . dukemhk-cmd-ex-keywords) ) )
  "Command primitives highlighted in DUKEMHK mode."
)


(defvar dukemhk-font-lock-keywords dukemhk-font-lock-keywords-4
  "Default highlighting expressions for DUKEMHK mode."
)


(defvar dukemhk-mode-syntax-table
  (let ((dukemhk-mode-syntax-table (make-syntax-table)))
    ; recognize braces pairs
    (modify-syntax-entry ?{ "(}" dukemhk-mode-syntax-table)
    (modify-syntax-entry ?} "){" dukemhk-mode-syntax-table)
    
    ; Comment styles are same as C++
    (modify-syntax-entry ?/  ". 124b" dukemhk-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   dukemhk-mode-syntax-table)
    (modify-syntax-entry ?\n "> b"    dukemhk-mode-syntax-table)
    dukemhk-mode-syntax-table)
  "Syntax table for dukemhk-mode")


(defun dukemhk-mode ()
  "Major mode for editing Eduke .mhk files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table dukemhk-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(dukemhk-font-lock-keywords))
  (setq major-mode 'dukemhk-mode)
  (setq mode-name "Duke3D-MHK")
  (run-hooks 'dukemhk-mode-hook))

;; --------------------------------

(provide 'dukemhk-mode)

;;; dukemhk-mode.el ends here
