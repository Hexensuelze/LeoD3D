;;; dukegrpinfo-mode.el -- Major mode for editing EDuke32 .grpinfo files

;; This file is NOT part of Emacs.

;; Syntax Highlighting Only

;; Author: LeoD, by search&replace-editing:
;; dukecon-mode by Philip Kutin, based on a tutorial by Scott Andrew Borton
;; Created: 2021-09-01
;; Keywords: Duke3D Eduke32 grpinfo major-mode
;; Last updated: 2022-04-26 (EDuke32 r9505 - r9913 -> r10033)
;; -> .../source/duke3d/src/grpscan.cpp : grpinfotokens[]



;; Copyright (C) 2021-2022 LeoD
;; Copyright (C) 2007-2012 Philipp Kutin
;; Copyright (C) 2000, 2003 Scott Andrew Borton

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.

;;; Installation:
;;
;; Have the directory which contains this file in your load-path,
;;  for example: (add-to-list 'load-path "~/.emacs.d/site-lisp")
;; Add the following to your ~/.emacs file:
;;  (require 'dukegrpinfo-mode)


;;; Code:
(defvar dukegrpinfo-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.grpinfo\\'" . dukegrpinfo-mode))




(defconst dukegrpinfo-keywords-1 ;; token list
  '(
    name size crc flags dependency scriptname defname rtsname
   ))

(defconst dukegrpinfo-keywords-2 ;; 
  '(
     grpinfo
     0xFD3DCFF1 ;; Atomic CRC / DUKE15_CRC
     0xBBC9CE44 ;; DUKE13_CRC
     0xAA4F6A40 ;; DUKEKR_CRC
     0xF514A6AC ;; DUKEPP_CRC
     0x982AFE4A ;; DUKEWT_CRC b386
     0x02F18900 ;; DUKE099_CRC
     0xA28AA589 ;; DUKE10_CRC
     0x912E1E8D ;; DUKE11_CRC
     0x983AD923 ;; DUKESW_CRC
     0xC5F71561 ;; DUKEMD_CRC
     0x73A15EE7 ;; DUKEMD2_CRC
     0xA9242158 ;; DUKEDC13_CRC
     0xB79D997F ;; DUKEDCPP_CRC
     0xA8CF80DA ;; DUKEDC_CRC
     0x4A2DBB62 ;; VACA13_CRC
     0x2F4FCCEE ;; VACAPP_CRC
     0xB62B42FD ;; VACA15_CRC
     0x18F01C5B ;; DUKECB_CRC
     0xF1CAE8E4 ;; DUKENW_CRC
     0x82C1B47F ;; DZ2_13_CRC
     0x7FB6117C ;; DZ2_PP_CRC
     0x75C1F07B ;; NAM_CRC
     0x3DE1589A ;; NAPALM_CRC
     0x907B82BF ;; WW2GI_CRC
     0x907B82BF ;; WW2GI_CRC
     0xD1ED8C0C ;; PLATOONL_CRC
   ))

(defconst dukegrpinfo-keywords-3 ;; 
  '(
     \-46280719 ;; Atomic CRC / DUKE15_CRC - decimal
     0xB44A89DE ;; paintb_crc
     0x89792E05 ;; dukewt_crc_b351
   \-1988547067 ;; dukewt_crc_b351 - decimal
   \-1742012854 ;; dukewt_crc_b386 - decimal
     0x5BD05463 ;; e32wt_crc / worldtour stopgap crc
    \1540379747 ;; e32wt_crc / worldtour stopgap crc - decimal
     0x2888348F ;; maiden_crc
     0x5DD10030 ;; fury_1.0_crc
     0xD5B0E641 ;; fury_1.02_crc
     0x5F0266E4 ;; fury_1.1_crc
     0x960B3686 ;; fury_2.0_crc
     PAINTB_CRC   GAMEFLAG_PAINTB
   ))

(defconst dukegrpinfo-keywords-4 ;; grpscan.h
  '(
     DUKE13_CRC   DUKEKR_CRC DUKE15_CRC DUKEPP_CRC DUKEWT_CRC  DUKE099_CRC
     DUKE10_CRC   DUKE11_CRC DUKESW_CRC DUKEMD_CRC DUKEMD2_CRC DUKEDC13_CRC
     DUKEDCPP_CRC DUKEDC_CRC VACA13_CRC VACAPP_CRC VACA15_CRC  DUKECB_CRC
     DUKENW_CRC   DZ2_13_CRC DZ2_PP_CRC NAM_CRC    NAPALM_CRC  WW2GI_CRC
     PLATOONL_CRC
   ))

(defconst dukegrpinfo-keywords-5 ;; common_game.h
  '(
     GAMEFLAG_ADDON GAMEFLAG_STANDALONE
     GAMEFLAG_DUKE  GAMEFLAG_DUKEBETA   GAMEFLAG_SHAREWARE
     GAMEFLAG_FURY  GAMEFLAG_NAM        GAMEFLAG_NAPALM     GAMEFLAG_WW2GI
   ))


;;; Font lock
(defgroup dukegrpinfo nil
  "Major mode for editing Duke3D/Eduke(32) grpinfo files."
  :group 'languages
)

(defvar dukegrpinfo-object-keywords  'dukegrpinfo-object-keywords)
(defvar dukegrpinfo-command-keywords 'dukegrpinfo-command-keywords)
(defvar dukegrpinfo-cmd-kw           'dukegrpinfo-cmd-kw)
(defvar dukegrpinfo-obj-ex-keywords  'dukegrpinfo-obj-ex-keywords)
(defvar dukegrpinfo-cmd-ex-keywords  'dukegrpinfo-cmd-ex-keywords)

(defface dukegrpinfo-object-keywords  '((t (:inherit font-lock-type-face)))
  "*Object  keywords for Dukegrpinfo Mode." :group 'dukegrpinfo :group 'faces)
(defface dukegrpinfo-command-keywords '((t (:inherit font-lock-keyword-face)))
  "*Command keywords for Dukegrpinfo Mode." :group 'dukegrpinfo :group 'faces)
(defface dukegrpinfo-cmd-kw           '((t (:inherit font-lock-warning-face)))
  "*Command keywords for Dukegrpinfo Mode." :group 'dukegrpinfo :group 'faces)
(defface dukegrpinfo-obj-ex-keywords  '((t (:inherit font-lock-variable-name-face)))
  "*Object  keywords for Dukegrpinfo Mode." :group 'dukegrpinfo :group 'faces)
(defface dukegrpinfo-cmd-ex-keywords '((t (:inherit font-lock-constant-face)))
  "*Command keywords for Dukegrpinfo Mode." :group 'dukegrpinfo :group 'faces)

(defconst dukegrpinfo-font-lock-keywords-1
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukegrpinfo-keywords-1) t) "\\_>") .  dukegrpinfo-object-keywords) )
  "Object primitives highlighted in DUKEGRPINFO mode."
)

(defconst dukegrpinfo-font-lock-keywords-2
  (append dukegrpinfo-font-lock-keywords-1
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukegrpinfo-keywords-2) t) "\\_>") . dukegrpinfo-command-keywords) ) )
  "Command primitives highlighted in DUKEGRPINFO mode."
)

(defconst dukegrpinfo-font-lock-keywords-3
  (append dukegrpinfo-font-lock-keywords-2
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukegrpinfo-keywords-3) t) "\\_>") . dukegrpinfo-cmd-kw) ) )
  "Command primitives highlighted in DUKEGRPINFO mode."
)

(defconst dukegrpinfo-font-lock-keywords-4
  (append dukegrpinfo-font-lock-keywords-3
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
;;     dukegrpinfo-keywords-4) t) "\\_>") . dukegrpinfo-obj-ex-keywords) ) )
     dukegrpinfo-keywords-4) t) "\\_>") . dukegrpinfo-cmd-ex-keywords) ) )
  "Object primitives highlighted in DUKEGRPINFO mode."
)

(defconst dukegrpinfo-font-lock-keywords-5
  (append dukegrpinfo-font-lock-keywords-4
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukegrpinfo-keywords-5) t) "\\_>") . dukegrpinfo-cmd-ex-keywords) ) )
  "Command primitives highlighted in DUKEGRPINFO mode."
)


(defvar dukegrpinfo-font-lock-keywords dukegrpinfo-font-lock-keywords-5
  "Default highlighting expressions for DUKEGRPINFO mode."
)


(defvar dukegrpinfo-mode-syntax-table
  (let ((dukegrpinfo-mode-syntax-table (make-syntax-table)))
    ; recognize braces pairs
    (modify-syntax-entry ?{ "(}" dukegrpinfo-mode-syntax-table)
    (modify-syntax-entry ?} "){" dukegrpinfo-mode-syntax-table)
    
    ; Comment styles are same as C++
    (modify-syntax-entry ?/  ". 124b" dukegrpinfo-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   dukegrpinfo-mode-syntax-table)
    (modify-syntax-entry ?\n "> b"    dukegrpinfo-mode-syntax-table)
    dukegrpinfo-mode-syntax-table)
  "Syntax table for dukegrpinfo-mode")


(defun dukegrpinfo-mode ()
  "Major mode for editing Eduke .grpinfo files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table dukegrpinfo-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(dukegrpinfo-font-lock-keywords))
  (setq major-mode 'dukegrpinfo-mode)
  (setq mode-name "Duke3D-GRPINFO")
  (run-hooks 'dukegrpinfo-mode-hook))

;; --------------------------------

(provide 'dukegrpinfo-mode)

;;; dukegrpinfo-mode.el ends here
