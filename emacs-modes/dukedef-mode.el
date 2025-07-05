;;; dukedef-mode.el -- Major mode for editing EDuke32 .def files

;; This file is NOT part of Emacs.

;; Syntax Highlighting Only

;; Author: LeoD, by search&replace-editing:
;; dukecon-mode by Philipp Kutin, based on a tutorial by Scott Andrew Borton
;; Created: 2012-03-25
;; Keywords: Duke3D EDuke32 def major-mode
;; Last updated: 2025-06-27 (EDuke32 r9959 - r10613 -> r10619)
;; -> .../source/build/src/defs.cpp : basetokens[] , mapinfotokens[]



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
;;  (require 'dukedef-mode)
;;  (setq auto-mode-alist (append '(("\\.\\([Dd][Ee][Ff]\\)$" . dukedef-mode))  auto-mode-alist ))


;;; Code:
(defvar dukedef-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.def\\'" . dukedef-mode))


(defconst dukedef-keywords-1
  '( 2dcol 2dcolidxrange
 alphahack alphahackrange animtilerange artfile
 basepalette blendtable cachesize copytile
 definemodel definemodelanim definemodelframe definemodelskin
 defineskybox definetexture definetint definevoxel definevoxeltiles
 dummytile dummytilefrompic dummytilerange globalflags highpalookup localization
 makepalookup mapinfo model multipsky
 nofloorpalrange nofullbrightrange numalphatables palookup renamefile
 selectmodelskin setuptile setuptilerange shadefactor skybox
 spritecol texhitscanrange texture tilefont tilefromtexture tint
 undefbasepaletterange undefblendtablerange undefinetile undefinetilerange
 undefmodel undefmodelof undefmodelrange undefpalookuprange undeftexture
 undeftexturerange voxel
 ))
;; OBSOLETE: renamefile

(defconst dukedef-keywords-2
  '( anim basepal detail glow fogpal frame hud normal pal raw skin specular
 front right back left top bottom forward ceil ceiling down floor
 ;; ft rt bk lf lt up dn
 ))

(defconst dukedef-keywords-3
  '( alpha alphacut angadd artquality blue both copy crc32
 define detailscale dfactor echo
 file flag flags flipped floorpal fov forcefilter fps frame0 frame1
 glblend green hide horizfrac
 ID id ifcrc ifmatch include includedefault indexed intensity
 loadgrp lognumtiles mapart mapfile mapmd4 maptitle mhkfile
 name nobob nodepth nocompress nodownsize
 nofloorpal nofullbright noshades notrans offset orig_sizex orig_sizey panel
 parallaxbias parallaxscale red remappal remapself reverse scale sfactor
 shade shadeblue shadegreen shadered shiftleft smoothduration specfactor
 specpower specularfactor specularpower src size surf surface
 texhitscan tile tile0 tile1 truenpot undef
 xadd xoff xoffset xscale wide yadd yoff yoffset yscale zadd
 ;; bottom forward raw texture top sb sg sr
 ;;
 ;; BUILDGDX : includeif
 includeif
 ))

(defconst dukedef-keywords-4 ;; game-side tokens
  '( addplayercolor animsounds cutscene delplayercolor globalgameflags keyconfig
 Music music newgamechoices Sound sound
 ))

(defconst dukedef-keywords-5 ;; BuildGDX
  '( rotate
 ))


;;; Font lock
(defgroup dukedef nil
  "Major mode for editing Duke3D/Eduke(32) def files."
  :group 'languages
)

(defvar dukedef-1-keywords 'dukedef-1-keywords)
(defvar dukedef-2-keywords 'dukedef-2-keywords)
(defvar dukedef-3-keywords 'dukedef-3-keywords)
(defvar dukedef-4-keywords 'dukedef-4-keywords)
(defvar dukedef-5-keywords 'dukedef-5-keywords)

(defface dukedef-1-keywords '((t (:inherit font-lock-type-face)))
  "*Block-enclosing keywords for Dukedef Mode." :group 'dukedef :group 'faces)
(defface dukedef-2-keywords '((t (:inherit font-lock-keyword-face)))
  "*Control-flow keywords    for Dukedef Mode." :group 'dukedef :group 'faces)
(defface dukedef-3-keywords '((t (:inherit font-lock-constant-face)))
  "*All other keywords       for Dukedef Mode." :group 'dukedef :group 'faces)
;;(defface dukedef-4-keywords '((t (:inherit font-lock-builtin-face)))
(defface dukedef-4-keywords '((t (:inherit font-lock-type-face)))
  "*BuildGDX DEF keywords    for Dukedef Mode." :group 'dukedef :group 'faces)
(defface dukedef-5-keywords '((t (:inherit font-lock-variable-name-face)))
  "*BuildGDX DEF keywords    for Dukedef Mode." :group 'dukedef :group 'faces)

(defconst dukedef-font-lock-keywords-1
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukedef-keywords-1) t) "\\_>") .  dukedef-1-keywords) )
  "Block-enclosing primitives highlighted in DUKEDEF mode."
)

(defconst dukedef-font-lock-keywords-2
  (append dukedef-font-lock-keywords-1
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukedef-keywords-2) t) "\\_>") . dukedef-2-keywords) ) )
  "Control-flow primitives highlighted in DUKEDEF mode."
)

(defconst dukedef-font-lock-keywords-3
  (append dukedef-font-lock-keywords-2
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukedef-keywords-3) t) "\\_>") . dukedef-3-keywords) ) )
  "All other primitives highlighted in DUKEDEF mode."
)

(defconst dukedef-font-lock-keywords-4
  (append dukedef-font-lock-keywords-3
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukedef-keywords-4) t) "\\_>") . dukedef-4-keywords) ) )
  "BuildGDX DEF keywords highlighted in DUKEDEF mode."
)

(defconst dukedef-font-lock-keywords-5
  (append dukedef-font-lock-keywords-4
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukedef-keywords-5) t) "\\_>") . dukedef-5-keywords) ) )
  "BuildGDX DEF keywords highlighted in DUKEDEF mode."
)


(defvar dukedef-font-lock-keywords dukedef-font-lock-keywords-5
  "Default highlighting expressions for DUKEDEF mode."
)


(defvar dukedef-mode-syntax-table
  (let ((dukedef-mode-syntax-table (make-syntax-table)))
    ; recognize braces pairs
    (modify-syntax-entry ?{ "(}" dukedef-mode-syntax-table)
    (modify-syntax-entry ?} "){" dukedef-mode-syntax-table)
    
    ; Comment styles are same as C++
    (modify-syntax-entry ?/  ". 124b" dukedef-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   dukedef-mode-syntax-table)
    (modify-syntax-entry ?\n "> b"    dukedef-mode-syntax-table)
    dukedef-mode-syntax-table)
  "Syntax table for dukedef-mode")


(defun dukedef-mode ()
  "Major mode for editing Eduke .def files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table dukedef-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(dukedef-font-lock-keywords))
  (setq major-mode 'dukedef-mode)
  (setq mode-name "Duke3D-DEF")
  (run-hooks 'dukedef-mode-hook))

;; --------------------------------

(provide 'dukedef-mode)

;;; dukedef-mode.el ends here
