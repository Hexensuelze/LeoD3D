;;; dukelog-mode.el -- Major mode for viewing/editing EDuke32/Ion Fury log files

;; This file is NOT part of Emacs.

;; Syntax Highlighting Only

;; Author: LeoD, by search&replace-editing:
;;  dukecon-mode by Philip Kutin, based on a tutorial by Scott Andrew Borton
;; Created: 2019-05-07
;; Keywords: Duke3D Eduke32 Fury log major-mode
;; Last updated: 2022-04-26      (EDuke32/Mapster32 r9954 -> r10033)




;; Copyright (C) 2019-2022 LeoD
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
;;  (require 'dukelog-mode)


;;; Code:
(defvar dukelog-mode-hook nil)

;;(add-to-list 'auto-mode-alist '("\\.log\\'"                  . dukelog-mode))
(add-to-list 'auto-mode-alist '("eduke32\\(.*\\)\\.log\\'"   . dukelog-mode))
(add-to-list 'auto-mode-alist '("mapster32\\(.*\\)\\.log\\'" . dukelog-mode))
(add-to-list 'auto-mode-alist '("fury\\(.*\\)\\.log\\'"      . dukelog-mode))

(defconst dukelog-keywords-01 ;; do not highlight
  '(
     DO_NOT_HIGHLIGHT
     compiled\ code
     created\ with
     Disperser\ out\ of\ shells
     HOLODUKE\ NOT\ FOUND\ YET
     JETPACK\ NOT\ FOUND\ YET
     ; No\ game\ controllers\ found
     No\ Spawn
     no\ updates\ available
     show\ map\ on
     very\ happy\ ammo\ added
     ;;
     ;; mimalloc
     pointer\ might\ not\ point\ to\ a\ valid\ heap\ region
   ))

(defconst dukelog-keywords-02 ;; Info messages / others
  '(
     ;; Highlight doesn't work with quotes, "(" , ")" , "|" ,
     ;; leading "\ " or trailing ":"
     ;; Mapster32 Ion Fury 2.0:
     ;Warning:\ Truncating\ name\ \"A_NUKEMUTANT_GDF_DEADHEAD\"
     ;;
     ;; Duke3D Voxel Pack tank warnings
     7630:\ warning:\ duplicate\ action\ \`ATANKSPIN\'\ ignored
     7631:\ warning:\ duplicate\ action\ \`ATANKSHOOTING\'\ ignored
     7632:\ warning:\ duplicate\ action\ \`ATANKWAIT\'\ ignored
     7633:\ warning:\ duplicate\ action\ \`ATANKDESTRUCT\'\ ignored
     7634:\ warning:\ duplicate\ action\ \`ATANKDEAD\'\ ignored
     ;;
     ;; LeoD's Ion Fury Mod warnings
     fury_leod/foobar.con:25:\ warning:\ sound\ 190\ already\ defined
     fury_leod/foobar.con:30:\ warning:\ sound\ 193\ already\ defined
     fury_leod/foobar.con:31:\ warning:\ sound\ 194\ already\ defined
     fury_leod/foobar.con:32:\ warning:\ sound\ 195\ already\ defined
     fury_leod/foobar.con:33:\ warning:\ sound\ 196\ already\ defined
     fury_leod/foobar.con:34:\ warning:\ sound\ 197\ already\ defined
     fury_leod/foobar.con:35:\ warning:\ sound\ 198\ already\ defined
     fury_leod/foobar.con:36:\ warning:\ sound\ 199\ already\ defined
     fury_leod/foobar.con:37:\ warning:\ sound\ 230\ already\ defined
     fury_leod/foobar.con:38:\ warning:\ sound\ 231\ already\ defined
     fury_leod/foobar.con:39:\ warning:\ sound\ 232\ already\ defined
     overwriting\ existing\ definition\ for\ sound
     ;;
     0\ warning 0\ warnings
     0\ error   0\ errors
     0-bpp\ windowed 8-bpp\ windowed   32-bpp\ windowed
                     8-bpp\ fullscreen 32-bpp\ fullscreen
     0-bit\ windowed 8-bit\ windowed   32-bit\ windowed
                     8-bit\ fullscreen 32-bit\ fullscreen
     Application\ parameters
     Autoload\ disabled
     Cache\ time
     Changed\ sectnum
     classic\ software
     debug:\ This\ level\ is\ missing\ a\ HUBSECTION
     Focus\ change
     garbage\ data
     High\ Resolution\ Pack
     Jonathon\ Fowler
     Ken\ Silverman
     keyboard\ layout
     mimalloc:\ warning
     Monsters\ off
     No\ game\ controllers\ found
     polygonal\ OpenGL
     Polymer\ subsystem
     Quick\ Exit
     Rendering\ method\ changed\ to\ great\ justice
     User\ Map
     151190\ bytes ; Atomic GAME.CON
      35992\ bytes ; Atomic DEFS.CON
      45482\ bytes ; Atomic USER.CON
     ;;
     LeoD
     Debug debug Info info Note note
     Polymer Polymost
     sv_saveandmakesnapshot snapshot workaround
   ))

(defconst dukelog-keywords-1 ;; objects
  '(
     EDuke32 Mapster32
     GRP grp grpinfo ZIP zip DAT dat
     CON con DEF def HLP hlp lua m32 MAP map mhk
     BIN bin CFG cfg DMO dmo H   h
     ART art BMP bmp JPG jpg PCX pcx PNG png TGA tga
     KVX kvx MD2 md2 MD3 md3
     ANM anm ivf
     FLAC flac MID mid OGG ogg RTS rts TMB tmb VOC voc WAV wav
     esv SAV sav
     ;PCK pck ;; Modem
     autoload
     maphack map\ hack\ file clip\ map
     DEFS\.CON defs\.con DUKE3D\.GRP duke3d\.grp DUKE\.RTS duke\.rts
     EDUKE\.CON eduke\.con GAME\.CON game\.con
     NAMES\.H names\.h USER\.CON user\.con
     eduke32\.cfg settings.\cfg mapster32.\cfg m32_settings.\cfg
     duke3d\.def editor\.def m32help\.hlp
     texturecache texturecache\.index textures textures\.cache

     ;; JFDuke:
     JFDuke3D

     ;; KenBuild:
     EKenBuild EKenBuild\ Editor

     ;; Shadow Warrior:
     VoidSW Wangulator SW\.GRP sw\.grp
     PAL pal SYM sym
     voidsw\.cfg voidsw_autoexec\.cfg voidsw_cvars\.cfg

     ;; Blood:
     NBlood
     kpf PK3 pk3
     AVI avi DEM dem INI ini ogv RFF rff sf2 SMK smk
     nblood\.cfg nblood_cvars\.cfg
     ;bloodgdx\.dat

     ;; Redneck Rampage:
     Rednukem RRMapster32 REDNECK\.GRP redneck\.grp REDNECK\.RTS redneck\.rts
     rednukem\.cfg
     ;rrgdx\.dat

     ;; Powerslave/Exhumed:
     Exhumed Exhumed_editor
     MOV mov RMP rmp VCR vcr SETUP.\CFG STUFF\.DAT stuff\.dat
     pcexhumed\.cfg pcexhumed_cvars\.cfg
     ;psgdx\.dat

     ;; TekWar:
     TekWar
     etekwar\.cfg etekwar_cvars\.cfg

     ;; Witchaven:
     Witchaven
     ewitchaven\.cfg ewitchaven_cvars\.cfg

     ;; Ion Fury:
     Ion\ Fury
     ext
     fury\.cfg

     ;; BuildGDX:
     BUILD BuildGdx
     BloodGDX DukeGDX LSPGDX PowerslaveGDX
     RedneckGDX TekWarGDX WangGDX WitchavenGDX
     bloodgdx\.dat lspgdx\.dat psgdx\.dat rrgdx\.dat twgdx\.dat whgdx\.dat

     ;; Raze:
     raze\.pk3

     ;; MISC:
     de-DE
   ))

(defconst dukelog-keywords-2 ;; error/warning messages
  '(
     angoff\ used\ after\ a\ pitch\,\ roll\,\ or\ md\[pivot\]<x\|y\|z>off\ token
      pitch\ used\ after\ a\ roll\,\ or\ md\[pivot\]<x\|y\|z>off\ token
       roll\ used\ after\ md\[pivot\]<x\|y\|z>off\ token
     mdxoff\ used\ after\ md\[pivot\]<y\|z\off\ token
     mdyoff\ used\ after\ md\[pivot\]zoff\ token
     ;
     already\ defined already\ used
     An\ error\ occurred
     attempted\ to\ save\ state\ in\ invalid\ gamemode
     bad\ handle bad\ voice
     bailed\ out bailing\ out
     Can\'t\ find Can\'t\ open can\'t\ open can\'t\ reach
     corruption\ level
     could\ not\ find\ file
     could\ not\ find\ main\ data\ file
     could\ not\ find
     does\ not\ exist
     doesn\'t\ match
     duplicate\ action duplicate\ definition duplicate\ sound
     empty\ skill\ name
     EOF\ in\ comment
     Error\ compiling\ CON\ files
     exceeds\ limit
     Excessive\ script\ errors
     expecting\ symbol
     Failed\ including
     Fatal\ error
     Fatal\ Signal
     file\ corrupt
     file\ not\ found
     Found\ no\ recognized\ game\ data
     GL\ DEBUG
     GL\ ERROR GL_INVALID_VALUE
     Incompatible\ savegame Incompatible\ Save
     invalid\ array invalid\ character invalid\ sector
     Invalid\ sound invalid\ sound
     is\ read\ only
     HEAVY\ corruption
     label\ starts\ with\ a\ digit
     Map\ warning
     menu\ has\ no\ active\ entries
     masks\ keyword
     moderate\ corruption
     No\ number\ given
     No\ palette\ found No\ palette\ loaded
     no\ recognized\ game\ data
     not\ a\ valid\ command\ or\ cvar
     not\ available not\ defined not\ found not\ loaded not\ supported
     Out\ of\ memory out\ of\ memory
     out\ of\ range
     out\ of\ the\ range
     performance\ warning
     player\ killed\ by\ cursectnum
     should\ be
     TextureCache\ uninited
     too\ many\ labels\ defined
     unable\ to\ configure
     Unable\ to\ create
     Unable\ to\ Load
     Unable\ to\ load
     Unable\ to\ play
     Unexpected\ EOF
     unknown\ data unknown\ token
     unknown\ error
     Unrecognized\ token
     unsupported\ format
     very\ sad\ sector
     You\ need\ SDL or\ newer\ to\ run
     ;;
     Bad bad bisect bounds
     Can\ not can\ not
     Cannot cannot
     Can\'t can\'t
     Corrupt corrupt Corrupted corrupted Corruption corruption
     Could\ not could\ not
     Couldn\'t  couldn\'t
     Debug debug
     Does\ not does\ not
     Doesn\'t  doesn\'t
     duplicate
     ERROR Error error errors exceeded exceeds
     Expected expected expecting
     FAIL fail FAILED Failed failed Failure failure
     Fatal fatal_exit fatal
     Ignoring ignored Illegal illegal Incompatible incompatible
     Invalid invalid
     Malformed malformed mismatch mismatched Missing missing
     No no NOT Not not
     out\ of
     overflow
     SIGSEGV
     Truncating truncating Truncated truncated
     Unable unable Undefined undefined Unexpected unexpected unhandled
     Unknown unknown Unrecognized unrecognized Unsupported unsupported
     WARNING Warning warning warnings Wrong wrong
     ;;  ERR WARN INFO GFX CON ASS GL VM 7
     ;; MISC:
     en-US
   ))

(defconst dukelog-keywords-3 ;; activity start messages
  '(
     adding Checking Checksumming Compiling Connecting Detecting
     Entering Enumerating Executing Generating
     Including Initialising Initializing initializing Loading\ module Loading
     Playing Post-processing
     Relocating Removing Resizing resizing Restarting restarting Running
     Saving Scanning Searching Setting Switching Syncing
     Trying Uninitialising Uninitializing Using using
;; including initialising initializing Initialisation Initialization overwriting
     ;; 2nd Level:
     ;Allocating Creating Getting Freeing Releasing Unloading Waiting
     ;; Blood:
     Creating Waiting
   ))

(defconst dukelog-keywords-4 ;; activity end messages
  '(
     Cache\ size\ increased
     created\ successfully
     Initialization\ complete
     Loaded\ map\ hack\ file
     No\ maphack\ found\ for\ map
     PR\ \:\ Board\ loaded
     PR\ \:\ Cache\ increased Cache\ increased
     Game\ Saved Game\ saved 
     PR\ \:\ Initialization\ complete
     Saved\ screenshot
     Script\ compiled
     ;;
     Added added complete changed Compiled compiled created
     Detected detected Enabled enabled Finished finished
     Increased increased Initialised Initialized initialised initialized
     Loaded loaded Mapped Opened opened
     Refreshed reloaded Relocated Restored restored
     Saved saved Started started unloaded written Wrote wrote
     Found
     ; detected Disabled disabled Found found overwritten
     ; succeeded Successfully successfully
   ))


;;; Font lock
(defgroup dukelog nil
  "Major mode for viewing/editing Duke3D/Eduke(32) log files."
  :group 'languages
)

(defvar dukelog-non-keywords     'dukelog-non-keywords)
(defvar dukelog-info-keywords    'dukelog-info-keywords)
(defvar dukelog-object-keywords  'dukelog-object-keywords)
(defvar dukelog-message-keywords 'dukelog-message-keywords)
(defvar dukelog-start-keywords   'dukelog-start-keywords)
(defvar dukelog-end-keywords     'dukelog-end-keywords)

(defface dukelog-non-keywords     '((t ()))
  "*Do not  highlight in Dukelog Mode." :group 'dukelog :group 'faces)
(defface dukelog-info-keywords    '((t (:inherit font-lock-comment-face)))
  "*Information keywords for Dukelog Mode." :group 'dukelog :group 'faces)
(defface dukelog-object-keywords  '((t (:inherit font-lock-keyword-face)))
  "*Object  keywords for Dukelog Mode." :group 'dukelog :group 'faces)
(defface dukelog-message-keywords '((t (:inherit font-lock-warning-face)))
  "*Message keywords for Dukelog Mode." :group 'dukelog :group 'faces)
(defface dukelog-start-keywords   '((t (:inherit font-lock-constant-face)))
  "*Start   keywords for Dukelog Mode." :group 'dukelog :group 'faces)
;;(defface dukelog-end-keywords  '((t ()))
;;(defface dukelog-end-keywords  '((t (:inherit font-lock-builtin-face)))
;;(defface dukelog-end-keywords  '((t (:inherit font-lock-comment-face)))
;;(defface dukelog-end-keywords  '((t (:inherit font-lock-constant-face)))
;;(defface dukelog-end-keywords  '((t (:inherit font-lock-doc-face)))
;;(defface dukelog-end-keywords  '((t (:inherit font-lock-function-name-face)))
;;(defface dukelog-end-keywords  '((t (:inherit font-lock-keyword-face)))
;;(defface dukelog-end-keywords  '((t (:inherit font-lock-negation-char-face)))
;;(defface dukelog-end-keywords  '((t (:inherit font-lock-string-face)))
;;(defface dukelog-end-keywords  '((t (:inherit font-lock-type-face)))
;;(defface dukelog-end-keywords  '((t (:inherit font-lock-variable-name-face)))
;;(defface dukelog-end-keywords  '((t (:inherit font-lock-warning-face)))
(defface dukelog-end-keywords  '((t (:inherit font-lock-variable-name-face)))
  "*End     keywords for Dukelog Mode." :group 'dukelog :group 'faces)

(defconst dukelog-font-lock-keywords-01
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukelog-keywords-01) t) "\\_>") .  dukelog-non-keywords) )
  "Strings not to be highlighted in DUKELOG mode."
)

(defconst dukelog-font-lock-keywords-02
  (append dukelog-font-lock-keywords-01
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukelog-keywords-02) t) "\\_>") . dukelog-info-keywords) ) )
  "Messages highlighted in DUKELOG mode."
)

(defconst dukelog-font-lock-keywords-1
  (append dukelog-font-lock-keywords-02
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukelog-keywords-1) t) "\\_>") .  dukelog-object-keywords) ) )
  "Object primitives highlighted in DUKELOG mode."
)

(defconst dukelog-font-lock-keywords-2
  (append dukelog-font-lock-keywords-1
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukelog-keywords-2) t) "\\_>") . dukelog-message-keywords) ) )
  "Message primitives highlighted in DUKELOG mode."
)

(defconst dukelog-font-lock-keywords-3
  (append dukelog-font-lock-keywords-2
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukelog-keywords-4) t) "\\_>") . dukelog-end-keywords) ) )
  "Start primitives highlighted in DUKELOG mode."
)

(defconst dukelog-font-lock-keywords-4
  (append dukelog-font-lock-keywords-3
  `((,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukelog-keywords-3) t) "\\_>") . dukelog-start-keywords) ) )
  "Start primitives highlighted in DUKELOG mode."
)


(defvar dukelog-font-lock-keywords dukelog-font-lock-keywords-4
  "Default highlighting expressions for DUKELOG mode."
)


(defvar  dukelog-mode-syntax-table
  (let ((dukelog-mode-syntax-table (make-syntax-table)))
    ; Recognize braces pairs
    (modify-syntax-entry ?{ "(}" dukelog-mode-syntax-table)
    (modify-syntax-entry ?} "){" dukelog-mode-syntax-table)
    
    ; Comment styles are same as C++
    (modify-syntax-entry ?/  ". 124b" dukelog-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   dukelog-mode-syntax-table)
    (modify-syntax-entry ?\n "> b"    dukelog-mode-syntax-table)
    dukelog-mode-syntax-table)
  "Syntax table for dukelog-mode")


(defun dukelog-mode ()
  "Major mode for editing Eduke32 .log files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table dukelog-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(dukelog-font-lock-keywords))
  (setq major-mode 'dukelog-mode)
  (setq mode-name "Duke3D-LOG")
  (run-hooks 'dukelog-mode-hook))

;; --------------------------------

(provide 'dukelog-mode)

;;; dukelog-mode.el ends here