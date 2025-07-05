;;; dukecon-mode.el -- Major mode for editing Duke Nukem 3D/EDuke32 .con files

;; This file is NOT part of Emacs.

;; NOTE : The ElDoc section is outdated.

;; Author: Philipp Kutin, based on a tutorial by Scott Andrew Borton

;; Created: 2007-09-01
;; Keywords: Duke3D EDuke32 con major-mode
;; Last updated by LeoD: 2025-06-27 (EDuke32 r10609 - r10609 -> r10619)
;; -> .../duke3d/src/gamedef.cpp (svn6432 - r1????): vm_keywords[],iter_tokens[]
;; -> .../duke3d/src/gamedef.h   (svn6460 - svn6483) : vm_keywords []
;; -> .../duke3d/src/gamedef.c (- svn6431): altkeyw [] , *keyw[] , iter_tokens[]

;; Copyright (C) 2016-2025 LeoD
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
;;  (require 'dukecon-mode)
;;  (setq auto-mode-alist (append '(("\\.\\([Cc][Oo][Nn]\\)$" . dukecon-mode))  auto-mode-alist ))

;;; Commentary:
;; 
;; The mode is based on a tutorial about Emacs
;; mode creation which can be found here:
;; http://two-wugs.net/emacs/mode-tutorial.html
;; (link dead as of 2009-03-23)

;; The following features are implemented:
;; imenu, eldoc, movement by top-level definitions (states, onevents, actors)
;; using the keys for by-defun-movement (`C-M-a' and `C-M-e' by default),
;; outline (outlining top-level definitions).
;; A function `dukecon-find-definition' which takes the symbol at point
;; and searches `dukecon-main-con-file-name' and files included therein
;; for a "define" or "gamevar" definition of that symbol. If it is found,
;; the line containing the definition is printed on the mode line.
;; For this to work, `dukecon-working-directory' must also be set.

;; The following keybindings are made:
;; `C-c C-d': `dukecon-find-definition'
;; `C-c C-s', `C-c C-e', `C-c C-a' insert a new state, onevent, and actor
;; block, respectively.

;; Also, check out the customization group `dukecon' for some
;; user-customizable options.

;;; Code:
(defvar dukecon-mode-hook nil)
(defvar dukecon-mode-map
  (let ((dukecon-mode-map (make-sparse-keymap)))
    (define-key dukecon-mode-map "\C-c\C-d" 'dukecon-find-definition)
    (define-key dukecon-mode-map "\C-c\C-s" 'dukecon-insert-state)
    (define-key dukecon-mode-map "\C-c\C-e" 'dukecon-insert-event)
    (define-key dukecon-mode-map "\C-c\C-a" 'dukecon-insert-actor)
    dukecon-mode-map)
  "Keymap for DUKECON major mode")

(add-to-list 'auto-mode-alist '("\\.con\\'" . dukecon-mode))
(add-to-list 'auto-mode-alist '("\\.CON\\'" . dukecon-mode))


;; block-enclosing keywords
(defconst dukecon-keywords-block
  '( actor appendevent appendstate defstate enda endevent ends eventloadactor
 onevent prependstate state useractor
 include includedefault includeoptional \#include
 ))

;; control flow keywords
(defconst dukecon-keywords-flow
  '( break case continue default else endswitch exit for
 ifaction ifactioncount ifactor ifactorsound ifactornotstayput ifa ifae ifai
 ifand ifb ifbe
 ifangdiffl ifawayfromwall ifboth ifbulletnear ifcansee ifcanseetarget
 ifcanshoottarget ifceilingdistl ifclient ifcount ifcutscene ifdead ife
 ifeither iffloordistl ifg ifge ifgapzl ifgotweaponce ifhitspace ifhitweapon
 ifinouterspace ifinspace ifinwater ifl ifle ifmove ifmultiplayer ifn
 ifnosounds ifnotmoving ifonwater ifor ifoutside ifp ifpdistg ifpdistl
 ifphealthl ifpinventory ifplaybackon ifplayersl ifrespawn ifrnd ifserver
 ifsound ifspawnedby ifspritepal ifsquished ifstrength ifvara ifvarae ifvarand
 ifvarb ifvarbe ifvarboth ifvare ifvareither ifvarg ifvarge ifvarl ifvarle
 ifvarn ifvaror ifvarvara ifvarvarae ifvarvarand ifvarvarb ifvarvarbe
 ifvarvarboth ifvarvare ifvarvareither ifvarvarg ifvarvarge ifvarvarl
 ifvarvarle ifvarvarn ifvarvaror ifvarvarxor ifvarxor ifwasweapon ifxor
 return switch terminate
 whilee whilel whilen whilevare whilevarl whilevarn
 whilevarvare whilevarvarl whilevarvarn
 yield
 ;; Redneck Rampage only control flow keywords
 ifactorhealthg ifactorhealthl ifcoop ifhittruck ifonmud
 ifsounddist ifsoundid iftipcow 
 ))

;; iteration list keywords
(defconst dukecon-keywords-iter
  '( activelights allsectors allsprites allspritesbystat allspritesbysect
 allwalls drawnsprites lights loopofwall range
 spritesofsector sprofsec sprofstat spritesofstatus wallsofsector walofsec
 ))

;; special vars keywords
(defconst dukecon-keywords-special
  '( __actor__ __sprite__ __spriteext__
 actorvar input paldata player playervar projectile sector sprite wall
 thisprojectile tiledata tspr userdef
 ;;
 RETURN THISACTOR WORKSLIKE
 ;;
 enemy enemystayput notenemy
 ))

;; the rest
(defconst dukecon-keywords-misc
  '( \#define abs action
 activate activatebysector activatecheat actorsound add addammo
 addinventory addkills addlog addlogvar addphealth addstrength addvar addvarvar
 addweapon addweaponvar ai al and andvar andvarvar angoff angoffvar array
 betaname cactor calchypotenuse cansee canseespr capia capis changespritesect
 changespritestat cheatkeys checkactivatormotion checkavailinven
 checkavailweapon ck clamp clearmapstate clipdist clipmove clipmovenoslide cmenu
 copy cos count cstat cstator damageeventtile damageeventtilerange
 dc dcd debris debug define definecheat definecheatdescription
 definegamefuncname definegametype definelevelname defineprojectile definequote
 defineskillname definesound definevolumeflags definevolumename digitalnumber
 digitalnumberz displayrand displayrandvar displayrandvarvar dist div divr
 divrd divru divscale divvar divvarvar dragpoint drawline256 drawlinergb
 dynamicremap dynamicsoundremap echo endofgame endoflevel enhanced
 eqspawn eqspawnvar eshoot eshootvar espawn espawnvar ezshoot ezshootvar fall
 findnearactor findnearactor3d findnearactor3dvar findnearactorvar
 findnearactorz findnearactorzvar findnearsprite findnearsprite3d
 findnearsprite3dvar findnearspritevar findnearspritez findnearspritezvar
 findotherplayer findplayer flash
 gamearray gamestartup gametext gametextz gamevar getactor geta getav
 getactorangle getactorvar getangle getangletotarget getarraysequence
 getarraysize getceilzofslope getclosestcol getcurraddress getflorzofslope
 getgamefuncbind geti getincangle getinput getkeyname getlastpal
 getmusicposition getngcflags getp getplayer getplayerangle getplayervar
 getpname getprojectile getpv gets getsector
 gettextureceiling gettexturefloor getthisprojectile getticks gettiledata
 gettimedate gettspr getu getuserdef getw getwall getzrange globalsound
 globalsoundvar gmaxammo guniqhudid guts
 headspritesect headspritestat hitradius hitradiusvar hitscan
 inittimer insertspriteq inv jump killit klabs
 ldist lineintersect loadmapstate lockplayer lotsofglass
 mail mikesnd minitext mod modvar modvarvar money move movesector movesprite
 mul mulscale mulvar mulvarvar music myos myospal myospalx myosx
 neartag nextsectorneighborz nextspritesect nextspritestat nullop
 operate operateactivators operatemasterswitches operaterespawns operatesectors
 or orvar orvarvar palfrom paper pkick precache prevspritesect prevspritestat
 preloadtrackslotforswap print pstomp qgetsysstr qputs qspawn qspawnvar qsprintf
 qstrcat qstrcmp qstrcpy qstrdim qstrncat qstrlen qsubstr quake quote
 rand randvar randvarvar rayintersect readarrayfromfile readgamevar
 redefinequote resetactioncount resetcount resetplayer resetplayerflags
 resizearray respawnhitag rotatepoint rotatesprite rotatesprite16 rotatespritea
 save savegamevar savemapstate savenn scalevar screenpal screensound screentext
 scriptsize sectclearinterpolation sectgethitag sectgetlotag
 sectorofwall sectsetinterpolation set seta setactor setactorangle
 setactorsoundpitch setactorvar setarray setarraysequence setaspect setav
 setcfgname setdefname setgamename setgamepalette seti setinput
 setmusicposition setngcflags setp setplayer
 setplayerangle setplayervar setprojectile setpv sets setsector setsprite
 setthisprojectile settiledata settspr setu setuserdef setvar setvarvar setw
 setwall shadeto shiftl shiftr shiftvarl shiftvarr shiftvarvarl shiftvarvarr
 shoot shootvar showview showviewunbiased sin sizeat sizeto sleeptime smaxammo
 sound soundonce soundoncevar soundvar spawn spawnceilingglass spawnwallglass
 spawnwallstainedglass spgethitag spgetlotag
 spriteflags spritenopal spritenoshade spritenvg spritepal spriteshadow sqrt ssp
 startcutscene startlevel startscreen starttrack starttrackslot starttrackvar
 stopactorsound stopallmusic stopallsounds stopsound stopsoundvar strength
 string sub subvar subvarvar swaparrays swaptrackslot time tip tossweapon
 udc undefinecheat undefinegamefunc undefinelevel undefineskill undefinevolume
 updatesector updatesectorneighbor updatesectorneighborz updatesectorz userquote
 var wackplayer writearraytofile xor xorvar xorvarvar zshoot zshootvar
 ;; Redneck Rampage only misc keywords
 destroyit feathers iseat larrybird slapplayer smackbubba soundtagonce
 strafeleft straferight tearitup 
 ))


;;; Font lock
(defgroup dukecon nil
  "Major mode for editing Duke3D/Eduke(32) con files."
  :group 'languages
  :link `(url-link :tag "Mail the author"
          ,(concat (string 109 97 105 108 116 111 58 104 101 108 105
120 104 111 114 110 101 100 64 103 109 97 105 108 46 99 111 109)
"?subject=Dukecon Mode")))

(defvar dukecon-block-keywords   'dukecon-block-keywords)
(defvar dukecon-flow-keywords    'dukecon-flow-keywords)
(defvar dukecon-iter-keywords    'dukecon-iter-keywords)
(defvar dukecon-special-keywords 'dukecon-special-keywords)
(defvar dukecon-misc-keywords    'dukecon-misc-keywords)

(defface dukecon-block-keywords   '((t (:inherit font-lock-type-face)))
  "*Block-enclosing keywords for Dukecon Mode." :group 'dukecon :group 'faces)
(defface dukecon-flow-keywords    '((t (:inherit font-lock-keyword-face)))
  "*Control-flow keywords for Dukecon Mode."    :group 'dukecon :group 'faces)
(defface dukecon-iter-keywords    '((t (:inherit font-lock-variable-name-face)))
  "*Iteration-list keywords for Dukecon Mode."  :group 'dukecon :group 'faces)
(defface dukecon-special-keywords '((t (:inherit font-lock-variable-name-face)))
  "*Special-var keywords for Dukecon Mode."     :group 'dukecon :group 'faces)
(defface dukecon-misc-keywords    '((t (:inherit font-lock-constant-face)))
  "*All other keywords for Dukecon Mode."       :group 'dukecon :group 'faces)

(defconst dukecon-font-lock-keywords-block
  `(
    (,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
     dukecon-keywords-block) t) "\\_>") .  dukecon-block-keywords) )
  "Block-enclosing primitives highlighted in DUKECON mode.")

(defconst dukecon-font-lock-keywords-flow
  (append dukecon-font-lock-keywords-block
          `(
            (,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
            dukecon-keywords-flow) t) "\\_>")
            . dukecon-flow-keywords) ) )
  "Control-flow primitives highlighted in DUKECON mode.")

(defconst dukecon-font-lock-keywords-iter
  (append dukecon-font-lock-keywords-flow
          `(
            (,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
            dukecon-keywords-iter) t) "\\_>")
            . dukecon-iter-keywords) ) )
  "Iteration-list primitives highlighted in DUKECON mode.")

(defconst dukecon-font-lock-keywords-special
  (append dukecon-font-lock-keywords-iter
          `(
            (,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
            dukecon-keywords-special) t) "\\_>")
            . dukecon-special-keywords) ) )
  "Special primitives highlighted in DUKECON mode.")

(defconst dukecon-font-lock-keywords-misc
  (append dukecon-font-lock-keywords-special
          `(
            (,(concat "\\_<" (regexp-opt (mapcar 'symbol-name
            dukecon-keywords-misc) t) "\\_>")
            . dukecon-misc-keywords)
;            ("\\<\\(TODO\\|FIXME\\|HACK\\)\\>:" 1 font-lock-warning-face)
            )
          )
  "All other primitives highlighted in DUKECON mode.")

(defvar dukecon-font-lock-keywords dukecon-font-lock-keywords-misc
  "Default highlighting expressions for DUKECON mode.")


(defcustom dukecon-tab-width 4
  "Tab width for Dukecon Mode."
  :group 'dukecon)

(defvar dukecon-paragraph-start "^\\(state\\|onevent\\|\\(user\\|eventload\\)?actor\\)")
(defvar dukecon-paragraph-seperate "^end\\(s\\|event\\|a\\)")

(defvar dukecon-outline-regexp dukecon-paragraph-start)

(defvar dukecon-imenu-generic-expression
  (let ((id-regex "[[:alpha:]_][[:alnum:]_]+")) ;duke syntax is actually more permissive
    `(("states" ,(concat "^\\(state\\)[     ]+\\(" id-regex "\\)") 2)
      ("events" ,(concat "^\\(onevent\\)[     ]+\\(" id-regex "\\)") 2)
      ("actors" ,(concat "^\\(\\(?:user\\|eventload\\)?actor\\)[     ]+\\(" id-regex "\\)") 2)
      )))

(defun dukecon-beginning-of-defun ()
  (interactive)
  (search-backward-regexp dukecon-paragraph-start))

(defun dukecon-end-of-defun ()
  (interactive)
  (search-forward-regexp dukecon-paragraph-seperate))

(defun dukecon-indent-line ()
  "Indent current line as DUKECON code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)  ; First line is always non-indented
    (let ((not-indented t)
          cur-indent
          (looking-at-break (looking-at "^[ \t]*break\\>")))

      (save-excursion
        (cond

         ;; If the line we are looking at the end of a block, then decrease the indentation
         ((looking-at "^[ \t]*}")
          (forward-line -1)
          (setq cur-indent (max 0 (- (current-indentation) dukecon-tab-width))))

         ;; state/actor/event blocks cannot be nested in CON, so no indentation here
         ((looking-at "^[ \t]*\\(end\\(a\\|s\\|event\\)\\|eventloadactor\\|useractor\\|actor\\|onevent\\|define\\)\\_>")
          (setq cur-indent 0))

         ((looking-at "^[ \t]*endswitch\\_>")
          (forward-line -1)
          (while (not (or (looking-at "^[ \t]*switch\\_>") (bobp)))
            (forward-line -1))
          (if (looking-at "^[ \t]*switch\\_>")
              (setq cur-indent (current-indentation))))

         ((looking-at "^[ \t]*case\\_>")
          (forward-line -1)
          (while (not (or (looking-at "^[ \t]*switch\\_>") (looking-at "^[ \t]*case\\_>") (bobp)))
            (forward-line -1))
          (if (looking-at "^[ \t]*switch\\_>")
              (setq cur-indent (+ (current-indentation) (/ dukecon-tab-width 2)))
            (if (looking-at "^[ \t]*case\\_>")
                (setq cur-indent (current-indentation)))))

         (t
          (while not-indented ; Iterate backwards until we find an indentation hint
            (forward-line -1)
            (if (looking-at "^[ \t]*case\\_>")
                (setq  not-indented nil
                       cur-indent (if looking-at-break
                                      (current-indentation)
                                    (+ (current-indentation) (/ dukecon-tab-width 2))))
              ;; This hint indicates that we need to indent at the level of the end... or } token
              (if (looking-at "[ \t]*\\(end\\(a\\|s\\|event\\)\\_>\\|}\\)")
                  (setq  not-indented nil  cur-indent (current-indentation))
                ;; This hint indicates that we need to indent an extra level
                (if (looking-at "\\(eventloadactor\\|useractor\\|actor\\|\\(local[ \t]+\\)?state\\|onevent\\)\\_>")
                    (setq  not-indented nil  cur-indent (+ (current-indentation) dukecon-tab-width))
                  (if (looking-at ".*{")
                      (progn
                        (let ((numlbr (count-matches "{" (line-beginning-position) (line-end-position)))
                              (numrbr (count-matches "}" (line-beginning-position) (line-end-position))))
                          (setq  not-indented nil
                                 cur-indent (+ (current-indentation)
                                               (if (> numlbr numrbr) dukecon-tab-width 0)))))
                    (if (bobp)
                        (setq not-indented nil))))))))
         ))  ; cond/save-excursion
      (if cur-indent
          (indent-line-to cur-indent)
        ;; If we didn't see an indentation hint, then allow no indentation
        (indent-line-to 0)))))

(defvar dukecon-mode-syntax-table
  (let ((dukecon-mode-syntax-table (make-syntax-table)))
    ; recognize braces pairs
    (modify-syntax-entry ?{ "(}" dukecon-mode-syntax-table)
    (modify-syntax-entry ?} "){" dukecon-mode-syntax-table)
    
    ; Comment styles are same as C++
    (modify-syntax-entry ?/ ". 124b" dukecon-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" dukecon-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" dukecon-mode-syntax-table)
    dukecon-mode-syntax-table)
  "Syntax table for dukecon-mode")


;;; Eldoc support
(setq dukecon-eldoc-obarray (make-vector 307 0))

(defun dukecon-add-eldoc-strings ()
  (dolist (l dukecon-eldoc-string-list)
    (set (intern (car l) dukecon-eldoc-obarray) (cadr l))))

(defun dukecon-eldoc-function ()
  "Returns a documentation string appropriate for the current context or nil."
  (let* ((sym (intern-soft (thing-at-point 'symbol) dukecon-eldoc-obarray))
         (doc (symbol-value sym)))
    (if sym nil
      (save-excursion
        (catch 'break
          (while (search-backward-regexp "\\_<.*?\\_>" (point-at-bol) t)
            (setq sym (intern-soft (match-string-no-properties 0) dukecon-eldoc-obarray))
            (when sym
              (setq doc (symbol-value sym))
              (throw 'break t))
            ))))
    (when doc
      (put-text-property 0 (- (match-end 0) (match-beginning 0)) 'face 'font-lock-keyword-face doc)
      doc)))

(defconst dukecon-eldoc-orientation-string
  (let ((f '(lambda (s) (propertize s 'face 'font-lock-keyword-face)))
        (g '(lambda (s) (propertize s 'face 'font-lock-comment-face))))
    (concat "\norientation: "
            (funcall f "1") (funcall g ":t1 ") (funcall f "2") (funcall g ":320x200 ")
            (funcall f "4") (funcall g ":invY ") (funcall f "8") (funcall g ":clip start[ud]most ")
            (funcall f "16") (funcall g ":center top-left ") (funcall f "32") (funcall g ":t2 ")
            (funcall f "64") (funcall g ":masking off"))))

(defconst dukecon-eldoc-string-list 
  `(
    ("activate" "activate <lotag>\n(Pre-1.3D)")
    ("activatecheat" "activatecheat <cheat number>")
    ("calchypotenuse" "calchypotenuse <<ret>> <x> <y>")
    ("clearmapstate" "clearmapstate <level>")
    ("cmenu" "cmenu <value>")
    ("clipmove" "clipmove <<ret>> <<x>> <<y>> <z> <<sectnum>> <xvect> <yvect> <walldist> <floordist> <ceildist> <clipmask>")
    ("clipmovenoslide" "clipmovenoslide <<ret>> <<x>> <<y>> <z> <<sectnum>> <xvect> <yvect> <walldist> <floordist> <ceildist> <clipmask>")
    ("digitalnumberz" "digitalnumberz <tilenum> <x> <y> <number> <shade> <pal> <orientation> <x1> <y1> <x2> <y2> <digitalscale> ")
    ("gamearray" "gamearray <name> <size>")
    ("getarraysize" "getarraysize <array name> <<return>>")
    ("getceilzofslope" "getceilzofslope <sectnum> <x> <y> <<return>>")
    ("getclosestcol" "getclosestcol <return> <color> <limit>")
    ("getflorzofslope" "getflorzofslope <sectnum> <x> <y> <<return>>")
    ("getkeyname" "getkeyname <quoteID> <funcID> <key>")
    ("getticks" "getticks <<return>>")
    ("gettimedate" "gettimedate <<sec>> <<min>> <<hour>> <<mday>> <<mon>> <<year>> <<wday>> <<yday>>")
;    ("gettspr" "gettspr ")
    ("headspritesect" "headspritesect <<sprite>> <sectnum>")
    ("headspritestat" "headspritestat <<sprite>> <statenum>")
    ("inittimer" "inittimer <rate>")
    ("lineintersect" "lineintersect  <x1> <y1> <z1>  <x2> <y2> <z2>  <x3> <y3>  <x4> <y4>  <<intx>> <<inty>> <<intz>> <<ret>>")
    ("nextspritesect" "nextspritesect <<next>> <current>")
    ("nextspritestat" "nextspritestat <<next>> <current>")
    ("prevspritesect" "prevspritesect <<prev>> <current>")
    ("prevspritestat" "prevspritestat <<prev>> <current>")
    ("qgetsysstr" "qgetsysstr <<quoteID>> <strID>")
    ("qsubstr" "qsubstr <<destQuote>> <srcQuote> <start> <length>")
    ("qsprintf" "qsprintf <<destQuote>> <formatQuote> [args...]")
    ("rayintersect" "rayintersect <x1> <y1> <z1>  <xv> <yv> <zv>  <x3> <y3>  <x4> <y4>  <<intx>> <<inty>> <<intz>> <<ret>>")
    ("readarrayfromfile" "readarrayfromfile <array name> <quote number>")
    ("resizearray" "resizearray <array name> <new size>")
    ("scriptsize" "scriptsize <size>")
    ("sectsetinterpolation", "sectsetinterpolation <sector>")
    ("sectclearinterpolation", "sectclearinterpolation <sector>")
    ("setarray" "setarray <gamearray>[<index>] <gamevar>")
    ("setcfgname" "setcfgname <filename>")
    ("setdefname" "setdefname <filename>")
    ("setgamename" "setgamename <name>")
    ("setgamepalette" "setgamepalette <basePalIdx>")
;    ("settspr" "settspr ")
    ("spritenopal" "spritenopal <tilenum>")
    ("time" "time <gamevar>\n(No-op)")
    ("writearraytofile" "writearraytofile <array name> <quote number>")

    ("action" "action <name> <startframe> <frames> <viewtype> <incvalue> <delay>")
    ("action" "action <name>")
    ("actor" "actor <name> <strength> <action> <move> <ai> { actor code } enda")
    ("addammo" "addammo <weapon> <amount>")
    ("addinventory" "addinventory <item> <amount>")
    ("addkills" "addkills <number>")
    ("addlogvar" "addlogvar <gamevar>")
    ("addphealth" "addphealth <number>")
    ("addstrength" "addstrength <number>")
    ("addvar" "addvar <gamevar> <number>")
    ("addvarvar" "addvarvar <gamevar1> <gamevar2>")
    ("addweapon" "addweapon <weapon> <amount>")
    ("addweaponvar" "addweaponvar <var1> <var2>")
    ("andvar" "andvar <gamevar> <value>")
    ("andvarvar" "andvarvar <gamevar1> <gamevar2>")
    ("angoff" "angoff <number>")
    ("cactor" "cactor <tilenum>")
    ("canseespr" "canseespr <spriteID1> <spriteID2> <returnvar>")
    ("changespritesect" "changespritesect <actorid> <sectnum>")
    ("changespritestat" "changespritestat <sprite id> <statnum>")
    ("clipdist" "clipdist <number>")
    ("action" "action <name> [<startframe> <frames> <viewtype> <incvalue> <delay>]")
    ("actor" "actor <name> <strength> <action> <speed> <aifunction> {actor code} enda")
    ("ai" "ai <name> [<action> <speed> <type>]")
;    ("state" "state <name> {code} ends")
;    ("state" "state <name>")
    ("useractor" "useractor <type> <name> <strength> {actor code}")
;    ("action" "action <name>")
;    ("ai" "ai <name>")
;    ("state" "state <statename>")
    ("cos" "cos <gamevar> <gamevar2>")
    ("count" "count <number>")
    ("cstat" "cstat 1:b 2:t 4:fx 8:fy 16:rv 32:rh 64:1 128:c 256:h 512:t2 32767:inv")
    ("cstator" "cstator 1:b 2:t 4:fx 8:fy 16:rv 32:rh 64:1 128:c 256:h 512:t2 32767:inv")
    ("dc" "definecheat <cheat number> <text to activate cheat>")
    ("debris" "debris <tilenum> <amount>")
    ("debug" "debug <parameter>")
    ("define" "define <name> <number>")
    ("definecheat" "definecheat <cheat number> <text to activate cheat>")
    ("definegamefuncname" "definegamefuncname <function> <name>")
    ("definelevelname" "definelevelname <episode> <levelnum> <mapname> <partime> <3dr> <levname>")
    ("defineprojectile" "defineprojectile <tilenum> <function> <value>")
    ("definequote" "definequote <quote number> <quote text>")
    ("defineskillname" "defineskillname <skill> <name>")
    ("definesound" "definesound <value> <filename> <pitch1> <pitch2> <priority> <type> <volume>")
    ("definevolumename" "definevolumename <episode number> <name>")
    ("digitalnumber" ,(concat "digitalnumber <tilenum> <x> <y> <number> <shade> <pal> <orientation> <x1> <y1> <x2> <y2>"
                             dukecon-eldoc-orientation-string))
    ("divr" "divr <gamevar> <divisor>")
    ("divrd" "divrd <gamevar> <divisor>")
    ("divru" "divru <gamevar> <divisor>")
    ("divvar" "divvar <gamevar> <value>")
    ("divvarvar" "divvarvar <gamevar1> <gamevar2>")
    ("dragpoint" "dragpoint <wallnum> <x> <y>")
    ("drawline256" "drawline256 <x0> <y0> <x1> <y1> <index>")
    ("drawlinergb" "drawlinergb <x0> <y0> <x1> <y1> <index> <rgb>")
    ("echo" "echo <quote>")
    ("endofgame" "endofgame <number>")
    ("endoflevel" "endoflevel <number>\n(Pre-1.3D)")
    ("enhanced" "enhanced <value>")
    ("eqspawn" "eqspawn <actor>")
    ("eqspawnvar" "eqspawnvar <gamevar>")
    ("eshoot" "eshoot <projectilenumber>")
    ("eshootvar" "eshootvar <gamevar>")
    ("espawn" "espawn <tilenum>")
    ("espawnvar" "espawnvar <gamevar>")
    ("ezshoot" "ezshoot <zvel> <projectile>")
    ("ezshootvar" "ezshootvar <zvel> <gamevar>")
    ("findnearactor" "findnearactor <actor> <distance> <gamevar>")
    ("findnearactorvar" "findnearactorvar <actor> <distance> <gamevar>")
    ("findnearactor3d" "findnearactor3d <actor> <distance> <gamevar>")
    ("findnearactor3dvar" "findnearactor3dvar <actor> <distance> <gamevar>")
    ("findnearactorz" "findnearactorz <actor> <xydistance> <zdistance> <gamevar>")
    ("findnearactorzvar" "findnearactorzvar <actor> <xydistance> <zdistance> <gamevar>")
    ("findnearsprite" "findnearsprite <sprite> <distance> <gamevar>")
    ("findnearspritevar" "findnearspritevar <sprite> <distance> <gamevar>")
    ("findnearsprite3d" "findnearsprite3d <sprite> <distance> <gamevar>")
    ("findnearsprite3dvar" "findnearsprite3dvar <sprite> <distance> <gamevar>")
    ("findnearspritez" "findnearspritez <sprite> <xydistance> <zdistance> <gamevar>")
    ("findnearspritezvar" "findnearspritezvar <sprite> <xydistance> <zdistance> <gamevar>")
    ("gametext" ,(concat "gametext <tilenum> <x> <y> <quote> <shade> <pal> <orientation> <x1> <y1> <x2> <y2>"
                        dukecon-eldoc-orientation-string))
    ("gametextz" ,(concat "gametextz <tilenum> <x> <y> <quote> <shade> <pal> <orientation> <x1> <y1> <x2> <y2> <textscale>"
                        dukecon-eldoc-orientation-string))
    ("getangle" "getangle <<return>> <x> <y>")
    ("getangletotarget" "getangletotarget <<ang>>")
    ("getpname" "getpname <QUOTE #> <gamevar holding PLAYER ID>")
    ("getprojectile" "getprojectile[tilenum].parameter <gamevar>")
    ("getthisprojectile" "getthisprojectile[<sprite id>].parameter <gamevar>")
    ("getmusicposition" "getmusicposition <<pos>>")
    ("setmusicposition" "getmusicposition <pos>")
    ("globalsound" "globalsound <sound>")
    ("globalsoundvar" "globalsoundvar <gamevar>")
    ("guts" "guts <tilenum> <amount>")
    ("hitradius" "hitradius <radius> <1 (furthest)> <2> <3> <4 (closest)>")
    ("hitradiusvar" "hitradiusvar <radius> <1 (furthest)> <2> <3> <4 (closest)>")
    ("hitscan" "hitscan <x1> <y1> <z1> <sect1> <cos of ang> <sin of ang> <zvel> <hit sector return var> <hit wall return var> <hit sprite return var> <hit x return var> <hit y return var> <hit z return var> <clip mask>")
    ("ifaction" "ifaction <action> { do something } else { do something else }")
    ("ifactioncount" "ifactioncount <number> { do something } else { do something else }")
    ("ifactor" "ifactor <tilenum> { do something } else { do something else }")
    ("ifai" "ifai <ai> { do something } else { do something else }")
    ("ifangdiffl" "ifangdiffl <number> { do something } else { do something else }")
    ("ifceilingdistl" "ifceilingdistl <number>")
    ("ifclient" "ifclient { ... }")
    ("ifcount" "ifcount <number>")
    ("iffloordistl" "iffloordistl <number>")
    ("ifgapzl" "ifgapzl <number>")
    ("ifgotweaponce" "ifgotweaponce <number>")
    ("ifmove" "ifmove <move>")
    ("ifp" "ifp <condition>")
    ("ifpdistg" "ifpdistg <number>")
    ("ifpdistl" "ifpdistl <number>")
    ("ifphealthl" "ifphealthl <number> { do something } else { do something else }")
    ("ifpinventory" "ifpinventory <inventory item> <value>")
    ("ifplayersl" "ifplayersl <numplayers>\n(Pre-1.3D)")
    ("ifrnd" "ifrnd <value> { <do somethin!> }")
    ("ifserver" "ifserver { ... }")
    ("ifsound" "ifsound <sound> { code }")
    ("ifspawnedby" "ifspawnedby <actor> { do something } else { do something else }")
    ("ifspritepal" "ifspritepal <pal> { do something } else { do something else }")
    ("ifstrength" "ifstrength <number>")
    ("ifvarand" "ifvarand <gamevar> <number>")
    ("ifvare" "ifvare <gamevar> <number>")
    ("ifvareither" "ifvareither <gamevar> <number> ?")
    ("ifvaror" "ifvaror <gamevar> <number>")
    ("ifvarxor" "ifvarxor <gamevar> <number>")
    ("ifvarvareither" "ifvarvareither <gamevar1> <gamevar2>")
    ("ifvarvaror" "ifvarvaror <gamevar1> <gamevar2>")
    ("ifvarvarxor" "ifvarvarxor <gamevar1> <gamevar2>")
    ("ifvarg" "ifvarg <gamevar> <number>")
    ("ifvarl" "ifvarl <gamevar> <number>")
    ("ifvarn" "ifvarn <gamevar> <number>")
    ("ifvarvarand" "ifvarvarand <gamevar1> <gamevar2>")
    ("ifvarvare" "ifvarvare <gamevar1> <gamevar2>")
    ("ifvarvarg" "ifvarvarg <gamevar1> <gamevar2>")
    ("ifvarvarl" "ifvarvarl <gamevar1> <gamevar2>")
    ("ifvarvarn" "ifvarvarn <gamevar1> <gamevar2>")
    ("ifwasweapon" "ifwasweapon <weapon>")
    ("include" "include <filename>")
    ("includedefault" "includedefault")
    ("ldist" "ldist <variable to return distance value to> <actor 1> <actor 2>")
    ("loadmapstate" "loadmapstate")
    ("lockplayer" "lockplayer <gamevar>")
    ("lotsofglass" "lotsofglass <number>")
    ("mail" "mail <number>")
    ("minitext" "minitext <x> <y> <quote> <shade> <pal>")
    ("money" "money <number>")
    ("move" "move <name> [decl: <horizontal> <vertical>|call: <type>]")
;    ("move" "move <name> <type>")
    ("movesprite" "movesprite <ACTORID> <XVELS> <YVELS> <ZVELS> <CLIPMASK> <RETURNVAR>")
    ("mulvar" "mulvar <gamevar> <value>")
    ("mulvarvar" "mulvarvar <gamevar1> <gamevar2>")
    ("music" "music <num> <x1> <x2> <3> <4> <5> <6> <7> <8> <etc>")
    ("myos" ,(concat "myos <x> <y> <tilenum> <shade> <orientation>"
                    dukecon-eldoc-orientation-string))
    ("myospal" ,(concat "myospal <x> <y> <tilenum> <shade> <orientation> <pal>"
                       dukecon-eldoc-orientation-string))
    ("operateactivators" "operateactivators <lotag> <playerIdx>")
    ("operatesectors" "operatesectors <sector> <actor>")
    ("orvar" "orvar <gamevar> <value>")
    ("orvarvar" "orvarvar <gamevar1> <gamevar2>")
    ("palfrom" "palfrom <intensity> <red> <green> <blue>")
    ("paper" "paper <value>")
    ("precache" "precache <tile0> <tile1> <flag>")
    ("qputs" "redefinequote <quote number> <quote text>")
    ("qspawn" "qspawn <tilenum>")
    ("qspawnvar" "qspawnvar <gamevar>")
    ("qstrcat" "qstrcat <quote1> <quote2>")
    ("qstrcmp" "qstrcmp <quote1> <quote2> <var>")
    ("qstrcpy" "qstrcpy <quote1> <quote2>")
    ("qstrdim" "qstrdim <<width>> <<height>> <tilenum> <x> <y> <zoom> <block angle> <quote> <orientation> <xspace> <yline> <xbetween> <ybetween> <text flags> <x1> <y1> <x2> <y2>")
    ("quote" "quote <quote number>")
;    ("onevent" "onevent GETLOADTILE setvar RETURN <value> endevent")
    ("randvar" "randvar <gamevar> <value>")
    ("randvarvar" "randvarvar <gamevar1> <gamevar2>")
    ("readgamevar" "readgamevar <varname>")
    ("redefinequote" "redefinequote <quote number> <quote text>")
    ("rotatepoint" "rotatepoint <xpivot> <ypivot> <x> <y> <ang> <xreturnvar> <yreturnvar>")
    ("rotatesprite" ,(concat "rotatesprite <x> <y> <zoom> <a> <tilenum> <shade> <pal> <orientation> <x1> <x2> <y1> <y2>"
                            dukecon-eldoc-orientation-string))
    ("rotatesprite16" ,(concat "rotatesprite16 <x> <y> <zoom> <a> <tilenum> <shade> <pal> <orientation> <x1> <x2> <y1> <y2>"
                            dukecon-eldoc-orientation-string))
    ("rotatespritea" ,(concat "rotatespritea <x> <y> <zoom> <a> <tilenum> <shade> <pal> <orientation> <alpha> <x1> <x2> <y1> <y2>"
                            dukecon-eldoc-orientation-string))
    ("save" "save <slot number>")
    ("savenn" "savenn <slot number>")
    ("savegamevar" "savegamevar <varname>")
    ("savemapstate" "savemapstate")
    ("screenpal" "screenpal <r> <g> <b> <a>")
    ("screensound" "screensound <sound number>")
    ("screentext" "screentext <tilenum> <x> <y> <zoom> <block angle> <character angle> <quote> <shade> <pal> <orientation> <alpha> <xspace> <yline> <xbetween> <ybetween> <text flags> <x1> <y1> <x2> <y2>")
    ("setactorangle" "setactorangle <gamevar>")
    ("setplayerangle" "setplayerangle <gamevar>")
    ("setprojectile" "setprojectile[tilenum].parameter <gamevar>")
    ("setthisprojectile" "setthisprojectile[<sprite id>].parameter <gamevar>")
    ("setvar" "setvar <gamevar> <number>")
    ("setvarvar" "setvarvar <gamevar1> <gamevar2>")
    ("shadeto" "shadeto <shade>\n(Pre-1.3D)")
    ("shiftvarl" "shiftvarl <gamevar> <number>")
    ("shiftvarr" "shiftvarr <gamevar> <number>")
    ("shoot" "shoot <tilenum>")
    ("shootvar" "shootvar <gamevar>")
    ("showview" "showview <x> <y> <z> <angle> <horiz> <sector> <scrn_x1> <scrn_y1> <scrn_x2> <scrn_y2>")
    ("showviewunbiased" "showviewunbiased <x> <y> <z> <angle> <horiz> <sector> <scrn_x1> <scrn_y1> <scrn_x2> <scrn_y2>")
    ("sin" "sin <gamevar> <gamevar2>")
    ("sizeat" "sizeat <xrepeat> <yrepeat>")
    ("sizeto" "sizeto <xrepeat> <yrepeat>")
    ("sleeptime" "sleeptime <count>")
    ("sound" "sound <sound number>")
    ("soundonce" "soundonce <sound number>")
    ("soundoncevar" "soundoncevar <gamevar>")
    ("soundvar" "soundvar <gamevar>")
    ("spawn" "spawn <tilenum>")
    ("spritenoshade" "spritenoshade <tilenum>")
    ("spritenvg" "spritenvg <tilenum>")
    ("spritepal" "spritepal <number>")
    ("spriteshadow" "spriteshadow <tilenum>")
    ("startlevel" "startlevel <gamevar1> <gamevar2>")
    ("starttrack" "starttrack <track#>")
    ("starttrackslot" "starttrackslot <volume> <level>")
    ("starttrackvar" "starttrackvar <track#>")
;    ("state" "state <name> { state code } ends")
;    ("state" "state <name>")
    ("stopsound" "stopsound <sound number>")
    ("stopsoundvar" "stopsoundvar <gamevar>")
    ("strength" "strength <number>")
    ("subvar" "subvar <gamevar> <number>")
    ("subvarvar" "subvarvar <gamevar1> <gamevar2>")
    ("udc" "undefinecheat <cheat number>")
    ("updatesector" "updatesector <x> <y> <<gamevar>>")
    ("updatesectorz" "updatesectorz <x> <y> <z> <<gamevar>>")
    ("undefinecheat" "undefinecheat <cheat number>")
    ("undefinegamefunc" "undefinegamefunc <function number>")
    ("undefinelevel" "undefinelevel <volume> <level>")
    ("undefinevolume" "undefinevolume <volume>")
    ("undefineskill" "undefineskill <skill>")
    ("xorvar" "xorvar <gamevar> <number>")
    ("xorvarvar" "xorvarvar <gamevar1> <gamevar2>")
    ("zshoot" "zshoot <zvel> <projectile>")
    ("zshootvar" "zshootvar <zvel> <gamevar>")
    ))


(defun dukecon-mode ()
  "Major mode for editing Eduke .con files"
  (interactive)
  (kill-all-local-variables)
  (use-local-map dukecon-mode-map)
  (set-syntax-table dukecon-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(dukecon-font-lock-keywords))
  ;; Register our indentation function
  (set (make-local-variable 'indent-line-function) 'dukecon-indent-line)
  (set (make-local-variable 'paragraph-start) dukecon-paragraph-start)
  (set (make-local-variable 'paragraph-seperate) dukecon-paragraph-seperate)
  (set (make-local-variable 'eldoc-documentation-function) 'dukecon-eldoc-function)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'beginning-of-defun-function) 'dukecon-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'dukecon-end-of-defun)
  (set (make-local-variable 'outline-regexp) dukecon-outline-regexp)
  (set (make-local-variable 'imenu-generic-expression) dukecon-imenu-generic-expression)
  (setq major-mode 'dukecon-mode)
  (setq mode-name "Duke3D-CON")
  (dukecon-add-eldoc-strings)
  (run-hooks 'dukecon-mode-hook))

;; --------------------------------

(defun dukecon-insert-block (name start end &optional nospace) (interactive "M")
  (insert (concat start (if nospace "" " ") name "\n\n" end))
  (previous-line)
  (dukecon-indent-line))

(defun dukecon-insert-state (name) (interactive "MState name: ")
  (dukecon-insert-block name "state" "ends"))

(defun dukecon-insert-event (name) (interactive "MEvent name: ")
  (dukecon-insert-block name "onevent EVENT_" "endevent" t))

(defun dukecon-insert-actor (name) (interactive "MActor Name: ")
  (dukecon-insert-block name "actor" "enda"))


(defcustom dukecon-main-con-file-name "GAME.CON"
  "* File which will be searched first for \"includes\"."
  :group 'dukecon :type 'string)

(defcustom dukecon-working-directory
  "D:/Games/EDuke32/"
  "* Duke working directory, must end in a slash. Will be searched for other con files."
  :group 'dukecon :type 'string)

(defun dukecon-try-set-buffer-cases (name)
  "Try to return the buffer named NAME, trying different case."
   (condition-case nil (set-buffer name)
     (error (condition-case nil (set-buffer (downcase name))
              (error (condition-case nil (set-buffer (upcase name))
                       (error (setq buffer-was-there nil))))))))

(defun dukecon-find-definition ()
  "Try to find the definition of a \"define\" or \"gamevar\",
gathering con files to search from dukecon-main-con-file-name.
Don't recurse con files."
  (interactive)
  (let* (cons-to-examine-list
         (case-fold-search nil)    ;make search case-sensitive
         (sym-to-search (substring-no-properties (thing-at-point 'symbol)))
         (search-str
          (concat "\\<\\(define\\|gamevar\\) " sym-to-search "\\>.*$"))
         (include-con-regexp "\\<include[ \t]*\\(.*\.[Cc][Oo][Nn]\\>\\)"))
      (set-buffer (find-file-noselect dukecon-main-con-file-name t))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward include-con-regexp nil t)
          (let* ((conname (match-string-no-properties 1)))
            (add-to-list 'cons-to-examine-list conname t))));)
      (dolist (cur-con cons-to-examine-list)
;        (when (equal cur-con (car cons-to-examine-list))
;          (message (concat "\`" sym-to-search "\' not found!")))
        ;; first, see if the buffer in question is already open
        (let ((buffer-was-there (dukecon-try-set-buffer-cases cur-con)))
          (if (not buffer-was-there)
              (set-buffer (find-file-noselect
                           (concat dukecon-working-directory cur-con) t)))
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward search-str nil t)
              (message (concat cur-con
                               ":  "
                               (replace-regexp-in-string
                                "[ \t]+" " " (match-string-no-properties 0))))))
          (if (not buffer-was-there) (kill-this-buffer))
          ))))
;; --------------------------------

(provide 'dukecon-mode)

;;; dukecon-mode.el ends here
