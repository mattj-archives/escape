DECLARE SUB GameOver ()
DECLARE SUB ShutDown ()
DECLARE SUB SndCtrl ()
DECLARE SUB SaveGame ()
DECLARE SUB FO (tx!, ty!, t$)
DECLARE SUB KeyInpt (t$)
DECLARE SUB ShowCodes (t)
DECLARE SUB LoadStat ()
DECLARE SUB FFUpdate ()
DECLARE SUB BeatUB ()
DECLARE SUB FinishGame ()
DECLARE SUB FinishPL (t!)
DECLARE SUB EraseVars ()
DECLARE SUB PassLevel ()
DECLARE SUB UpdateQBeam (t!)
DECLARE SUB ShootQuad (t!)
DECLARE SUB UpdateQuad ()
DECLARE SUB DisLas2 (t!, t2)
DECLARE SUB DisLas (t!, c!, f)
DECLARE SUB HitChar (hd)
DECLARE SUB MsgPut (x$, flg!)
DECLARE SUB UpdateBeam (t!)
DECLARE SUB ShootLaser (t!)
DECLARE SUB LevelIntro ()
DECLARE SUB RunComp (t!)
DECLARE SUB compentr (tx, ty)
DECLARE SUB UpdateSprites ()
DECLARE SUB GotKey ()
DECLARE SUB MoveRt ()
DECLARE SUB MoveLt ()
DECLARE SUB MoveDn ()
DECLARE SUB DrawCharacter (dir!)
DECLARE SUB MoveUp ()
DECLARE FUNCTION ChkLaser (tx, ty, td, ln)
DECLARE SUB MainLoop ()
DECLARE SUB UpdateLaser ()
DECLARE SUB LGFX ()
DECLARE SUB SetItems ()
DECLARE SUB SetFreeItems ()
DECLARE SUB LevelSet (LevelNum)
'***********************************************************
'*              E S C A P E   1.0                         '*
'***********************************************************
'*****************************************************
DIM SHARED LUP%(45), LDN%(45), LLT%(45), LRT%(45)
DIM SHARED UP1%(33), UP2%(33), DN1%(33), DN2%(33)
DIM SHARED LT1%(33), LT2%(33), RT1%(33), RT2%(33)
DIM SHARED LIFE%(33), MC%(9), CBI1%(17), QUAD%(14), HEART%(14)
DIM SHARED CB%(19), LH%(4), LV%(4), PASS%(19), DIE%(45)
'*****************************************************
RANDOMIZE TIMER
TYPE L
 x AS INTEGER
 y AS INTEGER
 onflag AS INTEGER
 shot AS INTEGER
 dir AS INTEGER
 X1 AS INTEGER
 X2 AS INTEGER
 Y1 AS INTEGER
 Y2 AS INTEGER
 typ AS INTEGER
 shotx AS INTEGER
 shoty AS INTEGER
 shotdir AS INTEGER
END TYPE

TYPE mcomp
 x AS INTEGER
 y AS INTEGER
 numreq AS INTEGER
 totobj AS INTEGER
 limx AS INTEGER
 limy AS INTEGER
 typ AS INTEGER
END TYPE

TYPE f
X1 AS INTEGER
X2 AS INTEGER
Y1 AS INTEGER
Y2 AS INTEGER
onflag AS INTEGER
END TYPE

TYPE qd
x AS INTEGER
y AS INTEGER
stp AS INTEGER
onflag AS INTEGER
dir AS INTEGER
totstps AS INTEGER
END TYPE

TYPE o
typ AS INTEGER
num AS INTEGER
END TYPE

DIM SHARED Keys(6), KeyTyp(6)
DIM SHARED KeyX(6), KeyY(6)
DIM SHARED FF(20) AS f
DIM SHARED obj AS o, FFN, FFNum(20)       'type*num
DIM SHARED LASER(20) AS L, NumOfFF(10), NumOfL(10), NumOfQd(10)
DIM SHARED FFCH(10, 5), LCH(10, 8), QCH(10, 5)
DIM SHARED minicomp(10) AS mcomp
DIM SHARED Lasers, tempmcomp
DIM SHARED LaserTyp(20), LaserNum(20)
DIM SHARED dir, frame, framecount: frame = 0: framecount = 0
DIM SHARED x, y, Level
DIM SHARED lft, ltime: lft = .05
DIM SHARED health, ht: health = 100
DIM SHARED msg, msgt
DIM SHARED StartX, StartY, StartDir
DIM SHARED q(4) AS qd
DIM SHARED qx(4, 15), qy(4, 15), qs(4, 4), qsx(4, 4), qsy(4, 4), qdl
DIM SHARED mf, s1, lf, ffu, code$, sfx, mainpause: mf = 0: sfx = 1
DIM SHARED ilk$(6), Lives: Lives = 3
'qs=shot flag
MainStart:
'************************Others
'Warp code: /LWCDE10E1
IF LEFT$(COMMAND$, 10) <> "/LWCDE10E1" THEN
 SCREEN 0, 0, 0: PRINT "Plese run ESCAPE.BAT"
SYSTEM
END IF
'Level = 30
mainpause = 70
'***********************************************************
SCREEN 13
LGFX
LoadStat
SetItems
LevelIntro
'Level = 1
LevelSet Level
MainLoop

SUB BeatUB
CLS
OPEN "game.dat" FOR INPUT AS #1: INPUT #1, temp$: CLOSE
OPEN "game.dat" FOR OUTPUT AS #1: MID$(temp$, 4, 1) = "1": PRINT #1, temp$: CLOSE : CLOSE
IF temp$ = "1111" THEN
COLOR 3
PRINT "Wow... You beat the Almost unbeatable"
PRINT "level... For a useful cheat, type: "
PRINT : PRINT CHR$(66); : PRINT CHR$(83);
PRINT CHR$(89); : PRINT CHR$(49);
PRINT CHR$(52); : PRINT CHR$(49): PRINT
PRINT "During the game."
WHILE INKEY$ <> "": WEND
WHILE INKEY$ = "": WEND
ShutDown
ELSE
COLOR 3
PRINT "To get the cheat, complete the"
PRINT "Practice levels."
WHILE INKEY$ <> "": WEND
WHILE INKEY$ = "": WEND
ShutDown
END IF
'66,83,89,49,52,49
END SUB

SUB compentr (tx, ty)
IF dir = 1 THEN ty = ty - 1
IF dir = 2 THEN ty = ty + 1
IF dir = 3 THEN tx = tx - 1
IF dir = 4 THEN tx = tx + 1
FOR z = 1 TO tempmcomp
IF tx >= minicomp(z).x AND tx <= minicomp(z).x + minicomp(z).limx THEN
IF ty >= minicomp(z).y AND ty <= minicomp(z).y + minicomp(z).limy THEN
 GOTO gk2
 END IF
END IF
NEXT
EXIT SUB

gk2:
LINE (0, 0)-(320, 9), 0, BF
LOCATE 1, 1: COLOR 9: PRINT "Accessing..."
IF sfx = 1 THEN FOR z3 = 1 TO 20: SOUND INT(RND * 500) + 137, 1: NEXT
IF Keys(minicomp(z).numreq) = 0 THEN GOTO nokey
MsgPut "Access granted!", 1
IF sfx = 1 THEN SOUND 1000, 4
t = TIMER: WHILE TIMER - t < 1: WEND
RunComp z
GOTO es
nokey:
MsgPut "You are missing CC" + STR$(minicomp(z).numreq), 1
IF sfx = 1 THEN SOUND 50, 4
es:
WHILE INKEY$ <> "": WEND
END SUB

SUB DisLas (t, c, f)
SELECT CASE f
CASE 1
tx = LASER(t).x
ty = LASER(t).y
SELECT CASE LASER(t).dir
CASE 1 TO 2
LINE (tx, ty)-(tx + 7, ty + 10), c, B
CASE 3 TO 4
LINE (tx, ty)-(tx + 11, ty + 7), c, B
END SELECT
CASE 2
tx = q(t).x
ty = q(t).y
LINE (tx, ty)-(tx + 5, ty + 5), c, B
END SELECT

END SUB

SUB DisLas2 (t, t2)

FOR z = 1 TO Lasers
IF LASER(z).onflag = 0 THEN
DisLas z, 28, 1
IF sfx = 1 THEN FOR zz = 600 TO 300 STEP -50: SOUND zz, .5: SOUND zz + 100, .5: NEXT
END IF
NEXT

FOR z = 1 TO qdl
IF q(z).onflag = 0 THEN
DisLas z, 28, 2
IF sfx = 1 THEN FOR zz = 100 TO 500 STEP 50: SOUND zz, .1: SOUND 1100 - zz, .1: NEXT
END IF
NEXT

END SUB

SUB DrawCharacter (dir)
framecount = framecount + 1
IF framecount >= 20 THEN
 IF frame = 1 THEN frame = 0 ELSE frame = 1
 framecount = 0
END IF

SELECT CASE dir
CASE 1
IF frame = 0 THEN PUT (x, y), UP1%, PSET ELSE PUT (x, y), UP2%, PSET
CASE 2
IF frame = 0 THEN PUT (x, y), DN1%, PSET ELSE PUT (x, y), DN2%, PSET
CASE 3
IF frame = 0 THEN PUT (x, y), LT1%, PSET ELSE PUT (x, y), LT2%, PSET
CASE 4
IF frame = 0 THEN PUT (x, y), RT1%, PSET ELSE PUT (x, y), RT2%, PSET
END SELECT
END SUB

SUB EraseVars
ERASE Keys, KeyTyp, KeyX, KeyY, FF, FFNum, LASER, NumOfFF, NumOfL, NumOfQd
ERASE FFCH, LCH, QCH, minicomp, LaserTyp, LaserNum, q, qx, qy, qs, qsx, qsy
FFN = 0: Lasers = 0: tempmcomp = 0: health = 100: qdl = 0

END SUB

SUB FFUpdate
FOR z = 1 TO FFN
 IF FF(z).onflag = 1 THEN LINE (FF(z).X1, FF(z).Y1)-(FF(z).X2, FF(z).Y2), 50
NEXT
ffu = TIMER
END SUB

SUB FinishGame
CLS
FO 10, 1, "Well done, you beat this game..."
FO 11, 1, "The Unbeatable Level is now"
FO 12, 1, "accessable..."
OPEN "game.dat" FOR INPUT AS #1
INPUT #1, temp$
CLOSE
MID$(temp$, 4, 1) = "1"
OPEN "game.dat" FOR OUTPUT AS #1
PRINT #1, temp$
CLOSE



END SUB

SUB FinishPL (t)
CLS
OPEN "game.dat" FOR INPUT AS #1: INPUT #1, temp$: CLOSE
SELECT CASE t
CASE 1
FO 1, 1, "Practice level 2 is available"
MID$(temp$, 1, 1) = "1"
MID$(temp$, 2, 1) = "1"
CASE 2
FO 1, 1, "Practice level 3 is available"
MID$(temp$, 3, 1) = "1"
CASE 3
FO 1, 1, "All practice levels completed"
END SELECT
FO 2, 1, "Press a key..."
WHILE INKEY$ <> "": WEND
WHILE INKEY$ = "": WEND
OPEN "game.dat" FOR OUTPUT AS #1: PRINT #1, temp$: CLOSE
SCREEN 0, 0, 0: WIDTH 80: COLOR 7, 0: ShutDown
ShutDown
END SUB

SUB FO (tx, ty, t$)
FOR z = 16 TO 31
        LOCATE tx, ty: COLOR z: PRINT t$
        t = TIMER: WHILE TIMER - t < .001: WEND
NEXT

END SUB

SUB GameOver
FOR z = 1 TO 300
CIRCLE (160, 100), z, 4
FOR p = 1 TO mainpause * 2: NEXT
NEXT
FOR z = 41 TO 1 STEP -1
PALETTE 4, 65536 * 0 + 256 * 0 + z
FOR p = 1 TO mainpause * 10: NEXT
NEXT
CLS
COLOR 4: LOCATE 10, 14: PRINT "Game Over!!!"
FOR z = 1 TO 41
PALETTE 4, 65536 * 0 + 256 * 0 + z
FOR p = 1 TO mainpause * 10: NEXT
NEXT
t = TIMER: WHILE TIMER - t < 2: WEND
FOR z = 41 TO 1 STEP -1
PALETTE 4, 65536 * 0 + 256 * 0 + z
FOR p = 1 TO mainpause * 10: NEXT
NEXT
CLS
ShutDown
END SUB

SUB GotKey
FOR z = 1 TO 6
IF x > KeyX(z) AND x < KeyX(z) + 6 THEN
 IF y > KeyY(z) AND y < KeyY(z) + 6 THEN
  IF sfx = 1 THEN FOR z2 = 500 TO 4000 STEP 500: SOUND z2, .1: SOUND z2 + 500, .1: NEXT
  GOTO gk
 END IF
END IF
NEXT
EXIT SUB
gk:
LINE (KeyX(z), KeyY(z))-(KeyX(z) + 6, KeyY(z) + 6), 0, BF
Keys(z) = 1
KeyX(z) = 0: KeyY(z) = 0
IF mf = 0 THEN
FOR z = 2 TO 12 STEP 2
LOCATE 1, z / 2: IF Keys(z / 2) = 1 THEN COLOR 2: PRINT LTRIM$(RTRIM$(STR$(z / 2)))
NEXT
END IF
DrawCharacter dir
END SUB

SUB HitChar (hd)
'PLAY "mb o1 l64t255 geg-e-fded-e-cdd-ct120"

IF sfx = 1 THEN SOUND 500, .5
health = health - hd
IF health < 0 THEN GOTO DIE2
LINE (100, 0)-(200, 8), 0, BF
LINE (100, 0)-(200, 8), 4, B
LINE (100, 0)-(100 + health, 8), 4, BF
EXIT SUB

DIE2:
FOR z = 1 TO 5
CIRCLE (x + 3, y + 3), z, 4
IF sfx = 1 THEN SOUND 0, 2
NEXT

FOR z = 1 TO 5
CIRCLE (x + 3, y + 3), z, 0
IF sfx = 1 THEN SOUND 0, 2
NEXT

LINE (x, y)-(x + 7, y + 7), 0, BF
PUT (x, y), DIE%
t = TIMER: WHILE TIMER - t < 2: WEND
LINE (x, y)-(x + 7, y + 7), 0, BF
x = StartX: y = StartY: dir = StartDir
Lives = Lives - 1: IF Lives = -1 THEN GameOver
LINE (0, 0)-(320, 9), 0, BF
LOCATE 1, 1: PRINT "Lives:", Lives
WHILE INKEY$ <> "": WEND
WHILE INKEY$ = "": WEND
IF dir = 1 THEN PUT (x, y), UP1%
IF dir = 2 THEN PUT (x, y), DN1%
IF dir = 3 THEN PUT (x, y), LT1%
IF dir = 4 THEN PUT (x, y), RT1%
LINE (0, 0)-(200, 8), 0, BF
health = 100
WHILE INKEY$ <> "": WEND
END SUB

SUB KeyInpt (t$)
IF ilk$(6) <> "" OR ilk$(6) = "" AND ilk$(5) = "" THEN
 FOR z = 1 TO 5: ilk$(z) = ilk$(z + 1): NEXT
ilk$(6) = t$
GOTO EIF1
ELSE
FOR z = 1 TO 5
 IF ilk$(z) = "" THEN ilk$(z) = t$: GOTO EIF1
NEXT
EIF1:
END IF
k$ = ""
FOR z = 1 TO 6
 k$ = k$ + ilk$(z)
NEXT
IF k$ = "BSY141" THEN ShowCodes 1
END SUB

SUB LevelIntro
code$ = ""
IF Level < 32 THEN
 FOR z = 1 TO 4
 code$ = code$ + CHR$(INT(RND * 25) + 65)
NEXT
END IF
roi:
tlevel = Level
IF Level = 32 THEN tlevel = 1
IF Level = 33 THEN tlevel = 2
IF Level = 34 THEN tlevel = 3
IF Level > 31 THEN X2$ = "Practice level " ELSE X2$ = "Level "
IF tlevel <> 31 THEN X2$ = X2$ + LTRIM$(RTRIM$(STR$(tlevel))) ELSE X2$ = "The Unbeatable Level"

col = (40 - LEN(X2$)) / 2
col2 = (40 - LEN("Level code:" + code$)) / 2
CLOSE
CLS
'                  b         g   r
PALETTE 1, 65536 * 0 + 256 * 0 + 0
PALETTE 4, 65536 * 0 + 256 * 0 + 0
COLOR 4: LOCATE 10, col: PRINT X2$
COLOR 1: LOCATE 15, col2: PRINT "Level code:" + code$
COLOR 4: LOCATE 12, 5: PRINT "Lives remaining:"; Lives
FOR z = 1 TO 41
PALETTE 1, 65536 * z + 256 * 0 + 0
PALETTE 4, 65536 * 0 + 256 * 0 + z
FOR p = 1 TO mainpause * 10: NEXT
NEXT
LOCATE 24, 1: COLOR 5: PRINT "Press a key...";
WHILE INKEY$ <> "": WEND
WHILE INKEY$ = "": WEND
FOR z = 41 TO 1 STEP -1
PALETTE 1, 65536 * z + 256 * 0 + 0
PALETTE 4, 65536 * 0 + 256 * 0 + z
FOR p = 1 TO mainpause * 10: NEXT
NEXT
CLS
PALETTE 1, 65536 * 41 + 256 * 0 + 0
PALETTE 4, 65536 * 0 + 256 * 0 + 41

END SUB

SUB LevelSet (LevelNum)
LINE (0, 10)-(320, 10), 4
FFN = 0
keyt = 0
lnum = 0
Lasers = 0
tempmcomp = 0
cobjnum = 1
qdl = 0
x$ = "levels\level" + LTRIM$(RTRIM$(STR$(LevelNum)))
OPEN x$ FOR INPUT AS #1
rt:
IF EOF(1) THEN GOTO CloseLevelFile
INPUT #1, TempObjType, objnum
IF objnum <> 0 THEN cobjnum = cobjnum + 1
GOTO wrobj

CloseLevelFile:
CLOSE #1
DisLas2 Lasers, qdl
EXIT SUB
wrobj:
SELECT CASE TempObjType

CASE 0
INPUT #1, tx1, ty1, tx2, ty2
IF tx1 = 0 AND ty1 = 0 AND tx2 = 0 AND ty2 = 0 THEN GOTO CloseLevelFile
LINE (tx1, ty1)-(tx2, ty2), 32

CASE 1 TO 4
 lnum = lnum + 1
 LaserTyp(lnum) = TempObjType
 LaserNum(lnum) = cobjnum
 t = lnum
 INPUT #1, tx1, ty1, tx2, ty2, tof, tdir
 IF tdir = 1 THEN LASER(t).x = LASER(t).x - 1
 IF tdir = 2 THEN LASER(t).x = LASER(t).x + 1
 IF tdir = 3 THEN LASER(t).y = LASER(t).y - 1
 IF tdir = 4 THEN LASER(t).y = LASER(t).y + 1
LASER(t).x = tx1: LASER(t).y = ty1
LASER(t).X1 = tx1: LASER(t).X2 = tx2: LASER(t).Y1 = ty1: LASER(t).Y2 = ty2
LASER(t).dir = tdir: LASER(t).shot = 0: LASER(t).onflag = tof
LASER(t).typ = TempObjType
IF TempObjType = 1 THEN PUT (tx1, ty1), LUP%
IF TempObjType = 2 THEN PUT (tx1, ty1), LDN%
IF TempObjType = 3 THEN PUT (tx1, ty1), LLT%
IF TempObjType = 4 THEN PUT (tx1, ty1), LRT%
Lasers = Lasers + 1

CASE 5
 keyt = keyt + 1
 INPUT #1, tx, ty, tnum
 KeyX(keyt) = tx: KeyY(keyt) = ty
 KeyTyp(keyt) = tnum
 PUT (tx, ty), CB%

CASE 7
 FFN = FFN + 1
 INPUT #1, tx1, ty1, tx2, ty2, FFNum(FFN), FFT
 IF FFT = 1 THEN LINE (tx1, ty1)-(tx2, ty2), 50
 FF(FFN).X1 = tx1: FF(FFN).X2 = tx2: FF(FFN).Y1 = ty1: FF(FFN).Y2 = ty2
 FF(FFN).onflag = FFT

CASE 10
'*******
INPUT #1, qdl
INPUT #1, tmpstps
FOR z = 1 TO tmpstps
 INPUT #1, qx(qdl, z), qy(qdl, z)
NEXT
INPUT #1, q(qdl).onflag
INPUT #1, q(qdl).dir
q(qdl).x = qx(qdl, 1): q(qdl).y = qy(qdl, 1)
q(qdl).totstps = tmpstps
q(qdl).stp = 2
PUT (q(qdl).x, q(qdl).y), QUAD%

CASE 12
INPUT #1, tx, ty
 PUT (tx, ty), PASS%

CASE 6, 13
INPUT #1, nr
tempmcomp = tempmcomp + 1
INPUT #1, tx, ty
INPUT #1, NumOfFF(tempmcomp)
IF NumOfFF(tempmcomp) = 0 THEN INPUT #1, dummy
FOR z = 1 TO NumOfFF(tempmcomp)
IF NumOfFF(tempmcomp) >= z THEN INPUT #1, FFCH(tempmcomp, z)
NEXT
INPUT #1, NumOfL(tempmcomp)
IF NumOfL(tempmcomp) = 0 THEN INPUT #1, dummy
FOR z = 1 TO NumOfL(tempmcomp)
IF NumOfL(tempmcomp) >= z THEN INPUT #1, LCH(tempmcomp, z)
NEXT
INPUT #1, NumOfQd(tempmcomp)
IF NumOfQd(tempmcomp) = 0 THEN INPUT #1, dummy
FOR z = 1 TO NumOfQd(tempmcomp)
IF NumOfQd(tempmcomp) >= z THEN INPUT #1, QCH(tempmcomp, z)
NEXT

IF TempObjType = 6 THEN lx = 4: ly = 8
IF TempObjType = 13 THEN lx = 4: ly = 4
minicomp(tempmcomp).typ = TempObjType
minicomp(tempmcomp).limx = lx: minicomp(tempmcomp).limy = ly
minicomp(tempmcomp).x = tx: minicomp(tempmcomp).y = ty
minicomp(tempmcomp).numreq = nr: minicomp(tempmcomp).totobj = tobj
IF TempObjType = 6 THEN PUT (tx, ty), CBI1%
IF TempObjType = 13 THEN PUT (tx, ty), MC%
CASE 20

INPUT #1, StartX, StartY, StartDir
x = StartX: y = StartY: dir = StartDir
IF dir = 1 THEN PUT (x, y), UP1%
IF dir = 2 THEN PUT (x, y), DN1%
IF dir = 3 THEN PUT (x, y), LT1%
IF dir = 4 THEN PUT (x, y), RT1%

END SELECT
GOTO rt
END SUB

SUB LGFX
a = 1

OPEN "GFX\GFXLIST.LST" FOR INPUT AS #1
DO UNTIL a = 23
        INPUT #1, GFXFILE$, tx, ty
                IF GFXFILE$ = "" THEN GOTO SL
                MGFXFILE$ = "GFX\" + GFXFILE$ + ".GFX"
        OPEN MGFXFILE$ FOR INPUT AS #2
DO UNTIL (EOF(2))
CG:
INPUT #2, x, y, c
PSET (x, y), c
LOOP
2
11
IF GFXFILE$ = "LUP" THEN GET (0, 0)-(tx, ty), LUP%
IF GFXFILE$ = "LDN" THEN GET (0, 0)-(tx, ty), LDN%
IF GFXFILE$ = "LLT" THEN GET (0, 0)-(tx, ty), LLT%
IF GFXFILE$ = "LRT" THEN GET (0, 0)-(tx, ty), LRT%
IF GFXFILE$ = "UP1" THEN GET (0, 0)-(tx, ty), UP1%
IF GFXFILE$ = "DN1" THEN GET (0, 0)-(tx, ty), DN1%
IF GFXFILE$ = "LT1" THEN GET (0, 0)-(tx, ty), LT1%
IF GFXFILE$ = "RT1" THEN GET (0, 0)-(tx, ty), RT1%
IF GFXFILE$ = "UP2" THEN GET (0, 0)-(tx, ty), UP2%
IF GFXFILE$ = "DN2" THEN GET (0, 0)-(tx, ty), DN2%
IF GFXFILE$ = "LT2" THEN GET (0, 0)-(tx, ty), LT2%
IF GFXFILE$ = "RT2" THEN GET (0, 0)-(tx, ty), RT2%
IF GFXFILE$ = "LIFE" THEN GET (0, 0)-(tx, ty), LIFE%
IF GFXFILE$ = "MC" THEN GET (0, 0)-(tx, ty), MC%
IF GFXFILE$ = "CBI1" THEN GET (0, 0)-(tx, ty), CBI1%
IF GFXFILE$ = "QUAD" THEN GET (0, 0)-(tx, ty), QUAD%
IF GFXFILE$ = "HEART" THEN GET (0, 0)-(tx, ty), HEART%
IF GFXFILE$ = "CB" THEN GET (0, 0)-(tx, ty), CB%
IF GFXFILE$ = "LASER" THEN GET (0, 0)-(tx, ty), LH%
IF GFXFILE$ = "LV" THEN GET (0, 0)-(tx, ty), LV%
IF GFXFILE$ = "PASS" THEN GET (0, 0)-(tx, ty), PASS%
IF GFXFILE$ = "DIE" THEN GET (0, 0)-(tx, ty), DIE%

LINE (0, 0)-(25, 25), 0, BF
CLOSE #2
a = a + 1
LOOP
SL:
CLOSE #1
CLS

END SUB

SUB LoadStat
OPEN "LW.CHK" FOR INPUT AS #1
 INPUT #1, Level
CLOSE
IF Level = -1 THEN GOTO NextFile
GOTO ispeed

NextFile:
OPEN "LG.FLG" FOR INPUT AS #1
 INPUT #1, LoadNum
CLOSE
OPEN "gfx\sm.gfx" FOR INPUT AS #1
FOR z = 0 TO LoadNum
INPUT #1, null, n$, Level, health
NEXT
CLOSE

ispeed:
OPEN "speed.dat" FOR INPUT AS #1: INPUT #1, mainpause: CLOSE
END SUB

SUB MainLoop
ht = TIMER
ml:
a$ = INKEY$
IF INP(96) = 72 THEN MoveUp
IF INP(96) = 75 THEN MoveLt
IF INP(96) = 77 THEN MoveRt
IF INP(96) = 80 THEN MoveDn
IF UCASE$(a$) = "Q" THEN ShutDown
IF a$ = "+" THEN ShowCodes 2
IF a$ > "0" AND a$ < "Z" THEN KeyInpt a$
IF a$ = CHR$(0) + "<" THEN SaveGame
IF a$ = "," THEN mainpause = mainpause - 10: MsgPut "Pauseloop" + STR$(mainpause) + "*100", 1
IF a$ = "." THEN mainpause = mainpause + 10: MsgPut "Pauseloop" + STR$(mainpause) + "*100", 1
IF UCASE$(a$) = "S" THEN SndCtrl
UpdateLaser
IF qdl > 0 THEN UpdateQuad
UpdateSprites
FOR z = 1 TO mainpause * 100: NEXT
IF msg = 1 THEN IF TIMER - msgt >= 2 THEN MsgPut "", 0
IF msg = 0 AND health < 100 AND TIMER - ht >= 1 THEN
 health = health + 1
 COLOR 3: LOCATE 1, 1: PRINT "Health: "; LTRIM$(RTRIM$(STR$(health))); "%"
 LINE (100, 0)-(200, 8), 4, B
 LINE (100, 0)-(100 + health, 8), 4, BF
 ht = TIMER
 IF health = 100 THEN LINE (0, 0)-(320, 9), 0, BF
 mf = 1
END IF
GOTO ml
END SUB

SUB MoveDn
IF y + 9 >= 200 THEN EXIT SUB

FOR z = x TO x + 7
IF POINT(z, y + 8) = 32 THEN GOTO cancel2
IF POINT(z, y + 8) = 28 THEN GOTO cancel2
IF POINT(z, y + 8) = 33 THEN PassLevel: GOTO cancel2
IF POINT(z, y + 8) = 5 THEN dir = 2: compentr z, y + 8: EXIT SUB
IF POINT(z, y + 8) = 50 THEN HitChar 30: GOTO cancel2
NEXT
GotKey

LINE (x, y)-(x + 7, y + 7), 0, BF
y = y + 1

cancel2:
dir = 2
DrawCharacter 2

END SUB

SUB MoveLt
IF x - 1 = -1 THEN EXIT SUB

FOR z = y TO y + 7
IF POINT(x - 1, z) = 32 THEN GOTO cancel3
IF POINT(x - 1, z) = 28 THEN GOTO cancel3
IF POINT(x - 1, z) = 33 THEN PassLevel: GOTO cancel3
IF POINT(x - 1, z) = 5 THEN dir = 3: compentr x - 1, z: EXIT SUB
IF POINT(x - 1, z) = 50 THEN HitChar 30: GOTO cancel3
NEXT
GotKey

LINE (x, y)-(x + 7, y + 7), 0, BF
x = x - 1

cancel3:
dir = 3
DrawCharacter 3

END SUB

SUB MoveRt
IF x + 9 = 321 THEN EXIT SUB

FOR z = y TO y + 7
IF POINT(x + 8, z) = 32 THEN GOTO cancel4
IF POINT(x + 8, z) = 28 THEN GOTO cancel4
IF POINT(x + 8, z) = 33 THEN PassLevel: GOTO cancel4
IF POINT(x + 8, z) = 5 THEN dir = 4: compentr x + 8, z: EXIT SUB
IF POINT(x + 8, z) = 50 THEN HitChar 30: GOTO cancel4
NEXT
GotKey

'IF POINT(x + 9, y) = 32 THEN GOTO cancel4
LINE (x, y)-(x + 7, y + 7), 0, BF
x = x + 1

cancel4:
dir = 4
DrawCharacter 4

END SUB

SUB MoveUp
IF y - 1 = 10 THEN EXIT SUB

FOR z = x TO x + 7
IF POINT(z, y - 1) = 32 THEN GOTO cancel1
IF POINT(z, y - 1) = 28 THEN GOTO cancel1
IF POINT(z, y - 1) = 33 THEN PassLevel: GOTO cancel1
IF POINT(z, y - 1) = 5 THEN dir = 1: compentr z, y - 1: EXIT SUB
IF POINT(z, y - 1) = 50 THEN HitChar 30: GOTO cancel1
NEXT
GotKey

LINE (x, y)-(x + 7, y + 7), 0, BF
y = y - 1

cancel1:
dir = 1
DrawCharacter 1

END SUB

SUB MsgPut (x$, flg)
SELECT CASE flg
CASE 0
msg = 0
LINE (0, 0)-(320, 9), 0, BF

CASE 1
LINE (0, 0)-(320, 9), 0, BF
LOCATE 1, 1: COLOR 3: PRINT x$
msgt = TIMER
msg = 1
mf = 1
END SELECT

END SUB

SUB PassLevel
LINE (0, 0)-(320, 9), 0, BF
IF sfx = 1 THEN SOUND 250, 1: SOUND 700, 1: SOUND 300, 1: SOUND 100, 1: SOUND 500, 1
WHILE INKEY$ <> "": WEND
COLOR 3: LOCATE 1, 1: INPUT "Code?", t$
IF t$ <> code$ THEN
 LINE (0, 0)-(320, 9), 0, BF
 COLOR 4: LOCATE 1, 1: PRINT "Incorrect!"
 IF sfx = 1 THEN FOR z = 1000 TO 37 STEP -100: SOUND z, .5: NEXT
 FOR z = 1 TO (health / 10) + 1: HitChar 10: NEXT
 EraseVars
 LevelIntro
 LevelSet Level
END IF
IF t$ <> code$ THEN EXIT SUB
LINE (0, 0)-(320, 9), 0, BF
COLOR 10: LOCATE 1, 1: PRINT "Correct!"
IF sfx = 1 THEN SOUND 0, 5
IF sfx = 1 THEN
 FOR z = 100 TO 4000 STEP 500: SOUND z, .5: NEXT
 FOR z = 37 TO 400 STEP 5: SOUND z, .05: NEXT
 FOR z = 200 TO 600 STEP 5: SOUND z, .05: NEXT
 FOR z = 400 TO 800 STEP 5: SOUND z, .05: NEXT
 FOR z = 600 TO 1000 STEP 5: SOUND z, .05: NEXT
END IF
X1 = 0: X2 = 320
Y1 = 0: Y2 = 200
WHILE X1 <> 160
LINE (X1, Y1)-(X2, Y2), 0, B
X1 = X1 + 1
X2 = X2 - 1
Y1 = Y1 + .625
Y2 = Y2 - .625
LINE (X1, Y1)-(X2, Y2), 3, B
FOR z = 1 TO 600: NEXT
WEND
LINE (X1, Y1)-(X2, Y2), 0, B
EraseVars
Lives = Lives + 1
Level = Level + 1
IF Level = 31 THEN FinishGame
IF Level = 32 THEN BeatUB
IF Level = 33 THEN FinishPL 1
IF Level = 34 THEN FinishPL 2
IF Level = 35 THEN FinishPL 3
LevelIntro
LevelSet Level

END SUB

SUB RunComp (t)

FOR z2 = 1 TO NumOfFF(t)
IF FFCH(t, z2) THEN pp = 7: GOTO dobj
do2:
NEXT

FOR z2 = 1 TO NumOfL(t)
IF LCH(t, z2) THEN pp = 1: GOTO dobj
do3:
NEXT

FOR z2 = 1 TO NumOfQd(t)
IF QCH(t, z2) THEN pp = 10: GOTO dobj
do4:
NEXT

EXIT SUB

dobj:
SELECT CASE pp

CASE 1 TO 4
z = LCH(t, z2)

IF LASER(z).onflag = 0 THEN LASER(z).onflag = 1 ELSE LASER(z).onflag = 0

IF LASER(z).onflag = 1 THEN
 DisLas z, 0, 1
 IF sfx = 1 THEN FOR zz = 300 TO 600 STEP 50: SOUND zz, .5: SOUND zz + 100, .5: NEXT
END IF

IF LASER(z).onflag = 0 THEN
DisLas z, 28, 1
IF sfx = 1 THEN FOR zz = 600 TO 300 STEP -50: SOUND zz, .5: SOUND zz + 100, .5: NEXT
END IF

CASE 7
z = FFCH(t, z2)
tempx1 = FF(z).X1
tempx2 = FF(z).X2
tempy1 = FF(z).Y1
tempy2 = FF(z).Y2
IF FF(z).onflag = 0 THEN FF(z).onflag = 1 ELSE FF(z).onflag = 0

IF FF(z).onflag = 0 THEN
LINE (tempx1, tempy1)-(tempx2, tempy2), 0
IF sfx = 1 THEN FOR zz = 800 TO 37 STEP -5: SOUND zz, .05: SOUND zz + 145, .05: SOUND zz + 80, .05: NEXT
END IF


IF FF(z).onflag = 1 THEN
LINE (tempx1, tempy1)-(tempx2, tempy2), 50
IF sfx = 1 THEN FOR zz = 37 TO 800 STEP 5: SOUND zz, .05: SOUND zz + 145, .05: SOUND zz + 80, .05: NEXT
END IF

CASE 10
z = QCH(t, z2)
IF q(z).onflag = 0 THEN q(z).onflag = 1 ELSE q(z).onflag = 0

IF q(z).onflag = 1 THEN
 DisLas z, 0, 2
 IF sfx = 1 THEN FOR zz = 500 TO 100 STEP -50: SOUND zz, .1: SOUND 1100 - zz, .1: NEXT
END IF

IF q(z).onflag = 0 THEN
 DisLas z, 28, 2
 IF sfx = 1 THEN FOR zz = 100 TO 500 STEP 50: SOUND zz, .1: SOUND 1100 - zz, .1: NEXT
END IF

END SELECT
IF pp = 1 THEN GOTO do3
IF pp = 7 THEN GOTO do2
IF pp = 10 THEN GOTO do4
END SUB

SUB SaveGame
LINE (0, 0)-(320, 9), 0, BF
ON ERR GOTO 1000
1000 IF sfx = 1 THEN SOUND 800, 1
COLOR 3
LOCATE 1, 1: INPUT "Save number?", sn$
LOCATE 1, 1: PRINT "                "
1002 LOCATE 1, 1: LINE INPUT "Name?", n$
IF n$ = "*Nothing" THEN BEEP: LINE (0, 0)-(320, 9), 0, BF: GOTO 1002
sn = VAL(sn$)
n$ = LEFT$(n$, 8)
n$ = n$ + SPACE$(8 - LEN(n$))
OPEN "GFX\SM.gfx" FOR INPUT AS #1
OPEN "GFX\SM2.gfx" FOR OUTPUT AS #2
FOR z = 0 TO 9
 INPUT #1, null, n2$, tlevel, thealth
 IF null = sn THEN
  PRINT #2, LTRIM$(STR$(null)) + CHR$(44) + CHR$(34) + n$ + CHR$(34) + CHR$(44) + LTRIM$(STR$(Level)) + CHR$(44) + LTRIM$(STR$(health))
 ELSE
  PRINT #2, LTRIM$(STR$(null)) + CHR$(44) + CHR$(34) + n2$ + CHR$(34) + CHR$(44) + LTRIM$(STR$(tlevel)) + CHR$(44) + LTRIM$(STR$(thealth))
 END IF
NEXT

CLOSE
CLOSE
KILL "gfx\sm.gfx"
NAME "gfx\sm2.gfx" AS "gfx\sm.gfx"
LINE (0, 0)-(320, 9), 0, BF

END SUB

SUB SetItems
Lives = 3
'ERASE obj
FOR z = 1 TO 5: Keys(z) = 0: NEXT
END SUB

SUB ShootLaser (t)

SELECT CASE LASER(t).typ
CASE 1
IF x > LASER(t).x - 1 AND x < LASER(t).x + 12 THEN
LASER(t).shotdir = 1
LASER(t).shotx = LASER(t).x + 5
LASER(t).shoty = LASER(t).y
PUT (LASER(t).shotx, LASER(t).shoty), LV%
LASER(t).shot = 1
END IF

CASE 2
IF x > LASER(t).x - 1 AND x < LASER(t).x + 12 THEN
LASER(t).shotdir = 2
LASER(t).shotx = LASER(t).x + 5
LASER(t).shoty = LASER(t).y + 2
PUT (LASER(t).shotx, LASER(t).shoty), LV%
LASER(t).shot = 1
END IF

CASE 3
IF y > LASER(t).y - 1 AND y < LASER(t).y + 12 THEN
LASER(t).shotdir = 3
LASER(t).shotx = LASER(t).x + 1
LASER(t).shoty = LASER(t).y + 5
PUT (LASER(t).shotx, LASER(t).shoty), LH%
LASER(t).shot = 1
END IF

CASE 4
IF y > LASER(t).y - 1 AND y < LASER(t).y + 12 THEN
LASER(t).shotdir = 4
LASER(t).shotx = LASER(t).x + 2
LASER(t).shoty = LASER(t).y + 5
PUT (LASER(t).shotx, LASER(t).shoty), LH%
LASER(t).shot = 1
END IF

END SELECT
END SUB

SUB ShootQuad (t)
IF qs(1, t) = 0 THEN GOTO shootup
nsq1:
IF qs(2, t) = 0 THEN GOTO shootdn
nsq2:
IF qs(3, t) = 0 THEN GOTO shootlt
nsq3:
IF qs(4, t) = 0 THEN GOTO shootrt
nsq4:
EXIT SUB

shootup:
IF x > q(t).x AND x < q(t).x + 5 THEN
qs(1, t) = 1
qsx(1, t) = q(t).x + 2
qsy(1, t) = q(t).y - 5
PUT (qsx(1, t), qsy(1, t)), LV%
END IF
GOTO nsq1

shootdn:
IF x > q(t).x AND x < q(t).x + 5 THEN
qs(2, t) = 1
qsx(2, t) = q(t).x + 2
qsy(2, t) = q(t).y + 4
PUT (qsx(2, t), qsy(2, t)), LV%
END IF
GOTO nsq2

shootlt:
IF y > q(t).y AND y < q(t).y + 5 THEN
qs(3, t) = 1
qsx(3, t) = q(t).x - 4
qsy(3, t) = q(t).y + 2
PUT (qsx(3, t), qsy(3, t)), LH%
END IF
GOTO nsq3

shootrt:
IF y > q(t).y AND y < q(t).y + 5 THEN
qs(4, t) = 1
qsx(4, t) = q(t).x + 4
qsy(4, t) = q(t).y + 2
PUT (qsx(4, t), qsy(4, t)), LH%
END IF
GOTO nsq4

END SUB

SUB ShowCodes (t)
SELECT CASE t
CASE 1
FOR z = 1 TO 6: ilk$(z) = "": NEXT
LINE (0, 0)-(320, 9), 0, BF
LOCATE 1, 1: COLOR 15: PRINT "Cheater!!! The code is "; code$
WHILE INKEY$ <> "": WEND
WHILE INKEY$ = "": WEND
LINE (0, 0)-(320, 9), 0, BF

CASE 2
LINE (0, 0)-(320, 9), 0, BF
FOR z = 2 TO 10 STEP 2
LOCATE 1, z / 2: IF Keys(z / 2) = 1 THEN COLOR 2: PRINT LTRIM$(RTRIM$(STR$(z / 2)))
NEXT
WHILE INKEY$ <> "": WEND
WHILE INKEY$ = "": WEND
LINE (0, 0)-(320, 9), 0, BF
END SELECT
END SUB

SUB ShutDown
OPEN "speed.dat" FOR OUTPUT AS #1: PRINT #1, mainpause: CLOSE
SYSTEM
END SUB

SUB SndCtrl
IF sfx = 1 THEN sfx = 0: MsgPut "Sound Off", 1: EXIT SUB
IF sfx = 0 THEN sfx = 1: MsgPut "Sound On", 1: SOUND 200, 1: SOUND 600, 1

END SUB

SUB UpdateBeam (t)
tx = LASER(t).shotx
ty = LASER(t).shoty
SELECT CASE LASER(t).shotdir


CASE 1
IF tx >= x AND tx < x + 8 AND ty >= y AND ty < y + 8 THEN HitChar 10: GOTO ebeam1
IF POINT(tx, ty - 1) = 50 THEN GOTO gb1
IF POINT(tx, ty - 1) <> 0 THEN GOTO ebeam1
gb1:
LINE (tx, ty)-(tx, ty + 4), 0
ty = ty - 1
PUT (tx, ty), LV%

CASE 2
IF tx >= x AND tx < x + 8 AND ty + 5 >= y AND ty + 5 < y + 8 THEN HitChar 10: GOTO ebeam1
IF POINT(tx, ty + 5) = 50 THEN GOTO gb2
IF POINT(tx, ty + 5) <> 0 THEN GOTO ebeam1
gb2:
LINE (tx, ty)-(tx, ty + 4), 0
ty = ty + 1
PUT (tx, ty), LV%

CASE 3
IF tx >= x AND tx < x + 8 AND ty >= y AND ty < y + 8 THEN HitChar 10: GOTO ebeam2
IF POINT(tx - 1, ty) = 50 THEN GOTO gb3
IF POINT(tx - 1, ty) <> 0 THEN GOTO ebeam2
gb3:
LINE (tx, ty)-(tx + 4, ty), 0
tx = tx - 1
PUT (tx, ty), LH%

CASE 4
IF tx + 5 >= x AND tx + 5 < x + 8 AND ty >= y AND ty < y + 8 THEN HitChar 10: GOTO ebeam2
IF POINT(tx + 5, ty) = 50 THEN GOTO gb4
IF POINT(tx + 5, ty) <> 0 THEN GOTO ebeam2
gb4:
LINE (tx, ty)-(tx + 4, ty), 0
tx = tx + 1
PUT (tx, ty), LH%

END SELECT

LASER(t).shotx = tx
LASER(t).shoty = ty

roub:
EXIT SUB

ebeam1:
LINE (tx, ty)-(tx, ty + 4), 0
LASER(t).shot = 0
GOTO roub

ebeam2:
LINE (tx + 4, ty)-(tx, ty), 0
LASER(t).shot = 0
GOTO roub

END SUB

SUB UpdateLaser
FOR z = 1 TO Lasers
rsl:
IF LASER(z).onflag = 0 THEN GOTO nextlaser
temp = LASER(z).dir
temp2 = LASER(z).x
temp3 = LASER(z).y

LASER(z).dir = temp
IF LASER(z).dir = 1 THEN GOTO L.UP
IF LASER(z).dir = 2 THEN GOTO L.DN
IF LASER(z).dir = 3 THEN GOTO L.LT
IF LASER(z).dir = 4 THEN GOTO L.RT

nextlaser:
IF LASER(z).shot = 0 THEN ShootLaser z
IF LASER(z).shot = 1 THEN UpdateBeam z
NEXT
EXIT SUB

L.UP:
IF LASER(z).y - 1 = LASER(z).Y1 THEN LASER(z).dir = 2: GOTO nextlaser
IF LASER(z).y - 1 = LASER(z).Y2 THEN LASER(z).dir = 2: GOTO nextlaser
LINE (LASER(z).x, LASER(z).y)-(LASER(z).x + 7, LASER(z).y + 10), 0, BF
LASER(z).y = LASER(z).y - 1
IF LASER(z).typ = 3 THEN PUT (LASER(z).x, LASER(z).y), LLT%
IF LASER(z).typ = 4 THEN PUT (LASER(z).x, LASER(z).y), LRT%
GOTO nextlaser

L.DN:
IF LASER(z).y + 1 = LASER(z).Y1 THEN LASER(z).dir = 1: GOTO nextlaser
IF LASER(z).y + 1 = LASER(z).Y2 THEN LASER(z).dir = 1: GOTO nextlaser
LINE (LASER(z).x, LASER(z).y)-(LASER(z).x + 7, LASER(z).y + 10), 0, BF
LASER(z).y = LASER(z).y + 1
IF LASER(z).typ = 3 THEN PUT (LASER(z).x, LASER(z).y), LLT%
IF LASER(z).typ = 4 THEN PUT (LASER(z).x, LASER(z).y), LRT%
GOTO nextlaser

L.LT:
IF LASER(z).x - 1 = LASER(z).X1 THEN LASER(z).dir = 4: GOTO nextlaser
IF LASER(z).x - 1 = LASER(z).X2 THEN LASER(z).dir = 4: GOTO nextlaser
LINE (LASER(z).x, LASER(z).y)-(LASER(z).x + 10, LASER(z).y + 7), 0, BF
LASER(z).x = LASER(z).x - 1
IF LASER(z).typ = 1 THEN PUT (LASER(z).x, LASER(z).y), LUP%
IF LASER(z).typ = 2 THEN PUT (LASER(z).x, LASER(z).y), LDN%
GOTO nextlaser

L.RT:
IF LASER(z).x + 1 = LASER(z).X1 THEN LASER(z).dir = 3: GOTO nextlaser
IF LASER(z).x + 1 = LASER(z).X2 THEN LASER(z).dir = 3: GOTO nextlaser
LINE (LASER(z).x, LASER(z).y)-(LASER(z).x + 10, LASER(z).y + 7), 0, BF
LASER(z).x = LASER(z).x + 1
IF LASER(z).typ = 1 THEN PUT (LASER(z).x, LASER(z).y), LUP%
IF LASER(z).typ = 2 THEN PUT (LASER(z).x, LASER(z).y), LDN%
GOTO nextlaser

END SUB

SUB UpdateQBeam (t)

FOR z = 1 TO 4
tx = qsx(z, t)
ty = qsy(z, t)
IF qs(z, t) = 1 THEN GOTO mql
nq1:
qsx(z, t) = tx
qsy(z, t) = ty
NEXT

EXIT SUB
mql:
SELECT CASE z
CASE 1

uup:
IF tx >= x AND tx < x + 8 AND ty >= y AND ty < y + 8 THEN HitChar 10: GOTO ebeam3
IF POINT(tx, ty - 1) = 50 THEN GOTO gb5
IF POINT(tx, ty - 1) <> 0 THEN GOTO ebeam3
gb5:
LINE (tx, ty)-(tx, ty + 4), 0
ty = ty - 1
PUT (tx, ty), LV%

CASE 2

udn:
IF tx >= x AND tx < x + 8 AND ty + 5 >= y AND ty + 5 < y + 8 THEN HitChar 10: GOTO ebeam3
IF POINT(tx, ty + 5) = 50 THEN GOTO gb6
IF POINT(tx, ty + 5) <> 0 THEN GOTO ebeam3
gb6:
LINE (tx, ty)-(tx, ty + 4), 0
ty = ty + 1
PUT (tx, ty), LV%

CASE 3
ult:
IF tx >= x AND tx < x + 8 AND ty >= y AND ty < y + 8 THEN HitChar 10: GOTO ebeam4
IF POINT(tx - 1, ty) = 50 THEN GOTO gb7
IF POINT(tx - 1, ty) <> 0 THEN GOTO ebeam4
gb7:
LINE (tx, ty)-(tx + 4, ty), 0
tx = tx - 1
PUT (tx, ty), LH%

CASE 4
urt:
IF tx + 5 >= x AND tx + 5 < x + 8 AND ty >= y AND ty < y + 8 THEN HitChar 10: GOTO ebeam4
IF POINT(tx + 5, ty) = 50 THEN GOTO gb8
IF POINT(tx + 5, ty) <> 0 THEN GOTO ebeam4
gb8:
LINE (tx, ty)-(tx + 4, ty), 0
tx = tx + 1
PUT (tx, ty), LH%

END SELECT
GOTO nq1

ebeam3:
LINE (tx, ty)-(tx, ty + 4), 0
qs(z, t) = 0
GOTO nq1

ebeam4:
LINE (tx + 4, ty)-(tx, ty), 0
qs(z, t) = 0

GOTO nq1

END SUB

SUB UpdateQuad
FOR z = 1 TO qdl
IF q(z).onflag = 0 THEN GOTO nextquad
s1 = q(z).stp
IF q(z).dir = 1 THEN GOTO qup
IF q(z).dir = 2 THEN GOTO qdn
IF q(z).dir = 3 THEN GOTO qlt
IF q(z).dir = 4 THEN GOTO qrt
nextquad:
ShootQuad z
UpdateQBeam z
NEXT
EXIT SUB

'*******
qup:
IF q(z).y = qy(z, s1) AND q(z).x = qx(z, s1) THEN GOTO changedir
LINE (q(z).x, q(z).y)-(q(z).x + 5, q(z).y + 5), 0, BF
q(z).y = q(z).y - 1
PUT (q(z).x, q(z).y), QUAD%
GOTO nextquad

qdn:
IF q(z).y = qy(z, s1) AND q(z).x = qx(z, s1) THEN GOTO changedir
LINE (q(z).x, q(z).y)-(q(z).x + 5, q(z).y + 5), 0, BF
q(z).y = q(z).y + 1
PUT (q(z).x, q(z).y), QUAD%
GOTO nextquad

qlt:
IF q(z).x = qx(z, s1) AND q(z).y = qy(z, s1) THEN GOTO changedir
LINE (q(z).x, q(z).y)-(q(z).x + 5, q(z).y + 5), 0, BF
q(z).x = q(z).x - 1
PUT (q(z).x, q(z).y), QUAD%
GOTO nextquad

qrt:
IF q(z).x = qx(z, s1) AND q(z).y = qy(z, s1) THEN GOTO changedir
LINE (q(z).x, q(z).y)-(q(z).x + 5, q(z).y + 5), 0, BF
q(z).x = q(z).x + 1
PUT (q(z).x, q(z).y), QUAD%
GOTO nextquad

changedir:
s2 = s1 + 1
IF s2 > q(z).totstps THEN s2 = 2
IF qx(z, s2) = q(z).x AND qy(z, s2) < q(z).y THEN q(z).dir = 1: GOTO nq2
IF qx(z, s2) = q(z).x AND qy(z, s2) > q(z).y THEN q(z).dir = 2: GOTO nq2
IF qx(z, s2) < q(z).x AND qy(z, s2) = q(z).y THEN q(z).dir = 3: GOTO nq2
IF qx(z, s2) > q(z).x AND qy(z, s2) = q(z).y THEN q(z).dir = 4
nq2:
q(z).stp = q(z).stp + 1
IF q(z).stp > q(z).totstps THEN q(z).stp = 2
GOTO nextquad
END SUB

SUB UpdateSprites
FOR z = 1 TO 5
IF KeyX(z) = 0 AND KeyY(z) = 0 THEN GOTO nk
PUT (KeyX(z), KeyY(z)), CB%, PSET
nk:
NEXT

IF msg = 0 AND health > 99 AND mf = 1 THEN
FOR z = 2 TO 10 STEP 2
LOCATE 1, z / 2: IF Keys(z / 2) = 1 THEN COLOR 2: PRINT LTRIM$(RTRIM$(STR$(z / 2)))
NEXT
mf = 0
END IF

IF TIMER - ffu >= 2 THEN FFUpdate
END SUB

