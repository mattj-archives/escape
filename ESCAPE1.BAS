DECLARE SUB KeyboardOn ()
DECLARE SUB WaitForKeyOld ()
DECLARE SUB Menu ()
DECLARE FUNCTION Menu.GetFName! ()
DECLARE FUNCTION Menu.GetInpt! (te$)
DECLARE SUB Menu.Special ()
DECLARE SUB Intro ()
DECLARE FUNCTION Intro.ShortWait! (delay AS SINGLE)
DECLARE SUB WaitVSync ()
DECLARE SUB SaveSavedData ()
DECLARE SUB WaitForKey ()
DECLARE SUB KeyboardInit ()
DECLARE SUB GameOver ()
DECLARE SUB ShutDown ()
DECLARE SUB SndCtrl ()
DECLARE SUB SaveGame ()
DECLARE SUB FO (tx!, ty!, t$)
DECLARE SUB KeyInpt (t$)
DECLARE SUB ShowCodes (t)
DECLARE SUB LoadStat ()
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
DECLARE SUB LevelSet (LevelNum AS INTEGER)

' External
DECLARE SUB KeyboardEnable
DECLARE SUB KeyboardDisable
DECLARE SUB KeyboardLibInit (BYVAL arraySeg%, BYVAL arrayOffs%, BYVAL prevArraySeg%, BYVAL prevArrayOffs%)
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

TYPE TSaveGame
 name AS STRING * 80
 Level AS INTEGER
END TYPE

TYPE TSavedData
 ' 0 = Can't access
 ' 1 = Can access, not finished
 ' 2 = Finished
 bonusLevels(1 TO 4) AS INTEGER
 saveGames(1 TO 4) AS TSaveGame
END TYPE

DIM SHARED keys(6), KeyTyp(6)
DIM SHARED KeyX(6), KeyY(6)
DIM SHARED FF(20) AS f
DIM SHARED obj AS o, FFN, FFNum(20)       'type*num
DIM SHARED LASER(20) AS L, NumOfFF(10), NumOfL(10), NumOfQd(10)
DIM SHARED FFCH(10, 5), LCH(10, 8), QCH(10, 5)
DIM SHARED minicomp(10) AS mcomp
DIM SHARED Lasers, tempmcomp
DIM SHARED LaserTyp(20), LaserNum(20)
DIM SHARED dir, frame, framecount: frame = 0: framecount = 0
DIM SHARED x, y
DIM SHARED Level%
DIM SHARED lft, ltime: lft = .05
DIM SHARED health, ht: health = 100
DIM SHARED msg, msgt
DIM SHARED StartX, StartY, StartDir
DIM SHARED q(4) AS qd
DIM SHARED qx(4, 15), qy(4, 15), qs(4, 4), qsx(4, 4), qsy(4, 4), qdl
DIM SHARED mf, s1, lf, ffu, sfx, mainpause: mf = 0: sfx = 1
DIM SHARED ilk$(6), Lives: Lives = 3

DIM SHARED kb(128) AS INTEGER
DIM SHARED prevKeys(128) AS INTEGER

DIM SHARED SavedData AS TSavedData

DIM SHARED MenuChoice%, MenuLevelWarp%

' Is level running?
DIM SHARED Running AS INTEGER: Running = 1

ON ERROR RESUME NEXT
DEF SEG = VARSEG(SavedData)
BLOAD "game.dat", VARPTR(SavedData)
DEF SEG

'qs=shot flag
Level% = 31

SCREEN 13
LGFX
KeyboardInit
Intro

DO
        Menu
        SELECT CASE MenuChoice%
        CASE 1:
                SetItems
                Level% = MenuLevelWarp%
                LevelIntro
                LevelSet Level%
                MainLoop
                KeyboardDisable
        CASE 5:
                EXIT DO
        END SELECT
LOOP

SaveSavedData

SCREEN 0: WIDTH 80

SUB BeatUB
CLS
SavedData.bonusLevels(1) = 2

finishedCount% = 0
FOR i = 1 TO 4
 IF SavedData.bonusLevels(i) = 2 THEN finishedCount% = finishedCount% + 1
NEXT

IF finishedCount% = 4 THEN
COLOR 3
PRINT "Wow... You beat the Almost unbeatable"
PRINT "level... For a useful cheat, type: "
PRINT : PRINT CHR$(66); : PRINT CHR$(83);
PRINT CHR$(89); : PRINT CHR$(49);
PRINT CHR$(52); : PRINT CHR$(49): PRINT
PRINT "During the game."
ELSE
COLOR 3
PRINT "To get the cheat, complete the"
PRINT "Practice levels."
END IF

WaitForKey
Running = 0
'66,83,89,49,52,49
END SUB

SUB compentr (tx, ty)
IF dir = 1 THEN ty = ty - 1
IF dir = 2 THEN ty = ty + 1
IF dir = 3 THEN tx = tx - 1
IF dir = 4 THEN tx = tx + 1
FOR Z = 1 TO tempmcomp
IF tx >= minicomp(Z).x AND tx <= minicomp(Z).x + minicomp(Z).limx THEN
IF ty >= minicomp(Z).y AND ty <= minicomp(Z).y + minicomp(Z).limy THEN
 GOTO gk2
 END IF
END IF
NEXT
EXIT SUB

gk2:
LINE (0, 0)-(320, 9), 0, BF
LOCATE 1, 1: COLOR 9: PRINT "Accessing..."
IF sfx = 1 THEN FOR z3 = 1 TO 20: SOUND INT(RND * 500) + 137, 1: NEXT
IF keys(minicomp(Z).numreq) = 0 THEN GOTO nokey
MsgPut "Access granted!", 1
IF sfx = 1 THEN SOUND 1000, 4
t = TIMER: WHILE TIMER - t < 1: WEND
RunComp Z
GOTO es
nokey:
MsgPut "You are missing CC" + STR$(minicomp(Z).numreq), 1
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

FOR Z = 1 TO Lasers
IF LASER(Z).onflag = 0 THEN
DisLas Z, 28, 1
IF sfx = 1 THEN FOR zz = 600 TO 300 STEP -50: SOUND zz, .5: SOUND zz + 100, .5: NEXT
END IF
NEXT

FOR Z = 1 TO qdl
IF q(Z).onflag = 0 THEN
DisLas Z, 28, 2
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
ERASE keys, KeyTyp, KeyX, KeyY, FF, FFNum, LASER, NumOfFF, NumOfL, NumOfQd
ERASE FFCH, LCH, QCH, minicomp, LaserTyp, LaserNum, q, qx, qy, qs, qsx, qsy
FFN = 0: Lasers = 0: tempmcomp = 0: health = 100: qdl = 0

END SUB

SUB FinishGame
CLS
SavedData.bonusLevels(1) = 1

FO 10, 1, "Well done, you beat this game..."
FO 11, 1, "The Unbeatable Level is now"
FO 12, 1, "accessable..."
WaitForKey
Running = 0

END SUB

SUB FinishPL (t)

SavedData.bonusLevels(t + 1) = 2

IF t < 3 THEN
 SavedData.bonusLevels(t + 2) = 1
END IF

CLS
SELECT CASE t
CASE 1
        FO 1, 1, "Practice level 2 is available"
CASE 2
        FO 1, 1, "Practice level 3 is available"
CASE 3
        FO 1, 1, "All practice levels completed"
END SELECT
FO 2, 1, "Press a key..."
WaitForKey
Running = 0

END SUB

SUB FO (tx, ty, t$)
FOR Z = 16 TO 31
        LOCATE tx, ty: COLOR Z: PRINT t$
        t = TIMER: WHILE TIMER - t < .001: WEND
NEXT

END SUB

SUB GameOver
FOR Z = 1 TO 300
CIRCLE (160, 100), Z, 4
IF Z MOD 8 = 0 THEN
 WAIT &H3DA, 8: WAIT &H3DA, 8, 8
END IF
NEXT
FOR Z = 41 TO 1 STEP -1
PALETTE 4, 65536 * 0 + 256 * 0 + Z
 WAIT &H3DA, 8: WAIT &H3DA, 8, 8
NEXT
CLS
COLOR 4: LOCATE 10, 14: PRINT "Game Over!!!"
FOR Z = 1 TO 41
PALETTE 4, 65536 * 0 + 256 * 0 + Z
 WAIT &H3DA, 8: WAIT &H3DA, 8, 8
NEXT
t = TIMER: WHILE TIMER - t < 2: WEND
FOR Z = 41 TO 1 STEP -1
PALETTE 4, 65536 * 0 + 256 * 0 + Z
 WAIT &H3DA, 8: WAIT &H3DA, 8, 8
NEXT
Running = 0
END SUB

SUB GotKey
FOR Z = 1 TO 6
IF x > KeyX(Z) AND x < KeyX(Z) + 6 THEN
 IF y > KeyY(Z) AND y < KeyY(Z) + 6 THEN
  IF sfx = 1 THEN FOR z2 = 500 TO 4000 STEP 500: SOUND z2, .1: SOUND z2 + 500, .1: NEXT
  GOTO gk
 END IF
END IF
NEXT
EXIT SUB
gk:
LINE (KeyX(Z), KeyY(Z))-(KeyX(Z) + 6, KeyY(Z) + 6), 0, BF
keys(Z) = 1
KeyX(Z) = 0: KeyY(Z) = 0
IF mf = 0 THEN
FOR Z = 2 TO 12 STEP 2
LOCATE 1, Z / 2: IF keys(Z / 2) = 1 THEN COLOR 2: PRINT LTRIM$(RTRIM$(STR$(Z / 2)))
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
FOR Z = 1 TO 5
CIRCLE (x + 3, y + 3), Z, 4
IF sfx = 1 THEN SOUND 0, 2
NEXT

FOR Z = 1 TO 5
CIRCLE (x + 3, y + 3), Z, 0
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
WaitForKey
IF dir = 1 THEN PUT (x, y), UP1%
IF dir = 2 THEN PUT (x, y), DN1%
IF dir = 3 THEN PUT (x, y), LT1%
IF dir = 4 THEN PUT (x, y), RT1%
LINE (0, 0)-(200, 8), 0, BF
health = 100
WHILE INKEY$ <> "": WEND
END SUB

SUB Intro
FOR Z = 1 TO 15
SIZE$ = STR$(Z)
SIZE$ = LTRIM$(SIZE$)
XY1$ = "C2 S" + SIZE$
tx = 0: ty = 70
x = 160 - (Z * 10): y = 100 - (Z * 2)
SX$ = STR$(x)
SY$ = STR$(y)
SX$ = LTRIM$(SX$)
SY$ = LTRIM$(SY$)
XY2$ = " BM"
XY2$ = XY2$ + SX$ + ","
XY2$ = XY2$ + SY$ + " R10L10D10R10L10D10R10BR5"
XY2$ = XY2$ + "R10U10L10U10R10BR5"
XY2$ = XY2$ + "R10L10D20R10BR5"
XY2$ = XY2$ + "U20R10D20U10L10R10D10BR5"
XY2$ = XY2$ + "U20R10D10L10D10BR15"
XY2$ = XY2$ + "R10 L10 U10 R10 L10 U10 R10"

XY1$ = XY1$ + XY2$
DRAW XY1$

WaitVSync: WaitVSync: WaitVSync

'*******************************************
XY1$ = "C1 S" + SIZE$
XY2$ = " BM"
XY2$ = XY2$ + SX$ + ","
XY2$ = XY2$ + SY$ + " R10 L10 D10 R10L10D10R10BR5"
XY2$ = XY2$ + "R10U10L10U10R10BR5"
XY2$ = XY2$ + "R10L10D20R10BR5"
XY2$ = XY2$ + "U20R10D20U10L10R10D10BR5"
XY2$ = XY2$ + "U20R10D10L10D10BR15"
XY2$ = XY2$ + "R10 L10 U10 R10 L10 U10 R10"

XY1$ = XY1$ + XY2$
DRAW XY1$
a$ = INKEY$: IF a$ <> "" THEN EXIT SUB
NEXT


x = 160 - (15 * 10): y = 100 - (15 * 2)
SIZE$ = "15"
XY1$ = "C2 S" + SIZE$
SX$ = STR$(x)
SY$ = STR$(y)
SX$ = LTRIM$(SX$)
SY$ = LTRIM$(SY$)
XY2$ = " BM"
XY2$ = XY2$ + SX$ + ","
XY2$ = XY2$ + SY$ + " R10L10D10R10L10D10R10BR5"
XY2$ = XY2$ + "R10U10L10U10R10BR5"
XY2$ = XY2$ + "R10L10D20R10BR5"
XY2$ = XY2$ + "U20R10D20U10L10R10D10BR5"
XY2$ = XY2$ + "U20R10D10L10D10BR15"
XY2$ = XY2$ + "R10 L10 U10 R10 L10 U10 R10"
XY1$ = XY1$ + XY2$
DRAW XY1$

IF Intro.ShortWait(2) THEN EXIT SUB

tr = 0: tg = 0: tb = 1
FOR Z = 41 TO 1 STEP -1
PALETTE 1, 65536 * Z + 256 * 0 + 0
PALETTE 4, 65536 * 0 + 256 * 0 + Z

IF Intro.ShortWait(.01) THEN EXIT SUB

NEXT

COLOR 4: LOCATE 20, 10: PRINT "By Matthew Johnson"

FOR Z = 0 TO 63
PALETTE 4, 65536 * 0 + 256 * 0 + Z

IF Intro.ShortWait(.01) THEN EXIT SUB
NEXT

IF Intro.ShortWait(1) THEN EXIT SUB

FOR Z = 41 TO 0 STEP -1
PALETTE 2, 65536 * 0 + 256 * Z + 0
IF Intro.ShortWait(.01) THEN EXIT SUB
NEXT

FOR Z = 63 TO 0 STEP -1
PALETTE 4, 65536 * 0 + 256 * 0 + Z
IF Intro.ShortWait(.01) THEN EXIT SUB
NEXT
CLS
COLOR 4: LOCATE 10, 10: PRINT "Escape... If you can..."
FOR Z = 0 TO 63
PALETTE 4, 65536 * 0 + 256 * 0 + Z
IF Intro.ShortWait(.01) THEN EXIT SUB
NEXT
ts = TIMER: WHILE TIMER - ts < 2: WEND
FOR Z = 63 TO 0 STEP -1
PALETTE 4, 65536 * 0 + 256 * 0 + Z
IF Intro.ShortWait(.01) THEN EXIT SUB
NEXT

END SUB

FUNCTION Intro.ShortWait (delay AS SINGLE)
t! = TIMER
WHILE TIMER - t! < delay
 a$ = INKEY$
 IF a$ <> "" THEN
        Intro.ShortWait = 1
        EXIT FUNCTION
 END IF
WEND

Intro.ShortWait = 0
END FUNCTION

SUB KeyboardInit
KeyboardLibInit BYVAL VARSEG(kb(0)), BYVAL VARPTR(kb(0)), BYVAL VARSEG(prevKeys(0)), BYVAL VARPTR(prevKeys(0))
END SUB

SUB KeyboardOn

FOR i% = 0 TO 128
 kb(i%) = 0
NEXT

KeyboardEnable
END SUB

SUB KeyInpt (t$)
IF ilk$(6) <> "" OR ilk$(6) = "" AND ilk$(5) = "" THEN
 FOR Z = 1 TO 5: ilk$(Z) = ilk$(Z + 1): NEXT
ilk$(6) = t$
GOTO EIF1
ELSE
FOR Z = 1 TO 5
 IF ilk$(Z) = "" THEN ilk$(Z) = t$: GOTO EIF1
NEXT
EIF1:
END IF
k$ = ""
FOR Z = 1 TO 6
 k$ = k$ + ilk$(Z)
NEXT
' IF k$ = "BSY141" THEN ShowCodes 1
END SUB

SUB LevelIntro
KeyboardDisable
roi:
tlevel = Level%
IF Level% = 32 THEN tlevel = 1
IF Level% = 33 THEN tlevel = 2
IF Level% = 34 THEN tlevel = 3
IF Level% > 31 THEN X2$ = "Practice level " ELSE X2$ = "Level "
IF tlevel <> 31 THEN X2$ = X2$ + LTRIM$(RTRIM$(STR$(tlevel))) ELSE X2$ = "The Unbeatable Level"

col = (40 - LEN(X2$)) / 2
CLOSE
CLS
'                  b         g   r
PALETTE 1, 65536 * 0 + 256 * 0 + 0
PALETTE 4, 65536 * 0 + 256 * 0 + 0
COLOR 4: LOCATE 10, col: PRINT X2$
COLOR 4: LOCATE 12, 5: PRINT "Lives remaining:"; Lives
FOR Z = 1 TO 41
PALETTE 1, 65536 * Z + 256 * 0 + 0
PALETTE 4, 65536 * 0 + 256 * 0 + Z
WAIT &H3DA, 8: WAIT &H3DA, 8, 8
NEXT
LOCATE 24, 1: COLOR 5: PRINT "Press a key...";
WaitForKey
FOR Z = 41 TO 1 STEP -1
PALETTE 1, 65536 * Z + 256 * 0 + 0
PALETTE 4, 65536 * 0 + 256 * 0 + Z
WAIT &H3DA, 8: WAIT &H3DA, 8, 8
NEXT
CLS
PALETTE 1, 65536 * 41 + 256 * 0 + 0
PALETTE 4, 65536 * 0 + 256 * 0 + 41
KeyboardOn
END SUB

SUB LevelSet (LevelNum AS INTEGER)
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
FOR Z = 1 TO tmpstps
 INPUT #1, qx(qdl, Z), qy(qdl, Z)
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
FOR Z = 1 TO NumOfFF(tempmcomp)
IF NumOfFF(tempmcomp) >= Z THEN INPUT #1, FFCH(tempmcomp, Z)
NEXT
INPUT #1, NumOfL(tempmcomp)
IF NumOfL(tempmcomp) = 0 THEN INPUT #1, dummy
FOR Z = 1 TO NumOfL(tempmcomp)
IF NumOfL(tempmcomp) >= Z THEN INPUT #1, LCH(tempmcomp, Z)
NEXT
INPUT #1, NumOfQd(tempmcomp)
IF NumOfQd(tempmcomp) = 0 THEN INPUT #1, dummy
FOR Z = 1 TO NumOfQd(tempmcomp)
IF NumOfQd(tempmcomp) >= Z THEN INPUT #1, QCH(tempmcomp, Z)
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

SUB MainLoop
ht = TIMER
Running = 1

DO WHILE Running = 1

        IF kb(72) THEN MoveUp
        IF kb(75) THEN MoveLt
        IF kb(77) THEN MoveRt
        IF kb(80) THEN MoveDn

IF kb(16) THEN
 Running = 0
END IF

IF kb(17) THEN
 PassLevel
END IF
'IF UCASE$(a$) = "Q" THEN ShutDown
'IF a$ = "+" THEN ShowCodes 2
'IF a$ > "0" AND a$ < "Z" THEN KeyInpt a$
IF a$ = CHR$(0) + "<" THEN SaveGame
'IF a$ = "," THEN mainpause = mainpause - 10: MsgPut "Pauseloop" + STR$(mainpause) + "*100", 1
'IF a$ = "." THEN mainpause = mainpause + 10: MsgPut "Pauseloop" + STR$(mainpause) + "*100", 1
IF UCASE$(a$) = "S" THEN SndCtrl
UpdateLaser
IF qdl > 0 THEN UpdateQuad
UpdateSprites


' FOR z = 1 TO mainpause * 100: NEXT

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
WAIT &H3DA, 8
WAIT &H3DA, 8, 8
LOOP
END SUB

SUB Menu
RSWP:
DIM AR%(200)
DIM SN$(0 TO 9)
DIM SL(0 TO 9)
DIM SH(0 TO 9)
DIM MenuNames(1 TO 5) AS STRING
DIM MenuHelp(1 TO 5) AS STRING

'***************************
M = 1                     '*
MenuNames(1) = "Play Game"
MenuNames(2) = "Load Game"
MenuNames(3) = "Special"
MenuNames(4) = "Read This!!!"
MenuNames(5) = "Quit"

MenuHelp(1) = "Starts a new game"
MenuHelp(2) = "Loads a saved game"
MenuHelp(3) = "Special/Practice level accsess"
MenuHelp(4) = "Instructions"
MenuHelp(5) = "Quits the game"

AY = 73         '*

CLS

' Load the arrow graphic
PSET (0, 2), 32: PSET (1, 2), 33: PSET (2, 2), 34: PSET (3, 2), 35
PSET (3, 1), 35: PSET (3, 3), 35: PSET (4, 2), 36
GET (0, 0)-(5, 5), AR%

'***************************
ST:
Main:
CLS
Main2:
PUT (60, AY), AR%, XOR
FOR PM = 1 TO 5
IF PM = M THEN LOCATE PM + 9, 10: COLOR 14: PRINT MenuNames(PM)
IF PM <> M THEN LOCATE PM + 9, 10: COLOR 32: PRINT MenuNames(PM)
NEXT
LOCATE 20, 1: PRINT STRING$(40, " "): COLOR 7: LOCATE 20, 1: PRINT MenuHelp(M)

'*****************************************************
DO
a$ = INKEY$
IF a$ = CHR$(0) + "H" THEN
        IF M > 1 THEN GOTO MenuUp
END IF

IF a$ = CHR$(0) + "P" THEN
        IF M < 5 THEN GOTO MenuDown
END IF

IF UCASE$(a$) = "Q" THEN
        MenuChoice% = 5
        EXIT SUB
END IF

IF a$ = CHR$(13) OR a$ = " " THEN GOTO MenuRet
IF a$ = "+" AND M = 1 THEN lwk = 1: GOTO MenuRet
LOOP

'*****************************************************
MenuUp:
PUT (60, AY), AR%, XOR
td = 2: c = 31
FOR MA = 1 TO 8
        AY = AY - 1
        PUT (60, AY), AR%, XOR
        t = TIMER: WHILE TIMER - t < .001: WEND
        PUT (60, AY), AR%, XOR
        IF td = 2 AND c >= 16 THEN c = c - 4: IF c <= 15 THEN c = 16: td = 1: GOTO pl1
        IF td = 1 AND c <= 32 THEN c = c + 4: IF c >= 32 THEN c = 31: td = 0

pl1:
COLOR c
IF td = 2 THEN LOCATE 20, 1: PRINT MenuHelp(M) ELSE LOCATE 20, 1: PRINT MenuHelp(M - 1)
NEXT
M = M - 1
GOTO Main2

MenuDown:
td = 2: c = 31
PUT (60, AY), AR%, XOR
FOR MA = 1 TO 8
        AY = AY + 1
        PUT (60, AY), AR%, XOR
        t = TIMER: WHILE TIMER - t < .001: WEND
        PUT (60, AY), AR%, XOR
        IF td = 2 AND c >= 16 THEN c = c - 4: IF c <= 15 THEN c = 16: td = 1: GOTO pl2
        IF td = 1 AND c <= 32 THEN c = c + 4: IF c >= 32 THEN c = 31: td = 0

pl2:
COLOR c
IF td = 2 THEN LOCATE 20, 1: PRINT MenuHelp(M) ELSE LOCATE 20, 1: PRINT MenuHelp(M + 1)
NEXT
M = M + 1
GOTO Main2

MenuRet:
FOR Z = 31 TO 16 STEP -1
        LOCATE 9 + M, 10: COLOR Z: PRINT MenuNames(M)
        t = TIMER: WHILE TIMER - t < .001: WEND
NEXT
CLS

MenuChoice% = M
IF M = 1 THEN GOTO PlayGame
IF M = 2 THEN GOTO LoadGame
IF M = 3 THEN
        Menu.Special
        IF MenuLevelWarp% <> 0 THEN
                MenuChoice% = 1
                EXIT SUB
        END IF
        GOTO Main
END IF
IF M = 4 THEN GOTO ReadThis
IF M = 5 THEN GOTO Quit
'**********************************PLAY GAME
PlayGame:
MenuLevelWarp% = 1

COLOR 32
IF lwk = 1 THEN INPUT "Level warp 1-34>>>"; MenuLevelWarp%
EXIT SUB
'**********************************LOAD GAME
LoadGame:
COLOR 32
'***********
OPEN "gfx\sm.gfx" FOR INPUT AS #1
COLOR 32: LOCATE 1, 1: PRINT "#"; "   Name"; "      Level";
FOR SI = 0 TO 9
INPUT #1, null, SN$(SI), SL(SI), SH(SI)
LOCATE SI + 2, 1: PRINT SI; SN$(SI); "    "; SL(SI); "       "
NEXT
CLOSE
'***********
ReStartLoad:
LOCATE 17, 1: COLOR 32: PRINT "ENTER 99 TO EXIT"
FB = Menu.GetFName
IF FB = 99 THEN CLOSE : GOTO QL
IF FB < 0 OR FB > 9 THEN BEEP: GOTO ReStartLoad
IF SN$(FB) = "*Nothing" THEN BEEP: GOTO ReStartLoad
GOTO MLG
'************************
QL:
FOR Z = 31 TO 16 STEP -1
        LOCATE 16, 1: COLOR Z: PRINT "Enter 0-9>"
        t = TIMER: WHILE TIMER - t < .001: WEND
NEXT
GOTO ST

MLG:
OPEN "LG.FLG" FOR OUTPUT AS #1: PRINT #1, FB: CLOSE
OPEN "LW.CHK" FOR OUTPUT AS #1: PRINT #1, -1: CLOSE
FOR Z = 31 TO 16 STEP -1
        LOCATE 16, 1: COLOR Z: PRINT "Enter 0-9>"
        t = TIMER: WHILE TIMER - t < .001: WEND
NEXT
SYSTEM
'***********************************************PASSWORD

ReadThis:
EXIT SUB

Quit:
COLOR 4
PRINT "QUIT??!!!! TYPE Y OR N"
QUITLOOP:
        a$ = INKEY$
                IF a$ = "Y" OR a$ = "y" THEN GOTO YQUIT
                IF a$ = "N" OR a$ = "n" THEN GOTO NQUIT
        GOTO QUITLOOP

YQUIT:
FOR Z = 31 TO 16 STEP -1
        LOCATE 1, 1: COLOR Z: PRINT "QUIT??!!!! TYPE Y OR N"
        t = TIMER: WHILE TIMER - t < .001: WEND
NEXT
EXIT SUB

NQUIT:
FOR Z = 31 TO 16 STEP -1
        LOCATE 1, 1: COLOR Z: PRINT "QUIT??!!!! TYPE Y OR N"
        t = TIMER: WHILE TIMER - t < .001: WEND
NEXT
GOTO ST


END SUB

FUNCTION Menu.GetFName
LOCATE 16, 11: PRINT STRING$(29, " ")
tcol = 11
t$ = ""
COLOR 2: LOCATE 16, 1: PRINT "Enter 0-9>"
LOCATE 16, tcol: COLOR 10: PRINT "_"

mm:
a$ = INKEY$
IF a$ <> "" THEN GOTO gotn
GOTO mm

gotn:
SELECT CASE a$

CASE "0" TO "9"
t$ = t$ + a$
COLOR 2: LOCATE 16, 11: PRINT t$; : COLOR 10: PRINT "_"
tcol = tcol + 1

CASE CHR$(8)
IF tcol - 1 = 10 THEN GOTO mm
t$ = LEFT$(t$, LEN(t$) - 1)
COLOR 2: LOCATE 16, 11: PRINT t$; : COLOR 10: PRINT "_"
LOCATE 16, tcol: PRINT " "
tcol = tcol - 1

CASE CHR$(13)
Menu.GetFName = VAL(t$)
EXIT FUNCTION
END SELECT
GOTO mm

END FUNCTION

FUNCTION Menu.GetInpt (te$)
LOCATE 8, 1: PRINT STRING$(40, " ")
tcol = LEN(te$) + 1
mtc = tcol
t$ = ""
COLOR 5: LOCATE 8, 1: PRINT te$
LOCATE 8, tcol: COLOR 10: PRINT "_"

mm2:
a$ = INKEY$
IF a$ <> "" THEN GOTO gotn2
GOTO mm2

gotn2:
SELECT CASE a$

CASE "0" TO "9"
t$ = t$ + a$
COLOR 2: LOCATE 8, mtc: PRINT t$; : COLOR 10: PRINT "_"
tcol = tcol + 1

CASE CHR$(8)
IF tcol - 1 < mtc THEN GOTO mm2
t$ = LEFT$(t$, LEN(t$) - 1)
COLOR 2: LOCATE 8, mtc: PRINT t$; : COLOR 10: PRINT "_"
LOCATE 8, tcol: PRINT " "
tcol = tcol - 1

CASE CHR$(13)
Menu.GetInpt = VAL(t$)
EXIT FUNCTION
END SELECT
GOTO mm2

END FUNCTION

SUB Menu.Special

MenuLevelWarp% = 0

COLOR 2: LOCATE 9, 1: PRINT "Type 99 to exit"
COLOR 14: LOCATE 1, 1: PRINT "Special levels:": PRINT
COLOR 32: PRINT "1-Practice level 1"

LOCATE 4
IF SavedData.bonusLevels(3) > 0 THEN COLOR 32 ELSE COLOR 8
PRINT "2-Practice level 2"

LOCATE 5
IF SavedData.bonusLevels(4) > 0 THEN COLOR 32 ELSE COLOR 8
PRINT "3-Practice level 3"

LOCATE 6
IF SavedData.bonusLevels(1) > 0 THEN COLOR 32 ELSE COLOR 8
PRINT "4-The Unbeatable Level (31)"

rsp:
ts = Menu.GetInpt("Select 1-4>")
IF ts = 99 THEN EXIT SUB
IF ts < 1 OR ts > 4 THEN BEEP: GOTO rsp

SELECT CASE ts
CASE 1
MenuLevelWarp% = 32
EXIT SUB

CASE 2
IF SavedData.bonusLevels(2) <> 2 THEN
FO 20, 1, "This level cannot be accessed until"
FO 21, 1, "Practice level 1 is completed."
FO 22, 1, "Press a key..."

WaitForKeyOld
ELSE
LevelStart = 33: LWW = 1
END IF

CASE 3
IF SavedData.bonusLevels(3) <> 2 THEN
FO 20, 1, "This level cannot be accessed until"
FO 21, 1, "Practice level 2 is completed."
FO 22, 1, "Press a key..."
WaitForKeyOld
ELSE
LevelStart = 34: LWW = 1
END IF

CASE 4
finishCount% = 0
FOR i% = 1 TO 4
 IF SavedData.bonusLevels(i%) > 0 THEN finishCount% = finishCount% + 1
NEXT

IF finishCount% <> 4 THEN
FO 20, 1, "You must complete all 30 levels, and the"
FO 21, 1, "practice levels to go here."
FO 22, 1, "Press a key..."
WaitForKeyOld
ELSE
LevelStart = 31: LWW = 1
END IF

END SELECT
LOCATE 20: PRINT STRING$(40, " "): LOCATE 21: PRINT STRING$(40, " "): LOCATE 22: PRINT STRING$(40, " ")

IF LWW = 1 THEN
MenuLevelWarp% = LevelStart

EXIT SUB
' TODO

'OPEN "LW.CHK" FOR OUTPUT AS #1: PRINT #1, LevelStart: CLOSE
'OPEN "PG.FLG" FOR OUTPUT AS #1
'PRINT #1, "FLAG ON"
'CLOSE
'SYSTEM
END IF

GOTO rsp

END SUB

SUB MoveDn
IF y + 9 >= 200 THEN EXIT SUB

FOR Z = x TO x + 7
IF POINT(Z, y + 8) = 32 THEN GOTO cancel2
IF POINT(Z, y + 8) = 28 THEN GOTO cancel2
IF POINT(Z, y + 8) = 33 THEN PassLevel: GOTO cancel2
IF POINT(Z, y + 8) = 5 THEN dir = 2: compentr Z, y + 8: EXIT SUB
IF POINT(Z, y + 8) = 50 THEN HitChar 30: GOTO cancel2
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

FOR Z = y TO y + 7
IF POINT(x - 1, Z) = 32 THEN GOTO cancel3
IF POINT(x - 1, Z) = 28 THEN GOTO cancel3
IF POINT(x - 1, Z) = 33 THEN PassLevel: GOTO cancel3
IF POINT(x - 1, Z) = 5 THEN dir = 3: compentr x - 1, Z: EXIT SUB
IF POINT(x - 1, Z) = 50 THEN HitChar 30: GOTO cancel3
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

FOR Z = y TO y + 7
IF POINT(x + 8, Z) = 32 THEN GOTO cancel4
IF POINT(x + 8, Z) = 28 THEN GOTO cancel4
IF POINT(x + 8, Z) = 33 THEN PassLevel: GOTO cancel4
IF POINT(x + 8, Z) = 5 THEN dir = 4: compentr x + 8, Z: EXIT SUB
IF POINT(x + 8, Z) = 50 THEN HitChar 30: GOTO cancel4
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

FOR Z = x TO x + 7
IF POINT(Z, y - 1) = 32 THEN GOTO cancel1
IF POINT(Z, y - 1) = 28 THEN GOTO cancel1
IF POINT(Z, y - 1) = 33 THEN PassLevel: GOTO cancel1
IF POINT(Z, y - 1) = 5 THEN dir = 1: compentr Z, y - 1: EXIT SUB
IF POINT(Z, y - 1) = 50 THEN HitChar 30: GOTO cancel1
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

'IF sfx = 1 THEN SOUND 250, 1: SOUND 700, 1: SOUND 300, 1: SOUND 100, 1: SOUND 500, 1

IF sfx = 1 THEN SOUND 0, 5

IF sfx = 1 THEN
 FOR Z = 100 TO 4000 STEP 500: SOUND Z, .5: NEXT
 FOR Z = 37 TO 400 STEP 5: SOUND Z, .05: NEXT
 FOR Z = 200 TO 600 STEP 5: SOUND Z, .05: NEXT
 FOR Z = 400 TO 800 STEP 5: SOUND Z, .05: NEXT
 FOR Z = 600 TO 1000 STEP 5: SOUND Z, .05: NEXT
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

        IF X1 MOD 6 = 0 THEN
         WAIT &H3DA, 8: WAIT &H3DA, 8, 8
        END IF
WEND

LINE (X1, Y1)-(X2, Y2), 0, B
EraseVars
Lives = Lives + 1
Level% = Level% + 1
IF Level% = 31 THEN FinishGame
IF Level% = 32 THEN BeatUB
IF Level% = 33 THEN FinishPL 1
IF Level% = 34 THEN FinishPL 2
IF Level% = 35 THEN FinishPL 3

IF Running = 1 THEN
        LevelIntro
        LevelSet Level%
END IF

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
Z = LCH(t, z2)

IF LASER(Z).onflag = 0 THEN LASER(Z).onflag = 1 ELSE LASER(Z).onflag = 0

IF LASER(Z).onflag = 1 THEN
 DisLas Z, 0, 1
 IF sfx = 1 THEN FOR zz = 300 TO 600 STEP 50: SOUND zz, .5: SOUND zz + 100, .5: NEXT
END IF

IF LASER(Z).onflag = 0 THEN
DisLas Z, 28, 1
IF sfx = 1 THEN FOR zz = 600 TO 300 STEP -50: SOUND zz, .5: SOUND zz + 100, .5: NEXT
END IF

CASE 7
Z = FFCH(t, z2)
tempx1 = FF(Z).X1
tempx2 = FF(Z).X2
tempy1 = FF(Z).Y1
tempy2 = FF(Z).Y2
IF FF(Z).onflag = 0 THEN FF(Z).onflag = 1 ELSE FF(Z).onflag = 0

IF FF(Z).onflag = 0 THEN
LINE (tempx1, tempy1)-(tempx2, tempy2), 0
IF sfx = 1 THEN FOR zz = 800 TO 37 STEP -5: SOUND zz, .05: SOUND zz + 145, .05: SOUND zz + 80, .05: NEXT
END IF


IF FF(Z).onflag = 1 THEN
LINE (tempx1, tempy1)-(tempx2, tempy2), 50
IF sfx = 1 THEN FOR zz = 37 TO 800 STEP 5: SOUND zz, .05: SOUND zz + 145, .05: SOUND zz + 80, .05: NEXT
END IF

CASE 10
Z = QCH(t, z2)
IF q(Z).onflag = 0 THEN q(Z).onflag = 1 ELSE q(Z).onflag = 0

IF q(Z).onflag = 1 THEN
 DisLas Z, 0, 2
 IF sfx = 1 THEN FOR zz = 500 TO 100 STEP -50: SOUND zz, .1: SOUND 1100 - zz, .1: NEXT
END IF

IF q(Z).onflag = 0 THEN
 DisLas Z, 28, 2
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
LOCATE 1, 1: INPUT "Save number?", SN$
LOCATE 1, 1: PRINT "                "
1002 LOCATE 1, 1: LINE INPUT "Name?", n$
IF n$ = "*Nothing" THEN BEEP: LINE (0, 0)-(320, 9), 0, BF: GOTO 1002
SN = VAL(SN$)
n$ = LEFT$(n$, 8)
n$ = n$ + SPACE$(8 - LEN(n$))
OPEN "GFX\SM.gfx" FOR INPUT AS #1
OPEN "GFX\SM2.gfx" FOR OUTPUT AS #2
FOR Z = 0 TO 9
 INPUT #1, null, n2$, tlevel, thealth
 IF null = SN THEN
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

SUB SaveSavedData
DEF SEG = VARSEG(SavedData)

ON ERROR RESUME NEXT
BSAVE "game.dat", VARPTR(SavedData), LEN(SavedData)
DEF SEG
END SUB

SUB SetItems
Lives = 3
'ERASE obj
FOR Z = 1 TO 5: keys(Z) = 0: NEXT
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
FOR Z = 1 TO Lasers
rsl:
IF LASER(Z).onflag = 0 THEN GOTO nextlaser
temp = LASER(Z).dir
temp2 = LASER(Z).x
temp3 = LASER(Z).y

LASER(Z).dir = temp
IF LASER(Z).dir = 1 THEN GOTO L.UP
IF LASER(Z).dir = 2 THEN GOTO L.DN
IF LASER(Z).dir = 3 THEN GOTO L.LT
IF LASER(Z).dir = 4 THEN GOTO L.RT

nextlaser:
IF LASER(Z).shot = 0 THEN ShootLaser Z
IF LASER(Z).shot = 1 THEN UpdateBeam Z
NEXT
EXIT SUB

L.UP:
IF LASER(Z).y - 1 = LASER(Z).Y1 THEN LASER(Z).dir = 2: GOTO nextlaser
IF LASER(Z).y - 1 = LASER(Z).Y2 THEN LASER(Z).dir = 2: GOTO nextlaser
LINE (LASER(Z).x, LASER(Z).y)-(LASER(Z).x + 7, LASER(Z).y + 10), 0, BF
LASER(Z).y = LASER(Z).y - 1
IF LASER(Z).typ = 3 THEN PUT (LASER(Z).x, LASER(Z).y), LLT%
IF LASER(Z).typ = 4 THEN PUT (LASER(Z).x, LASER(Z).y), LRT%
GOTO nextlaser

L.DN:
IF LASER(Z).y + 1 = LASER(Z).Y1 THEN LASER(Z).dir = 1: GOTO nextlaser
IF LASER(Z).y + 1 = LASER(Z).Y2 THEN LASER(Z).dir = 1: GOTO nextlaser
LINE (LASER(Z).x, LASER(Z).y)-(LASER(Z).x + 7, LASER(Z).y + 10), 0, BF
LASER(Z).y = LASER(Z).y + 1
IF LASER(Z).typ = 3 THEN PUT (LASER(Z).x, LASER(Z).y), LLT%
IF LASER(Z).typ = 4 THEN PUT (LASER(Z).x, LASER(Z).y), LRT%
GOTO nextlaser

L.LT:
IF LASER(Z).x - 1 = LASER(Z).X1 THEN LASER(Z).dir = 4: GOTO nextlaser
IF LASER(Z).x - 1 = LASER(Z).X2 THEN LASER(Z).dir = 4: GOTO nextlaser
LINE (LASER(Z).x, LASER(Z).y)-(LASER(Z).x + 10, LASER(Z).y + 7), 0, BF
LASER(Z).x = LASER(Z).x - 1
IF LASER(Z).typ = 1 THEN PUT (LASER(Z).x, LASER(Z).y), LUP%
IF LASER(Z).typ = 2 THEN PUT (LASER(Z).x, LASER(Z).y), LDN%
GOTO nextlaser

L.RT:
IF LASER(Z).x + 1 = LASER(Z).X1 THEN LASER(Z).dir = 3: GOTO nextlaser
IF LASER(Z).x + 1 = LASER(Z).X2 THEN LASER(Z).dir = 3: GOTO nextlaser
LINE (LASER(Z).x, LASER(Z).y)-(LASER(Z).x + 10, LASER(Z).y + 7), 0, BF
LASER(Z).x = LASER(Z).x + 1
IF LASER(Z).typ = 1 THEN PUT (LASER(Z).x, LASER(Z).y), LUP%
IF LASER(Z).typ = 2 THEN PUT (LASER(Z).x, LASER(Z).y), LDN%
GOTO nextlaser

END SUB

SUB UpdateQBeam (t)

FOR Z = 1 TO 4
tx = qsx(Z, t)
ty = qsy(Z, t)
IF qs(Z, t) = 1 THEN GOTO mql
nq1:
qsx(Z, t) = tx
qsy(Z, t) = ty
NEXT

EXIT SUB
mql:
SELECT CASE Z
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
qs(Z, t) = 0
GOTO nq1

ebeam4:
LINE (tx + 4, ty)-(tx, ty), 0
qs(Z, t) = 0

GOTO nq1

END SUB

SUB UpdateQuad
FOR Z = 1 TO qdl
IF q(Z).onflag = 0 THEN GOTO nextquad
s1 = q(Z).stp
IF q(Z).dir = 1 THEN GOTO qup
IF q(Z).dir = 2 THEN GOTO qdn
IF q(Z).dir = 3 THEN GOTO qlt
IF q(Z).dir = 4 THEN GOTO qrt
nextquad:
ShootQuad Z
UpdateQBeam Z
NEXT
EXIT SUB

'*******
qup:
IF q(Z).y = qy(Z, s1) AND q(Z).x = qx(Z, s1) THEN GOTO changedir
LINE (q(Z).x, q(Z).y)-(q(Z).x + 5, q(Z).y + 5), 0, BF
q(Z).y = q(Z).y - 1
PUT (q(Z).x, q(Z).y), QUAD%
GOTO nextquad

qdn:
IF q(Z).y = qy(Z, s1) AND q(Z).x = qx(Z, s1) THEN GOTO changedir
LINE (q(Z).x, q(Z).y)-(q(Z).x + 5, q(Z).y + 5), 0, BF
q(Z).y = q(Z).y + 1
PUT (q(Z).x, q(Z).y), QUAD%
GOTO nextquad

qlt:
IF q(Z).x = qx(Z, s1) AND q(Z).y = qy(Z, s1) THEN GOTO changedir
LINE (q(Z).x, q(Z).y)-(q(Z).x + 5, q(Z).y + 5), 0, BF
q(Z).x = q(Z).x - 1
PUT (q(Z).x, q(Z).y), QUAD%
GOTO nextquad

qrt:
IF q(Z).x = qx(Z, s1) AND q(Z).y = qy(Z, s1) THEN GOTO changedir
LINE (q(Z).x, q(Z).y)-(q(Z).x + 5, q(Z).y + 5), 0, BF
q(Z).x = q(Z).x + 1
PUT (q(Z).x, q(Z).y), QUAD%
GOTO nextquad

changedir:
s2 = s1 + 1
IF s2 > q(Z).totstps THEN s2 = 2
IF qx(Z, s2) = q(Z).x AND qy(Z, s2) < q(Z).y THEN q(Z).dir = 1: GOTO nq2
IF qx(Z, s2) = q(Z).x AND qy(Z, s2) > q(Z).y THEN q(Z).dir = 2: GOTO nq2
IF qx(Z, s2) < q(Z).x AND qy(Z, s2) = q(Z).y THEN q(Z).dir = 3: GOTO nq2
IF qx(Z, s2) > q(Z).x AND qy(Z, s2) = q(Z).y THEN q(Z).dir = 4
nq2:
q(Z).stp = q(Z).stp + 1
IF q(Z).stp > q(Z).totstps THEN q(Z).stp = 2
GOTO nextquad
END SUB

SUB UpdateSprites
FOR Z = 1 TO 5
IF KeyX(Z) <> 0 AND KeyY(Z) <> 0 THEN
        PUT (KeyX(Z), KeyY(Z)), CB%, PSET
END IF
NEXT

IF msg = 0 AND health > 99 AND mf = 1 THEN

FOR Z = 1 TO 5
 IF keys(Z) = 1 THEN
  COLOR 2
  LOCATE 1, Z
  PRINT LTRIM$(RTRIM$(STR$(Z)))
 END IF
NEXT

mf = 0
END IF

IF TIMER - ffu >= 2 THEN
        FOR Z = 1 TO FFN
         IF FF(Z).onflag = 1 THEN LINE (FF(Z).X1, FF(Z).Y1)-(FF(Z).X2, FF(Z).Y2), 50
        NEXT
        ffu = TIMER
END IF

END SUB

SUB WaitForKey
KeyboardDisable
WaitForKeyOld
KeyboardEnable
END SUB

SUB WaitForKeyOld
WHILE INKEY$ <> "": WEND
WHILE INKEY$ = "": WEND
END SUB

SUB WaitVSync
WAIT &H3DA, 8
WAIT &H3DA, 8, 8
END SUB

