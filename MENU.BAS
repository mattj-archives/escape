DECLARE FUNCTION Menu.GetInpt! (te$)
DECLARE FUNCTION Menu.GetFName! ()
DECLARE SUB Menu.Special ()
DECLARE SUB Menu ()
DECLARE SUB FO (x, y, t$)
DECLARE FUNCTION GetInpt! (te$)
DECLARE FUNCTION GenInpt (te$)
DECLARE SUB Special ()
DECLARE FUNCTION GetFName! ()
DECLARE SUB PE ()

SCREEN 13

DIM SHARED MenuChoice%, MenuLevelWarp%

Menu


SUB FO (tx, ty, t$)
FOR Z = 16 TO 31
        LOCATE tx, ty: COLOR Z: PRINT t$
        t = TIMER: WHILE TIMER - t < .001: WEND
NEXT

END SUB

SUB Menu
MenuLevelWarp% = 1
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
GOSUB PutArrow            '*
'***************************
ST:
PUT (60, AY), AR%, XOR
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
IF M = 3 THEN Menu.Special: GOTO Main
IF M = 4 THEN GOTO ReadThis
IF M = 5 THEN GOTO Quit
'**********************************PLAY GAME
PlayGame:
COLOR 32
IF lwk <> 1 THEN MenuLevelWarp% = 1 ELSE INPUT "Level warp 1-34>>>"; MenuLevelWarp%
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
OPEN "QUIT.FLG" FOR OUTPUT AS #1
PRINT #1, "YES"
CLOSE
FOR Z = 31 TO 16 STEP -1
        LOCATE 1, 1: COLOR Z: PRINT "QUIT??!!!! TYPE Y OR N"
        t = TIMER: WHILE TIMER - t < .001: WEND
NEXT
SCREEN 0, 0, 0: WIDTH 80: COLOR 7, 0: CLS
SYSTEM

NQUIT:
FOR Z = 31 TO 16 STEP -1
        LOCATE 1, 1: COLOR Z: PRINT "QUIT??!!!! TYPE Y OR N"
        t = TIMER: WHILE TIMER - t < .001: WEND
NEXT
GOTO ST




PutArrow:
PSET (0, 2), 32: PSET (1, 2), 33: PSET (2, 2), 34: PSET (3, 2), 35
PSET (3, 1), 35: PSET (3, 3), 35: PSET (4, 2), 36
GET (0, 0)-(5, 5), AR%
RETURN

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
COLOR 2: LOCATE 9, 1: PRINT "Type 99 to exit"
COLOR 14: LOCATE 1, 1: PRINT "Special levels:": PRINT
OPEN "game.dat" FOR INPUT AS #1
INPUT #1, temp$
CLOSE
COLOR 32: PRINT "1-Practice level 1"
IF MID$(temp$, 2, 1) = "1" THEN LOCATE 4: COLOR 32: PRINT "2-Practice level 2"
IF MID$(temp$, 3, 1) = "1" THEN LOCATE 5: COLOR 32: PRINT "3-Practice level 3"
IF MID$(temp$, 4, 1) = "1" THEN LOCATE 6: COLOR 32: PRINT "4-The Unbeatable Level (31)"

IF MID$(temp$, 2, 1) = "0" THEN LOCATE 4: COLOR 8: PRINT "2-Practice level 2"
IF MID$(temp$, 3, 1) = "0" THEN LOCATE 5: COLOR 8: PRINT "3-Practice level 3"
IF MID$(temp$, 4, 1) = "0" THEN LOCATE 6: COLOR 8: PRINT "4-The Unbeatable Level (31)"

rsp:
ts = Menu.GetInpt("Select 1-4>")
IF ts = 99 THEN EXIT SUB
IF ts < 1 OR ts > 4 THEN BEEP: GOTO rsp

SELECT CASE ts
CASE 1
LEVELSTART = 32: LWW = 1

CASE 2
IF MID$(temp$, 2, 1) = "0" THEN
FO 20, 1, "This level cannot be accessed until"
FO 21, 1, "Practice level 1 is completed."
FO 22, 1, "Press a key...": WHILE INKEY$ = "": WEND
ELSE
LEVELSTART = 33: LWW = 1
END IF

CASE 3
IF MID$(temp$, 3, 1) = "0" THEN
FO 20, 1, "This level cannot be accessed until"
FO 21, 1, "Practice level 2 is completed."
FO 22, 1, "Press a key...": WHILE INKEY$ = "": WEND
ELSE
LEVELSTART = 34: LWW = 1
END IF

CASE 4
IF MID$(temp$, 4, 1) = "0" THEN
FO 20, 1, "You must complete all 30 levels, and the"
FO 21, 1, "practice levels to go here."
FO 22, 1, "Press a key...": WHILE INKEY$ = "": WEND
ELSE
LEVELSTART = 31: LWW = 1
END IF

END SELECT
LOCATE 20: PRINT STRING$(40, " "): LOCATE 21: PRINT STRING$(40, " "): LOCATE 22: PRINT STRING$(40, " ")

IF LWW = 1 THEN
OPEN "LW.CHK" FOR OUTPUT AS #1: PRINT #1, LEVELSTART: CLOSE
OPEN "PG.FLG" FOR OUTPUT AS #1
PRINT #1, "FLAG ON"
CLOSE
SYSTEM
END IF

GOTO rsp

END SUB

