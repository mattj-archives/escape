DECLARE SUB epr ()
IF COMMAND$ <> "I1O1I1" THEN PRINT "Run ESCAPE.BAT": SYSTEM
SCREEN 13

FOR z = 1 TO 15
SIZE$ = STR$(z)
SIZE$ = LTRIM$(SIZE$)
XY1$ = "C2 S" + SIZE$
tx = 0: ty = 70
x = 160 - (z * 10): y = 100 - (z * 2)
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

ts = TIMER: WHILE TIMER - ts < .05: WEND
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
a$ = INKEY$: IF a$ <> "" THEN epr
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
a$ = INKEY$: IF a$ <> "" THEN epr
ts = TIMER: WHILE TIMER - ts < 2: WEND

tr = 0: tg = 0: tb = 1
FOR z = 41 TO 1 STEP -1
PALETTE 1, 65536 * z + 256 * 0 + 0
PALETTE 4, 65536 * 0 + 256 * 0 + z
a$ = INKEY$: IF a$ <> "" THEN epr
ts = TIMER: WHILE TIMER - ts < .01: WEND
NEXT

COLOR 4: LOCATE 20, 10: PRINT "By Matthew Johnson"

FOR z = 0 TO 63
PALETTE 4, 65536 * 0 + 256 * 0 + z
ts = TIMER: WHILE TIMER - ts < .01: WEND
a$ = INKEY$: IF a$ <> "" THEN epr
NEXT

ts = TIMER: WHILE TIMER - ts < 1: WEND

FOR z = 41 TO 0 STEP -1
PALETTE 2, 65536 * 0 + 256 * z + 0
ts = TIMER: WHILE TIMER - ts < .01: WEND
a$ = INKEY$: IF a$ <> "" THEN epr
NEXT

FOR z = 63 TO 0 STEP -1
PALETTE 4, 65536 * 0 + 256 * 0 + z
ts = TIMER: WHILE TIMER - ts < .01: WEND
a$ = INKEY$: IF a$ <> "" THEN epr
NEXT
CLS
COLOR 4: LOCATE 10, 10: PRINT "Escape... If you can..."
FOR z = 0 TO 63
PALETTE 4, 65536 * 0 + 256 * 0 + z
ts = TIMER: WHILE TIMER - ts < .01: WEND
a$ = INKEY$: IF a$ <> "" THEN epr
NEXT
ts = TIMER: WHILE TIMER - ts < 2: WEND
FOR z = 63 TO 0 STEP -1
PALETTE 4, 65536 * 0 + 256 * 0 + z
ts = TIMER: WHILE TIMER - ts < .01: WEND
a$ = INKEY$: IF a$ <> "" THEN epr
NEXT

SUB epr
CLS
SYSTEM
END SUB

