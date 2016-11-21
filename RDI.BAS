DECLARE SUB RI ()
DECLARE SUB UpdateL ()
DECLARE SUB UpdateINLogo ()
DECLARE SUB UpdateTitle ()
DIM SHARED TitleCDir, TitleC, TitleCT
DIM SHARED INL, INTIMER, I$: I$ = "I n s t r u c t i o n s"
DIM SHARED th: th = 70
DIM SHARED helpcol(th), help$(th)

FOR z = 1 TO th: READ helpcol(z), help$(z): NEXT
FOR z = 1 TO th
x$ = STRING$((80 - LEN(help$(z))), " ")
help$(z) = help$(z) + x$
NEXT

TitleCDir = 1: TitleC = 0: TitleCT = TIMER
INL = 1: INTIMER = TIMER
SCREEN 0, 0, 0
WIDTH 80
COLOR 1, 0: CLS
LOCATE 1

RI
SCREEN 0, 0, 0: WIDTH 80: COLOR 7, 0: CLS
SYSTEM
DATA 15,"PRESS SPACE TO EXIT "
DATA 10,"          Instructions to Escape"
DATA 14,"*****The object of the game "
DATA 0," "
DATA 9, "       The object of this game is to pass each level - sounds simple?"
DATA 9, "This game has no storyline. There are 30 main levels in this game,"
DATA 9, "and each usually involves walking about the screen, picking up"
DATA 9, "computer cards, shutting down force feilds, and leaving the level"
DATA 9, "without geting hit by the moving laser cannons along the walls."
DATA 0," "
DATA 14,"*****Moving"
DATA 0, " "
DATA 9, "       The only keys you'll only ever really need in this game are the"
DATA 9, "arrow keys. All you need to do is move around."
DATA 0, " "
DATA 14,"*****Laser cannons"
DATA 0, " "
DATA 9, "       The first thing you will meet in this game are the laser cannons.
DATA 9, "They are smart, and shoot at you if they detect you. If you are hit by"
DATA 9, "a laser cannon, you lose 10% of your health points."
DATA 0," "
DATA 14,"*****Quad laser cannons"
DATA 0," "
DATA 9, "       The red + shaped things you see in level 5+ can shoot in four
DATA 9, "directions. They are also smart, and they move on their own `tracks'"
DATA 9, "throughout the level."
DATA 0," "
DATA 14,"*****Health"
DATA 0," "
DATA 9,"        Your health is displayed at the top of the screen if it is"
DATA 9,"below 100% You will die if your health is below 1%. Then you will restart"
DATA 9,"the level, but you will still have the computer hards that you would have"
DATA 9,"already collected."
DATA 0," "
DATA 14,"*****Computer cards"
DATA 0," "
DATA 9,"        The computer cards have their own specific number. They must be"
DATA 9,"inserted in the right number of computer in order or work. They are"
DATA 9,"abbreaviated CC. To veiw the numbers of CCs you have, press + during the"
DATA 9,"game."
DATA 0," "
DATA 14,"*****Level codes"
DATA 0," "
DATA 9,"        When you start a level, you revieve a four letter code. When you"
DATA 9,"pass that level, you are asked to type it in. It MUST be in capital"
DATA 9,"letters. If you enter the incorrect code, your DEAD! You will restart"
DATA 9,"the level with a new code."
DATA 0," "
DATA 14,"*****Force feilds"
DATA 0," "
DATA 9,"        Don't touch the green lines!!! You'll lose 30% health!!!"
DATA 0," "
DATA 14,"*****Those grey boxes"
DATA 0," "
DATA 9,"        Grey boxes around a laser cannon or a quad laser mean it's"
DATA 9,"disabled. Computer terminals in the game can change that..."
DATA 0," "
DATA 14,"*****Practice levels"
DATA 0," "
DATA 9,"        Why I added these, I don't know..."
DATA 0," "
DATA 14,"*****Saving games"
DATA 0," "
DATA 9,"        Press F2 during a game, enter a number between 0 and 9, and then"
DATA 9,"an 8-letter word."
DATA 0," "
DATA 15,"*****Everything else"
DATA 9, " "
DATA 9,"        Find out for yourself!!!"
DATA 0, " "
'67

SUB RI
GOTO lnup
topline = 1
'VIEW PRINT 7 TO 25
ml:
a$ = INKEY$
IF a$ = CHR$(0) + "H" THEN GOTO lnup
IF a$ = CHR$(0) + "P" THEN GOTO lndn
IF a$ = " " THEN EXIT SUB
IF TIMER - TitleCT >= .2 THEN UpdateTitle
IF TIMER - INTIMER >= .5 THEN UpdateINLogo
GOTO ml

lnup:
topline = topline - 1
IF topline < 1 THEN topline = 1
temp = topline
FOR z = 9 TO 25
LOCATE z, 1: COLOR helpcol(temp): PRINT help$(temp);

IF help$(temp) = "" THEN
LOCATE z, 1: COLOR helpcol(temp): PRINT STRING$(80, " ");
END IF

temp = temp + 1
NEXT
GOTO ml

lndn:
IF topline + 1 + 18 >= 73 THEN topline = topline - 1: GOTO ml
topline = topline + 1
temp = topline
FOR z = 9 TO 25
LOCATE z, 1: COLOR helpcol(temp): PRINT help$(temp);
IF help$(temp) = "" THEN
LOCATE z, 1: COLOR helpcol(temp): PRINT STRING$(80, " ");
END IF

temp = temp + 1
NEXT
GOTO ml

END SUB

SUB UpdateINLogo
FOR z = 1 TO 24
 IF z = INL THEN LOCATE 8, 27 + z: COLOR 14: PRINT MID$(I$, z, 1)
 IF z <> INL THEN LOCATE 8, 27 + z: COLOR 9: PRINT MID$(I$, z, 1)
NEXT

INL = INL + 2
IF INL = 25 THEN INL = 1
INTIMER = TIMER
END SUB

SUB UpdateTitle
TitleCT = TIMER
IF TitleCDir = 1 THEN
 IF TitleC = 0 THEN TitleC = 8: GOTO nc
 IF TitleC = 8 THEN TitleC = 1: GOTO nc
 IF TitleC = 1 THEN TitleC = 9: GOTO nc
 IF TitleC = 9 THEN TitleC = 15: TitleCT = TIMER + 2
nc:
END IF

IF TitleCDir = 2 THEN
 IF TitleC = 15 THEN TitleC = 9: GOTO nc2
 IF TitleC = 9 THEN TitleC = 1: GOTO nc2
 IF TitleC = 1 THEN TitleC = 8: GOTO nc2
 IF TitleC = 8 THEN TitleC = 0
nc2:
END IF
IF TitleCDir = 1 AND TitleC = 15 THEN TitleCDir = 2
IF TitleCDir = 2 AND TitleC = 0 THEN TitleCDir = 1

LOCATE 1, 1: COLOR TitleC

PRINT "         EEEEEEE    SSSSS   CCCCCC   AAAAA    PPPPPP   EEEEEEE"
PRINT "         E         S       C        A     A   P     P  E"
PRINT "         E         S       C        A     A   P     P  E"
PRINT "         EEEEEEE    SSSS   C        A     A   PPPPPP   EEEEEEE"
PRINT "         E              S  C        AAAAAAA   P        E      "
PRINT "         E              S  C        A     A   P        E"
PRINT "         EEEEEEE   SSSSS    CCCCCC  A     A   P        EEEEEEE"

END SUB

