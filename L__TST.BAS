DECLARE SUB SetScr ()
DECLARE SUB LGFX ()
DIM SHARED LUP%(45), LDN%(45), LLT%(45), LRT%(45)
DIM SHARED UP1%(33), UP2%(33), DN1%(33), DN2%(33)
DIM SHARED LT1%(33), LT2%(33), RT1%(33), RT2%(33)
DIM SHARED LIFE%(33), MC%(9), CBI1%(17), QUAD%(14), HEART%(14)
DIM SHARED CB%(19), LH%(4), LV%(4), PASS%(19)
SCREEN 13
LGFX
SetScr
'**********************************

WHILE INKEY$ = "": WEND

SUB LGFX
a = 1

OPEN "C:\escape\GFX\GFXLIST.LST" FOR INPUT AS #1
DO UNTIL a = 22
        INPUT #1, GFXFILE$, tx, ty
                IF GFXFILE$ = "" THEN GOTO SL
                MGFXFILE$ = "C:\escape\GFX\" + GFXFILE$ + ".GFX"
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

LINE (0, 0)-(25, 25), 0, BF
CLOSE #2
a = a + 1
LOOP
SL:
CLOSE #1
CLS

END SUB

SUB SetScr
SCREEN 13
LINE (0, 10)-(320, 10), 4
'Episode number is at 1,8
'Level number is at 1,10
'Lives is at 1,22
'                   0        1         2         3
'                   123456789012345678901234567890
LOCATE 1, 1: PRINT "Level E3L11   Lives: 99"

END SUB

