' QB64 - Meta-command that allows window to be resized
$Resize:Stretch

'MODULE 0 MAIN    ******CURSED CASTLE******9-2-90
'CHANGED GAMEDTA.BAS TO CURSAV.BAS ON 1-24-91
'CHANGED TURNNUMBER TO GYNTURN FOR GIANT TURNS 18FEB92
'CHANGED TIMER ON 2-3-94
'CHANGED SAVE GAME TIME$ DATE$ 7-30-95
'snips added 12-18-95  i/5
'REPORT ADDED 3-5-96  CURSDATA.TXT 10-24-96
'7-2-23, Henrik Aasman: Modified for QB64
'  Moved all definition of global (shared) variables to Main module
'  QB64 needs unique names so MOVEMENTTABLE --> MOVEMENTTABLESUB and INVENTORY --> INVENTORYSUB
'  Changed syntax for BEEP so BEEP 3 --> BEEP
'  Changed syntax for DELAY so DELAY --> _DELAY
'  Fixed __ to _
'  Renamed DEF to FUNCTION
'  Removed reading/writing to REPORT.DTA

' QB64 don't allow global variables to be defined inside subroutines,
' instead they are definied in the main module as shared and that then
' allows arrays to be redimensioned later inside the subroutines.
ReDim Shared ANSWER$(0)
ReDim Shared DESCRIPTION$(0)
ReDim Shared NUMBERROOMS
ReDim Shared DESCRIBEFLAG(0)
ReDim Shared ROOMDESCRIBE$(0)
ReDim Shared ROOMDES2$(0)
ReDim Shared ROOMOBJECT$(0, 0)
ReDim Shared MOVEMENTTABLE(0, 0)
ReDim Shared INVENTORY$(0)
ReDim Shared GAMESTATE$
ReDim Shared ROOM
ReDim Shared TURNNUMBER
ReDim Shared THING$
ReDim Shared FLAG(0)
ReDim Shared OBJ$(0)
ReDim Shared OBJ2$(0)
ReDim Shared GYNTURN
ReDim Shared VERB$
ReDim Shared OBJECT$
ReDim Shared C$
ReDim Shared DIRECTION
ReDim Shared ITEMNUMBER
ReDim Shared STRT

Cls: ReDim FLAG(30), OBJ$(30), OBJ2$(30)
STRT = Timer
Call LOGON
Locate 12, 12: Print "ONE MOMENT PLEASE...."
Call SETUP 'MODULE 1 SET UP VARIABLES
Call TURN 'MODULE 2 RUN TURNS
Call CLOSING 'MODULE 3 END OF GAME STUFF
End

'MODULE 0.1 LOGON
Sub LOGON
    Shared ANSWER$()
    ReDim ANSWER$(305)
    Input "YOUR NAME-NUMBER PLEASE"; ANSWER$(0)
    Let ANSWER$(0) = ANSWER$(0) + "  " + Time$ + "  " + Date$
End Sub


'MODULE 1 SETUP
Sub SETUP
    Call ROOMINFO 'MODULE 1.1 READ ROOM INFORMATION
    Call ROOMOBJECTS 'MODULE 1.2 READ ROOM OBJECTS
    Call MOVEMENTTABLESUB 'MODULE 1.3 READ MOVEMENT TABLE
    Call INVENTORYSUB 'MODULE 1.4 INITIALIZE INVENTORY ARRAY
    '             TO AN EMPTY LIST
    Call OTHERS 'MODULE 1.5 INITIALIZE OTHER VARIABLES
End Sub

'MODULE 1.1 ROOMINFO
Sub ROOMINFO
    Shared DESCRIPTION$(), NUMBERROOMS, DESCRIBEFLAG(), ROOMDESCRIBE$(), ROOMDES2$()
    Read NUMBERROOMS
    ReDim DESCRIPTION$(NUMBERROOMS), DESCRIBEFLAG(NUMBERROOMS), ROOMDESCRIBE$(NUMBERROOMS)
    ReDim ROOMDES2$(NUMBERROOMS)
    For I = 1 To NUMBERROOMS
        Read DESCRIPTION$(I)
    Next I
    Data 16,DUNGEON,STAIRCASE,ARMORY,STAIRCASE,WALK-IN FIREPLACE
    Data BANQUET HALL,THRONE ROOM,COURTYARD,GATEHOUSE,"BLACKSMITH'S"
    Data STABLES,STAIRCASE,DRAWING ROOM,BEDCHAMBER,BEDCHAMBER
    Data TOWER

    For I = 1 To NUMBERROOMS
        Let DESCRIBEFLAG(I) = 1
        Let ROOMDESCRIBE$(I) = "EMPTY"
        Let ROOMDES2$(I) = "EMPTY"
    Next I

End Sub

'MODULE 1.2 ROOM OBJECT INFORMATION
Sub ROOMOBJECTS
    Shared ROOMOBJECT$(), NUMBERROOMS
    ReDim ROOMOBJECT$(NUMBERROOMS, 15)
    For I = 1 To NUMBERROOMS
        For J = 1 To 15
            Let ROOMOBJECT$(I, J) = "EMPTY"
        Next J
    Next I
End Sub

'MODULE 1.3 MOVEMENTTABLE
Sub MOVEMENTTABLESUB
    Shared MOVEMENTTABLE(), NUMBERROOMS
    ReDim MOVEMENTTABLE(NUMBERROOMS, 6)
    For I = 1 To NUMBERROOMS
        For J = 1 To 6
            Read MOVEMENTTABLE(I, J)
        Next J
    Next I
    Data 0,0,2,0,0,0
    Data 0,3,0,1,4,0
    Data 2,0,0,0,0,0
    Data 0,6,0,0,12,2
    Data 0,0,6,0,0,0

    Data 4,7,8,5,0,0
    Data 6,0,0,0,0,0
    Data 11,10,9,6,0,0
    Data 0,0,0,8,0,0
    Data 8,0,0,0,0,0

    Data 0,8,0,0,0,0
    Data 14,15,0,13,16,4
    Data 0,0,12,0,0,0
    Data 0,12,0,0,0,0
    Data 12,0,0,0,0,0

    Data 0,0,0,0,0,12
End Sub

'MODULE 1.4 INVENTORY
Sub INVENTORYSUB
    Shared INVENTORY$()
    ReDim INVENTORY$(5)
    For I = 1 To 5
        Let INVENTORY$(I) = "EMPTY"
    Next I
End Sub

'MODULE 1.5 OTHERS
Sub OTHERS
    SHARED GAMESTATE$,ROOM,TURNNUMBER,ROOMOBJECT$(),THING$,INVENTORY$(),_
        FLAG(),OBJ$(),OBJ2$(),DESCRIBEFLAG(),ROOMDESCRIBE$(),ROOMDES2$()
    Let TURNNUMBER = 0
    Let THING$ = ""
    For I = 1 To 30: Let FLAG(I) = 0: Next I 'SET FLAGS TO 0
    'FLAG #  MEANING OF 0      FLAG# MEANING OF 0
    ' 1      GAME NOT OVER       2   FIRE NOT MADE
    'FOR 1, 1 MEANS GAME WON -1 GAME LOST 2 MEANS GAME QUIT

    ' 3 OATMEAL NOT MADE         4  THRONE NOT BROKEN
    ' 5 TYPE CARRYOVER FLAG      6  WATER NOT IN BUCKET
    ' 7 PUMP NOT OILED           8  GIANT NOT FLED
    ' 9 DUNGEON NOT DESCRIBED   10  SWORD PICKED UP
    '11 GOLD! NOT DUG           12 STABLES NOT DESCRIBED
    '13 COURTYARD NOT DESCRIBED 15 THRONE ROOM NOT DESCRIBED
    '14 NEED TO EXIT TURN (GIANT OR DOG IN STREET)
    For I = 1 To 20
        Read ROOM, J, ROOMOBJECT$(ROOM, J)
    Next I
    Data 1,1,EMPTYGOLD!,3,1,EMPTYSWORD,5,1,KETTLE*,5,2,EMPTYFIRE*
    Data 7,2,THRONE*,6,1,EMPTYGIANT*,7,1,SIGN*,8,1,PUMP*
    Data 9,1,EMPTYSHOVEL,9,2,EMPTYBUCKET,10,1,FLINT-STEEL
    Data 11,1,STRAW,11,2,EMPTYOATS,13,1,EMPTYHAIR-OIL
    Data 13,2,EMPTYPEARLS!,13,3,SIGN*,14,1,EMPTYRUBIES!
    Data 15,1,EMPTYEMERALDS!,16,1,EMPTYDIAMONDS!
    Data 16,2,AXE

    'THE STARTING ROOM IS THE BANQUET HALL - 6
    ROOM = 6


    For I = 1 To 20 'READ IN WORD!* CONVERSION
        Read OBJ$(I), OBJ2$(I)
    Next I
    Data SIGN,SIGN*,PEARL,PEARLS!,PEARLS,PEARLS!,OIL,HAIR-OIL
    Data HAIR,HAIR-OIL,GOLD,GOLD!,KETTLE,KETTLE*,FIRE,FIRE*
    Data GIANT,GIANT*,THRONE,THRONE*,PUMP,PUMP*,FLINT,FLINT-STEEL
    Data STEEL,FLINT-STEEL,RUBY,RUBIES!,RUBIES,RUBIES!
    Data DIAMOND,DIAMONDS!,DIAMONDS,DIAMONDS!
    Data EMERALD,EMERALDS!,EMERALDS,EMERALDS!
    Data PUMP,PUMP*
End Sub


'MODULE 2 TURN
Sub TURN
    Shared FLAG(), TURNNUMBER, GYNTURN
    Do Until FLAG(1) <> 0
        FLAG(14) = 0
        TURNNUMBER = TURNNUMBER + 1
        GYNTURN = GYNTURN + 1
        IF TURNNUMBER=295 THEN BEEP:_
           PRINT "Type   SAVE GAME  and then  QUIT":_
           PRINT "You can then return to this spot by typing":_
           PRINT "CURSED (at the DOS prompt) and later,":_
           PRINT "as a command, RESTORE GAME":_
           PRINT "Please forgive the necessary inconvenience."
        If TURNNUMBER = 300 Then FLAG(1) = 2
        Call DESCRIBE 'MODULE 2.1 DESCRIBE THE ROOM
        Call COMMANDS 'MODULE 2.2 INPUT THE COMMANDS
        Call EVALUATE 'MODULE 2.3 EVALUATE COMMANDS
    Loop
End Sub

'MODULE 2.1 DESCRIBE THE CURRENT ROOM
Sub DESCRIBE
    SHARED DESCRIPTION$(),ROOM,MOVEMENTTABLE(),ROOMOBJECT$(),FLAG(),_
        TURNNUMBER,INVENTORY$(),THING$,OBJECT$,DESCRIBEFLAG(),ROOMDESCRIBE$(),_
        ROOMDES2$(),GYNTURN

    If TURNNUMBER = 1 Then Call GAMESTARTER 'MODULE 2.1.1
    If DESCRIBEFLAG(ROOM) = 1 Then Print "You are in the  "; DESCRIPTION$(ROOM)
    IF DESCRIBEFLAG(ROOM)=0 THEN_
     PRINT ROOMDESCRIBE$(ROOM):PRINT ROOMDES2$(ROOM):DESCRIBEFLAG(ROOM)=1
    _Delay 0.3
    Print "The noticeable exits are: ";
    If MOVEMENTTABLE(ROOM, 1) <> 0 Then Print "NORTH ";
    If MOVEMENTTABLE(ROOM, 2) <> 0 Then Print "SOUTH ";
    If MOVEMENTTABLE(ROOM, 3) <> 0 Then Print "EAST ";
    If MOVEMENTTABLE(ROOM, 4) <> 0 Then Print "WEST ";
    If MOVEMENTTABLE(ROOM, 5) <> 0 Then Print "UP ";
    If MOVEMENTTABLE(ROOM, 6) <> 0 Then Print "DOWN ";
    Print
    _Delay 0.3
    Print "The noticeable objects are: ";
    COUNTER = 0
    For I = 1 To 15
        Let STUFF$ = ROOMOBJECT$(ROOM, I)
        IF LEFT$(STUFF$,5)<>"EMPTY" THEN PRINT "   ";ROOMOBJECT$(ROOM,I);_
          :COUNTER=COUNTER+1
    Next I
    If COUNTER = 0 Then Print "noticeably absent!";
    Print
    _Delay 0.3
    Print "You are carrying: ";
    COUNTER = 0
    For I = 1 To 5
                IF INVENTORY$(I)<>"EMPTY" THEN COUNTER=COUNTER + 1:_
                    PRINT INVENTORY$(I);"   ";
    Next I
    If COUNTER = 0 Then Print "nothing at all."
    Print
    _Delay 0.3

    'IN DUNGEON THE FIRST TIME
    IF ROOM=1 AND FLAG(9)=0 THEN FLAG(9)=1:_
      PRINT "The dungeon has the only dirt floor in the castle."
    'IN THRONE ROOM FOR THE FIRST TIME
   IF ROOM=7 AND FLAG(15)=0 THEN FLAG(15)=1:_
       PRINT "The throne is sturdy and made out of wood."
    'IN STABLES FOR THE FIRST TIME
   IF ROOM=11 AND FLAG(12)=0 THEN FLAG(12)=1:_
      PRINT "The straw here is as dry as tinder."
    'IN COURTYARD FOR THE FIRST TIME
   IF ROOM=8 AND FLAG(13)=0 THEN FLAG(13)=1:_
     PRINT "The rusty water pump needs to be oiled."
    '
    ' TEN TURNS UP AND GIANT RETURNS YOU TO BANQUET HALL
   IF GYNTURN=10 AND FLAG(3)=1 AND FLAG(8)=0 AND FLAG(10)=0 THEN _
     PRINT "The GIANT drags you back to the warm hall; says he hates cold food.":_
     GYNTURN=1:ROOM=6:FLAG(14)=1:EXIT SUB
    'GIANT DOESN'T GET YOU IF YOU HAVE THE SWORD- RESET TURNNUMBER TO 1
    If GYNTURN = 10 And FLAG(10) = 1 Then GYNTURN = 1

    'GIANT FLEES AT SIGHT OF SWORD
   IF ROOM=6 AND FLAG(10)=1 AND FLAG(8)=0 THEN FLAG(8)=1:_
     PRINT "The cowardly GIANT flees at the sight of the sword!":_
     ROOMOBJECT$(6,1)="EMPTY":FLAG(14)=1:ROOMOBJECT$(9,1)="SHOVEL":_
     EXIT SUB

    Print

End Sub

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
Sub GAMESTARTER
    Cls
    Print "          WELCOME TO CURSED CASTLE!"
    Print
    Print "    Come along on a creepy adventure to far-off places."
    Print " Battle GIANTS!  Find treasures!  MAKE OATMEAL!"
    Print
    Print "    While on a vacation to Scotland, you fall asleep "
    Print "reading about the human-eating GIANT that supposedly used"
    Print "to terrorize old castle towns."
    Print "    In your dream you find yourself in the banquet hall "
    Print "of an abandoned castle.  You are very cold and hungry."
    Print "If you do not build a fire soon, you will die!"
    Print
    Print "    Give commands as VERB then OBJECT, such as GO NORTH,"
    Print "READ SIGN, BUILD FIRE, OIL PUMP, DIG DIRT, CHOP"
    Print "THRONE, and so forth."
    Print
    Print "    Exceptions to this two-word sentence rule are single-"
    Print "letter commands such as N to GO NORTH (not GO N, just N),"
    Print "U to GO UP, and D to GO DOWN."
    Print "    P.S. Don't try to GET objects ending in an *, e.g., KETTLE*,"
    Print "as they are quite unobtainable.  Note also that things appear"
    Print "and disappear suddenly in dreams.  Keep you wits about you!"
    Print "If you're through taking notes, press the Enter key to begin"
    Do While Len(InKey$) = 0: Loop
    Cls
End Sub

'MODULE 2.1.2 ERASER ERASES ONE LINE OF TEXT
Sub ERASER
    Rem  FOR I=1 TO 12:PRINT "          ";:NEXT I
End Sub

'MODULE 2.2 COMMANDS
Sub COMMANDS
    SHARED VERB$, OBJECT$,ROOM,DESCRIPTION$(),FLAG(),INVENTORY$(),_
        ROOMOBJECT$(),OBJ$(),OBJ2$(),C$,ANSWER$(),TURNNUMBER

    If FLAG(1) <> 0 Then Exit Sub
    If FLAG(14) = 1 Then Exit Sub
    ' BE SURE THAT INPUT IS OK  DUMMY2=0
    DUMMY2 = 0
    Do Until DUMMY2 = 1
        'C$=THE INPUTTED COMMAND SENTENCE - MAY BE MORE THAN 2 WORDS
        'BUT ONLY THE FIRST TWO WORDS ARE ACCEPTED
        ' THE FIRST IS CALLED VERB$ AND THE SECOND OBJECT$
        Let C$ = ""
        Print: Print
        Print "*****************************************************************"
        Do Until C$ <> ""
            Input "COMMAND"; C$ 'GET THE RAW SENTENCE
        Loop

        Rem SPACES TAKE OFF LEADING AND PERHAPS TRAINING CHR$(32)'S
        Do While Right$(C$, 1) = Chr$(32)
            Let C$ = Left$(C$, Len(C$) - 1)
        Loop

        Let C$ = UCase$(C$)

        Let I = 1 'LETTER COUNTER

        Do While Not (Asc(Mid$(C$, I, 1)) = 32) And Not (I = Len(C$))
            'LOOP TO LOOK FOR FIRST SPACE
            Let I = I + 1 'WHICH SHOULD DENOTE END OF FIRST
        Loop 'WORD

        Let VERB$ = Mid$(C$, 1, I) 'ASSIGN THOSE CHARACTERS TO VERB$

        Let ANSWER$(TURNNUMBER) = C$

        Let OBJECT$ = Mid$(C$, I + 1, Len(C$)) 'ASSIGN REST OF SENTENCE TO OBJECT$


        If Len(OBJECT$) <> 0 Then VERB$ = Left$(VERB$, Len(VERB$) - 1)
        For J = 1 To 20 'CONVERT OBJECT$ LACKING * OR !
            If OBJECT$ = OBJ$(J) Then Let OBJECT$ = OBJ2$(J)
        Next J
        'CHECK FOR EXTRA WORDS
        COUNTER = 0
        For J = 1 To Len(OBJECT$)
            Let DUMMY$ = Mid$(OBJECT$, J, 1)
            If DUMMY$ = Chr$(32) Then COUNTER = COUNTER + 1
        Next J
    IF COUNTER<>0 THEN PRINT "One space (two words) only, please." _
      ELSE DUMMY2=1

    Loop
End Sub

'MODULE 2.3 EVALUATE THE COMMANDS
Sub EVALUATE
    SHARED VERB$, OBJECT$,ROOM,DIRECTION,MOVEMENTTABLE(),NUMBERROOMS,_
        ROOMOBJECT$(),ITEMNUMBER,TURNNUMBER,THING$,INVENTORY$(),FLAG(),_
        GYNTURN
    If FLAG(1) <> 0 Then Exit Sub
    If FLAG(14) = 1 Then Exit Sub
    Select Case VERB$
        Case "QUIT", "Q", "AWAKEN", "WAKE"
            Let FLAG(1) = 2 'GAME NOT WON(1) OR LOST(-1):EXIT SELECT
        Case "SAVE"
            If OBJECT$ <> "GAME" Then Print "SAVE GAME": Exit Select
            Open "CURSAV.BAS" For Output As #1

            Write #1, ROOM


            For I = 1 To 5
                Let DUMMY$ = ""
                For J = 1 To Len(INVENTORY$(I))
                    Let DUMMY$ = DUMMY$ + Chr$(Asc(Mid$(INVENTORY$(I), J, 1)) + 4)
                Next J
                Write #1, DUMMY$
            Next I
            For I = 1 To NUMBERROOMS
                For J = 1 To 15
                    Let DUMMY$ = ""
                    For K = 1 To Len(ROOMOBJECT$(I, J))
                        Let DUMMY$ = DUMMY$ + Chr$(Asc(Mid$(ROOMOBJECT$(I, J), K, 1)) + 4)
                    Next K
                    Write #1, DUMMY$
                Next J
            Next I
            For I = 1 To 30
                Write #1, FLAG(I)
            Next I
            Close #1

            Print "OK"
            Exit Select
        Case "RESTORE", "LOAD"
            If OBJECT$ <> "GAME" Then Print "RESTORE GAME": Exit Select
            Open "CURSAV.BAS" For Input As #1

            Input #1, ROOM

            For I = 1 To 5
                Input #1, DUMMY$
                Let INVENTORY$(I) = ""
                For J = 1 To Len(DUMMY$)
                    Let INVENTORY$(I) = INVENTORY$(I) + Chr$(Asc(Mid$(DUMMY$, J, 1)) - 4)
                Next J
            Next I
            For I = 1 To NUMBERROOMS
                For J = 1 To 15
                    Input #1, DUMMY$
                    Let ROOMOBJECT$(I, J) = ""
                    For K = 1 To Len(DUMMY$)
                        Let ROOMOBJECT$(I, J) = ROOMOBJECT$(I, J) + Chr$(Asc(Mid$(DUMMY$, K, 1)) - 4)
                    Next K
                Next J
            Next I


            For I = 1 To 30
                Input #1, FLAG(I)
            Next I
            Close #1
            Print "OK"
            Exit Select

        Case "INVENTORY", "I"
            COUNTER = 0
            Print "   YOU CARRY: ";
            For I = 1 To 5
                IF INVENTORY$(I)<>"EMPTY" THEN COUNTER=COUNTER + 1:_
                    PRINT INVENTORY$(I);"   ";
            Next I
            If COUNTER = 0 Then Print "NOTHING AT ALL": Exit Select
            Print: Exit Select

        Case "GO", "MOVE", "N", "S", "E", "W", "U", "D"
            If Len(VERB$) = 1 Then OBJECT$ = VERB$
            If OBJECT$ = "NORTH" Or OBJECT$ = "N" Then DIRECTION = 1
            If OBJECT$ = "SOUTH" Or OBJECT$ = "S" Then DIRECTION = 2
            If OBJECT$ = "EAST" Or OBJECT$ = "E" Then DIRECTION = 3
            If OBJECT$ = "WEST" Or OBJECT$ = "W" Then DIRECTION = 4
            If OBJECT$ = "UP" Or OBJECT$ = "U" Then DIRECTION = 5
            If OBJECT$ = "DOWN" Or OBJECT$ = "D" Then DIRECTION = 6


            IF MOVEMENTTABLE (ROOM,DIRECTION) = 0 THEN _
                PRINT "You can't go that way" ELSE _
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
            Exit Select

        Case "GET", "TAKE", "PLUCK", "EXTRACT", "PULL"

            IF ROOM=7 AND RIGHT$(OBJECT$,1)="!" THEN _
              PRINT "Best leave the treasures here, love":EXIT SELECT

            THING$ = "BUCKET"
            IF FNCARRY=0 AND OBJECT$="WATER" THEN _
              PRINT "GET the BUCKET":EXIT SELECT
            IF OBJECT$="WATER" AND FLAG(7)=0 THEN _
              PRINT "OIL the PUMP first.  It's rusty.":EXIT SELECT
            IF OBJECT$="WATER" AND ROOM <> 8 THEN _
              PRINT "GO to the courtyard":EXIT SELECT
            IF OBJECT$="WATER" THEN FLAG(6)=1:_
              PRINT "The BUCKET is full of nice WATER.":EXIT SELECT

            If FNPRESENT = 0 Then Print "I don't see  "; OBJECT$: Exit Select
            If Right$(OBJECT$, 1) = "*" Then Print "I can't handle it": Exit Select

            If OBJECT$ = "SWORD" Then FLAG(10) = 1

            For I = 1 To 5
                IF INVENTORY$(I)="EMPTY" THEN INVENTORY$(I)=OBJECT$:_
                    PRINT "Got it.":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY":_
                    EXIT SELECT
            Next I

            If OBJECT$ = "SWORD" Then FLAG(10) = 0

            Print "You're carrying too much.  DROP something.": Exit Select
        Case "PUT", "DROP", "GIVE"

            THING$ = OBJECT$
            If FNCARRY = 0 Then Print "You don't have the "; OBJECT$: Exit Select

            If OBJECT$ = "SWORD" Then Print "You'll need the SWORD.": Exit Select

            For J = 1 To 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":_
                    EXIT SELECT
            Next J
            Print "This room is full, take it elsewhere": Exit Select


        Case "READ"
            IF OBJECT$="SIGN*" AND ROOM=7 THEN _
               PRINT "It says - DROP TREASURES! such as GOLD!":_
               PRINT "here and type SCORE":EXIT SELECT
            IF OBJECT$="SIGN*"AND ROOM=13 THEN _
               PRINT "Recipe for OATMEAL -> DROP OATS and water BUCKET":_
               PRINT "at kettle and MAKE OATMEAL":EXIT SELECT

            Print "Try READ SIGN* ": Exit Select

        Case "OIL", "LUBRICATE", "GREASE", "HAIR-OIL"
            THING$ = "HAIR-OIL"
            If FNCARRY = 0 Then Print "You don't have the HAIR-OIL": Exit Select
           IF ROOM=8 THEN PRINT "Now you can GET the WATER.":_
              FLAG(7)=1:EXIT SELECT
            Print "You are in the wrong place": Exit Select
        Case "DRINK"
            Print "You are not thirsty": Exit Select
        Case "EAT"
            If FLAG(3) = 0 Then Print "MAKE OATMEAL first": Exit Select
            Print "You are in the wrong place": Exit Select
        Case "BREAK", "CHOP", "CUT"
          IF FLAG(4)=1 THEN _
             PRINT "The only thing worth breaking was the THRONE":EXIT SELECT
            If ROOM <> 7 Then Print "GO to the throne room": Exit Select
            THING$ = "AXE"
            If FNCARRY = 0 Then Print "GET the AXE": Exit Select
          PRINT "Whew! Now you have some WOOD for a fire":_
             ROOMOBJECT$(7,2)="WOOD":FLAG(4)=1:_
             INVENTORY$(ITEMNUMBER)="EMPTY":EXIT SELECT
        Case "STRIKE"
            Print "Try -   MAKE FIRE ": Exit Select
        Case "FILL"
            Print "Try -   GET WATER": Exit Select
        Case "PUMP"
            Print "Try -   GET WATER ": Exit Select
        Case "SCORE"
            If ROOM <> 7 Then Print "Return to the THRONE ROOM": Exit Select
            SCORE = 0
            For I = 1 To 10
                Let STUFF$ = ROOMOBJECT$(7, I)
                If Right$(STUFF$, 1) = "!" Then SCORE = SCORE + 20
            Next I
            Print "Your SCORE is "; SCORE
         IF SCORE=100 THEN PRINT "YOU WIN!!!!!  FANTASTIC JOB!!!":_
           FLAG(1)=1:EXIT SELECT
            Print (100 - SCORE); " points to go!": Exit Select

        Case "DIG", "SHOVEL"
            THING$ = "SHOVEL"
            If FNCARRY = 0 Then Print "GET the SHOVEL": Exit Select
            If ROOM <> 1 Then Print "The floor is solid rock here": Exit Select
            If FLAG(11) = 1 Then Print "Nothing else to DIG": Exit Select
         PRINT "YOU HIT SOMETHING!":ROOMOBJECT$(1,1)="GOLD!":_
           FLAG(11)=1:EXIT SELECT
        Case "KILL", "MURDER", "SLAY"
            Print "GET a suitable weapon": Exit Select
        Case "MAKE", "BUILD", "PREPARE", "COOK"
            If ROOM <> 5 Then Print "GO to the FIREPLACE": Exit Select
            'MAKE FIRE OPTIONS
            If OBJECT$ <> "FIRE*" Then GoTo JUMPOVER
            If FLAG(2) = 1 Then Print "There is a fire!": Exit Select
           OBJECT$="FLINT-STEEL":IF FNPRESENT=0 THEN _
             PRINT "DROP FLINT-STEEL here":EXIT SELECT
           OBJECT$="STRAW":IF FNPRESENT=0 THEN _
             PRINT "DROP STRAW here":EXIT SELECT
           OBJECT$="WOOD":IF FNPRESENT=0 THEN _
             PRINT "DROP WOOD  here":EXIT SELECT
           FLAG(2)=1:PRINT "You now have a lovely, warm fire":_
             ROOMOBJECT$(5,2)="FIRE*":ROOMOBJECT$(9,2)="BUCKET":_
             ROOMOBJECT$(11,2)="OATS":ROOMOBJECT$(13,1)="HAIR-OIL":_
             FLAG(5)=1
             OBJECT$="FLINT-STEEL":IF FLAG(5)=1 AND FNPRESENT=1 THEN _
               ROOMOBJECT$(5,ITEMNUMBER)="EMPTY"
             OBJECT$="STRAW":IF FLAG(5)=1 AND FNPRESENT=1 THEN _
               ROOMOBJECT$(5,ITEMNUMBER)="EMPTY"
             OBJECT$="WOOD":IF FLAG(5)=1 AND FNPRESENT=1 THEN _
               ROOMOBJECT$(5,ITEMNUMBER)="EMPTY"
            If FLAG(5) = 1 Then FLAG(5) = 0: Exit Select

            'JUMPOVER TO MAKE OATMEAL OPTIONS
            JUMPOVER:
          IF LEFT$(OBJECT$,3)<>"OAT" AND OBJECT$<>"FOOD" THEN _
            PRINT "Try - BUILD FIRE or MAKE OATMEAL":EXIT SELECT
            If FLAG(2) = 0 Then Print "BUILD FIRE first": Exit Select
            If FLAG(3) = 1 Then Print "Once is enough!": Exit Select
         IF FLAG(6)=0 THEN PRINT "GET WATER in the bucket first":_
           EXIT SELECT
         OBJECT$="BUCKET":IF FNPRESENT=0 THEN _
           PRINT "DROP BUCKET  here first":EXIT SELECT
         OBJECT$="OATS":IF FNPRESENT=0 THEN _
           PRINT "DROP OATS  here first":EXIT SELECT
         FLAG(3)=1:PRINT "The oatmeal was filling.  Just then, a GIANT arrives,":_
           PRINT "eats the remaining OATMEAL, pokes you gently in the stomach":_
           PRINT "and wanders into the HALL.":ROOMOBJECT$(6,1)="GIANT*":_
           GYNTURN=1:FLAG(5)=1
           IF FLAG(5)=1 THEN  ROOMOBJECT$_
             (13,2)="PEARLS!":ROOMOBJECT$(16,1)="DIAMONDS!":_
             ROOMOBJECT$(14,1)="RUBIES!":ROOMOBJECT$(15,1)="EMERALDS!":_
             ROOMOBJECT$(3,1)="SWORD"
            If FLAG(5) = 1 Then FLAG(5) = 0: Exit Select
        Case "USE"
            Print "To do what?  Try something more specific."
            Exit Select
        Case Else
            Print "I don't know that VERB": Exit Select
    End Select
End Sub

'MODULE 2.3.1 DEFINE THE FUNCTION- IS THE OBJECT PRESENT?
Function FNPRESENT
    Shared ROOMOBJECT$(), OBJECT$, ITEMNUMBER
    For J = 1 To 15
             IF ROOMOBJECT$(ROOM,J)=OBJECT$ THEN _
                 FNPRESENT=1:ITEMNUMBER=J:EXIT Function
    Next J
    FNPRESENT = 0
End Function

'MODULE 2.3.2 DEFINE THE FUNCTION- IS IT BEING CARRIED?
Function FNCARRY
    Shared INVENTORY$(), THING$, ITEMNUMBER
    For I = 1 To 5
        If INVENTORY$(I) = THING$ Then FNCARRY = 1: ITEMNUMBER = I: Exit Function
    Next I
    FNCARRY = 0
End Function

'MODULE 2.4 UPDATE DATA
Sub UPDATE
End Sub

'MODULE 3 CLOSING
Sub CLOSING
    Shared FLAG(), TURNNUMBER, ANSWER$(), STRT
    Print
    If FLAG(1) = 1 Then Print " *****   HOORAY FOR YOU!!  YOU'VE WON THE GAME!!  ****"
    Print
    For I = 1 To 6: Print: Next I
    Input "BE SURE THAT YOUR DISK IS IN THE DRIVE AND PRESS ENTER"; DUMMY$

    '    Dim DTA(40), DTA$(10)
    '    Open "REPORT.DTA" For Input As #1
    '    Rem RETRIEVES OLD DATA FROM REPORT.DTA
    '    For I = 1 To 40
    '        Input #1, DTA(I)
    '    Next I
    '    For I = 1 To 10
    '        Input #1, DTA$(I)
    '    Next I
    '    Close #1

    '    Let DTA(8) = FLAG(1): Let DTA(33) = DTA(33) + Int((Timer - STRT) / 6)
    '    Open "REPORT.DTA" For Output As #1
    '    Rem SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
    '    For I = 1 To 40
    '        Write #1, DTA(I)
    '    Next I
    '    For I = 1 To 10
    '        Write #1, DTA$(I)
    '    Next I
    '    Close #1

    Open "CURSDATA.TXT" For Append As #2
    Print #2, Time$
    For I = 0 To TURNNUMBER
        If Int(I / 5) = I / 5 Then Print #2, ANSWER$(I) Else Print #2, ANSWER$(I),
    Next I
    Print #2, Int((STRT - Timer) / 6), Int(1000 * Rnd(0))
    Close #2
    Print "This game is over.  Type CURSED  to play again."
End Sub

'END OF PROGRAM
