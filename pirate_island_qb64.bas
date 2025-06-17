' QB64 - Meta-command that allows window to be resized
$Resize:Stretch

'MODULE 0 MAIN  ***PIRATE 8-31-90***
'CHANGED GAMEDTA.BAS TO PIRESAV.BAS ON 1-24-91
'GO DOWNSTAIRS FROM MAIN DECK 6MAR92
'CHANGED CLOCK TIMER STRT 2-3-94
'CHANGED SAVE GAME DATE$ TIME$ 7-30-95
'snips added 1-2-96
'PIRTDATA.TXT ADDED AND REPORT.DTA CHECKED 10-24-96
'6-30-23, Henrik Aasman: Modified for QB64
'  Moved all definition of global (shared) variables to Main module
'  QB64 needs unique names so MOVEMENTTABLE --> MOVEMENTTABLESUB and INVENTORY --> INVENTORYSUB
'  Changed syntax for BEEP so BEEP 3 --> BEEP
'  Changed syntax for DELAY so DELAY --> _DELAY
'  Fixed __ to _
'  Renamed DEF to FUNCTION
'  Added NUMBERCONVERTS to SHARED and used it in the REVEAL-action
'  Removed reading/writing to REPORT.DTA
'  Added "" around some of the data in DATA-tables

' QB64 don't allow global variables to be defined inside subroutines,
' instead they are definied in the main module as shared and that then
' allows arrays to be redimensioned later inside the subroutines.
ReDim Shared ANSWER$(0)
ReDim Shared DESCRIPTION$(0)
ReDim Shared DESCRIBEFLAG(0)
ReDim Shared ROOMDESCRIBE$(0)
ReDim Shared ROOMDES2$(0)
ReDim Shared ROOMOBJECT$(0, 0)
ReDim Shared MOVEMENTTABLE(0, 0)
ReDim Shared INVENTORY$(0)
ReDim Shared FLAG(0)
ReDim Shared OBJ$(0)
ReDim Shared OBJ2$(0)
ReDim Shared VERB$
ReDim Shared OBJECT$
ReDim Shared NUMBERROOMS
ReDim Shared ROOM
ReDim Shared TURNNUMBER
ReDim Shared THING$
ReDim Shared C$
ReDim Shared DIRECTION
ReDim Shared ITEMNUMBER
ReDim Shared STRT
ReDim Shared NUMBERCONVERTS

STRT = Timer
Cls
Call LOGON
Locate 12, 12: Print "ONE MOMENT PLEASE . . . ."
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
    Data 21,FOOD LOCKER,"SHIP'S LAUNDRY",MAIN DECK,CODE ROOM,"OPEN SEA"
    Data ARMORY,SANDY BEACH,ROCKY BEACH,TIDE POOL,UPPER BEACH
    Data WOODS,HILLSIDE,HILLTOP,"PIRATE'S FORT",HUT,CAMPFIRE AREA,STOCKADE
    Data PASSAGEWAY,SICK BAY,BRIG,TOOL ROOM

    For I = 1 To NUMBERROOMS
        Let DESCRIBEFLAG(I) = 0
    Next I
    For I = 1 To NUMBERROOMS
        Read ROOMDESCRIBE$(I)
        Read ROOMDES2$(I)
    Next I
    Data "You are in the ship's FOOD LOCKER.  There is some fruit"
    Data "here, but you will have to  REVEAL  it by name."
    Data "As you might expect, the laundry room is a mess.  The "
    Data "PIRATES are lucky their moms aren't here to see it."
    Data "From the deck of the  BASE CANARD,  you can see "
    Data "PIRATE ISLAND a short distance to the NORTH."
    Data "Here in the ship's CODE ROOM, secret coded messages"
    Data "are enciphered and deciphered."
    Data "The SEA WATER isn't cold and you can see a SANDY"
    Data "BEACH just to the NORTH."

    Data "Here in the ARMORY is where the PIRATES keep their weapons."
    Data "It looks as if they've taken all but one of them away."
    Data "You are on the BEACH of a beautiful, tropical island."
    Data "The warm sun quickly dries your clothes."
    Data "In this part of the BEACH, waves crash over the rocks."
    Data "For a moment, you forget the danger you are in."
    Data "There are FISH in the TIDE POOL."
    Data "You'll need to try your hand at spear-fishing."
    Data "Here at the UPPER BEACH, The sand is firmer."
    Data "To the EAST you can see WOODS."

    Data "The WOODED AREA you are in is quite small."
    Data "The trees are tall, with long, straight limbs."
    Data "You are on the side of the island's one hill."
    Data "The path is steep and hasn't been used much."
    Data "From the top of the hill, you see the ship to the SOUTH.  Your"
    Data "attention, however, is focused on the poor fellow before you."
    Data "The gate of the PIRATE'S FORT is wide open.  You sense "
    Data "danger and hope that you are dressed appropriately."
    Data "The PIRATE'S rude HUT is also messy."
    Data "The PIRATES need to learn more self-discipline."
    Data "The fearsome-looking PIRATES are standing around the "
    Data "COALS of a once-roaring campfire.  BE CAREFUL!"

    Data "The prisoners locked inside the GATE are glad to see you at first."
    Data "But when you don't  UNLOCK the GATE, they move to the far side."
    Data "You are in the dark, dank ship's PASSAGEWAY."
    Data "You can see light at the top of the stairs."
    Data "Here in the SICK BAY, you can find something to take for the"
    Data "malarial fever you picked up in the tropics."
    Data "You find yourself tied up with ropes in the BRIG of a PIRATE"
    Data "SHIP.  The foul odor suggests deferred maintenance."
    Data "The TOOL ROOM is empty except for one tool left behind.  You"
    Data "will have to unscramble the name of the tool and  REVEAL  it."


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
    Data 3,0,0,0,0,0,0,0,3,0,0,0
    Data 5,1,4,2,0,18,0,0,0,3,0,0
    Data 7,3,0,0,0,0,0,18,0,0,0,0
    Data 10,5,8,0,0,0,11,0,9,7,0,0
    Data 0,0,0,8,0,0,14,7,11,0,0,0
    Data 0,8,0,10,12,0,0,0,0,0,13,11
    Data 0,0,0,0,0,12,17,10,15,16,0,0
    Data 0,0,0,14,0,0,0,0,14,0,0,0
    Data 0,14,0,0,0,0
    Data 6,21,19,20,3,0,0,0,0,18,0,0
    Data 0,0,18,0,0,0,18,0,0,0,0,0
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
        FLAG(),OBJ$(),OBJ2$(),NUMBERCONVERTS
    Let ROOM = 20 'START IN THE BRIG- ROOM 20
    Let TURNNUMBER = 0
    Let THING$ = ""
    ReDim FLAG(30)
    For I = 1 To 10: Let FLAG(I) = 0: Next I 'SET FLAGS TO 0
    'FLAG #  MEANING OF 0      FLAG# MEANING OF 0
    ' 1      GAME NOT OVER       2   CLOTHES NOT WORN
    ' 3      SPEAR NOT CARRIED   4   BRANCH NOT SHARPENED
    ' 5      FISH NOT COOKED     6   HERMIT NOT FED
    ' 7      HERMIT NOT WELL     8   HERMIT NOT MOVED
    ' 9     HOW OFTEN IN ROOM16 10   QUININE NOT TAKEN
    '11     RAT NOT REVEALED    12   BRANCH NOT REVEALED
    '13     KEY NOT REVEALED
    '14   IS USED TO EXIT TURN BUT NOT GAME- FOR HINTS AND EXCEPTIONS
    '15   SHOVEL NOT REVEALED   16   SWORD NOT REVEALED
    '17   CLOTHES NOT REVEALED  18   QUININE NOT REVEALED
    '19   PARROT NOT NAMED      20   ORANGES NOT REVEALED
    '21   KEY NOT REVEALED

    Let ROOMOBJECT$(1, 1) = "SOMETHING*" 'ORANGES
    Let ROOMOBJECT$(1, 2) = "SIGN*"
    Let ROOMOBJECT$(2, 1) = "SOMETHING*" 'CLOTHES
    Let ROOMOBJECT$(2, 2) = "SIGN*"
    Let ROOMOBJECT$(3, 1) = "CANNON*" 'STARTING LOCATIONS FOR OBJECTS
    Let ROOMOBJECT$(4, 1) = "SIGN*"
    Let ROOMOBJECT$(6, 1) = "SOMETHING*" 'SWORD
    Let ROOMOBJECT$(6, 2) = "SIGN*"
    Let ROOMOBJECT$(9, 1) = "FISH"
    Let ROOMOBJECT$(11, 1) = "CNBAHR*" 'BRANCH
    Let ROOMOBJECT$(13, 1) = "A SICK AND HUNGRY HERMIT*"
    Let ROOMOBJECT$(15, 1) = "GUNBOX*"
    Let ROOMOBJECT$(15, 2) = "EKY*" 'KEY
    Let ROOMOBJECT$(16, 1) = "PIRATES*"
    Let ROOMOBJECT$(16, 2) = "COALS"
    Let ROOMOBJECT$(17, 1) = "PRISONERS*"
    Let ROOMOBJECT$(17, 2) = "LOCK*"
    Let ROOMOBJECT$(18, 1) = "SIGN*"
    Let ROOMOBJECT$(19, 1) = "SOMETHING*" 'QUININE
    Let ROOMOBJECT$(19, 2) = "SIGN*"
    Let ROOMOBJECT$(20, 1) = "SOMETHING*" 'RAT
    Let ROOMOBJECT$(20, 2) = "SIGN*"
    Let ROOMOBJECT$(21, 1) = "VLHSOE*" 'SHOVEL

    Read NUMBERCONVERTS
    ReDim OBJ$(NUMBERCONVERTS), OBJ2$(NUMBERCONVERTS)
    For I = 1 To NUMBERCONVERTS 'READ IN WORD!* CONVERSIONS
        Read OBJ$(I), OBJ2$(I)
    Next I
    Data 12,SIGN,SIGN*,GUNBOX,GUNBOX*,BOX,GUNBOX*,PIRATES,PIRATES*
    Data PIRATE,PIRATES*,PRISONER,PRISONERS*,PRISONERS,PRISONERS*
    Data "LOCK","LOCK*",RAT,RAT*,ORANGE,ORANGES,SOMETHING,SOMETHING*
    Data CANNON,CANNON*
End Sub

'MODULE 2 TURN
Sub TURN
    Shared FLAG(), TURNNUMBER
    Do Until FLAG(1) <> 0
        FLAG(14) = 0
        TURNNUMBER = TURNNUMBER + 1
        IF TURNNUMBER=295 THEN BEEP:_
           PRINT "Type   SAVE GAME  and then  QUIT":_
           PRINT "You can then return to this spot by typing":_
           PRINT "PIRATE (at the DOS prompt) and later,":_
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
        ROOMDES2$()

    If TURNNUMBER = 1 Then Call GAMESTARTER 'MODULE 2.1.1
    Select Case DESCRIBEFLAG(ROOM)
        Case 0
            Print ROOMDESCRIBE$(ROOM)
            Print ROOMDES2$(ROOM)
            DESCRIBEFLAG(ROOM) = 1: Exit Select
        Case 1
            Select Case DESCRIPTION$(ROOM)
                Case "MAIN DECK", "SANDY BEACH", "ROCKY BEACH", "UPPER BEACH"
                    Print "You find yourself on the "; DESCRIPTION$(ROOM)
        CASE "OPEN SEA","BRIG","FOOD LOCKER","CODE ROOM","WOODS","HUT",_
            "PASSAGEWAY","SICK BAY","TOOL ROOM"
                    Print "You find yourself in the "; DESCRIPTION$(ROOM)
                Case Else
                    Print "You find yourself at the "; DESCRIPTION$(ROOM)
            End Select
    End Select
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
   IF TURNNUMBER=8 AND FLAG(11)=0 THEN _
      PRINT "A cat strolls by hunting for something."
   IF TURNNUMBER=30 AND FLAG(18)=0 THEN _
      PRINT "A PARROT squawks,  TAKE QUININE ! "
   IF TURNNUMBER=60 AND FLAG(2)=0 THEN _
      PRINT "The PARROT squawks, change your outfit!'"
   IF TURNNUMBER=80 AND FLAG(19)=0 THEN _
      PRINT "The PARROT squawks,  If A is 1, what is B?'"
   IF TURNNUMBER=100 AND FLAG(4)=0 THEN _
      PRINT "A PARROT squawks,  SHARPEN BRANCH, Matey.'"

    IF FLAG(9)=2 AND ROOM = 16 AND FLAG(8)=0 AND FLAG(2)= 0 THEN_
       PRINT "The PIRATES notice your CLOTHES and recapture you.":_
       FLAG(14)=1:ROOM=20:EXIT SUB
    IF FLAG(9)=0 AND ROOM = 16 AND FLAG(2)= 0 THEN _
       PRINT "The PIRATES eye you suspiciously - You'd better leave":FLAG(14)=1:_
       FLAG(9)=1:EXIT SUB
    If FLAG(9) = 1 And ROOM = 16 Then FLAG(9) = 2
    IF ROOM=3 AND FLAG(10)=0 THEN ROOM=18:CLS:PRINT:PRINT:PRINT: _
      PRINT "You are too weak with malaria to climb the stairs.":_
      FLAG(14)=1:DESCRIBEFLAG(3)=0:EXIT SUB
   IF ROOM=18 AND FLAG(11)=0 THEN ROOM=20:CLS:PRINT:PRINT:PRINT: _
      PRINT "You can't move very well while tied up.":FLAG(14)=1:_
      DESCRIBEFLAG(18)=0:EXIT SUB



End Sub

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
Sub GAMESTARTER
    Cls
    Print
    Print "                WELCOME TO PIRATE ISLAND"
    Print
    Print "Are you ready for this: PIRATES, danger, secret codes,"
    Print "spear-fishing?  Good!  I thought you would be!"
    Print "Give COMMANDS as VERB then OBJECT, such as GO NORTH,"
    Print "SAVE GAME, RESTORE GAME, READ SIGN, TAKE FISH,"
    Print "AND REVEAL APPLES."
    Print "Exceptions to this two-word sentence rule are single-letter"
    Print "COMMANDS such as N to GO NORTH (not GO N, just N), U to GO UP,"
    Print "and D to GO DOWN."
    Print "Don't try to GET OBJECTS ending in an *, e.g., CANNON*,"
    Print "as they are quite unobtainable.  Sometimes the names of"
    Print "OBJECTS will be scrambled, e.g., APPLES may appear as "
    Print "PELSPA*.   To GET the APPLES, type REVEAL APPLES and then"
    Print "TYPE  GET APPLES.  Sometimes the presence of an object "
    Print "will be indicated by SOMETHING* and you must guess and"
    Print "REVEAL what it is before you can GET it.  For the word "
    Print "That 'starts with a Q and ends with an E', You may"
    Print "need to consult a dictionary."
    Print "If you get stuck, keep trying different things because"
    Print "after a certain number of turns, a hint will show up."
    Print "Happy Adventuring!   Press ENTER to begin."
    Do While Len(InKey$) = 0: Loop
    Cls
End Sub

'MODULE 2.1.2 ERASER ERASES ONE LINE OF TEXT
Sub ERASER
    Rem FOR I=1 TO 12:PRINT "          ";:NEXT I
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
        For J = 1 To NUMBERCONVERTS 'CONVERT OBJECT$ LACKING * OR !
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
        ROOMOBJECT$(),ITEMNUMBER,TURNNUMBER,THING$,INVENTORY$(),FLAG()
    If FLAG(1) <> 0 Then Exit Sub
    If FLAG(14) = 1 Then Exit Sub
    Select Case VERB$
        Case "QUIT"
            Let FLAG(1) = 2 'GAME NOT WON(1) OR LOST(-1):EXIT SELECT
        Case "SAVE"
            If OBJECT$ <> "GAME" Then Print "SAVE GAME": Exit Select
            Open "PIRESAV.BAS" For Output As #1
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
            If OBJECT$ = "CANNON" Then Print "IT'S LOADED!": Exit Select
            If OBJECT$ <> "GAME" Then Print "RESTORE GAME": Exit Select
            Open "PIRESAV.BAS" For Input As #1
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
            Print "You carry:  ";
            For I = 1 To 5
                IF INVENTORY$(I)<>"EMPTY" THEN COUNTER=COUNTER + 1:_
                    PRINT INVENTORY$(I);"   ";
            Next I
            If COUNTER = 0 Then Print "nothing at all" Else Print
        Case "GO", "MOVE", "N", "S", "E", "W", "U", "D"
            If Len(VERB$) = 1 Then OBJECT$ = VERB$
            If OBJECT$ = "NORTH" Or OBJECT$ = "N" Then DIRECTION = 1
            If OBJECT$ = "SOUTH" Or OBJECT$ = "S" Then DIRECTION = 2
            If OBJECT$ = "EAST" Or OBJECT$ = "E" Then DIRECTION = 3
            If OBJECT$ = "WEST" Or OBJECT$ = "W" Then DIRECTION = 4
            If OBJECT$ = "UP" Or OBJECT$ = "U" Then DIRECTION = 5
            If OBJECT$ = "DOWN" Or OBJECT$ = "D" Then DIRECTION = 6
            If Mid$(OBJECT$, 1, 4) = "PORT" And ROOM = 6 Then DIRECTION = 4
            IF MOVEMENTTABLE (ROOM,DIRECTION) = 0 THEN _
                PRINT "You can't GO that way" ELSE _
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
        Case "GET", "TAKE"
            IF OBJECT$="QUININE" AND FLAG(10)=1 THEN _
               PRINT "That should be enough.":EXIT SELECT
            If FNPRESENT = 0 Then Print "I don't see  "; OBJECT$: Exit Select
            IF RIGHT$(OBJECT$,1)="*" THEN _
               PRINT "I can't handle it.":EXIT SELECT
            If OBJECT$ = "QUININE" And FNPRESENT = 1 Then FLAG(10) = 1
            IF OBJECT$="CLOTHES" THEN _
               PRINT "GETting something is not the same as WEARing it"
            IF OBJECT$="BRANCH" AND ROOM=11 AND FLAG(19)=0 THEN _
               PRINT "PARROT won't let me...wants to be CALLed by name.":_
               EXIT SELECT
            IF OBJECT$="FISH" THEN THING$="SPEAR":IF FNCARRY =0 THEN _
                PRINT "You'll need a SPEAR":THING$="":EXIT SELECT
            IF MID$(OBJECT$,1,4)="COAL" THEN THING$="SHOVEL":IF FNCARRY = 0_
                THEN PRINT "You'll need something to carry it in.":_
                THING$="":EXIT SELECT
            For I = 1 To 5
                IF INVENTORY$(I)="EMPTY" THEN INVENTORY$(I)=OBJECT$:_
                    PRINT "GOT IT":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY":_
                    EXIT SELECT
            Next I
            Print "You're carrying too much.  DROP something!"
        Case "PUT", "DROP", "GIVE"
            THING$ = OBJECT$
            If FNCARRY = 0 Then Print "You don't have the "; OBJECT$: Exit Select
            IF OBJECT$="FISH" AND ROOM=13 AND FLAG(7)=0 THEN _
               PRINT "Cure the scurby first.":EXIT SELECT
            IF OBJECT$="FISH" AND FLAG(5)=0 THEN PRINT _
                "COOK it first":EXIT SELECT
            IF OBJECT$="ORANGES" AND ROOM = 13 AND FLAG(7)=0 THEN FLAG(7)=1:_
               PRINT "OK":ROOMOBJECT$(13,1)="A still hungry HERMIT*" _
               :EXIT SELECT
            IF OBJECT$="FISH" AND ROOM=13 AND FLAG(8)=0 THEN_
               FLAG(8)=1:ROOMOBJECT$(16,1)="EMPTY":_
               PRINT "The grateful HERMIT leaves to fire the CANNON ":_
               PRINT "in order to lure the PIRATES to the SHIP.":_
               ROOMOBJECT$(13,1)="EMPTY":EXIT SELECT

            For J = 1 To 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":EXIT SELECT
            Next J
            Print "This location is full, take it elsewhere."
        Case "FEED"
            Print "Try:   GIVE FISH": Exit Select
        Case "KILL"
            Print "Who do you think you are, Rambo?": Exit Select
        Case "WEAR", "CHANGE"
            THING$ = OBJECT$
            IF OBJECT$="CLOTHES" AND FNCARRY = 1 THEN FLAG(2)=1:_
               PRINT "Now you look somewhat like a PIRATE.":EXIT SELECT
            If OBJECT$ = "CLOTHES" Then Print "GET CLOTHES first": Exit Select
            Print "Didn't help.  Try something different.": Exit Select
        Case "CUT", "TRIM", "SHARPEN", "CARVE"
            THING$ = OBJECT$
            If FNCARRY = 0 Then Print "You don't have the BRANCH.": Exit Select
            THING$ = "SWORD"
            If FNCARRY = 0 Then Print "You'll need a cutting instrument.": Exit Select
            THING$ = OBJECT$
            IF FNCARRY =1 THEN LET INVENTORY$(ITEMNUMBER)="SPEAR":_
                PRINT "You now have a fine SPEAR.":EXIT SELECT


        Case "READ"
            If OBJECT$ <> "SIGN*" Then Print "I only READ SIGNS*": Exit Select
            Print "It says:"
            IF ROOM=1 THEN _
               PRINT "I'm a color, I'm a fruit,":_
               PRINT "I'm a lovely thing to be;":_
               PRINT "Feed me to that HERMIT guy,":_
               PRINT "'cause he lacks vitamin C."
            IF ROOM=2 THEN _
               PRINT "No, No, No, Not open,":_
               PRINT "What, oh what, do I wear?;":_
               PRINT "I must change my whole look,":_
               PRINT "or face the PIRATES' glare."
            IF ROOM=4 THEN _
               PRINT "Just in from the A1 code service: ":_
               PRINT:_
               PRINT "20-8-5 16-1-18-18-15-20'19":_
               PRINT "14-1-13-5 9-19 2-9-12-12."
            IF ROOM=6 THEN _
               PRINT "One letter is silent,":_
               PRINT "Like the k in knife;":_
               PRINT "This truly fine weapon,":_
               PRINT "May yet save a life."
            IF ROOM=18 THEN _
               PRINT "Foot is to shoe as hand is to glove.":_
               PRINT "Cut is to knife as dig is to ________.":_
               PRINT "Sugar is to horse as cracker is to __________.":_
               PRINT "Switch is to light as ________ is to lock."
            IF ROOM=19 THEN _
               PRINT "What starts with a  Q,":_
               PRINT "And ends with an E;":_
               PRINT "A medicine strong,":_
               PRINT "Tastes bitter to me."
            IF ROOM=20 THEN _
               PRINT "He hides on the ship,":_
               PRINT "And he's no human being;":_
               PRINT "REVEAL what he is,":_
               PRINT "And your ropes he'll be freeing."
            Print
        Case "REVEAL"
           IF OBJECT$="BRANCH" AND ROOM=11 AND FLAG(12)=0 THEN _
              PRINT "OK":ROOMOBJECT$(11,1)="BRANCH":FLAG(12)=1:EXIT SELECT
           IF OBJECT$="KEY" AND ROOM=15 AND FLAG(21)=0 THEN PRINT "OK":_
              ROOMOBJECT$(15,2)="KEY":FLAG(21)=1:EXIT SELECT
           IF OBJECT$="RAT*" AND ROOM=20 AND FLAG(11)=0 THEN ROOMOBJECT$(20,1)="RAT*":_
              PRINT "The RAT* has gnawed through your ropes!":_
              FLAG(11)=1:EXIT SELECT
           IF OBJECT$="SHOVEL" AND FLAG(15)=0 AND ROOM=21 THEN PRINT "OK":_
              ROOMOBJECT$(21,1)="SHOVEL":FLAG(15)=1:EXIT SELECT
           IF OBJECT$="SWORD" AND ROOM=6 AND FLAG(16)=0 THEN PRINT "OK":_
              ROOMOBJECT$(6,1)="SWORD":FLAG(16)=1:EXIT SELECT
           IF OBJECT$="CLOTHES" AND ROOM=2 AND FLAG(17)=0 THEN PRINT "OK":_
              ROOMOBJECT$(2,1)="CLOTHES":FLAG(17)=1:EXIT SELECT
           IF OBJECT$="QUININE" AND ROOM=19 AND FLAG(18)=0 THEN PRINT "OK":_
              ROOMOBJECT$(19,1)="QUININE":FLAG(18)=1:EXIT SELECT
           IF OBJECT$="ORANGES" AND ROOM=1 AND FLAG(20)=0 THEN PRINT "OK":_
              ROOMOBJECT$(1,1)="ORANGES":FLAG(20)=1:EXIT SELECT
           IF OBJECT$="MOUSE" OR OBJECT$="MICE" THEN _
              PRINT "You are thinking too small.":EXIT SELECT
           IF LEFT$(OBJECT$,4)="CLOS" THEN _
              PRINT "Try a different spelling.":EXIT SELECT
            If OBJECT$ = "KNIFE" Then Print "Ouch! Better not.": Exit Select
            Print "Nothing happened.  Try something else.": Exit Select

        Case "COOK", "HEAT", "WARM", "ROAST"
            THING$ = OBJECT$
            If FNCARRY = 0 Then Print "You don't have the fish": Exit Select
            THING$ = "COALS"
            If FNCARRY = 0 Then Print "You'll need to GET the COALS": Exit Select
            Let FLAG(5) = 1: Print "OK": Exit Select
        Case "UNLOCK", "OPEN"
            Select Case OBJECT$
                Case "BOX", "GUNBOX"
                    Print "You'll need a crowbar to OPEN it": Exit Select
                Case "LOCK", "GATE", "DOOR"
                    THING$ = "KEY"
                    IF FNCARRY=0 THEN PRINT "You'll need the KEY"_
                        :EXIT SELECT
                    IF FLAG (8)=0 THEN PRINT "The PIRATES recapture you as you";_
                        :PRINT "try to UNLOCK the GATE":ROOM=20:_
                        EXIT SELECT
                    FLAG(1) = 1: Print "OK": Exit Select
            End Select
        Case "CLIMB"
            If Left$(OBJECT$, 4) = "TREE" Then Print "Too difficult.": Exit Select
            Print "Try:    GO UP.": Exit Select
        Case "CALL", "NAME", "SAY"
            Print "Just type the name you were going to CALL.": Exit Select
        Case "FISH", "CATCH"
            Print "Try:    GET FISH": Exit Select
        Case "EAT", "CONSUME"
            Print "Save the food for the hermit.": Exit Select
        Case "BILL"
            IF ROOM=11 THEN FLAG(19)=1:_
               PRINT "The PARROT flies over and affectionately bites your nose.":_
               EXIT SELECT
            Print "Not here": Exit Select
        Case "POLLY"
            Print "The male PARROT is quite offended.": Exit Select
        Case "USE"
            Print "To do what?  Be more specific.": Exit Select
        Case Else
            Print "I don't know that verb": Exit Select
    End Select
End Sub

'MODULE 2.3.1 DEFINE THE FUNCTION- IS THE OBJECT PRESENT?
Function FNPRESENT
    For J = 1 To 15
             IF ROOMOBJECT$(ROOM,J)=OBJECT$ THEN _
                 FNPRESENT=1:ITEMNUMBER=J:EXIT FUNCTION
    Next J
    FNPRESENT = 0
End Function

'MODULE 2.3.2 DEFINE THE FUNCTION- IS IT BEING CARRIED?
Function FNCARRY
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
    If FLAG(1) = 1 Then Print "One of the prisoners has a crowbar and GETs the guns."
    If FLAG(1) = 1 Then Print "Fully armed, the goodguys capture the PIRATES. (Thanks to you!)"
    If FLAG(1) = 1 Then Print "Amidst cheers and adulation, you know you've won the PIRATE adventure!"
    For I = 1 To 6: Print: Next I
    Input "Be sure that your disk is in the drive and press ENTER "; DUMMY$

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
    '    Let DTA(9) = FLAG(1): Let DTA(34) = DTA(34) + Int((Timer - STRT) / 6)

    '    Open "REPORT.DTA" For Output As #1
    '    Rem SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
    '    For I = 1 To 40
    '        Write #1, DTA(I)
    '    Next I
    '    For I = 1 To 10
    '        Write #1, DTA$(I)
    '    Next I
    '    Close #1

    Open "PIRTDATA.TXT" For Append As #2
    Print #2, Date$, Time$
    For I = 0 To TURNNUMBER
        If Int(I / 5) = I / 5 Then Print #2, ANSWER$(I) Else Print #2, ANSWER$(I),
    Next I
    Print #2, Int((STRT - Timer) / 6), Int(1000 * Rnd(0))
    Close #2
    Print "This game is over.  Type PIRATE to play again."
End Sub


'END OF PROGRAM
