' QB64 - Meta-command that allows window to be resized
$Resize:Stretch

'MODULE 0 MAIN    ******FARMTOWN*******9-2-90
'GAMEDTA.BAS CHANGED TO FARMSAV.BAS ON 1-24-91
'ALSO FIXED ENDING WITH WIN NO MATTER WHETHER
'CHANGE SAW RIDDLE AND MAKE STEW 6MAR92
'CHANGED TIMER AND STRT  2-3-94
'CHANGED SAVE GAME TIME$DATE$ CARROTS 2ND FLARE HINT 7-30-95
'changed snips 1-3-96
'added report 3-5-96  FARMDATA.TXT 10-24-96
'6-30-23, Henrik Aasman: Modified for QB64
'  Moved all definition of global (shared) variables to Main module
'  QB64 needs unique names so MOVEMENTTABLE --> MOVEMENTTABLESUB and INVENTORY --> INVENTORYSUB
'  Changed syntax for BEEP so BEEP 3 --> BEEP
'  Changed syntax for DELAY so DELAY --> _DELAY
'  Fixed ROOMBOJECTS$ to ROOMOBJECTS$
'  Renamed DEF to FUNCTION
'  Removed reading/writing to REPORT.DTA
'  Fixed spelling on a couple of places
'  Added "" around some of the data in DATA-tables
'  HIGHWAY always have DESCRIBEFLAG set to 1
'  Moved BREAD to CAR instead of FARM HOUSE at start of game (as in game https://archive.org/details/TheHangtownTrilogy4amCrack)
'  DOLLAR is revealed immediately when fifth BOTTLE is dropped
'  Changed so that the filled GAS-CAN has to be returned to CAR and CAR started to win game

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
ReDim Shared GAMESTATE$
ReDim Shared CONVERTNUM

Cls
STRT = Timer
Call LOGON
Locate 12, 12: Print "ONE MOMENT PLEASE...."
Call SETUP '  MODULE 1 SET UP VARIABLES
Call TURN '   MODULE 2 RUN TURNS
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
    Call ROOMINFO '        MODULE 1.1 READ ROOM INFORMATION
    Call ROOMOBJECTS '     MODULE 1.2 READ ROOM OBJECTS
    Call MOVEMENTTABLESUB 'MODULE 1.3 READ MOVEMENT TABLE
    Call INVENTORYSUB '    MODULE 1.4 INITIALIZE INVENTORY ARRAY
    '                      TO AN EMPTY LIST
    Call OTHERS '          MODULE 1.5 INITIALIZE OTHER VARIABLES
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
    Data 21,"RABBIT'S WARREN",STREAM,CORNFIELD,PASTURE,PIG STY
    Data STORAGE SHED,BARN,POND,GARDEN,BARNYARD
    Data FARMHOUSE,HIGHWAY,HIGHWAY,HIGHWAY,HIGHWAY
    Data HIGHWAY,HIGHWAY,RECYCLING CENTER,STREET,GAS STATION
    Data "CAR PARKED ON SOUTH SIDE OF HIGHWAY"

    For I = 1 To NUMBERROOMS
        Let DESCRIBEFLAG(I) = 0
    Next I
    For I = 1 To NUMBERROOMS
        Read ROOMDESCRIBE$(I)
        If ROOMDESCRIBE$(I) = "HIGHWAY" Then Let DESCRIBEFLAG(I) = 1 ' QB64
        Read ROOMDES2$(I)
    Next I
    Data "The eyes of the rabbit sitting outside his warren are more"
    Data "orange than pink.  Too much vitamin A!"
    Data "Though the gurgle is friendly enough,"
    Data "STICK'S STREAM is more dangerous than it looks."
    Data "Here at KELLOG'S CORNFIELD,"
    Data "You see plenty of pig food."
    Data "Here at PIZENSIGH'S PASTURE, we see the dangers of over-"
    Data "grazing.  The grass is too short to eat."
    Data "Here at the INNAH PIG STY, The scene reminds you of your"
    Data "school cafeteria on a rainy day."
    Data "There are tools and fruit in the MAGICAL STORAGE SHED,"
    Data "but you have to ask for them by name.  Weird."
    Data "The BARN smells, well, like a barn.  No wonder people"
    Data "held barn dances out of door whenever possible."
    Data "If the DUCKS weren't so grouchy, this POND"
    Data "would be a good place to spend some time."
    Data "Some vandalous animal has dug up and stolen all of the"
    Data "CARROTS, but you may still find some POTATOES."
    Data "Watch your step!"
    Data "This has been a very active BARNYARD."
    Data "What we have here in the FARMHOUSE is a very crabby FARMER"
    Data "who seems to be waiting for someone to fix supper."
    Data "HIGHWAY"," ","HIGHWAY"," ","HIGHWAY"," ","HIGHWAY"," "
    Data "HIGHWAY"," "
    Data "The sound of the trucks barreling by on the narrow"
    Data "HIGHWAY is quite deafening.  "
    Data "Here at the SAVE OUR GLASS RECYCLING CENTER,"
    Data "nothing is wasted."
    Data "MAIN STREET could just as easily"
    Data "have been called ONLY STREET."
    Data "The semi-automated GAS STATION"
    Data "is quite deserted."
    Data "You are out of gas and money, parked along the SOUTH SIDE OF"
    Data "A busy HIGHWAY.  You recall passing a town a little way back."

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
    Data 0,2,0,0,0,0,1,4,0,0,0,0
    Data 0,0,4,0,0,0,2,7,5,3,0,0
    Data 0,0,0,4,0,0,0,0,7,0,0,0
    Data 4,10,8,6,0,0,0,0,0,7,0,0
    Data 0,0,10,0,0,0,7,14,11,9,0,0

    Data 0,0,0,10,0,0,0,0,13,12,0,0
    Data 0,0,14,12,0,0,10,19,15,13,0,0
    Data 0,0,16,14,0,0,0,21,17,15,0,0
    Data 0,0,17,16,0,0,0,0,19,0,0,0
    Data 14,0,20,18,0,0,0,0,0,19,0,0
    Data 16,0,0,0,0,0
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
        FLAG(),OBJ$(),OBJ2$(),CONVERTNUM
    Let ROOM = 21 'START IN CAR
    Let TURNNUMBER = 0
    DANGEROUS = 1
    Let THING$ = ""
    ReDim FLAG(30)
    For I = 1 To 30: Let FLAG(I) = 0: Next I 'SET FLAGS TO 0
    'FLAG #  MEANING OF 0      FLAG# MEANING OF 0
    ' 1      GAME NOT OVER       2   TREE NOT SAWED
    'FOR 1, 1 MEANS GAME WON -1 GAME LOST 2 MEANS GAME QUIT
    ' 3      PIGS* NOT FED       4   HAY NOT GOTTEN
    ' 5      DUCKS* NOT FED      6   COWS* NOT FED
    ' 7      PEARS NOT REVEALED  8   HOE NOT REVEALED
    ' 9      POTATOES NOT GOTTEN 10  FARMER NOT ASLEEP
    '11      FLARE NOT DROPPED   12  5 BOTTLES NOT DROPPED
    '13      GAS NOT GOTTEN      14  USED IN DESCRIBE
    '14 IS USED TO EXIT TURN BUT NOT EXIT THE GAME
    '15      SAW NOT REVEALED    16  FLARE NOT REVEALED
    '17      CAR SITUATION NOT DESCRIBED
    '18      FLARE HINT NOT GIVEN
    '19 2ND FLARE HINT NOT GIVEN
    Let ROOMOBJECT$(1, 1) = "SIGN*" 'STARTING LOCATIONS FOR OBJECTS
    Let ROOMOBJECT$(1, 2) = "RABBIT*"
    Let ROOMOBJECT$(1, 3) = "CARROTS"
    Let ROOMOBJECT$(2, 1) = "EMPTYTREE-BRIDGE*"
    Let ROOMOBJECT$(3, 1) = "SIGN*"
    Let ROOMOBJECT$(3, 2) = "CORN"
    Let ROOMOBJECT$(4, 1) = "TREE*"
    Let ROOMOBJECT$(4, 2) = "COWS*"
    Let ROOMOBJECT$(4, 3) = "EMPTYBOTTLE"
    Let ROOMOBJECT$(4, 4) = "EMPTYBOTTLE"
    Let ROOMOBJECT$(5, 1) = "PIGS*"
    Let ROOMOBJECT$(5, 2) = "EMPTYBOTTLE"
    Let ROOMOBJECT$(6, 1) = "FRUIT*"
    Let ROOMOBJECT$(6, 2) = "TOOLS*"
    Let ROOMOBJECT$(6, 3) = "SIGN*"
    Let ROOMOBJECT$(6, 4) = "EMPTYPEARS"
    Let ROOMOBJECT$(6, 5) = "EMPTYHOE"
    Let ROOMOBJECT$(6, 6) = "EMPTYSAW"
    Let ROOMOBJECT$(7, 1) = "HAY"
    Let ROOMOBJECT$(7, 2) = "PITCHFORK"
    Let ROOMOBJECT$(7, 3) = "MEAT"
    Let ROOMOBJECT$(7, 4) = "EMPTYBOTTLE"
    Let ROOMOBJECT$(8, 1) = "DUCKS*"
    Let ROOMOBJECT$(8, 2) = "EMPTYBOTTLE"
    Let ROOMOBJECT$(9, 1) = "SIGN*"
    Let ROOMOBJECT$(9, 2) = "EMPTYPOTATOES"
    Let ROOMOBJECT$(10, 1) = "SIGN*"
    Let ROOMOBJECT$(11, 1) = "SIGN*"
    Let ROOMOBJECT$(11, 2) = "FARMER*"
    Let ROOMOBJECT$(11, 3) = "STOVE*"
    Let ROOMOBJECT$(11, 4) = "POT*"
    Let ROOMOBJECT$(11, 5) = "GAS-CAN"
    Let ROOMOBJECT$(18, 1) = "SIGN*"
    Let ROOMOBJECT$(18, 2) = "EMPTYDOLLAR"
    Let ROOMOBJECT$(19, 1) = "SIGN*"
    Let ROOMOBJECT$(20, 1) = "SIGN*"
    Let ROOMOBJECT$(21, 1) = "BREAD"
    Let ROOMOBJECT$(21, 2) = "SIGN*"
    Let ROOMOBJECT$(21, 3) = "EMPTYFLARE"

    Read CONVERTNUM
    ReDim OBJ$(CONVERTNUM), OBJ2$(CONVERTNUM)
    For I = 1 To CONVERTNUM 'READ IN WORD!* CONVERSIONS
        Read OBJ$(I), OBJ2$(I)
    Next I
    Data 19,SIGN,SIGN*,RABBIT,RABBIT*,TREE-BRIDGE,TREE-BRIDGE*
    Data BRIDGE,TREE-BRIDGE*,TREE,TREE*,STUMP,STUMP*,COWS,COWS*
    Data PIGS,PIGS*,FRUIT,FRUIT*,TOOLS,TOOLS*,DUCKS,DUCKS*
    Data FARMER,FARMER*,STOVE,STOVE*,POT,POT*,CAN,GAS-CAN,PEAR,PEARS
    Data CARROT,CARROTS,PAIR,PEARS,FLAIR,FLARE
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
           PRINT "FARMTOWN (at the DOS prompt) and later,":_
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
                Case "HIGHWAY"
                    Print "You find yourself on the SOUTH side of a HIGHWAY"
                Case "STREET", "BARN", "FARMHOUSE"
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

    'PUT DOLLAR IN 18 IF 5 BOTTLES ARE PRESENT
    IF ROOM=18 AND FNFIVEBOTTLES=1 AND FLAG(12)=0 THEN _
       ROOMOBJECT$(18,2)="DOLLAR":FLAG(12)=1

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

    IF ROOM=2 AND FLAG(2)=0 THEN PRINT "The narrow streem is too deep to cross":_
        FLAG(14)=1:ROOM=4:EXIT SUB
    IF ROOM=10 AND FLAG(11)=0 THEN ROOM=14:CLS:PRINT:PRINT:_
        PRINT "The blast of air of a big truck going by blows you back to":_
        PRINT "the SOUTH SIDE OF THE HIGHWAY.  They should slow down!":_
        FLAG(14)=1:EXIT SUB
    IF FLAG(18)=0 AND TURNNUMBER>30 THEN FLAG(18)=1:_
       PRINT "An outrageously dressed passerby tells you your outfit lacks flair"
    IF FLAG(19)=0 AND FLAG(11)=0 AND TURNNUMBER>50 AND ROOM=14 THEN FLAG(19)=1:_
       PRINT "Maybe I should DROP the REVEALed FLARE here."
    Print
End Sub

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
Sub GAMESTARTER
    Cls
    Print
    Print "                WELCOME TO FARMTOWN"
    Print
    Print "Stop vegetating and come away with me on an exciting adventure "
    Print "to FARMTOWN, home of the rutabaga!  You'll DIG the rural life "
    Print "along a dangerous HIGHWAY.  Cope with the wild hare!"
    Print "MAKE STEW!  GET GAS!"
    Print
    Print "Give COMMANDs as verb then object, such as GO NORTH,"
    Print "SAVE GAME, RESTORE GAME, READ SIGN, DIG POTATOES,"
    Print "AND REVEAL APPLES (REVEALed OBJECTS will show up where you"
    Print "would expect that OBJECT to be, not where you typed  REVEAL )"
    Print
    Print "Exceptions to this two-word sentence rule are single-letter"
    Print "COMMANDs such as N to GO NORTH, U to GO UP, D to GO DOWN"
    Print
    Print "P.S. Don't try to get objects ending in an *, e.g., TREE*,"
    Print "as they are quite unobtainable-- you get my drift?"
    Print
    Print "And if you get stuck, keep trying different things because"
    Print "after a certain number of turns, a hint will show up."
    Print "Happy Adventuring!"
    Print "If done taking notes, press ENTER to begin"
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
        ROOMOBJECT$(),OBJ$(),OBJ2$(),C$,ANSWER$(),TURNNUMBER,CONVERTNUM

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
        For J = 1 To 19 'CONVERT OBJECT$ LACKING * OR !
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
            Open "FARMSAV.BAS" For Output As #1

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
            Open "FARMSAV.BAS" For Input As #1

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
            Print "   You carry: ";
            For I = 1 To 5
                IF INVENTORY$(I)<>"EMPTY" THEN COUNTER=COUNTER + 1:_
                    PRINT INVENTORY$(I);"   ";
            Next I
            If COUNTER = 0 Then Print "nothing at all": Exit Select
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
                PRINT "You can't GO that way" ELSE _
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
            Exit Select

        Case "GET", "TAKE", "ACQUIRE"

            HOLDING$ = OBJECT$
               IF ROOM<>1 AND OBJECT$="CARROTS"  THEN PRINT "They aren't here":_
                  EXIT SELECT
               OBJECT$="PEARS":IF ROOM=1 AND FNPRESENT=0 THEN _
                  PRINT "The RABBIT won't let me":EXIT SELECT
            OBJECT$ = HOLDING$

            If ROOM = 6 And OBJECT$ = "FRUIT*" Then Print "READ SIGN*": Exit Select
            If ROOM = 6 And OBJECT$ = "TOOL*" Then Print "READ SIGN*": Exit Select

            THING$="PITCHFORK":IF OBJECT$="HAY" AND FNCARRY=0 THEN _
               PRINT "You'll need the PITCHFORK":EXIT SELECT
            IF FNCARRY=1 AND ROOM=7 AND FLAG(4)=0 AND FNINVENTORYSPACE=1 _
               THEN PRINT "Moving the HAY REVEALs a BOTTLE":_
               ROOMOBJECT$(7,4)="BOTTLE":FLAG(4)=1

            IF OBJECT$="POTATOES" AND FLAG(9)=0 THEN _
               PRINT "DIG them up first":EXIT SELECT

            IF OBJECT$="GAS-CAN"AND FLAG(10)=0 THEN _
               PRINT "The FARMER won't let you":EXIT SELECT
            If OBJECT$ = "GAS" Then Print "Try  FILL GAS-CAN ": Exit Select
            If OBJECT$ = "CARROTS" Then Print "DROP only at FARMHOUSE!"

            If FNPRESENT = 0 Then Print "I don't see "; OBJECT$: Exit Select
            If Right$(OBJECT$, 1) = "*" Then Print "I can't handle it": Exit Select

            For I = 1 To 5
                IF INVENTORY$(I)="EMPTY" THEN INVENTORY$(I)=OBJECT$:_
                    PRINT "GOT IT":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY":_
                    EXIT SELECT
            Next I
            Print "You're carrying too much.  DROP something!"
        Case "PUT", "DROP", "GIVE", "LEAVE"

            THING$="HAY":IF ROOM=4 AND FNCARRY=1 AND FNDROPABLESPOT=1 AND _
               FLAG(6)=0 AND OBJECT$="HAY" THEN _
               PRINT "As the COWS move to the HAY, you see a BOTTLE":_
               ROOMOBJECT$(4,3)="BOTTLE":FLAG(6)=1

            THING$="CORN":IF ROOM=5 AND FNCARRY=1 AND FNDROPABLESPOT=1 AND _
               FLAG(3)=0 THEN PRINT "As the PIGS come over you see a BOTTLE":_
               ROOMOBJECT$(5,2)="BOTTLE":FLAG(3)=1

            THING$="BREAD":IF ROOM=8 AND OBJECT$="BREAD" AND FNCARRY=1 AND _
               FNDROPABLESPOT=1 AND FLAG(5)=0 THEN _
               PRINT "As the DUCKS move, you see a BOTTLE":ROOMOBJECT$(8,2)="BOTTLE":_
               FLAG(5)=1

            If OBJECT$ = "FLARE" And ROOM <> 14 Then Print "Not here": Exit Select
            If OBJECT$ = "CARROTS" And ROOM <> 11 Then Print "Not here": Exit Select

            THING$="FLARE":IF ROOM=14 AND OBJECT$="FLARE" AND FNCARRY=1 AND _
               FNDROPABLESPOT=1 AND FLAG(11)=0 THEN FLAG(11)=1:_
               PRINT "Traffic has slowed.  You can now GO NORTH "
            THING$ = OBJECT$
            If FNCARRY = 0 Then Print "You don't have the "; OBJECT$: Exit Select

            For J = 1 To 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":_
                    EXIT SELECT
            Next J
            Print "This room is full, take it elsewhere": Exit Select
        Case "LOOK", "EXAMINE", "INSPECT"
            If OBJECT$ = "SIGN*" Then Print "Try READ SIGN* ": Exit Select
            Print "I don't see anything unexpected": Exit Select
        Case "READ"
            If OBJECT$ <> "SIGN*" Then Print "I only READ SIGNS*": Exit Select
            Print "It says:"
            IF ROOM=1 THEN _
               PRINT "I'm so sick of CARROTS,":_
               PRINT "They're all that I eat;":_
               PRINT "A nice piece of fresh fruit,":_
               PRINT "Would sure be a treat."
            IF ROOM=3 THEN _
               PRINT "Santa,s last word,":_
               PRINT "Or is it a greeting?":_
               PRINT "GET this good tool,":_
               PRINT "And STEW you'll be eating."
            IF ROOM=6 THEN _
               PRINT "You need to ask for something specific like ":_
               PRINT "REVEAL APPLES or REVEAL SHOVEL"
            IF ROOM=9 THEN _
               PRINT "A fruit named for two,":_
               PRINT "Like shoes, socks and aces;":_
               PRINT "Whose shape is unique,":_
               PRINT "And curvy in places."
            IF ROOM=10 THEN _
               PRINT "Hundreds of teeth,":_
               PRINT "Of steel not enamel;":_
               PRINT "When you don't push,":_
               PRINT "You pull on my handle."
            IF ROOM=11 THEN _
               PRINT "Recipe for FARMER's STEW:":_
               PRINT "DROP MEAT, POTATOES and CARROTS here,":_
               PRINT "And type MAKE STEW "
            IF ROOM=18 THEN _
               PRINT "Leave 5 BOTTLES here and GET a DOLLAR"
            IF ROOM=19 THEN _
               PRINT "My red glow of caution,":_
               PRINT "You start with a scratch;":_
               PRINT "Or a way of doing,":_
               PRINT "With style and panache."
            IF ROOM=20 THEN _
               PRINT "DROP a DOLLAR and FILL your GAS-CAN with":_
               PRINT "a gallon of gas."
            IF ROOM=21 THEN _
               PRINT "An object in this car,":_
               PRINT "Will help you with your chores;":_
               PRINT "So come back when you know,":_
               PRINT "Say its name and it's yours."
            Print
            Exit Select


        Case "DRINK", "EAT"
            Print "This is not the time to worry about your stomach": Exit Select
        Case "CLIMB"
            If OBJECT$ = "TREE*" Then Print "Easy Tarzan!": Exit Select
            Print "Come down from there!": Exit Select

        Case "MILK"
            Print "Oh, leave them alone!": Exit Select
        Case "KILL"
            Print "Mellow out, it's only a game!": Exit Select

        Case "DIG", "HOE"
            If OBJECT$ <> "POTATOES" Then Print "Try  DIG POTATOES ": Exit Select
         THING$="HOE":IF FNCARRY=1 AND ROOM=9 AND FLAG(9)=0 THEN _
            PRINT "THERE THEY ARE!":ROOMOBJECT$(9,2)="POTATOES":_
            FLAG(9)=1:EXIT SELECT
            Print "You'll need the right tool": Exit Select


            Print "The substrate is too hard here to dig": Exit Select
        Case "MAKE", "PREPARE"
            If OBJECT$ <> "STEW" Then Print "Try  MAKE STEW ": Exit Select
            OBJECT$ = "MEAT": If FNPRESENT = 0 Then Print "Where's the MEAT?": Exit Select
            OBJECT$ = "POTATOES": If FNPRESENT = 0 Then Print "Where're the POTATOES?": Exit Select
            OBJECT$ = "CARROTS": If FNPRESENT = 0 Then Print "Where're the CARROTS?": Exit Select
         IF FLAG(10)=0 THEN PRINT "The FARMER ate all of your delicious STEW and fell asleep":_
            ROOMOBJECT$(11,2)="SLEEPING-FARMER*":FLAG(10)=1:EXIT SELECT
            Print "Not again": Exit Select

        Case "CROSS"
            If OBJECT$ = "HIGHWAY" Then Print "Try  N  or GO NORTH ": Exit Select
            Print "Don't be CROSS!": Exit Select

        Case "ENTER"
            Print "No go, sorry": Exit Select

        Case "SAY"
            Print "Just type the word you were going to say": Exit Select

        Case "SAW"
        THING$="SAW":IF OBJECT$="TREE*" AND FNCARRY=1 AND FLAG(2)=0 THEN _
           PRINT "Timber!  I see a BOTTLE in the hollow STUMP*":FLAG(2)=1 :_
           ROOMOBJECT$(2,1)="TREE-BRIDGE*":ROOMOBJECT$(4,1)="STUMP*":_
           ROOMOBJECT$(4,4)="BOTTLE":EXIT SELECT
            Print "You need a SAW and a TREE": Exit Select

        Case "FEED"
            Print "Try  GIVE CORN  or GIVE BREAD  or  GIVE HAY": Exit Select

        Case "NO"
            Print "Fascinating": Exit Select
        Case "FILL"
        IF OBJECT$="TANK" THEN _
           PRINT "Just  PUT GAS-CAN  in the car":EXIT SELECT
        IF OBJECT$="POT*" THEN _
           PRINT "Just  PUT the ingredients in the FARMHOUSE":EXIT SELECT
        IF OBJECT$<>"GAS-CAN" THEN _
           PRINT "Are you sure it isn't full already?":EXIT SELECT
            If FLAG(13) = 1 Then Print "You did that!": Exit Select
            If ROOM <> 20 Then Print "GO to the GAS STATION": Exit Select
            If FNPRESENT = 0 Then Print "PUT GAS-CAN here": Exit Select
        OBJECT$="DOLLAR":IF FNPRESENT=0 THEN PRINT " PUT DOLLAR  here":_
           EXIT SELECT
        PRINT "The GAS-CAN is now full.":_
           ROOMOBJECT$(20,ITEMNUMBER)="EMPTY":_
           FLAG(13)=1:EXIT SELECT
        Case "START", "DRIVE"
            If OBJECT$ <> "CAR" Then Print "Try  DRIVE CAR ": Exit Select
         OBJECT$="GAS-CAN":IF FNPRESENT=0 OR FLAG(13)=0 THEN _
            PRINT "PUT the GAS-CAN in the car when it is full":EXIT SELECT
         PRINT "As you drive home, you congratulate yourself on winning":_
            PRINT "the FARMTOWN ADVENTURE!  GOOD WORK!!":FLAG(1)=1:EXIT SELECT
        Case "REVEAL"
        IF OBJECT$="PEARS" AND FLAG(7)=0 THEN PRINT "OK":_
           ROOMOBJECT$(6,4)="PEARS":FLAG(7)=1:EXIT SELECT
        IF OBJECT$="HOE" AND FLAG(8)=0 THEN PRINT "OK":_
           ROOMOBJECT$(6,5)="HOE":FLAG(8)=1:EXIT SELECT
        IF OBJECT$="FLARE" AND FLAG(16)=0 THEN FLAG(16)=1:_
           PRINT "The FLARE will light when DROPped.":_
           ROOMOBJECT$(21,2)="FLARE":FLAG(16)=1:EXIT SELECT
        IF OBJECT$="SAW" AND FLAG(15)=0 THEN PRINT "OK":_
           ROOMOBJECT$(6,6)="SAW":FLAG(15)=1:EXIT SELECT
            Print "Didn't work": Exit Select
        Case "FLARE", "FLAIR"
            Print "Try  REVEAL FLARE ": Exit Select
        Case "LIGHT", "IGNITE", "STRIKE"
            If OBJECT$ = "FLARE" Then Print "OK": Exit Select
            Print "That wouldn't be safe": Exit Select

        Case "USE"
            Print "To do what.  Be more specific.": Exit Select
        Case Else
            Print "I don't know that verb": Exit Select
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

'MODULE 2.3.3 DEFINE THE FUNCTION- IS THERE SPACE IN INVENTORY
Function FNINVENTORYSPACE
    Shared INVENTORY$()
    FNINVENTORYSPACE = 0
    For I = 1 To 5
        If INVENTORY$(I) = "EMPTY" Then FNINVENTORYSPACE = 1: Exit Function
    Next I
End Function

'MODULE 2.3.4 DEFINE THE FUNCTION- IS THERE SPACE HERE TO DROP SOMETHING
Function FNDROPABLESPOT
    Shared ROOMOBJECT$(), ROOM
    For J = 1 To 15
        If ROOMOBJECT$(ROOM, J) = "EMPTY" Then FNDROPABLESPOT = 1: Exit Function
    Next J
    FNDROPABLESPOT = 0
End Function

'MODULE 2.3.5 DEFINE THE FUNCTION- DO WE HAVE 5 BOTTLES YET
Function FNFIVEBOTTLES
    Shared ROOMOBJECT$(), ROOM
    TOTAL = 0
    For J = 1 To 15
        If ROOMOBJECT$(ROOM, J) = "BOTTLE" Then TOTAL = TOTAL + 1
    Next J
    If TOTAL = 5 Then FNFIVEBOTTLES = 1 Else FNFIVEBOTTLES = 0
End Function

'MODULE 3 CLOSING


Sub CLOSING
    Shared FLAG(), TURNNUMBER, ANSWER$(), STRT
    Print
    IF FLAG(1)= 1 THEN CLS:PRINT:PRINT:_
         PRINT "As you drive home, you congratulate yourself on winning":_
         Print "the FARMTOWN ADVENTURE!  GOOD WORK!!"
    For I = 1 To 6: Print: Next I
    Input "Be sure that your disk is in the drive and press ENTER"; DUMMY$

    '    Dim DTA(40), DTA$(10)
    '    Open "REPORT.DTA" For Input As #1
    '    Rem RETRIEVES OLD DATA FROM REPORT.DTA
    '    For I = 1 To 40
    '    Input #1, DTA(I)
    '    Next I
    '    For I = 1 To 10
    '        Input #1, DTA$(I)
    '    Next I
    '    Close #1

    '    Let DTA(10) = FLAG(1): Let DTA(35) = DTA(35) + Int((Timer - STRT) / 6)
    '    Open "REPORT.DTA" For Output As #1
    '    Rem SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
    '    For I = 1 To 40
    '        Write #1, DTA(I)
    '    Next I
    '    For I = 1 To 10
    '        Write #1, DTA$(I)
    '    Next I
    '    Close #1

    Open "FARMDATA.TXT" For Append As #2
    Print #2, Date$, Time$
    For I = 0 To TURNNUMBER
        If Int(I / 5) = I / 5 Then Print #2, ANSWER$(I) Else Print #2, ANSWER$(I),
    Next I
    Print #2, Int((STRT - Timer) / 6),


    Print #2, Int(1000 * Rnd(0))

    Close #2
    Print "This game is over.  Type FARMTOWN to play again."
End Sub

' END OF PROGRAM
