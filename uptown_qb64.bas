' QB64 - Meta-command that allows window to be resized
$Resize:Stretch

'MODULE 0 MAIN    ******UPTOWN******10-17-95
'CONVERTED FROM CURSED (SEE NEXT 4 REMS)
'CHANGED GAMEDTA.BAS TO CURSAV.BAS ON 1-24-91
'CHANGED TURNNUMBER TO GYNTURN FOR GIANT TURNS 18FEB92
'CHANGED TIMER ON 2-3-94
'CHANGED SAVE GAME TIME$ DATE$ 7-30-95
'CHANGED ROOMOBJECT$ LIMIT TO 15 OBJECTS THROUGHOUT 10-24-95
'MANY CHANGES ON THE WEEK OF 10-24-95
'copyright 1995 by John H. Doolittle  All rights reserved.  Thank you.
'10-29-95 put spaces in logon name date time
'REPORT STUFF 3-5-96 UPDATA.TXT 10-24-96
'7-3-23, Henrik Aasman: Modified for QB64
'  Moved all definition of global (shared) variables to Main module
'  QB64 needs unique names so MOVEMENTTABLE --> MOVEMENTTABLESUB and INVENTORY --> INVENTORYSUB
'  Changed syntax for BEEP so BEEP 3 --> BEEP
'  Changed syntax for DELAY so DELAY --> _DELAY
'  Fixed __ to _
'  Renamed DEF to FUNCTION
'  Removed reading/writing to REPORT.DTA
'  Added "" around some of the data in DATA-tables
'  Added DIM of DUMMY3$

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
ReDim Shared GUESSNUMB
ReDim Shared CLUE$(0)
ReDim Shared WURD$(8)
ReDim Shared LOGIC$
ReDim Shared SLAMMER
ReDim Shared TRYNUMB
ReDim Shared VERB$
ReDim Shared OBJECT$
ReDim Shared C$
ReDim Shared DIRECTION
ReDim Shared ITEMNUMBER
ReDim Shared STRT

Cls: ReDim FLAG(30), OBJ$(30), OBJ2$(30), CLUE$(30), WURD$(30)
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
    Data 16,BACKYARD,GARAGE,TOOL ROOM,LOWER LANDING,BATHROOM
    Data DINING ROOM,KITCHEN,HALL,ENTRYWAY,"LIBRARY"
    Data OFFICE,UPPER LANDING,BLUE BEDROOM,PINK BEDROOM,BEIGE BEDROOM
    Data ATTIC

    For I = 1 To NUMBERROOMS
        Let DESCRIBEFLAG(I) = 0
        Read ROOMDESCRIBE$(I), ROOMDES2$(I)
    Next I
    Data "We're outside of the house in the backyard.  The dirt"
    Data "shows the signs of much digging and smoothing over."

    Data "Here in the garage is a vintage classic car.  Andrea"
    Data "is admiring the paint job."

    Data "Funny, you don't notice any tools here in the tool room."
    Data "This is a weird house."

    Data "Here on the lower landing stands a magnificent bust of"
    Data "Wolfgang Amadeus Mozart.  Careful, it doesn't look stable."

    Data "The bathroom is nice ... running water ... the whole bit."
    Data " "

    Data "Here in the dining room, you can't help but notice that"
    Data "a portable radio is bolted to a shelf.  Strange."

    Data "The kitchen is clearly the scene of a major"
    Data "remodeling project."

    Data "Here in the cavernous hall, your footsteps echo on the"
    Data "marble floor.  Ahead, you see a cat scamper by."

    Data "The entryway could have been called an atrium -- it's"
    Data "that airy and light.  Andrea loves it."

    Data "From the dust in the library, you guess that Uncle Gus"
    Data "doesn't read much."

    Data "The office is an office .....  What can I say?"
    Data "The painting is ugly and the plant's about dead."

    Data "Architecturally, the upper landing is not noteworthy."
    Data " "

    Data "As you enter the blue bedroom, a huge dog tries to take a "
    Data "bite out of you.  Luckily, a rope ties him to the bedpost."

    Data "You thought you heard a motor shutting off as you "
    Data "entered the pink bedroom.  Very odd."

    Data "The beige bedroom is ... well ..... beige."
    Data " "

    Data "No surprise here.  The musty attic is dark and damp."
    Data "I'll bet the roof leaks in winter."
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
    Data 14,15,0,13,0,4
    Data 0,0,12,0,0,0
    Data 0,12,0,0,0,0
    Data 12,0,0,0,0,0

    Data 0,0,0,0,0,14

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
        FLAG(),OBJ$(),OBJ2$(),DESCRIBEFLAG(),ROOMDESCRIBE$(),ROOMDES2$(),_
        GUESSNUMB,CLUE$(),WURD$(),NUMBERROOMS,LOGIC$,SLAMMER,TRYNUMB
    Let TURNNUMBER = 0
    Let THING$ = ""
    For I = 1 To 30: Let FLAG(I) = 0: Next I 'SET FLAGS TO 0
    'FLAG #  MEANING OF 0      FLAG# MEANING OF 0
    ' 1      GAME NOT OVER       2 WURD2 GARAGE NOT SOLVED
    'FOR 1, 1 MEANS GAME WON -1 GAME LOST 2 MEANS GAME QUIT
    '3 CAT NOT YET APPEARED   4 DOG NOT YET SEEN
    '5 DOG NOT YET FREED    6 CAT HAS NOT YET APPEARED
    '7 GLASS NOT FILLED   8 PLANT NOT WATERED
    '9 SAFE NOT OPENED    10 YARN NOT CREATED
    '11 WURD 1 OFFICE NOT SOLVED  12 CAT NOT YET ATOP BED
    '13 TRUNK NOT OPENED IN CAR 14 BATTERIES NOT REMOVED
    '15 WURD3 BEIGE BEDROOM NOT SOLV 16 BATTERIES NOT LOADED
    '17 BUTTON NOT YET PUSHED 18 ROOM 1 NOTE
    '19 ROOM 8 NOTE                 20 ROOM 16 NOTE
    '21 ROOM 14 NOTE                22 BOGUS ROOM 6 NOTE
    '23 BOGUS ROOM 12 NOTE          24 NIGHTSTAND NOT PICKED

    'SET UP WORD GUESSING DATA
    For I = 1 To NUMBERROOMS
        Let CLUE$(I) = "": Let WURD$(I) = ""
    Next I
    Let CLUE$(11) = "PLANTS"
    Let WURD$(11) = "WATER"
    Let CLUE$(2) = "RING"
    Let WURD$(2) = "FINGER"
    Let CLUE$(15) = "ELECTRICITY"
    Let WURD$(15) = "WIRE"
    GUESSNUMB = 0: 'TOTAL NUMBER OF GUESSES AT WORDS
    TRYNUMB = 0: 'TOTAL # OF TRIES AT COMBO SAFE LOCK
    SLAMMER = 0: 'TOTAL # OF TRIES AT FINAL SOLUTION MATRIX
    LOGIC$ = "Logfal - "

    For I = 1 To 35
        Read ROOM, J, ROOMOBJECT$(ROOM, J)
    Next I
    Data 1,1,EMPTYPISTOL,1,2,EMPTYHOLES*,1,3,NOTE*
    Data 2,1,CAR*,2,2,EMPTYFLASHLIGHT,3,1,EMPTYWIRE
    Data 4,1,BUST*,4,2,EMPTYRING,4,3,EMPTYRUBBLE*
    Data 4,4,NOTE*,5,1,SINK*,6,1,SHELF*
    Data 6,2,RADIO*,6,3,EMPTYBATTERIES,7,1,SIGN*

    Data 7,2,GLASS,7,3,EMPTYKEY,6,4,NOTE*
    Data 8,1,NOTE*,9,1,SIGN*,10,1,NOTE*
    Data 10,2,EMPTYYARN,11,1,PLANT*,11,2,PAINTING*
    Data 11,3,EMPTYSHARD,12,1,NOTE*,13,1,BED*
    Data 13,2,DOG*,13,3,EMPTYCAT*,14,1,NOTE*

    Data 14,2,EMPTYBUTTON*,15,1,EMPTYNIGHTSTAND*,15,2,EMPTYNECKLACE
    Data 16,1,NOTE*,14,3,EMPTYLADDER*

    'THE STARTING ROOM IS THE ENTRYWAY - 9
    ROOM = 9


    For I = 1 To 25 'READ IN WORD!* CONVERSION
        Read OBJ$(I), OBJ2$(I)
    Next I
    Data GUN,PISTOL,HOLES,HOLES*,HOLE,HOLE*
    Data NOTE,NOTE*,LIGHT,FLASHLIGHT,BUST,BUST*
    Data RUBBLE,RUBBLE*,SINK,SINK*,SHELF,SHELF*
    Data RADIO,RADIO*,BATTERY,BATTERIES,SIGN,SIGN*
    Data PLANT,PLANT*,PAINTING,PAINTING*,SAFE,SAFE*

    Data LETTER,LETTERS,SHARDS,SHARD,BED,BED*
    Data DOG,DOG*,BUTTON,BUTTON*,NIGHTSTAND,NIGHTSTAND*
    Data LADDER,LADDER*,CHARGE,CHARGES,CAT,CAT*
    Data CAR,CAR*
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
           PRINT "UPTOWN (at the DOS prompt) and later,":_
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
        ROOMDES2$(),CLUE$(),WURD$(),GUESSNUMB,TRYNUMB,LOGIC$
    Print: Print
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
    If ROOM = 2 Or ROOM = 11 Or ROOM = 15 Or ROOM = 13 Or ROOM = 4 Then _Delay 0.3
     IF (ROOM=2 OR ROOM=11 OR ROOM=15) AND FLAG(ROOM)=0 THEN PRINT _
       "Andrea says that she's thinking of a ";LEN(WURD$(ROOM));_
        "-letter word":PRINT _
       "that has something to do with ";CLUE$(ROOM);".":_DELAY 0.3:PRINT:PRINT _
       "If you would like to guess what the word is, type GUESS followed ":_
       PRINT "by your guess, for example,  GUESS COW ":_DELAY 0.3

    THING$ = "YARN"
     IF ROOM=13 AND FNCARRY=1 AND FLAG(12)=0 THEN FLAG(12)=1:PRINT _
      "The cat came in with you and leaped atop the canopy bed just ":PRINT _
      "as the dog was making a lunge for her.  He's barking up a storm,": PRINT_
      "concentrating all of his attention on the cat.":ROOMOBJECT$(13,3)="CAT*":
     IF ROOM=4 AND FNCARRY=1 AND FLAG(3)=0 THEN FLAG(3)=1: PRINT _
      "The cat makes an unsuccessful dive for the yarn ":PRINT _
      "and then scampers away."


    Print

End Sub

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
Sub GAMESTARTER
    Cls
    Print "              WELCOME TO UPTOWN!"
    Print
    Print "     Bear with me because this is a little strange.  Your"
    Print "uncle, Gustav (call me Gus) Braenover, the famous police "
    Print "inspector, has had one of his fits and is in the police"
    Print "sanitarium for a few days.  He'll be okay, but in the interim,"
    Print "it will be up to you, and your good friend Andrea the android,"
    Print "to crack the three major cases he was working on.  If you are"
    Print "unable to gather enough evidence to PRESS CHARGES, the perps"
    Print "will walk.  Fortunately, Uncle Gus left some notes to himself"
    Print "as he walked around his uptown mansion, Braenover Hall.  Read"
    Print "these notes and treat them as true statements in reaching "
    Print "your conclusions.  Andrea the android, although maddeningly"
    Print "absent-minded and fastidious, gives good advice that you've"
    Print "come to rely upon.  "
    Print "     Briefly, the cases boil down to this:  Three suspects,"
    Print "named Ash, Baker, and Carr are currently being held on three"
    Print "separate charges, murder, jewel theft, and blackmail.  The"
    Print "bad news is that only Uncle Gus knows who should be charged"
    Print "with which crime, and he can't talk right now.  Your task is"
    Print "to assemble sufficient clues, including the incriminating "
    Print "letters, the murder weapon, and the stolen jewels, before "
    Print "it's too late!"
    Print "If you're through taking notes, press the Enter key for more."
    Do While Len(InKey$) = 0: Loop
    Cls
    Print
    Print: Print
    Print "                COMMAND INSTRUCTIONS:"
    Print: Print: Print
    Print "    Give COMMANDs as VERB then OBJECT, Such as GO NORTH,"
    Print "READ SIGN, PRESS CHARGES, GET SHARD, FILL GLASS, DROP PISTOL,"
    Print "CUT ROPE, OPEN SAFE, and so forth."
    Print: Print
    Print "    Exceptions to this two-word sentence rule are single-"
    Print "letter commands such as N to GO NORTH, and U to GO UP."
    Print "    P.S. Don't try to get objects ending in an *, e.g., SAFE*,"
    Print "As they are quite unobtainable.  "
    Print: Print
    Print "If you're through taking notes, press the Enter key to begin"
    Do While Len(InKey$) = 0: Loop
    Cls
End Sub

'MODULE 2.1.2 ERASER kept as dummy in case it's referenced somewhere
Sub ERASER
    Rem
End Sub
'MODULE 2.1.3 MESSAGE  AFTER WORD GAME IS COMPLETED
Sub MESSAGE
    Shared ROOM, ROOMOBJECT$()
  IF ROOM=2 THEN PRINT _
   "Andrea says that she's reminded that we will need to find":PRINT _
   "a ring and a necklace and wants to know if you noticed the ":PRINT _
   "locked NIGHTSTAND* in the beige bedroom.":_
   LET ROOMOBJECT$(15,1)="NIGHTSTAND*"
 IF ROOM =11 THEN PRINT _
  "Andrea says that she's reminded that we really ought to try ":PRINT _
  "to water that poor plant and that we'll need to find the ":PRINT _
  "blackmail letters and, oh yes, that she thinks we really ":PRINT _
  "should try to free the poor dog."
 IF ROOM = 15 THEN PRINT _
  "Andrea says that she's reminded to ask you if you noticed ":PRINT _
  "the baling WIRE in the tool room and to tell you that she ":PRINT _
  "thinks we'll have to try to find the gun used in the murders,":PRINT _
  "and, oh yes, wonders if you noticed the KEY in the kitchen.":_
  LET ROOMOBJECT$(3,1)="WIRE":LET ROOMOBJECT$(7,3)="KEY"
End Sub

'MODULE 2.2 COMMANDS
Sub COMMANDS
    SHARED VERB$, OBJECT$,ROOM,DESCRIPTION$(),FLAG(),INVENTORY$(),_
        ROOMOBJECT$(),OBJ$(),OBJ2$(),C$,ANSWER$(),TURNNUMBER,_
        CLUE$(),WURD$(),GUESSNUMB

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

        Let DUMMY$ = "" 'CONVERT TO UPPERCASE
        For I = 1 To Len(C$)
    IF ASC(MID$(C$,I,1))>96 AND ASC(MID$(C$,I,1))<123 THEN_
        LET DUMMY$=DUMMY$+CHR$(ASC(MID$(C$,I,1))-32)_
        ELSE LET DUMMY$=DUMMY$+MID$(C$,I,1)
        Next I
        Let C$ = DUMMY$

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
    IF COUNTER<>0 THEN PRINT "Two words only, please." _
      ELSE DUMMY2=1

    Loop
End Sub

'MODULE 2.3 EVALUATE THE COMMANDS
Sub EVALUATE
    SHARED VERB$, OBJECT$,ROOM,DIRECTION,MOVEMENTTABLE(),NUMBERROOMS,_
        ROOMOBJECT$(),ITEMNUMBER,TURNNUMBER,THING$,INVENTORY$(),FLAG(),_
        GUESSNUMB,CLUE$(),WURD$(),LOGIC$,TRYNUMB,SLAMMER
    If FLAG(1) <> 0 Then Exit Sub
    If FLAG(14) = 1 Then Exit Sub
    Select Case VERB$
        Case "QUIT", "Q"
            Let FLAG(1) = 2 'GAME NOT WON(1) OR LOST(-1):EXIT SELECT
        Case "SAVE"
            If OBJECT$ <> "GAME" Then Print "SAVE GAME": Exit Select
            Open "UPSAV.BAS" For Output As #1
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
            Write #1, GUESSNUMB, LOGIC$, SLAMMER, TRYNUMB
            Close #1
            Print "OK"
            Exit Select
        Case "RESTORE"
            If OBJECT$ <> "GAME" Then Print "Try  RESTORE GAME": Exit Select
            Open "UPSAV.BAS" For Input As #1
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
            Input #1, GUESSNUMB, LOGIC$, SLAMMER, TRYNUMB
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
                PRINT "You can't go that way." ELSE _
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
            Exit Select

        Case "GET", "TAKE"
            IF OBJECT$="BATTERIES" AND FLAG(14)=0 THEN PRINT _
              "Try  REMOVE BATTERIES":EXIT SELECT
            If OBJECT$ = "NOTE*" Then Print "Try  READ NOTE*": Exit Select
            If OBJECT$ = "SIGN*" Then Print "Try  READ SIGN*": Exit Select
            If OBJECT$ = "WATER" Then Print "Try  FILL GLASS  ": Exit Select
            If FNPRESENT = 0 Then Print "I DON'T SEE  "; OBJECT$: Exit Select
            If Right$(OBJECT$, 1) = "*" Then Print "I CAN'T HANDLE IT": Exit Select

            IF FLAG(6)=0 AND OBJECT$="YARN" AND ROOM=10 THEN PRINT _
              "A cat seems to be showing great interest in the yarn.":PRINT _
              "Noticing you noticing her, she scampers off.":FLAG(6)=1

            For I = 1 To 5
                IF INVENTORY$(I)="EMPTY" THEN INVENTORY$(I)=OBJECT$:_
                    PRINT "GOT IT":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY":_
                    EXIT SELECT
            Next I

            Print "You're carrying too much.  Drop something first."
            Exit Select
        Case "PUT", "DROP", "GIVE"
            IF OBJECT$="WATER" OR OBJECT$="GLASS" THEN PRINT _
             "Try   WATER PLANT   ":EXIT SELECT
            THING$ = OBJECT$
            If FNCARRY = 0 Then Print "You don't have the "; OBJECT$: Exit Select


            For J = 1 To 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":_
                    EXIT SELECT
            Next J
            Print "This room is full; take it elsewhere": Exit Select

        Case "READ"
            IF ROOM=7 THEN PRINT _
             "The sign says that the kitchen is under repair and":PRINT_
             "to get water from the bathroom if you need it.":EXIT SELECT
            IF ROOM=9 THEN PRINT _
             "You'll need to gather 4 vital pieces of evidence and ":PRINT _
             "DROP them here.  You'll also need to put together 4 ":PRINT _
             "additional clues before you are ready to PRESS CHARGES ":_
             EXIT SELECT
            Print "The inspector's note reads:"
            IF ROOM = 1 THEN PRINT _
             "If Ash is the murderer then the murderer is older than":PRINT_
             "I am.":LET FLAG(18)=1:EXIT SELECT
            IF ROOM=4 THEN PRINT _
             "If Ash is the murderer then COUGH TWICE ":EXIT SELECT
            IF ROOM=6 THEN PRINT _
             "If Ash is the murderer then the office is north of the hall.":_
             LET FLAG(22)=1:EXIT SELECT
            IF ROOM=8 THEN PRINT _
             "Ash is 5 feet 9 inches tall and the murderer is younger":_
             PRINT "than I am.":LET FLAG(19)=1:EXIT SELECT
            IF ROOM=10 THEN PRINT _
             "If Carr is not the murderer then SNEEZE ONCE ":EXIT SELECT
            IF ROOM=12 THEN PRINT _
             "If the kitchen is north of the dining room then ":PRINT _
             "Carr is the murderer.":LET FLAG(23)=1:EXIT SELECT
            IF ROOM=14 THEN PRINT _
             "If Ash is not the murderer then Baker isn't either.":_
             LET FLAG(21)=1:EXIT SELECT
            IF ROOM=16 THEN PRINT _
             "Either Ash is over 6 feet tall or Baker is not the jewel thief.":_
             LET FLAG(20)=1:EXIT SELECT
            Print "Andrea says that we're in the wrong room for that."
            Exit Select

        Case ",DRINK", "IMBIBE"
            Print "Andrea says that you are not thirsty.": Exit Select
        Case "GUESS"
        IF ROOM<>2 AND ROOM<>11 AND ROOM<>15 THEN PRINT _
         "Andrea says that this isn't the place for that.":EXIT SELECT
        IF FLAG(ROOM)=1 THEN PRINT _
         "Andrea says that you're done with this one.":EXIT SELECT
        IF LEN(OBJECT$)<>LEN(WURD$(ROOM)) THEN PRINT _
          "Andreas says that she's thinking of a ";LEN(WURD$(ROOM));_
          "-letter word.  Try again.":EXIT SELECT
            GUESSNUMB = GUESSNUMB + 1
        IF OBJECT$=WURD$(ROOM) THEN FLAG(ROOM)=1:_
          CALL MESSAGE:EXIT SELECT
            HITS = 0
            For I = 1 To Len(OBJECT$)
                Let DUMMY$ = Mid$(OBJECT$, I, 1)
                For J = 1 To Len(WURD$(ROOM))
          IF DUMMY$=MID$(WURD$(ROOM),J,1) THEN _
           HITS=HITS+1:EXIT FOR
                Next J
            Next I
        PRINT "Andrea says that ";HITS;" letters of your word ":PRINT _
              "are also found in her word.  Try again."
            Exit Select
        Case "PLAY"
         IF ROOM=6 AND OBJECT$="RADIO*" THEN PRINT _
           "Sounds fine. ":EXIT SELECT
            Print "Andrea warns that we have no time for such foolishness."
            Exit Select
        Case "COUGH"
         IF FLAG(22)=0 THEN PRINT _
          "Andrea thinks you're jumping to conclusions.":EXIT SELECT
         PRINT "You may have fallen for an affirming the ":_
         PRINT "consequent fallacy.  Check your text.":_
         LOGIC$=LOGIC$+" AFFIRM":EXIT SELECT
        Case "SNEEZE"
         IF FLAG(23)=0 THEN PRINT_
          "You need more information to reach that conclusion.":EXIT SELECT
         PRINT "You may have fallen for a denying the antecedent":_
         PRINT "fallacy.  Check your text.":_
         LOGIC$=LOGIC$+" DENY":EXIT SELECT
        Case "WATER"
         IF ROOM<>11 THEN PRINT _
          "Andrea reminds you that the plant is in the office.":EXIT SELECT
            THING$ = "GLASS"
        IF FNCARRY=0 THEN PRINT _
         "Andrea thinks you should get the glass first.":EXIT SELECT
       IF FLAG(7)=0  THEN PRINT _
         "Andrea thinks you should FILL GLASS with water first.":EXIT SELECT
            Let ROOMOBJECT$(11, 3) = "SAFE*"
            Let ROOMOBJECT$(11, 5) = "RUBBLE*"
            Let ROOMOBJECT$(11, 6) = "SHARD"
       PRINT "The glass slips from your hand, breaking as you try to water":PRINT_
        "the plant.  The glass breaks into one sharp shard and a bunch of":PRINT_
        "rubble.  Fortunately, the water fell on the plant.  Gadzooks! ":PRINT_
        "The crazy plant, having absorbed the water is regaining its":PRINT_
        "original shape!  One branch just bumped the painting, tipping":PRINT_
        "it enough to reveal a hidden wall safe behind.  That was cool!" :
            For I = 1 To 5
                If INVENTORY$(I) = "GLASS" Then INVENTORY$(I) = "EMPTY"
            Next I
            Exit Select
        Case "FILL"
         IF OBJECT$<>"GLASS" THEN PRINT _
           "Andrea thinks the only thing to fill is the glass.":EXIT SELECT
            If ROOM <> 5 Then Print "This isn't the place for that.": Exit Select
            THING$ = "GLASS"
            If FNCARRY = 0 Then Print "Get the glass first.": Exit Select
            If FLAG(7) = 2 Then Print "Already accomplished.": Exit Select
            Print "You now have a full glass of water!": FLAG(7) = 1
            Exit Select
        Case "OPEN"
            If OBJECT$ = "CAR*" Or OBJECT$ = "CAR" Then Print "Try  UNLOCK CAR*": Exit Select
         IF OBJECT$="NIGHTSTAND*" THEN PRINT _
           "Didn't work.  Andrea says to try to PICK LOCK":EXIT SELECT
            If OBJECT$ <> "SAFE*" Then Print "Can't.": Exit Select
         PRINT "Andrea notices that the safe is a 3-dial tumbler lock.":PRINT_
         "Each dial can be set to either 1, 2, or 3.  To try to":PRINT _
         "open the safe, type TRY followed by the combination, e.g.,":PRINT_
         "TRY 121 or TRY 213 until it opens.  Listen closely to ":PRINT_
         "hear the number of tumblers that fall with each try."
            Exit Select
        Case "BREAK"
            Print "Andrea is sulking.  She hates violence."
            Exit Select
        Case "TRY"
            If ROOM <> 11 Then Print "Go to the office first.": Exit Select
            COMBO$ = "231"
            If FLAG(9) = 1 Then Print "The safe is already open.": Exit Select
            HITS = 0
            For I = 1 To 3
                If I = Val(Left$(OBJECT$, 1)) Then HITS = HITS + 1
            Next I
            If HITS = 0 Or Len(OBJECT$) <> 3 Then Print "TRY a 3-digit number.": Exit Select
            TRYNUMB = TRYNUMB + 1
            Print "Andrea spins the dials and tries "; OBJECT$
         IF OBJECT$=COMBO$ THEN PRINT _
          "The door of the safe pops open, revealing the blackmail letters.":_
          ROOMOBJECT$(11,4)="LETTERS":FLAG(9)=1:EXIT SELECT
            HITS = 0
            For I = 1 To 3
                Let DUMMY$ = Mid$(OBJECT$, I, 1)
                If DUMMY$ = Mid$(COMBO$, I, 1) Then HITS = HITS + 1
            Next I
            Print "The safe didn't open, but Andrea heard "; HITS; " tumbler(s) fall."
            Exit Select
        Case "DIG"
            Print "Can't.  Get some help."
            Exit Select
        Case "USE"
            Print "To do what?  Be more specific."
            Exit Select
        Case "FREE", "UNTIE"
         IF ROOM<>13 THEN PRINT "You are in the wrong room for that.":_
           EXIT SELECT
            If FLAG(5) = 1 Then Print "The dog is long gone.": Exit Select
         IF FLAG(4)=1 THEN PRINT _
          "For some reason, you back up when the dog growls.":EXIT SELECT
         FLAG(4)=1:PRINT _
          "Andrea has an idea.  She says she noticed a piece ":PRINT_
          "of yarn in the library what might help."
            ROOMOBJECT$(10, 2) = "YARN"
            Exit Select
        Case "CUT", "SLICE"
            If ROOM <> 13 Then Print "Not here.": Exit Select
            If OBJECT$ <> "ROPE" Then Print "Try  CUT ROPE ": Exit Select
            If FLAG(5) = 1 Then Print "The dog, as they say, has split.": Exit Select
         IF FLAG(12)=0 THEN PRINT _
          "Andrea pulls you back just as the dog's teeth close ":PRINT_
          "on your hand.  That was close. Whew!":EXIT SELECT
            THING$ = "SHARD"
            If FNCARRY = 0 Then Print "Get something to cut with.": Exit Select
         IF FLAG(12)=1 THEN PRINT _
          "As soon as the dog realized he was free, he bounded":PRINT_
          "out the door with the cat in hot pursuit.  A few":PRINT_
          "seconds later, you hear a loud crash!":FLAG(5)=1
            Let ROOMOBJECT$(13, 3) = "EMPTY"
            Let ROOMOBJECT$(13, 2) = "EMPTY"
            Let ROOMOBJECT$(4, 1) = "EMPTY"
            Let ROOMOBJECT$(4, 2) = "RING"
            Let ROOMOBJECT$(4, 3) = "RUBBLE*"
            Let ROOMOBJECT$(1, 1) = "PISTOL"
            Let ROOMOBJECT$(1, 2) = "HOLES*"
            Exit Select
        Case "UNLOCK"
            If ROOM <> 2 Then Print "Try  PICK  or  OPEN": Exit Select
            If FLAG(13) = 1 Then Print "It's been done, as they say.": Exit Select
            THING$ = "KEY"
         IF FNCARRY=0 THEN PRINT _
          "Get the key first.":EXIT SELECT
         FLAG(13)=1: PRINT _
          "The car trunk opens, revealing an empty flashlight. ":PRINT_
          "Andrea doesn't see what good that will do."
            ROOMOBJECT$(2, 2) = "FLASHLIGHT"
            Exit Select
        Case "PICK"
            If ROOM <> 15 Then Print "Not here.": Exit Select
            If FLAG(24) = 1 Then Print "We've finished with that.": Exit Select
            THING$ = "WIRE"
            If FNCARRY = 0 Then Print "You'll need a piece of wire.": Exit Select
         PRINT "After Andrea tried and failed (you need the touch),":PRINT_
          "you manage to wiggle the piece of wire in the lock ":PRINT_
          "of the nightstand opening it.  Inside you see a":PRINT_
          "necklace that must be worth a fortune. Wow!":
            Let ROOMOBJECT$(15, 2) = "NECKLACE": FLAG(24) = 1: Exit Select
        Case "REMOVE"
            If FLAG(14) = 1 Then Print "That's been done already.": Exit Select
            If ROOM <> 6 Then Print "This is hardly the place.": Exit Select
         FLAG(14)=1:ROOMOBJECT$(6,3)="BATTERIES":PRINT_
          "Andrea says we can now LOAD FLASHLIGHT. Duh."
            Exit Select
        Case "UNLOAD"
            Print "Andrea doesn't like that idea.": Exit Select
        Case "LOAD"
         IF OBJECT$<>"FLASHLIGHT" AND OBJECT$<>"BATTERIES" THEN _
          PRINT "Andrea says you shouldn't do something you'll regret.":_
          EXIT SELECT
            If FLAG(16) = 1 Then Print "You're becoming repetitive.": Exit Select
            THING$ = "FLASHLIGHT"
            If FNCARRY = 0 Then Print "Get the flashlight.": Exit Select
            OBJECT$ = "BATTERIES"
            If FNPRESENT = 0 Then Print "I don't see the batteries.": Exit Select
            ROOMOBJECT$(6, 3) = "EMPTY"
         FLAG(16)=1:ROOMOBJECT$(14,2)="BUTTON*":PRINT_
          "The flashlight now shines with a bright beam of light.":PRINT_
          "Andrea wonders why it has no on-off switch.  Weird.":PRINT_
          "Oh yes, Andrea says that she is reminded that she":PRINT_
          "wonders if you noticed the wall button in the pink":PRINT_
          "bedroom.  Have we been in the pink bedroom?  Who knows?"
            Exit Select
        Case "TURN", "SWITCH"
            Print "Somehow, that didn't work."
            Exit Select
        Case "PUSH"
         IF FLAG(16)=0 THEN PRINT _
          "Andrea says she hates people who push or press too much.":EXIT SELECT
            If ROOM <> 14 Then Print "Not here.": Exit Select
            If FLAG(17) = 1 Then Print "Nothing else happens.": Exit Select
         PRINT "You hear the sound of a motor and see a ladder ":PRINT_
          "descend from an opening in the ceiling.  Andrea says":PRINT_
          "that if we have a light we can CLIMB LADDER to the attic."
            FLAG(17) = 1: ROOMOBJECT$(14, 3) = "LADDER*"
            Exit Select
        Case "CLIMB"
            If ROOM <> 14 Then Print "This isn't the place for that.": Exit Select
            If FLAG(17) = 0 Then Print "PUSH BUTTON first.": Exit Select
            THING$ = "FLASHLIGHT"
          IF FNCARRY=0 THEN PRINT _
           "You'll need to carry the flashlight.":EXIT SELECT
            Print "Upsy daisy.": ROOM = 16
            Exit Select

        Case "PRESS"
        IF OBJECT$<>"CHARGES" THEN PRINT _
         "Try PRESS CHARGES or PUSH BUTTON":EXIT SELECT
        IF ROOM<>9 THEN PRINT _
         "Go to the entryway first.":EXIT SELECT
            Dim DUMMY3$(4)
            Let DUMMY3$(1) = "LETTERS"
            Let DUMMY3$(2) = "NECKLACE"
            Let DUMMY3$(3) = "RING"
            Let DUMMY3$(4) = "PISTOL"
            For I = 1 To 4
                Let OBJECT$ = DUMMY3$(I)
          IF FNPRESENT=0 THEN PRINT _
           "DROP the ";OBJECT$;" here first.":EXIT SELECT
            Next I
            Data LETTERS,NECKLACE,RING,PISTOL
            HITS = 0
            For I = 18 To 21
                If FLAG(I) = 1 Then HITS = HITS + 1
            Next I
       IF HITS<4 THEN PRINT _
        "You still have one or more notes to ponder.":EXIT SELECT
       SOLUTION$="CAB":PRINT _
         "Enter the solution by first letter of last name in the":PRINT_
         "following order: murderer then jewel thief then blackmailer.":PRINT_
         "For example, typing ABC means you think Ash was the ":PRINT_
         "murderer, Baker the jewel thief, and Carr the blackmailer."
            DUMMY$ = ""
            Do Until Len(DUMMY$) = 3
                Input "Enter a 3-letter answer (e.g., ABC), please. "; DUMMY$
            Loop
            Let DUMMY$ = UCase$(DUMMY$)
            SLAMMER = SLAMMER + 1
       IF SOLUTION$=DUMMY$ THEN FLAG(1)=1:PRINT _
        "Charges are pressed and the rascals are taken to the hoosgow.":_
        EXIT SELECT
            Let DUMMY2$ = Left$(DUMMY$, 1)
       IF DUMMY2$="C" THEN PRINT _
        "You may have fallen for a denying the antecedent fallacy.":_
        LOGIC$=LOGIC$+" DENY+ ":EXIT SELECT
       IF DUMMY2$="B" THEN PRINT_
        "You may have failed to follow an affirming the antecedent":PRINT_
        "syllogism.":LOGIC$=LOGIC$+" affirm-":EXIT SELECT
            Let DUMMY2$ = Mid$(DUMMY$, 2, 1)
       IF DUMMY2$="B" THEN PRINT _
        "You may have misread a disjunctive syllogism.  Go over your notes."
            LOGIC$ = LOGIC$ + " DISJUNCT": Exit Select


        Case Else
            Print "Andrea says to try another verb.": Exit Select
    End Select
End Sub

'MODULE 2.3.1 DEFINE THE FUNCTION- IS THE OBJECT PRESENT?
Function FNPRESENT
    Shared ROOMOBJECT$(), OBJECT$, ITEMNUMBER
    For J = 1 To 15
             IF ROOMOBJECT$(ROOM,J)=OBJECT$ THEN _
                 FNPRESENT=1:ITEMNUMBER=J:EXIT FUNCTION
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
    Shared FLAG(), TURNNUMBER, ANSWER$(), STRT, GUESSNUMB, LOGIC$, TRYNUMB, SLAMMER
    Print
    If FLAG(1) = 1 Then Print " *****   HOORAY FOR YOU!!  YOU'VE WON THE GAME!!  ****"
    Print
    For I = 1 To 6: Print: Next I
    Input "Be sure that your disk is in the drive and press ENTER. OK"; DUMMY$

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

    '    Let DTA(11) = FLAG(1): Let DTA(36) = DTA(36) + Int((Timer - STRT) / 6)
    '    Open "REPORT.DTA" For Output As #1
    '    Rem SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
    '    For I = 1 To 40
    '        Write #1, DTA(I)
    '    Next I
    '    For I = 1 To 10
    '        Write #1, DTA$(I)
    '    Next I
    '    Close #1

    Open "UPDATA.TXT" For Append As #2
    Print #2, Time$, GUESSNUMB, LOGIC$
    Print #2, TRYNUMB, SLAMMER, ANSWER$(0)
    For I = 1 To TURNNUMBER
        If Int(I / 5) = I / 5 Then Print #2,
        Print #2, ANSWER$(I),
    Next I
    Print #2, Int((STRT - Timer) / 6), Int(1000 * Rnd(0))
    Close #2
    Print "This game is over, type  UPTOWN  to play again."
End Sub

'END OF PROGRAM
