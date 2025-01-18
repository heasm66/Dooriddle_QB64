'MODULE 0 MAIN    ******HANGTOWN******  10-3-90
'MULTIPLE REVISIONS ON 1-25-91
'GOLD HINT BURN HINT KEY HINT REVEAL HINT RESTORE 30 6MAR92
'TIMER AND STRT CHANGE  2-3-94
'TIME$, DATE$, AND SAVE GAME FIX 7-30-95
'SNIp fixes 1-3-96
'REPORT STUFF 3-5-96  HANGDATA.TXT 10-24-96
'7-1-23, Henrik Aasman: Modified for QB64
'  Moved all definition of global (shared) variables to Main module
'  QB64 needs unique names so MOVEMENTTABLE --> MOVEMENTTABLESUB and INVENTORY --> INVENTORYSUB
'  DATA don't allow split lines with _ in B64
'  Changed syntax for BEEP so BEEP 3 --> BEEP
'  Changed syntax for DELAY so DELAY --> _DELAY
'  Fixed __ to _
'  Renamed DEF to FUNCTION
'  Removed reading/writing to REPORT.DTA
'  Added "" around some of the data in DATA-tables

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
ReDim Shared VERB$
ReDim Shared OBJECT$
ReDim Shared C$
ReDim Shared CONVERTNUM
ReDim Shared DIRECTION
ReDim Shared ITEMNUMBER
ReDim Shared STRT

Cls: ReDim FLAG(30), OBJ$(18), OBJ2$(18)
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
    Data 33,"LEDGE just inside WELL*","RATTLESNAKE WELLS","HANGTOWN SQUARE","SCAFFOLD","SANDY DESERT"
    Data "LAST CHANCE SALOON","STREET","HARDROCK JAIL","STEEP'S TEA ROOM","STREET","GOLDEN WELL* HOTEL"
    Data "DOCTOR'S OFFICE","STREET","LIVERY STABLE","PASTURE","STREET","LIMPING HORSE CORRAL"
    Data "MINER'S SHACK","STREET","FIELD","TOP OF BOULDER","CELLAR","MINESHAFT","MINESHAFT"
    Data "MINESHAFT","MINESHAFT","MINESHAFT","MINESHAFT","MINESHAFT","MINESHAFT","MINESHAFT"
    Data "MINESHAFT","RANGE"

    For I = 1 To NUMBERROOMS
        Let DESCRIBEFLAG(I) = 1
        Let ROOMDESCRIBE$(I) = "EMPTY"
        Let ROOMDES2$(I) = "EMPTY"
    Next I
    For I = 1 To 15
        Read DESCRIBEROOM
        Let DESCRIBEFLAG(DESCRIBEROOM) = 0
        Read ROOMDESCRIBE$(DESCRIBEROOM)
        Read ROOMDES2$(DESCRIBEROOM)
    Next I
    Data 2,"Your blood chills with fear at the sight of the huge and horrible"
    Data "RATTLESNAKE guarding the WELL.  You know that one of you must die!"
    Data 3,"Curiously, you find yourself in the TOWN SQUARE of a dry, dusty,"
    Data "apparently deserted gold rush town."
    Data 4,"You stand at the top of a gruesome SCAFFOLD.  The hangperson"
    Data "left his/her well-worn noose here. Is this a PC PC or what?"
    Data 5,"The DESERT seems to stretch out forever to the EAST."
    Data "It's hard to walk in the soft sand."
    Data 6,"As you enter the aroma that greets you isn't the stale smoke and "
    Data "whiskey smell you expected.  You feel magic in the air here."
    Data 7,"From the STREET you can see the town JAIL to the EAST.      "
    Data "You wonder why the JAIL door was left locked."
    Data 9,"An elegantly-dressed but crabby-looking ENGLISHMAN looks at you as"
    Data "you enter the TEA ROOM, then turns away.  He needs a nice CUP of TEA."
    Data 10,"All you can see to the SOUTH are hoof prints and wagon ruts,"
    Data "frozen forever in the hard-baked dirt of the town STREET."
    Data 11,"The curiously named HOTEL looks pretty run down.  "
    Data "Hopefully, you won't have to spend the night here."
    Data 12,"You can almost hear the elevator music in the DOCTOR'S OFFICE. "
    Data "You see all of the earliest issues of INVESTMENT MAGAZINE."
    Data 13,"The ROCK you see in the STREET could do a lot of damage if"
    Data "thrown accurately."
    Data 14,"With HAY on the floor, the LIVERY STABLE looks more comfortable"
    Data "than the HOTEL.  There is a small milk-pail here, but no COW."
    Data 15,"Here in the PASTURE is the most pathetic COW you've ever seen. "
    Data "GET her some food before it's too late!"
    Data 17,"Here in the LIMPING HORSE CORRAL you see some SUGAR cubes that"
    Data "are still in good condition.  Horses love 'em."
    Data 20,"Here in the FIELD is a huge BOULDER with sheer walls.  To"
    Data "CLIMB it, You'll need something to stand upon."
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
    Data 0,0,0,0,2,0,0,0,3,0,0,0,0,7,5,2,4,0
    Data 0,0,0,0,0,3,0,0,5,3,0,0,0,0,7,0,0,0
    Data 3,10,8,6,0,0,0,0,0,7,0,0,0,0,10,0,0,0
    Data 7,13,11,9,0,0,0,0,0,10,0,0,0,0,13,0,0,0
    Data 10,16,14,12,0,0,0,0,0,13,0,0,0,0,16,0,0,0
    Data 13,19,17,15,0,0,0,0,0,16,0,0,0,0,19,0,0,0
    Data 16,33,20,18,0,0,0,0,0,19,0,0,0,0,0,0,0,20
    Data 0,0,0,0,18,25,0,27,24,0,0,0,0,28,25,23,0,0
    Data 0,29,26,24,22,0,0,30,0,25,0,0,23,31,28,0,0,0
    Data 24,0,28,27,0,0,25,32,30,28,0,0,26,0,0,29,0,0
    Data 27,0,0,0,0,0,29,0,0,0,0,0,19,0,0,0,0,0
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
    SHARED GAMESTATE$,ROOM,TURNNUMBER,ROOMOBJECT$(),THING$,INVENTORY$()
    SHARED FLAG(),OBJ$(),OBJ2$(),DESCRIBEFLAG(),ROOMDESCRIBE$(),ROOMDES2$()
    SHARED CONVERTNUM
    Let ROOM = 3 'START IN HANGTOWN SQUARE
    Let TURNNUMBER = 0
    DANGEROUS = 1
    Let THING$ = ""
    For I = 1 To 30: Let FLAG(I) = 0: Next I 'SET FLAGS TO 0
    'FLAG #  MEANING OF 0      FLAG# MEANING OF 0
    ' 1      GAME NOT OVER       2   DOG NOT GONE
    'FOR 1, 1 MEANS GAME WON -1 GAME LOST 2 MEANS GAME QUIT
    ' 3      SNAKE NOT KILLED    4   TEA NOT MADE
    ' 5      COW NOT FED         6   JAIL DOOR LOCKED
    ' 7      LANTERN NOT CARRIED 8   KITTEN NOT FED
    ' 9      THORN NOT OUT      10   KETTLE NOT FILLED
    '11      BUCKET EMPTY       12   ROPE NOT TIED TO BUCKET
    '13    SAFE HINGE NOT OILED  14  NOT CHASED BY DOG
    '14 IS USED TO EXIT TURN BUT NOT EXIT THE GAME
    '15 SAFE NOT OPENED          16 SILVER NOT DUG
    '17 KEY NOT DUG              18 EMERALD NOT DROPPED
    '19 ROOM NOT DANGEROUS       20 NEVER BEEN TO LANTERN ROOM22
    '21 COYOTE HINT NOT GIVEN    22 COFFEE NOT NAMED
    '23 PAIL NOT FILLED WITH MILK 24 2ND DOG HINT NOT GIVEN
    '25 GOLD HINT NOT GIVEN       26 KEY HINT NOT GIVEN
    Let ROOMOBJECT$(1, 1) = "GOLD!" 'STARTING LOCATIONS FOR OBJECTS
    Let ROOMOBJECT$(2, 1) = "WELL*"
    Let ROOMOBJECT$(2, 2) = "SNAKE"
    Let ROOMOBJECT$(2, 3) = "BUCKET"
    Let ROOMOBJECT$(3, 1) = "SIGN*"
    Let ROOMOBJECT$(4, 1) = "ROPE"
    Let ROOMOBJECT$(5, 1) = "EMPTYKEY"
    Let ROOMOBJECT$(6, 1) = "KETTLE"
    Let ROOMOBJECT$(6, 2) = "SIGN*"
    Let ROOMOBJECT$(6, 3) = "EMPTYMIRROR*"
    Let ROOMOBJECT$(6, 4) = "EMPTYSILVER!"
    Let ROOMOBJECT$(7, 1) = "DOOR*"
    Let ROOMOBJECT$(8, 1) = "CUP"
    Let ROOMOBJECT$(9, 1) = "STOVE*"
    Let ROOMOBJECT$(9, 2) = "SIGN*"
    Let ROOMOBJECT$(9, 4) = "CHAIR"
    Let ROOMOBJECT$(9, 5) = "EMPTYEMERALD!"
    Let ROOMOBJECT$(10, 1) = "EMPTYRUBY!"
    Let ROOMOBJECT$(11, 1) = "SAFE*"
    Let ROOMOBJECT$(11, 2) = "EMPTYJADE!"
    Let ROOMOBJECT$(11, 3) = "MATCHES"
    Let ROOMOBJECT$(12, 1) = "MAGAZINES"
    Let ROOMOBJECT$(12, 2) = "TWEEZERS"
    Let ROOMOBJECT$(12, 3) = "MINERAL-OIL"
    Let ROOMOBJECT$(13, 1) = "ROCK"
    Let ROOMOBJECT$(14, 1) = "SHOVEL"
    Let ROOMOBJECT$(14, 2) = "HAY"
    Let ROOMOBJECT$(14, 3) = "PAIL"
    Let ROOMOBJECT$(15, 1) = "COW*"
    Let ROOMOBJECT$(17, 1) = "SUGAR"
    Let ROOMOBJECT$(18, 1) = "TEA"
    Let ROOMOBJECT$(18, 2) = "CELLAR*"
    Let ROOMOBJECT$(20, 1) = "BOULDER*"
    Let ROOMOBJECT$(21, 1) = "MAP"
    Let ROOMOBJECT$(22, 1) = "LANTERN"
    Let ROOMOBJECT$(31, 1) = "EMPTYDIAMONDS!"
    Let ROOMOBJECT$(33, 1) = "EMPTYTURQUOISE!"

    Let CONVERTNUM = 18
    For I = 1 To CONVERTNUM 'READ IN WORD!* CONVERSIONS
        Read OBJ$(I), OBJ2$(I)
    Next I
'MODULE 1.5 OTHERS
Sub OTHERS
    SHARED GAMESTATE$,ROOM,TURNNUMBER,ROOMOBJECT$(),THING$,INVENTORY$()
    SHARED FLAG(),OBJ$(),OBJ2$(),DESCRIBEFLAG(),ROOMDESCRIBE$(),ROOMDES2$()
    SHARED CONVERTNUM
    Let ROOM = 3 'START IN HANGTOWN SQUARE
    Let TURNNUMBER = 0
    DANGEROUS = 1
    Let THING$ = ""
    For I = 1 To 30: Let FLAG(I) = 0: Next I 'SET FLAGS TO 0
    'FLAG #  MEANING OF 0      FLAG# MEANING OF 0
    ' 1      GAME NOT OVER       2   DOG NOT GONE
    'FOR 1, 1 MEANS GAME WON -1 GAME LOST 2 MEANS GAME QUIT
    ' 3      SNAKE NOT KILLED    4   TEA NOT MADE
    ' 5      COW NOT FED         6   JAIL DOOR LOCKED
    ' 7      LANTERN NOT CARRIED 8   KITTEN NOT FED
    ' 9      THORN NOT OUT      10   KETTLE NOT FILLED
    '11      BUCKET EMPTY       12   ROPE NOT TIED TO BUCKET
    '13    SAFE HINGE NOT OILED  14  NOT CHASED BY DOG
    '14 IS USED TO EXIT TURN BUT NOT EXIT THE GAME
    '15 SAFE NOT OPENED          16 SILVER NOT DUG
    '17 KEY NOT DUG              18 EMERALD NOT DROPPED
    '19 ROOM NOT DANGEROUS       20 NEVER BEEN TO LANTERN ROOM22
    '21 COYOTE HINT NOT GIVEN    22 COFFEE NOT NAMED
    '23 PAIL NOT FILLED WITH MILK 24 2ND DOG HINT NOT GIVEN
    '25 GOLD HINT NOT GIVEN       26 KEY HINT NOT GIVEN
    Let ROOMOBJECT$(1, 1) = "GOLD!" 'STARTING LOCATIONS FOR OBJECTS
    Let ROOMOBJECT$(2, 1) = "WELL*"
    Let ROOMOBJECT$(2, 2) = "SNAKE"
    Let ROOMOBJECT$(2, 3) = "BUCKET"
    Let ROOMOBJECT$(3, 1) = "SIGN*"
    Let ROOMOBJECT$(4, 1) = "ROPE"
    Let ROOMOBJECT$(5, 1) = "EMPTYKEY"
    Let ROOMOBJECT$(6, 1) = "KETTLE"
    Let ROOMOBJECT$(6, 2) = "SIGN*"
    Let ROOMOBJECT$(6, 3) = "EMPTYMIRROR*"
    Let ROOMOBJECT$(6, 4) = "EMPTYSILVER!"
    Let ROOMOBJECT$(7, 1) = "DOOR*"
    Let ROOMOBJECT$(8, 1) = "CUP"
    Let ROOMOBJECT$(9, 1) = "STOVE*"
    Let ROOMOBJECT$(9, 2) = "SIGN*"
    Let ROOMOBJECT$(9, 4) = "CHAIR"
    Let ROOMOBJECT$(9, 5) = "EMPTYEMERALD!"
    Let ROOMOBJECT$(10, 1) = "EMPTYRUBY!"
    Let ROOMOBJECT$(11, 1) = "SAFE*"
    Let ROOMOBJECT$(11, 2) = "EMPTYJADE!"
    Let ROOMOBJECT$(11, 3) = "MATCHES"
    Let ROOMOBJECT$(12, 1) = "MAGAZINES"
    Let ROOMOBJECT$(12, 2) = "TWEEZERS"
    Let ROOMOBJECT$(12, 3) = "MINERAL-OIL"
    Let ROOMOBJECT$(13, 1) = "ROCK"
    Let ROOMOBJECT$(14, 1) = "SHOVEL"
    Let ROOMOBJECT$(14, 2) = "HAY"
    Let ROOMOBJECT$(14, 3) = "PAIL"
    Let ROOMOBJECT$(15, 1) = "COW*"
    Let ROOMOBJECT$(17, 1) = "SUGAR"
    Let ROOMOBJECT$(18, 1) = "TEA"
    Let ROOMOBJECT$(18, 2) = "CELLAR*"
    Let ROOMOBJECT$(20, 1) = "BOULDER*"
    Let ROOMOBJECT$(21, 1) = "MAP"
    Let ROOMOBJECT$(22, 1) = "LANTERN"
    Let ROOMOBJECT$(31, 1) = "EMPTYDIAMONDS!"
    Let ROOMOBJECT$(33, 1) = "EMPTYTURQUOISE!"

    Let CONVERTNUM = 18
    For I = 1 To CONVERTNUM 'READ IN WORD!* CONVERSIONS
        Read OBJ$(I), OBJ2$(I)
    Next I
    Data GOLD,GOLD!,WELL,WELL*,SIGN,SIGN*,DOOR,DOOR*,STOVE,STOVE*,EMERALD
    Data EMERALD!,RUBY,RUBY!,DIAMONDS,DIAMONDS!,COW,COW*,CELLAR,CELLAR*
    Data BOULDER,BOULDER*,JADE,JADE!,SILVER,SILVER!,TURQUOISE,TURQUOISE!
    Data MINERAL,MINERAL-OIL,OIL,MINERAL-OIL,SAFE,SAFE*,MIRROR,MIRROR*
End Sub

'MODULE 2 TURN
Sub TURN
    Shared FLAG(), TURNNUMBER
    Do Until FLAG(1) <> 0
        FLAG(14) = 0
        TURNNUMBER = TURNNUMBER + 1
        If TURNNUMBER=295 THEN 
            BEEP
            PRINT "Type   SAVE GAME  and then  QUIT"
            PRINT "You can then return to this spot by typing"
            PRINT "HANGTOWN (at the DOS prompt) and later,"
            PRINT "as a command, RESTORE GAME"
            PRINT "Please forgive the necessary inconvenience."
        End If
        If TURNNUMBER = 300 Then FLAG(1) = 2
        Call DESCRIBE 'MODULE 2.1 DESCRIBE THE ROOM
        Call COMMANDS 'MODULE 2.2 INPUT THE COMMANDS
        Call EVALUATE 'MODULE 2.3 EVALUATE COMMANDS
    Loop
End Sub

'MODULE 2.1 DESCRIBE THE CURRENT ROOM
Sub DESCRIBE
    SHARED DESCRIPTION$(),ROOM,MOVEMENTTABLE(),ROOMOBJECT$(),FLAG()
    SHARED TURNNUMBER,INVENTORY$(),THING$,OBJECT$,DESCRIBEFLAG(),ROOMDESCRIBE$()
    SHARED ROOMDES2$()

    If TURNNUMBER = 1 Then Call GAMESTARTER 'MODULE 2.1.1
    IF DESCRIBEFLAG(ROOM)=0 THEN 
        PRINT ROOMDESCRIBE$(ROOM)
        PRINT ROOMDES2$(ROOM):DESCRIBEFLAG(ROOM)=1
    ElseIF DESCRIBEFLAG(ROOM)=1 AND DESCRIPTION$(ROOM)="STREET" THEN
        PRINT "You find yourself in the STREET"
    ElseIF DESCRIBEFLAG(ROOM)=1 AND DESCRIPTION$(ROOM)<>"STREET" THEN 
        PRINT "You find yourself at the ";:PRINT DESCRIPTION$(ROOM)
    End If
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
        IF LEFT$(STUFF$,5)<>"EMPTY" THEN 
            PRINT "   ";ROOMOBJECT$(ROOM,I);
            COUNTER=COUNTER+1
        End If
    Next I
    If COUNTER = 0 Then Print "noticeably absent!";
    Print
    _Delay 0.3
    Print "You are carrying: ";
    COUNTER = 0
    For I = 1 To 5
        IF INVENTORY$(I)<>"EMPTY" THEN 
            COUNTER=COUNTER + 1
            PRINT INVENTORY$(I);"   ";
        End If
    Next I
    If COUNTER = 0 Then Print "nothing at all."
    Print
    _Delay 0.3
    OBJECT$ = "SNAKE"
        IF FLAG(21)=0 AND TURNNUMBER>90 AND FLAG(2)=0 AND FNPRESENT=1 THEN
            PRINT "A coyote strolls by, sees the dead SNAKE and runs away"
            FLAG(21)=1
        End If
        IF FLAG(24)=0 AND FLAG(21)=1 AND DESCRIPTION$(ROOM)="STREET" AND TURNNUMBER>120 THEN
            FLAG(24)=1
            PRINT "A tourist tells you his dog ran off after seeing a dead SNAKE."
        End If
    IF ROOM =22 AND FLAG(20)=0 THEN 
        PRINT "The glowing LANTERN is quite bright"
        FLAG(20)=1
    End If
    IF ROOM = 10 AND FLAG(4)=1 AND FLAG(8)=0 THEN PRINT "There is a shy and hungry KITTEN here"
    If ROOM = 9 And FLAG(4) = 0 Then Print "There is an unhappy ENGLISHMAN* here"
    OBJECT$ = "SUGAR"
        IF ROOM=33 AND FLAG(4)=1 AND FNPRESENT = 0 AND FLAG(9)=0 THEN
            PRINT "You see a limping HORSE. You'll need a bribe to lure him over."
        End If
        IF ROOM = 33 AND FLAG(4)=1 AND FNPRESENT = 1 AND FLAG(9)=0 THEN
            PRINT "The HORSE with a THORN in his hoof is nibbling on the SUGAR"
        End If
    THING$ = "LANTERN"
        IF ROOM=23 OR ROOM=24 OR ROOM=25 OR ROOM=26 OR ROOM=27 OR ROOM=28 OR ROOM=29 OR ROOM=30 OR ROOM=31 OR ROOM=32 THEN 
            FLAG(19)=1 
        ELSE 
            FLAG(19)=0
        End If
        IF FLAG(19)=1 AND FNCARRY=0 THEN 
            PRINT "Too dark -- Dangerous!"
            DANGEROUS=DANGEROUS + 1
        End If
        IF DANGEROUS=8 THEN 
            PRINT "You bumped your head in the dark"
            FLAG(1)=-1
            EXIT SUB
        End If
    If ROOM = 15 And FLAG(5) = 0 Then Print "There's a mighty hungry COW here"
    If ROOM = 15 And FLAG(5) = 1 Then Print "There is a contented COW here"
    THING$="SNAKE"
        IF ROOM =18 AND FLAG(2)=0 AND FNCARRY=0 THEN 
            CLS
            PRINT
            PRINT
            PRINT "A vicious DOG chases you out of the shack and back into the STREET"
            FLAG(14)=1
            ROOM=19
            EXIT SUB
        End If
    IF ROOM=18 AND FLAG(2)=0 AND FNCARRY=1 THEN
        FLAG(2)=1
        PRINT "The DOG was frightened off by the dead RATTLESNAKE"
    End If
    IF TURNNUMBER=150 AND FLAG(17)=0 AND FLAG(26)=0 THEN 
        FLAG(26)=1
        PRINT "The JAIL door KEY is buried somewhere."
    End If
    IF SCORE=86 AND FLAG(25)=0 THEN 
        FLAG(25)=1
        PRINT "A stranger asks you the name of the HOTEL."
    End If
    Print
End Sub

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
Sub GAMESTARTER
    Cls
    Print
    Print "                WELCOME TO HANGTOWN"
    Print
    Print
    Print "Come away with me on an exciting adventure to "
    Print "HANGTOWN, home of the GOLD rush!  Cope with "
    Print "mad dogs and ENGLISHMEN!  Save endangered animals!"
    Print "GET TREASURES!     MAKE TEA!"
    Print
    Print "Give COMMANDs as verb then object, such as  GO NORTH,"
    Print "SAVE GAME, RESTORE GAME, READ SIGN, KILL SNAKE,"
    Print "and GO CELLAR."
    Print
    Print "Exceptions to this two-word sentence rule are single-letter"
    Print "COMMANDs such as  N to GO NORTH, U to GO UP, AND D to GO"
    Print "DOWN."
    Print
    Print "P.S. Don't try to GET objects ending in an *, e.g., BOULDER*,"
    Print "as they are quite unobtainable-- comprendez, podner?"
    Print
    Print "And if you get stuck, just keep trying things, because"
    Print "After a certain number of turns, a hint will show up."
    Print "Press ENTER to begin"
    Do While Len(InKey$) = 0: Loop
    Cls
End Sub

'MODULE 2.1.2 ERASER ERASES ONE LINE OF TEXT
Sub ERASER
    Rem    FOR I=1 TO 12:PRINT "          ";:NEXT I
End Sub

'MODULE 2.2 COMMANDS
Sub COMMANDS
    SHARED VERB$, OBJECT$,ROOM,DESCRIPTION$(),FLAG(),INVENTORY$()
    SHARED ROOMOBJECT$(),OBJ$(),OBJ2$(),C$,ANSWER$(),TURNNUMBER,CONVERTNUM

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
        For J = 1 To 18 'CONVERT OBJECT$ LACKING * OR !
            If OBJECT$ = OBJ$(J) Then Let OBJECT$ = OBJ2$(J)
        Next J
        'CHECK FOR EXTRA WORDS
        COUNTER = 0
        For J = 1 To Len(OBJECT$)
            Let DUMMY$ = Mid$(OBJECT$, J, 1)
            If DUMMY$ = Chr$(32) Then COUNTER = COUNTER + 1
        Next J

        IF COUNTER<>0 THEN 
            PRINT "One space (two words) only, please."
        ELSE 
            DUMMY2=1
        End If
    Loop
End Sub

'MODULE 2.3 EVALUATE THE COMMANDS
Sub EVALUATE
    SHARED VERB$, OBJECT$,ROOM,DIRECTION,MOVEMENTTABLE(),NUMBERROOMS
    SHARED ROOMOBJECT$(),ITEMNUMBER,TURNNUMBER,THING$,INVENTORY$(),FLAG()
    If FLAG(1) <> 0 Then Exit Sub
    If FLAG(14) = 1 Then Exit Sub
    Select Case VERB$
        Case "QUIT"
            Let FLAG(1) = 2 'GAME NOT WON(1) OR LOST(-1):Exit Sub
        Case "SAVE"
            If OBJECT$ <> "GAME" Then Print "TRY-  SAVE GAME": Exit Sub
            Open "HANGSAV.BAS" For Output As #1

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
            Exit Sub
        Case "RESTORE", "LOAD"
            If OBJECT$ <> "GAME" Then Print "RESTORE GAME": Exit Sub
            Open "HANGSAV.BAS" For Input As #1

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
            Exit Sub
        Case "INVENTORY", "I"
            COUNTER = 0
            Print "   You carry: ";
            For I = 1 To 5
                IF INVENTORY$(I)<>"EMPTY" THEN COUNTER=COUNTER + 1:PRINT INVENTORY$(I);"   ";
            Next I
            If COUNTER = 0 Then Print "nothing at all": Exit Sub
            Print: Exit Sub
        Case "GO", "MOVE", "N", "S", "E", "W", "U", "D"
            If Len(VERB$) = 1 Then OBJECT$ = VERB$
            If OBJECT$ = "NORTH" Or OBJECT$ = "N" Then DIRECTION = 1
            If OBJECT$ = "SOUTH" Or OBJECT$ = "S" Then DIRECTION = 2
            If OBJECT$ = "EAST" Or OBJECT$ = "E" Then DIRECTION = 3
            If OBJECT$ = "WEST" Or OBJECT$ = "W" Then DIRECTION = 4
            If OBJECT$ = "UP" Or OBJECT$ = "U" Then DIRECTION = 5
            If OBJECT$ = "DOWN" Or OBJECT$ = "D" Then DIRECTION = 6
            IF OBJECT$="WELL*" AND FLAG(3)=0 THEN
               PRINT "SNAKE won't let me"
               Exit Sub
            End If
            IF ROOM=2 AND OBJECT$="WELL*" AND FLAG(3)=1 THEN 
                PRINT "OK"
                ROOM=1
                Exit Sub
            End If
            IF ROOM=7 AND FLAG(6)=0 AND DIRECTION = 3 THEN PRINT "Can't enter JAIL - door is locked":Exit Sub
            If ROOM = 18 And OBJECT$ = "CELLAR*" Then Print "OK": ROOM = 22: Exit Sub
            IF MOVEMENTTABLE (ROOM,DIRECTION) = 0 THEN
                PRINT "You can't GO that way" 
            ELSE 
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
            End If
            Exit Sub
        Case "GET", "TAKE", "PLUCK", "EXTRACT", "PULL", "REMOVE"
            IF ROOM=1 AND OBJECT$="WATER" THEN
                PRINT "Too dangerous from the LEDGE"
                Exit Sub
            End If
            IF ROOM=2 AND OBJECT$="WATER" AND FLAG(3)=0 THEN
               PRINT "The SNAKE won't let me"
               Exit Sub
            End If
            THING$ = "BUCKET"
            IF ROOM = 2 AND OBJECT$="WATER" AND FLAG(3)=1 AND FNCARRY = 0 THEN PRINT "You'll need a BUCKET.":Exit Sub
            IF ROOM =2 AND FNCARRY = 1 AND OBJECT$="WATER" AND FLAG(12)=0 THEN PRINT "The BUCKET won't reach the WATER.":Exit Sub
            IF ROOM = 2 AND OBJECT$="WATER" AND FLAG(3)=1 AND FNCARRY = 1 AND FLAG(12)=1 THEN
                PRINT "OK"
                FLAG(11)=1
                Exit Sub
            End If
            THING$ = "PAIL"
               IF FLAG(5)=1 AND ROOM=15 AND FNCARRY=0 AND OBJECT$="MILK" THEN _
                  PRINT "You'll need a PAIL":Exit Sub
               IF FLAG(5)=1 AND ROOM = 15 AND OBJECT$="MILK" AND FNCARRY=1 THEN _
                  PRINT "OK":FLAG(23)=1: Exit Sub
               IF FLAG(5)=0 AND ROOM=15 AND OBJECT$="MILK" THEN
                  PRINT "The hungry COW is too nervous"
				  Exit Sub
			   End If
            If ROOM <> 15 And OBJECT$ = "MILK" Then Print "Find a COW": Exit Sub
            Let THING$ = "TWEEZERS"
               IF OBJECT$="THORN" AND FNCARRY=0 THEN
                  PRINT "You'll need something to remove the THORN with."
                  Exit Sub
               End If
               IF ROOM=33 AND FLAG(4)=1 AND FNCARRY=1 AND FLAG(9)=0 AND OBJECT$="THORN" THEN
                  PRINT "The happy HORSE shakes something from its mane and runs away"
                  ROOMOBJECT$(33,1)="TURQUOISE!":FLAG(9)=1
                  Exit Sub
               End If
            If OBJECT$ = "LANTERN" Then FLAG(7) = 1
            IF ROOM=10 AND OBJECT$="KITTEN" THEN PRINT "Too shy - Runs away from you":Exit Sub

            Let HOLDING$ = OBJECT$
            Let OBJECT$ = "SUGAR"
            IF ROOM = 33 AND FLAG(4)=1 AND HOLDING$= "HORSE" AND FNPRESENT=0 AND FLAG(9)=0 THEN
               PRINT "The HORSE won't come to you without a bribe":Exit Sub
            End If
            Let OBJECT$ = HOLDING$

            If FNPRESENT = 0 Then Print "I don't see  "; OBJECT$: Exit Sub
            If Right$(OBJECT$, 1) = "*" Then Print "I can't handle it": Exit Sub
            If OBJECT$ = "LANTERN" Then FLAG(7) = 1

            IF RIGHT$(OBJECT$,5)="SNAKE" AND FLAG(3)=0 THEN PRINT "I don't handle live SNAKES, thank you very much":Exit Sub

            For I = 1 To 5
                IF INVENTORY$(I)="EMPTY" THEN 
                    INVENTORY$(I)=OBJECT$
                    PRINT "GOT IT":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY"
                    Exit Sub
                End If
            Next I
            Print "You're carrying too much.  Drop something!"
        Case "PUT", "DROP", "GIVE"
            If OBJECT$ = "MILK" Then Print "Try  DROP PAIL": Exit Sub
            THING$ = OBJECT$
            IF OBJECT$="PAIL" AND ROOM=10 AND FLAG(8)=0 AND FNCARRY = 1 THEN 
                FLAG(8)=1
                PRINT "The now full KITTEN returns with a pack rat."
                PRINT "The pack rat drops something and runs off, "
                PRINT "with the KITTEN in hot pursuit"
                ROOMOBJECT$(10,1)="RUBY!"
                PRINT:Exit Sub
            End If
            If OBJECT$ = "MILK" And ROOM = 9 Then Print "Try  DROP PAIL": Exit Sub
            If OBJECT$ = "WATER" Then Print "Try  FILL KETTLE ": Exit Sub
            THING$ = OBJECT$
            If FNCARRY = 0 Then Print "You don't have the "; OBJECT$: Exit Sub
            'PREVENTS DROPPING TWO OF ANYTHING ANYWHERE
            If FNPRESENT = 1 Then Print "Not again.": Exit Sub
            If OBJECT$ = "LANTERN" Then FLAG(7) = 0
            If OBJECT$ = "HAY" And ROOM = 15 Then FLAG(5) = 1
            For J = 1 To 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN 
                    ROOMOBJECT$(ROOM,J)=OBJECT$
                    LET INVENTORY$(ITEMNUMBER)="EMPTY" 
                    PRINT "OK"
                    Exit Sub
                End If
            Next J
            Print "This room is full, take it elsewhere": Exit Sub
        Case "LOOK", "EXAMINE", "INSPECT"
            IF ROOM=2 AND OBJECT$="WELL*" THEN PRINT _
               "I see GOLD!  Maybe I should GO WELL*":Exit Sub
            IF ROOM=6 AND OBJECT$="MIRROR*" THEN
               PRINT "The rest of the sign can now be seen.  It says:"
               PRINT "      ITEM #2          Sometimes I'm perky,"
               PRINT "                       Sometimes I'm a drip;"
               PRINT "                       When I'm freshly brewed,"
               PRINT "                       Would you like a sip?"
               PRINT
               Exit Sub
            End If
            If OBJECT$ = "SIGN*" Then Print "Try  READ SIGN*  ": Exit Sub
            If OBJECT$ = "MAP" Then Print "Try -   READ MAP": Exit Sub
            Print "I don't see anything unexpected": Exit Sub
        Case "READ"
            IF ROOM=9 AND OBJECT$="SIGN*" THEN 
                PRINT "To MAKE TEA --"
                PRINT "Bring here and DROP the following -"
                PRINT "TEA, KETTLE, WATER, MILK, MATCHES, SUGAR, "
                PRINT "CUP, and something to burn, then type MAKE TEA"
                Exit Sub
            End If
            IF ROOM=3 AND OBJECT$="SIGN*" THEN 
                PRINT "The SIGN* says -"
                PRINT "DROP TREASURES! Such as GOLD! here and type  SCORE  "
                Exit Sub
            End If
            IF ROOM=6 AND OBJECT$="SIGN*" THEN 
                PRINT "         The SIGN* says: Some things you need,"
                PRINT "                         Can be found here;"
                PRINT "                         Just say the word,"
                PRINT "                         And they'll appear."
                PRINT "           ITEM #1:      I hang on the wall,"
                PRINT "                         With nothing to do;"
                PRINT "                         When you LOOK at me,"
                PRINT "                         You see only you."
                PRINT "       You can't make out the last 4 or 5 lines."
                PRINT "       They look backwards or something."
                PRINT
                Exit Sub
            End If
            IF ROOM=6 AND OBJECT$="MIRROR*" THEN
                PRINT "Try -    LOOK MIRROR"
                Exit Sub
            End If
            THING$ = "MAP": If OBJECT$ = "MAP" And FNCARRY = 0 Then Print "You don't have the MAP": Exit Sub
            THING$="MAP":IF OBJECT$="MAP" AND FNCARRY=1 THEN
                PRINT "Says  DOWN WEST WEST SOUTH SOUTH DIG"
                Exit Sub
            End If
            Print "Try READ SIGN* or  READ MAP": Exit Sub
        Case "OIL", "LUBRICATE", "GREASE"
            THING$ = "MINERAL-OIL"
            If FNCARRY = 0 Then Print "You don't have the MINERAL-OIL": Exit Sub
            If ROOM = 11 Then Print "OK": FLAG(13) = 1: Exit Sub
            Print "Not here": Exit Sub
        Case "DRINK", "EAT"
            Print "What is it with you and food?": Exit Sub
        Case "SCORE"
            If ROOM <> 3 Then Print "Return to the TOWN SQUARE": Exit Sub
            SCORE = 2
            For I = 1 To 10
                Let STUFF$ = ROOMOBJECT$(3, I)
                If Right$(STUFF$, 1) = "!" Then SCORE = SCORE + 14
            Next I
            Print "Your SCORE is "; SCORE
            If SCORE = 100 Then Print "You win!!!!  Fantastic Job!!!  YAHOO!!": FLAG(1) = 1: Exit Sub
            Print (100 - SCORE); " points to go": Exit Sub
        Case "CLIMB"
            If OBJECT$ = "WELL" Or OBJECT$ = "WELL*" Then Print "Try GO WELL*": Exit Sub
            THING$ = "CHAIR"
              IF ROOM=20 AND FNCARRY=1 THEN
                 PRINT "PUT or DROP CHAIR here first."
                 Exit Sub
              End If
              OBJECT$="CHAIR"
              IF ROOM=20 AND FNPRESENT=0 THEN
                 PRINT "The BOULDER is too high, GET something to stand on"
                 Exit Sub
              End If
            If ROOM = 20 Then Print "Good climbing!": ROOM = 21: Exit Sub
            Print "Come down from there!": Exit Sub
        Case "TIE", "CONNECT", "ATTACH"
            If Left$(OBJECT$, 2) = "UP" Then Print "Try  TIE ROPE ": Exit Sub
            THING$ = "ROPE": If FNCARRY = 0 Then Print "You'll need the ROPE": Exit Sub
            THING$ = "BUCKET": If FNCARRY = 0 Then Print "GET the BUCKET": Exit Sub
         IF OBJECT$="DOG" THEN 
             PRINT "You want me to try to TIE up a vicious DOG?  HA! HA!"
             Exit Sub
         End If
         IF OBJECT$="ROPE" OR OBJECT$="BUCKET" THEN
             PRINT "One end of the rope is tied to the BUCKET"
             FLAG(12)=1
             Exit Sub
         End If
         Print "I hardly know you!": Exit Sub
        Case "UNTIE", "UNDO"
          IF FLAG(12)=1 AND (OBJECT$="ROPE" OR OBJECT$="BUCKET") THEN
              PRINT "OK":FLAG(12)=0
              Exit Sub
          End If
        Case "LOWER"
            Print "Try  GET WATER": Exit Sub
        Case "MILK"
            Print "Try  GET MILK": Exit Sub
        Case "KILL"
            THING$ = "ROCK"
              IF FNCARRY=0 AND ROOM=2 THEN 
                  PRINT "You'll need a suitable weapon"
                  Exit Sub
              End If
              IF FNCARRY=1 AND ROOM=2 AND FLAG(3)=0 THEN
                  PRINT "The dead RATTLESNAKE is still frightening."
                  LET ROOMOBJECT$(2,2)="SNAKE"
                  FLAG(3)=1
                  Exit Sub
              End If
            Print "You must think I'm crazy!": Exit Sub
        Case "UNLOCK"
            If OBJECT$ = "SAFE" Then Print "Try-  OPEN SAFE": Exit Sub
            THING$ = "KEY"
            If FNCARRY = 0 Then Print "You'll need a KEY": Exit Sub
              IF ROOM=7 THEN 
                  PRINT "The JAIL DOOR* is unlocked. You can GO EAST"
                  FLAG(6)=1
                  Exit Sub
              End If
            Print "No lock here": Exit Sub
        Case "OPEN"
          IF ROOM=11 AND OBJECT$="SAFE*" AND FLAG(13)=0 THEN
              PRINT "The rusty hinge needs OIL to open"
              Exit Sub
          End If
          IF ROOM=11 AND OBJECT$="SAFE*" AND FLAG(13)=1 AND FLAG(15)=0 THEN
              PRINT "I see JADE!":ROOMOBJECT$(11,2)="JADE!"
              FLAG(15)=1
              Exit Sub
          End If
          IF ROOM=11 AND OBJECT$="SAFE*" AND FLAG(15)=1 THEN
              PRINT "It is open!"
              Exit Sub
          End If
          IF OBJECT$="DOOR*" AND FLAG(6)=0 THEN
              PRINT "The JAIL DOOR is locked!"
              Exit Sub
          End If
          IF ROOM=7 AND OBJECT$="DOOR*" AND FLAG(6)=1 THEN
              PRINT "The JAIL DOOR is open, GO EAST to enter"
              Exit Sub
          End If
          Print "No........You can't make me": Exit Sub
        Case "THROW"
            If ROOM = 2 Then Print "Try  KILL SNAKE ": Exit Sub
            Print "Not here": Exit Sub
        Case "DIG", "SHOVEL"
            THING$ = "SHOVEL"
            If FNCARRY = 0 Then Print "You'll need a SHOVEL": Exit Sub
            If ROOM = 31 Or ROOM = 5 Then Print "I hit something"
             IF ROOM=31 AND FLAG(16)=0 THEN 
                 ROOMOBJECT$(31,1)="DIAMONDS!"
                 FLAG(16)=1
                 Exit Sub
             End If
             IF ROOM=5 AND FLAG(17)=0 THEN 
                 ROOMOBJECT$(5,1)="KEY"
                 FLAG(17)=1
                 Exit Sub
             End If
             Print "The substrate is too hard here to dig": Exit Sub
        Case "MAKE", "PREPARE"
            If OBJECT$ <> "TEA" Then Print "Don't know how": Exit Sub
            If FNPRESENT = 0 Then Print "You'll need TEA": Exit Sub
            OBJECT$="KETTLE"
            IF FNPRESENT=0 OR FLAG(10)=0 THEN
                PRINT "You'll need a KETTLE FILLed with WATER."
                Exit Sub
            End If
         OBJECT$="PAIL":IF FNPRESENT=0 OR FLAG(23)=0 THEN _
            PRINT "You'll need a PAIL FILLed with MILK.":Exit Sub
            OBJECT$ = "SUGAR": If FNPRESENT = 0 Then Print "You'll need the SUGAR ": Exit Sub
            OBJECT$ = "MATCHES": If FNPRESENT = 0 Then Print "You'll need some MATCHES": Exit Sub
            OBJECT$ = "CUP": If FNPRESENT = 0 Then Print "You'll need a CUP": Exit Sub
            OBJECT$ = "MAGAZINES": If FNPRESENT = 0 Then Print "You'll need something to burn": Exit Sub

            IF FLAG(18)=0 THEN 
                FLAG(4)=1
                FLAG(18)=1
                ROOMOBJECT$(9,2)="EMERALD!"
                PRINT "The grateful ENGLISHMAN leaves a big tip"
                Exit Sub
            End If
            If FLAG(18) = 1 Then Print "Not again!": Exit Sub
            Print "PUT the items you need in the TEA ROOM": Exit Sub
        Case "FILL"
            THING$ = "KETTLE"
            IF OBJECT$="KETTLE" AND FNCARRY=1 AND FLAG(11)=1 THEN
                FLAG(10)=1
                PRINT "The KETTLE is FILLed."
                Exit Sub
            End If
            IF OBJECT$="KETTLE" AND FNPRESENT=1 AND FLAG(11)=1 THEN
                FLAG(10)=1
                PRINT "The KETTLE is FILLed."
                Exit Sub
            End If
            If OBJECT$ = "BUCKET" And ROOM = 2 Then Print "Try GET WATER": Exit Sub
            If OBJECT$ = "PAIL" And ROOM = 15 Then Print "Try  GET MILK": Exit Sub
            Print "You'll need a KETTLE or a BUCKET or a PAIL": Exit Sub
        Case "ENTER"
            Print "Try GO WELL* or GO CELLAR*": Exit Sub
        Case "BURN"
            If OBJECT$ = "HAY" Then Print "Too green --- Won't BURN.": Exit Sub
            Print "Don't play with fire!": Exit Sub
        Case "MIRROR*", "MIRROR"
          PRINT "Suddenly you notice a MIRROR* on the wall"
          PRINT "opposite the SIGN*"
          LET ROOMOBJECT$(6,3)="MIRROR*"
          Exit Sub
        Case "SAY"
            Print "Just type the word you were going to say": Exit Sub
        Case "COFFEE"
         IF FLAG(22)=0 THEN 
             PRINT "Suddenly you smell and then see some"
             PRINT "fresh coffee on the bar.  So does a passing prospector"
             PRINT "who takes it, leaving some SILVER! in exchange."
             LET ROOMOBJECT$(6,4)="SILVER!"
             FLAG(22)=1
             Exit Sub
          End If
          Print "All that's left of the coffee is the aroma": Exit Sub
        Case "FEED"
            PRINT "Try the word  GIVE  followed by the name of the food."
            Exit Sub
        Case "USE"
            Print "Try - DIG or UNLOCK or TIE or PLUCK": Exit Sub
        Case "REVEAL"
            Print "Just type the name of the object by itself.": Exit Sub
        Case Else
          IF ROOM=6 AND OBJECT$="MIRROR*" THEN
             PRINT "Try -   LOOK MIRROR"
             Exit Sub
          End If
          IF OBJECT$="CELLAR*" THEN
             PRINT "Try-    GO CELLAR"
             Exit Sub
          End If
          If OBJECT$ = "WELL*" Then Print "Try-  GO WELL": Exit Sub
          Print "I don't know that verb": Exit Sub
    End Select
End Sub

'MODULE 0 MAIN    ******HANGTOWN******  10-3-90
'MULTIPLE REVISIONS ON 1-25-91
'GOLD HINT BURN HINT KEY HINT REVEAL HINT RESTORE 30 6MAR92
'TIMER AND STRT CHANGE  2-3-94
'TIME$, DATE$, AND SAVE GAME FIX 7-30-95
'SNIp fixes 1-3-96
'REPORT STUFF 3-5-96  HANGDATA.TXT 10-24-96
'7-1-23, Henrik Aasman: Modified for QB64
'  Moved all definition of global (shared) variables to Main module
'  QB64 needs unique names so MOVEMENTTABLE --> MOVEMENTTABLESUB and INVENTORY --> INVENTORYSUB
'  DATA don't allow split lines with _ in B64
'  Changed syntax for BEEP so BEEP 3 --> BEEP
'  Changed syntax for DELAY so DELAY --> _DELAY
'  Fixed __ to _
'  Renamed DEF to FUNCTION
'  Removed reading/writing to REPORT.DTA
'  Added "" around some of the data in DATA-tables

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
ReDim Shared VERB$
ReDim Shared OBJECT$
ReDim Shared C$
ReDim Shared CONVERTNUM
ReDim Shared DIRECTION
ReDim Shared ITEMNUMBER
ReDim Shared STRT
ReDim Shared SCORE

Cls: ReDim FLAG(30), OBJ$(18), OBJ2$(18)
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
    Data 33,"LEDGE just inside WELL*","RATTLESNAKE WELLS","HANGTOWN SQUARE","SCAFFOLD","SANDY DESERT"
    Data "LAST CHANCE SALOON","STREET","HARDROCK JAIL","STEEP'S TEA ROOM","STREET","GOLDEN WELL* HOTEL"
    Data "DOCTOR'S OFFICE","STREET","LIVERY STABLE","PASTURE","STREET","LIMPING HORSE CORRAL"
    Data "MINER'S SHACK","STREET","FIELD","TOP OF BOULDER","CELLAR","MINESHAFT","MINESHAFT"
    Data "MINESHAFT","MINESHAFT","MINESHAFT","MINESHAFT","MINESHAFT","MINESHAFT","MINESHAFT"
    Data "MINESHAFT","RANGE"

    For I = 1 To NUMBERROOMS
        Let DESCRIBEFLAG(I) = 1
        Let ROOMDESCRIBE$(I) = "EMPTY"
        Let ROOMDES2$(I) = "EMPTY"
    Next I
    For I = 1 To 15
        Read DESCRIBEROOM
        Let DESCRIBEFLAG(DESCRIBEROOM) = 0
        Read ROOMDESCRIBE$(DESCRIBEROOM)
        Read ROOMDES2$(DESCRIBEROOM)
    Next I
    Data 2,"Your blood chills with fear at the sight of the huge and horrible"
    Data "RATTLESNAKE guarding the WELL.  You know that one of you must die!"
    Data 3,"Curiously, you find yourself in the TOWN SQUARE of a dry, dusty,"
    Data "apparently deserted gold rush town."
    Data 4,"You stand at the top of a gruesome SCAFFOLD.  The hangperson"
    Data "left his/her well-worn noose here. Is this a PC PC or what?"
    Data 5,"The DESERT seems to stretch out forever to the EAST."
    Data "It's hard to walk in the soft sand."
    Data 6,"As you enter the aroma that greets you isn't the stale smoke and "
    Data "whiskey smell you expected.  You feel magic in the air here."
    Data 7,"From the STREET you can see the town JAIL to the EAST.      "
    Data "You wonder why the JAIL door was left locked."
    Data 9,"An elegantly-dressed but crabby-looking ENGLISHMAN looks at you as"
    Data "you enter the TEA ROOM, then turns away.  He needs a nice CUP of TEA."
    Data 10,"All you can see to the SOUTH are hoof prints and wagon ruts,"
    Data "frozen forever in the hard-baked dirt of the town STREET."
    Data 11,"The curiously named HOTEL looks pretty run down.  "
    Data "Hopefully, you won't have to spend the night here."
    Data 12,"You can almost hear the elevator music in the DOCTOR'S OFFICE. "
    Data "You see all of the earliest issues of INVESTMENT MAGAZINE."
    Data 13,"The ROCK you see in the STREET could do a lot of damage if"
    Data "thrown accurately."
    Data 14,"With HAY on the floor, the LIVERY STABLE looks more comfortable"
    Data "than the HOTEL.  There is a small milk-pail here, but no COW."
    Data 15,"Here in the PASTURE is the most pathetic COW you've ever seen. "
    Data "GET her some food before it's too late!"
    Data 17,"Here in the LIMPING HORSE CORRAL you see some SUGAR cubes that"
    Data "are still in good condition.  Horses love 'em."
    Data 20,"Here in the FIELD is a huge BOULDER with sheer walls.  To"
    Data "CLIMB it, You'll need something to stand upon."
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
    Data 0,0,0,0,2,0,0,0,3,0,0,0,0,7,5,2,4,0
    Data 0,0,0,0,0,3,0,0,5,3,0,0,0,0,7,0,0,0
    Data 3,10,8,6,0,0,0,0,0,7,0,0,0,0,10,0,0,0
    Data 7,13,11,9,0,0,0,0,0,10,0,0,0,0,13,0,0,0
    Data 10,16,14,12,0,0,0,0,0,13,0,0,0,0,16,0,0,0
    Data 13,19,17,15,0,0,0,0,0,16,0,0,0,0,19,0,0,0
    Data 16,33,20,18,0,0,0,0,0,19,0,0,0,0,0,0,0,20
    Data 0,0,0,0,18,25,0,27,24,0,0,0,0,28,25,23,0,0
    Data 0,29,26,24,22,0,0,30,0,25,0,0,23,31,28,0,0,0
    Data 24,0,28,27,0,0,25,32,30,28,0,0,26,0,0,29,0,0
    Data 27,0,0,0,0,0,29,0,0,0,0,0,19,0,0,0,0,0
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
    SHARED GAMESTATE$,ROOM,TURNNUMBER,ROOMOBJECT$(),THING$,INVENTORY$()
    SHARED FLAG(),OBJ$(),OBJ2$(),DESCRIBEFLAG(),ROOMDESCRIBE$(),ROOMDES2$()
    SHARED CONVERTNUM
    Let ROOM = 3 'START IN HANGTOWN SQUARE
    Let TURNNUMBER = 0
    DANGEROUS = 1
    Let THING$ = ""
    For I = 1 To 30: Let FLAG(I) = 0: Next I 'SET FLAGS TO 0
    'FLAG #  MEANING OF 0      FLAG# MEANING OF 0
    ' 1      GAME NOT OVER       2   DOG NOT GONE
    'FOR 1, 1 MEANS GAME WON -1 GAME LOST 2 MEANS GAME QUIT
    ' 3      SNAKE NOT KILLED    4   TEA NOT MADE
    ' 5      COW NOT FED         6   JAIL DOOR LOCKED
    ' 7      LANTERN NOT CARRIED 8   KITTEN NOT FED
    ' 9      THORN NOT OUT      10   KETTLE NOT FILLED
    '11      BUCKET EMPTY       12   ROPE NOT TIED TO BUCKET
    '13    SAFE HINGE NOT OILED  14  NOT CHASED BY DOG
    '14 IS USED TO EXIT TURN BUT NOT EXIT THE GAME
    '15 SAFE NOT OPENED          16 SILVER NOT DUG
    '17 KEY NOT DUG              18 EMERALD NOT DROPPED
    '19 ROOM NOT DANGEROUS       20 NEVER BEEN TO LANTERN ROOM22
    '21 COYOTE HINT NOT GIVEN    22 COFFEE NOT NAMED
    '23 PAIL NOT FILLED WITH MILK 24 2ND DOG HINT NOT GIVEN
    '25 GOLD HINT NOT GIVEN       26 KEY HINT NOT GIVEN
    Let ROOMOBJECT$(1, 1) = "GOLD!" 'STARTING LOCATIONS FOR OBJECTS
    Let ROOMOBJECT$(2, 1) = "WELL*"
    Let ROOMOBJECT$(2, 2) = "SNAKE"
    Let ROOMOBJECT$(2, 3) = "BUCKET"
    Let ROOMOBJECT$(3, 1) = "SIGN*"
    Let ROOMOBJECT$(4, 1) = "ROPE"
    Let ROOMOBJECT$(5, 1) = "EMPTYKEY"
    Let ROOMOBJECT$(6, 1) = "KETTLE"
    Let ROOMOBJECT$(6, 2) = "SIGN*"
    Let ROOMOBJECT$(6, 3) = "EMPTYMIRROR*"
    Let ROOMOBJECT$(6, 4) = "EMPTYSILVER!"
    Let ROOMOBJECT$(7, 1) = "DOOR*"
    Let ROOMOBJECT$(8, 1) = "CUP"
    Let ROOMOBJECT$(9, 1) = "STOVE*"
    Let ROOMOBJECT$(9, 2) = "SIGN*"
    Let ROOMOBJECT$(9, 4) = "CHAIR"
    Let ROOMOBJECT$(9, 5) = "EMPTYEMERALD!"
    Let ROOMOBJECT$(10, 1) = "EMPTYRUBY!"
    Let ROOMOBJECT$(11, 1) = "SAFE*"
    Let ROOMOBJECT$(11, 2) = "EMPTYJADE!"
    Let ROOMOBJECT$(11, 3) = "MATCHES"
    Let ROOMOBJECT$(12, 1) = "MAGAZINES"
    Let ROOMOBJECT$(12, 2) = "TWEEZERS"
    Let ROOMOBJECT$(12, 3) = "MINERAL-OIL"
    Let ROOMOBJECT$(13, 1) = "ROCK"
    Let ROOMOBJECT$(14, 1) = "SHOVEL"
    Let ROOMOBJECT$(14, 2) = "HAY"
    Let ROOMOBJECT$(14, 3) = "PAIL"
    Let ROOMOBJECT$(15, 1) = "COW*"
    Let ROOMOBJECT$(17, 1) = "SUGAR"
    Let ROOMOBJECT$(18, 1) = "TEA"
    Let ROOMOBJECT$(18, 2) = "CELLAR*"
    Let ROOMOBJECT$(20, 1) = "BOULDER*"
    Let ROOMOBJECT$(21, 1) = "MAP"
    Let ROOMOBJECT$(22, 1) = "LANTERN"
    Let ROOMOBJECT$(31, 1) = "EMPTYDIAMONDS!"
    Let ROOMOBJECT$(33, 1) = "EMPTYTURQUOISE!"

    Let CONVERTNUM = 18
    For I = 1 To CONVERTNUM 'READ IN WORD!* CONVERSIONS
        Read OBJ$(I), OBJ2$(I)
    Next I
End Sub
    
'MODULE 1.5 OTHERS
Sub OTHERS
    SHARED GAMESTATE$,ROOM,TURNNUMBER,ROOMOBJECT$(),THING$,INVENTORY$()
    SHARED FLAG(),OBJ$(),OBJ2$(),DESCRIBEFLAG(),ROOMDESCRIBE$(),ROOMDES2$()
    SHARED CONVERTNUM
    Let ROOM = 3 'START IN HANGTOWN SQUARE
    Let TURNNUMBER = 0
    DANGEROUS = 1
    Let THING$ = ""
    For I = 1 To 30: Let FLAG(I) = 0: Next I 'SET FLAGS TO 0
    'FLAG #  MEANING OF 0      FLAG# MEANING OF 0
    ' 1      GAME NOT OVER       2   DOG NOT GONE
    'FOR 1, 1 MEANS GAME WON -1 GAME LOST 2 MEANS GAME QUIT
    ' 3      SNAKE NOT KILLED    4   TEA NOT MADE
    ' 5      COW NOT FED         6   JAIL DOOR LOCKED
    ' 7      LANTERN NOT CARRIED 8   KITTEN NOT FED
    ' 9      THORN NOT OUT      10   KETTLE NOT FILLED
    '11      BUCKET EMPTY       12   ROPE NOT TIED TO BUCKET
    '13    SAFE HINGE NOT OILED  14  NOT CHASED BY DOG
    '14 IS USED TO EXIT TURN BUT NOT EXIT THE GAME
    '15 SAFE NOT OPENED          16 SILVER NOT DUG
    '17 KEY NOT DUG              18 EMERALD NOT DROPPED
    '19 ROOM NOT DANGEROUS       20 NEVER BEEN TO LANTERN ROOM22
    '21 COYOTE HINT NOT GIVEN    22 COFFEE NOT NAMED
    '23 PAIL NOT FILLED WITH MILK 24 2ND DOG HINT NOT GIVEN
    '25 GOLD HINT NOT GIVEN       26 KEY HINT NOT GIVEN
    Let ROOMOBJECT$(1, 1) = "GOLD!" 'STARTING LOCATIONS FOR OBJECTS
    Let ROOMOBJECT$(2, 1) = "WELL*"
    Let ROOMOBJECT$(2, 2) = "SNAKE"
    Let ROOMOBJECT$(2, 3) = "BUCKET"
    Let ROOMOBJECT$(3, 1) = "SIGN*"
    Let ROOMOBJECT$(4, 1) = "ROPE"
    Let ROOMOBJECT$(5, 1) = "EMPTYKEY"
    Let ROOMOBJECT$(6, 1) = "KETTLE"
    Let ROOMOBJECT$(6, 2) = "SIGN*"
    Let ROOMOBJECT$(6, 3) = "EMPTYMIRROR*"
    Let ROOMOBJECT$(6, 4) = "EMPTYSILVER!"
    Let ROOMOBJECT$(7, 1) = "DOOR*"
    Let ROOMOBJECT$(8, 1) = "CUP"
    Let ROOMOBJECT$(9, 1) = "STOVE*"
    Let ROOMOBJECT$(9, 2) = "SIGN*"
    Let ROOMOBJECT$(9, 4) = "CHAIR"
    Let ROOMOBJECT$(9, 5) = "EMPTYEMERALD!"
    Let ROOMOBJECT$(10, 1) = "EMPTYRUBY!"
    Let ROOMOBJECT$(11, 1) = "SAFE*"
    Let ROOMOBJECT$(11, 2) = "EMPTYJADE!"
    Let ROOMOBJECT$(11, 3) = "MATCHES"
    Let ROOMOBJECT$(12, 1) = "MAGAZINES"
    Let ROOMOBJECT$(12, 2) = "TWEEZERS"
    Let ROOMOBJECT$(12, 3) = "MINERAL-OIL"
    Let ROOMOBJECT$(13, 1) = "ROCK"
    Let ROOMOBJECT$(14, 1) = "SHOVEL"
    Let ROOMOBJECT$(14, 2) = "HAY"
    Let ROOMOBJECT$(14, 3) = "PAIL"
    Let ROOMOBJECT$(15, 1) = "COW*"
    Let ROOMOBJECT$(17, 1) = "SUGAR"
    Let ROOMOBJECT$(18, 1) = "TEA"
    Let ROOMOBJECT$(18, 2) = "CELLAR*"
    Let ROOMOBJECT$(20, 1) = "BOULDER*"
    Let ROOMOBJECT$(21, 1) = "MAP"
    Let ROOMOBJECT$(22, 1) = "LANTERN"
    Let ROOMOBJECT$(31, 1) = "EMPTYDIAMONDS!"
    Let ROOMOBJECT$(33, 1) = "EMPTYTURQUOISE!"

    Let CONVERTNUM = 18
    For I = 1 To CONVERTNUM 'READ IN WORD!* CONVERSIONS
        Read OBJ$(I), OBJ2$(I)
    Next I
    Data "GOLD","GOLD!","WELL","WELL*","SIGN","SIGN*","DOOR","DOOR*","STOVE","STOVE*","EMERALD"
    Data "EMERALD!","RUBY","RUBY!","DIAMONDS","DIAMONDS!","COW","COW*","CELLAR","CELLAR*"
    Data "BOULDER","BOULDER*","JADE","JADE!","SILVER","SILVER!","TURQUOISE","TURQUOISE!"
    Data "MINERAL","MINERAL-OIL","OIL","MINERAL-OIL","SAFE","SAFE*","MIRROR","MIRROR*"
End Sub

'MODULE 2 TURN
Sub TURN
    Shared FLAG(), TURNNUMBER
    Do Until FLAG(1) <> 0
        FLAG(14) = 0
        TURNNUMBER = TURNNUMBER + 1
        If TURNNUMBER=295 THEN 
            BEEP
            PRINT "Type   SAVE GAME  and then  QUIT"
            PRINT "You can then return to this spot by typing"
            PRINT "HANGTOWN (at the DOS prompt) and later,"
            PRINT "as a command, RESTORE GAME"
            PRINT "Please forgive the necessary inconvenience."
        End If
        If TURNNUMBER = 300 Then FLAG(1) = 2
        Call DESCRIBE 'MODULE 2.1 DESCRIBE THE ROOM
        Call COMMANDS 'MODULE 2.2 INPUT THE COMMANDS
        Call EVALUATE 'MODULE 2.3 EVALUATE COMMANDS
    Loop
End Sub

'MODULE 2.1 DESCRIBE THE CURRENT ROOM
Sub DESCRIBE
    SHARED DESCRIPTION$(),ROOM,MOVEMENTTABLE(),ROOMOBJECT$(),FLAG()
    SHARED TURNNUMBER,INVENTORY$(),THING$,OBJECT$,DESCRIBEFLAG(),ROOMDESCRIBE$()
    SHARED ROOMDES2$(), SCORE
	Dim STUFF$
	
    If TURNNUMBER = 1 Then Call GAMESTARTER 'MODULE 2.1.1
    IF DESCRIBEFLAG(ROOM)=0 THEN 
        PRINT ROOMDESCRIBE$(ROOM)
        PRINT ROOMDES2$(ROOM):DESCRIBEFLAG(ROOM)=1
    ElseIF DESCRIBEFLAG(ROOM)=1 AND DESCRIPTION$(ROOM)="STREET" THEN
        PRINT "You find yourself in the STREET"
    ElseIF DESCRIBEFLAG(ROOM)=1 AND DESCRIPTION$(ROOM)<>"STREET" THEN 
        PRINT "You find yourself at the ";:PRINT DESCRIPTION$(ROOM)
    End If
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
        IF LEFT$(STUFF$,5)<>"EMPTY" THEN 
            PRINT "   ";ROOMOBJECT$(ROOM,I);
            COUNTER=COUNTER+1
        End If
    Next I
    If COUNTER = 0 Then Print "noticeably absent!";
    Print
    _Delay 0.3
    Print "You are carrying: ";
    COUNTER = 0
    For I = 1 To 5
        IF INVENTORY$(I)<>"EMPTY" THEN 
            COUNTER=COUNTER + 1
            PRINT INVENTORY$(I);"   ";
        End If
    Next I
    If COUNTER = 0 Then Print "nothing at all."
    Print
    _Delay 0.3
    OBJECT$ = "SNAKE"
        IF FLAG(21)=0 AND TURNNUMBER>90 AND FLAG(2)=0 AND FNPRESENT=1 THEN
            PRINT "A coyote strolls by, sees the dead SNAKE and runs away"
            FLAG(21)=1
        End If
        IF FLAG(24)=0 AND FLAG(21)=1 AND DESCRIPTION$(ROOM)="STREET" AND TURNNUMBER>120 THEN
            FLAG(24)=1
            PRINT "A tourist tells you his dog ran off after seeing a dead SNAKE."
        End If
    IF ROOM =22 AND FLAG(20)=0 THEN 
        PRINT "The glowing LANTERN is quite bright"
        FLAG(20)=1
    End If
    IF ROOM = 10 AND FLAG(4)=1 AND FLAG(8)=0 THEN PRINT "There is a shy and hungry KITTEN here"
    If ROOM = 9 And FLAG(4) = 0 Then Print "There is an unhappy ENGLISHMAN* here"
    OBJECT$ = "SUGAR"
        IF ROOM=33 AND FLAG(4)=1 AND FNPRESENT = 0 AND FLAG(9)=0 THEN
            PRINT "You see a limping HORSE. You'll need a bribe to lure him over."
        End If
        IF ROOM = 33 AND FLAG(4)=1 AND FNPRESENT = 1 AND FLAG(9)=0 THEN
            PRINT "The HORSE with a THORN in his hoof is nibbling on the SUGAR"
        End If
    THING$ = "LANTERN"
        IF ROOM=23 OR ROOM=24 OR ROOM=25 OR ROOM=26 OR ROOM=27 OR ROOM=28 OR ROOM=29 OR ROOM=30 OR ROOM=31 OR ROOM=32 THEN 
            FLAG(19)=1 
        ELSE 
            FLAG(19)=0
        End If
        IF FLAG(19)=1 AND FNCARRY=0 THEN 
            PRINT "Too dark -- Dangerous!"
            DANGEROUS=DANGEROUS + 1
        End If
        IF DANGEROUS=8 THEN 
            PRINT "You bumped your head in the dark"
            FLAG(1)=-1
            EXIT SUB
        End If
    If ROOM = 15 And FLAG(5) = 0 Then Print "There's a mighty hungry COW here"
    If ROOM = 15 And FLAG(5) = 1 Then Print "There is a contented COW here"
    THING$="SNAKE"
        IF ROOM =18 AND FLAG(2)=0 AND FNCARRY=0 THEN 
            CLS
            PRINT
            PRINT
            PRINT "A vicious DOG chases you out of the shack and back into the STREET"
            FLAG(14)=1
            ROOM=19
            EXIT SUB
        End If
    IF ROOM=18 AND FLAG(2)=0 AND FNCARRY=1 THEN
        FLAG(2)=1
        PRINT "The DOG was frightened off by the dead RATTLESNAKE"
    End If
    IF TURNNUMBER=150 AND FLAG(17)=0 AND FLAG(26)=0 THEN 
        FLAG(26)=1
        PRINT "The JAIL door KEY is buried somewhere."
    End If
    IF SCORE=86 AND FLAG(25)=0 THEN 
        FLAG(25)=1
        PRINT "A stranger asks you the name of the HOTEL."
    End If
    Print
End Sub

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
Sub GAMESTARTER
    Cls
    Print
    Print "                WELCOME TO HANGTOWN"
    Print
    Print
    Print "Come away with me on an exciting adventure to "
    Print "HANGTOWN, home of the GOLD rush!  Cope with "
    Print "mad dogs and ENGLISHMEN!  Save endangered animals!"
    Print "GET TREASURES!     MAKE TEA!"
    Print
    Print "Give COMMANDs as verb then object, such as  GO NORTH,"
    Print "SAVE GAME, RESTORE GAME, READ SIGN, KILL SNAKE,"
    Print "and GO CELLAR."
    Print
    Print "Exceptions to this two-word sentence rule are single-letter"
    Print "COMMANDs such as  N to GO NORTH, U to GO UP, AND D to GO"
    Print "DOWN."
    Print
    Print "P.S. Don't try to GET objects ending in an *, e.g., BOULDER*,"
    Print "as they are quite unobtainable-- comprendez, podner?"
    Print
    Print "And if you get stuck, just keep trying things, because"
    Print "After a certain number of turns, a hint will show up."
    Print "Press ENTER to begin"
    Do While Len(InKey$) = 0: Loop
    Cls
End Sub

'MODULE 2.1.2 ERASER ERASES ONE LINE OF TEXT
Sub ERASER
    Rem    FOR I=1 TO 12:PRINT "          ";:NEXT I
End Sub

'MODULE 2.2 COMMANDS
Sub COMMANDS
    SHARED VERB$, OBJECT$,ROOM,DESCRIPTION$(),FLAG(),INVENTORY$()
    SHARED ROOMOBJECT$(),OBJ$(),OBJ2$(),C$,ANSWER$(),TURNNUMBER,CONVERTNUM

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
        For J = 1 To 18 'CONVERT OBJECT$ LACKING * OR !
            If OBJECT$ = OBJ$(J) Then Let OBJECT$ = OBJ2$(J)
        Next J
        'CHECK FOR EXTRA WORDS
        COUNTER = 0
        For J = 1 To Len(OBJECT$)
            Let DUMMY$ = Mid$(OBJECT$, J, 1)
            If DUMMY$ = Chr$(32) Then COUNTER = COUNTER + 1
        Next J

        IF COUNTER<>0 THEN 
            PRINT "One space (two words) only, please."
        ELSE 
            DUMMY2=1
        End If
    Loop
End Sub

'MODULE 2.3 EVALUATE THE COMMANDS
Sub EVALUATE
    SHARED VERB$, OBJECT$,ROOM,DIRECTION,MOVEMENTTABLE(),NUMBERROOMS
    SHARED ROOMOBJECT$(),ITEMNUMBER,TURNNUMBER,THING$,INVENTORY$(),FLAG()
	SHARED SCORE
	
    If FLAG(1) <> 0 Then Exit Sub
    If FLAG(14) = 1 Then Exit Sub
    Select Case VERB$
        Case "QUIT"
            Let FLAG(1) = 2 'GAME NOT WON(1) OR LOST(-1):Exit Sub
        Case "SAVE"
            If OBJECT$ <> "GAME" Then Print "TRY-  SAVE GAME": Exit Sub
            Open "HANGSAV.BAS" For Output As #1

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
            Exit Sub
        Case "RESTORE", "LOAD"
            If OBJECT$ <> "GAME" Then Print "RESTORE GAME": Exit Sub
            Open "HANGSAV.BAS" For Input As #1

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
            Exit Sub
        Case "INVENTORY", "I"
            COUNTER = 0
            Print "   You carry: ";
            For I = 1 To 5
                IF INVENTORY$(I)<>"EMPTY" THEN COUNTER=COUNTER + 1:PRINT INVENTORY$(I);"   ";
            Next I
            If COUNTER = 0 Then Print "nothing at all": Exit Sub
            Print: Exit Sub
        Case "GO", "MOVE", "N", "S", "E", "W", "U", "D"
            If Len(VERB$) = 1 Then OBJECT$ = VERB$
            If OBJECT$ = "NORTH" Or OBJECT$ = "N" Then DIRECTION = 1
            If OBJECT$ = "SOUTH" Or OBJECT$ = "S" Then DIRECTION = 2
            If OBJECT$ = "EAST" Or OBJECT$ = "E" Then DIRECTION = 3
            If OBJECT$ = "WEST" Or OBJECT$ = "W" Then DIRECTION = 4
            If OBJECT$ = "UP" Or OBJECT$ = "U" Then DIRECTION = 5
            If OBJECT$ = "DOWN" Or OBJECT$ = "D" Then DIRECTION = 6
            IF OBJECT$="WELL*" AND FLAG(3)=0 THEN
               PRINT "SNAKE won't let me"
               Exit Sub
            End If
            IF ROOM=2 AND OBJECT$="WELL*" AND FLAG(3)=1 THEN 
                PRINT "OK"
                ROOM=1
                Exit Sub
            End If
            IF ROOM=7 AND FLAG(6)=0 AND DIRECTION = 3 THEN PRINT "Can't enter JAIL - door is locked":Exit Sub
            If ROOM = 18 And OBJECT$ = "CELLAR*" Then Print "OK": ROOM = 22: Exit Sub
            IF MOVEMENTTABLE (ROOM,DIRECTION) = 0 THEN
                PRINT "You can't GO that way" 
            ELSE 
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
            End If
            Exit Sub
        Case "GET", "TAKE", "PLUCK", "EXTRACT", "PULL", "REMOVE"
            IF ROOM=1 AND OBJECT$="WATER" THEN
                PRINT "Too dangerous from the LEDGE"
                Exit Sub
            End If
            IF ROOM=2 AND OBJECT$="WATER" AND FLAG(3)=0 THEN
               PRINT "The SNAKE won't let me"
               Exit Sub
            End If
            THING$ = "BUCKET"
            IF ROOM = 2 AND OBJECT$="WATER" AND FLAG(3)=1 AND FNCARRY = 0 THEN PRINT "You'll need a BUCKET.":Exit Sub
            IF ROOM =2 AND FNCARRY = 1 AND OBJECT$="WATER" AND FLAG(12)=0 THEN PRINT "The BUCKET won't reach the WATER.":Exit Sub
            IF ROOM = 2 AND OBJECT$="WATER" AND FLAG(3)=1 AND FNCARRY = 1 AND FLAG(12)=1 THEN
                PRINT "OK"
                FLAG(11)=1
                Exit Sub
            End If
            THING$ = "PAIL"
               IF FLAG(5)=1 AND ROOM=15 AND FNCARRY=0 AND OBJECT$="MILK" THEN
                  PRINT "You'll need a PAIL"
                  Exit Sub
               End If
               IF FLAG(5)=1 AND ROOM = 15 AND OBJECT$="MILK" AND FNCARRY=1 THEN
                  PRINT "OK":FLAG(23)=1
                  Exit Sub
               End If
               IF FLAG(5)=0 AND ROOM=15 AND OBJECT$="MILK" THEN
                  PRINT "The hungry COW is too nervous"
                  Exit Sub
               End If
            If ROOM <> 15 And OBJECT$ = "MILK" Then Print "Find a COW": Exit Sub
            Let THING$ = "TWEEZERS"
               IF OBJECT$="THORN" AND FNCARRY=0 THEN
                  PRINT "You'll need something to remove the THORN with."
                  Exit Sub
               End If
               IF ROOM=33 AND FLAG(4)=1 AND FNCARRY=1 AND FLAG(9)=0 AND OBJECT$="THORN" THEN
                  PRINT "The happy HORSE shakes something from its mane and runs away"
                  ROOMOBJECT$(33,1)="TURQUOISE!":FLAG(9)=1
                  Exit Sub
               End If
            If OBJECT$ = "LANTERN" Then FLAG(7) = 1
            IF ROOM=10 AND OBJECT$="KITTEN" THEN PRINT "Too shy - Runs away from you":Exit Sub

            Let HOLDING$ = OBJECT$
            Let OBJECT$ = "SUGAR"
            IF ROOM = 33 AND FLAG(4)=1 AND HOLDING$= "HORSE" AND FNPRESENT=0 AND FLAG(9)=0 THEN
               PRINT "The HORSE won't come to you without a bribe":Exit Sub
            End If
            Let OBJECT$ = HOLDING$

            If FNPRESENT = 0 Then Print "I don't see  "; OBJECT$: Exit Sub
            If Right$(OBJECT$, 1) = "*" Then Print "I can't handle it": Exit Sub
            If OBJECT$ = "LANTERN" Then FLAG(7) = 1

            IF RIGHT$(OBJECT$,5)="SNAKE" AND FLAG(3)=0 THEN PRINT "I don't handle live SNAKES, thank you very much":Exit Sub

            For I = 1 To 5
                IF INVENTORY$(I)="EMPTY" THEN 
                    INVENTORY$(I)=OBJECT$
                    PRINT "GOT IT":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY"
                    Exit Sub
                End If
            Next I
            Print "You're carrying too much.  Drop something!"
        Case "PUT", "DROP", "GIVE"
            If OBJECT$ = "MILK" Then Print "Try  DROP PAIL": Exit Sub
            THING$ = OBJECT$
            IF OBJECT$="PAIL" AND ROOM=10 AND FLAG(8)=0 AND FNCARRY = 1 THEN 
                FLAG(8)=1
                PRINT "The now full KITTEN returns with a pack rat."
                PRINT "The pack rat drops something and runs off, "
                PRINT "with the KITTEN in hot pursuit"
                ROOMOBJECT$(10,1)="RUBY!"
                PRINT:Exit Sub
            End If
            If OBJECT$ = "MILK" And ROOM = 9 Then Print "Try  DROP PAIL": Exit Sub
            If OBJECT$ = "WATER" Then Print "Try  FILL KETTLE ": Exit Sub
            THING$ = OBJECT$
            If FNCARRY = 0 Then Print "You don't have the "; OBJECT$: Exit Sub
            'PREVENTS DROPPING TWO OF ANYTHING ANYWHERE
            If FNPRESENT = 1 Then Print "Not again.": Exit Sub
            If OBJECT$ = "LANTERN" Then FLAG(7) = 0
            If OBJECT$ = "HAY" And ROOM = 15 Then FLAG(5) = 1
            For J = 1 To 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY" THEN 
                    ROOMOBJECT$(ROOM,J)=OBJECT$
                    LET INVENTORY$(ITEMNUMBER)="EMPTY" 
                    PRINT "OK"
                    Exit Sub
                End If
            Next J
            Print "This room is full, take it elsewhere": Exit Sub
        Case "LOOK", "EXAMINE", "INSPECT"
            IF ROOM=2 AND OBJECT$="WELL*" THEN PRINT "I see GOLD!  Maybe I should GO WELL*":Exit Sub
            IF ROOM=6 AND OBJECT$="MIRROR*" THEN
               PRINT "The rest of the sign can now be seen.  It says:"
               PRINT "      ITEM #2          Sometimes I'm perky,"
               PRINT "                       Sometimes I'm a drip;"
               PRINT "                       When I'm freshly brewed,"
               PRINT "                       Would you like a sip?"
               PRINT
               Exit Sub
            End If
            If OBJECT$ = "SIGN*" Then Print "Try  READ SIGN*  ": Exit Sub
            If OBJECT$ = "MAP" Then Print "Try -   READ MAP": Exit Sub
            Print "I don't see anything unexpected": Exit Sub
        Case "READ"
            IF ROOM=9 AND OBJECT$="SIGN*" THEN 
                PRINT "To MAKE TEA --"
                PRINT "Bring here and DROP the following -"
                PRINT "TEA, KETTLE, WATER, MILK, MATCHES, SUGAR, "
                PRINT "CUP, and something to burn, then type MAKE TEA"
                Exit Sub
            End If
            IF ROOM=3 AND OBJECT$="SIGN*" THEN 
                PRINT "The SIGN* says -"
                PRINT "DROP TREASURES! Such as GOLD! here and type  SCORE  "
                Exit Sub
            End If
            IF ROOM=6 AND OBJECT$="SIGN*" THEN 
                PRINT "         The SIGN* says: Some things you need,"
                PRINT "                         Can be found here;"
                PRINT "                         Just say the word,"
                PRINT "                         And they'll appear."
                PRINT "           ITEM #1:      I hang on the wall,"
                PRINT "                         With nothing to do;"
                PRINT "                         When you LOOK at me,"
                PRINT "                         You see only you."
                PRINT "       You can't make out the last 4 or 5 lines."
                PRINT "       They look backwards or something."
                PRINT
                Exit Sub
            End If
            IF ROOM=6 AND OBJECT$="MIRROR*" THEN
                PRINT "Try -    LOOK MIRROR"
                Exit Sub
            End If
            THING$ = "MAP"
            If OBJECT$ = "MAP" And FNCARRY = 0 Then Print "You don't have the MAP": Exit Sub
            THING$="MAP"
            IF OBJECT$="MAP" AND FNCARRY=1 THEN
                PRINT "Says  DOWN WEST WEST SOUTH SOUTH DIG"
                Exit Sub
            End If
            Print "Try READ SIGN* or  READ MAP": Exit Sub
        Case "OIL", "LUBRICATE", "GREASE"
            THING$ = "MINERAL-OIL"
            If FNCARRY = 0 Then Print "You don't have the MINERAL-OIL": Exit Sub
            If ROOM = 11 Then Print "OK": FLAG(13) = 1: Exit Sub
            Print "Not here": Exit Sub
        Case "DRINK", "EAT"
            Print "What is it with you and food?": Exit Sub
        Case "SCORE"
            If ROOM <> 3 Then Print "Return to the TOWN SQUARE": Exit Sub
            SCORE = 2
            For I = 1 To 10
                Let STUFF$ = ROOMOBJECT$(3, I)
                If Right$(STUFF$, 1) = "!" Then SCORE = SCORE + 14
            Next I
            Print "Your SCORE is "; SCORE
            If SCORE = 100 Then Print "You win!!!!  Fantastic Job!!!  YAHOO!!": FLAG(1) = 1: Exit Sub
            Print (100 - SCORE); " points to go": Exit Sub
        Case "CLIMB"
            If OBJECT$ = "WELL" Or OBJECT$ = "WELL*" Then Print "Try GO WELL*": Exit Sub
            THING$ = "CHAIR"
              IF ROOM=20 AND FNCARRY=1 THEN
                 PRINT "PUT or DROP CHAIR here first."
                 Exit Sub
              End If
              OBJECT$="CHAIR"
              IF ROOM=20 AND FNPRESENT=0 THEN
                 PRINT "The BOULDER is too high, GET something to stand on"
                 Exit Sub
              End If
            If ROOM = 20 Then Print "Good climbing!": ROOM = 21: Exit Sub
            Print "Come down from there!": Exit Sub
        Case "TIE", "CONNECT", "ATTACH"
            If Left$(OBJECT$, 2) = "UP" Then Print "Try  TIE ROPE ": Exit Sub
            THING$ = "ROPE": If FNCARRY = 0 Then Print "You'll need the ROPE": Exit Sub
            THING$ = "BUCKET": If FNCARRY = 0 Then Print "GET the BUCKET": Exit Sub
         IF OBJECT$="DOG" THEN 
             PRINT "You want me to try to TIE up a vicious DOG?  HA! HA!"
             Exit Sub
         End If
         IF OBJECT$="ROPE" OR OBJECT$="BUCKET" THEN
             PRINT "One end of the rope is tied to the BUCKET"
             FLAG(12)=1
             Exit Sub
         End If
         Print "I hardly know you!": Exit Sub
        Case "UNTIE", "UNDO"
          IF FLAG(12)=1 AND (OBJECT$="ROPE" OR OBJECT$="BUCKET") THEN
              PRINT "OK":FLAG(12)=0
              Exit Sub
          End If
        Case "LOWER"
            Print "Try  GET WATER": Exit Sub
        Case "MILK"
            Print "Try  GET MILK": Exit Sub
        Case "KILL"
            THING$ = "ROCK"
              IF FNCARRY=0 AND ROOM=2 THEN 
                  PRINT "You'll need a suitable weapon"
                  Exit Sub
              End If
              IF FNCARRY=1 AND ROOM=2 AND FLAG(3)=0 THEN
                  PRINT "The dead RATTLESNAKE is still frightening."
                  LET ROOMOBJECT$(2,2)="SNAKE"
                  FLAG(3)=1
                  Exit Sub
              End If
            Print "You must think I'm crazy!": Exit Sub
        Case "UNLOCK"
            If OBJECT$ = "SAFE" Then Print "Try-  OPEN SAFE": Exit Sub
            THING$ = "KEY"
            If FNCARRY = 0 Then Print "You'll need a KEY": Exit Sub
              IF ROOM=7 THEN 
                  PRINT "The JAIL DOOR* is unlocked. You can GO EAST"
                  FLAG(6)=1
                  Exit Sub
              End If
            Print "No lock here": Exit Sub
        Case "OPEN"
          IF ROOM=11 AND OBJECT$="SAFE*" AND FLAG(13)=0 THEN
              PRINT "The rusty hinge needs OIL to open"
              Exit Sub
          End If
          IF ROOM=11 AND OBJECT$="SAFE*" AND FLAG(13)=1 AND FLAG(15)=0 THEN
              PRINT "I see JADE!":ROOMOBJECT$(11,2)="JADE!"
              FLAG(15)=1
              Exit Sub
          End If
          IF ROOM=11 AND OBJECT$="SAFE*" AND FLAG(15)=1 THEN
              PRINT "It is open!"
              Exit Sub
          End If
          IF OBJECT$="DOOR*" AND FLAG(6)=0 THEN
              PRINT "The JAIL DOOR is locked!"
              Exit Sub
          End If
          IF ROOM=7 AND OBJECT$="DOOR*" AND FLAG(6)=1 THEN
              PRINT "The JAIL DOOR is open, GO EAST to enter"
              Exit Sub
          End If
          Print "No........You can't make me": Exit Sub
        Case "THROW"
            If ROOM = 2 Then Print "Try  KILL SNAKE ": Exit Sub
            Print "Not here": Exit Sub
        Case "DIG", "SHOVEL"
            THING$ = "SHOVEL"
            If FNCARRY = 0 Then Print "You'll need a SHOVEL": Exit Sub
            If ROOM = 31 Or ROOM = 5 Then Print "I hit something"
             IF ROOM=31 AND FLAG(16)=0 THEN 
                 ROOMOBJECT$(31,1)="DIAMONDS!"
                 FLAG(16)=1
                 Exit Sub
             End If
             IF ROOM=5 AND FLAG(17)=0 THEN 
                 ROOMOBJECT$(5,1)="KEY"
                 FLAG(17)=1
                 Exit Sub
             End If
             Print "The substrate is too hard here to dig": Exit Sub
        Case "MAKE", "PREPARE"
            If OBJECT$ <> "TEA" Then Print "Don't know how": Exit Sub
            If FNPRESENT = 0 Then Print "You'll need TEA": Exit Sub
            OBJECT$="KETTLE"
            IF FNPRESENT=0 OR FLAG(10)=0 THEN
                PRINT "You'll need a KETTLE FILLed with WATER."
                Exit Sub
            End If
         OBJECT$="PAIL"
            IF FNPRESENT=0 OR FLAG(23)=0 THEN 
               PRINT "You'll need a PAIL FILLed with MILK."
               Exit Sub
            End If
            OBJECT$ = "SUGAR": If FNPRESENT = 0 Then Print "You'll need the SUGAR ": Exit Sub
            OBJECT$ = "MATCHES": If FNPRESENT = 0 Then Print "You'll need some MATCHES": Exit Sub
            OBJECT$ = "CUP": If FNPRESENT = 0 Then Print "You'll need a CUP": Exit Sub
            OBJECT$ = "MAGAZINES": If FNPRESENT = 0 Then Print "You'll need something to burn": Exit Sub

            IF FLAG(18)=0 THEN 
                FLAG(4)=1
                FLAG(18)=1
                ROOMOBJECT$(9,2)="EMERALD!"
                PRINT "The grateful ENGLISHMAN leaves a big tip"
                Exit Sub
            End If
            If FLAG(18) = 1 Then Print "Not again!": Exit Sub
            Print "PUT the items you need in the TEA ROOM": Exit Sub
        Case "FILL"
            THING$ = "KETTLE"
            IF OBJECT$="KETTLE" AND FNCARRY=1 AND FLAG(11)=1 THEN
                FLAG(10)=1
                PRINT "The KETTLE is FILLed."
                Exit Sub
            End If
            IF OBJECT$="KETTLE" AND FNPRESENT=1 AND FLAG(11)=1 THEN
                FLAG(10)=1
                PRINT "The KETTLE is FILLed."
                Exit Sub
            End If
            If OBJECT$ = "BUCKET" And ROOM = 2 Then Print "Try GET WATER": Exit Sub
            If OBJECT$ = "PAIL" And ROOM = 15 Then Print "Try  GET MILK": Exit Sub
            Print "You'll need a KETTLE or a BUCKET or a PAIL": Exit Sub
        Case "ENTER"
            Print "Try GO WELL* or GO CELLAR*": Exit Sub
        Case "BURN"
            If OBJECT$ = "HAY" Then Print "Too green --- Won't BURN.": Exit Sub
            Print "Don't play with fire!": Exit Sub
        Case "MIRROR*", "MIRROR"
          PRINT "Suddenly you notice a MIRROR* on the wall"
          PRINT "opposite the SIGN*"
          LET ROOMOBJECT$(6,3)="MIRROR*"
          Exit Sub
        Case "SAY"
            Print "Just type the word you were going to say": Exit Sub
        Case "COFFEE"
         IF FLAG(22)=0 THEN 
             PRINT "Suddenly you smell and then see some"
             PRINT "fresh coffee on the bar.  So does a passing prospector"
             PRINT "who takes it, leaving some SILVER! in exchange."
             LET ROOMOBJECT$(6,4)="SILVER!"
             FLAG(22)=1
             Exit Sub
          End If
          Print "All that's left of the coffee is the aroma": Exit Sub
        Case "FEED"
            PRINT "Try the word  GIVE  followed by the name of the food."
            Exit Sub
        Case "USE"
            Print "Try - DIG or UNLOCK or TIE or PLUCK": Exit Sub
        Case "REVEAL"
            Print "Just type the name of the object by itself.": Exit Sub
        Case Else
          IF ROOM=6 AND OBJECT$="MIRROR*" THEN
             PRINT "Try -   LOOK MIRROR"
             Exit Sub
          End If
          IF OBJECT$="CELLAR*" THEN
             PRINT "Try-    GO CELLAR"
             Exit Sub
          End If
          If OBJECT$ = "WELL*" Then Print "Try-  GO WELL": Exit Sub
          Print "I don't know that verb": Exit Sub
    End Select
End Sub

'MODULE 2.3.1 DEFINE THE FUNCTION- IS THE OBJECT PRESENT?
Function FNPRESENT
    Shared ROOMOBJECT$(), OBJECT$, ITEMNUMBER
    For J = 1 To 15
             IF ROOMOBJECT$(ROOM,J)=OBJECT$ THEN _
                 FNPRESENT=1:ITEMNUMBER=J:EXIT function
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
    For I = 1 To 6: Print: Next I
    Input "Be sure that your disk is in the drive and press ENTER"; DUMMY$

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

    '    Let DTA(13) = FLAG(1): Let DTA(38) = DTA(38) + Int((Timer - STRT) / 6)
    '    Open "REPORT.DTA" For Output As #1
    '    Rem SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
    '    For I = 1 To 40
    '        Write #1, DTA(I)
    '    Next I
    '    For I = 1 To 10
    '        Write #1, DTA$(I)
    '    Next I
    '    Close #1

    Open "HANGDATA.TXT" For Append As #2
    Print #2, Date$, Time$
    For I = 0 To TURNNUMBER
        If Int(I / 5) = I / 5 Then Print #2, ANSWER$(I) Else Print #2, ANSWER$(I),
    Next I
    Print #2, Int((STRT - Timer) / 6),
    Print #2, Int(1000 * Rnd(0))
    Close #2
    Print "This game is over.  Type HANGTOWN to play again."
End Sub

'END OF PROGRAM

