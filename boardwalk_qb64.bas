'MODULE 0 MAIN    ******BOARDWALK******10-17-95
'CONVERTED FROM CURSED (SEE NEXT 4 REMS)
'CHANGED GAMEDTA.BAS TO CURSAV.BAS ON 1-24-91
'CHANGED TURNNUMBER TO GYNTURN FOR GIANT TURNS 18FEB92
'CHANGED TIMER ON 2-3-94
'CHANGED SAVE GAME TIME$ DATE$ 7-30-95
'CHANGED ROOMOBJECT$ LIMIT TO 15 OBJECTS THROUGHOUT 10-24-95
'MANY CHANGES ON THE WEEK OF 10-24-95
'copyright 1995 by John H. Doolittle  All rights reserved.  Thank you.
'10-29-95 put spaces in logon name date time
'CONVERTED UPTOWN TO BOARDWALK STARTING 1-19-96
'changes through 1-28-96
'REPORT STUFF 3-5-96
' REFIXED ORANGE FLAG 5-7-96  BOARDATA.TXT 10-24-96
'7-4-23, Henrik Aasman: Modified for QB64
'  Moved all definition of global (shared) variables to Main module
'  QB64 needs unique names so MOVEMENTTABLE --> MOVEMENTTABLESUB and INVENTORY --> INVENTORYSUB
'  Changed syntax for BEEP so BEEP 3 --> BEEP
'  Changed syntax for DELAY so DELAY --> _DELAY
'  Fixed __ to _
'  Renamed DEF to FUNCTION
'  Removed reading/writing to REPORT.DTA
'  Added "" around some of the data in DATA-tables
'  Extended DIMensioning of FLAG,CLUE$ & WURD$ to 33
'  Extended DIMensioning of ANSWER$ to 305

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
ReDim Shared WURD$(0)
ReDim Shared LOGIC$
ReDim Shared SLAMMER
ReDim Shared TRYNUMB
ReDim Shared TOTAL
ReDim Shared ITEMNUMBER
ReDim Shared VERB$
ReDim Shared OBJECT$
ReDim Shared C$
ReDim Shared DIRECTION
ReDim Shared STRT

Cls: ReDim FLAG(33), OBJ$(30), OBJ2$(30), CLUE$(33), WURD$(33)
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
    Data 33,BALLROOM,DRESS SHOP,JEWELRY STORE,BASKETBALL SHOOT,CRAFTS CENTER
    Data SHOOTING GALLERY,ROLLER-COASTER,BOARDWALK,BOARDWALK,BOARDWALK
    Data BOARDWALK,BOARDWALK,BOARDWALK,BOARDWALK,STORAGE ROOM
    Data END OF BEACH,BEACH,WHARF,BEACH,BEACH
    Data BEACH,TIDEPOOL,SURF,SURF,WHARF
    Data SURF,SURF,SURF,WHARF,FISH MARKET
    Data OCEAN FLOOR,"END OF WHARF",SEAL ROCK

    For I = 1 To NUMBERROOMS
        Let DESCRIBEFLAG(I) = 0
        Read ROOMDESCRIBE$(I), ROOMDES2$(I)
    Next I
    Rem ROOM 1
    Data "The music here in the BALLROOM, though melancholy, suggests that"
    Data "love is in the air.  Get cracking, you matchmaker, you."

    Data "Here in the DRESS SHOP is a beautiful ballroom GOWN that just been"
    Data "marked down to $115 ---WOW!  Too bad you don't have that much, yet."

    Data "The JEWELER* here in the JEWELRY STORE is eyeing you very closely."
    Data "Andrea's getting jumpy, keeps shouting 'I'm not a thief!'"

    Data "Here at the BASKETBALL SHOOT, you can SHOOT a BASKET  and win a "
    Data "stuffed ANIMAL.  Cool!  Except that you hate stuffed animals."

    Data "It's funny that you haven't notice some sort of SIGN*  here at "
    Data "the CRAFTS CENTER.  Perhaps you should return later."
    Rem 6
    Data "Here at the SHOOTING GALLERY is an unhappy SAILOR*.  He's trying"
    Data "to win a stuffed ANIMAL for his date but can't SHOOT straight."

    Data "Here at the ROLLER-COASTER, you can't tell who looks the most "
    Data "pale - the people getting on or the people getting off."

    Data "Here the BOARDWALK dead ends against the sheer wall of the sandstone"
    Data "cliff.  "

    Data "Lots of splinters in this section of the BOARDWALK  "
    Data "  "

    Data "Excuse me!  Isn't there a dress code on this BOARDWALK?  "
    Data "  "
    Rem 11

    Data "A badly burned, portly gentleman just passed by on this section  "
    Data "of the BOARDWALK.  Doesn't he teach at Sac State?  "

    Data "On this section of the BOARDWALK, you can still hear the screams "
    Data "coming from the folks enjoying the ROLLER-COASTER*  "

    Data "All along the BOARDWALK are steps leading down to the BEACH  "
    Data "  "

    Data "This end of the BOARDWALK ends outside a STORAGE ROOM"
    Data "  "

    Data "The LOCKER in the STORAGE ROOM is probably full of good stuff,"
    Data "too bad it's locked.  Andrea thinks you should find the key.  Duh!"
    Rem 16
    Data "Here the BEACH dead ends against the cliff wall.  READ the SIGN*"
    Data "and REVEAL something that you'll need."

    Data "This is the section of BEACH, next to the WHARF that has the best"
    Data "surf in the area.  The pilings are too close to 'shoot the pier.'"

    Data "This is the beginning of the world-famous WHARF.  You wonder what"
    Data "keeps the rotten-looking wooden structure up."

    Data "An attractive, but poorly dressed WOMAN* sits here on the BEACH"
    Data "gazing disconsolately out to sea.  Is she swaying?"

    Data "As you enter this section of the BEACH you overhear someone "
    Data "talking about a fraternity party.  Something about soap suds."
    Rem 21
    Data "As you enter this section of the BEACH the STORAGE ROOM guy tells"
    Data "Andrea that he has a hole in his pocket.  Andrea says, 'oh.'"

    Data "At the end of the beach is a terrific TIDEPOOL, which is uncovered"
    Data "right now revealing a STARFISH*.  He looks crabby."

    Data "The SURF here near the cliff is too dangerous to surf."
    Data "  "

    Data "The waves here near the WHARF curl very nicely.  Ouch, some dude"
    Data "just took a header off the end of his board. Ah, the taste of sand."

    Data "A wharf GUARD* is taking things from people before letting them"
    Data "GO SOUTH along the wharf.  Isn't that illegal?"


    Rem 26
    Data "The SURF here is quite cold. "
    Data "  "

    Data "Andrea is complaining about the salt water of the SURF here  "
    Data "  "

    Data "The waves are smaller and less dangerous in this part of  "
    Data "the surf "

    Data "This middle section of WHARF is opposite the FISH MARKET."
    Data "   "

    Data "A somewhat downcast FISHMONGER* dominates the scene at the"
    Data "FISH MARKET -- unless you count the smell."
    Rem 31
    Data "The water here on the OCEAN FLOOR is quite clear and pleasant."
    Data " You see something that a STARFISH* could go for."

    Data "Here at the end of the WHARF you can see SEAL ROCK to the south,"
    Data "but you can't GO there until you MAKE a BRIDGE.  Is that a pelican?"

    Data "Here on SEAL ROCK you see a most forlorn PELICAN* who is hope-"
    Data "lessly tangled in some fishing LINE.  UNTANGLE him, please."

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
    Data 0,8,0,0,0,0
    Data 0,9,0,0,0,0
    Data 0,10,0,0,0,0
    Data 0,11,0,0,0,0
    Data 0,12,0,0,0,0

    Data 0,13,0,0,0,0
    Data 0,14,0,0,0,0
    Data 1,0,9,0,0,16
    Data 2,0,10,8,1,17
    Data 3,0,11,9,0,18

    Data 4,0,12,10,0,19
    Data 5,0,13,11,0,20
    Data 6,0,14,12,0,21
    Data 7,0,15,13,0,0
    Data 0,0,0,14,0,0

    Data 0,23,17,0,8,0
    Data 0,24,18,16,9,0
    Data 0,25,19,17,10,0
    Data 0,26,20,18,11,0
    Data 0,27,21,19,12,0

    Data 0,28,22,20,13,0
    Data 0,0,0,21,0,0
    Data 16,0,0,0,0,0
    Data 17,0,0,0,0,0
    Data 18,29,0,0,0,0

    Data 19,0,0,0,0,0
    Data 20,0,0,0,0,0
    Data 21,0,0,0,0,0
    Data 25,32,30,0,0,0
    Data 0,0,0,29,0,0

    Data 0,0,0,0,29,0
    Data 29,33,0,0,0,0
    Data 32,0,0,0,0,0
End Sub

'MODULE 1.4 INVENTORY
Sub INVENTORYSUB
    Shared INVENTORY$()
    ReDim INVENTORY$(5)
    For I = 1 To 5
        Let INVENTORY$(I) = "EMPTY"
    Next I
    Let INVENTORY$(5) = "SURFBOARD"
End Sub

'MODULE 1.5 OTHERS
Sub OTHERS
    SHARED GAMESTATE$,ROOM,TURNNUMBER,ROOMOBJECT$(),THING$,INVENTORY$(),_
        FLAG(),OBJ$(),OBJ2$(),DESCRIBEFLAG(),ROOMDESCRIBE$(),ROOMDES2$(),_
        GUESSNUMB,CLUE$(),WURD$(),NUMBERROOMS,LOGIC$,SLAMMER,TRYNUMB,TOTAL
    Let TURNNUMBER = 0
    Let THING$ = ""
    For I = 1 To 33: Let FLAG(I) = 0: Next I 'SET FLAGS TO 0
    'FLAG #  MEANING OF 0      FLAG# MEANING OF 0
    ' 1      GAME NOT OVER       2 SEAL ROCK BRIDGE NOT MADE
    'FOR 1, 1 MEANS GAME WON -1 GAME LOST 2 MEANS GAME QUIT
    '3 PELICAN NOT UNTANGLED     4 FISH NOT SWAPPED
    '5 BASKET NOT SHOT           6 ANIMAL NOT GIVEN
    '7 DRIFTWOOD NOT REVEALED    8 DANCER NOT REVEALED
    '9 LOCKER LOCKED            10 STARFISH NOT SEEN
    '11 HICCUPS NOT CURED       12 GUARD* NOT FED
    '13 DOOR NOT LOCKED         14 EXITS SUB TO REFRESH SCREEN
    '18 GUESS MEDAL             19 orange not revealed
    '20 GUESS CANS
    '29 GUESS OYSTERS
    Let FLAG(18) = 1: Rem NOT FIRST TO BE GUESSED
    Let FLAG(29) = 1: Rem NOT THIS ONE EITHER

    'SET UP WORD GUESSING DATA
    For I = 1 To NUMBERROOMS
        Let CLUE$(I) = "": Let WURD$(I) = ""
    Next I
    Let CLUE$(18) = "OLYMPIC"
    Let WURD$(18) = "MEDAL"
    Let CLUE$(20) = "BEER"
    Let WURD$(20) = "CANS"
    Let CLUE$(29) = "CLAM"
    Let WURD$(29) = "OYSTER"
    GUESSNUMB = 0: 'TOTAL NUMBER OF GUESSES AT WORDS
    TRYNUMB = 0: 'TOTAL # OF TRIES AT COMBO DOOR LOCK
    SLAMMER = 0: 'TOTAL # OF TRIES AT FINAL SOLUTION MATRIX
    LOGIC$ = "Logfal - "
    TOTAL = 0

    For I = 1 To 39
        Read ROOM, J, ROOMOBJECT$(ROOM, J)
    Next I
    Data 1,1,SIGN*,1,2,EMPTYMAN*,2,1,EMPTYGOWN
    Data 3,1,JEWELER*,4,1,EMPTYBASKETBALL,5,1,EMPTYSIGN*
    Data 6,1,SAILOR*,13,1,EMPTYSIGN*,13,2,EMPTYVENDING-MACHINE
    Data 14,1,RECYCLER*,15,1,LOCKER*,15,2,EMPTYSCUBA
    Data 15,3,EMPTYWETSUIT,15,4,EMPTYFIN,15,5,EMPTYSAWBUCK

    Data 15,6,EMPTYC-NOTE,16,1,SIGN*,16,2,EMPTYDRIFTWOOD
    Data 19,1,WOMAN*,20,1,EMPTYCANS,22,1,STARFISH*
    Data 22,2,EMPTYSHELL*,29,1,EMPTYLADDER*,30,1,SIGN*
    Data 30,2,FISHMONGER*,30,3,EMPTYFISH*,31,1,OYSTER
    Data 31,2,LADDER*,33,1,PELICAN*,33,2,EMPTYFISH

    Data 7,1,SIGN*,32,1,EMPTYBRIDGE*,33,3,BRIDGE*
    Data 9,1,SIGN*,10,1,SIGN*,10,2,EMPTYORANGE
    Data 25,1,GUARD*,15,7,EMPTYSIGN*,15,8,EMPTYKEY*

    'THE STARTING ROOM IS THE ENTRYWAY - 7 ROLLER COASTER
    ROOM = 7


    For I = 1 To 17 'READ IN WORD!* CONVERSION
        Read OBJ$(I), OBJ2$(I)
    Next I
    Data SIGN,SIGN*,MAN,MAN*,JEWELER,JEWELER*
    Data BALL,BASKETBALL,SAILOR,SAILOR*,MACHINE,VENDING-MACHINE*
    Data RECYCLER,RECYCLER*,LOCKER,LOCKER*,WOOD,DRIFTWOOD
    Data WOMAN,WOMAN*,CAN,CANS,STARFISH,STARFISH*
    Data LADDER,LADDER*,FISHMONGER,FISHMONGER*,PELICAN,PELICAN*

    Data GUARD,GUARD*,BOARD,SURFBOARD

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
           PRINT "BOARDWALK (at the DOS prompt) and later,":_
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
        ROOMDES2$(),CLUE$(),WURD$(),GUESSNUMB,TRYNUMB,LOGIC$,ITEMNUMBER
    Print: Print
    If TURNNUMBER = 1 Then Call GAMESTARTER 'MODULE 2.1.1

    If DESCRIBEFLAG(ROOM) = 1 Then Print "You are at the  "; DESCRIPTION$(ROOM)
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
    If ROOM = 18 Or ROOM = 20 Or ROOM = 29 Then _Delay 0.3
     IF (ROOM=18 OR ROOM=20 OR ROOM=29) AND FLAG(ROOM)=0 THEN PRINT _
       "Andrea says that she's thinking of a ";LEN(WURD$(ROOM));_
        "-letter word":PRINT _
       "that has something to do with ";CLUE$(ROOM);".":_DELAY 0.3:PRINT:PRINT _
       "If you would like to guess what the word is, type GUESS followed ":_
       PRINT "by your guess, for example,  GUESS COW ":_DELAY 0.3

    THING$ = "ORANGE"
     IF ROOM=25 AND FNCARRY=1 THEN _
       LET INVENTORY$(ITEMNUMBER)="EMPTY":PRINT:FLAG(12)=1:_
       LET ROOMOBJECT$(ROOM,1)="EMPTY":FLAG(14)=1: PRINT _
       "The GUARD* takes the ORANGE and wanders off.  Creep."


    THING$ = "CANS"
     IF ROOM=14 AND FNCARRY=1 THEN _
       LET INVENTORY$(ITEMNUMBER)="BUCK":PRINT:PRINT _
       "The RECYCLER* glommed on to the CANS and slipped you a BUCK.  Cool!":_
       PRINT:LET ROOMOBJECT$(ROOM,1)="EMPTY":_
       LET FLAG(14)=1:REM EXITS SUB

    THING$ = "DETECTOR"
    If ROOM = 21 And FNCARRY = 1 Then
       LET INVENTORY$(ITEMNUMBER)="KEY":PRINT:PRINT _
       "You detected and picked up a KEY.  Promising!":PRINT:PRINT _
       "Just then the rental guy came by and took back the overdue":PRINT _
       "DETECTOR.  Andrea says she doesn't remember anything about":PRINT _
       "a time limit.  Da noive a da guy!":LET FLAG(14)=1
    End If


    THING$ = "OYSTER"
    If ROOM = 22 And FNCARRY = 1 Then
        Let INVENTORY$(ITEMNUMBER) = "PEARL": Print
        Print "You slip momentarily in the TIDEPOOL, dropping the OYSTER into the"
        Print "water.  In a flash, the STARFISH has it pried open and consumed,"
        Print "(that was disgusting), leaving you with only the PEARL.  Yes!"
        Let FLAG(14) = 1
    End If

    THING$ = "PEARL"
    If ROOM = 3 And FNCARRY = 1 Then
        Let INVENTORY$(ITEMNUMBER) = "C-NOTE"
        Print "The JEWELER's on you like cheese on a burger when he sees the"
        Print "PEARL.  Before you can react, he's taken the PEARL and given "
        Print "you $100.  Andrea asks if it's dress time.": Let FLAG(14) = 1
    End If

    THING$ = "GOWN"
    If ROOM = 19 And FNCARRY = 1 Then
        FLAG(1) = 1: INVENTORY$(ITEMNUMBER) = "EMPTY"
        Print
        Print "The WOMAN*'s face lights up when she sees the GOWN that "
        Print "you and Andrea hand her.  After thanking you profusely,"
        Print "she sprints off in the direction of the changing room."
        _Delay 5.5
        Print
        Print "#################################################"
        Print: Print
        Print "You follow the her to the BALLROOM in time to see the"
        Print "happy couple dancing the waltz.  Whatever the waltz is."
        Print "The MAN* turns out to be ex-surfing champ Blackjack Brown,"
        Print "who tosses you the wax you need to surf your best.  Dude."
        Exit Sub
    End If

     IF FLAG(11)=0 THEN PRINT _
       "Andrea has the hiccups -- wants to tell you something - but can't!"
    Print

End Sub

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
Sub GAMESTARTER
    Cls
    Print "              WELCOME TO THE BOARDWALK!"
    Print
    Print "  Okay, here's the deal:  Your seriously bitchin' poem, "
    Print "'Surf's up, dude!' won the Surfer's Open Poetry Contest and"
    Print "now you have the first prize: King Kamehameha's original,"
    Print "humungously long, wooden surfboard.  Excellant!  Unfortunately,"
    Print "you can't try it out -- no wax!  Bummer!  "
    Print "  You'll just have to wander around the BOARDWALK doing good"
    Print "until virtue is rewarded and you happen upon some wax.  Hurry"
    Print "though -- that storm out in the Pacific has stirred up some"
    Print "kicker waves.  Tubular!"
    Print
    Print "    Give COMMANDs as VERB then OBJECT, Such as GO NORTH,"
    Print "READ SIGN, RIDE ROLLER-COASTER*, CLIMB LADDER*, SHOOT BASKET,"
    Print "UNTANGLE PELICAN*, GET SCUBA (Self-Contained Underwater Breathing"
    Print "Apparatus - if you must know), PAY SAWBUCK ($10), PAY FIN ($5)"
    Print "PAY C-NOTE ($100), MAKE BRIDGE, SWAP FISH, DEPOSIT BUCK ($1), etc."
    Print "    Exceptions to this two-word sentence rule are single-"
    Print "letter commands such as N to GO NORTH, and U to GO UP."
    Print "    P.S. Don't try to get objects ending in an *, e.g., SAFE*,"
    Print "as they are quite unobtainable.  "
    Print
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
    Shared ROOM, ROOMOBJECT$(), FLAG()
    If ROOM = 20 Then
        Let ROOMOBJECT$(ROOM, 1) = "CANS"
        Let FLAG(18) = 0: Rem ENABLES YOU TO GET MEDAL CLUE
        Print "Andrea asks if you noticed how many CANS people strew about.  Strew?"
    End If
    If ROOM = 18 Then
        Let ROOMOBJECT$(13, 1) = "SIGN*"
        Let ROOMOBJECT$(13, 2) = "VENDING-MACHINE*"
        Let FLAG(29) = 0: Rem ENABLES YOU TO GET OYSTER CLUE
        Print "Andrea suggests you DEPOSIT the BUCK at the VENDING-MACHINE*"
        Print "and rent a metal DETECTOR"
    End If
    If ROOM = 29 Then
        Let ROOMOBJECT$(29, 1) = "LADDER*"
        Print "Andrea suggests you CLIMB LADDER* down to the OYSTER."
    End If
End Sub

'MODULE 2.2 COMMANDS
Sub COMMANDS
    SHARED VERB$, OBJECT$,ROOM,DESCRIPTION$(),FLAG(),INVENTORY$(),_
        ROOMOBJECT$(),OBJ$(),OBJ2$(),C$,ANSWER$(),TURNNUMBER,_
        CLUE$(),WURD$(),GUESSNUMB,TOTAL

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
        GUESSNUMB,CLUE$(),WURD$(),LOGIC$,TRYNUMB,SLAMMER,TOTAL
    If FLAG(1) <> 0 Then Exit Sub
    If FLAG(14) = 1 Then Exit Sub
    Select Case VERB$
        Case "QUIT", "Q"
            Let FLAG(1) = 2 'GAME NOT WON(1) OR LOST(-1):EXIT SELECT
        Case "SAVE"
            If OBJECT$ <> "GAME" Then Print "SAVE GAME": Exit Select
            Open "BOARDSAV.BAS" For Output As #1
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
            Write #1, GUESSNUMB, LOGIC$, SLAMMER, TRYNUMB, TOTAL
            Close #1
            Print "OK"
            Exit Select
        Case "RESTORE"
            If OBJECT$ <> "GAME" Then Print "Try  RESTORE GAME": Exit Select
            Open "BOARDSAV.BAS" For Input As #1
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
            Input #1, GUESSNUMB, LOGIC$, SLAMMER, TRYNUMB, TOTAL
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
       IF DIRECTION=2 AND ROOM=32 AND FLAG(2)=0 THEN PRINT _
         "You'll need to MAKE a BRIDGE to GO SOUTH":EXIT SELECT
       IF DIRECTION=2 AND ROOM=25 AND FLAG(12)=0 THEN PRINT _
         "The GUARD* wants a piece of fruit first.  Shakedown.":EXIT SELECT
       IF DIRECTION=4 AND ROOM=15 AND FLAG(13)=1 THEN PRINT _
         "Can't the DOOR's locked.  Andrea says: Try - READ SIGN*":EXIT SELECT
       IF DIRECTION=2 AND ROOM=7 AND FLAG(11)=0 THEN PRINT _
         "Help Andrea with her hiccups before moving on.":EXIT SELECT

            IF MOVEMENTTABLE (ROOM,DIRECTION) = 0 THEN _
                PRINT "You can't go that way." ELSE _
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
            Exit Select

        Case "GET", "TAKE"
            IF OBJECT$="SURFBOARD" THEN PRINT _
              "Leave it here until you get some wax":EXIT SELECT

            If FNPRESENT = 0 Then Print "I DON'T SEE  "; OBJECT$: Exit Select
            If Right$(OBJECT$, 1) = "*" Then Print "I CAN'T HANDLE IT": Exit Select

            For I = 1 To 5
                IF INVENTORY$(I)="EMPTY" THEN INVENTORY$(I)=OBJECT$:_
                    PRINT "GOT IT":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY":_
                    EXIT SELECT
            Next I

            Print "You're carrying too much.  Drop something first."
            Exit Select
        Case "PUT", "DROP", "GIVE"
            IF OBJECT$="SURFBOARD" THEN PRINT_
             "Andrea thinks you'll need it.":EXIT SELECT
            If OBJECT$ = "FISH" Then Print "Try - SWAP FISH": Exit Select
            IF OBJECT$="BASKETBALL" THEN PRINT _
              "Try - SHOOT BASKET":EXIT SELECT
            THING$="ANIMAL":IF OBJECT$="ANIMAL" AND ROOM=6 AND FNCARRY =1 _
              AND FLAG(6)=0 THEN LET INVENTORY$(ITEMNUMBER)="KNIFE":_
              LET FLAG(6)=1:LET ROOMOBJECT$(ROOM,1)="EMPTY": PRINT _
              "The grateful sailor runs off to find his date, but not"_
              "before giving you his swell pocket KNIFE!":EXIT SELECT
            IF OBJECT$="C-NOTE" OR OBJECT$="SAWBUCK" OR OBJECT$="FIN" THEN _
              PRINT "Try PAY -- Andrea fears you'll lose them.":_
              EXIT SELECT
            THING$ = OBJECT$
            If FNCARRY = 0 Then Print "You don't have the "; OBJECT$: Exit Select


            For J = 1 To 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":_
                    EXIT SELECT
            Next J
            Print "This room is full; take it elsewhere": Exit Select

        Case "READ"
            IF FNPRESENT=0 THEN PRINT _
             "I don't see the ";OBJECT$:EXIT SELECT
            IF ROOM=5 THEN PRINT _
             "It says: Bring DRIFTWOOD, LINE, and a KNIFE here and":PRINT _
             "type MAKE MOBILE.  We have buyers standing by!  Do it now!":_
             EXIT SELECT
            IF ROOM=7 AND FLAG(11)=0 THEN PRINT _
             "The sign says: To ride on the big dipper, type":PRINT_
             "RIDE ROLLER-COASTER*   The first ride is free.":EXIT SELECT
           IF ROOM=7 AND FLAG(11)=1 THEN PRINT _
             "It says:  Closed for cleaning.":EXIT SELECT
           IF ROOM=1 THEN PRINT _
             "It says: Solve the following riddle and REVEAL the object,":PRINT_
             "e.g., REVEAL ROMANCER.  Here's the riddle:":PRINT_
             "     I rhyme well with PRANCER,":PRINT_
             "     but don't rhyme well with CUPID;":PRINT _
             "     You move to the music,":PRINT_
             "     and try not to look stupid.":PRINT:EXIT SELECT
           IF ROOM=30 THEN PRINT _
             "It says: Will pay cash for FISH - 5 fins for a fin.":PRINT _
             "Bring FISH here and type SWAP FISH":EXIT SELECT
           IF ROOM=16 THEN PRINT _
            "It says: REVEAL something that is nine letters long and ":PRINT _
            "frequently found on ocean beaches.":EXIT SELECT
           IF ROOM=13 THEN PRINT _
             "It says: DEPOSIT BUCK here to rent a metal DETECTOR":EXIT SELECT
           IF ROOM=9 THEN PRINT _
             "It says: If you should REVEAL APPLE then the DRESS SHOP":PRINT_
             "is west of the JEWELRY STORE.":EXIT SELECT
           IF ROOM=10 THEN PRINT _
             "It says: If you should REVEAL ORANGE then the JEWELRY":PRINT_
             "STORE is east of the CRAFTS CENTER.":EXIT SELECT
            If ROOM = 15 Then
                Print "It says: The door lock is a 5 switch (Up or Down) combination"
                Print "To open, try different combinations of U and D: e.g., "
                Print "TRY UDUDU or TRY DDDUD.  Then, listen closely to hear the"
                Print "number of clicks coming from the lock mechanism.  Five "
                Print "clicks and the door will swing open.  Good luck."
                Exit Select
            End If

            Print "Andrea says that we're in the wrong room for that."
            Exit Select

        Case ",DRINK", "IMBIBE"
            Print "Andrea says that you are not thirsty.": Exit Select
        Case "GUESS"
        IF ROOM<>18 AND ROOM<>20 AND ROOM<>29 THEN PRINT _
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
        PRINT "Andrea says that ";HITS;" letter(s) of your word ":PRINT _
              "are also found in her word.  Try again."
            Exit Select
        Case "PLAY"
            Print "Andrea warns that we have no time for such foolishness."
            Exit Select
        Case "OPEN"
            If ROOM = 15 Then Print "Try  --- UNLOCK LOCKER": Exit Select
            Print "Doesn't work. ": Exit Select
        Case "CLOSE"
            Print "Andrea thinks you should leave it open.": Exit Select
        Case "BREAK"
            Print "Andrea is sulking.  She hates violence."
            Exit Select
        Case "DIG"
            Print "It's hopeless.  This is a job for a metal DETECTOR."
            Exit Select
        Case "USE"
            Print "To do what?  Be more specific."
            Exit Select
        Case "UNLOCK"
            THING$ = "KEY"
         IF FNCARRY=0 THEN PRINT _
          "Get the key first.":EXIT SELECT
            If ROOM <> 15 Then Print "Not here": Exit Select
            If FLAG(9) = 1 Then Print "Done that.": Exit Select
            Let FLAG(9) = 1: Let ROOMOBJECT$(ROOM, 2) = "SCUBA"
            Let ROOMOBJECT$(ROOM, 3) = "WETSUIT"
         LET INVENTORY$(ITEMNUMBER)="EMPTY":PRINT _
          "You see a WETSUIT and SCUBA in the LOCKER.  The KEY is stuck.":_
          _DELAY 0.3:PRINT "Oh great, the door just slammed shut locking.":_
          PRINT "Andrea thinks you should READ the SIGN*."
            Let ROOMOBJECT$(ROOM, 7) = "SIGN*"
            Let ROOMOBJECT$(ROOM, 8) = "KEY*"
            Let FLAG(13) = 1
            Exit Select
        Case "PICK"
            Print "Relax kid ---  Houdini you're not.": Exit Select
        Case "CLIMB"
         IF ROOM=31 THEN PRINT "Up you go!  Watch out for the bends.":_
           LET ROOM=29:EXIT SELECT
            If ROOM <> 29 Then Print "Not here.": Exit Select
         THING$="WETSUIT": IF FNCARRY=0 THEN PRINT _
          "The water is cold, GET a WETSUIT.":EXIT SELECT
         THING$="SCUBA":IF FNCARRY=0 THEN PRINT _
           "At this depth, you'll need diving equipment (SCUBA)":EXIT SELECT
            Let ROOM = 31: Print "A little awkward -- but OK"
            Exit Select

        Case "DEPOSIT"
        IF OBJECT$<>"BUCK" THEN PRINT _
          "Andrea says that the only thing you DEPOSIT is the BUCK":EXIT SELECT
            If ROOM <> 13 Then Print "Not here.": Exit Select
        THING$="BUCK":IF FNCARRY<>1 THEN PRINT_
         "Andrea says you need to GET the BUCK first.":EXIT SELECT
            Let INVENTORY$(ITEMNUMBER) = "DETECTOR"
            Print "You now carry a fine metal DETECTOR.  It says:"
            Print "For BEACH use only.": Exit Select

        Case "PAY"
            If ROOM <> 2 Then Print "Not here.": Exit Select
            DUMMY = 0
       THING$=OBJECT$:IF FNCARRY=0 THEN PRINT _
        "You don't have the ";OBJECT$:EXIT SELECT
            If OBJECT$ = "C-NOTE" Then TOTAL = TOTAL + 100: DUMMY = 1
            If OBJECT$ = "SAWBUCK" Then TOTAL = TOTAL + 10: DUMMY = 1
            If OBJECT$ = "FIN" Then TOTAL = TOTAL + 5: DUMMY = 1
            INVENTORY$(ITEMNUMBER) = "EMPTY"
            Print "The total paid so far is "; TOTAL; " dollars."
            Print "Additional needed for GOWN = "; 115 - TOTAL; " dollars."
       IF TOTAL=115 THEN LET INVENTORY$(ITEMNUMBER)="GOWN":_
        LET ROOMOBJECT$(ROOM,1)="EMPTY":PRINT:PRINT _
        "The shopkeeper takes the last of your money and":PRINT_
        "gives you the beautiful ballroom GOWN.":EXIT SELECT
            If DUMMY = 0 Then Print "That's not for that."
            Exit Select

        Case "RIDE"
            If ROOM <> 7 Then Print "Not here.": Exit Select
       IF FLAG(11)=1 THEN PRINT _
         "The ROLLER-COASTER* is closed for cleaning.":EXIT SELECT
            Let FLAG(11) = 1: Let ROOMOBJECT$(4, 1) = "BASKETBALL"
            Print "You and Andrea have never been so scared.  Andrea's hiccups are"
            Print "definitely gone.  She says she wants to see you SHOOT a BASKET."
            Exit Select

        Case "UNTANGLE"
            If ROOM <> 33 Then Print "Not here.": Exit Select
            If FLAG(3) = 1 Then Print "Not again.": Exit Select
            Let FLAG(3) = 1: Let ROOMOBJECT$(ROOM, 1) = "LINE"
      LET ROOMOBJECT$(ROOM,2)="FISH":PRINT _
       "Ain't that the darndest thing -- the grateful PELICAN has gone ":PRINT_
       "off and returned with lots of smelly FISH.  Imagine your pleasure."
            Exit Select

        Case "IMAGINE"
            Print "Cute --- very cute.": Exit Select

        Case "SWAP"
            If ROOM <> 30 Then Print "Not here.": Exit Select
            If FLAG(4) = 1 Then Print "Not again.": Exit Select
            THING$ = "FISH": If FNCARRY = 0 Then Print "GET the FISH first.": Exit Select
            FLAG(4) = 1: INVENTORY$(ITEMNUMBER) = "FIN"
     ROOMOBJECT$(ROOM,2)="FISH*":PRINT _
       "The nice FISHMONGER has given you a FIN ($5) for the FISH."
            Exit Select

        Case "SHOOT"
            If ROOM <> 4 Then Print "Not here.": Exit Select
            If FLAG(5) = 1 Then Print "Not again.": Exit Select
     THING$="BASKETBALL":IF FNCARRY=0 THEN PRINT _
       "GET the BASKETBALL first.":EXIT SELECT
            Print "Nothing but net!  You coulda been a contenda!  Your reward is a "
            Print "nice stuffed ANIMAL.  Andrea turns away - she's allergic."
            Let INVENTORY$(ITEMNUMBER) = "ANIMAL"
            Exit Select

        Case "REVEAL"
    IF ROOM<>1 AND ROOM<>9 AND ROOM<>10 AND ROOM<>16 THEN PRINT_
      "Not here.":EXIT SELECT
            If ROOM = 16 Then
                If FLAG(7) = 1 Then Print "Not again.": Exit Select
      IF LEN(OBJECT$)<>9 THEN PRINT _
        "REVEAL a word 9 letters long, please.":EXIT SELECT
                If OBJECT$ <> "DRIFTWOOD" Then Print "No, try again.": Exit Select
      LET ROOMOBJECT$(ROOM,2)="DRIFTWOOD":LET FLAG(7)=1:PRINT _
        "Andrea wonders why you hadn't notice the DRIFTWOOD and ":PRINT_
        "bets that you could MAKE a swell MOBILE with it at the":PRINT_
        "CRAFTS CENTER.":LET ROOMOBJECT$(5,1)="SIGN*":_
        EXIT SELECT
            End If
            If ROOM = 1 Then
                If FLAG(8) = 1 Then Print "Not again.": Exit Select
                If OBJECT$ <> "DANCER" Then Print "No, try again.": Exit Select
                Let FLAG(8) = 1: Let ROOMOBJECT$(ROOM, 2) = "MAN*"
                Print "Andrea notices the forlorn figure of a MAN* dancing alone."
            End If
            If ROOM = 9 Then
                If OBJECT$ <> "APPLE" Then Print "I'm confused - READ SIGN*": Exit Select
                Print "Andrea thinks you fell for an AFFIRMING THE CONSEQUENT "
      PRINT "fallacy.  Look over that section of the workbook.":_
      LET LOGIC$=LOGIC$+"..AFFIRM..":EXIT SELECT
            End If
            If ROOM = 10 Then
                If FLAG(19) = 1 Then Print "Not again.": Exit Select
                If OBJECT$ <> "ORANGE" Then Print "I'm confused -- READ the SIGN*": Exit Select
                Let ROOMOBJECT$(ROOM, 2) = "ORANGE": Let FLAG(19) = 1
                Print "Andrea suggests we OFFER the ORANGE to the GUARD*": Exit Select
            End If
            Exit Select

        Case "MAKE"
            If ROOM <> 32 And ROOM <> 5 Then Print "Not here.": Exit Select
            If ROOM = 32 Then
                If FLAG(2) = 1 Then Print "It's been done.": Exit Select
                Let ROOMOBJECT$(ROOM, 1) = "BRIDGE*"
      THING$="SURFBOARD":IF FNCARRY=1 THEN _
        LET INVENTORY$(ITEMNUMBER)="EMPTY" ELSE PRINT "WEIRD":EXIT SELECT
      PRINT "The huge SURFBOARD makes a nice BRIDGE and you can now GO SOUTH.":_
        LET FLAG(2)=1:EXIT SELECT
            End If
            If ROOM = 5 Then
      THING$="DRIFTWOOD":IF FNCARRY=0 AND FLAG(7)=0 THEN _
        PRINT "REVEAL object on BEACH first.":EXIT SELECT
                If FNCARRY = 0 Then Print "GET the DRIFTWOOD.": Exit Select
      THING$="LINE":IF FNCARRY=0 THEN PRINT _
       "GET some LINE first.":EXIT SELECT
      THING$="KNIFE":IF FNCARRY=0 THEN PRINT _
       "Andrea says you'll need a KNIFE.":EXIT SELECT
                THING$ = "DRIFTWOOD": Let INVENTORY$(ITEMNUMBER) = "EMPTY"
                THING$ = "LINE": Let INVENTORY$(ITEMNUMBER) = "EMPTY"
                THING$ = "KNIFE": Let INVENTORY$(ITEMNUMBER) = "SAWBUCK"
                Print "The owner quickly sold your excellent MOBILE, giving you"
                Print "a sawbuck ($10).  Is there no end to your talent?  Don't speak."
            End If
            Exit Select
        Case "OFFER"
            If ROOM <> 25 Then Print "Not here": Exit Select
            If FLAG(12) = 1 Then Print "Not again": Exit Select
            If OBJECT$ <> "ORANGE" Then Print "Not that.": Exit Select
            THING$ = "ORANGE": If FNCARRY = 0 Then Print "You don't have it.": Exit Select
            Let FLAG(12) = 1
            Let ROOMOBJECT$(ROOM, 1) = "EMPTY"
            Let INVENTORY$(ITEMNUMBER) = "EMPTY"
            Print "The GUARD* wanders off eating the ORANGE without saying thanks."
            Exit Select
        Case "TRY"
            If ROOM <> 15 Then Print "Not here": Exit Select
            If FLAG(13) = 0 Then Print "No need": Exit Select
            Let COMBO$ = "DUDDU"
    IF (LEN(OBJECT$)<>5)OR (LEFT$(OBJECT$,1)<>"U" AND LEFT$(OBJECT$,1)<>"D")_
      THEN PRINT "Try - TRY UDUDU or some such.":EXIT SELECT
            TRYNUMB = TRYNUMB + 1
            If COMBO$ = OBJECT$ Then
                Let FLAG(13) = 0
                Print "The door swings gently open.  Andrea says --well done": Exit Select
            End If
            HITS = 0
            For I = 1 To 5
                Let DUMMY$ = Mid$(OBJECT$, I, 1)
                If DUMMY$ = Mid$(COMBO$, I, 1) Then HITS = HITS + 1
            Next I
            Print "You hear the lock mechanism whir and click "; HITS; " times."
            Exit Select
        Case "KILL", "MURDER", "RAPE", "DESTROY", "ASSAULT"
            Print "Your mom is right - you watch entirely too much tv."
            Exit Select

        Case Else
            Print "Andrea says to try another verb.": Exit Select
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
    Shared FLAG(), TURNNUMBER, ANSWER$(), STRT, GUESSNUMB, LOGIC$, TRYNUMB, SLAMMER
    Print: Print
    If FLAG(1) = 1 Then Print " *****   HOORAY FOR YOU!!  YOU'VE WON THE GAME!!  ****"
    Print
    For I = 1 To 4: Print: Next I
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

    '    Let DTA(12) = FLAG(1): Let DTA(37) = DTA(37) + Int((Timer - STRT) / 6)
    '    Open "REPORT.DTA" For Output As #1
    '    Rem SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
    '    For I = 1 To 40
    '        Write #1, DTA(I)
    '    Next I
    '    For I = 1 To 10
    '        Write #1, DTA$(I)
    '    Next I
    '    Close #1

    Open "BOARDATA.TXT" For Append As #2
    Print #2, Time$, GUESSNUMB, LOGIC$
    Print #2, TRYNUMB, SLAMMER, ANSWER$(0)
    For I = 1 To TURNNUMBER
        If Int(I / 5) = I / 5 Then Print #2,
        Print #2, ANSWER$(I),
    Next I
    Print #2, Int((STRT - Timer) / 6), Int(1000 * Rnd(0))
    Close #2
    Print "This game is over, type BOARDWALK  to play again."
End Sub

'END OF PROGRAM

