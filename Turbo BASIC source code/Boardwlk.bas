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

    CLS:DIM FLAG(30),OBJ$(30),OBJ2$(30),CLUE$(30),WURD$(30)
    STRT=TIMER
    CALL LOGON
    LOCATE 12,12:PRINT "ONE MOMENT PLEASE...."
    CALL SETUP              'MODULE 1 SET UP VARIABLES
    CALL TURN               'MODULE 2 RUN TURNS
    CALL CLOSING            'MODULE 3 END OF GAME STUFF
END

'MODULE 0.1 LOGON
SUB LOGON
   SHARED ANSWER$()
   DIM ANSWER$(205)
   INPUT "YOUR NAME-NUMBER PLEASE";ANSWER$(0)
   LET ANSWER$(0)=ANSWER$(0)+"  "+TIME$+"  "+DATE$
END SUB

'MODULE 1 SETUP
SUB SETUP
    CALL ROOMINFO           'MODULE 1.1 READ ROOM INFORMATION
    CALL ROOMOBJECTS        'MODULE 1.2 READ ROOM OBJECTS
    CALL MOVEMENTTABLE      'MODULE 1.3 READ MOVEMENT TABLE
    CALL INVENTORY          'MODULE 1.4 INITIALIZE INVENTORY ARRAY
                            '             TO AN EMPTY LIST
    CALL OTHERS             'MODULE 1.5 INITIALIZE OTHER VARIABLES
END SUB

'MODULE 1.1 ROOMINFO
SUB ROOMINFO
    SHARED DESCRIPTION$(),NUMBERROOMS,DESCRIBEFLAG(),ROOMDESCRIBE$(),ROOMDES2$()
    READ NUMBERROOMS
    DIM DESCRIPTION$(NUMBERROOMS),DESCRIBEFLAG(NUMBERROOMS),ROOMDESCRIBE$(NUMBERROOMS)
    DIM ROOMDES2$(NUMBERROOMS)
    FOR I = 1 TO NUMBERROOMS
        READ DESCRIPTION$(I)
    NEXT I
    DATA 33,BALLROOM,DRESS SHOP,JEWELRY STORE,BASKETBALL SHOOT,CRAFTS CENTER
    DATA SHOOTING GALLERY,ROLLER-COASTER,BOARDWALK,BOARDWALK,BOARDWALK
    DATA BOARDWALK,BOARDWALK,BOARDWALK,BOARDWALK,STORAGE ROOM
    DATA END OF BEACH,BEACH,WHARF,BEACH,BEACH
    DATA BEACH,TIDEPOOL,SURF,SURF,WHARF
    DATA SURF,SURF,SURF,WHARF,FISH MARKET
    DATA OCEAN FLOOR,END OF WHARF,SEAL ROCK

    FOR I=1 TO NUMBERROOMS
       LET DESCRIBEFLAG(I)=0
       READ ROOMDESCRIBE$(I),ROOMDES2$(I)
    NEXT I
 REM ROOM 1
    DATA "The music here in the BALLROOM, though melancholy, suggests that"
    DATA "love is in the air.  Get cracking, you matchmaker, you."

    DATA "Here in the DRESS SHOP is a beautiful ballroom GOWN that just been"
    DATA "marked down to $115 ---WOW!  Too bad you don't have that much, yet."

    DATA "The JEWELER* here in the JEWELRY STORE is eyeing you very closely."
    DATA "Andrea's getting jumpy, keeps shouting 'I'm not a thief!'"

    DATA "Here at the BASKETBALL SHOOT, you can SHOOT a BASKET  and win a "
    DATA "stuffed ANIMAL.  Cool!  Except that you hate stuffed animals."

    DATA "It's funny that you haven't notice some sort of SIGN*  here at "
    DATA "the CRAFTS CENTER.  Perhaps you should return later."
REM 6
    DATA "Here at the SHOOTING GALLERY is an unhappy SAILOR*.  He's trying"
    DATA "to win a stuffed ANIMAL for his date but can't SHOOT straight."

    DATA "Here at the ROLLER-COASTER, you can't tell who looks the most "
    DATA "pale - the people getting on or the people getting off."

    DATA "Here the BOARDWALK dead ends against the sheer wall of the sandstone"
    DATA "cliff.  "

    DATA "Lots of splinters in this section of the BOARDWALK  "
    DATA "  "

    DATA "Excuse me!  Isn't there a dress code on this BOARDWALK?  "
    DATA "  "
 REM 11

    DATA "A badly burned, portly gentleman just passed by on this section  "
    DATA "of the BOARDWALK.  Doesn't he teach at Sac State?  "

    DATA "On this section of the BOARDWALK, you can still hear the screams "
    DATA "coming from the folks enjoying the ROLLER-COASTER*  "

    DATA "All along the BOARDWALK are steps leading down to the BEACH  "
    DATA "  "

    DATA "This end of the BOARDWALK ends outside a STORAGE ROOM"
    DATA "  "

    DATA "The LOCKER in the STORAGE ROOM is probably full of good stuff,"
    DATA "too bad it's locked.  Andrea thinks you should find the key.  Duh!"
REM 16
    DATA "Here the BEACH dead ends against the cliff wall.  READ the SIGN*"
    DATA "and REVEAL something that you'll need."

    DATA "This is the section of BEACH, next to the WHARF that has the best"
    DATA "surf in the area.  The pilings are too close to 'shoot the pier.'"

    DATA "This is the beginning of the world-famous WHARF.  You wonder what"
    DATA "keeps the rotten-looking wooden structure up."

    DATA "An attractive, but poorly dressed WOMAN* sits here on the BEACH"
    DATA "gazing disconsolately out to sea.  Is she swaying?"

    DATA "As you enter this section of the BEACH you overhear someone "
    DATA "talking about a fraternity party.  Something about soap suds."
REM 21
    DATA "As you enter this section of the BEACH the STORAGE ROOM guy tells"
    DATA "Andrea that he has a hole in his pocket.  Andrea says, 'oh.'"

    DATA "At the end of the beach is a terrific TIDEPOOL, which is uncovered"
    DATA "right now revealing a STARFISH*.  He looks crabby."

    DATA "The SURF here near the cliff is too dangerous to surf."
    DATA "  "

   DATA "The waves here near the WHARF curl very nicely.  Ouch, some dude"
   DATA "just took a header off the end of his board. Ah, the taste of sand."

    DATA "A wharf GUARD* is taking things from people before letting them"
    DATA "GO SOUTH along the wharf.  Isn't that illegal?"


REM 26
    DATA "The SURF here is quite cold. "
    DATA "  "

    DATA "Andrea is complaining about the salt water of the SURF here  "
    DATA "  "

    DATA "The waves are smaller and less dangerous in this part of  "
    DATA "the surf "

    DATA "This middle section of WHARF is opposite the FISH MARKET."
    DATA "   "

    DATA "A somewhat downcast FISHMONGER* dominates the scene at the"
    DATA "FISH MARKET -- unless you count the smell."
 REM 31
    DATA "The water here on the OCEAN FLOOR is quite clear and pleasant."
    DATA " You see something that a STARFISH* could go for."

    DATA "Here at the end of the WHARF you can see SEAL ROCK to the south,"
    DATA "but you can't GO there until you MAKE a BRIDGE.  Is that a pelican?"

    DATA "Here on SEAL ROCK you see a most forlorn PELICAN* who is hope-"
    DATA "lessly tangled in some fishing LINE.  UNTANGLE him, please."

END SUB

'MODULE 1.2 ROOM OBJECT INFORMATION
SUB ROOMOBJECTS
    SHARED ROOMOBJECT$(),NUMBERROOMS
    DIM ROOMOBJECT$(NUMBERROOMS,15)
        FOR I=1 TO NUMBERROOMS
            FOR J=1 TO 15
                LET ROOMOBJECT$(I,J)="EMPTY"
            NEXT J
        NEXT I
END SUB

'MODULE 1.3 MOVEMENTTABLE
SUB MOVEMENTTABLE
    SHARED MOVEMENTTABLE(),NUMBERROOMS
    DIM MOVEMENTTABLE(NUMBERROOMS,6)
    FOR I=1 TO NUMBERROOMS
        FOR J = 1 TO 6
           READ MOVEMENTTABLE(I,J)
        NEXT J
    NEXT I
    DATA 0,8,0,0,0,0
    DATA 0,9,0,0,0,0
    DATA 0,10,0,0,0,0
    DATA 0,11,0,0,0,0
    DATA 0,12,0,0,0,0

    DATA 0,13,0,0,0,0
    DATA 0,14,0,0,0,0
    DATA 1,0,9,0,0,16
    DATA 2,0,10,8,1,17
    DATA 3,0,11,9,0,18

    DATA 4,0,12,10,0,19
    DATA 5,0,13,11,0,20
    DATA 6,0,14,12,0,21
    DATA 7,0,15,13,0,0
    DATA 0,0,0,14,0,0

    DATA 0,23,17,0,8,0
    DATA 0,24,18,16,9,0
    DATA 0,25,19,17,10,0
    DATA 0,26,20,18,11,0
    DATA 0,27,21,19,12,0

    DATA 0,28,22,20,13,0
    DATA 0,0,0,21,0,0
    DATA 16,0,0,0,0,0
    DATA 17,0,0,0,0,0
    DATA 18,29,0,0,0,0

    DATA 19,0,0,0,0,0
    DATA 20,0,0,0,0,0
    DATA 21,0,0,0,0,0
    DATA 25,32,30,0,0,0
    DATA 0,0,0,29,0,0

    DATA 0,0,0,0,29,0
    DATA 29,33,0,0,0,0
    DATA 32,0,0,0,0,0
END SUB

'MODULE 1.4 INVENTORY
SUB INVENTORY
    SHARED INVENTORY$()
    DIM INVENTORY$(5)
    FOR I=1 TO 5
        LET INVENTORY$(I)="EMPTY"
    NEXT I
    LET INVENTORY$(5)="SURFBOARD"
END SUB

'MODULE 1.5 OTHERS
SUB OTHERS
    SHARED GAMESTATE$,ROOM,TURNNUMBER,ROOMOBJECT$(),THING$,INVENTORY$(),_
        FLAG(),OBJ$(),OBJ2$(),DESCRIBEFLAG(),ROOMDESCRIBE$(),ROOMDES2$(),_
        GUESSNUMB,CLUE$(),WURD$(),NUMBERROOMS,LOGIC$,SLAMMER,TRYNUMB,TOTAL
    LET TURNNUMBER =0
    LET THING$=""
    FOR I=1 TO 30:LET FLAG(I)=0:NEXT I      'SET FLAGS TO 0
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
        LET FLAG(18)=1:REM NOT FIRST TO BE GUESSED
        LET FLAG(29)=1:REM NOT THIS ONE EITHER

    'SET UP WORD GUESSING DATA
     FOR I=1 TO NUMBERROOMS
       LET CLUE$(I)="":LET WURD$(I)=""
     NEXT I
     LET CLUE$(18)="OLYMPIC"
     LET WURD$(18)="MEDAL"
     LET CLUE$(20)="BEER"
     LET WURD$(20)="CANS"
     LET CLUE$(29)="CLAM"
     LET WURD$(29)="OYSTER"
    GUESSNUMB=0:'TOTAL NUMBER OF GUESSES AT WORDS
    TRYNUMB=0:'TOTAL # OF TRIES AT COMBO DOOR LOCK
    SLAMMER=0:'TOTAL # OF TRIES AT FINAL SOLUTION MATRIX
    LOGIC$="Logfal - "
    TOTAL=0

    FOR I=1 TO 39
      READ ROOM,J,ROOMOBJECT$(ROOM,J)
    NEXT I
    DATA 1,1,SIGN*,1,2,EMPTYMAN*,2,1,EMPTYGOWN
    DATA 3,1,JEWELER*,4,1,EMPTYBASKETBALL,5,1,EMPTYSIGN*
    DATA 6,1,SAILOR*,13,1,EMPTYSIGN*,13,2,EMPTYVENDING-MACHINE
    DATA 14,1,RECYCLER*,15,1,LOCKER*,15,2,EMPTYSCUBA
    DATA 15,3,EMPTYWETSUIT,15,4,EMPTYFIN,15,5,EMPTYSAWBUCK

    DATA 15,6,EMPTYC-NOTE,16,1,SIGN*,16,2,EMPTYDRIFTWOOD
    DATA 19,1,WOMAN*,20,1,EMPTYCANS,22,1,STARFISH*
    DATA 22,2,EMPTYSHELL*,29,1,EMPTYLADDER*,30,1,SIGN*
    DATA 30,2,FISHMONGER*,30,3,EMPTYFISH*,31,1,OYSTER
    DATA 31,2,LADDER*,33,1,PELICAN*,33,2,EMPTYFISH

    DATA 7,1,SIGN*,32,1,EMPTYBRIDGE*,33,3,BRIDGE*
    DATA 9,1,SIGN*,10,1,SIGN*,10,2,EMPTYORANGE
    DATA 25,1,GUARD*,15,7,EMPTYSIGN*,15,8,EMPTYKEY*

 'THE STARTING ROOM IS THE ENTRYWAY - 7 ROLLER COASTER
    ROOM=7


    FOR I=1 TO 17                       'READ IN WORD!* CONVERSION
        READ OBJ$(I),OBJ2$(I)
    NEXT I
    DATA SIGN,SIGN*,MAN,MAN*,JEWELER,JEWELER*
    DATA BALL,BASKETBALL,SAILOR,SAILOR*,MACHINE,VENDING-MACHINE*
    DATA RECYCLER,RECYCLER*,LOCKER,LOCKER*,WOOD,DRIFTWOOD
    DATA WOMAN,WOMAN*,CAN,CANS,STARFISH,STARFISH*
    DATA LADDER,LADDER*,FISHMONGER,FISHMONGER*,PELICAN,PELICAN*

    DATA GUARD,GUARD*,BOARD,SURFBOARD

END SUB

'MODULE 2 TURN
SUB TURN
    SHARED FLAG(),TURNNUMBER
    DO UNTIL FLAG(1) <> 0
        FLAG(14)=0
        TURNNUMBER = TURNNUMBER + 1
        IF TURNNUMBER=295 THEN BEEP 3:_
           PRINT "Type   SAVE GAME  and then  QUIT":_
           PRINT "You can then return to this spot by typing":_
           PRINT "BOARDWALK (at the DOS prompt) and later,":_
           PRINT "as a command, RESTORE GAME":_
           PRINT "Please forgive the necessary inconvenience."
        IF TURNNUMBER=300 THEN FLAG(1)=2
        CALL DESCRIBE      'MODULE 2.1 DESCRIBE THE ROOM
        CALL COMMANDS      'MODULE 2.2 INPUT THE COMMANDS
        CALL EVALUATE      'MODULE 2.3 EVALUATE COMMANDS
    LOOP
END SUB

'MODULE 2.1 DESCRIBE THE CURRENT ROOM
SUB DESCRIBE
    SHARED DESCRIPTION$(),ROOM,MOVEMENTTABLE(),ROOMOBJECT$(),FLAG(),_
        TURNNUMBER,INVENTORY$(),THING$,OBJECT$,DESCRIBEFLAG(),ROOMDESCRIBE$(),_
        ROOMDES2$(),CLUE$(),WURD$(),GUESSNUMB,TRYNUMB,LOGIC$,ITEMNUMBER
    PRINT:PRINT
    IF TURNNUMBER=1 THEN CALL GAMESTARTER   'MODULE 2.1.1

    IF DESCRIBEFLAG(ROOM)=1 THEN PRINT "You are at the  ";DESCRIPTION$(ROOM)
    IF DESCRIBEFLAG(ROOM)=0 THEN__
     PRINT ROOMDESCRIBE$(ROOM):PRINT ROOMDES2$(ROOM):DESCRIBEFLAG(ROOM)=1
    DELAY 0.3
    PRINT "The noticeable exits are: ";
    IF MOVEMENTTABLE(ROOM,1)<>0 THEN PRINT "NORTH ";
    IF MOVEMENTTABLE(ROOM,2)<>0 THEN PRINT "SOUTH ";
    IF MOVEMENTTABLE(ROOM,3)<>0 THEN PRINT "EAST ";
    IF MOVEMENTTABLE(ROOM,4)<>0 THEN PRINT "WEST ";
    IF MOVEMENTTABLE(ROOM,5)<>0 THEN PRINT "UP ";
    IF MOVEMENTTABLE(ROOM,6)<>0 THEN PRINT "DOWN ";
    PRINT
    DELAY 0.3
    PRINT "The noticeable objects are: ";
    COUNTER=0
    FOR I=1 TO 15
        LET STUFF$= ROOMOBJECT$(ROOM,I)
        IF LEFT$(STUFF$,5)<>"EMPTY" THEN PRINT "   ";ROOMOBJECT$(ROOM,I);_
          :COUNTER=COUNTER+1
    NEXT I
    IF COUNTER=0 THEN PRINT "noticeably absent!";
    PRINT
    DELAY 0.3
    PRINT "You are carrying: ";
            COUNTER=0
            FOR I= 1 TO 5
                IF INVENTORY$(I)<>"EMPTY" THEN COUNTER=COUNTER + 1:_
                    PRINT INVENTORY$(I);"   ";
            NEXT I
            IF COUNTER = 0 THEN PRINT "nothing at all."
     PRINT
     DELAY 0.3
     IF ROOM=18 OR ROOM=20 OR ROOM=29 THEN DELAY 0.3
     IF (ROOM=18 OR ROOM=20 OR ROOM=29) AND FLAG(ROOM)=0 THEN PRINT _
       "Andrea says that she's thinking of a ";LEN(WURD$(ROOM));_
        "-letter word":PRINT _
       "that has something to do with ";CLUE$(ROOM);".":DELAY 0.3:PRINT:PRINT _
       "If you would like to guess what the word is, type GUESS followed ":_
       PRINT "by your guess, for example,  GUESS COW ":DELAY 0.3

     THING$="ORANGE"
     IF ROOM=25 AND FNCARRY=1 THEN _
       LET INVENTORY$(ITEMNUMBER)="EMPTY":PRINT:FLAG(12)=1:_
       LET ROOMOBJECT$(ROOM,1)="EMPTY":FLAG(14)=1: PRINT _
       "The GUARD* takes the ORANGE and wanders off.  Creep."


     THING$="CANS"
     IF ROOM=14 AND FNCARRY=1 THEN _
       LET INVENTORY$(ITEMNUMBER)="BUCK":PRINT:PRINT _
       "The RECYCLER* glommed on to the CANS and slipped you a BUCK.  Cool!":_
       PRINT:LET ROOMOBJECT$(ROOM,1)="EMPTY":_
       LET FLAG(14)=1:REM EXITS SUB

     THING$="DETECTOR"
     IF ROOM=21 AND FNCARRY=1 THEN
       LET INVENTORY$(ITEMNUMBER)="KEY":PRINT:PRINT _
       "You detected and picked up a KEY.  Promising!":PRINT:PRINT _
       "Just then the rental guy came by and took back the overdue":PRINT _
       "DETECTOR.  Andrea says she doesn't remember anything about":PRINT _
       "a time limit.  Da noive a da guy!":LET FLAG(14)=1
     END IF


     THING$="OYSTER"
     IF ROOM=22 AND FNCARRY=1 THEN
       LET INVENTORY$(ITEMNUMBER)="PEARL":PRINT
       PRINT"You slip momentarily in the TIDEPOOL, dropping the OYSTER into the"
       PRINT "water.  In a flash, the STARFISH has it pried open and consumed,"
       PRINT "(that was disgusting), leaving you with only the PEARL.  Yes!"
       LET FLAG(14)=1
     END IF

     THING$="PEARL"
     IF ROOM=3 AND FNCARRY=1 THEN
       LET INVENTORY$(ITEMNUMBER)="C-NOTE"
       PRINT "The JEWELER's on you like cheese on a burger when he sees the"
       PRINT "PEARL.  Before you can react, he's taken the PEARL and given "
       PRINT "you $100.  Andrea asks if it's dress time.":LET FLAG(14)=1
     END IF

     THING$="GOWN"
     IF ROOM=19 AND FNCARRY=1 THEN
       FLAG(1)=1:INVENTORY$(ITEMNUMBER)="EMPTY"
       PRINT
       PRINT "The WOMAN*'s face lights up when she sees the GOWN that "
       PRINT "you and Andrea hand her.  After thanking you profusely,"
       PRINT "she sprints off in the direction of the changing room."
       Delay 5.5
       PRINT
       PRINT "#################################################"
       PRINT:PRINT
       PRINT "You follow the her to the BALLROOM in time to see the"
       PRINT "happy couple dancing the waltz.  Whatever the waltz is."
       PRINT "The MAN* turns out to be ex-surfing champ Blackjack Brown,"
       PRINT "who tosses you the wax you need to surf your best.  Dude."
       EXIT SUB
     END IF

     IF FLAG(11)=0 THEN PRINT _
       "Andrea has the hiccups -- wants to tell you something - but can't!"
  PRINT

END SUB

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
SUB GAMESTARTER
     CLS
     PRINT "              WELCOME TO THE BOARDWALK!"
     PRINT
     PRINT "  Okay, here's the deal:  Your seriously bitchin' poem, "
     PRINT "'Surf's up, dude!' won the Surfer's Open Poetry Contest and"
     PRINT "now you have the first prize: King Kamehameha's original,"
     PRINT "humungously long, wooden surfboard.  Excellant!  Unfortunately,"
     PRINT "you can't try it out -- no wax!  Bummer!  "
     PRINT "  You'll just have to wander around the BOARDWALK doing good"
     PRINT "until virtue is rewarded and you happen upon some wax.  Hurry"
     PRINT "though -- that storm out in the Pacific has stirred up some"
     PRINT "kicker waves.  Tubular!"
     PRINT
     PRINT "    Give COMMANDs as VERB then OBJECT, Such as GO NORTH,"
     PRINT "READ SIGN, RIDE ROLLER-COASTER*, CLIMB LADDER*, SHOOT BASKET,"
     PRINT "UNTANGLE PELICAN*, GET SCUBA (Self-Contained Underwater Breathing"
     PRINT "Apparatus - if you must know), PAY SAWBUCK ($10), PAY FIN ($5)"
     PRINT "PAY C-NOTE ($100), MAKE BRIDGE, SWAP FISH, DEPOSIT BUCK ($1), etc."
     PRINT "    Exceptions to this two-word sentence rule are single-"
     PRINT "letter commands such as N to GO NORTH, and U to GO UP."
     PRINT "    P.S. Don't try to get objects ending in an *, e.g., SAFE*,"
     PRINT "as they are quite unobtainable.  "
     PRINT
     PRINT "If you're through taking notes, press the Enter key to begin"
     DO WHILE LEN(INKEY$)=0:LOOP
     CLS
END SUB

'MODULE 2.1.2 ERASER kept as dummy in case it's referenced somewhere
SUB ERASER
REM
END SUB
 'MODULE 2.1.3 MESSAGE  AFTER WORD GAME IS COMPLETED
SUB MESSAGE
  SHARED ROOM,ROOMOBJECT$(),FLAG()
  IF ROOM= 20 THEN
    LET ROOMOBJECT$(ROOM,1)="CANS"
    LET FLAG(18)=0:REM ENABLES YOU TO GET MEDAL CLUE
    PRINT "Andrea asks if you noticed how many CANS people strew about.  Strew?"
  END IF
  IF ROOM=18 THEN
    LET ROOMOBJECT$(13,1)="SIGN*"
    LET ROOMOBJECT$(13,2)="VENDING-MACHINE*"
    LET FLAG(29)=0:REM ENABLES YOU TO GET OYSTER CLUE
    PRINT "Andrea suggests you DEPOSIT the BUCK at the VENDING-MACHINE*"
    PRINT "and rent a metal DETECTOR"
  END IF
  IF ROOM=29 THEN
    LET ROOMOBJECT$(29,1)="LADDER*"
    PRINT "Andrea suggests you CLIMB LADDER* down to the OYSTER."
  END IF
END SUB

'MODULE 2.2 COMMANDS
SUB COMMANDS
    SHARED VERB$, OBJECT$,ROOM,DESCRIPTION$(),FLAG(),INVENTORY$(),_
        ROOMOBJECT$(),OBJ$(),OBJ2$(),C$,ANSWER$(),TURNNUMBER,_
        CLUE$(),WURD$(),GUESSNUMB,TOTAL

    IF FLAG(1)<>0 THEN EXIT SUB
    IF FLAG(14)=1 THEN EXIT SUB
    ' BE SURE THAT INPUT IS OK  DUMMY2=0
    DUMMY2=0
   DO UNTIL DUMMY2=1
    'C$=THE INPUTTED COMMAND SENTENCE - MAY BE MORE THAN 2 WORDS
    'BUT ONLY THE FIRST TWO WORDS ARE ACCEPTED
    ' THE FIRST IS CALLED VERB$ AND THE SECOND OBJECT$
    LET C$=""
    PRINT:PRINT
    PRINT "*****************************************************************"
    DO UNTIL C$<>""
        INPUT "COMMAND";C$                 'GET THE RAW SENTENCE
    LOOP

    REM SPACES TAKE OFF LEADING AND PERHAPS TRAINING CHR$(32)'S
    DO WHILE RIGHT$(C$,1)=CHR$(32)
      LET C$=LEFT$(C$,LEN(C$)-1)
    LOOP

    LET DUMMY$=""                          'CONVERT TO UPPERCASE
    FOR I=1 TO LEN (C$)
    IF ASC(MID$(C$,I,1))>96 AND ASC(MID$(C$,I,1))<123 THEN_
        LET DUMMY$=DUMMY$+CHR$(ASC(MID$(C$,I,1))-32)_
        ELSE LET DUMMY$=DUMMY$+MID$(C$,I,1)
    NEXT I
    LET C$=DUMMY$

    LET I=1                                'LETTER COUNTER

    DO WHILE NOT (ASC (MID$(C$,I,1)) = 32) AND NOT (I= LEN(C$))
                                           'LOOP TO LOOK FOR FIRST SPACE
        LET I = I + 1                      'WHICH SHOULD DENOTE END OF FIRST
    LOOP                                   'WORD

    LET VERB$=MID$(C$,1,I)                 'ASSIGN THOSE CHARACTERS TO VERB$

LET ANSWER$(TURNNUMBER)=C$

    LET OBJECT$ = MID$(C$,I+1,LEN(C$))     'ASSIGN REST OF SENTENCE TO OBJECT$


    IF LEN(OBJECT$)<>0 THEN VERB$=LEFT$(VERB$,LEN(VERB$)-1)
    FOR J=1 TO 20                          'CONVERT OBJECT$ LACKING * OR !
        IF OBJECT$=OBJ$(J) THEN LET OBJECT$=OBJ2$(J)
    NEXT J
    'CHECK FOR EXTRA WORDS
    COUNTER=0
    FOR J=1 TO LEN(OBJECT$)
      LET DUMMY$=MID$(OBJECT$,J,1)
      IF DUMMY$=CHR$(32) THEN COUNTER=COUNTER+1
    NEXT J
    IF COUNTER<>0 THEN PRINT "Two words only, please." _
      ELSE DUMMY2=1

 LOOP
END SUB

'MODULE 2.3 EVALUATE THE COMMANDS
SUB EVALUATE
    SHARED VERB$, OBJECT$,ROOM,DIRECTION,MOVEMENTTABLE(),NUMBERROOMS,_
        ROOMOBJECT$(),ITEMNUMBER,TURNNUMBER,THING$,INVENTORY$(),FLAG(),_
        GUESSNUMB,CLUE$(),WURD$(),LOGIC$,TRYNUMB,SLAMMER,TOTAL
    IF FLAG(1)<>0 THEN EXIT SUB
    IF FLAG(14)=1 THEN EXIT SUB
    SELECT CASE VERB$
        CASE "QUIT","Q"
            LET FLAG(1)=2   'GAME NOT WON(1) OR LOST(-1):EXIT SELECT
        CASE "SAVE"
            IF OBJECT$<>"GAME" THEN PRINT "SAVE GAME":EXIT SELECT
            OPEN "BOARDSAV.BAS" FOR OUTPUT AS #1
            WRITE #1,ROOM


            FOR I=1 TO 5
                LET DUMMY$=""
                FOR J=1 TO LEN(INVENTORY$(I))
                 LET DUMMY$=DUMMY$+CHR$(ASC(MID$(INVENTORY$(I),J,1))+4)
                NEXT J
                WRITE #1,DUMMY$
            NEXT I
            FOR I=1 TO NUMBERROOMS
               FOR J=1 TO 15
                LET DUMMY$=""
                FOR K=1 TO LEN (ROOMOBJECT$(I,J))
                 LET DUMMY$=DUMMY$+CHR$(ASC(MID$(ROOMOBJECT$(I,J),K,1))+4)
                NEXT K
                WRITE #1,DUMMY$
               NEXT J
            NEXT I
            FOR I=1 TO 30
                WRITE  #1,FLAG(I)
            NEXT I
            WRITE #1,GUESSNUMB,LOGIC$,SLAMMER,TRYNUMB,TOTAL
            CLOSE #1
            PRINT "OK
            EXIT SELECT
        CASE "RESTORE"
            IF OBJECT$<> "GAME" THEN PRINT "Try  RESTORE GAME":EXIT SELECT
            OPEN "BOARDSAV.BAS" FOR INPUT AS #1
            INPUT #1,ROOM


            FOR I=1 TO 5
              INPUT #1,DUMMY$
                LET INVENTORY$(I)=""
                FOR J=1 TO LEN(DUMMY$)
                 LET INVENTORY$(I)=INVENTORY$(I)+CHR$(ASC(MID$(DUMMY$,J,1))-4)
                NEXT J
            NEXT I
            FOR I=1 TO NUMBERROOMS
               FOR J=1 TO 15
                INPUT #1,DUMMY$
                LET ROOMOBJECT$(I,J)=""
                FOR K=1 TO LEN (DUMMY$)
                 LET ROOMOBJECT$(I,J)=ROOMOBJECT$(I,J)+CHR$(ASC(MID$(DUMMY$,K,1))-4)
                NEXT K
               NEXT J
            NEXT I


            FOR I=1 TO 30
                INPUT  #1,FLAG(I)
            NEXT I
            INPUT #1,GUESSNUMB,LOGIC$,SLAMMER,TRYNUMB,TOTAL
            CLOSE #1
            PRINT "OK"
            EXIT SELECT

        CASE "INVENTORY","I"
            COUNTER=0
            PRINT "   YOU CARRY: ";
            FOR I= 1 TO 5
                IF INVENTORY$(I)<>"EMPTY" THEN COUNTER=COUNTER + 1:_
                    PRINT INVENTORY$(I);"   ";
            NEXT I
            IF COUNTER = 0 THEN PRINT "NOTHING AT ALL" :EXIT SELECT
            PRINT:EXIT SELECT

        CASE "GO","MOVE","N","S","E","W","U","D"
            IF LEN (VERB$)=1 THEN OBJECT$=VERB$
            IF OBJECT$="NORTH" OR OBJECT$="N" THEN DIRECTION = 1
            IF OBJECT$="SOUTH" OR OBJECT$="S" THEN DIRECTION = 2
            IF OBJECT$="EAST"  OR OBJECT$="E" THEN DIRECTION = 3
            IF OBJECT$="WEST"  OR OBJECT$="W" THEN DIRECTION = 4
            IF OBJECT$="UP"    OR OBJECT$="U" THEN DIRECTION = 5
            IF OBJECT$="DOWN"  OR OBJECT$="D" THEN DIRECTION = 6
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
            EXIT SELECT

        CASE "GET","TAKE"
            IF OBJECT$="SURFBOARD" THEN PRINT _
              "Leave it here until you get some wax":EXIT SELECT

            IF  FNPRESENT = 0 THEN PRINT "I DON'T SEE  ";OBJECT$:EXIT SELECT
            IF RIGHT$(OBJECT$,1)="*" THEN PRINT "I CAN'T HANDLE IT":EXIT SELECT

            FOR I=1 TO 5
                IF INVENTORY$(I)="EMPTY" THEN INVENTORY$(I)=OBJECT$:_
                    PRINT "GOT IT":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY":_
                    EXIT SELECT
            NEXT I

            PRINT "You're carrying too much.  Drop something first."
            EXIT SELECT
        CASE "PUT","DROP","GIVE"
            IF OBJECT$="SURFBOARD" THEN PRINT_
             "Andrea thinks you'll need it.":EXIT SELECT
            IF OBJECT$="FISH" THEN PRINT "Try - SWAP FISH":EXIT SELECT
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
            THING$=OBJECT$
            IF FNCARRY=0 THEN PRINT "You don't have the ";OBJECT$:EXIT SELECT


            FOR J=1 TO 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":_
                    EXIT SELECT
            NEXT J
            PRINT "This room is full; take it elsewhere":EXIT SELECT

        CASE "READ"
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
           IF ROOM=15 THEN
             PRINT "It says: The door lock is a 5 switch (Up or Down) combination"
             PRINT "To open, try different combinations of U and D: e.g., "
             PRINT "TRY UDUDU or TRY DDDUD.  Then, listen closely to hear the"
             PRINT "number of clicks coming from the lock mechanism.  Five "
             PRINT "clicks and the door will swing open.  Good luck."
             EXIT SELECT
           END IF

            PRINT "Andrea says that we're in the wrong room for that."
            EXIT SELECT

       CASE ",DRINK","IMBIBE"
          PRINT "Andrea says that you are not thirsty.":EXIT SELECT
       CASE "GUESS"
        IF ROOM<>18 AND ROOM<>20 AND ROOM<>29 THEN PRINT _
         "Andrea says that this isn't the place for that.":EXIT SELECT
        IF FLAG(ROOM)=1 THEN PRINT _
         "Andrea says that you're done with this one.":EXIT SELECT
        IF LEN(OBJECT$)<>LEN(WURD$(ROOM)) THEN PRINT _
          "Andreas says that she's thinking of a ";LEN(WURD$(ROOM));_
          "-letter word.  Try again.":EXIT SELECT
        GUESSNUMB=GUESSNUMB+1
        IF OBJECT$=WURD$(ROOM) THEN FLAG(ROOM)=1:_
          CALL MESSAGE:EXIT SELECT
        HITS=0
        FOR I=1 TO LEN(OBJECT$)
         LET DUMMY$=MID$(OBJECT$,I,1)
         FOR J=1 TO LEN(WURD$(ROOM))
          IF DUMMY$=MID$(WURD$(ROOM),J,1) THEN _
           HITS=HITS+1:EXIT FOR
         NEXT J
        NEXT I
        PRINT "Andrea says that ";HITS;" letter(s) of your word ":PRINT _
              "are also found in her word.  Try again."
         EXIT SELECT
       CASE "PLAY"
         PRINT "Andrea warns that we have no time for such foolishness."
         EXIT SELECT
       CASE "OPEN"
         IF ROOM=15 THEN PRINT "Try  --- UNLOCK LOCKER":EXIT SELECT
         PRINT "Doesn't work. ":EXIT SELECT
       CASE "CLOSE"
        PRINT "Andrea thinks you should leave it open.":EXIT SELECT
       CASE "BREAK"
         PRINT "Andrea is sulking.  She hates violence."
         EXIT SELECT
       CASE "DIG"
         PRINT "It's hopeless.  This is a job for a metal DETECTOR."
         EXIT SELECT
       CASE "USE"
         PRINT "To do what?  Be more specific."
         EXIT SELECT
       CASE "UNLOCK"
         THING$="KEY"
         IF FNCARRY=0 THEN PRINT _
          "Get the key first.":EXIT SELECT
         IF ROOM<>15 THEN PRINT "Not here":EXIT SELECT
         IF FLAG(9)=1 THEN PRINT "Done that.":EXIT SELECT
         LET FLAG(9)=1:LET ROOMOBJECT$(ROOM,2)="SCUBA"
         LET ROOMOBJECT$(ROOM,3)="WETSUIT"
         LET INVENTORY$(ITEMNUMBER)="EMPTY":PRINT _
          "You see a WETSUIT and SCUBA in the LOCKER.  The KEY is stuck.":_
          DELAY 0.3:PRINT "Oh great, the door just slammed shut locking.":_
          PRINT "Andrea thinks you should READ the SIGN*."
          LET ROOMOBJECT$(ROOM,7)="SIGN*"
          LET ROOMOBJECT$(ROOM,8)="KEY*"
          LET FLAG(13)=1
         EXIT SELECT
       CASE "PICK"
          PRINT "Relax kid ---  Houdini you're not.":EXIT SELECT
       CASE "CLIMB"
         IF ROOM=31 THEN PRINT "Up you go!  Watch out for the bends.":_
           LET ROOM=29:EXIT SELECT
         IF ROOM<>29 THEN PRINT "Not here.":EXIT SELECT
         THING$="WETSUIT": IF FNCARRY=0 THEN PRINT _
          "The water is cold, GET a WETSUIT.":EXIT SELECT
         THING$="SCUBA":IF FNCARRY=0 THEN PRINT _
           "At this depth, you'll need diving equipment (SCUBA)":EXIT SELECT
         LET ROOM=31:PRINT "A little awkward -- but OK"
         EXIT SELECT

      CASE "DEPOSIT"
        IF OBJECT$<>"BUCK" THEN PRINT _
          "Andrea says that the only thing you DEPOSIT is the BUCK":EXIT SELECT
        IF ROOM<>13 THEN PRINT "Not here.":EXIT SELECT
        THING$="BUCK":IF FNCARRY<>1 THEN PRINT_
         "Andrea says you need to GET the BUCK first.":EXIT SELECT
        LET INVENTORY$(ITEMNUMBER)="DETECTOR"
        PRINT "You now carry a fine metal DETECTOR.  It says:"
        PRINT "For BEACH use only.":EXIT SELECT

     CASE "PAY"
       IF ROOM<>2 THEN PRINT "Not here.":EXIT SELECT
       DUMMY=0
       THING$=OBJECT$:IF FNCARRY=0 THEN PRINT _
        "You don't have the ";OBJECT$:EXIT SELECT
       IF OBJECT$="C-NOTE" THEN TOTAL=TOTAL+100:DUMMY=1
       IF OBJECT$="SAWBUCK" THEN TOTAL=TOTAL+10:DUMMY=1
       IF OBJECT$="FIN" THEN TOTAL=TOTAL+5:DUMMY=1
       INVENTORY$(ITEMNUMBER)="EMPTY"
       PRINT "The total paid so far is ";TOTAL;" dollars."
       PRINT "Additional needed for GOWN = ";115-TOTAL;" dollars."
       IF TOTAL=115 THEN LET INVENTORY$(ITEMNUMBER)="GOWN":_
        LET ROOMOBJECT$(ROOM,1)="EMPTY":PRINT:PRINT _
        "The shopkeeper takes the last of your money and":PRINT_
        "gives you the beautiful ballroom GOWN.":EXIT SELECT
       IF DUMMY=0 THEN PRINT "That's not for that."
       EXIT SELECT

     CASE "RIDE"
       IF ROOM<>7 THEN PRINT "Not here.":EXIT SELECT
       IF FLAG(11)=1 THEN PRINT _
         "The ROLLER-COASTER* is closed for cleaning.":EXIT SELECT
       LET FLAG(11)=1:LET ROOMOBJECT$(4,1)="BASKETBALL"
       PRINT "You and Andrea have never been so scared.  Andrea's hiccups are"
       PRINT "definitely gone.  She says she wants to see you SHOOT a BASKET."
       EXIT SELECT

    CASE "UNTANGLE"
      IF ROOM<>33 THEN PRINT "Not here.":EXIT SELECT
      IF FLAG(3)=1 THEN PRINT "Not again.":EXIT SELECT
      LET FLAG(3)=1:LET ROOMOBJECT$(ROOM,1)="LINE"
      LET ROOMOBJECT$(ROOM,2)="FISH":PRINT _
       "Ain't that the darndest thing -- the grateful PELICAN has gone ":PRINT_
       "off and returned with lots of smelly FISH.  Imagine your pleasure."
      EXIT SELECT

   CASE "IMAGINE"
     PRINT "Cute --- very cute.":EXIT SELECT

   CASE "SWAP"
     IF ROOM<>30 THEN PRINT "Not here.":EXIT SELECT
     IF FLAG(4)=1 THEN PRINT "Not again.":EXIT SELECT
     THING$="FISH":IF FNCARRY=0 THEN PRINT "GET the FISH first.":EXIT SELECT
     FLAG(4)=1:INVENTORY$(ITEMNUMBER)="FIN"
     ROOMOBJECT$(ROOM,2)="FISH*":PRINT _
       "The nice FISHMONGER has given you a FIN ($5) for the FISH."
     EXIT SELECT

   CASE "SHOOT"
     IF ROOM<>4 THEN PRINT "Not here.":EXIT SELECT
     IF FLAG(5)=1 THEN PRINT "Not again.":EXIT SELECT
     THING$="BASKETBALL":IF FNCARRY=0 THEN PRINT _
       "GET the BASKETBALL first.":EXIT SELECT
     PRINT "Nothing but net!  You coulda been a contenda!  Your reward is a "
     PRINT "nice stuffed ANIMAL.  Andrea turns away - she's allergic."
     LET INVENTORY$(ITEMNUMBER)="ANIMAL"
     EXIT SELECT

  CASE "REVEAL"
    IF ROOM<>1 AND ROOM<>9 AND ROOM<>10 AND ROOM<>16 THEN PRINT_
      "Not here.":EXIT SELECT
    IF ROOM=16 THEN
      IF FLAG(7)=1 THEN PRINT "Not again.":EXIT SELECT
      IF LEN(OBJECT$)<>9 THEN PRINT _
        "REVEAL a word 9 letters long, please.":EXIT SELECT
      IF OBJECT$<>"DRIFTWOOD" THEN PRINT "No, try again.":EXIT SELECT
      LET ROOMOBJECT$(ROOM,2)="DRIFTWOOD":LET FLAG(7)=1:PRINT _
        "Andrea wonders why you hadn't notice the DRIFTWOOD and ":PRINT_
        "bets that you could MAKE a swell MOBILE with it at the":PRINT_
        "CRAFTS CENTER.":LET ROOMOBJECT$(5,1)="SIGN*":_
        EXIT SELECT
    END IF
    IF ROOM=1 THEN
      IF FLAG(8)=1 THEN PRINT "Not again.":EXIT SELECT
      IF OBJECT$<>"DANCER" THEN PRINT "No, try again.":EXIT SELECT
      LET FLAG(8)=1:LET ROOMOBJECT$(ROOM,2)="MAN*"
      PRINT "Andrea notices the forlorn figure of a MAN* dancing alone."
    END IF
    IF ROOM=9 THEN
      IF OBJECT$<>"APPLE" THEN PRINT "I'm confused - READ SIGN*":EXIT SELECT
      PRINT "Andrea thinks you fell for an AFFIRMING THE CONSEQUENT "
      PRINT "fallacy.  Look over that section of the workbook.":_
      LET LOGIC$=LOGIC$+"..AFFIRM..":EXIT SELECT
    END IF
    IF ROOM=10 THEN
      IF FLAG(19)=1 THEN PRINT "Not again.":EXIT SELECT
      IF OBJECT$<>"ORANGE" THEN PRINT "I'm confused -- READ the SIGN*":EXIT SELECT
      LET ROOMOBJECT$(ROOM,2)="ORANGE":LET FLAG(19)=1
      PRINT "Andrea suggests we OFFER the ORANGE to the GUARD*":EXIT SELECT
    END IF
    EXIT SELECT

  CASE "MAKE"
    IF ROOM<>32 AND ROOM<>5 THEN PRINT "Not here.":EXIT SELECT
    IF ROOM=32 THEN
      IF FLAG(2)=1 THEN PRINT "It's been done.":EXIT SELECT
      LET ROOMOBJECT$(ROOM,1)="BRIDGE*"
      THING$="SURFBOARD":IF FNCARRY=1 THEN _
        LET INVENTORY$(ITEMNUMBER)="EMPTY" ELSE PRINT "WEIRD":EXIT SELECT
      PRINT "The huge SURFBOARD makes a nice BRIDGE and you can now GO SOUTH.":_
        LET FLAG(2)=1:EXIT SELECT
    END IF
    IF ROOM=5 THEN
      THING$="DRIFTWOOD":IF FNCARRY=0 AND FLAG(7)=0 THEN _
        PRINT "REVEAL object on BEACH first.":EXIT SELECT
      IF FNCARRY=0 THEN PRINT "GET the DRIFTWOOD.":EXIT SELECT
      THING$="LINE":IF FNCARRY=0 THEN PRINT _
       "GET some LINE first.":EXIT SELECT
      THING$="KNIFE":IF FNCARRY=0 THEN PRINT _
       "Andrea says you'll need a KNIFE.":EXIT SELECT
      THING$="DRIFTWOOD":LET INVENTORY$(ITEMNUMBER)="EMPTY"
      THING$="LINE":LET INVENTORY$(ITEMNUMBER)="EMPTY"
      THING$="KNIFE":LET INVENTORY$(ITEMNUMBER)="SAWBUCK"
      PRINT "The owner quickly sold your excellent MOBILE, giving you"
      PRINT "a sawbuck ($10).  Is there no end to your talent?  Don't speak."
    END IF
    EXIT SELECT
  CASE "OFFER"
    IF ROOM<>25 THEN PRINT "Not here":EXIT SELECT
    IF FLAG(12)=1 THEN PRINT "Not again":EXIT SELECT
    IF OBJECT$<>"ORANGE" THEN PRINT "Not that.":EXIT SELECT
    THING$="ORANGE":IF FNCARRY=0 THEN PRINT "You don't have it.":EXIT SELECT
    LET FLAG(12)=1
    LET ROOMOBJECT$(ROOM,1)="EMPTY"
    LET INVENTORY$(ITEMNUMBER)="EMPTY"
    PRINT "The GUARD* wanders off eating the ORANGE without saying thanks."
   EXIT SELECT
  CASE "TRY"
    IF ROOM<>15 THEN PRINT "Not here":EXIT SELECT
    IF FLAG(13)=0 THEN PRINT "No need":EXIT SELECT
    LET COMBO$="DUDDU"
    IF (LEN(OBJECT$)<>5)OR (LEFT$(OBJECT$,1)<>"U" AND LEFT$(OBJECT$,1)<>"D")_
      THEN PRINT "Try - TRY UDUDU or some such.":EXIT SELECT
    TRYNUMB=TRYNUMB+1
    IF COMBO$=OBJECT$ THEN
      LET FLAG(13)=0
      PRINT "The door swings gently open.  Andrea says --well done":EXIT SELECT
    END IF
    HITS=0
    FOR I=1 TO 5
     LET DUMMY$=MID$(OBJECT$,I,1)
     IF DUMMY$=MID$(COMBO$,I,1) THEN HITS=HITS+1
    NEXT I
    PRINT "You hear the lock mechanism whir and click ";HITS;" times."
   EXIT SELECT
   CASE "KILL","MURDER","RAPE","DESTROY","ASSAULT"
     PRINT "Your mom is right - you watch entirely too much tv."
    EXIT SELECT

      CASE ELSE
            PRINT "Andrea says to try another verb.":EXIT SELECT
    END  SELECT
END SUB

'MODULE 2.3.1 DEFINE THE FUNCTION- IS THE OBJECT PRESENT?
DEF FNPRESENT
    SHARED ROOMOBJECT$(),OBJECT$,ITEMNUMBER
         FOR J=1 TO 15
             IF ROOMOBJECT$(ROOM,J)=OBJECT$ THEN _
                 FNPRESENT=1:ITEMNUMBER=J:EXIT DEF
         NEXT J
         FNPRESENT=0
END DEF

'MODULE 2.3.2 DEFINE THE FUNCTION- IS IT BEING CARRIED?
DEF FNCARRY
    SHARED INVENTORY$(),THING$,ITEMNUMBER
    FOR I = 1 TO 5
        IF INVENTORY$(I)=THING$ THEN FNCARRY = 1: ITEMNUMBER = I: EXIT DEF
    NEXT I
    FNCARRY=0
END DEF

'MODULE 2.4 UPDATE DATA
SUB UPDATE
END SUB

'MODULE 3 CLOSING
SUB CLOSING
    SHARED FLAG(),TURNNUMBER,ANSWER$(),STRT,GUESSNUMB,LOGIC$,TRYNUMB,SLAMMER
    PRINT:PRINT
IF FLAG(1)=1 THEN PRINT " *****   HOORAY FOR YOU!!  YOU'VE WON THE GAME!!  ****"
    PRINT
   FOR I=1 TO 4:    PRINT:NEXT I
    INPUT "Be sure that your disk is in the drive and press ENTER. OK";DUMMY$

       DIM DTA(40),DTA$(10)
       OPEN "REPORT.DTA" FOR INPUT AS #1
         REM RETRIEVES OLD DATA FROM REPORT.DTA
         FOR I=1 TO 40
           INPUT #1,DTA(I)
         NEXT I
         FOR I=1 TO 10
           INPUT #1,DTA$(I)
         NEXT I
       CLOSE #1

    LET DTA(12)=FLAG(1):LET DTA(37)=DTA(37)+INT((TIMER-STRT)/6)
       OPEN "REPORT.DTA" FOR OUTPUT AS #1
         REM SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
         FOR I=1 TO 40
           WRITE #1,DTA(I)
         NEXT I
         FOR I=1 TO 10
           WRITE #1,DTA$(I)
         NEXT I
       CLOSE #1

    OPEN "BOARDATA.TXT" FOR APPEND AS #2
       PRINT #2,TIME$,GUESSNUMB,LOGIC$
       PRINT #2,TRYNUMB,SLAMMER,ANSWER$(0)
       FOR I=1 TO TURNNUMBER
         IF INT(I/5)=I/5 THEN PRINT #2,
         PRINT #2,ANSWER$(I),
       NEXT I
       PRINT #2,INT((STRT-TIMER)/6),INT(1000*RND(0))
    CLOSE #2
    PRINT "This game is over, type BOARDWALK  to play again."
END SUB

'END OF PROGRAM
