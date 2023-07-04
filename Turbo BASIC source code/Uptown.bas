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
    DATA 16,BACKYARD,GARAGE,TOOL ROOM,LOWER LANDING,BATHROOM
    DATA DINING ROOM,KITCHEN,HALL,ENTRYWAY,LIBRARY
    DATA OFFICE,UPPER LANDING,BLUE BEDROOM,PINK BEDROOM,BEIGE BEDROOM
    DATA ATTIC

    FOR I=1 TO NUMBERROOMS
       LET DESCRIBEFLAG(I)=0
       READ ROOMDESCRIBE$(I),ROOMDES2$(I)
    NEXT I
    DATA "We're outside of the house in the backyard.  The dirt"
    DATA "shows the signs of much digging and smoothing over."

    DATA "Here in the garage is a vintage classic car.  Andrea"
    DATA "is admiring the paint job."

    DATA "Funny, you don't notice any tools here in the tool room."
    DATA "This is a weird house."

    DATA "Here on the lower landing stands a magnificent bust of"
    DATA "Wolfgang Amadeus Mozart.  Careful, it doesn't look stable."

    DATA "The bathroom is nice ... running water ... the whole bit."
    DATA " "

    DATA "Here in the dining room, you can't help but notice that"
    DATA "a portable radio is bolted to a shelf.  Strange."

    DATA "The kitchen is clearly the scene of a major"
    DATA "remodeling project."

    DATA "Here in the cavernous hall, your footsteps echo on the"
    DATA "marble floor.  Ahead, you see a cat scamper by."

    DATA "The entryway could have been called an atrium -- it's"
    DATA "that airy and light.  Andrea loves it."

    DATA "From the dust in the library, you guess that Uncle Gus"
    DATA "doesn't read much."

    DATA "The office is an office .....  What can I say?"
    DATA "The painting is ugly and the plant's about dead."

    DATA "Architecturally, the upper landing is not noteworthy."
    DATA " "

    DATA "As you enter the blue bedroom, a huge dog tries to take a "
    DATA "bite out of you.  Luckily, a rope ties him to the bedpost."

    DATA "You thought you heard a motor shutting off as you "
    DATA "entered the pink bedroom.  Very odd."

    DATA "The beige bedroom is ... well ..... beige."
    DATA " "

    DATA "No surprise here.  The musty attic is dark and damp."
    DATA "I'll bet the roof leaks in winter."
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
    DATA 0,0,2,0,0,0
    DATA 0,3,0,1,4,0
    DATA 2,0,0,0,0,0
    DATA 0,6,0,0,12,2
    DATA 0,0,6,0,0,0

    DATA 4,7,8,5,0,0
    DATA 6,0,0,0,0,0
    DATA 11,10,9,6,0,0
    DATA 0,0,0,8,0,0
    DATA 8,0,0,0,0,0

    DATA 0,8,0,0,0,0
    DATA 14,15,0,13,0,4
    DATA 0,0,12,0,0,0
    DATA 0,12,0,0,0,0
    DATA 12,0,0,0,0,0

    DATA 0,0,0,0,0,14



END SUB

'MODULE 1.4 INVENTORY
SUB INVENTORY
    SHARED INVENTORY$()
    DIM INVENTORY$(5)
    FOR I=1 TO 5
        LET INVENTORY$(I)="EMPTY"
    NEXT I
END SUB

'MODULE 1.5 OTHERS
SUB OTHERS
    SHARED GAMESTATE$,ROOM,TURNNUMBER,ROOMOBJECT$(),THING$,INVENTORY$(),_
        FLAG(),OBJ$(),OBJ2$(),DESCRIBEFLAG(),ROOMDESCRIBE$(),ROOMDES2$(),_
        GUESSNUMB,CLUE$(),WURD$(),NUMBERROOMS,LOGIC$,SLAMMER,TRYNUMB
    LET TURNNUMBER =0
    LET THING$=""
    FOR I=1 TO 30:LET FLAG(I)=0:NEXT I      'SET FLAGS TO 0
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
     FOR I=1 TO NUMBERROOMS
       LET CLUE$(I)="":LET WURD$(I)=""
     NEXT I
     LET CLUE$(11)="PLANTS"
     LET WURD$(11)="WATER"
     LET CLUE$(2)="RING"
     LET WURD$(2)="FINGER"
     LET CLUE$(15)="ELECTRICITY"
     LET WURD$(15)="WIRE"
    GUESSNUMB=0:'TOTAL NUMBER OF GUESSES AT WORDS
    TRYNUMB=0:'TOTAL # OF TRIES AT COMBO SAFE LOCK
    SLAMMER=0:'TOTAL # OF TRIES AT FINAL SOLUTION MATRIX
    LOGIC$="Logfal - "

    FOR I=1 TO 35
      READ ROOM,J,ROOMOBJECT$(ROOM,J)
    NEXT I
    DATA 1,1,EMPTYPISTOL,1,2,EMPTYHOLES*,1,3,NOTE*
    DATA 2,1,CAR*,2,2,EMPTYFLASHLIGHT,3,1,EMPTYWIRE
    DATA 4,1,BUST*,4,2,EMPTYRING,4,3,EMPTYRUBBLE*
    DATA 4,4,NOTE*,5,1,SINK*,6,1,SHELF*
    DATA 6,2,RADIO*,6,3,EMPTYBATTERIES,7,1,SIGN*

    DATA 7,2,GLASS,7,3,EMPTYKEY,6,4,NOTE*
    DATA 8,1,NOTE*,9,1,SIGN*,10,1,NOTE*
    DATA 10,2,EMPTYYARN,11,1,PLANT*,11,2,PAINTING*
    DATA 11,3,EMPTYSHARD,12,1,NOTE*,13,1,BED*
    DATA 13,2,DOG*,13,3,EMPTYCAT*,14,1,NOTE*

    DATA 14,2,EMPTYBUTTON*,15,1,EMPTYNIGHTSTAND*,15,2,EMPTYNECKLACE
    DATA 16,1,NOTE*,14,3,EMPTYLADDER*

 'THE STARTING ROOM IS THE ENTRYWAY - 9
    ROOM=9


    FOR I=1 TO 25                       'READ IN WORD!* CONVERSION
        READ OBJ$(I),OBJ2$(I)
    NEXT I
    DATA GUN,PISTOL,HOLES,HOLES*,HOLE,HOLE*
    DATA NOTE,NOTE*,LIGHT,FLASHLIGHT,BUST,BUST*
    DATA RUBBLE,RUBBLE*,SINK,SINK*,SHELF,SHELF*
    DATA RADIO,RADIO*,BATTERY,BATTERIES,SIGN,SIGN*
    DATA PLANT,PLANT*,PAINTING,PAINTING*,SAFE,SAFE*

    DATA LETTER,LETTERS,SHARDS,SHARD,BED,BED*
    DATA DOG,DOG*,BUTTON,BUTTON*,NIGHTSTAND,NIGHTSTAND*
    DATA LADDER,LADDER*,CHARGE,CHARGES,CAT,CAT*
    DATA CAR,CAR*
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
           PRINT "UPTOWN (at the DOS prompt) and later,":_
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
        ROOMDES2$(),CLUE$(),WURD$(),GUESSNUMB,TRYNUMB,LOGIC$
    PRINT:PRINT
    IF TURNNUMBER=1 THEN CALL GAMESTARTER   'MODULE 2.1.1
    IF DESCRIBEFLAG(ROOM)=1 THEN PRINT "You are in the  ";DESCRIPTION$(ROOM)
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
     IF ROOM=2 OR ROOM=11 OR ROOM=15 OR ROOM=13 OR ROOM=4 THEN DELAY 0.3
     IF (ROOM=2 OR ROOM=11 OR ROOM=15) AND FLAG(ROOM)=0 THEN PRINT _
       "Andrea says that she's thinking of a ";LEN(WURD$(ROOM));_
        "-letter word":PRINT _
       "that has something to do with ";CLUE$(ROOM);".":DELAY 0.3:PRINT:PRINT _
       "If you would like to guess what the word is, type GUESS followed ":_
       PRINT "by your guess, for example,  GUESS COW ":DELAY 0.3

     THING$="YARN"
     IF ROOM=13 AND FNCARRY=1 AND FLAG(12)=0 THEN FLAG(12)=1:PRINT _
      "The cat came in with you and leaped atop the canopy bed just ":PRINT _
      "as the dog was making a lunge for her.  He's barking up a storm,": PRINT_
      "concentrating all of his attention on the cat.":ROOMOBJECT$(13,3)="CAT*":
     IF ROOM=4 AND FNCARRY=1 AND FLAG(3)=0 THEN FLAG(3)=1: PRINT _
      "The cat makes an unsuccessful dive for the yarn ":PRINT _
      "and then scampers away."


  PRINT

END SUB

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
SUB GAMESTARTER
     CLS
     PRINT "              WELCOME TO UPTOWN!"
     PRINT
     PRINT "     Bear with me because this is a little strange.  Your"
     PRINT "uncle, Gustav (call me Gus) Braenover, the famous police "
     PRINT "inspector, has had one of his fits and is in the police"
     PRINT "sanitarium for a few days.  He'll be okay, but in the interim,"
     PRINT "it will be up to you, and your good friend Andrea the android,"
     PRINT "to crack the three major cases he was working on.  If you are"
     PRINT "unable to gather enough evidence to PRESS CHARGES, the perps"
     PRINT "will walk.  Fortunately, Uncle Gus left some notes to himself"
     PRINT "as he walked around his uptown mansion, Braenover Hall.  Read"
     PRINT "these notes and treat them as true statements in reaching "
     PRINT "your conclusions.  Andrea the android, although maddeningly"
     PRINT "absent-minded and fastidious, gives good advice that you've"
     PRINT "come to rely upon.  "
     PRINT "     Briefly, the cases boil down to this:  Three suspects,"
     PRINT "named Ash, Baker, and Carr are currently being held on three"
     PRINT "separate charges, murder, jewel theft, and blackmail.  The"
     PRINT "bad news is that only Uncle Gus knows who should be charged"
     PRINT "with which crime, and he can't talk right now.  Your task is"
     PRINT "to assemble sufficient clues, including the incriminating "
     PRINT "letters, the murder weapon, and the stolen jewels, before "
     PRINT "it's too late!"
     PRINT "If you're through taking notes, press the Enter key for more."
     DO WHILE LEN(INKEY$)=0:LOOP
     CLS
     PRINT
     PRINT:PRINT
     PRINT"                COMMAND INSTRUCTIONS:"
     PRINT:PRINT:PRINT
     PRINT "    Give COMMANDs as VERB then OBJECT, Such as GO NORTH,"
     PRINT "READ SIGN, PRESS CHARGES, GET SHARD, FILL GLASS, DROP PISTOL,"
     PRINT "CUT ROPE, OPEN SAFE, and so forth."
     PRINT:PRINT
     PRINT "    Exceptions to this two-word sentence rule are single-"
     PRINT "letter commands such as N to GO NORTH, and U to GO UP."
     PRINT "    P.S. Don't try to get objects ending in an *, e.g., SAFE*,"
     PRINT "As they are quite unobtainable.  "
     PRINT:PRINT
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
  SHARED ROOM,ROOMOBJECT$()
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
END SUB

'MODULE 2.2 COMMANDS
SUB COMMANDS
    SHARED VERB$, OBJECT$,ROOM,DESCRIPTION$(),FLAG(),INVENTORY$(),_
        ROOMOBJECT$(),OBJ$(),OBJ2$(),C$,ANSWER$(),TURNNUMBER,_
        CLUE$(),WURD$(),GUESSNUMB

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
        GUESSNUMB,CLUE$(),WURD$(),LOGIC$,TRYNUMB,SLAMMER
    IF FLAG(1)<>0 THEN EXIT SUB
    IF FLAG(14)=1 THEN EXIT SUB
    SELECT CASE VERB$
        CASE "QUIT","Q"
            LET FLAG(1)=2   'GAME NOT WON(1) OR LOST(-1):EXIT SELECT
        CASE "SAVE"
            IF OBJECT$<>"GAME" THEN PRINT "SAVE GAME":EXIT SELECT
            OPEN "UPSAV.BAS" FOR OUTPUT AS #1
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
            WRITE #1,GUESSNUMB,LOGIC$,SLAMMER,TRYNUMB
            CLOSE #1
            PRINT "OK
            EXIT SELECT
        CASE "RESTORE"
            IF OBJECT$<> "GAME" THEN PRINT "Try  RESTORE GAME":EXIT SELECT
            OPEN "UPSAV.BAS" FOR INPUT AS #1
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
            INPUT #1,GUESSNUMB,LOGIC$,SLAMMER,TRYNUMB
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


            IF MOVEMENTTABLE (ROOM,DIRECTION) = 0 THEN _
                PRINT "You can't go that way." ELSE _
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
            EXIT SELECT

        CASE "GET","TAKE"
            IF OBJECT$="BATTERIES" AND FLAG(14)=0 THEN PRINT _
              "Try  REMOVE BATTERIES":EXIT SELECT
            IF OBJECT$="NOTE*" THEN PRINT "Try  READ NOTE*":EXIT SELECT
            IF OBJECT$="SIGN*" THEN PRINT "Try  READ SIGN*":EXIT SELECT
            IF OBJECT$="WATER" THEN PRINT "Try  FILL GLASS  ":EXIT SELECT
            IF  FNPRESENT = 0 THEN PRINT "I DON'T SEE  ";OBJECT$:EXIT SELECT
            IF RIGHT$(OBJECT$,1)="*" THEN PRINT "I CAN'T HANDLE IT":EXIT SELECT

            IF FLAG(6)=0 AND OBJECT$="YARN" AND ROOM=10 THEN PRINT _
              "A cat seems to be showing great interest in the yarn.":PRINT _
              "Noticing you noticing her, she scampers off.":FLAG(6)=1

            FOR I=1 TO 5
                IF INVENTORY$(I)="EMPTY" THEN INVENTORY$(I)=OBJECT$:_
                    PRINT "GOT IT":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY":_
                    EXIT SELECT
            NEXT I

            PRINT "You're carrying too much.  Drop something first."
            EXIT SELECT
        CASE "PUT","DROP","GIVE"
            IF OBJECT$="WATER" OR OBJECT$="GLASS" THEN PRINT _
             "Try   WATER PLANT   ":EXIT SELECT
            THING$=OBJECT$
            IF FNCARRY=0 THEN PRINT "You don't have the ";OBJECT$:EXIT SELECT


            FOR J=1 TO 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":_
                    EXIT SELECT
            NEXT J
            PRINT "This room is full; take it elsewhere":EXIT SELECT

        CASE "READ"
            IF ROOM=7 THEN PRINT _
             "The sign says that the kitchen is under repair and":PRINT_
             "to get water from the bathroom if you need it.":EXIT SELECT
            IF ROOM=9 THEN PRINT _
             "You'll need to gather 4 vital pieces of evidence and ":PRINT _
             "DROP them here.  You'll also need to put together 4 ":PRINT _
             "additional clues before you are ready to PRESS CHARGES ":_
             EXIT SELECT
            PRINT "The inspector's note reads:"
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
            PRINT "Andrea says that we're in the wrong room for that."
            EXIT SELECT

       CASE ",DRINK","IMBIBE"
          PRINT "Andrea says that you are not thirsty.":EXIT SELECT
       CASE "GUESS"
        IF ROOM<>2 AND ROOM<>11 AND ROOM<>15 THEN PRINT _
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
        PRINT "Andrea says that ";HITS;" letters of your word ":PRINT _
              "are also found in her word.  Try again."
         EXIT SELECT
       CASE "PLAY"
         IF ROOM=6 AND OBJECT$="RADIO*" THEN PRINT _
           "Sounds fine. ":EXIT SELECT
         PRINT "Andrea warns that we have no time for such foolishness."
         EXIT SELECT
       CASE "COUGH"
         IF FLAG(22)=0 THEN PRINT _
          "Andrea thinks you're jumping to conclusions.":EXIT SELECT
         PRINT "You may have fallen for an affirming the ":_
         PRINT "consequent fallacy.  Check your text.":_
         LOGIC$=LOGIC$+" AFFIRM":EXIT SELECT
       CASE "SNEEZE"
         IF FLAG(23)=0 THEN PRINT_
          "You need more information to reach that conclusion.":EXIT SELECT
         PRINT "You may have fallen for a denying the antecedent":_
         PRINT "fallacy.  Check your text.":_
         LOGIC$=LOGIC$+" DENY":EXIT SELECT
       CASE "WATER"
         IF ROOM<>11 THEN PRINT _
          "Andrea reminds you that the plant is in the office.":EXIT SELECT
        THING$="GLASS"
        IF FNCARRY=0 THEN PRINT _
         "Andrea thinks you should get the glass first.":EXIT SELECT
       IF FLAG(7)=0  THEN PRINT _
         "Andrea thinks you should FILL GLASS with water first.":EXIT SELECT
       LET ROOMOBJECT$(11,3)="SAFE*"
       LET ROOMOBJECT$(11,5)="RUBBLE*"
       LET ROOMOBJECT$(11,6)="SHARD"
       PRINT "The glass slips from your hand, breaking as you try to water":PRINT_
        "the plant.  The glass breaks into one sharp shard and a bunch of":PRINT_
        "rubble.  Fortunately, the water fell on the plant.  Gadzooks! ":PRINT_
        "The crazy plant, having absorbed the water is regaining its":PRINT_
        "original shape!  One branch just bumped the painting, tipping":PRINT_
        "it enough to reveal a hidden wall safe behind.  That was cool!" :
        FOR I=1 TO 5
         IF INVENTORY$(I)="GLASS" THEN INVENTORY$(I)="EMPTY"
        NEXT I
         EXIT SELECT
       CASE "FILL"
         IF OBJECT$<>"GLASS" THEN PRINT _
           "Andrea thinks the only thing to fill is the glass.":EXIT SELECT
         IF ROOM<>5 THEN PRINT "This isn't the place for that.":EXIT SELECT
         THING$="GLASS"
           IF FNCARRY=0 THEN PRINT "Get the glass first.":EXIT SELECT
           IF FLAG(7)=2 THEN PRINT "Already accomplished.":EXIT SELECT
           PRINT "You now have a full glass of water!":FLAG(7)=1
         EXIT SELECT
       CASE "OPEN"
         IF OBJECT$="CAR*" OR OBJECT$="CAR" THEN PRINT "Try  UNLOCK CAR*":EXIT SELECT
         IF OBJECT$="NIGHTSTAND*" THEN PRINT _
           "Didn't work.  Andrea says to try to PICK LOCK":EXIT SELECT
         IF OBJECT$<>"SAFE*" THEN PRINT "Can't.":EXIT SELECT
         PRINT "Andrea notices that the safe is a 3-dial tumbler lock.":PRINT_
         "Each dial can be set to either 1, 2, or 3.  To try to":PRINT _
         "open the safe, type TRY followed by the combination, e.g.,":PRINT_
         "TRY 121 or TRY 213 until it opens.  Listen closely to ":PRINT_
         "hear the number of tumblers that fall with each try."
         EXIT SELECT
       CASE "BREAK"
         PRINT "Andrea is sulking.  She hates violence."
         EXIT SELECT
       CASE "TRY"
         IF ROOM<>11 THEN PRINT "Go to the office first.":EXIT SELECT
         COMBO$="231"
         IF FLAG(9)=1 THEN PRINT "The safe is already open.":EXIT SELECT
         HITS=0
         FOR I=1 TO 3
           IF I=VAL(LEFT$(OBJECT$,1)) THEN HITS=HITS+1
         NEXT I
         IF HITS=0 OR LEN(OBJECT$)<>3 THEN PRINT "TRY a 3-digit number.":EXIT SELECT
         TRYNUMB=TRYNUMB+1
         PRINT "Andrea spins the dials and tries ";OBJECT$
         IF OBJECT$=COMBO$ THEN PRINT _
          "The door of the safe pops open, revealing the blackmail letters.":_
          ROOMOBJECT$(11,4)="LETTERS":FLAG(9)=1:EXIT SELECT
         HITS=0
         FOR I=1 TO 3
          LET DUMMY$=MID$(OBJECT$,I,1)
          IF DUMMY$=MID$(COMBO$,I,1) THEN HITS=HITS+1
         NEXT I
         PRINT "The safe didn't open, but Andrea heard ";HITS;" tumbler(s) fall."
         EXIT SELECT
       CASE "DIG"
         PRINT "Can't.  Get some help."
         EXIT SELECT
       CASE "USE"
         PRINT "To do what?  Be more specific."
         EXIT SELECT
       CASE "FREE","UNTIE"
         IF ROOM<>13 THEN PRINT "You are in the wrong room for that.":_
           EXIT SELECT
         IF FLAG(5)=1 THEN PRINT "The dog is long gone.":EXIT SELECT
         IF FLAG(4)=1 THEN PRINT _
          "For some reason, you back up when the dog growls.":EXIT SELECT
         FLAG(4)=1:PRINT _
          "Andrea has an idea.  She says she noticed a piece ":PRINT_
          "of yarn in the library what might help."
          ROOMOBJECT$(10,2)="YARN"
         EXIT SELECT
       CASE "CUT","SLICE"
         IF ROOM<>13 THEN PRINT "Not here.":EXIT SELECT
         IF OBJECT$<>"ROPE" THEN PRINT "Try  CUT ROPE ":EXIT SELECT
         IF FLAG(5)=1 THEN PRINT "The dog, as they say, has split.":EXIT SELECT
         IF FLAG(12)=0 THEN PRINT _
          "Andrea pulls you back just as the dog's teeth close ":PRINT_
          "on your hand.  That was close. Whew!":EXIT SELECT
         THING$="SHARD"
         IF FNCARRY=0 THEN PRINT "Get something to cut with.":EXIT SELECT
         IF FLAG(12)=1 THEN PRINT _
          "As soon as the dog realized he was free, he bounded":PRINT_
          "out the door with the cat in hot pursuit.  A few":PRINT_
          "seconds later, you hear a loud crash!":FLAG(5)=1
          LET ROOMOBJECT$(13,3)="EMPTY"
          LET ROOMOBJECT$(13,2)="EMPTY"
          LET ROOMOBJECT$(4,1)="EMPTY"
          LET ROOMOBJECT$(4,2)="RING"
          LET ROOMOBJECT$(4,3)="RUBBLE*"
          LET ROOMOBJECT$(1,1)="PISTOL"
          LET ROOMOBJECT$(1,2)="HOLES*"
         EXIT SELECT
       CASE "UNLOCK"
         IF ROOM<>2 THEN PRINT "Try  PICK  or  OPEN":EXIT SELECT
         IF FLAG(13)=1 THEN PRINT "It's been done, as they say.":EXIT SELECT
         THING$="KEY"
         IF FNCARRY=0 THEN PRINT _
          "Get the key first.":EXIT SELECT
         FLAG(13)=1: PRINT _
          "The car trunk opens, revealing an empty flashlight. ":PRINT_
          "Andrea doesn't see what good that will do."
          ROOMOBJECT$(2,2)="FLASHLIGHT"
         EXIT SELECT
       CASE "PICK"
         IF ROOM<>15 THEN PRINT "Not here.":EXIT SELECT
         IF FLAG(24)=1 THEN PRINT "We've finished with that.":EXIT SELECT
         THING$="WIRE"
           IF FNCARRY=0 THEN PRINT "You'll need a piece of wire.":EXIT SELECT
         PRINT "After Andrea tried and failed (you need the touch),":PRINT_
          "you manage to wiggle the piece of wire in the lock ":PRINT_
          "of the nightstand opening it.  Inside you see a":PRINT_
          "necklace that must be worth a fortune. Wow!":
         LET ROOMOBJECT$(15,2)="NECKLACE":FLAG(24)=1:EXIT SELECT
       CASE "REMOVE"
         IF FLAG(14)=1 THEN PRINT "That's been done already.":EXIT SELECT
         IF ROOM<>6 THEN PRINT "This is hardly the place.":EXIT SELECT
         FLAG(14)=1:ROOMOBJECT$(6,3)="BATTERIES":PRINT_
          "Andrea says we can now LOAD FLASHLIGHT. Duh."
         EXIT SELECT
       CASE "UNLOAD"
         PRINT "Andrea doesn't like that idea.":EXIT SELECT
       CASE "LOAD"
         IF OBJECT$<>"FLASHLIGHT" AND OBJECT$<>"BATTERIES" THEN _
          PRINT "Andrea says you shouldn't do something you'll regret.":_
          EXIT SELECT
         IF FLAG(16)=1 THEN PRINT "You're becoming repetitive.":EXIT SELECT
         THING$="FLASHLIGHT"
           IF FNCARRY=0 THEN PRINT "Get the flashlight.":EXIT SELECT
         OBJECT$="BATTERIES"
         IF FNPRESENT=0 THEN PRINT "I don't see the batteries.":EXIT SELECT
         ROOMOBJECT$(6,3)="EMPTY"
         FLAG(16)=1:ROOMOBJECT$(14,2)="BUTTON*":PRINT_
          "The flashlight now shines with a bright beam of light.":PRINT_
          "Andrea wonders why it has no on-off switch.  Weird.":PRINT_
          "Oh yes, Andrea says that she is reminded that she":PRINT_
          "wonders if you noticed the wall button in the pink":PRINT_
          "bedroom.  Have we been in the pink bedroom?  Who knows?"
         EXIT SELECT
       CASE "TURN","SWITCH"
         PRINT "Somehow, that didn't work."
         EXIT SELECT
       CASE "PUSH"
         IF FLAG(16)=0 THEN PRINT _
          "Andrea says she hates people who push or press too much.":EXIT SELECT
         IF ROOM<>14 THEN PRINT "Not here.":EXIT SELECT
         IF FLAG(17)=1 THEN PRINT "Nothing else happens.":EXIT SELECT
         PRINT "You hear the sound of a motor and see a ladder ":PRINT_
          "descend from an opening in the ceiling.  Andrea says":PRINT_
          "that if we have a light we can CLIMB LADDER to the attic."
          FLAG(17)=1:ROOMOBJECT$(14,3)="LADDER*"
         EXIT SELECT
       CASE "CLIMB"
         IF ROOM<>14 THEN PRINT "This isn't the place for that.":EXIT SELECT
         IF FLAG(17)=0 THEN PRINT "PUSH BUTTON first.":EXIT SELECT
         THING$="FLASHLIGHT"
          IF FNCARRY=0 THEN PRINT _
           "You'll need to carry the flashlight.":EXIT SELECT
         PRINT "Upsy daisy.":ROOM=16
         EXIT SELECT

      CASE "PRESS"
        IF OBJECT$<>"CHARGES" THEN PRINT _
         "Try PRESS CHARGES or PUSH BUTTON":EXIT SELECT
        IF ROOM<>9 THEN PRINT _
         "Go to the entryway first.":EXIT SELECT
          LET DUMMY3$(1)="LETTERS"
          LET DUMMY3$(2)="NECKLACE"
          LET DUMMY3$(3)="RING"
          LET DUMMY3$(4)="PISTOL"
          FOR I=1 TO 4
          LET OBJECT$=DUMMY3$(I)
          IF FNPRESENT=0 THEN PRINT _
           "DROP the ";OBJECT$;" here first.":EXIT SELECT
        NEXT I
        DATA LETTERS,NECKLACE,RING,PISTOL
        HITS=0
        FOR I=18 TO 21
         IF FLAG(I)=1 THEN HITS=HITS+1
       NEXT I
       IF HITS<4 THEN PRINT _
        "You still have one or more notes to ponder.":EXIT SELECT
       SOLUTION$="CAB":PRINT _
         "Enter the solution by first letter of last name in the":PRINT_
         "following order: murderer then jewel thief then blackmailer.":PRINT_
         "For example, typing ABC means you think Ash was the ":PRINT_
         "murderer, Baker the jewel thief, and Carr the blackmailer."
       DUMMY$=""
       DO UNTIL LEN(DUMMY$)=3
        INPUT "Enter a 3-letter answer (e.g., ABC), please. ";DUMMY$
       LOOP
       LET DUMMY$=UCASE$(DUMMY$)
       SLAMMER=SLAMMER+1
       IF SOLUTION$=DUMMY$ THEN FLAG(1)=1:PRINT _
        "Charges are pressed and the rascals are taken to the hoosgow.":_
        EXIT SELECT
       LET DUMMY2$=LEFT$(DUMMY$,1)
       IF DUMMY2$="C" THEN PRINT _
        "You may have fallen for a denying the antecedent fallacy.":_
        LOGIC$=LOGIC$+" DENY+ ":EXIT SELECT
       IF DUMMY2$="B" THEN PRINT_
        "You may have failed to follow an affirming the antecedent":PRINT_
        "syllogism.":LOGIC$=LOGIC$+" affirm-":EXIT SELECT
       LET DUMMY2$=MID$(DUMMY$,2,1)
       IF DUMMY2$="B" THEN PRINT _
        "You may have misread a disjunctive syllogism.  Go over your notes."
         LOGIC$=LOGIC$+" DISJUNCT":EXIT SELECT


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
    PRINT
IF FLAG(1)=1 THEN PRINT " *****   HOORAY FOR YOU!!  YOU'VE WON THE GAME!!  ****"
    PRINT
   FOR I=1 TO 6:    PRINT:NEXT I
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

    LET DTA(11)=FLAG(1):LET DTA(36)=DTA(36)+INT((TIMER-STRT)/6)
       OPEN "REPORT.DTA" FOR OUTPUT AS #1
         REM SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
         FOR I=1 TO 40
           WRITE #1,DTA(I)
         NEXT I
         FOR I=1 TO 10
           WRITE #1,DTA$(I)
         NEXT I
       CLOSE #1

    OPEN "UPDATA.TXT" FOR APPEND AS #2
       PRINT #2,TIME$,GUESSNUMB,LOGIC$
       PRINT #2,TRYNUMB,SLAMMER,ANSWER$(0)
       FOR I=1 TO TURNNUMBER
         IF INT(I/5)=I/5 THEN PRINT #2,
         PRINT #2,ANSWER$(I),
       NEXT I
       PRINT #2,INT((STRT-TIMER)/6),INT(1000*RND(0))
    CLOSE #2
    PRINT "This game is over, type  UPTOWN  to play again."
END SUB

'END OF PROGRAM
