'MODULE 0 MAIN  ***PIRATE 8-31-90***
'CHANGED GAMEDTA.BAS TO PIRESAV.BAS ON 1-24-91
'GO DOWNSTAIRS FROM MAIN DECK 6MAR92
'CHANGED CLOCK TIMER STRT 2-3-94
'CHANGED SAVE GAME DATE$ TIME$ 7-30-95
'snips added 1-2-96
'PIRTDATA.TXT ADDED AND REPORT.DTA CHECKED 10-24-96
    STRT=TIMER
    CLS
    CALL LOGON
    LOCATE 12,12:PRINT "ONE MOMENT PLEASE . . . ."
    CALL SETUP              'MODULE 1 SET UP VARIABLES
    CALL TURN             'MODULE 2 RUN TURNS
    CALL CLOSING            'MODULE 3 END OF GAME STUFF
END

'MODULE 0.1 LOGON
SUB LOGON
   SHARED ANSWER$()
   DIM ANSWER$(305)
   INPUT "YOUR NAME-NUMBER PLEASE";ANSWER$(0)
   LET ANSWER$(0)=ANSWER$(0)+"  "+TIME$+"  "+DATE$
END SUB

'MODULE 1 SETUP
SUB SETUP
    CALL ROOMINFO       'MODULE 1.1 READ ROOM INFORMATION
    CALL ROOMOBJECTS    'MODULE 1.2 READ ROOM OBJECTS
    CALL MOVEMENTTABLE  'MODULE 1.3 READ MOVEMENT TABLE
    CALL INVENTORY      'MODULE 1.4 INITIALIZE INVENTORY ARRAY
                        '             TO AN EMPTY LIST
    CALL OTHERS         'MODULE 1.5 INITIALIZE OTHER VARIABLES
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
    DATA 21,FOOD LOCKER,SHIP'S LAUNDRY,MAIN DECK,CODE ROOM,OPEN SEA
    DATA ARMORY,SANDY BEACH,ROCKY BEACH,TIDE POOL,UPPER BEACH
    DATA WOODS,HILLSIDE,HILLTOP,PIRATE'S FORT,HUT,CAMPFIRE AREA,STOCKADE
    DATA PASSAGEWAY,SICK BAY,BRIG,TOOL ROOM

    FOR I=1 TO NUMBERROOMS
       LET DESCRIBEFLAG(I)=0
    NEXT I
    FOR I=1 TO NUMBERROOMS
       READ ROOMDESCRIBE$(I)
       READ ROOMDES2$(I)
    NEXT I
    DATA "You are in the ship's FOOD LOCKER.  There is some fruit"
    DATA "here, but you will have to  REVEAL  it by name."
    DATA "As you might expect, the laundry room is a mess.  The "
    DATA "PIRATES are lucky their moms aren't here to see it."
    DATA "From the deck of the  BASE CANARD,  you can see "
    DATA "PIRATE ISLAND a short distance to the NORTH."
    DATA "Here in the ship's CODE ROOM, secret coded messages"
    DATA "are enciphered and deciphered."
    DATA "The SEA WATER isn't cold and you can see a SANDY"
    DATA "BEACH just to the NORTH."

    DATA "Here in the ARMORY is where the PIRATES keep their weapons."
    DATA "It looks as if they've taken all but one of them away."
    DATA "You are on the BEACH of a beautiful, tropical island."
    DATA "The warm sun quickly dries your clothes."
    DATA "In this part of the BEACH, waves crash over the rocks."
    DATA "For a moment, you forget the danger you are in."
    DATA "There are FISH in the TIDE POOL."
    DATA "You'll need to try your hand at spear-fishing."
    DATA "Here at the UPPER BEACH, The sand is firmer."
    DATA "To the EAST you can see WOODS."

    DATA "The WOODED AREA you are in is quite small."
    DATA "The trees are tall, with long, straight limbs."
    DATA "You are on the side of the island's one hill."
    DATA "The path is steep and hasn't been used much."
    DATA "From the top of the hill, you see the ship to the SOUTH.  Your"
    DATA "attention, however, is focused on the poor fellow before you."
    DATA "The gate of the PIRATE'S FORT is wide open.  You sense "
    DATA "danger and hope that you are dressed appropriately."
    DATA "The PIRATE'S rude HUT is also messy."
    DATA "The PIRATES need to learn more self-discipline."
    DATA "The fearsome-looking PIRATES are standing around the "
    DATA "COALS of a once-roaring campfire.  BE CAREFUL!"

    DATA "The prisoners locked inside the GATE are glad to see you at first."
    DATA "But when you don't  UNLOCK the GATE, they move to the far side."
    DATA "You are in the dark, dank ship's PASSAGEWAY."
    DATA "You can see light at the top of the stairs."
    DATA "Here in the SICK BAY, you can find something to take for the"
    DATA "malarial fever you picked up in the tropics."
    DATA "You find yourself tied up with ropes in the BRIG of a PIRATE"
    DATA "SHIP.  The foul odor suggests deferred maintenance."
    DATA "The TOOL ROOM is empty except for one tool left behind.  You"
    DATA "will have to unscramble the name of the tool and  REVEAL  it."


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
    DATA 3,0,0,0,0,0,0,0,3,0,0,0
    DATA 5,1,4,2,0,18,0,0,0,3,0,0
    DATA 7,3,0,0,0,0,0,18,0,0,0,0
    DATA 10,5,8,0,0,0,11,0,9,7,0,0
    DATA 0,0,0,8,0,0,14,7,11,0,0,0
    DATA 0,8,0,10,12,0,0,0,0,0,13,11
    DATA 0,0,0,0,0,12,17,10,15,16,0,0
    DATA 0,0,0,14,0,0,0,0,14,0,0,0
    DATA 0,14,0,0,0,0
    DATA 6,21,19,20,3,0,0,0,0,18,0,0
    DATA 0,0,18,0,0,0,18,0,0,0,0,0
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
        FLAG(),OBJ$(),OBJ2$()
    LET ROOM = 20                'START IN THE BRIG- ROOM 20
    LET TURNNUMBER =0
    LET THING$=""
    DIM FLAG(30)
    FOR I=1 TO 10:LET FLAG(I)=0:NEXT I      'SET FLAGS TO 0
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

    LET ROOMOBJECT$(1,1)="SOMETHING*"  'ORANGES
    LET ROOMOBJECT$(1,2)="SIGN*"
    LET ROOMOBJECT$(2,1)="SOMETHING*"  'CLOTHES
    LET ROOMOBJECT$(2,2)="SIGN*"
    LET ROOMOBJECT$(3,1)="CANNON*"     'STARTING LOCATIONS FOR OBJECTS
    LET ROOMOBJECT$(4,1)="SIGN*"
    LET ROOMOBJECT$(6,1)="SOMETHING*"  'SWORD
    LET ROOMOBJECT$(6,2)="SIGN*"
    LET ROOMOBJECT$(9,1)="FISH"
    LET ROOMOBJECT$(11,1)="CNBAHR*"    'BRANCH
    LET ROOMOBJECT$(13,1)="A SICK AND HUNGRY HERMIT*"
    LET ROOMOBJECT$(15,1)="GUNBOX*"
    LET ROOMOBJECT$(15,2)="EKY*"       'KEY
    LET ROOMOBJECT$(16,1)="PIRATES*"
    LET ROOMOBJECT$(16,2)="COALS"
    LET ROOMOBJECT$(17,1)="PRISONERS*"
    LET ROOMOBJECT$(17,2)="LOCK*"
    LET ROOMOBJECT$(18,1)="SIGN*"
    LET ROOMOBJECT$(19,1)="SOMETHING*"  'QUININE
    LET ROOMOBJECT$(19,2)="SIGN*"
    LET ROOMOBJECT$(20,1)="SOMETHING*"  'RAT
    LET ROOMOBJECT$(20,2)="SIGN*"
    LET ROOMOBJECT$(21,1)="VLHSOE*"     'SHOVEL

    READ NUMBERCONVERTS
    DIM OBJ$(NUMBERCONVERTS),OBJ2$(NUMBERCONVERTS)
    FOR I=1 TO NUMBERCONVERTS     'READ IN WORD!* CONVERSIONS
        READ OBJ$(I),OBJ2$(I)
    NEXT I
    DATA 12,SIGN,SIGN*,GUNBOX,GUNBOX*,BOX,GUNBOX*,PIRATES,PIRATES*
    DATA PIRATE,PIRATES*,PRISONER,PRISONERS*,PRISONERS,PRISONERS*
    DATA LOCK,LOCK*,RAT,RAT*,ORANGE,ORANGES,SOMETHING,SOMETHING*
    DATA CANNON,CANNON*
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
           PRINT "PIRATE (at the DOS prompt) and later,":_
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
        ROOMDES2$()

    IF TURNNUMBER=1 THEN CALL GAMESTARTER   'MODULE 2.1.1
  SELECT CASE DESCRIBEFLAG(ROOM)
   CASE 0
    PRINT ROOMDESCRIBE$(ROOM)
    PRINT ROOMDES2$(ROOM)
    DESCRIBEFLAG(ROOM)=1:EXIT SELECT
  CASE 1
    SELECT CASE DESCRIPTION$(ROOM)
        CASE "MAIN DECK","SANDY BEACH","ROCKY BEACH","UPPER BEACH"
           PRINT "You find yourself on the ";DESCRIPTION$(ROOM)
        CASE "OPEN SEA","BRIG","FOOD LOCKER","CODE ROOM","WOODS","HUT",_
            "PASSAGEWAY","SICK BAY","TOOL ROOM"
            PRINT "You find yourself in the ";DESCRIPTION$(ROOM)
        CASE ELSE
           PRINT "You find yourself at the ";DESCRIPTION$(ROOM)
    END SELECT
  END SELECT
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
    IF FLAG(9)=1 AND ROOM=16 THEN FLAG(9)=2
    IF ROOM=3 AND FLAG(10)=0 THEN ROOM=18:CLS:PRINT:PRINT:PRINT: _
      PRINT "You are too weak with malaria to climb the stairs.":_
      FLAG(14)=1:DESCRIBEFLAG(3)=0:EXIT SUB
   IF ROOM=18 AND FLAG(11)=0 THEN ROOM=20:CLS:PRINT:PRINT:PRINT: _
      PRINT "You can't move very well while tied up.":FLAG(14)=1:_
      DESCRIBEFLAG(18)=0:EXIT SUB



END SUB

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
SUB GAMESTARTER
     CLS
     PRINT
     PRINT "                WELCOME TO PIRATE ISLAND"
     PRINT
     PRINT "Are you ready for this: PIRATES, danger, secret codes,"
     PRINT "spear-fishing?  Good!  I thought you would be!"
     PRINT "Give COMMANDS as VERB then OBJECT, such as GO NORTH,"
     PRINT "SAVE GAME, RESTORE GAME, READ SIGN, TAKE FISH,"
     PRINT "AND REVEAL APPLES."
     PRINT "Exceptions to this two-word sentence rule are single-letter"
     PRINT "COMMANDS such as N to GO NORTH (not GO N, just N), U to GO UP,"
     PRINT "and D to GO DOWN."
     PRINT "Don't try to GET OBJECTS ending in an *, e.g., CANNON*,"
     PRINT "as they are quite unobtainable.  Sometimes the names of"
     PRINT "OBJECTS will be scrambled, e.g., APPLES may appear as "
     PRINT "PELSPA*.   To GET the APPLES, type REVEAL APPLES and then"
     PRINT "TYPE  GET APPLES.  Sometimes the presence of an object "
     PRINT "will be indicated by SOMETHING* and you must guess and"
     PRINT "REVEAL what it is before you can GET it.  For the word "
     PRINT "That 'starts with a Q and ends with an E', You may"
     PRINT "need to consult a dictionary."
     PRINT "If you get stuck, keep trying different things because"
     PRINT "after a certain number of turns, a hint will show up."
     PRINT "Happy Adventuring!   Press ENTER to begin."
     DO WHILE LEN(INKEY$)=0:LOOP
     CLS
END SUB

'MODULE 2.1.2 ERASER ERASES ONE LINE OF TEXT
SUB ERASER
 REM FOR I=1 TO 12:PRINT "          ";:NEXT I
END SUB

'MODULE 2.2 COMMANDS
SUB COMMANDS
    SHARED VERB$, OBJECT$,ROOM,DESCRIPTION$(),FLAG(),INVENTORY$(),_
        ROOMOBJECT$(),OBJ$(),OBJ2$(),C$,ANSWER$(),TURNNUMBER

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

    LET C$=UCASE$(C$)

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
    IF COUNTER<>0 THEN PRINT "One space (two words) only, please." _
      ELSE DUMMY2=1

 LOOP
END SUB


'MODULE 2.3 EVALUATE THE COMMANDS
SUB EVALUATE
    SHARED VERB$, OBJECT$,ROOM,DIRECTION,MOVEMENTTABLE(),NUMBERROOMS,_
        ROOMOBJECT$(),ITEMNUMBER,TURNNUMBER,THING$,INVENTORY$(),FLAG()
    IF FLAG(1)<>0 THEN EXIT SUB
    IF FLAG(14)=1 THEN EXIT SUB
    SELECT CASE VERB$
        CASE "QUIT"
            LET FLAG(1)=2   'GAME NOT WON(1) OR LOST(-1):EXIT SELECT
        CASE "SAVE"
            IF OBJECT$<>"GAME" THEN PRINT "SAVE GAME":EXIT SELECT
            OPEN "PIRESAV.BAS" FOR OUTPUT AS #1
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
            CLOSE #1
            PRINT "OK
            EXIT SELECT
        CASE "RESTORE", "LOAD"
            IF OBJECT$="CANNON" THEN PRINT "IT'S LOADED!":EXIT SELECT
            IF OBJECT$<> "GAME" THEN PRINT "RESTORE GAME":EXIT SELECT
            OPEN "PIRESAV.BAS" FOR INPUT AS #1
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
            CLOSE #1
            PRINT "OK
            EXIT SELECT

        CASE "INVENTORY","I"
            COUNTER=0
            PRINT "You carry:  ";
            FOR I= 1 TO 5
                IF INVENTORY$(I)<>"EMPTY" THEN COUNTER=COUNTER + 1:_
                    PRINT INVENTORY$(I);"   ";
            NEXT I
            IF COUNTER = 0 THEN PRINT "nothing at all" ELSE PRINT
        CASE "GO","MOVE","N","S","E","W","U","D"
            IF LEN (VERB$)=1 THEN OBJECT$=VERB$
            IF OBJECT$="NORTH" OR OBJECT$="N" THEN DIRECTION = 1
            IF OBJECT$="SOUTH" OR OBJECT$="S" THEN DIRECTION = 2
            IF OBJECT$="EAST"  OR OBJECT$="E" THEN DIRECTION = 3
            IF OBJECT$="WEST"  OR OBJECT$="W" THEN DIRECTION = 4
            IF OBJECT$="UP"    OR OBJECT$="U" THEN DIRECTION = 5
            IF OBJECT$="DOWN"  OR OBJECT$="D" THEN DIRECTION = 6
            IF MID$(OBJECT$,1,4)="PORT" AND ROOM = 6 THEN DIRECTION = 4
            IF MOVEMENTTABLE (ROOM,DIRECTION) = 0 THEN _
                PRINT "You can't GO that way" ELSE _
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
        CASE "GET","TAKE"
            IF OBJECT$="QUININE" AND FLAG(10)=1 THEN _
               PRINT "That should be enough.":EXIT SELECT
            IF  FNPRESENT = 0 THEN PRINT "I don't see  ";OBJECT$:EXIT SELECT
            IF RIGHT$(OBJECT$,1)="*" THEN _
               PRINT "I can't handle it.":EXIT SELECT
            IF OBJECT$="QUININE" AND FNPRESENT=1 THEN FLAG(10)=1
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
            FOR I=1 TO 5
                IF INVENTORY$(I)="EMPTY" THEN INVENTORY$(I)=OBJECT$:_
                    PRINT "GOT IT":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY":_
                    EXIT SELECT
            NEXT I
            PRINT "You're carrying too much.  DROP something!"
        CASE "PUT","DROP","GIVE"
            THING$=OBJECT$
            IF FNCARRY=0 THEN PRINT "You don't have the ";OBJECT$:EXIT SELECT
            IF OBJECT$="FISH" AND ROOM=13 AND FLAG(7)=0 THEN _
               PRINT "Cure the scurby first.":EXIT SELECT
            IF OBJECT$="FISH" AND FLAG(5)=0 THEN PRINT _
                "COOK it first":EXIT SELECT
            IF OBJECT$="ORANGES" AND ROOM = 13 AND FLAG(7)=0 THEN FLAG(7)=1:__
               PRINT "OK":ROOMOBJECT$(13,1)="A still hungry HERMIT*" _
               :EXIT SELECT
            IF OBJECT$="FISH" AND ROOM=13 AND FLAG(8)=0 THEN_
               FLAG(8)=1:ROOMOBJECT$(16,1)="EMPTY":_
               PRINT "The grateful HERMIT leaves to fire the CANNON ":_
               PRINT "in order to lure the PIRATES to the SHIP.":_
               ROOMOBJECT$(13,1)="EMPTY":EXIT SELECT

            FOR J=1 TO 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":EXIT SELECT
            NEXT J
            PRINT "This location is full, take it elsewhere."
        CASE "FEED"
            PRINT "Try:   GIVE FISH":EXIT SELECT
        CASE "KILL"
            PRINT "Who do you think you are, Rambo?":EXIT SELECT
        CASE "WEAR","CHANGE"
            THING$=OBJECT$
            IF OBJECT$="CLOTHES" AND FNCARRY = 1 THEN FLAG(2)=1:_
               PRINT "Now you look somewhat like a PIRATE.":EXIT SELECT
            IF OBJECT$="CLOTHES" THEN PRINT "GET CLOTHES first":EXIT SELECT
            PRINT "Didn't help.  Try something different.":EXIT SELECT
        CASE "CUT","TRIM","SHARPEN","CARVE"
            THING$=OBJECT$
            IF FNCARRY = 0 THEN PRINT "You don't have the BRANCH.":EXIT SELECT
            THING$="SWORD"
            IF FNCARRY= 0 THEN PRINT "You'll need a cutting instrument.":EXIT SELECT
            THING$=OBJECT$
            IF FNCARRY =1 THEN LET INVENTORY$(ITEMNUMBER)="SPEAR":_
                PRINT "You now have a fine SPEAR.":EXIT SELECT


        CASE "READ"
            IF OBJECT$<>"SIGN*" THEN PRINT "I only READ SIGNS*":EXIT SELECT
            PRINT "It says:"
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
            PRINT
        CASE "REVEAL"
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
           IF OBJECT$="KNIFE" THEN PRINT "Ouch! Better not.":EXIT SELECT
           PRINT "Nothing happened.  Try something else.":EXIT SELECT

        CASE "COOK","HEAT","WARM","ROAST"
            THING$=OBJECT$
            IF FNCARRY=0 THEN PRINT "You don't have the fish":EXIT SELECT
            THING$="COALS"
            IF FNCARRY=0 THEN PRINT "You'll need to GET the COALS":EXIT SELECT
            LET FLAG(5)=1:PRINT "OK":EXIT SELECT
        CASE "UNLOCK","OPEN"
            SELECT CASE OBJECT$
                CASE "BOX","GUNBOX"
                    PRINT "You'll need a crowbar to OPEN it":EXIT SELECT
                CASE "LOCK","GATE","DOOR"
                    THING$="KEY"
                    IF FNCARRY=0 THEN PRINT "You'll need the KEY"_
                        :EXIT SELECT
                    IF FLAG (8)=0 THEN PRINT "The PIRATES recapture you as you";_
                        :PRINT "try to UNLOCK the GATE":ROOM=20:_
                        EXIT SELECT
                    FLAG(1)=1:PRINT "OK":EXIT SELECT
            END SELECT
        CASE "CLIMB"
             IF LEFT$(OBJECT$,4)="TREE" THEN PRINT "Too difficult.":EXIT SELECT
             PRINT "Try:    GO UP.":EXIT SELECT
        CASE "CALL","NAME","SAY"
             PRINT "Just type the name you were going to CALL.":EXIT SELECT
        CASE "FISH","CATCH"
            PRINT "Try:    GET FISH":EXIT SELECT
        CASE "EAT","CONSUME"
            PRINT "Save the food for the hermit.":EXIT SELECT
        CASE "BILL"
            IF ROOM=11 THEN FLAG(19)=1:_
               PRINT "The PARROT flies over and affectionately bites your nose.":_
               EXIT SELECT
            PRINT "Not here":EXIT SELECT
        CASE "POLLY"
           PRINT "The male PARROT is quite offended.":EXIT SELECT
        CASE "USE"
           PRINT "To do what?  Be more specific.":EXIT SELECT
        CASE ELSE
            PRINT "I don't know that verb":EXIT SELECT
    END  SELECT
END SUB

'MODULE 2.3.1 DEFINE THE FUNCTION- IS THE OBJECT PRESENT?
DEF FNPRESENT
         FOR J=1 TO 15
             IF ROOMOBJECT$(ROOM,J)=OBJECT$ THEN _
                 FNPRESENT=1:ITEMNUMBER=J:EXIT DEF
         NEXT J
         FNPRESENT=0
END DEF

'MODULE 2.3.2 DEFINE THE FUNCTION- IS IT BEING CARRIED?
DEF FNCARRY
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
    SHARED FLAG(),TURNNUMBER,ANSWER$(),STRT
    IF FLAG(1)=1 THEN PRINT "One of the prisoners has a crowbar and GETs the guns."
    IF FLAG(1)=1 THEN PRINT "Fully armed, the goodguys capture the PIRATES. (Thanks to you!)
    IF FLAG(1)=1 THEN PRINT "Amidst cheers and adulation, you know you've won the PIRATE adventure!"
FOR I=1 TO 6:    PRINT:NEXT I
    INPUT "Be sure that your disk is in the drive and press ENTER ";DUMMY$

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
    LET DTA(9)=FLAG(1):LET DTA(34)=DTA(34)+INT((TIMER-STRT)/6)

       OPEN "REPORT.DTA" FOR OUTPUT AS #1
         REM SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
         FOR I=1 TO 40
           WRITE #1,DTA(I)
         NEXT I
         FOR I=1 TO 10
           WRITE #1,DTA$(I)
         NEXT I
       CLOSE #1

    OPEN "PIRTDATA.TXT" FOR APPEND AS #2
    PRINT #2, DATE$,TIME$
    FOR I=0 TO TURNNUMBER
        IF INT(I/5)=I/5 THEN PRINT #2,ANSWER$(I) ELSE PRINT #2,ANSWER$(I),
    NEXT I
    PRINT #2,INT((STRT-TIMER)/6),INT(1000*RND(0))
    CLOSE #2
    PRINT "This game is over.  Type PIRATE to play again."
END SUB


'END OF PROGRAM
