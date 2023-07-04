'MODULE 0 MAIN    ******HANGTOWN******  10-3-90
'MULTIPLE REVISIONS ON 1-25-91
'GOLD HINT BURN HINT KEY HINT REVEAL HINT RESTORE 30 6MAR92
'TIMER AND STRT CHANGE  2-3-94
'TIME$, DATE$, AND SAVE GAME FIX 7-30-95
'SNIp fixes 1-3-96
'REPORT STUFF 3-5-96  HANGDATA.TXT 10-24-96

    CLS:DIM FLAG(30),OBJ$(18),OBJ2$(18)
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
   DIM ANSWER$(305)
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
    CALL OTHERS	            'MODULE 1.5 INITIALIZE OTHER VARIABLES
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
    DATA 33, LEDGE just inside WELL*, RATTLESNAKE WELLS, HANGTOWN SQUARE, SCAFFOLD, SANDY DESERT
    DATA LAST CHANCE SALOON, STREET, HARDROCK JAIL, STEEP'S TEA ROOM, STREET, GOLDEN WELL* HOTEL
    DATA DOCTOR'S OFFICE, STREET, LIVERY STABLE, PASTURE, STREET, LIMPING HORSE CORRAL
    DATA MINER'S SHACK, STREET, FIELD, TOP OF BOULDER, CELLAR, MINESHAFT, MINESHAFT
    DATA MINESHAFT, MINESHAFT, MINESHAFT, MINESHAFT, MINESHAFT, MINESHAFT, MINESHAFT
    DATA MINESHAFT, RANGE

    FOR I=1 TO NUMBERROOMS
       LET DESCRIBEFLAG(I)=1
       LET ROOMDESCRIBE$(I)="EMPTY"
       LET ROOMDES2$(I)="EMPTY"
    NEXT I
    FOR I=1 TO 15
       READ DESCRIBEROOM
       LET DESCRIBEFLAG(DESCRIBEROOM)=0
       READ ROOMDESCRIBE$(DESCRIBEROOM)
       READ ROOMDES2$(DESCRIBEROOM)
    NEXT I
    DATA 2,"Your blood chills with fear at the sight of the huge and horrible"
    DATA "RATTLESNAKE guarding the WELL.  You know that one of you must die!"
    DATA 3,"Curiously, you find yourself in the TOWN SQUARE of a dry, dusty,"
    DATA "apparently deserted gold rush town."
    DATA 4,"You stand at the top of a gruesome SCAFFOLD.  The hangperson"
    DATA "left his/her well-worn noose here. Is this a PC PC or what?"
    DATA 5,"The DESERT seems to stretch out forever to the EAST."
    DATA "It's hard to walk in the soft sand."
    DATA 6,"As you enter the aroma that greets you isn't the stale smoke and "
    DATA "whiskey smell you expected.  You feel magic in the air here."
    DATA 7,"From the STREET you can see the town JAIL to the EAST.      "
    DATA "You wonder why the JAIL door was left locked."
    DATA 9,"An elegantly-dressed but crabby-looking ENGLISHMAN looks at you as"
    DATA "you enter the TEA ROOM, then turns away.  He needs a nice CUP of TEA."
   DATA 10,"All you can see to the SOUTH are hoof prints and wagon ruts,"
    DATA "frozen forever in the hard-baked dirt of the town STREET."
   DATA 11,"The curiously named HOTEL looks pretty run down.  "
    DATA "Hopefully, you won't have to spend the night here."
   DATA 12,"You can almost hear the elevator music in the DOCTOR'S OFFICE. "
    DATA "You see all of the earliest issues of INVESTMENT MAGAZINE."
   DATA 13,"The ROCK you see in the STREET could do a lot of damage if"
    DATA "thrown accurately."
   DATA 14,"With HAY on the floor, the LIVERY STABLE looks more comfortable"
    DATA "than the HOTEL.  There is a small milk-pail here, but no COW."
   DATA 15,"Here in the PASTURE is the most pathetic COW you've ever seen. "
    DATA "GET her some food before it's too late!"
   DATA 17,"Here in the LIMPING HORSE CORRAL you see some SUGAR cubes that"
    DATA "are still in good condition.  Horses love 'em."
   DATA 20,"Here in the FIELD is a huge BOULDER with sheer walls.  To"
    DATA "CLIMB it, You'll need something to stand upon."

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
    DATA 0,0,0,0,2,0,0,0,3,0,0,0,0,7,5,2,4,0
    DATA 0,0,0,0,0,3,0,0,5,3,0,0,0,0,7,0,0,0
    DATA 3,10,8,6,0,0,0,0,0,7,0,0,0,0,10,0,0,0
    DATA 7,13,11,9,0,0,0,0,0,10,0,0,0,0,13,0,0,0
    DATA 10,16,14,12,0,0,0,0,0,13,0,0,0,0,16,0,0,0
    DATA 13,19,17,15,0,0,0,0,0,16,0,0,0,0,19,0,0,0
    DATA 16,33,20,18,0,0,0,0,0,19,0,0,0,0,0,0,0,20
    DATA 0,0,0,0,18,25,0,27,24,0,0,0,0,28,25,23,0,0
    DATA 0,29,26,24,22,0,0,30,0,25,0,0,23,31,28,0,0,0
    DATA 24,0,28,27,0,0,25,32,30,28,0,0,26,0,0,29,0,0
    DATA 27,0,0,0,0,0,29,0,0,0,0,0,19,0,0,0,0,0

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
        CONVERTNUM
    LET ROOM = 3               'START IN HANGTOWN SQUARE
    LET TURNNUMBER =0
    DANGEROUS=1
    LET THING$=""
    FOR I=1 TO 30:LET FLAG(I)=0:NEXT I      'SET FLAGS TO 0
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
    LET ROOMOBJECT$(1,1)="GOLD!"         'STARTING LOCATIONS FOR OBJECTS
    LET ROOMOBJECT$(2,1)="WELL*"
    LET ROOMOBJECT$(2,2)="SNAKE"
    LET ROOMOBJECT$(2,3)="BUCKET"
    LET ROOMOBJECT$(3,1)="SIGN*"
    LET ROOMOBJECT$(4,1)="ROPE"
    LET ROOMOBJECT$(5,1)="EMPTYKEY"
    LET ROOMOBJECT$(6,1)="KETTLE"
    LET ROOMOBJECT$(6,2)="SIGN*"
    LET ROOMOBJECT$(6,3)="EMPTYMIRROR*"
    LET ROOMOBJECT$(6,4)="EMPTYSILVER!"
    LET ROOMOBJECT$(7,1)="DOOR*"
    LET ROOMOBJECT$(8,1)="CUP"
    LET ROOMOBJECT$(9,1)="STOVE*"
    LET ROOMOBJECT$(9,2)="SIGN*"
    LET ROOMOBJECT$(9,4)="CHAIR"
    LET ROOMOBJECT$(9,5)="EMPTYEMERALD!"
    LET ROOMOBJECT$(10,1)="EMPTYRUBY!"
    LET ROOMOBJECT$(11,1)="SAFE*"
    LET ROOMOBJECT$(11,2)="EMPTYJADE!"
    LET ROOMOBJECT$(11,3)="MATCHES"
    LET ROOMOBJECT$(12,1)="MAGAZINES"
    LET ROOMOBJECT$(12,2)="TWEEZERS"
    LET ROOMOBJECT$(12,3)="MINERAL-OIL"
    LET ROOMOBJECT$(13,1)="ROCK"
    LET ROOMOBJECT$(14,1)="SHOVEL"
    LET ROOMOBJECT$(14,2)="HAY"
    LET ROOMOBJECT$(14,3)="PAIL"
    LET ROOMOBJECT$(15,1)="COW*"
    LET ROOMOBJECT$(17,1)="SUGAR"
    LET ROOMOBJECT$(18,1)="TEA"
    LET ROOMOBJECT$(18,2)="CELLAR*"
    LET ROOMOBJECT$(20,1)="BOULDER*"
    LET ROOMOBJECT$(21,1)="MAP"
    LET ROOMOBJECT$(22,1)="LANTERN"
    LET ROOMOBJECT$(31,1)="EMPTYDIAMONDS!"
    LET ROOMOBJECT$(33,1)="EMPTYTURQUOISE!"

    LET CONVERTNUM=18
    FOR I=1 TO CONVERTNUM              'READ IN WORD!* CONVERSIONS
        READ OBJ$(I),OBJ2$(I)
    NEXT I
    DATA GOLD,GOLD!,WELL,WELL*,SIGN,SIGN*,DOOR,DOOR*,STOVE,STOVE*,EMERALD,_
        EMERALD!,RUBY,RUBY!,DIAMONDS,DIAMONDS!,COW,COW*,CELLAR,CELLAR*,_
        BOULDER,BOULDER*,JADE,JADE!,SILVER,SILVER!,TURQUOISE,TURQUOISE!,_
        MINERAL,MINERAL-OIL,OIL,MINERAL-OIL,SAFE,SAFE*,MIRROR,MIRROR*

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
           PRINT "HANGTOWN (at the DOS prompt) and later,":_
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
    IF DESCRIBEFLAG(ROOM)=0 THEN PRINT ROOMDESCRIBE$(ROOM):_
       PRINT ROOMDES2$(ROOM):DESCRIBEFLAG(ROOM)=1:GOTO JUMPOVER
    IF DESCRIBEFLAG(ROOM)=1 AND DESCRIPTION$(ROOM)="STREET" THEN _
        PRINT "You find yourself in the STREET"
    IF DESCRIBEFLAG(ROOM)=1 AND DESCRIPTION$(ROOM)<>"STREET" THEN _
        PRINT "You find yourself at the ";:PRINT DESCRIPTION$(ROOM)
    JUMPOVER:     'HOPELESS KLUDGE I MUST BE GETTING TIRED
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
    OBJECT$="SNAKE"
        IF FLAG(21)=0 AND TURNNUMBER>90 AND FLAG(2)=0 AND FNPRESENT=1 THEN_
          PRINT "A coyote strolls by, sees the dead SNAKE and runs away":_
          FLAG(21)=1
        IF FLAG(24)=0 AND FLAG(21)=1 AND DESCRIPTION$(ROOM)="STREET" AND _
          TURNNUMBER>120 THEN  FLAG(24)=1:__
          PRINT "A tourist tells you his dog ran off after seeing a dead SNAKE."

    IF ROOM =22 AND FLAG(20)=0 THEN PRINT "The glowing LANTERN is quite bright":_
        FLAG(20)=1

    IF ROOM = 10 AND FLAG(4)=1 AND FLAG(8)=0 THEN PRINT_
       "There is a shy and hungry KITTEN here"
    IF ROOM = 9 AND FLAG(4)=0 THEN PRINT "There is an unhappy ENGLISHMAN* here"
    OBJECT$="SUGAR"
       IF ROOM=33 AND FLAG(4)=1 AND FNPRESENT = 0 AND FLAG(9)=0 THEN _
          PRINT "You see a limping HORSE. You'll need a bribe to lure him over."
       IF ROOM = 33 AND FLAG(4)=1 AND FNPRESENT = 1 AND FLAG(9)=0 THEN_
          PRINT "The HORSE with a THORN in his hoof is nibbling on the SUGAR"
    THING$="LANTERN"
       IF ROOM=23 OR ROOM=24 OR ROOM=25 OR ROOM=26 OR ROOM=27 OR ROOM=28_
          OR ROOM=29 OR ROOM=30 OR ROOM=31 OR ROOM=32 THEN FLAG(19)=1 ELSE FLAG(19)=0
       IF FLAG(19)=1 AND FNCARRY=0 THEN PRINT "Too dark -- Dangerous!":_
           DANGEROUS=DANGEROUS + 1
       IF DANGEROUS=8 THEN PRINT "You bumped your head in the dark": _
          FLAG(1)=-1:EXIT SUB
    IF ROOM=15 AND FLAG(5)=0 THEN PRINT "There's a mighty hungry COW here"
    IF ROOM=15 AND FLAG(5)=1 THEN PRINT "There is a contented COW here"
    THING$="SNAKE": IF ROOM =18 AND FLAG(2)=0 AND FNCARRY=0 THEN CLS:PRINT:PRINT:_
       PRINT "A vicious DOG chases you out of the shack and back into the STREET":_
       FLAG(14)=1:ROOM=19:EXIT SUB
    IF ROOM=18 AND FLAG(2)=0 AND FNCARRY=1 THEN FLAG(2)=1:PRINT _
       "The DOG was frightened off by the dead RATTLESNAKE"
    IF TURNNUMBER=150 AND FLAG(17)=0 AND FLAG(26)=0 THEN FLAG(26)=1:_
       PRINT "The JAIL door KEY is buried somewhere."
    IF SCORE=86 AND FLAG(25)=0 THEN FLAG(25)=1:_
       PRINT "A stranger asks you the name of the HOTEL."
    PRINT
END SUB

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
SUB GAMESTARTER
     CLS
     PRINT
     PRINT "                WELCOME TO HANGTOWN"
     PRINT
     PRINT
     PRINT "Come away with me on an exciting adventure to "
     PRINT "HANGTOWN, home of the GOLD rush!  Cope with "
     PRINT "mad dogs and ENGLISHMEN!  Save endangered animals!"
     PRINT "GET TREASURES!     MAKE TEA!"
     PRINT
     PRINT "Give COMMANDs as verb then object, such as  GO NORTH,"
     PRINT "SAVE GAME, RESTORE GAME, READ SIGN, KILL SNAKE,"
     PRINT "and GO CELLAR."
     PRINT
     PRINT "Exceptions to this two-word sentence rule are single-letter"
     PRINT "COMMANDs such as  N to GO NORTH, U to GO UP, AND D to GO"
     PRINT "DOWN."
     PRINT
     PRINT "P.S. Don't try to GET objects ending in an *, e.g., BOULDER*,"
     PRINT "as they are quite unobtainable-- comprendez, podner?"
     PRINT
     PRINT "And if you get stuck, just keep trying things, because"
     PRINT "After a certain number of turns, a hint will show up."
     PRINT "Press ENTER to begin"
     DO WHILE LEN(INKEY$)=0:LOOP
     CLS
END SUB

'MODULE 2.1.2 ERASER ERASES ONE LINE OF TEXT
SUB ERASER
    REM    FOR I=1 TO 12:PRINT "          ";:NEXT I
END SUB

'MODULE 2.2 COMMANDS
SUB COMMANDS
    SHARED VERB$, OBJECT$,ROOM,DESCRIPTION$(),FLAG(),INVENTORY$(),_
        ROOMOBJECT$(),OBJ$(),OBJ2$(),C$,ANSWER$(),TURNNUMBER,CONVERTNUM

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
    FOR J=1 TO 18                          'CONVERT OBJECT$ LACKING * OR !
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
            IF OBJECT$<>"GAME" THEN PRINT "TRY-  SAVE GAME":EXIT SELECT
            OPEN "HANGSAV.BAS" FOR OUTPUT AS #1

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
            IF OBJECT$<> "GAME" THEN PRINT "RESTORE GAME":EXIT SELECT
            OPEN "HANGSAV.BAS" FOR INPUT AS #1

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
            PRINT "   You carry: ";
            FOR I= 1 TO 5
                IF INVENTORY$(I)<>"EMPTY" THEN COUNTER=COUNTER + 1:_
                    PRINT INVENTORY$(I);"   ";
            NEXT I
            IF COUNTER = 0 THEN PRINT "nothing at all" :EXIT SELECT
            PRINT:EXIT SELECT

        CASE "GO","MOVE","N","S","E","W","U","D"
            IF LEN (VERB$)=1 THEN OBJECT$=VERB$
            IF OBJECT$="NORTH" OR OBJECT$="N" THEN DIRECTION = 1
            IF OBJECT$="SOUTH" OR OBJECT$="S" THEN DIRECTION = 2
            IF OBJECT$="EAST"  OR OBJECT$="E" THEN DIRECTION = 3
            IF OBJECT$="WEST"  OR OBJECT$="W" THEN DIRECTION = 4
            IF OBJECT$="UP"    OR OBJECT$="U" THEN DIRECTION = 5
            IF OBJECT$="DOWN"  OR OBJECT$="D" THEN DIRECTION = 6
            IF OBJECT$="WELL*" AND FLAG(3)=0 THEN_
               PRINT "SNAKE won't let me":EXIT SELECT
            IF ROOM=2 AND OBJECT$="WELL*" AND FLAG(3)=1 THEN PRINT "OK":_
               ROOM=1:EXIT SELECT
            IF ROOM=7 AND FLAG(6)=0 AND DIRECTION = 3 THEN_
               PRINT "Can't enter JAIL - door is locked":EXIT SELECT
            IF ROOM=18 AND OBJECT$="CELLAR*" THEN PRINT "OK":ROOM=22:EXIT SELECT
            IF MOVEMENTTABLE (ROOM,DIRECTION) = 0 THEN _
                PRINT "You can't GO that way" ELSE _
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
            EXIT SELECT

        CASE "GET","TAKE","PLUCK","EXTRACT","PULL","REMOVE"
            IF ROOM=1 AND OBJECT$="WATER" THEN_
                PRINT "Too dangerous from the LEDGE":EXIT SELECT
            IF ROOM=2 AND OBJECT$="WATER" AND FLAG(3)=0 THEN _
               PRINT "The SNAKE won't let me":EXIT SELECT
            THING$="BUCKET"
            IF ROOM = 2 AND OBJECT$="WATER" AND FLAG(3)=1 AND FNCARRY = 0_
               THEN PRINT "You'll need a BUCKET.":EXIT SELECT
            IF ROOM =2 AND FNCARRY = 1 AND OBJECT$="WATER" AND FLAG(12)=0 THEN PRINT_
                "The BUCKET won't reach the WATER.":EXIT SELECT
            IF ROOM = 2 AND OBJECT$="WATER" AND FLAG(3)=1 AND FNCARRY = 1_
               AND FLAG(12)=1 THEN PRINT "OK":_
               FLAG(11)=1:EXIT SELECT
            THING$="PAIL"
               IF FLAG(5)=1 AND ROOM=15 AND FNCARRY=0 AND OBJECT$="MILK" THEN _
                  PRINT "You'll need a PAIL":EXIT SELECT
               IF FLAG(5)=1 AND ROOM = 15 AND OBJECT$="MILK" AND FNCARRY=1 THEN _
                  PRINT "OK":FLAG(23)=1: EXIT SELECT
               IF FLAG(5)=0 AND ROOM=15 AND OBJECT$="MILK" THEN_
                  PRINT "The hungry COW is too nervous":EXIT SELECT
               IF ROOM<>15 AND OBJECT$="MILK" THEN PRINT "Find a COW":EXIT SELECT
            LET THING$="TWEEZERS"
               IF OBJECT$="THORN" AND FNCARRY=0 THEN _
                  PRINT "You'll need something to remove the THORN with.":_
                  EXIT SELECT
               IF ROOM=33 AND FLAG(4)=1 AND FNCARRY=1 AND FLAG(9)=0 AND OBJECT$="THORN" THEN _
                  PRINT "The happy HORSE shakes something from its mane and runs away":_
                  ROOMOBJECT$(33,1)="TURQUOISE!":FLAG(9)=1:EXIT SELECT
            IF OBJECT$="LANTERN" THEN FLAG(7)=1
            IF ROOM=10 AND OBJECT$="KITTEN" THEN _
               PRINT "Too shy - Runs away from you":EXIT SELECT

            LET HOLDING$=OBJECT$
            LET OBJECT$="SUGAR"
            IF ROOM = 33 AND FLAG(4)=1 AND HOLDING$= "HORSE" AND _
               FNPRESENT=0 AND FLAG(9)=0 THEN _
               PRINT "The HORSE won't come to you without a bribe":EXIT SELECT
            LET OBJECT$=HOLDING$

            IF  FNPRESENT = 0 THEN PRINT "I don't see  ";OBJECT$:EXIT SELECT
            IF RIGHT$(OBJECT$,1)="*" THEN PRINT "I can't handle it":EXIT SELECT
            IF OBJECT$="LANTERN" THEN FLAG(7)=1

            IF RIGHT$(OBJECT$,5)="SNAKE" AND FLAG(3)=0 THEN PRINT_
               "I don't handle live SNAKES, thank you very much":EXIT SELECT

            FOR I=1 TO 5
                IF INVENTORY$(I)="EMPTY" THEN INVENTORY$(I)=OBJECT$:_
                    PRINT "GOT IT":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY":_
                    EXIT SELECT
            NEXT I
            PRINT "You're carrying too much.  Drop something!"
        CASE "PUT","DROP","GIVE"
            IF OBJECT$="MILK"  THEN PRINT "Try  DROP PAIL":EXIT SELECT
            THING$=OBJECT$
            IF OBJECT$="PAIL" AND ROOM=10 AND FLAG(8)=0 AND _
                FNCARRY = 1 THEN FLAG(8)=1:_
                PRINT "The now full KITTEN returns with a pack rat.":_
                PRINT "The pack rat drops something and runs off, ":_
                PRINT "with the KITTEN in hot pursuit":_
                ROOMOBJECT$(10,1)="RUBY!":_
                PRINT:EXIT SELECT
            IF OBJECT$="MILK" AND ROOM=9 THEN PRINT "Try  DROP PAIL":EXIT SELECT
            IF OBJECT$="WATER" THEN PRINT "Try  FILL KETTLE ":EXIT SELECT
            THING$=OBJECT$
            IF FNCARRY=0 THEN PRINT "You don't have the ";OBJECT$:EXIT SELECT
            'PREVENTS DROPPING TWO OF ANYTHING ANYWHERE
            IF FNPRESENT=1 THEN PRINT "Not again.":EXIT SELECT
            IF OBJECT$="LANTERN" THEN FLAG(7)=0
            IF OBJECT$="HAY" AND ROOM=15 THEN FLAG(5)=1
            FOR J=1 TO 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":_
                    EXIT SELECT
            NEXT J
            PRINT "This room is full, take it elsewhere":EXIT SELECT
        CASE "LOOK","EXAMINE","INSPECT"
            IF ROOM=2 AND OBJECT$="WELL*" THEN PRINT _
               "I see GOLD!  Maybe I should GO WELL*":EXIT SELECT
            IF ROOM=6 AND OBJECT$="MIRROR*" THEN _
               PRINT "The rest of the sign can now be seen.  It says:":_
               PRINT "      ITEM #2          Sometimes I'm perky,":_
               PRINT "                       Sometimes I'm a drip;":_
               PRINT "                       When I'm freshly brewed,":_
               PRINT "                       Would you like a sip?":_
               PRINT:EXIT SELECT
            IF OBJECT$="SIGN*" THEN PRINT "Try  READ SIGN*  ":EXIT SELECT
            IF OBJECT$="MAP" THEN PRINT "Try -   READ MAP":EXIT SELECT
            PRINT "I don't see anything unexpected":EXIT SELECT
        CASE "READ"
            IF ROOM=9 AND OBJECT$="SIGN*" THEN PRINT "To MAKE TEA --":_
                PRINT "Bring here and DROP the following -":_
                PRINT "TEA, KETTLE, WATER, MILK, MATCHES, SUGAR, ":_
                PRINT "CUP, and something to burn, then type MAKE TEA":EXIT SELECT
            IF ROOM=3 AND OBJECT$="SIGN*" THEN PRINT "The SIGN* says -":_
                PRINT "DROP TREASURES! Such as GOLD! here and type  SCORE  ":EXIT SELECT
            IF ROOM=6 AND OBJECT$="SIGN*" THEN _
                PRINT "         The SIGN* says: Some things you need,":_
                PRINT "                         Can be found here;":_
                PRINT "                         Just say the word,":_
                PRINT "                         And they'll appear.":_
                PRINT "           ITEM #1:      I hang on the wall,":_
                PRINT "                         With nothing to do;":_
                PRINT "                         When you LOOK at me,":_
                PRINT "                         You see only you.":_
                PRINT "       You can't make out the last 4 or 5 lines.":_
                PRINT "       They look backwards or something.":_
                PRINT:EXIT SELECT
            IF ROOM=6 AND OBJECT$="MIRROR*" THEN _
                PRINT "Try -    LOOK MIRROR":EXIT SELECT

            THING$="MAP":IF OBJECT$="MAP" AND FNCARRY=0 THEN PRINT "You don't have the MAP":EXIT SELECT
            THING$="MAP":IF OBJECT$="MAP" AND FNCARRY=1 THEN _
                PRINT "Says  DOWN WEST WEST SOUTH SOUTH DIG":EXIT SELECT
            PRINT "Try READ SIGN* or  READ MAP":EXIT SELECT
       CASE "OIL","LUBRICATE","GREASE"
           THING$="MINERAL-OIL"
               IF FNCARRY=0 THEN PRINT "You don't have the MINERAL-OIL":EXIT SELECT
           IF ROOM=11 THEN PRINT "OK":FLAG(13)=1:EXIT SELECT
           PRINT "Not here":EXIT SELECT
       CASE "DRINK","EAT"
          PRINT "What is it with you and food?":EXIT SELECT
      CASE "SCORE"
          IF ROOM <>3 THEN PRINT "Return to the TOWN SQUARE":EXIT SELECT
          SCORE=2
          FOR I=1 TO 10
              LET STUFF$=ROOMOBJECT$(3,I)
              IF RIGHT$(STUFF$,1)="!" THEN SCORE=SCORE + 14
          NEXT I
          PRINT "Your SCORE is ";SCORE
          IF SCORE=100 THEN PRINT "You win!!!!  Fantastic Job!!!  YAHOO!!":FLAG(1)=1:EXIT SELECT
          PRINT (100-SCORE);" points to go":EXIT SELECT
      CASE "CLIMB"
          IF OBJECT$="WELL" OR OBJECT$="WELL*" THEN PRINT "Try GO WELL*":EXIT SELECT
          THING$="CHAIR"
              IF ROOM=20 AND FNCARRY=1 THEN_
                 PRINT "PUT or DROP CHAIR here first.":EXIT SELECT
              OBJECT$="CHAIR":IF ROOM=20 AND FNPRESENT=0 THEN_
                 PRINT "The BOULDER is too high, GET something to stand on":_
                 EXIT SELECT
              IF ROOM=20 THEN PRINT "Good climbing!":ROOM=21:EXIT SELECT
          PRINT "Come down from there!":EXIT SELECT
      CASE "TIE","CONNECT","ATTACH"
         IF LEFT$(OBJECT$,2)="UP" THEN PRINT "Try  TIE ROPE ":EXIT SELECT
         THING$="ROPE":IF FNCARRY=0 THEN PRINT "You'll need the ROPE":EXIT SELECT
         THING$="BUCKET":IF FNCARRY=0 THEN PRINT "GET the BUCKET":EXIT SELECT
         IF OBJECT$="DOG" THEN PRINT "You want me to try to TIE up a vicious DOG?  HA! HA!":_
             EXIT SELECT
         IF OBJECT$="ROPE" OR OBJECT$="BUCKET" THEN PRINT "One end of the rope is tied to the BUCKET"_
             :FLAG(12)=1:EXIT SELECT
         PRINT "I hardly know you!":EXIT SELECT

      CASE "UNTIE","UNDO"
          IF FLAG(12)=1 AND (OBJECT$="ROPE" OR OBJECT$="BUCKET") THEN_
              PRINT "OK":FLAG(12)=0:EXIT SELECT
      CASE "LOWER"
          PRINT "Try  GET WATER":EXIT SELECT
      CASE "MILK"
          PRINT "Try  GET MILK":EXIT SELECT
      CASE "KILL"
          THING$="ROCK"
              IF FNCARRY=0 AND ROOM=2 THEN PRINT "You'll need a suitable weapon":_
                  EXIT SELECT
              IF FNCARRY=1 AND ROOM=2 AND FLAG(3)=0 THEN_
                  PRINT "The dead RATTLESNAKE is still frightening.":_
                  LET ROOMOBJECT$(2,2)="SNAKE":FLAG(3)=1:EXIT SELECT
          PRINT "You must think I'm crazy!":EXIT SELECT
      CASE "UNLOCK"
          IF OBJECT$="SAFE" THEN PRINT "Try-  OPEN SAFE":EXIT SELECT
          THING$="KEY"
              IF FNCARRY=0 THEN PRINT "You'll need a KEY":EXIT SELECT
              IF ROOM=7 THEN PRINT "The JAIL DOOR* is unlocked. You can GO EAST":FLAG(6)=1:_
                  EXIT SELECT
          PRINT "No lock here":EXIT SELECT
      CASE "OPEN"
          IF ROOM=11 AND OBJECT$="SAFE*" AND FLAG(13)=0 THEN _
              PRINT "The rusty hinge needs OIL to open":EXIT SELECT
          IF ROOM=11 AND OBJECT$="SAFE*" AND FLAG(13)=1 AND FLAG(15)=0 THEN _
              PRINT "I see JADE!":ROOMOBJECT$(11,2)="JADE!":_
              FLAG(15)=1:EXIT SELECT
          IF ROOM=11 AND OBJECT$="SAFE*" AND FLAG(15)=1 THEN _
              PRINT "It is open!":EXIT SELECT
          IF OBJECT$="DOOR*" AND FLAG(6)=0 THEN _
              PRINT "The JAIL DOOR is locked!":EXIT SELECT
          IF ROOM=7 AND OBJECT$="DOOR*" AND FLAG(6)=1 THEN _
              PRINT "The JAIL DOOR is open, GO EAST to enter":EXIT SELECT
          PRINT "No........You can't make me":EXIT SELECT
     CASE "THROW"
         IF ROOM=2 THEN PRINT "Try  KILL SNAKE ":EXIT SELECT
         PRINT "Not here":EXIT SELECT
     CASE "DIG","SHOVEL"
         THING$="SHOVEL"
             IF FNCARRY=0 THEN PRINT "You'll need a SHOVEL":EXIT SELECT
             IF ROOM=31 OR ROOM=5 THEN PRINT "I hit something"
             IF ROOM=31 AND FLAG(16)=0 THEN ROOMOBJECT$(31,1)="DIAMONDS!":_
                 FLAG(16)=1:EXIT SELECT
             IF ROOM=5 AND FLAG(17)=0 THEN ROOMOBJECT$(5,1)="KEY":_
                 FLAG(17)=1:EXIT SELECT
         PRINT "The substrate is too hard here to dig":EXIT SELECT
     CASE "MAKE","PREPARE"
         IF OBJECT$<>"TEA" THEN PRINT "Don't know how":EXIT SELECT
         IF FNPRESENT = 0 THEN PRINT "You'll need TEA":EXIT SELECT
         OBJECT$="KETTLE":IF FNPRESENT=0 OR FLAG(10)=0 THEN _
            PRINT "You'll need a KETTLE FILLed with WATER.":EXIT SELECT
         OBJECT$="PAIL":IF FNPRESENT=0 OR FLAG(23)=0 THEN _
            PRINT "You'll need a PAIL FILLed with MILK.":EXIT SELECT
         OBJECT$="SUGAR":IF FNPRESENT=0 THEN PRINT "You'll need the SUGAR ":EXIT SELECT
         OBJECT$="MATCHES":IF FNPRESENT=0 THEN PRINT "You'll need some MATCHES":EXIT SELECT
         OBJECT$="CUP":IF FNPRESENT=0 THEN PRINT "You'll need a CUP":EXIT SELECT
         OBJECT$="MAGAZINES":IF FNPRESENT=0 THEN PRINT "You'll need something to burn":EXIT SELECT

         IF FLAG(18)=0 THEN FLAG(4)=1:_
              FLAG(18)=1 :ROOMOBJECT$(9,2)="EMERALD!":_
              PRINT "The grateful ENGLISHMAN leaves a big tip":EXIT SELECT
         IF FLAG(18)=1 THEN PRINT "Not again!":EXIT SELECT
         PRINT "PUT the items you need in the TEA ROOM":EXIT SELECT
     CASE "FILL"
         THING$="KETTLE"
         IF OBJECT$="KETTLE" AND FNCARRY=1 AND FLAG(11)=1 THEN _
            FLAG(10)=1:PRINT "The KETTLE is FILLed.":EXIT SELECT
         IF OBJECT$="KETTLE" AND FNPRESENT=1 AND FLAG(11)=1 THEN _
            FLAG(10)=1:PRINT "The KETTLE is FILLed.":EXIT SELECT

         IF OBJECT$="BUCKET" AND ROOM=2 THEN PRINT "Try GET WATER":EXIT SELECT
         IF OBJECT$="PAIL" AND ROOM=15 THEN PRINT "Try  GET MILK":EXIT SELECT
         PRINT "You'll need a KETTLE or a BUCKET or a PAIL":EXIT SELECT

     CASE "ENTER"
         PRINT "Try GO WELL* or GO CELLAR*":EXIT SELECT

     CASE "BURN"
         IF OBJECT$="HAY" THEN PRINT "Too green --- Won't BURN.":EXIT SELECT
         PRINT "Don't play with fire!":EXIT SELECT

     CASE "MIRROR*","MIRROR"
          PRINT "Suddenly you notice a MIRROR* on the wall":_
          PRINT "opposite the SIGN*":LET ROOMOBJECT$(6,3)="MIRROR*":EXIT SELECT

     CASE "SAY"
         PRINT "Just type the word you were going to say":EXIT SELECT

     CASE "COFFEE"
         IF FLAG(22)=0 THEN PRINT "Suddenly you smell and then see some":_
             PRINT "fresh coffee on the bar.  So does a passing prospector":_
             PRINT "who takes it, leaving some SILVER! in exchange.":_
             LET ROOMOBJECT$(6,4)="SILVER!":FLAG(22)=1:EXIT SELECT
         PRINT "All that's left of the coffee is the aroma":EXIT SELECT

     CASE "FEED"
        PRINT "Try the word  GIVE  followed by the name of the food.":_
           EXIT SELECT
     CASE "USE"
       PRINT "Try - DIG or UNLOCK or TIE or PLUCK":EXIT SELECT
     CASE "REVEAL"
       PRINT "Just type the name of the object by itself.":EXIT SELECT
      CASE ELSE
          IF ROOM=6 AND OBJECT$="MIRROR*" THEN _
             PRINT "Try -   LOOK MIRROR":EXIT SELECT
          IF OBJECT$="CELLAR*" THEN _
             PRINT "Try-    GO CELLAR":EXIT SELECT
          IF OBJECT$="WELL*" THEN PRINT "Try-  GO WELL":EXIT SELECT
          PRINT "I don't know that verb":EXIT SELECT
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
    SHARED FLAG(),TURNNUMBER,ANSWER$(),STRT
    FOR I=1 TO 6:PRINT:NEXT I
    INPUT "Be sure that your disk is in the drive and press ENTER";DUMMY$

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

      LET DTA(13)=FLAG(1):LET DTA(38)=DTA(38)+INT((TIMER-STRT)/6)
       OPEN "REPORT.DTA" FOR OUTPUT AS #1
         REM SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
         FOR I=1 TO 40
           WRITE #1,DTA(I)
         NEXT I
         FOR I=1 TO 10
           WRITE #1,DTA$(I)
         NEXT I
       CLOSE #1

    OPEN "HANGDATA.TXT" FOR APPEND AS #2
    PRINT #2, DATE$,TIME$
    FOR I=0 TO TURNNUMBER
         IF INT(I/5)=I/5 THEN PRINT #2, ANSWER$(I) ELSE PRINT #2,ANSWER$(I),
    NEXT I
    PRINT #2,INT((STRT-TIMER)/6),
    PRINT #2,INT(1000*RND(0))
    CLOSE #2
    PRINT "This game is over.  Type HANGTOWN to play again."
END SUB

'END OF PROGRAM
