    'MODULE 0 MAIN    ******CURSED CASTLE******9-2-90
'CHANGED GAMEDTA.BAS TO CURSAV.BAS ON 1-24-91
'CHANGED TURNNUMBER TO GYNTURN FOR GIANT TURNS 18FEB92
'CHANGED TIMER ON 2-3-94
'CHANGED SAVE GAME TIME$ DATE$ 7-30-95
'snips added 12-18-95  i/5
'REPORT ADDED 3-5-96  CURSDATA.TXT 10-24-96

    CLS:DIM FLAG(30),OBJ$(30),OBJ2$(30)
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
    DATA 16,DUNGEON,STAIRCASE,ARMORY,STAIRCASE,WALK-IN FIREPLACE
    DATA BANQUET HALL,THRONE ROOM,COURTYARD,GATEHOUSE,BLACKSMITH'S
    DATA STABLES,STAIRCASE,DRAWING ROOM,BEDCHAMBER,BEDCHAMBER
    DATA TOWER

    FOR I=1 TO NUMBERROOMS
       LET DESCRIBEFLAG(I)=1
       LET ROOMDESCRIBE$(I)="EMPTY"
       LET ROOMDES2$(I)="EMPTY"
    NEXT I

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
    DATA 14,15,0,13,16,4
    DATA 0,0,12,0,0,0
    DATA 0,12,0,0,0,0
    DATA 12,0,0,0,0,0

    DATA 0,0,0,0,0,12



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
        FLAG(),OBJ$(),OBJ2$(),DESCRIBEFLAG(),ROOMDESCRIBE$(),ROOMDES2$()
    LET TURNNUMBER =0
    LET THING$=""
    FOR I=1 TO 30:LET FLAG(I)=0:NEXT I      'SET FLAGS TO 0
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
    FOR I=1 TO 20
      READ ROOM,J,ROOMOBJECT$(ROOM,J)
    NEXT I
    DATA 1,1,EMPTYGOLD!,3,1,EMPTYSWORD,5,1,KETTLE*,5,2,EMPTYFIRE*
    DATA 7,2,THRONE*,6,1,EMPTYGIANT*,7,1,SIGN*,8,1,PUMP*
    DATA 9,1,EMPTYSHOVEL,9,2,EMPTYBUCKET,10,1,FLINT-STEEL
    DATA 11,1,STRAW,11,2,EMPTYOATS,13,1,EMPTYHAIR-OIL
    DATA 13,2,EMPTYPEARLS!,13,3,SIGN*,14,1,EMPTYRUBIES!
    DATA 15,1,EMPTYEMERALDS!,16,1,EMPTYDIAMONDS!
    DATA 16,2,AXE

 'THE STARTING ROOM IS THE BANQUET HALL - 6
    ROOM=6


    FOR I=1 TO 20                       'READ IN WORD!* CONVERSION
        READ OBJ$(I),OBJ2$(I)
    NEXT I
    DATA SIGN,SIGN*,PEARL,PEARLS!,PEARLS,PEARLS!,OIL,HAIR-OIL
    DATA HAIR,HAIR-OIL,GOLD,GOLD!,KETTLE,KETTLE*,FIRE,FIRE*
    DATA GIANT,GIANT*,THRONE,THRONE*,PUMP,PUMP*,FLINT,FLINT-STEEL
    DATA STEEL,FLINT-STEEL,RUBY,RUBIES!,RUBIES,RUBIES!
    DATA DIAMOND,DIAMONDS!,DIAMONDS,DIAMONDS!
    DATA EMERALD,EMERALDS!,EMERALDS,EMERALDS!
    DATA PUMP,PUMP*
END SUB




'MODULE 2 TURN
SUB TURN
    SHARED FLAG(),TURNNUMBER,GYNTURN
    DO UNTIL FLAG(1) <> 0
        FLAG(14)=0
        TURNNUMBER = TURNNUMBER + 1
        GYNTURN=GYNTURN+1
        IF TURNNUMBER=295 THEN BEEP 3:_
           PRINT "Type   SAVE GAME  and then  QUIT":_
           PRINT "You can then return to this spot by typing":_
           PRINT "CURSED (at the DOS prompt) and later,":_
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
        ROOMDES2$(),GYNTURN

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
   IF GYNTURN=10 AND FLAG(10)=1 THEN GYNTURN=1

   'GIANT FLEES AT SIGHT OF SWORD
   IF ROOM=6 AND FLAG(10)=1 AND FLAG(8)=0 THEN FLAG(8)=1:_
     PRINT "The cowardly GIANT flees at the sight of the sword!":_
     ROOMOBJECT$(6,1)="EMPTY":FLAG(14)=1:ROOMOBJECT$(9,1)="SHOVEL":_
     EXIT SUB

  PRINT

END SUB

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
SUB GAMESTARTER
     CLS
     PRINT "          WELCOME TO CURSED CASTLE!"
     PRINT
     PRINT "    Come along on a creepy adventure to far-off places."
     PRINT" Battle GIANTS!  Find treasures!  MAKE OATMEAL!"
     PRINT
     PRINT "    While on a vacation to Scotland, you fall asleep "
     PRINT "reading about the human-eating GIANT that supposedly used"
     PRINT "to terrorize old castle towns."
     PRINT "    In your dream you find yourself in the banquet hall "
     PRINT "of an abandoned castle.  You are very cold and hungry."
     PRINT "If you do not build a fire soon, you will die!"
     PRINT
     PRINT "    Give commands as VERB then OBJECT, such as GO NORTH,"
     PRINT "READ SIGN, BUILD FIRE, OIL PUMP, DIG DIRT, CHOP"
     PRINT "THRONE, and so forth."
     PRINT
     PRINT "    Exceptions to this two-word sentence rule are single-"
     PRINT "letter commands such as N to GO NORTH (not GO N, just N),"
     PRINT "U to GO UP, and D to GO DOWN."
     PRINT "    P.S. Don't try to GET objects ending in an *, e.g., KETTLE*,"
     PRINT "as they are quite unobtainable.  Note also that things appear"
     PRINT "and disappear suddenly in dreams.  Keep you wits about you!"
     PRINT "If you're through taking notes, press the Enter key to begin"
     DO WHILE LEN(INKEY$)=0:LOOP
     CLS
END SUB

'MODULE 2.1.2 ERASER ERASES ONE LINE OF TEXT
SUB ERASER
  REM  FOR I=1 TO 12:PRINT "          ";:NEXT I
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
        ROOMOBJECT$(),ITEMNUMBER,TURNNUMBER,THING$,INVENTORY$(),FLAG(),_
        GYNTURN
    IF FLAG(1)<>0 THEN EXIT SUB
    IF FLAG(14)=1 THEN EXIT SUB
    SELECT CASE VERB$
        CASE "QUIT","Q","AWAKEN","WAKE"
            LET FLAG(1)=2   'GAME NOT WON(1) OR LOST(-1):EXIT SELECT
        CASE "SAVE"
            IF OBJECT$<>"GAME" THEN PRINT "SAVE GAME":EXIT SELECT
            OPEN "CURSAV.BAS" FOR OUTPUT AS #1

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
            OPEN "CURSAV.BAS" FOR INPUT AS #1

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
                PRINT "You can't go that way" ELSE _
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
            EXIT SELECT

        CASE "GET","TAKE","PLUCK","EXTRACT","PULL"

            IF ROOM=7 AND RIGHT$(OBJECT$,1)="!" THEN _
              PRINT "Best leave the treasures here, love":EXIT SELECT

            THING$="BUCKET"
            IF FNCARRY=0 AND OBJECT$="WATER" THEN _
              PRINT "GET the BUCKET":EXIT SELECT
            IF OBJECT$="WATER" AND FLAG(7)=0 THEN _
              PRINT "OIL the PUMP first.  It's rusty.":EXIT SELECT
            IF OBJECT$="WATER" AND ROOM <> 8 THEN _
              PRINT "GO to the courtyard":EXIT SELECT
            IF OBJECT$="WATER" THEN FLAG(6)=1:_
              PRINT "The BUCKET is full of nice WATER.":EXIT SELECT

            IF  FNPRESENT = 0 THEN PRINT "I don't see  ";OBJECT$:EXIT SELECT
            IF RIGHT$(OBJECT$,1)="*" THEN PRINT "I can't handle it":EXIT SELECT

            IF OBJECT$="SWORD" THEN FLAG(10)=1

            FOR I=1 TO 5
                IF INVENTORY$(I)="EMPTY" THEN INVENTORY$(I)=OBJECT$:_
                    PRINT "Got it.":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY":_
                    EXIT SELECT
            NEXT I

            IF OBJECT$="SWORD" THEN FLAG(10)=0

            PRINT "You're carrying too much.  DROP something.":EXIT SELECT
        CASE "PUT","DROP","GIVE"

            THING$=OBJECT$
            IF FNCARRY=0 THEN PRINT "You don't have the ";OBJECT$:EXIT SELECT

            IF OBJECT$="SWORD" THEN PRINT "You'll need the SWORD.":EXIT SELECT

            FOR J=1 TO 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":_
                    EXIT SELECT
            NEXT J
            PRINT "This room is full, take it elsewhere":EXIT SELECT


        CASE "READ"
            IF OBJECT$="SIGN*" AND ROOM=7 THEN _
               PRINT "It says - DROP TREASURES! such as GOLD!":_
               PRINT "here and type SCORE":EXIT SELECT
            IF OBJECT$="SIGN*"AND ROOM=13 THEN _
               PRINT "Recipe for OATMEAL -> DROP OATS and water BUCKET":_
               PRINT "at kettle and MAKE OATMEAL":EXIT SELECT

            PRINT "Try READ SIGN* ":EXIT SELECT

       CASE "OIL","LUBRICATE","GREASE","HAIR-OIL"
           THING$="HAIR-OIL"
               IF FNCARRY=0 THEN PRINT "You don't have the HAIR-OIL":EXIT SELECT
           IF ROOM=8 THEN PRINT "Now you can GET the WATER.":_
              FLAG(7)=1:EXIT SELECT
           PRINT "You are in the wrong place":EXIT SELECT
       CASE "DRINK"
          PRINT "You are not thirsty":EXIT SELECT
       CASE "EAT"
          IF FLAG(3)=0 THEN PRINT "MAKE OATMEAL first":EXIT SELECT
          PRINT "You are in the wrong place":EXIT SELECT
       CASE "BREAK","CHOP","CUT"
          IF FLAG(4)=1 THEN _
             PRINT "The only thing worth breaking was the THRONE":EXIT SELECT
          IF ROOM<>7 THEN PRINT "GO to the throne room":EXIT SELECT
          THING$="AXE"
            IF FNCARRY=0 THEN PRINT "GET the AXE":EXIT SELECT
          PRINT "Whew! Now you have some WOOD for a fire":_
             ROOMOBJECT$(7,2)="WOOD":FLAG(4)=1:_
             INVENTORY$(ITEMNUMBER)="EMPTY":EXIT SELECT
       CASE "STRIKE"
         PRINT "Try -   MAKE FIRE ":EXIT SELECT
       CASE "FILL"
         PRINT "Try -   GET WATER":EXIT SELECT
       CASE "PUMP"
         PRINT "Try -   GET WATER ":EXIT SELECT
       CASE "SCORE"
         IF ROOM<>7 THEN PRINT "Return to the THRONE ROOM":EXIT SELECT
         SCORE=0
         FOR I=1 TO 10
             LET STUFF$=ROOMOBJECT$(7,I)
             IF RIGHT$(STUFF$,1)="!" THEN SCORE=SCORE+20
         NEXT I
         PRINT "Your SCORE is ";SCORE
         IF SCORE=100 THEN PRINT "YOU WIN!!!!!  FANTASTIC JOB!!!":_
           FLAG(1)=1:EXIT SELECT
         PRINT (100-SCORE);" points to go!":EXIT SELECT

      CASE "DIG","SHOVEL"
         THING$="SHOVEL"
           IF FNCARRY=0 THEN PRINT "GET the SHOVEL":EXIT SELECT
         IF ROOM<>1 THEN PRINT "The floor is solid rock here":EXIT SELECT
         IF FLAG(11)=1 THEN PRINT "Nothing else to DIG":EXIT SELECT
         PRINT "YOU HIT SOMETHING!":ROOMOBJECT$(1,1)="GOLD!":_
           FLAG(11)=1:EXIT SELECT
      CASE "KILL","MURDER","SLAY"
         PRINT "GET a suitable weapon":EXIT SELECT
      CASE "MAKE","BUILD","PREPARE","COOK"
         IF ROOM<>5 THEN PRINT "GO to the FIREPLACE":EXIT SELECT
         'MAKE FIRE OPTIONS
         IF OBJECT$<>"FIRE*" THEN GOTO JUMPOVER
           IF FLAG(2)=1 THEN PRINT "There is a fire!":EXIT SELECT
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
            IF FLAG(5)=1 THEN FLAG(5)=0:EXIT SELECT

         'JUMPOVER TO MAKE OATMEAL OPTIONS
         JUMPOVER:
          IF LEFT$(OBJECT$,3)<>"OAT" AND OBJECT$<>"FOOD" THEN _
            PRINT "Try - BUILD FIRE or MAKE OATMEAL":EXIT SELECT
         IF FLAG(2)=0 THEN PRINT "BUILD FIRE first":EXIT SELECT
         IF FLAG(3)=1 THEN PRINT "Once is enough!":EXIT SELECT
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
          IF FLAG(5)=1 THEN FLAG(5)=0:EXIT SELECT
      CASE "USE"
        PRINT "To do what?  Try something more specific."
        EXIT SELECT
      CASE ELSE
            PRINT "I don't know that VERB":EXIT SELECT
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
    PRINT
IF FLAG(1)=1 THEN PRINT " *****   HOORAY FOR YOU!!  YOU'VE WON THE GAME!!  ****"
    PRINT
   FOR I=1 TO 6:    PRINT:NEXT I
    INPUT "BE SURE THAT YOUR DISK IS IN THE DRIVE AND PRESS ENTER";DUMMY$

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

    LET DTA(8)=FLAG(1):LET DTA(33)=DTA(33)+INT((TIMER-STRT)/6)
       OPEN "REPORT.DTA" FOR OUTPUT AS #1
         REM SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
         FOR I=1 TO 40
           WRITE #1,DTA(I)
         NEXT I
         FOR I=1 TO 10
           WRITE #1,DTA$(I)
         NEXT I
       CLOSE #1

    OPEN "CURSDATA.TXT" FOR APPEND AS #2
       PRINT #2,TIME$
       FOR I=0 TO TURNNUMBER
        IF INT(I/5)=I/5 THEN PRINT #2,ANSWER$(I) ELSE PRINT #2,ANSWER$(I),
       NEXT I
       PRINT #2,INT((STRT-TIMER)/6),INT(1000*RND(0))
    CLOSE #2
    PRINT "This game is over.  Type CURSED  to play again."
END SUB

'END OF PROGRAM
