'MODULE 0 MAIN    ******FARMTOWN*******9-2-90
'GAMEDTA.BAS CHANGED TO FARMSAV.BAS ON 1-24-91
'ALSO FIXED ENDING WITH WIN NO MATTER WHETHER
'CHANGE SAW RIDDLE AND MAKE STEW 6MAR92
'CHANGED TIMER AND STRT  2-3-94
'CHANGED SAVE GAME TIME$DATE$ CARROTS 2ND FLARE HINT 7-30-95
'changed snips 1-3-96
'added report 3-5-96  FARMDATA.TXT 10-24-96
    CLS
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
    DATA 21,RABBIT'S WARREN,STREAM,CORNFIELD,PASTURE,PIG STY
    DATA STORAGE SHED,BARN,POND,GARDEN,BARNYARD
    DATA FARMHOUSE,HIGHWAY,HIGHWAY,HIGHWAY,HIGHWAY
    DATA HIGHWAY,HIGHWAY,RECYCLING CENTER,STREET,GAS STATION
    DATA CAR PARKED ON SOUTH SIDE OF HIGHWAY

    FOR I=1 TO NUMBERROOMS
       LET DESCRIBEFLAG(I)=0
    NEXT I
    FOR I=1 TO NUMBERROOMS
       READ ROOMDESCRIBE$(I)
       READ ROOMDES2$(I)
    NEXT I
    DATA "The eyes of the rabbit sitting outside his warren are more"
    DATA "orange than pink.  Too much vitamin A!"
    DATA "Though the gurgle is friendly enough,"
    DATA "STICK'S STREAM is more dangerous than it looks."
    DATA "Here at KELLOG'S CORNFIELD,"
    DATA "You see plenty of pig food."
    DATA "Here at PIZENSIGH'S PASTURE, we see the dangers of over-"
    DATA "grazing.  The grass is too short to eat."
    DATA "Here at the INNAH PIG STY, The scene reminds you of your"
    DATA "school cafeteria on a rainy day."
    DATA "There are tools and fruit in the MAGICAL STORAGE SHED,"
    DATA "but you have to ask for them by name.  Weird."
    DATA "The BARN smells, well, like a barn.  No wonder people"
    DATA "held barn dances out of door whenever possible."
    DATA "If the DUCKS weren't so grouchy, this POND"
    DATA "would be a good place to spend some time."
    DATA "Some vandalous animal has dug up and stolen all of the"
    DATA "CARROTS, but you may still find some POTATOES."
    DATA "Watch your step!"
    DATA "This has been a very active BARNYARD."
    DATA "What we have here in the FARMHOUSE is a very crabby FARMER"
    DATA "who seems to be waiting for someone to fix supper."
    DATA "HIGHWAY"," ","HIGHWAY"," ","HIGHWAY"," ","HIGHWAY"," "
    DATA "HIGHWAY"," "
    DATA "The sound of the trucks barreling by on the narrow"
    DATA "HIGHWAY is quite deafening.  "
    DATA "Here at the SAVE OUR GLASS RECYCLING CENTER,"
    DATA "nothing is wasted."
    DATA "MAIN STREET could just as easily"
    DATA "have been called ONLY STREET."
    DATA "The semi-automated GAS STATION"
    DATA "is quite deserted."
    DATA "You are out of gas and money, parked along the SOUTH SIDE OF"
    DATA "A busy HIGHWAY.  You recall passing a town a little way back."

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
    DATA 0,2,0,0,0,0,1,4,0,0,0,0
    DATA 0,0,4,0,0,0,2,7,5,3,0,0
    DATA 0,0,0,4,0,0,0,0,7,0,0,0
    DATA 4,10,8,6,0,0,0,0,0,7,0,0
    DATA 0,0,10,0,0,0,7,14,11,9,0,0

    DATA 0,0,0,10,0,0,0,0,13,12,0,0
    DATA 0,0,14,12,0,0,10,19,15,13,0,0
    DATA 0,0,16,14,0,0,0,21,17,15,0,0
    DATA 0,0,17,16,0,0,0,0,19,0,0,0
    DATA 14,0,20,18,0,0,0,0,0,19,0,0
    DATA 16,0,0,0,0,0
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
        FLAG(),OBJ$(),OBJ2$(),CONVERTNUM
    LET ROOM = 21              'START IN CAR
    LET TURNNUMBER =0
    DANGEROUS=1
    LET THING$=""
    DIM FLAG(30)
    FOR I=1 TO 30:LET FLAG(I)=0:NEXT I      'SET FLAGS TO 0
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

    LET ROOMOBJECT$(1,1)="SIGN*"         'STARTING LOCATIONS FOR OBJECTS
    LET ROOMOBJECT$(1,2)="RABBIT*"
    LET ROOMOBJECT$(1,3)="CARROTS"
    LET ROOMOBJECT$(2,1)="EMPTYTREE-BRIDGE*"
    LET ROOMOBJECT$(3,1)="SIGN*"
    LET ROOMOBJECT$(3,2)="CORN"
    LET ROOMOBJECT$(4,1)="TREE*"
    LET ROOMOBJECT$(4,2)="COWS*"
    LET ROOMOBJECT$(4,3)="EMPTYBOTTLE"
    LET ROOMOBJECT$(4,4)="EMPTYBOTTLE"
    LET ROOMOBJECT$(5,1)="PIGS*"
    LET ROOMOBJECT$(5,2)="EMPTYBOTTLE"
    LET ROOMOBJECT$(6,1)="FRUIT*"
    LET ROOMOBJECT$(6,2)="TOOLS*"
    LET ROOMOBJECT$(6,3)="SIGN*"
    LET ROOMOBJECT$(6,4)="EMPTYPEARS"
    LET ROOMOBJECT$(6,5)="EMPTYHOE"
    LET ROOMBOJECT$(6,6)="EMPTYSAW"
    LET ROOMOBJECT$(7,1)="HAY"
    LET ROOMOBJECT$(7,2)="PITCHFORK"
    LET ROOMOBJECT$(7,3)="MEAT"
    LET ROOMOBJECT$(7,4)="EMPTYBOTTLE"
    LET ROOMOBJECT$(8,1)="DUCKS*"
    LET ROOMOBJECT$(8,2)="EMPTYBOTTLE"
    LET ROOMOBJECT$(9,1)="SIGN*"
    LET ROOMOBJECT$(9,2)="EMPTYPOTATOES"
    LET ROOMOBJECT$(10,1)="SIGN*"
    LET ROOMOBJECT$(11,1)="SIGN*"
    LET ROOMOBJECT$(11,2)="FARMER*"
    LET ROOMOBJECT$(11,3)="STOVE*"
    LET ROOMOBJECT$(11,4)="POT*"
    LET ROOMOBJECT$(11,5)="GAS-CAN"
    LET ROOMOBJECT$(11,6)="BREAD"
    LET ROOMOBJECT$(18,1)="SIGN*"
    LET ROOMOBJECT$(18,2)="EMPTYDOLLAR"
    LET ROOMOBJECT$(19,1)="SIGN*"
    LET ROOMOBJECT$(20,1)="SIGN*"
    LET ROOMOBJECT$(21,1)="SIGN*"
    LET ROOMOBJECT$(21,2)="EMPTYFLARE"

    READ CONVERTNUM
    DIM OBJ$(CONVERTNUM),OBJ2$(CONVERTNUM)
    FOR I=1 TO CONVERTNUM           'READ IN WORD!* CONVERSIONS
        READ OBJ$(I),OBJ2$(I)
    NEXT I
    DATA 19,SIGN,SIGN*,RABBIT,RABBIT*,TREE-BRIDGE,TREE-BRIDGE*
    DATA BRIDGE,TREE-BRIDGE*,TREE,TREE*,STUMP,STUMP*,COWS,COWS*
    DATA PIGS,PIGS*,FRUIT,FRUIT*,TOOLS,TOOLS*,DUCKS,DUCKS*
    DATA FARMER,FARMER*,STOVE,STOVE*,POT,POT*,CAN,GAS-CAN,PEAR,PEARS
    DATA CARROT,CARROTS,PAIR,PEARS,FLAIR,FLARE
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
           PRINT "FARMTOWN (at the DOS prompt) and later,":_
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
        CASE "HIGHWAY"
           PRINT "You find yourself on the SOUTH side of a HIGHWAY"
        CASE "STREET","BARN","FARMHOUSE"
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


            'PUT DOLLAR IN 18 IF 5 BOTTLES ARE PRESENT
    IF ROOM=18 AND FNFIVEBOTTLES=1 AND FLAG(12)=0 THEN _
       ROOMOBJECT$(18,2)="DOLLAR":FLAG(12)=1

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
    PRINT
END SUB

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
SUB GAMESTARTER
     CLS
     PRINT
     PRINT "                WELCOME TO FARMTOWN"
     PRINT
     PRINT "Stop vegetating and come away with me on an exciting adventure "
     PRINT "to FARMTOWN, home of the rutabaga!  You'll DIG the rural life "
     PRINT "along a dangerous HIGHWAY.  Cope with the widl hare!"
     PRINT "MAKE STEW!  GET GAS!"
     PRINT
     PRINT "Give COMMANDs as verb then object, such as GO NORTH,"
     PRINT "SAVE GAME, RESTORE GAME, READ SIGN, DIG POTATOES,"
     PRINT "AND REVEAL APPLES (REVEALed OBJECTS will show up where you"
     PRINT "would expect that OBJECT to be, not where you typed  REVEAL )"
     PRINT
     PRINT "Exceptions to this tow-word sentence rule are single-letter"
     PRINT "COMMANDs such as N to GO NORTH, U to GO UP, D to GO DOWN"
     PRINT
     PRINT "P.S. Don't try to get objects ending in an *, e.g., TREE*,"
     PRINT "as they are quite unobtainable-- you get my drift?"
     PRINT
     PRINT "And if you get stuck, keep trying different things because"
     PRINT "after a certain number of turns, a hint will show up."
     PRINT "Happy Adventuring!"
     PRINT "If done taking notes, press ENTER to begin"
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
    FOR J=1 TO 19                          'CONVERT OBJECT$ LACKING * OR !
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
            OPEN "FARMSAV.BAS" FOR OUTPUT AS #1

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
            OPEN "FARMSAV.BAS" FOR INPUT AS #1

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
            IF MOVEMENTTABLE (ROOM,DIRECTION) = 0 THEN _
                PRINT "You can't GO that way" ELSE _
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
            EXIT SELECT

        CASE "GET","TAKE","ACQUIRE"

            HOLDING$=OBJECT$
               IF ROOM<>1 AND OBJECT$="CARROTS"  THEN PRINT "They aren't here":_
                  EXIT SELECT
               OBJECT$="PEARS":IF ROOM=1 AND FNPRESENT=0 THEN _
                  PRINT "The RABBIT won't let me":EXIT SELECT
            OBJECT$=HOLDING$

            IF ROOM=6 AND OBJECT$="FRUIT*" THEN PRINT "READ SIGN*":EXIT SELECT
            IF ROOM=6 AND OBJECT$="TOOL*" THEN PRINT "READ SIGN*":EXIT SELECT

            THING$="PITCHFORK":IF OBJECT$="HAY" AND FNCARRY=0 THEN _
               PRINT "You'll need the PITCHFORK":EXIT SELECT
            IF FNCARRY=1 AND ROOM=7 AND FLAG(4)=0 AND FNINVENTORYSPACE=1 _
               THEN PRINT "Moving the HAY REVEALs a BOTTLE":_
               ROOMOBJECT$(7,4)="BOTTLE":FLAG(4)=1

            IF OBJECT$="POTATOES" AND FLAG(9)=0 THEN _
               PRINT "DIG them up first":EXIT SELECT

            IF OBJECT$="GAS-CAN"AND FLAG(10)=0 THEN _
               PRINT "The FARMER won't let you":EXIT SELECT
            IF OBJECT$="GAS" THEN PRINT "Try  FILL GAS-CAN ":EXIT SELECT
            IF OBJECT$="CARROTS" THEN PRINT "DROP only at FARMHOUSE!"

            IF FNPRESENT=0 THEN PRINT "I don't see ";OBJECT$:EXIT SELECT
            IF RIGHT$(OBJECT$,1)="*" THEN PRINT "I can't handle it":EXIT SELECT

            FOR I=1 TO 5
                IF INVENTORY$(I)="EMPTY" THEN INVENTORY$(I)=OBJECT$:_
                    PRINT "GOT IT":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY":_
                    EXIT SELECT
            NEXT I
            PRINT "You're carrying too much.  DROP something!"
        CASE "PUT","DROP","GIVE","LEAVE"

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

            IF OBJECT$="FLARE" AND ROOM<>14 THEN PRINT "Not here":EXIT SELECT
            IF OBJECT$="CARROTS" AND ROOM<>11 THEN PRINT "Not here":EXIT SELECT

            THING$="FLARE":IF ROOM=14 AND OBJECT$="FLARE" AND FNCARRY=1 AND _
               FNDROPABLESPOT=1 AND FLAG(11)=0 THEN FLAG(11)=1:_
               PRINT "Traffic has slowed.  You can now GO NORTH "
            THING$=OBJECT$
            IF FNCARRY=0 THEN PRINT "You don't have the ";OBJECT$:EXIT SELECT

            FOR J=1 TO 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":_
                    EXIT SELECT
            NEXT J
            PRINT "This room is full, take it elsewhere":EXIT SELECT
        CASE "LOOK","EXAMINE","INSPECT"
            IF OBJECT$="SIGN*" THEN PRINT "Try READ SIGN* ":EXIT SELECT
            PRINT "I don't see anything unexpected":EXIT SELECT
        CASE "READ"
            IF OBJECT$<>"SIGN*" THEN PRINT "I only READ SIGNS*":EXIT SELECT
            PRINT "It says:"
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
               PRINT "a gallon of gas to win the game."
            IF ROOM=21 THEN _
               PRINT "An object in this car,":_
               PRINT "Will help you with your chores;":_
               PRINT "So come back when you know,":_
               PRINT "Say its name and it's yours."
            PRINT
            EXIT SELECT


       CASE "DRINK","EAT"
          PRINT "This is not the time to worry about your stomach":EXIT SELECT
      CASE "CLIMB"
          IF OBJECT$="TREE*" THEN PRINT "Easy Tarzan!":EXIT SELECT
          PRINT "Come down from there!":EXIT SELECT

      CASE "MILK"
          PRINT "Oh, leave them alone!":EXIT SELECT
      CASE "KILL"
          PRINT "Mellow out, it's only a game!":EXIT SELECT

     CASE "DIG","HOE"
         IF OBJECT$<>"POTATOES" THEN PRINT "Try  DIG POTATOES ":EXIT SELECT
         THING$="HOE":IF FNCARRY=1 AND ROOM=9 AND FLAG(9)=0 THEN _
            PRINT "THERE THEY ARE!":ROOMOBJECT$(9,2)="POTATOES":_
            FLAG(9)=1:EXIT SELECT
         PRINT "You'll need the right tool":EXIT SELECT


         PRINT "The substrate is too hard here to dig":EXIT SELECT
     CASE "MAKE","PREPARE"
         IF OBJECT$<>"STEW" THEN PRINT "Try  MAKE STEW ":EXIT SELECT
         OBJECT$="MEAT":IF FNPRESENT=0 THEN PRINT "Where's the MEAT?":EXIT SELECT
         OBJECT$="POTATOES":IF FNPRESENT=0 THEN PRINT "Where're the POTATOES?":EXIT SELECT
         OBJECT$="CARROTS":IF FNPRESENT=0 THEN PRINT "Where're the CARROTS?":EXIT SELECT
         IF FLAG(10)=0 THEN PRINT "The FARMER ate all of your delicious STEW and fell asleep":_
            ROOMOBJECT$(11,2)="SLEEPING-FARMER*":FLAG(10)=1:EXIT SELECT
         PRINT "Not again":EXIT SELECT

     CASE "CROSS"
         IF OBJECT$="HIGHWAY" THEN PRINT "Try  N  or GO NORTH ":EXIT SELECT
         PRINT "Don't be CROSS!":EXIT SELECT

     CASE "ENTER"
         PRINT "No go, sorry":EXIT SELECT

     CASE "SAY"
         PRINT "Just type the word you were going to say":EXIT SELECT

     CASE "SAW"
        THING$="SAW":IF OBJECT$="TREE*" AND FNCARRY=1 AND FLAG(2)=0 THEN _
           PRINT "Timber!  I see a BOTTLE in the hollow STUMP*":FLAG(2)=1 :_
           ROOMOBJECT$(2,1)="TREE-BRIDGE*":ROOMOBJECT$(4,1)="STUMP*":_
           ROOMOBJECT$(4,4)="BOTTLE":EXIT SELECT
        PRINT "You need a SAW and a TREE":EXIT SELECT

     CASE "FEED"
        PRINT "Try  GIVE CORN  or GIVE BREAD  or  GIVE HAY":EXIT SELECT

     CASE "NO"
        PRINT "Fascinating":EXIT SELECT
     CASE "FILL"
        IF OBJECT$="TANK" THEN _
           PRINT "Just  PUT GAS-CAN  in the car":EXIT SELECT
        IF OBJECT$="POT*" THEN _
           PRINT "Just  PUT the ingredients in the FARMHOUSE":EXIT SELECT
        IF OBJECT$<>"GAS-CAN" THEN _
           PRINT "Are you sure it isn't full already?":EXIT SELECT
        IF FLAG(13)=1 THEN PRINT "You did that!":EXIT SELECT
        IF ROOM<>20 THEN PRINT "GO to the GAS STATION":EXIT SELECT
        IF FNPRESENT=0 THEN PRINT "PUT GAS-CAN here":EXIT SELECT
        OBJECT$="DOLLAR":IF FNPRESENT=0 THEN PRINT " PUT DOLLAR  here":_
           EXIT SELECT
        PRINT "With the gas, you are able to drive home,":_
           PRINT "Happy in the knowledge that you are awesome!":_
           PRINT "You've won FARMTOWN; now on to HANGTOWN!":_
           ROOMOBJECT$(20,ITEMNUMBER)="EMPTY":FLAG(1)=1:_
           FLAG(13)=1:EXIT SELECT
     CASE "START","DRIVE"
         IF OBJECT$<>"CAR" THEN PRINT "Try  DRIVE CAR ":EXIT SELECT
         OBJECT$="GAS-CAN":IF FNPRESENT=0 OR FLAG(13)=0 THEN _
            PRINT "PUT the GAS-CAN in the car when it is full":EXIT SELECT
         PRINT "As you drive home, you congratulate yourself on winning":_
            PRINT "the FARMTOWN ADVENTURE!  GOOD WORK!!":FLAG(1)=1:EXIT SELECT

     CASE "REVEAL"
        IF OBJECT$="PEARS" AND FLAG(7)=0 THEN PRINT "OK":_
           ROOMOBJECT$(6,4)="PEARS":FLAG(7)=1:EXIT SELECT
        IF OBJECT$="HOE" AND FLAG(8)=0 THEN PRINT "OK":_
           ROOMOBJECT$(6,5)="HOE":FLAG(8)=1:EXIT SELECT
        IF OBJECT$="FLARE" AND FLAG(16)=0 THEN FLAG(16)=1:_
           PRINT "The FLARE will light when DROPped.":_
           ROOMOBJECT$(21,2)="FLARE":FLAG(16)=1:EXIT SELECT
        IF OBJECT$="SAW" AND FLAG(15)=0 THEN PRINT "OK":_
           ROOMOBJECT$(6,6)="SAW":FLAG(15)=1:EXIT SELECT
        PRINT "Didn't work":EXIT SELECT
     CASE "FLARE","FLAIR"
        PRINT "Try  REVEAL FLARE ":EXIT SELECT
     CASE "LIGHT","IGNITE","STRIKE"
        IF OBJECT$="FLARE" THEN PRINT "OK":EXIT SELECT
        PRINT "That wouldn't be safe":EXIT SELECT

      CASE "USE"
        PRINT "To do what.  Be more specific.":EXIT SELECT
      CASE ELSE
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

'MODULE 2.3.3 DEFINE THE FUNCTION- IS THERE SPACE IN INVENTORY
DEF FNINVENTORYSPACE
   SHARED INVENTORY$()
   FNINVENTORYSPACE=0
   FOR I=1 TO 5
      IF INVENTORY$(I)="EMPTY" THEN FNINVENTORYSPACE=1:EXIT DEF
   NEXT I
END DEF

'MODULE 2.3.4 DEFINE THE FUNCTION- IS THERE SPACE HERE TO DROP SOMETHING
DEF FNDROPABLESPOT
   SHARED ROOMOBJECT$(),ROOM
      FOR J=1 TO 15
         IF ROOMOBJECT$(ROOM,J)="EMPTY" THEN FNDROPABLESPOT=1:EXIT DEF
      NEXT J
   FNDROPABLESPOT=0
END DEF

'MODULE 2.3.5 DEFINE THE FUNCTION- DO WE HAVE 5 BOTTLES YET
DEF FNFIVEBOTTLES
   SHARED ROOMOBJECT$(),ROOM
   TOTAL=0
   FOR J=1 TO 15
      IF ROOMOBJECT$(ROOM,J)="BOTTLE" THEN TOTAL=TOTAL+1
   NEXT J
   IF TOTAL=5 THEN FNFIVEBOTTLES=1 ELSE FNFIVEBOTTLES=0
END DEF

'MODULE 3 CLOSING


SUB CLOSING
    SHARED FLAG(),TURNNUMBER,ANSWER$(),STRT
    PRINT
    IF FLAG(1)= 1 THEN CLS:PRINT:PRINT:_
    PRINT "With the gas, you fill the tank and drive on!":_
    PRINT "You have conquered a most difficult adventure!  Good work!!"
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

    LET DTA(10)=FLAG(1):LET DTA(35)=DTA(35)+INT((TIMER-STRT)/6)
       OPEN "REPORT.DTA" FOR OUTPUT AS #1
         REM SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
         FOR I=1 TO 40
           WRITE #1,DTA(I)
         NEXT I
         FOR I=1 TO 10
           WRITE #1,DTA$(I)
         NEXT I
       CLOSE #1

    OPEN "FARMDATA.TXT" FOR APPEND AS #2
    PRINT #2, DATE$,TIME$
    FOR I=0 TO TURNNUMBER
        IF INT(I/5)=I/5 THEN PRINT #2, ANSWER$(I) ELSE PRINT #2,ANSWER$(I),
    NEXT I
    PRINT #2,INT((STRT-TIMER)/6),


        PRINT #2,INT(1000*RND(0))

    CLOSE #2
    PRINT "This game is over.  Type FARMTOWN to play again."
END SUB

END 'PROGRAM
