'MODULE 0 MAIN    ******TOWNHOUSE*******1-24-91 2-3-94
'CHANGED FROM 7-9-90 VERSION TO ADD LOGON AND CHANGE NAME OF
'THE SAVE GAME FILE AND ADD VARIABLE CONVERTNUM TO KEEP TRACK
'OF THE NUMBER OF TERMS CONVERTED ON INPUT
'CHANGED TIMER &STRT
'CHANGED SAVE GAME TIMEF$ DATE$ 7-30-95
'snips added and int/5 on 12-4-95
'MOP FLOOR DIDN'T CHECK ROOM fixed on 12-18-95  NO MORE MOP-DUSTMOP*
'REPORT STUFF ADDED 3-5-96  TOWNDATA.TXT 10-15-96

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
    DATA 13, PANTRY,BOTTOM OF THE STAIRWELL,KITCHEN,LOWER HALL, LIBRARY
    DATA LIVING ROOM,CLOSET,ATTIC,MASTER BEDROOM,UPPER HALL
    DATA BATHROOM,TOP OF THE STAIRWELL,SMALL BEDROOM

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
    DATA 0,3,0,0,0,0
    DATA 0,4,0,0,12,0
    DATA 1,5,4,0,0,0
    DATA 2,6,0,3,0,0
    DATA 3,0,6,0,0,0

    DATA 4,0,0,5,0,0
    DATA 0,0,0,0,0,0
    DATA 0,10,0,0,0,0
    DATA 7,11,10,0,0,0
    DATA 8,12,13,9,0,0

    DATA 9,0,0,0,0,0
    DATA 10,0,0,0,0,2
    DATA 0,0,0,10,0,0



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
    LET ROOM = 6               'START IN LIVING ROOM
    LET TURNNUMBER =0
    DANGEROUS=1
    LET THING$=""
    FOR I=1 TO 30:LET FLAG(I)=0:NEXT I      'SET FLAGS TO 0
          'FLAG #  MEANING OF 0      FLAG# MEANING OF 0
          ' 1      GAME NOT OVER       2   DISHES NOT WASHED
          'FOR 1, 1 MEANS GAME WON -1 GAME LOST 2 MEANS GAME QUIT

          ' 3 PANTRY DOOR NOT UNLOCKED 4   HALL NOT MOPPED
          ' 5 CLOTHES NOT WASHED       6   LIGHTS NOT ON
          ' 7 TELEPHONE NOT ANSWERED   8   SINK NOT CLEANED
          ' 9 ATTIC DOOR NOT UNLOCKED  10   DUMMY
          '11 CLOTHES NOT PICKED UP

    LET ROOMOBJECT$(1,1)="WASHER-DRYER*"
    LET ROOMOBJECT$(1,2)="EMPTYCLEAN-CLOTHES*"
    LET ROOMOBJECT$(3,1)="DUSTMOP"
    LET ROOMOBJECT$(3,2)="EMPTYKEY"
    LET ROOMOBJECT$(3,3)="SINK*"
    LET ROOMOBJECT$(4,1)="DUST"
    LET ROOMOBJECT$(4,2)="EMPTYNOTE"
    LET ROOMOBJECT$(5,1)="SIGN*"
    LET ROOMOBJECT$(5,2)="TELEPHONE*"
    LET ROOMOBJECT$(6,1)="COUCH*"
    LET ROOMOBJECT$(8,1)="EMPTYLATCHFREE"
    LET ROOMOBJECT$(9,1)="DIRTY-CLOTHES"
    LET ROOMOBJECT$(11,1)="SINK*"
    LET ROOMOBJECT$(11,2)="EMPTYHAIRPIN"

    LET CONVERTNUM=18

    FOR I=1 TO CONVERTNUM                   'READ IN WORD!* CONVERSION
        READ OBJ$(I),OBJ2$(I)
    NEXT I
    DATA SIGN,SIGN*,DISHES,DISHES*,TELEPHONE,TELEPHONE*,MOP,DUSTMOP
    DATA PHONE,TELEPHONE*,PIN,HAIRPIN,HAIR,HAIRPIN,CLOTHES,DIRTY-CLOTHES
    DATA DIRTY,DIRTY-CLOTHES,WASHER,WASHER-DRYER*,DRYER,WASHER-DRYER*
    DATA MACHINE,WASHER-DRYER*,CAN,LATCHFREE,LATCH,LATCHFREE
    DATA SPRAY,LATCHFREE,DIRTY,DISHES*,SINK,SINK*,COUCH,COUCH*



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
           PRINT "TOWNHOUSE (at the DOS prompt) and later,":_
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
    PRINT
    'CLOTHES WASHED(5) AND PHONE NOT ANSWERED(7)
    IF FLAG(5)=1 AND FLAG(7)=0 THEN PRINT "The phone is ringing.  ANSWER it!"

   'IN KITCHEN(3) AND DISHES NOT WASHED(2)
   IF ROOM=3 AND FLAG(2)=0 THEN PRINT "The sink is full of dirty dishes."

   'IN HALL(4) AND NOT SWEPT-4-
   IF ROOM=4 AND FLAG(4)=0 THEN PRINT "The floor is covered with dust."

   'CLOTHES NOT PICKED UP(11) AND ROOM=BEDROOM (9)
   IF ROOM=9 AND FLAG(11)=0 THEN PRINT "The floor is littered with dirty clothes.":_
        FLAG(11)=1

   'ROOM=BATHROOM(11) AND SINK NOT CLEANED (8)
   IF ROOM=11 AND FLAG(8)=0 THEN PRINT "The sink is filthy and you should CLEAN SINK"
    PRINT

END SUB

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
SUB GAMESTARTER
     CLS
     PRINT "          WELCOME TO THE TOWNHOUSE ADVENTURE!"
     PRINT
     PRINT "    Mr. and Mrs. Graham have gone out to a movie and left "
     PRINT "you to babysit their daughter, Anna.  "
     PRINT
     PRINT "    Somehow, you have fallen asleep on the living room"
     PRINT "couch!  In your dream, Mr. and Mrs. Graham came home, found"
     PRINT "Anna missing, and berated you severely (and rightly so)."
     PRINT
     PRINT "    Amid waves of utter humiliation, you awaken suddenly"
     PRINT "in a cold sweat.  You are on the couch and everything seems"
     PRINT "fine.  But is it?  You'll soon find out!"
     PRINT
     PRINT "    Give COMMANDs as verb then object, such as  GO NORTH ,"
     PRINT " READ SIGN, ANSWER PHONE, WASH DISHES, MOP FLOOR,"
     PRINT "PICK LOCK, SPRAY LATCHFREE, and so forth."
     PRINT
     PRINT "    Exceptions to this two-word sentence rule are single-"
     PRINT "letter COMMANDs such as N to GO NORTH, U to GO UP, and"
     PRINT "so forth.  Remember, not GO N, just N  "
     PRINT "    p.s. Don't try to GET objects ending in an *, e.g., COUCH*,"
     PRINT "as they are quite unobtainable-- if you know what I mean."
     PRINT "If you're through taking notes, press the ENTER key to begin"
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
            IF OBJECT$<>"GAME" THEN PRINT "SAVE GAME":EXIT SELECT
            OPEN "TOWNSAV.BAS" FOR OUTPUT AS #1
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
            OPEN "TOWNSAV.BAS" FOR INPUT AS #1
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
            IF COUNTER = 0 THEN PRINT "Nothing at all" :EXIT SELECT
            PRINT:EXIT SELECT

        CASE "GO","MOVE","N","S","E","W","U","D"
            IF LEN (VERB$)=1 THEN OBJECT$=VERB$
            IF OBJECT$="NORTH" OR OBJECT$="N" THEN DIRECTION = 1
            IF OBJECT$="SOUTH" OR OBJECT$="S" THEN DIRECTION = 2
            IF OBJECT$="EAST"  OR OBJECT$="E" THEN DIRECTION = 3
            IF OBJECT$="WEST"  OR OBJECT$="W" THEN DIRECTION = 4
            IF OBJECT$="UP"    OR OBJECT$="U" THEN DIRECTION = 5
            IF OBJECT$="DOWN"  OR OBJECT$="D" THEN DIRECTION = 6

           IF ROOM=3 AND DIRECTION=1 AND FLAG(3)=0 THEN _
             PRINT "Can't enter pantry -- door locked.":EXIT SELECT
           IF ROOM=2 AND DIRECTION=5 AND FLAG(6)=0 THEN_
             PRINT "Can't climb stairs -- Too dark!":EXIT SELECT
           IF ROOM=9 AND DIRECTION=1 AND FLAG(1)=0 THEN_
             PRINT "Can't enter closet -- SPRAY latch with LATCHFREE":EXIT SELECT
           IF ROOM=10 AND DIRECTION=1 AND FLAG(9)=0 THEN_
             PRINT "Can't enter attic -- Door locked!":EXIT SELECT

            IF MOVEMENTTABLE (ROOM,DIRECTION) = 0 THEN _
                PRINT "You can't go that way" ELSE _
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
            EXIT SELECT

        CASE "GET","TAKE","PLUCK","EXTRACT","PULL"

            IF ROOM=4 AND OBJECT$="DUST" THEN _
              PRINT "Try- MOP FLOOR ":EXIT SELECT

            IF  FNPRESENT = 0 THEN PRINT "I don't see  ";OBJECT$:EXIT SELECT
            IF RIGHT$(OBJECT$,1)="*" THEN PRINT "I can't handle it":EXIT SELECT


            FOR I=1 TO 5
                IF INVENTORY$(I)="EMPTY" THEN INVENTORY$(I)=OBJECT$:_
                    PRINT "GOT IT":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY":_
                    EXIT SELECT
            NEXT I
            PRINT "You're carrying too much.  Drop something."
        CASE "PUT","DROP","GIVE"

            THING$=OBJECT$
            IF FNCARRY=0 THEN PRINT "You don't have the ";OBJECT$:EXIT SELECT


            FOR J=1 TO 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":_
                    EXIT SELECT
            NEXT J
            PRINT "This room is full, take it elsewhere":EXIT SELECT


        CASE "READ"
            THING$=OBJECT$
            IF OBJECT$="NOTE" AND FNCARRY=0 THEN_
               PRINT "GET the NOTE first.":EXIT SELECT
            IF OBJECT$="NOTE" THEN _
               PRINT "It says- CLAP HANDS to turn on the lights.":_
               EXIT SELECT
            IF OBJECT$="SIGN*" THEN PRINT "It says - incoming calls only.":_
               EXIT SELECT

            PRINT "Try  READ SIGN*  OR  READ NOTE ":EXIT SELECT

       CASE "OIL","LUBRICATE","GREASE","SPRAY"
           THING$="LATCHFREE"
               IF FNCARRY=0 THEN PRINT "You don't have the LATCHFREE":EXIT SELECT
           IF ROOM=9 THEN PRINT "The latch and door open easily.":_
              FLAG(1)=1:EXIT SELECT
           PRINT "Not here":EXIT SELECT
       CASE "DRINK","EAT"
          PRINT "This is not the time to worry about your stomach":EXIT SELECT

      CASE "UNLOCK"
              IF ROOM=10 THEN PRINT "Try- PICK LOCK.":EXIT SELECT
          THING$="KEY"
              IF FNCARRY=0 THEN PRINT "You'll need a key.":_
                 EXIT SELECT
              IF ROOM=3 AND FLAG(3)=0 THEN PRINT "The pantry door is open!":_
                  FLAG(3)=1:INVENTORY$(ITEMNUMBER)="EMPTY":EXIT SELECT
          PRINT "No lock here":EXIT SELECT

      CASE "PICK"
         IF ROOM=3 THEN PRINT "Try- UNLOCK DOOR":EXIT SELECT
         THING$="HAIRPIN"
            IF FNCARRY=0 THEN PRINT "GET the HAIRPIN ":EXIT SELECT
            IF ROOM=10 AND FLAG(9)=0 THEN FLAG(9)=1:_
               PRINT "The attic door is now open!":EXIT SELECT
         PRINT "This is the wrong place.":EXIT SELECT

      CASE "MOP","DUSTMOP","DUST","SWEEP"
         THING$="DUSTMOP"
            IF FNCARRY=0 THEN PRINT "GET the DUSTMOP ":EXIT SELECT
         IF ROOM<>4 THEN PRINT "Not here.":EXIT SELECT
         IF ROOM=4 AND FLAG(4)=0 THEN FLAG(4)=1:_
            PRINT "You see a NOTE on the newly cleaned floor!":_
            ROOMOBJECT$(4,2)="NOTE":ROOMOBJECT$(4,1)="EMPTY":EXIT SELECT
         PRINT "That didn't help.":EXIT SELECT

     CASE "CLEAN"
         IF ROOM=3 THEN PRINT "Try- WASH DISHES ":EXIT SELECT
         IF ROOM=11 AND FLAG(8)=0 THEN _
            PRINT "The sink is now clean -- You see something!":_
            ROOMOBJECT$(11,2)="HAIRPIN":FLAG(8)=1:EXIT SELECT
        PRINT "That didn't help.":EXIT SELECT

    CASE "WASH"
        IF OBJECT$="DISHES*" AND ROOM=3 AND FLAG(2)=0 THEN_
           PRINT "Under the now clean dishes you see something!":_
           FLAG(2)=1:ROOMOBJECT$(3,2)="KEY":EXIT SELECT

        THING$=OBJECT$
        IF OBJECT$="DIRTY-CLOTHES" AND FNCARRY=0 THEN_
           PRINT "GET the CLOTHES first.":EXIT SELECT

       IF OBJECT$="DIRTY-CLOTHES" AND ROOM=1 AND FLAG(5)=0 THEN_
          PRINT "That was painless.":FLAG(5)=1:ROOMOBJECT$(1,2)=_
          "CLEAN-CLOTHES*":INVENTORY$(ITEMNUMBER)="EMPTY":_
          OBJ2$(8)="CLEAN-CLOTHES*":EXIT SELECT

      PRINT "Try-  CLEAN SINK ":EXIT SELECT

  CASE "CLAP"
     PRINT "The lights on the stairs just went on!":FLAG(6)=1:EXIT SELECT

  CASE "SNEEZE","SNEZE"
     IF ROOM<>8 THEN PRINT "Go to the attic.":EXIT SELECT
     IF FLAG(10)=0 THEN ROOMOBJECT$(8,1)="LATCHFREE":_
        PRINT "Dust clears, revealing a can of LATCHFREE spray.":_
        EXIT SELECT
     PRINT "That's nothing to sneeze at.":EXIT SELECT

   CASE "FLIP"
      PRINT "HEADS!..... You lose.":EXIT SELECT

    CASE "BREAK"
      PRINT "Temper......Temper...":EXIT SELECT

     CASE "CLIMB"
        PRINT "Try-  GO UP ":EXIT SELECT

      CASE "STICK"
         PRINT "Try-    PICK LOCK ":EXIT SELECT

      CASE "JUMP"
         PRINT "Come back down to earth!":EXIT SELECT

      CASE "PHONE","TELEPHONE"
         PRINT "Try-   READ SIGN ":EXIT SELECT

      CASE "ANSWER"
         IF FLAG(7)=0 AND FLAG(5)=1 AND ROOM=5 THEN FLAG(7)=1:_
            PRINT "A voice says -- Go to attic and  SNEEZE LOUDLY ":_
            EXIT SELECT
         PRINT "Not now or not here":EXIT SELECT

      CASE "CALL"
         PRINT "That didn't work ... Try something else.":EXIT SELECT

      CASE "USE"
         IF ROOM=4 THEN PRINT "Try-   MOP FLOOR ":EXIT SELECT
         PRINT "Try something more specific, like PICK LOCK ":EXIT SELECT

      CASE "DO"
         PRINT "Try-  WASH DISHES ":EXIT SELECT

      CASE "SIT","REST","LIE"
        PRINT "This is no time for rest -- The game is afoot!":EXIT SELECT

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

'MODULE 2.4 UPDATE DATA
SUB UPDATE
END SUB

'MODULE 3 CLOSING
SUB CLOSING
    SHARED FLAG(),TURNNUMBER,ANSWER$(),STRT
    PRINT
   IF FLAG(1)=1 THEN PRINT "You enter the closet to find Anna fast asleep":_
    PRINT "and completely unharmed!  You are so relieved.  Just then,":_
    PRINT "Anna's parents come home and marvel at the clean house!  They":_
    PRINT "Give you an extra $10 and nominate you as babysitter of the year!":_
    PRINT:_
    PRINT " *****   HOORAY FOR YOU!!  YOU'VE WON THE GAME!!  ****"
   FOR I=1 TO 6:PRINT: NEXT I
    INPUT "BE SURE THAT YOUR DISK IS IN THE DRIVE AND PRESS RETURN";DUMMY$

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
    LET DTA(7)=FLAG(1):LET DTA(32)=DTA(32)+INT((TIMER-STRT)/6)

       OPEN "REPORT.DTA" FOR OUTPUT AS #1
         REM SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
         FOR I=1 TO 40
           WRITE #1,DTA(I)
         NEXT I
         FOR I=1 TO 10
           WRITE #1,DTA$(I)
         NEXT I
       CLOSE #1

    OPEN "TOWNDATA.TXT" FOR APPEND AS #2
       PRINT #2,TIME$
       FOR I=0 TO TURNNUMBER
          IF I/5=INT(I/5) THEN PRINT #2,ANSWER$(I) ELSE PRINT #2,ANSWER$(I),
       NEXT I
    PRINT #2,INT((STRT-TIMER)/6)
    CLOSE #2
    PRINT
    PRINT "This game is over.  Type  TOWNHOUSE  to play again."
END SUB

'END OF PROGRAM
