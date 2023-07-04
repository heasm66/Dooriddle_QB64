    'MODULE 0 MAIN    ******TROLL******8-31-90
'REVISED 1-24-91 TO ADD NICKLE-NICKEL! AND CONVERTNUM
'AND TO CHANGE GAMEDTA.BAS TO TROLSAV.BAS
'CHANGED SAVE GAME TIME$ DATE$ 7-30-95
'snips added 11-27-95
'changed int(i/4) for troldata.bas on 12=4=95
'REPORT STUFF 3-5-96   TROLDATA.TXT ON 10-12-96
    LET START=TIMER
    CLS:DIM FLAG(30),OBJ$(18),OBJ2$(18)
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
    DATA 8,ENTRYWAY,BROOM CLOSET,UPPER HALL,VAULT,DUST CLOSET,LOWER HALL
    DATA DEN, GAMEROOM

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
    DATA 0,0,3,0,0,0
    DATA 1,0,4,2,0,6
    DATA 0,0,0,3,0,0
    DATA 0,0,6,0,0,0

    DATA 0,8,7,5,3,0
    DATA 0,0,0,6,0,0
    DATA 6,0,0,0,0,0

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
    LET ROOM = 1               :REM START IN ENTRYWAY
    LET TURNNUMBER =0
    LET TOTAL=0                 'TOTAL NUMBER OF CENTS DROPPEN IN ENTRYWAY
    LET THING$=""
    FOR I=1 TO 30:LET FLAG(I)=0:NEXT I      'SET FLAGS TO 0
          'FLAG #  MEANING OF 0      FLAG# MEANING OF 0
          ' 1      GAME NOT OVER       2   KEY NOT USED IN UPPER HALL(3)
          'FOR 1, 1 MEANS GAME WON -1 GAME LOST 2 MEANS GAME QUIT

          ' 3 BROOM NOT USED IN DUST CLOSET

    LET ROOMOBJECT$(1,1)="SIGN*"
    LET ROOMOBJECT$(2,1)="BROOM"
    LET ROOMOBJECT$(3,1)="LOCKED DOOR*"
    LET ROOMOBJECT$(4,1)="PENNY!"
    LET ROOMOBJECT$(5,1)="SIGN*"
    LET ROOMOBJECT$(5,2)="EMPTYKEY"
    LET ROOMOBJECT$(6,1)="QUARTER!"
    LET ROOMOBJECT$(7,1)="NICKEL!"
    LET ROOMOBJECT$(8,1)="DIME!"

    LET CONVERTNUM=6
    FOR I=1 TO CONVERTNUM              'READ IN WORD!* CONVERSION
        READ OBJ$(I),OBJ2$(I)
    NEXT I
    DATA SIGN,SIGN*,PENNY,PENNY!,NICKEL,NICKEL!,DIME,DIME!
    DATA QUARTER,QUARTER!,NICKLE,NICKEL!


END SUB

'MODULE 2 TURN
SUB TURN
    SHARED FLAG(),TURNNUMBER,TOTAL
    DO UNTIL FLAG(1) <> 0
        IF TOTAL=41 THEN FLAG(1)=1:EXIT LOOP
        FLAG(14)=0
        TURNNUMBER = TURNNUMBER + 1
        IF TURNNUMBER=295 THEN BEEP 3:_
           PRINT "Type   SAVE GAME  and then  QUIT":_
           PRINT "You can then return to this spot by typing":_
           PRINT "TROLL (at the DOS prompt) and later,":_
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
END SUB

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
SUB GAMESTARTER
     CLS
     PRINT "      Welcome to the Cave of the TROLL Adventure!"
     PRINT
     PRINT "    Woe is you!  You were at a carnival and entered"
     PRINT "the CAVE OF THE TROLL because the admission was free!"
     PRINT "Little did you know that you would have to pay to get"
     PRINT "out!  And you haven't a cent!!!"
     PRINT ""
     PRINT "    Your task is to find some money, pay the troll the toll,"
     PRINT "and return to your dull, but safe existence!  Luckily, you"
     PRINT "can find some coins in the cave.  Trolls, having no pockets,"
     PRINT "drop coins everywhere.  So, look around and maybe you can find"
     PRINT "the 41 cents you'll need to get out."
     PRINT
     PRINT "    Give COMMANDs as verb then object, such as GO NORTH,"
     PRINT "READ SIGN, GET QUARTER, DROP DIME, USE BROOM, and"
     PRINT "so forth.  By the way, these are the only five verbs you"
     PRINT "will need to use in this your first adventure."
     PRINT "    Exceptions to this two-word sentence rile are single-"
     PRINT "letter commands such as N to GO NORTH (not GO N), U to GO UP, "
     PRINT "and Q to QUIT."
     PRINT "    P.S. Don't try to GET objects ending in an *, e.g., SIGN*,"
     PRINT "as they are quite unobtainable-- you get my drift?"
     PRINT "If you're through taking notes, press the ENTER key to begin"
     DO WHILE LEN(INKEY$)=0:LOOP
     CLS
     PRINT "Here is a map of THE CAVE: (You won't see it again)"
     PRINT
     PRINT "                              ************"
     PRINT "You start in the ENTRY WAY->  *  ENTRY   * "
     PRINT "                              *   WAY    *  "
     PRINT "                              ************   "
     PRINT "                                   :          "
     PRINT "               ************   ************   ************"
     PRINT "               *  BROOM   *<->*  UPPER   *<->*  VAULT   *"
     PRINT "               * CLOSET   *   *   HALL   *   *          *"
     PRINT "               ************  /************   ************"
     PRINT "                            /"
     PRINT "                           /"
     PRINT "************   ************   ************"
     PRINT "*  DUST    *   *  LOWER   *   *   DEN    *"
     PRINT "* CLOSET   *<->*   HALL   *<->*          *"
     PRINT "************   ************   ************"
     PRINT "                    :                      "
     PRINT "               ************       DIRECTIONS:"
     PRINT "               *  GAME    *           NORTH             UP"
     PRINT "               *  ROOM    *             :              /"
     PRINT "               ************      WEST<- : ->EAST   DOWN"
     PRINT "                                        :"
     PRINT "  Push the ENTER key to continue:     SOUTH"
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
        TOTAL
    IF FLAG(1)<>0 THEN EXIT SUB
    IF FLAG(14)=1 THEN EXIT SUB
    SELECT CASE VERB$
        CASE "QUIT","Q"
            LET FLAG(1)=2   'GAME NOT WON(1) OR LOST(-1):EXIT SELECT
        CASE "SAVE"
            IF OBJECT$<>"GAME" THEN PRINT "SAVE GAME":EXIT SELECT
            OPEN "TROLSAV.BAS" FOR OUTPUT AS #1

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
        CASE "RESTORE", "LOAD"
            IF OBJECT$<> "GAME" THEN PRINT "RESTORE GAME":EXIT SELECT
            OPEN "TROLSAV.BAS" FOR INPUT AS #1
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

            IF ROOM=3 AND DIRECTION=3 AND FLAG(2)=0 THEN_
             PRINT "Can't enter VAULT -- Door locked!":EXIT SELECT

            IF MOVEMENTTABLE (ROOM,DIRECTION) = 0 THEN _
                PRINT "You can't GO that way" ELSE _
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
            EXIT SELECT

        CASE "GET","TAKE","PLUCK","EXTRACT","PULL"

            IF ROOM=5 AND OBJECT$="DUST" THEN _
              PRINT "Try- USE BROOM.":EXIT SELECT

            IF  FNPRESENT = 0 THEN PRINT "I don't see  ";OBJECT$:EXIT SELECT
            IF RIGHT$(OBJECT$,1)="*" THEN PRINT "I can't handle it":EXIT SELECT


            FOR I=1 TO 5
                IF INVENTORY$(I)="EMPTY" THEN INVENTORY$(I)=OBJECT$:_
                    PRINT "GOT IT":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY":_
                    EXIT SELECT
            NEXT I
            PRINT "You're carrying too much.  DROP something."
        CASE "PUT","DROP","GIVE"

            THING$=OBJECT$

            IF OBJECT$="COIN" OR OBJECT$="COINS" OR OBJECT$="MONEY" THEN _
               PRINT "Drop one coin at a time e.g., DROP DIME":EXIT SELECT

            IF FNCARRY=0 THEN PRINT "You don't have the ";OBJECT$:EXIT SELECT

            IF ROOM=1 AND OBJECT$="QUARTER!" THEN TOTAL=TOTAL+25:_
               INVENTORY$(ITEMNUMBER)="EMPTY":PRINT "OK":EXIT SELECT
            IF ROOM=1 AND OBJECT$="DIME!" THEN TOTAL=TOTAL+10:_
               INVENTORY$(ITEMNUMBER)="EMPTY":PRINT "OK":EXIT SELECT
            IF ROOM=1 AND OBJECT$="NICKEL!" THEN TOTAL=TOTAL+5:_
               INVENTORY$(ITEMNUMBER)="EMPTY":PRINT "OK":EXIT SELECT
            IF ROOM=1 AND OBJECT$="PENNY!" THEN TOTAL=TOTAL+1:_
               INVENTORY$(ITEMNUMBER)="EMPTY":PRINT "OK":EXIT SELECT


            FOR J=1 TO 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":_
                    EXIT SELECT
            NEXT J
            PRINT "This room is full, take it elsewhere":EXIT SELECT


        CASE "READ"

            IF ROOM=1 THEN PRINT "Bring coins here and DROP.":_
               PRINT "Total so far -- ";TOTAL;" cents.":EXIT SELECT
            IF ROOM=5 THEN PRINT "USE BROOM here from time to time.":_
               EXIT SELECT
            PRINT "You are in the wrong room for that.":EXIT SELECT

      CASE "USE"
            THING$=OBJECT$
              IF FNCARRY=0 THEN PRINT "You don't have it!":EXIT SELECT
            IF OBJECT$="BROOM" AND ROOM<>5 THEN PRINT "Not here":_
               EXIT SELECT
            IF OBJECT$="BROOM" AND FLAG(3)=1 THEN PRINT "Not again":_
               EXIT SELECT
            IF OBJECT$="BROOM" THEN FLAG(3)=1:_
               PRINT "You see a KEY on the clean floor!":_
               ROOMOBJECT$(5,2)="KEY":EXIT SELECT
            IF OBJECT$<>"KEY" THEN PRINT "You can't use that!":EXIT SELECT
            IF ROOM<>3 THEN PRINT "Not here!":EXIT SELECT
            IF FLAG(2)=1 THEN PRINT "Not again!":EXIT SELECT
            FLAG(2)=1:PRINT "You can now GO EAST into the vault!":_
               ROOMOBJECT$(3,1)="OPEN DOOR*":EXIT SELECT

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
    SHARED FLAG(),TURNNUMBER,ANSWER$(),START
    PRINT
   IF FLAG(1)=1 THEN CLS:PRINT:PRINT:_
    PRINT "   The crafty, but honest (for a TROLL) TROLL":_
    PRINT "Takes your 41 cents and says you are free to go!":_
    PRINT :_
    PRINT "   You have escaped from THE CAVE OF THE TROLL!":_
    PRINT :_
    PRINT "      HOORAY FOR YOU!!!!":_
    PRINT "":_
    PRINT "   Now you are ready for THE TOWNHOUSE ADVENTURE! ":_
    PRINT:_
    PRINT " *****   WOW!!  IS THIS EXCITING OR WHAT??  ****"
    PRINT
    FOR I=1 TO 5:PRINT:NEXT I
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
      LET DTA(5)=FLAG(1):LET DTA(30)=DTA(30)+INT((TIMER-START)/6)

       OPEN "REPORT.DTA" FOR OUTPUT AS #1
         REM SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
         FOR I=1 TO 40
           WRITE #1,DTA(I)
         NEXT I
         FOR I=1 TO 10
           WRITE #1,DTA$(I)
         NEXT I
       CLOSE #1


    OPEN "TROLDATA.TXT" FOR APPEND AS #2
       PRINT #2,TIME$
      FOR I=0 TO TURNNUMBER
          IF INT(I/5)=I/5 THEN PRINT #2,ANSWER$(I) ELSE PRINT #2,ANSWER$(I),
      NEXT I
      PRINT #2,ANSWER$(0)
    CLOSE #2
    PRINT "This game is over.  Type TROLL to play again."
END SUB

'END OF PROGRAM
