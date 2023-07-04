    'MODULE 0 MAIN    ******TROLL  TOYLAND ******8-31-90
'REVISED 1-24-91 TO ADD NICKLE-NICKEL! AND CONVERTNUM
'AND TO CHANGE GAMEDTA.BAS TO TROLSAV.BAS
'CHANGED SAVE GAME TIME$ DATE$ 7-30-95
'snips added 11-27-95
'changed int(i/4) for troldata.bas on 12=4=95
'TOYLAND STARTS ON 12-14-95
'changes as of 1-5-96
'CHANGES AS OF 2-29-96 AND 3-5-96 DTA()
'toydata.txt 10-15-96  changed andrea sign 10-29-96

    LET STRT=TIMER
    CLS:DIM FLAG(30),OBJ$(18),OBJ2$(18)
    LET GUESSNUMB=0:LET WURD$(2)="GIRL":LET LOGIC$=""
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
    DATA 8,ENTRYWAY,JUNK HEAP,UPPER HALL,GUARD ROOM,DESIGN ROOM,LOWER HALL
    DATA STORAGE ROOM,MEETING ROOM

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
          ' 1      GAME NOT OVER       2   ANDREA SIGN NOT READ YET
          'FOR 1, 1 MEANS GAME WON -1 GAME LOST 2 MEANS GAME QUIT
          '13 GUARD ROOM NOT ENTERED FOR FIRST TIME
    LET ROOMOBJECT$(1,1)="SIGN*"
    LET ROOMOBJECT$(2,1)="ANDREA*"
    LET ROOMOBJECT$(2,2)="SIGN*"
    LET ROOMOBJECT$(3,1)="EMPTYBALL"
    LET ROOMOBJECT$(4,1)="GUARD*"
    LET ROOMOBJECT$(4,2)="EMPTYCHARGER"
    LET ROOMOBJECT$(4,3)="SIGN*"
    LET ROOMOBJECT$(5,1)="EMPTYDOLL"
    LET ROOMOBJECT$(7,1)="EMPTYCAPGUN"
    LET ROOMOBJECT$(8,1)="EMPTYJUMPROPE"
    LET ROOMOBJECT$(8,2)="SIGN*"

    LET CONVERTNUM=3
    FOR I=1 TO CONVERTNUM              'READ IN WORD!* CONVERSION
        READ OBJ$(I),OBJ2$(I)
    NEXT I
    DATA SIGN,SIGN*,GUARD,GUARD*,ANDREA,ANDREA*


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
           PRINT "TOYLAND (at the DOS prompt) and later,":_
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
     IF ROOM=4 AND FLAG(13)=0 THEN PRINT "The GUARD* is pointing to a SIGN*.  Perhaps we should":_
       PRINT "READ SIGN":FLAG(13)=1
     IF ROOM=2 AND FLAG(3)=0 THEN _
       PRINT "There is a forlorn-looking android here.  Her nametag":_
       PRINT "reads 'ANDREA - Model 1'.  A sticker attached to her reads":_
       PRINT "'not mobile - low batteries - JUNK'.  I'll bet if you charged":_
       PRINT "her batteries, she'd be as good as new.  There is a message":_
       PRINT "on her screen, perhaps I should READ SCREEN ":FLAG(3)=1
   PRINT
END SUB

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
SUB GAMESTARTER
     CLS
     PRINT "      Welcome to the TOYLAND Adventure!"
     PRINT
     PRINT "    You have wandered into the TOYLAND factory which uses"
     PRINT "Model 1-type androids to do the design and manufacturing."
     PRINT "As you may recall, the Model 1 is very helpful but has "
     PRINT "some memory problems.  Luckily, if you help them find the"
     PRINT "word they're trying to think of, they can usually think "
     PRINT "of a good suggestion for you to try.  "
     PRINT "    Because the Model 2 is cheaper and lacks these problems,"
     PRINT "Model 1's are junked as soon as they show signs of wear.  "
     PRINT "This is too bad, because an android would be a big help to"
     PRINT "you in your adventures.  Perhaps you can find one to rescue."
     PRINT "I've heard that the android named ANDREA is nice."
     PRINT
     PRINT "    Give COMMANDs as verb then object, such as GO NORTH,"
     PRINT "READ SIGN, GET CHARGER, CHARGE BATTERIES, DROP JUMPROPE,"
     PRINT "REVEAL CAPGUN, QUIT GAME, SAVE GAME, and RESTORE GAME."
     PRINT "My vocabulary is limited, but you may not need any other"
     PRINT "verbs in this, your first adventure."
     PRINT "    Exceptions to this two-word sentence rule are single-"
     PRINT "letter commands such as N to GO NORTH (not GO N), U to GO UP, "
     PRINT "and Q to QUIT."
     PRINT "    P.S. Don't try to GET objects ending in an *, e.g., SIGN*,"
     PRINT "as they are quite unobtainable-- you get my drift?"
     PRINT "If you're through taking notes, press the ENTER key to begin"
     DO WHILE LEN(INKEY$)=0:LOOP
     CLS
     PRINT "Here is a map of THE FACTORY: (You won't see it again)"
     PRINT
     PRINT "                              ************"
     PRINT "You start in the ENTRY WAY->  *  ENTRY   * "
     PRINT "                              *   WAY    *  "
     PRINT "                              ************   "
     PRINT "                                   :          "
     PRINT "               ************   ************   ************"
     PRINT "               *  JUNK    *<->*  UPPER   *<->*  GUARD   *"
     PRINT "               *  HEAP    *   *   HALL   *   *  ROOM    *"
     PRINT "               ************  /************   ************"
     PRINT "                            /"
     PRINT "                           /"
     PRINT "************   ************   ************"
     PRINT "* DESIGN    *   *  LOWER   *   * STORAGE  *"
     PRINT "*  ROOM     *<->*   HALL   *<->*   ROOM   *"
     PRINT "************   ************   ************"
     PRINT "                    :                      "
     PRINT "               ************       DIRECTIONS:"
     PRINT "               *  MEETING *           NORTH             UP"
     PRINT "               *   ROOM   *             :              /"
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
        TOTAL,WURD$(),GUESSNUMB,LOGIC$
    IF FLAG(1)<>0 THEN EXIT SUB
    IF FLAG(14)=1 THEN EXIT SUB
    SELECT CASE VERB$
        CASE "QUIT","Q"
            LET FLAG(1)=2   'GAME NOT WON(1) OR LOST(-1):EXIT SELECT
        CASE "SAVE"
            IF OBJECT$<>"GAME" THEN PRINT "SAVE GAME":EXIT SELECT
            OPEN "TOYSAV.BAS" FOR OUTPUT AS #1

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
            WRITE #1,GUESSNUMB,LOGIC$,TOTAL
            CLOSE #1
            PRINT "OK
            EXIT SELECT
        CASE "RESTORE", "LOAD"
            IF OBJECT$<> "GAME" THEN PRINT "RESTORE GAME":EXIT SELECT
            OPEN "TOYSAV.BAS" FOR INPUT AS #1
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
            INPUT #1,GUESSNUMB,LOGIC$,TOTAL
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
                PRINT "You can't GO that way" ELSE _
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
            EXIT SELECT

        CASE "GET","TAKE","PLUCK","EXTRACT","PULL"

            IF OBJECT$="TOY" OR OBJECT$="TOYS" THEN PRINT _
              "Be more specific, e.g., GET DOLL":EXIT SELECT
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
            IF ROOM<>4 THEN PRINT "Drop them in the GUARD ROOM.":EXIT SELECT
            IF OBJECT$="TOYS" OR OBJECT$="TOY" THEN _
               PRINT "DROP one specific TOY at a time e.g., DROP DOLL":_
               EXIT SELECT

            IF FNCARRY=0 THEN PRINT "You don't have the ";OBJECT$:EXIT SELECT

           TOTAL=TOTAL + 1: IF TOTAL=4 THEN ROOMOBJECT$(4,2)="CHARGER":_
             PRINT "You can now take the charger to Andrea and CHARGE BATTERIES"

            FOR J=1 TO 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":_
                    EXIT SELECT
            NEXT J
            PRINT "This room is full, take it elsewhere":EXIT SELECT

        CASE "BORROW"
          PRINT "Try  GET CHARGER":EXIT SELECT

        CASE "READ"

            IF ROOM=1 THEN PRINT "It says: you may only leave if ":_
               PRINT "accompanied by an android.   No exceptions!":EXIT SELECT
            IF ROOM=2 AND FLAG(ROOM)=0 THEN PRINT _
               "It says: I'm thinking of a four-letter word that has something":_
               PRINT "to do with BOY.  If you think you know, type GUESS WART or":_
               PRINT "GUESS COMB or something like that.  I will the compare the ":_
               PRINT "letters in the guess and answer words and tell you how many":_
               PRINT "of your letters are in my word.  HINT: no letter is used more ":_
               PRINT "than once in the answer word.  Good luck!":EXIT SELECT
            IF ROOM=2 AND FLAG(ROOM)=1 THEN PRINT _
               "It says:  BALL  DOLL CAPGUN  JUMPROPE ":EXIT SELECT
            IF ROOM =8 THEN PRINT _
              "It says: If you should GO to the LOWER HALL and REVEAL PUPPET":_
              PRINT "then the DESIGN ROOM is somewhere west of the STORAGE ROOM":_
              EXIT SELECT
            IF ROOM=4 THEN PRINT_
              "It says: if you want to borrow the battery charger, bring":PRINT_
              "4 toys here and DROP them one at a time.":EXIT SELECT
            PRINT "You are in the wrong room for that.":EXIT SELECT

      CASE "USE"
           PRINT "Try   CHARGE BATTERIES":EXIT SELECT
 CASE "GUESS"
        IF ROOM<>2  THEN PRINT _
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
        PRINT "Andrea says that ";HITS;" letter(s) of your":PRINT _
              "word are also found in her word.  Try again."
         EXIT SELECT
      CASE "REVEAL"
         IF ROOM=3 AND OBJECT$="BALL" THEN PRINT_
          "Voila!":ROOMOBJECT$(3,1)="BALL":EXIT SELECT
         IF ROOM=5 AND OBJECT$="DOLL" THEN PRINT_
          "Voila!":ROOMOBJECT$(5,1)="DOLL":EXIT SELECT
         IF ROOM=7 AND OBJECT$="CAPGUN" THEN PRINT_
          "Voila!":ROOMOBJECT$(7,1)="CAPGUN":EXIT SELECT
         IF ROOM=8 AND OBJECT$="JUMPROPE" THEN PRINT_
          "Voila!":ROOMOBJECT$(8,1)="JUMPROPE":EXIT SELECT
         IF OBJECT$="PUPPET" THEN PRINT _
           "Andrea fears you've fallen for an AFFIRMING THE":PRINT_
           "CONSEQUENT fallacy.  Check your text.":_
           LET LOGIC$=LOGIC$+"AFFIRM...":EXIT SELECT
         PRINT "Nothing happened.  Are we in the right spot?"
         EXIT SELECT
      CASE "CHARGE"
        IF ROOM<>2 THEN PRINT _
          "Take the CHARGER  to Andrea ":EXIT SELECT
        THING$="CHARGER"
          IF FNCARRY=0 THEN PRINT_
            "You don't have the CHARGER.  Try GET CHARGER":EXIT SELECT
        PRINT "Andrea leaps up with renewed energy!":PRINT:_
          FLAG(1)=1:EXIT SELECT

      CASE ELSE
            PRINT "I don't know that VERB":EXIT SELECT
    END  SELECT
END SUB
SUB MESSAGE
  SHARED ROOM
  IF ROOM=2 THEN PRINT _
     "Andrea says to GO to the UPPER HALL and REVEAL BALL":PRINT _
     " then GO to the DESIGN ROOM and REVEAL DOLL,":PRINT_
     "then GO to the STORAGE ROOM and REVEAL CAPGUN,":PRINT_
     "then GO to the MEETING ROOM and REVEAL JUMPROPE"
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
    SHARED FLAG(),TURNNUMBER,ANSWER$(),GUESSNUMB,LOGIC$,STRT
    PRINT
   IF FLAG(1)=1 THEN CLS:PRINT:PRINT:_
    PRINT "   You and your charged up new pal, Andrea, walk calmly":_
    PRINT "     out of the toy factory, winning the game!":_
    PRINT :_
    PRINT "   You have escaped from TOYLAND!  What a relief!":_
    PRINT :_
    PRINT "            HOORAY FOR YOU!!!!":_
    PRINT "":_
    PRINT "     Now you are ready for THE next ADVENTURE! ":_
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

    LET DTA(6)=FLAG(1):LET DTA(31)=DTA(31)+INT((TIMER-STRT)/6)
       OPEN "REPORT.DTA" FOR OUTPUT AS #1
         REM SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
         FOR I=1 TO 40
           WRITE #1,DTA(I)
         NEXT I
         FOR I=1 TO 10
           WRITE #1,DTA$(I)
         NEXT I
       CLOSE #1

    OPEN "TOYDATA.TXT" FOR APPEND AS #2
       PRINT #2,TIME$,GUESSNUMB,LOGIC$
      FOR I=0 TO TURNNUMBER
          IF INT(I/5)=I/5 THEN PRINT #2,ANSWER$(I) ELSE PRINT #2,ANSWER$(I),
      NEXT I
    CLOSE #2
    PRINT "This game is over.  Type TOYLAND to play again."
END SUB

'END OF PROGRAM

    
