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
'7-3-23, Henrik Aasman: Modified for QB64
'  Moved all definition of global (shared) variables to Main module
'  QB64 needs unique names so MOVEMENTTABLE --> MOVEMENTTABLESUB and INVENTORY --> INVENTORYSUB
'  EXIT LOOP changed to EXIT DO
'  Changed syntax for BEEP so BEEP 3 --> BEEP
'  Changed syntax for DELAY so DELAY --> _DELAY
'  Fixed __ to _
'  Renamed DEF to FUNCTION
'  Removed reading/writing to REPORT.DTA
'  DIMension WURD$ to 8.
'  Use CONVERTNUM in SUB COMMAND

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
ReDim Shared TOTAL
ReDim Shared CONVERTNUM
ReDim Shared VERB$
ReDim Shared OBJECT$
ReDim Shared C$
ReDim Shared DIRECTION
ReDim Shared ITEMNUMBER
ReDim Shared WURD$(8)
ReDim Shared GUESSNUMB
ReDim Shared LOGIC$
ReDim Shared STRT


Let STRT = Timer
Cls: ReDim FLAG(30), OBJ$(18), OBJ2$(18)
Let GUESSNUMB = 0: Let WURD$(2) = "GIRL": Let LOGIC$ = ""
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
    Data 8,ENTRYWAY,JUNK HEAP,UPPER HALL,GUARD ROOM,DESIGN ROOM,LOWER HALL
    Data STORAGE ROOM,MEETING ROOM

    For I = 1 To NUMBERROOMS
        Let DESCRIBEFLAG(I) = 1
        Let ROOMDESCRIBE$(I) = "EMPTY"
        Let ROOMDES2$(I) = "EMPTY"
    Next I

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
    Data 0,3,0,0,0,0
    Data 0,0,3,0,0,0
    Data 1,0,4,2,0,6
    Data 0,0,0,3,0,0
    Data 0,0,6,0,0,0

    Data 0,8,7,5,3,0
    Data 0,0,0,6,0,0
    Data 6,0,0,0,0,0

End Sub

'MODULE 1.4 INVENTORY
Sub INVENTORYSUB
    Shared INVENTORY$()
    ReDim INVENTORY$(5)
    For I = 1 To 5
        Let INVENTORY$(I) = "EMPTY"
    Next I
End Sub

'MODULE 1.5 OTHERS
Sub OTHERS
    SHARED GAMESTATE$,ROOM,TURNNUMBER,ROOMOBJECT$(),THING$,INVENTORY$(),_
        FLAG(),OBJ$(),OBJ2$(),DESCRIBEFLAG(),ROOMDESCRIBE$(),ROOMDES2$(),_
        CONVERTNUM
    Let ROOM = 1: Rem START IN ENTRYWAY
    Let TURNNUMBER = 0
    Let TOTAL = 0 'TOTAL NUMBER OF CENTS DROPPEN IN ENTRYWAY
    Let THING$ = ""
    For I = 1 To 30: Let FLAG(I) = 0: Next I 'SET FLAGS TO 0
    'FLAG #  MEANING OF 0      FLAG# MEANING OF 0
    ' 1      GAME NOT OVER       2   ANDREA SIGN NOT READ YET
    'FOR 1, 1 MEANS GAME WON -1 GAME LOST 2 MEANS GAME QUIT
    '13 GUARD ROOM NOT ENTERED FOR FIRST TIME
    Let ROOMOBJECT$(1, 1) = "SIGN*"
    Let ROOMOBJECT$(2, 1) = "ANDREA*"
    Let ROOMOBJECT$(2, 2) = "SIGN*"
    Let ROOMOBJECT$(3, 1) = "EMPTYBALL"
    Let ROOMOBJECT$(4, 1) = "GUARD*"
    Let ROOMOBJECT$(4, 2) = "EMPTYCHARGER"
    Let ROOMOBJECT$(4, 3) = "SIGN*"
    Let ROOMOBJECT$(5, 1) = "EMPTYDOLL"
    Let ROOMOBJECT$(7, 1) = "EMPTYCAPGUN"
    Let ROOMOBJECT$(8, 1) = "EMPTYJUMPROPE"
    Let ROOMOBJECT$(8, 2) = "SIGN*"

    Let CONVERTNUM = 3
    For I = 1 To CONVERTNUM 'READ IN WORD!* CONVERSION
        Read OBJ$(I), OBJ2$(I)
    Next I
    Data SIGN,SIGN*,GUARD,GUARD*,ANDREA,ANDREA*


End Sub

'MODULE 2 TURN
Sub TURN
    Shared FLAG(), TURNNUMBER, TOTAL
    Do Until FLAG(1) <> 0
        If TOTAL = 41 Then FLAG(1) = 1: Exit Do
        FLAG(14) = 0
        TURNNUMBER = TURNNUMBER + 1
        IF TURNNUMBER=295 THEN BEEP:_
           PRINT "Type   SAVE GAME  and then  QUIT":_
           PRINT "You can then return to this spot by typing":_
           PRINT "TOYLAND (at the DOS prompt) and later,":_
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
        ROOMDES2$()

    If TURNNUMBER = 1 Then Call GAMESTARTER 'MODULE 2.1.1
    If DESCRIBEFLAG(ROOM) = 1 Then Print "You are in the  "; DESCRIPTION$(ROOM)
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
     IF ROOM=4 AND FLAG(13)=0 THEN PRINT "The GUARD* is pointing to a SIGN*.  Perhaps we should":_
       PRINT "READ SIGN":FLAG(13)=1
     IF ROOM=2 AND FLAG(3)=0 THEN _
       PRINT "There is a forlorn-looking android here.  Her nametag":_
       PRINT "reads 'ANDREA - Model 1'.  A sticker attached to her reads":_
       PRINT "'not mobile - low batteries - JUNK'.  I'll bet if you charged":_
       PRINT "her batteries, she'd be as good as new.  There is a message":_
       PRINT "on her screen, perhaps I should READ SCREEN ":FLAG(3)=1
    Print
End Sub

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
Sub GAMESTARTER
    Cls
    Print "      Welcome to the TOYLAND Adventure!"
    Print
    Print "    You have wandered into the TOYLAND factory which uses"
    Print "Model 1-type androids to do the design and manufacturing."
    Print "As you may recall, the Model 1 is very helpful but has "
    Print "some memory problems.  Luckily, if you help them find the"
    Print "word they're trying to think of, they can usually think "
    Print "of a good suggestion for you to try.  "
    Print "    Because the Model 2 is cheaper and lacks these problems,"
    Print "Model 1's are junked as soon as they show signs of wear.  "
    Print "This is too bad, because an android would be a big help to"
    Print "you in your adventures.  Perhaps you can find one to rescue."
    Print "I've heard that the android named ANDREA is nice."
    Print
    Print "    Give COMMANDs as verb then object, such as GO NORTH,"
    Print "READ SIGN, GET CHARGER, CHARGE BATTERIES, DROP JUMPROPE,"
    Print "REVEAL CAPGUN, QUIT GAME, SAVE GAME, and RESTORE GAME."
    Print "My vocabulary is limited, but you may not need any other"
    Print "verbs in this, your first adventure."
    Print "    Exceptions to this two-word sentence rule are single-"
    Print "letter commands such as N to GO NORTH (not GO N), U to GO UP, "
    Print "and Q to QUIT."
    Print "    P.S. Don't try to GET objects ending in an *, e.g., SIGN*,"
    Print "as they are quite unobtainable-- you get my drift?"
    Print "If you're through taking notes, press the ENTER key to begin"
    Do While Len(InKey$) = 0: Loop
    Cls
    Print "Here is a map of THE FACTORY: (You won't see it again)"
    Print
    Print "                              ************"
    Print "You start in the ENTRY WAY->  *  ENTRY   * "
    Print "                              *   WAY    *  "
    Print "                              ************   "
    Print "                                   :          "
    Print "               ************   ************   ************"
    Print "               *  JUNK    *<->*  UPPER   *<->*  GUARD   *"
    Print "               *  HEAP    *   *   HALL   *   *  ROOM    *"
    Print "               ************  /************   ************"
    Print "                            /"
    Print "                           /"
    Print "************   ************   ************"
    Print "* DESIGN    *   *  LOWER   *   * STORAGE  *"
    Print "*  ROOM     *<->*   HALL   *<->*   ROOM   *"
    Print "************   ************   ************"
    Print "                    :                      "
    Print "               ************       DIRECTIONS:"
    Print "               *  MEETING *           NORTH             UP"
    Print "               *   ROOM   *             :              /"
    Print "               ************      WEST<- : ->EAST   DOWN"
    Print "                                        :"
    Print "  Push the ENTER key to continue:     SOUTH"
    Do While Len(InKey$) = 0: Loop
    Cls
End Sub

'MODULE 2.1.2 ERASER ERASES ONE LINE OF TEXT
Sub ERASER
    Rem  FOR I=1 TO 12:PRINT "          ";:NEXT I
End Sub


'MODULE 2.2 COMMANDS
Sub COMMANDS
    SHARED VERB$, OBJECT$,ROOM,DESCRIPTION$(),FLAG(),INVENTORY$(),_
        ROOMOBJECT$(),OBJ$(),OBJ2$(),C$,ANSWER$(),TURNNUMBER,CONVERTNUM

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

        Let C$ = UCase$(C$)

        Let I = 1 'LETTER COUNTER

        Do While Not (Asc(Mid$(C$, I, 1)) = 32) And Not (I = Len(C$))
            'LOOP TO LOOK FOR FIRST SPACE
            Let I = I + 1 'WHICH SHOULD DENOTE END OF FIRST
        Loop 'WORD

        Let VERB$ = Mid$(C$, 1, I) 'ASSIGN THOSE CHARACTERS TO VERB$

        Let ANSWER$(TURNNUMBER) = C$

        Let OBJECT$ = Mid$(C$, I + 1, Len(C$)) 'ASSIGN REST OF SENTENCE TO OBJECT$


        If Len(OBJECT$) <> 0 Then VERB$ = Left$(VERB$, Len(VERB$) - 1)
        For J = 1 To CONVERTNUM 'CONVERT OBJECT$ LACKING * OR !
            If OBJECT$ = OBJ$(J) Then Let OBJECT$ = OBJ2$(J)
        Next J
        'CHECK FOR EXTRA WORDS
        COUNTER = 0
        For J = 1 To Len(OBJECT$)
            Let DUMMY$ = Mid$(OBJECT$, J, 1)
            If DUMMY$ = Chr$(32) Then COUNTER = COUNTER + 1
        Next J
    IF COUNTER<>0 THEN PRINT "One space (two words) only, please." _
      ELSE DUMMY2=1

    Loop
End Sub


'MODULE 2.3 EVALUATE THE COMMANDS
Sub EVALUATE
    SHARED VERB$, OBJECT$,ROOM,DIRECTION,MOVEMENTTABLE(),NUMBERROOMS,_
        ROOMOBJECT$(),ITEMNUMBER,TURNNUMBER,THING$,INVENTORY$(),FLAG(),_
        TOTAL,WURD$(),GUESSNUMB,LOGIC$
    If FLAG(1) <> 0 Then Exit Sub
    If FLAG(14) = 1 Then Exit Sub
    Select Case VERB$
        Case "QUIT", "Q"
            Let FLAG(1) = 2 'GAME NOT WON(1) OR LOST(-1):EXIT SELECT
        Case "SAVE"
            If OBJECT$ <> "GAME" Then Print "SAVE GAME": Exit Select
            Open "TOYSAV.BAS" For Output As #1

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
            Write #1, GUESSNUMB, LOGIC$, TOTAL
            Close #1
            Print "OK"
            Exit Select
        Case "RESTORE", "LOAD"
            If OBJECT$ <> "GAME" Then Print "RESTORE GAME": Exit Select
            Open "TOYSAV.BAS" For Input As #1
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
            Input #1, GUESSNUMB, LOGIC$, TOTAL
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


            IF MOVEMENTTABLE (ROOM,DIRECTION) = 0 THEN _
                PRINT "You can't GO that way" ELSE _
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
            Exit Select

        Case "GET", "TAKE", "PLUCK", "EXTRACT", "PULL"

            IF OBJECT$="TOY" OR OBJECT$="TOYS" THEN PRINT _
              "Be more specific, e.g., GET DOLL":EXIT SELECT
            If FNPRESENT = 0 Then Print "I don't see  "; OBJECT$: Exit Select
            If Right$(OBJECT$, 1) = "*" Then Print "I can't handle it": Exit Select


            For I = 1 To 5
                IF INVENTORY$(I)="EMPTY" THEN INVENTORY$(I)=OBJECT$:_
                    PRINT "GOT IT":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY":_
                    EXIT SELECT
            Next I
            Print "You're carrying too much.  DROP something."
        Case "PUT", "DROP", "GIVE"

            THING$ = OBJECT$
            If ROOM <> 4 Then Print "Drop them in the GUARD ROOM.": Exit Select
            IF OBJECT$="TOYS" OR OBJECT$="TOY" THEN _
               PRINT "DROP one specific TOY at a time e.g., DROP DOLL":_
               EXIT SELECT

            If FNCARRY = 0 Then Print "You don't have the "; OBJECT$: Exit Select

           TOTAL=TOTAL + 1: IF TOTAL=4 THEN ROOMOBJECT$(4,2)="CHARGER":_
             PRINT "You can now take the charger to Andrea and CHARGE BATTERIES"

            For J = 1 To 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":_
                    EXIT SELECT
            Next J
            Print "This room is full, take it elsewhere": Exit Select

        Case "BORROW"
            Print "Try  GET CHARGER": Exit Select

        Case "READ"

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
            Print "You are in the wrong room for that.": Exit Select

        Case "USE"
            Print "Try   CHARGE BATTERIES": Exit Select
        Case "GUESS"
        IF ROOM<>2  THEN PRINT _
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
        PRINT "Andrea says that ";HITS;" letter(s) of your":PRINT _
              "word are also found in her word.  Try again."
            Exit Select
        Case "REVEAL"
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
            Print "Nothing happened.  Are we in the right spot?"
            Exit Select
        Case "CHARGE"
        IF ROOM<>2 THEN PRINT _
          "Take the CHARGER  to Andrea ":EXIT SELECT
            THING$ = "CHARGER"
          IF FNCARRY=0 THEN PRINT_
            "You don't have the CHARGER.  Try GET CHARGER":EXIT SELECT
        PRINT "Andrea leaps up with renewed energy!":PRINT:_
          FLAG(1)=1:EXIT SELECT

        Case Else
            Print "I don't know that VERB": Exit Select
    End Select
End Sub
Sub MESSAGE
    Shared ROOM
  IF ROOM=2 THEN PRINT _
     "Andrea says to GO to the UPPER HALL and REVEAL BALL":PRINT _
     " then GO to the DESIGN ROOM and REVEAL DOLL,":PRINT_
     "then GO to the STORAGE ROOM and REVEAL CAPGUN,":PRINT_
     "then GO to the MEETING ROOM and REVEAL JUMPROPE"
End Sub

'MODULE 2.3.1 DEFINE THE FUNCTION- IS THE OBJECT PRESENT?
Function FNPRESENT
    Shared ROOMOBJECT$(), OBJECT$, ITEMNUMBER
    For J = 1 To 15
             IF ROOMOBJECT$(ROOM,J)=OBJECT$ THEN _
                 FNPRESENT=1:ITEMNUMBER=J:EXIT FUNCTION
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
    Shared FLAG(), TURNNUMBER, ANSWER$(), GUESSNUMB, LOGIC$, STRT
    Print
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
    Print
    For I = 1 To 5: Print: Next I
    Input "Be sure that your disk is in the drive and press ENTER"; DUMMY$

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

    '    Let DTA(6) = FLAG(1): Let DTA(31) = DTA(31) + Int((Timer - STRT) / 6)
    '    Open "REPORT.DTA" For Output As #1
    '    Rem SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
    '    For I = 1 To 40
    '    Write #1, DTA(I)
    '    Next I
    '    For I = 1 To 10
    '        Write #1, DTA$(I)
    '    Next I
    '    Close #1

    Open "TOYDATA.TXT" For Append As #2
    Print #2, Time$, GUESSNUMB, LOGIC$
    For I = 0 To TURNNUMBER
        If Int(I / 5) = I / 5 Then Print #2, ANSWER$(I) Else Print #2, ANSWER$(I),
    Next I
    Close #2
    Print "This game is over.  Type TOYLAND to play again."
End Sub

'END OF PROGRAM


