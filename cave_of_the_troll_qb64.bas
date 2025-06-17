' QB64 - Meta-command that allows window to be resized
$Resize:Stretch

'MODULE 0 MAIN    ******TROLL******8-31-90
'REVISED 1-24-91 TO ADD NICKLE-NICKEL! AND CONVERTNUM
'AND TO CHANGE GAMEDTA.BAS TO TROLSAV.BAS
'CHANGED SAVE GAME TIME$ DATE$ 7-30-95
'snips added 11-27-95
'changed int(i/4) for troldata.bas on 12=4=95
'REPORT STUFF 3-5-96   TROLDATA.TXT ON 10-12-96
'7-1-23, Henrik Aasman: Modified for QB64
'  Moved all definition of global (shared) variables to Main module
'  QB64 needs unique names so MOVEMENTTABLE --> MOVEMENTTABLESUB and INVENTORY --> INVENTORYSUB
'  EXIT LOOP changed to EXIT DO
'  Changed syntax for BEEP so BEEP 3 --> BEEP
'  Changed syntax for DELAY so DELAY --> _DELAY
'  Fixed __ to _
'  Renamed DEF to FUNCTION
'  Removed reading/writing to REPORT.DTA

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
ReDim Shared VERB$
ReDim Shared OBJECT$
ReDim Shared C$
ReDim Shared CONVERTNUM
ReDim Shared DIRECTION
ReDim Shared ITEMNUMBER
ReDim Shared START

Let START = Timer
Cls: ReDim FLAG(30), OBJ$(18), OBJ2$(18)
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
    Call ROOMINFO '          MODULE 1.1 READ ROOM INFORMATION
    Call ROOMOBJECTS '       MODULE 1.2 READ ROOM OBJECTS
    Call MOVEMENTTABLESUB '  MODULE 1.3 READ MOVEMENT TABLE
    Call INVENTORYSUB '      MODULE 1.4 INITIALIZE INVENTORY ARRAY
    '                        TO AN EMPTY LIST
    Call OTHERS '            MODULE 1.5 INITIALIZE OTHER VARIABLES
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
    Data 8,ENTRYWAY,BROOM CLOSET,UPPER HALL,VAULT,DUST CLOSET,LOWER HALL
    Data DEN,GAMEROOM

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
    ' 1      GAME NOT OVER       2   KEY NOT USED IN UPPER HALL(3)
    'FOR 1, 1 MEANS GAME WON -1 GAME LOST 2 MEANS GAME QUIT

    ' 3 BROOM NOT USED IN DUST CLOSET

    Let ROOMOBJECT$(1, 1) = "SIGN*"
    Let ROOMOBJECT$(2, 1) = "BROOM"
    Let ROOMOBJECT$(3, 1) = "LOCKED DOOR*"
    Let ROOMOBJECT$(4, 1) = "PENNY!"
    Let ROOMOBJECT$(5, 1) = "SIGN*"
    Let ROOMOBJECT$(5, 2) = "EMPTYKEY"
    Let ROOMOBJECT$(6, 1) = "QUARTER!"
    Let ROOMOBJECT$(7, 1) = "NICKEL!"
    Let ROOMOBJECT$(8, 1) = "DIME!"

    Let CONVERTNUM = 6
    For I = 1 To CONVERTNUM 'READ IN WORD!* CONVERSION
        Read OBJ$(I), OBJ2$(I)
    Next I
    Data SIGN,SIGN*,PENNY,PENNY!,NICKEL,NICKEL!,DIME,DIME!
    Data QUARTER,QUARTER!,NICKLE,NICKEL!


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
           PRINT "TROLL (at the DOS prompt) and later,":_
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
    Print
End Sub

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
Sub GAMESTARTER
    Cls
    Print "      Welcome to the Cave of the TROLL Adventure!"
    Print
    Print "    Woe is you!  You were at a carnival and entered"
    Print "the CAVE OF THE TROLL because the admission was free!"
    Print "Little did you know that you would have to pay to get"
    Print "out!  And you haven't a cent!!!"
    Print ""
    Print "    Your task is to find some money, pay the troll the toll,"
    Print "and return to your dull, but safe existence!  Luckily, you"
    Print "can find some coins in the cave.  Trolls, having no pockets,"
    Print "drop coins everywhere.  So, look around and maybe you can find"
    Print "the 41 cents you'll need to get out."
    Print
    Print "    Give COMMANDs as verb then object, such as GO NORTH,"
    Print "READ SIGN, GET QUARTER, DROP DIME, USE BROOM, and"
    Print "so forth.  By the way, these are the only five verbs you"
    Print "will need to use in this your first adventure."
    Print "    Exceptions to this two-word sentence rile are single-"
    Print "letter commands such as N to GO NORTH (not GO N), U to GO UP, "
    Print "and Q to QUIT."
    Print "    P.S. Don't try to GET objects ending in an *, e.g., SIGN*,"
    Print "as they are quite unobtainable-- you get my drift?"
    Print "If you're through taking notes, press the ENTER key to begin"
    Do While Len(InKey$) = 0: Loop
    Cls
    Print "Here is a map of THE CAVE: (You won't see it again)"
    Print
    Print "                              ************"
    Print "You start in the ENTRY WAY->  *  ENTRY   * "
    Print "                              *   WAY    *  "
    Print "                              ************   "
    Print "                                   :          "
    Print "               ************   ************   ************"
    Print "               *  BROOM   *<->*  UPPER   *<->*  VAULT   *"
    Print "               * CLOSET   *   *   HALL   *   *          *"
    Print "               ************  /************   ************"
    Print "                            /"
    Print "                           /"
    Print "************   ************   ************"
    Print "*  DUST    *   *  LOWER   *   *   DEN    *"
    Print "* CLOSET   *<->*   HALL   *<->*          *"
    Print "************   ************   ************"
    Print "                    :                      "
    Print "               ************       DIRECTIONS:"
    Print "               *  GAME    *           NORTH             UP"
    Print "               *  ROOM    *             :              /"
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
        TOTAL
    If FLAG(1) <> 0 Then Exit Sub
    If FLAG(14) = 1 Then Exit Sub
    Select Case VERB$
        Case "QUIT", "Q"
            Let FLAG(1) = 2 'GAME NOT WON(1) OR LOST(-1):EXIT SELECT
        Case "SAVE"
            If OBJECT$ <> "GAME" Then Print "SAVE GAME": Exit Select
            Open "TROLSAV.BAS" For Output As #1

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
            Write #1, GUESSNUMB, LOGIC$, SLAMMER, TRYNUMB
            Close #1
            Print "OK"
            Exit Select
        Case "RESTORE", "LOAD"
            If OBJECT$ <> "GAME" Then Print "RESTORE GAME": Exit Select
            Open "TROLSAV.BAS" For Input As #1
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

            IF ROOM=3 AND DIRECTION=3 AND FLAG(2)=0 THEN_
             PRINT "Can't enter VAULT -- Door locked!":EXIT SELECT

            IF MOVEMENTTABLE (ROOM,DIRECTION) = 0 THEN _
                PRINT "You can't GO that way" ELSE _
                LET ROOM = MOVEMENTTABLE (ROOM,DIRECTION)
            Exit Select

        Case "GET", "TAKE", "PLUCK", "EXTRACT", "PULL"

            IF ROOM=5 AND OBJECT$="DUST" THEN _
              PRINT "Try- USE BROOM.":EXIT SELECT

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

            IF OBJECT$="COIN" OR OBJECT$="COINS" OR OBJECT$="MONEY" THEN _
               PRINT "Drop one coin at a time e.g., DROP DIME":EXIT SELECT

            If FNCARRY = 0 Then Print "You don't have the "; OBJECT$: Exit Select

            IF ROOM=1 AND OBJECT$="QUARTER!" THEN TOTAL=TOTAL+25:_
               INVENTORY$(ITEMNUMBER)="EMPTY":PRINT "OK":EXIT SELECT
            IF ROOM=1 AND OBJECT$="DIME!" THEN TOTAL=TOTAL+10:_
               INVENTORY$(ITEMNUMBER)="EMPTY":PRINT "OK":EXIT SELECT
            IF ROOM=1 AND OBJECT$="NICKEL!" THEN TOTAL=TOTAL+5:_
               INVENTORY$(ITEMNUMBER)="EMPTY":PRINT "OK":EXIT SELECT
            IF ROOM=1 AND OBJECT$="PENNY!" THEN TOTAL=TOTAL+1:_
               INVENTORY$(ITEMNUMBER)="EMPTY":PRINT "OK":EXIT SELECT


            For J = 1 To 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":_
                    EXIT SELECT
            Next J
            Print "This room is full, take it elsewhere": Exit Select


        Case "READ"

            IF ROOM=1 THEN PRINT "Bring coins here and DROP.":_
               PRINT "Total so far -- ";TOTAL;" cents.":EXIT SELECT
            IF ROOM=5 THEN PRINT "USE BROOM here from time to time.":_
               EXIT SELECT
            Print "You are in the wrong room for that.": Exit Select

        Case "USE"
            THING$ = OBJECT$
            If FNCARRY = 0 Then Print "You don't have it!": Exit Select
            IF OBJECT$="BROOM" AND ROOM<>5 THEN PRINT "Not here":_
               EXIT SELECT
            IF OBJECT$="BROOM" AND FLAG(3)=1 THEN PRINT "Not again":_
               EXIT SELECT
            IF OBJECT$="BROOM" THEN FLAG(3)=1:_
               PRINT "You see a KEY on the clean floor!":_
               ROOMOBJECT$(5,2)="KEY":EXIT SELECT
            If OBJECT$ <> "KEY" Then Print "You can't use that!": Exit Select
            If ROOM <> 3 Then Print "Not here!": Exit Select
            If FLAG(2) = 1 Then Print "Not again!": Exit Select
            FLAG(2)=1:PRINT "You can now GO EAST into the vault!":_
               ROOMOBJECT$(3,1)="OPEN DOOR*":EXIT SELECT

        Case Else
            Print "I don't know that VERB": Exit Select
    End Select
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
    Shared FLAG(), TURNNUMBER, ANSWER$(), START
    Print
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
    '    Let DTA(5) = FLAG(1): Let DTA(30) = DTA(30) + Int((Timer - START) / 6)

    '    Open "REPORT.DTA" For Output As #1
    '    Rem SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
    '    For I = 1 To 40
    '        Write #1, DTA(I)
    '    Next I
    '    For I = 1 To 10
    '        Write #1, DTA$(I)
    '    Next I
    '    Close #1

    Open "TROLDATA.TXT" For Append As #2
    Print #2, Time$
    For I = 0 To TURNNUMBER
        If Int(I / 5) = I / 5 Then Print #2, ANSWER$(I) Else Print #2, ANSWER$(I),
    Next I
    Print #2, ANSWER$(0)
    Close #2
    Print "This game is over.  Type TROLL to play again."
End Sub

'END OF PROGRAM
