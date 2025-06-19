' QB64 - Meta-command that allows window to be resized
$Resize:Stretch

'MODULE 0 MAIN    ******TOWNHOUSE*******1-24-91 2-3-94
'CHANGED FROM 7-9-90 VERSION TO ADD LOGON AND CHANGE NAME OF
'THE SAVE GAME FILE AND ADD VARIABLE CONVERTNUM TO KEEP TRACK
'OF THE NUMBER OF TERMS CONVERTED ON INPUT
'CHANGED TIMER &STRT
'CHANGED SAVE GAME TIMEF$ DATE$ 7-30-95
'snips added and int/5 on 12-4-95
'MOP FLOOR DIDN'T CHECK ROOM fixed on 12-18-95  NO MORE MOP-DUSTMOP*
'REPORT STUFF ADDED 3-5-96  TOWNDATA.TXT 10-15-96
'7-2-23, Henrik Aasman: Modified for QB64
'  Moved all definition of global (shared) variables to Main module
'  QB64 needs unique names so MOVEMENTTABLE --> MOVEMENTTABLESUB and INVENTORY --> INVENTORYSUB
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
ReDim Shared CONVERTNUM
ReDim Shared VERB$
ReDim Shared OBJECT$
ReDim Shared C$
ReDim Shared DIRECTION
ReDim Shared ITEMNUMBER
ReDim Shared STRT

Cls: ReDim FLAG(30), OBJ$(18), OBJ2$(18)
STRT = Timer
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
    Data 13,PANTRY,BOTTOM OF THE STAIRWELL,KITCHEN,LOWER HALL,"LIBRARY"
    Data LIVING ROOM,CLOSET,ATTIC,MASTER BEDROOM,UPPER HALL
    Data BATHROOM,TOP OF THE STAIRWELL,SMALL BEDROOM

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
    Data 0,4,0,0,12,0
    Data 1,5,4,0,0,0
    Data 2,6,0,3,0,0
    Data 3,0,6,0,0,0

    Data 4,0,0,5,0,0
    Data 0,0,0,0,0,0
    Data 0,10,0,0,0,0
    Data 7,11,10,0,0,0
    Data 8,12,13,9,0,0

    Data 9,0,0,0,0,0
    Data 10,0,0,0,0,2
    Data 0,0,0,10,0,0



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
    Let ROOM = 6 'START IN LIVING ROOM
    Let TURNNUMBER = 0
    DANGEROUS = 1
    Let THING$ = ""
    For I = 1 To 30: Let FLAG(I) = 0: Next I 'SET FLAGS TO 0
    'FLAG #  MEANING OF 0      FLAG# MEANING OF 0
    ' 1      GAME NOT OVER       2   DISHES NOT WASHED
    'FOR 1, 1 MEANS GAME WON -1 GAME LOST 2 MEANS GAME QUIT

    ' 3 PANTRY DOOR NOT UNLOCKED 4   HALL NOT MOPPED
    ' 5 CLOTHES NOT WASHED       6   LIGHTS NOT ON
    ' 7 TELEPHONE NOT ANSWERED   8   SINK NOT CLEANED
    ' 9 ATTIC DOOR NOT UNLOCKED  10   DUMMY
    '11 CLOTHES NOT PICKED UP

    Let ROOMOBJECT$(1, 1) = "WASHER-DRYER*"
    Let ROOMOBJECT$(1, 2) = "EMPTYCLEAN-CLOTHES*"
    Let ROOMOBJECT$(3, 1) = "DUSTMOP"
    Let ROOMOBJECT$(3, 2) = "EMPTYKEY"
    Let ROOMOBJECT$(3, 3) = "SINK*"
    Let ROOMOBJECT$(4, 1) = "DUST"
    Let ROOMOBJECT$(4, 2) = "EMPTYNOTE"
    Let ROOMOBJECT$(5, 1) = "SIGN*"
    Let ROOMOBJECT$(5, 2) = "TELEPHONE*"
    Let ROOMOBJECT$(6, 1) = "COUCH*"
    Let ROOMOBJECT$(8, 1) = "EMPTYLATCHFREE"
    Let ROOMOBJECT$(9, 1) = "DIRTY-CLOTHES"
    Let ROOMOBJECT$(11, 1) = "SINK*"
    Let ROOMOBJECT$(11, 2) = "EMPTYHAIRPIN"

    Let CONVERTNUM = 18

    For I = 1 To CONVERTNUM 'READ IN WORD!* CONVERSION
        Read OBJ$(I), OBJ2$(I)
    Next I
    Data SIGN,SIGN*,DISHES,DISHES*,TELEPHONE,TELEPHONE*,MOP,DUSTMOP
    Data PHONE,TELEPHONE*,PIN,HAIRPIN,HAIR,HAIRPIN,CLOTHES,DIRTY-CLOTHES
    Data DIRTY,DIRTY-CLOTHES,WASHER,WASHER-DRYER*,DRYER,WASHER-DRYER*
    Data MACHINE,WASHER-DRYER*,CAN,LATCHFREE,LATCH,LATCHFREE
    Data SPRAY,LATCHFREE,DIRTY,DISHES*,SINK,SINK*,COUCH,COUCH*



End Sub

'MODULE 2 TURN
Sub TURN
    Shared FLAG(), TURNNUMBER
    Do Until FLAG(1) <> 0
        FLAG(14) = 0
        TURNNUMBER = TURNNUMBER + 1
        IF TURNNUMBER=295 THEN BEEP:_
           PRINT "Type   SAVE GAME  and then  QUIT":_
           PRINT "You can then return to this spot by typing":_
           PRINT "TOWNHOUSE (at the DOS prompt) and later,":_
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
    'CLOTHES WASHED(5) AND PHONE NOT ANSWERED(7)
    If FLAG(5) = 1 And FLAG(7) = 0 Then Print "The phone is ringing.  ANSWER it!"

    'IN KITCHEN(3) AND DISHES NOT WASHED(2)
    If ROOM = 3 And FLAG(2) = 0 Then Print "The sink is full of dirty dishes."

    'IN HALL(4) AND NOT SWEPT-4-
    If ROOM = 4 And FLAG(4) = 0 Then Print "The floor is covered with dust."

    'CLOTHES NOT PICKED UP(11) AND ROOM=BEDROOM (9)
   IF ROOM=9 AND FLAG(11)=0 THEN PRINT "The floor is littered with dirty clothes.":_
        FLAG(11)=1

    'ROOM=BATHROOM(11) AND SINK NOT CLEANED (8)
    If ROOM = 11 And FLAG(8) = 0 Then Print "The sink is filthy and you should CLEAN SINK"
    Print

End Sub

'MODULE 2.1.1 GAME STARTER -- PRINT OUT INTRODUCTORY REMARKS
Sub GAMESTARTER
    Cls
    Print "          WELCOME TO THE TOWNHOUSE ADVENTURE!"
    Print
    Print "    Mr. and Mrs. Graham have gone out to a movie and left "
    Print "you to babysit their daughter, Anna.  "
    Print
    Print "    Somehow, you have fallen asleep on the living room"
    Print "couch!  In your dream, Mr. and Mrs. Graham came home, found"
    Print "Anna missing, and berated you severely (and rightly so)."
    Print
    Print "    Amid waves of utter humiliation, you awaken suddenly"
    Print "in a cold sweat.  You are on the couch and everything seems"
    Print "fine.  But is it?  You'll soon find out!"
    Print
    Print "    Give COMMANDs as verb then object, such as  GO NORTH ,"
    Print " READ SIGN, ANSWER PHONE, WASH DISHES, MOP FLOOR,"
    Print "PICK LOCK, SPRAY LATCHFREE, and so forth."
    Print
    Print "    Exceptions to this two-word sentence rule are single-"
    Print "letter COMMANDs such as N to GO NORTH, U to GO UP, and"
    Print "so forth.  Remember, not GO N, just N  "
    Print "    p.s. Don't try to GET objects ending in an *, e.g., COUCH*,"
    Print "as they are quite unobtainable-- if you know what I mean."
    Print "If you're through taking notes, press the ENTER key to begin"
    Do While Len(InKey$) = 0: Loop
    Cls
End Sub

'MODULE 2.1.2 ERASER ERASES ONE LINE OF TEXT
Sub ERASER
    Rem FOR I=1 TO 12:PRINT "          ";:NEXT I
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
        For J = 1 To 18 'CONVERT OBJECT$ LACKING * OR !
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
        ROOMOBJECT$(),ITEMNUMBER,TURNNUMBER,THING$,INVENTORY$(),FLAG()
    If FLAG(1) <> 0 Then Exit Sub
    If FLAG(14) = 1 Then Exit Sub
    Select Case VERB$
        Case "QUIT"
            Let FLAG(1) = 2 'GAME NOT WON(1) OR LOST(-1):EXIT SELECT
        Case "SAVE"
            If OBJECT$ <> "GAME" Then Print "SAVE GAME": Exit Select
            Open "TOWNSAV.BAS" For Output As #1
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
            Close #1
            Print "OK"
            Exit Select
        Case "RESTORE", "LOAD"
            If OBJECT$ <> "GAME" Then Print "RESTORE GAME": Exit Select
            Open "TOWNSAV.BAS" For Input As #1
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
            Print "   You carry: ";
            For I = 1 To 5
                IF INVENTORY$(I)<>"EMPTY" THEN COUNTER=COUNTER + 1:_
                    PRINT INVENTORY$(I);"   ";
            Next I
            If COUNTER = 0 Then Print "Nothing at all": Exit Select
            Print: Exit Select

        Case "GO", "MOVE", "N", "S", "E", "W", "U", "D"
            If Len(VERB$) = 1 Then OBJECT$ = VERB$
            If OBJECT$ = "NORTH" Or OBJECT$ = "N" Then DIRECTION = 1
            If OBJECT$ = "SOUTH" Or OBJECT$ = "S" Then DIRECTION = 2
            If OBJECT$ = "EAST" Or OBJECT$ = "E" Then DIRECTION = 3
            If OBJECT$ = "WEST" Or OBJECT$ = "W" Then DIRECTION = 4
            If OBJECT$ = "UP" Or OBJECT$ = "U" Then DIRECTION = 5
            If OBJECT$ = "DOWN" Or OBJECT$ = "D" Then DIRECTION = 6

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
            Exit Select

        Case "GET", "TAKE", "PLUCK", "EXTRACT", "PULL"

            IF ROOM=4 AND OBJECT$="DUST" THEN _
              PRINT "Try- MOP FLOOR ":EXIT SELECT

            If FNPRESENT = 0 Then Print "I don't see  "; OBJECT$: Exit Select
            If Right$(OBJECT$, 1) = "*" Then Print "I can't handle it": Exit Select


            For I = 1 To 5
                IF INVENTORY$(I)="EMPTY" THEN INVENTORY$(I)=OBJECT$:_
                    PRINT "GOT IT":ROOMOBJECT$(ROOM,ITEMNUMBER)="EMPTY":_
                    EXIT SELECT
            Next I
            Print "You're carrying too much.  Drop something."
        Case "PUT", "DROP", "GIVE"

            THING$ = OBJECT$
            If FNCARRY = 0 Then Print "You don't have the "; OBJECT$: Exit Select


            For J = 1 To 15
                IF ROOMOBJECT$(ROOM,J)="EMPTY"THEN ROOMOBJECT$(ROOM,J)=OBJECT$_
                    :LET INVENTORY$(ITEMNUMBER)="EMPTY" :PRINT "OK":_
                    EXIT SELECT
            Next J
            Print "This room is full, take it elsewhere": Exit Select


        Case "READ"
            THING$ = OBJECT$
            IF OBJECT$="NOTE" AND FNCARRY=0 THEN_
               PRINT "GET the NOTE first.":EXIT SELECT
            IF OBJECT$="NOTE" THEN _
               PRINT "It says- CLAP HANDS to turn on the lights.":_
               EXIT SELECT
            IF OBJECT$="SIGN*" THEN PRINT "It says - incoming calls only.":_
               EXIT SELECT

            Print "Try  READ SIGN*  OR  READ NOTE ": Exit Select

        Case "OIL", "LUBRICATE", "GREASE", "SPRAY"
            THING$ = "LATCHFREE"
            If FNCARRY = 0 Then Print "You don't have the LATCHFREE": Exit Select
           IF ROOM=9 THEN PRINT "The latch and door open easily.":_
              FLAG(1)=1:EXIT SELECT
            Print "Not here": Exit Select
        Case "DRINK", "EAT"
            Print "This is not the time to worry about your stomach": Exit Select

        Case "UNLOCK"
            If ROOM = 10 Then Print "Try- PICK LOCK.": Exit Select
            THING$ = "KEY"
              IF FNCARRY=0 THEN PRINT "You'll need a key.":_
                 EXIT SELECT
              IF ROOM=3 AND FLAG(3)=0 THEN PRINT "The pantry door is open!":_
                  FLAG(3)=1:INVENTORY$(ITEMNUMBER)="EMPTY":EXIT SELECT
            Print "No lock here": Exit Select

        Case "PICK"
            If ROOM = 3 Then Print "Try- UNLOCK DOOR": Exit Select
            THING$ = "HAIRPIN"
            If FNCARRY = 0 Then Print "GET the HAIRPIN ": Exit Select
            IF ROOM=10 AND FLAG(9)=0 THEN FLAG(9)=1:_
               PRINT "The attic door is now open!":EXIT SELECT
            Print "This is the wrong place.": Exit Select

        Case "MOP", "DUSTMOP", "DUST", "SWEEP"
            THING$ = "DUSTMOP"
            If FNCARRY = 0 Then Print "GET the DUSTMOP ": Exit Select
            If ROOM <> 4 Then Print "Not here.": Exit Select
         IF ROOM=4 AND FLAG(4)=0 THEN FLAG(4)=1:_
            PRINT "You see a NOTE on the newly cleaned floor!":_
            ROOMOBJECT$(4,2)="NOTE":ROOMOBJECT$(4,1)="EMPTY":EXIT SELECT
            Print "That didn't help.": Exit Select

        Case "CLEAN"
            If ROOM = 3 Then Print "Try- WASH DISHES ": Exit Select
         IF ROOM=11 AND FLAG(8)=0 THEN _
            PRINT "The sink is now clean -- You see something!":_
            ROOMOBJECT$(11,2)="HAIRPIN":FLAG(8)=1:EXIT SELECT
            Print "That didn't help.": Exit Select

        Case "WASH"
        IF OBJECT$="DISHES*" AND ROOM=3 AND FLAG(2)=0 THEN_
           PRINT "Under the now clean dishes you see something!":_
           FLAG(2)=1:ROOMOBJECT$(3,2)="KEY":EXIT SELECT

            THING$ = OBJECT$
        IF OBJECT$="DIRTY-CLOTHES" AND FNCARRY=0 THEN_
           PRINT "GET the CLOTHES first.":EXIT SELECT

       IF OBJECT$="DIRTY-CLOTHES" AND ROOM=1 AND FLAG(5)=0 THEN_
          PRINT "That was painless.":FLAG(5)=1:ROOMOBJECT$(1,2)=_
          "CLEAN-CLOTHES*":INVENTORY$(ITEMNUMBER)="EMPTY":_
          OBJ2$(8)="CLEAN-CLOTHES*":EXIT SELECT

            Print "Try-  CLEAN SINK ": Exit Select

        Case "CLAP"
            Print "The lights on the stairs just went on!": FLAG(6) = 1: Exit Select

        Case "SNEEZE", "SNEZE"
            If ROOM <> 8 Then Print "Go to the attic.": Exit Select
     IF FLAG(10)=0 THEN ROOMOBJECT$(8,1)="LATCHFREE":_
        PRINT "Dust clears, revealing a can of LATCHFREE spray.":_
        EXIT SELECT
            Print "That's nothing to sneeze at.": Exit Select

        Case "FLIP"
            Print "HEADS!..... You lose.": Exit Select

        Case "BREAK"
            Print "Temper......Temper...": Exit Select

        Case "CLIMB"
            Print "Try-  GO UP ": Exit Select

        Case "STICK"
            Print "Try-    PICK LOCK ": Exit Select

        Case "JUMP"
            Print "Come back down to earth!": Exit Select

        Case "PHONE", "TELEPHONE"
            Print "Try-   READ SIGN ": Exit Select

        Case "ANSWER"
         IF FLAG(7)=0 AND FLAG(5)=1 AND ROOM=5 THEN FLAG(7)=1:_
            PRINT "A voice says -- Go to attic and  SNEEZE LOUDLY ":_
            EXIT SELECT
            Print "Not now or not here": Exit Select

        Case "CALL"
            Print "That didn't work ... Try something else.": Exit Select

        Case "USE"
            If ROOM = 4 Then Print "Try-   MOP FLOOR ": Exit Select
            Print "Try something more specific, like PICK LOCK ": Exit Select

        Case "DO"
            Print "Try-  WASH DISHES ": Exit Select

        Case "SIT", "REST", "LIE"
            Print "This is no time for rest -- The game is afoot!": Exit Select

        Case Else
            Print "I don't know that verb": Exit Select
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
    Shared FLAG(), TURNNUMBER, ANSWER$(), STRT
    Print
   IF FLAG(1)=1 THEN PRINT "You enter the closet to find Anna fast asleep":_
    PRINT "and completely unharmed!  You are so relieved.  Just then,":_
    PRINT "Anna's parents come home and marvel at the clean house!  They":_
    PRINT "Give you an extra $10 and nominate you as babysitter of the year!":_
    PRINT:_
    PRINT " *****   HOORAY FOR YOU!!  YOU'VE WON THE GAME!!  ****"
    For I = 1 To 6: Print: Next I
    Input "BE SURE THAT YOUR DISK IS IN THE DRIVE AND PRESS RETURN"; DUMMY$

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
    '    Let DTA(7) = FLAG(1): Let DTA(32) = DTA(32) + Int((Timer - STRT) / 6)

    '    Open "REPORT.DTA" For Output As #1
    '    Rem SENDS UPDATED DATA TO REPORT.DTA (WITH NAIME$)
    '    For I = 1 To 40
    '        Write #1, DTA(I)
    '    Next I
    '    For I = 1 To 10
    '        Write #1, DTA$(I)
    '    Next I
    '    Close #1

    Open "TOWNDATA.TXT" For Append As #2
    Print #2, Time$
    For I = 0 To TURNNUMBER
        If I / 5 = Int(I / 5) Then Print #2, ANSWER$(I) Else Print #2, ANSWER$(I),
    Next I
    Print #2, Int((STRT - Timer) / 6)
    Close #2
    Print
    Print "This game is over.  Type  TOWNHOUSE  to play again."
End Sub

'END OF PROGRAM
