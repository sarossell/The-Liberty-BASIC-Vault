
'     PROGRAM: Hide Cursor
'    FILENAME: Hide Cursor.bas
'  PROGRAMMER: Scott A. Rossell
' DESCRIPTION: DLL call to hide cursor when right mouse button is clicked.
'        DATE: 19.12.9
'     VERSION: 1.0
'       NOTES: Based on original concept by Benjamin "colorcodegameauthor".

' ===> [ INITIALIZATION ] <=====================================================

nomainwin

WindowWidth = 500
WindowHeight = int(WindowWidth/1.618)
UpperLeftX = int((DisplayWidth-WindowWidth)/2)
UpperLeftY = int((DisplayHeight-WindowHeight)/2)

bShow = 0
open "Hide Cursor" for graphics as #g
#g, "place 50 100 ; color black";
#g, "\Right click mouse button inside this window to toggle cursor.";
#g, "trapclose [Quit]"

' ===> [ MAIN PROGRAM ] <=======================================================

[MainLoop]
  while 1
    #g, "when rightButtonDown [CursorToggle]"
    wait
  wend

end

[CursorToggle]
  calldll #user32, "ShowCursor",_
    bShow as boolean,_
    CursorShowInt as Long
  bShow = NOT(bShow)  ' Toggle logic.
  goto [MainLoop]

[Quit]
  close #g