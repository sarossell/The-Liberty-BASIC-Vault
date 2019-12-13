'                           __        __    __    __                 
'  |   . |_   _  _ |_      |__)  /\  (_  | /     |__)  _   _ |   _ | 
'  |__ | |_) (- |  |_ \/   |__) /--\ __) | \__   | \  (_) (_ |( _) . 
'                     /                                              
'
'     Program: Hide Cursor
'        Date: 19.12.9
'     Version: 1.0
' Description: DLL call to hide cursor when right mouse button is clicked.
'       Notes: Based on original concept by Benjamin "colorcodegameauthor".

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