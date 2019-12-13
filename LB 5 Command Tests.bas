' //://::/:::::::::---..-.----::://:::---.
' //////:::--::-//oso+++o+o+-.--.----..-.:  PROGRAM:
' ::::::-://///+++yyo+oss/oyso+-.--:-----.
' :::.:/:///+/s+ssyhyssyysyso+oy+------.--    Liberty Basic Command Tests
' --:/+osoossssoyhhdhhdddmhhdhssyy:-----::
' /://oyhhyys+++oyhhyhdmmmhyyhdhddydmmmmmd  NAME:
' yyssyyyys+::::/sysssyhhhhyyydmhh+---::::
' +++os++-----::/osoooosssyyhhhhdhhdmmmmmm    Scott A. Rossell
' mmmhoo+--:::::://////+/+++oosydmmmMMMMMM
' MNNmys+.-/:/::::::::////++oooshmNdhddydd  DESCRIPTION:
' ++/+/y+-/+////++ossssosyhhhdddddmy/+/:/:
' -----so+s+/+syhddddhhyydmmmmmmdhhs/+::-:    A sequential collection of tests
' --..-+yyyssyhddmmmmddyoshmmddhdhyy//---.    for each command and constant in
' ---.-/hhyoshhhdmdmdhs/:::yhyssyhhh+:::--    the Liberty Basic programming
' -----:ysso/+oosyhhyso//++shddddddh+/:::/    language.
' ::::-:/o+oo++ooooooysyhddmmyhmmddhooyyyh
' /////+o+/ossyyyyyyysoddmmNmhyhhddhsyhddd    This program is intended for use
' oossssso+/oyyyhhhys++sshmmmmmdhyhyshmddd    with Liberty Basic version 5,
' ++oossyys/+syyyyys+osyydmmmmddddyyshMMmd    particularly during alpha and beta
' oosoooydmso+oosssoydhhdddddddddhhyymMMmd    testing phases of development.
' ssyyyydddmhoo+++sydddddhhhhhhysosydNMMmm
' ssyyyydmddmhs+/+syysssooydmyso/oshNMMNNm  DATE: 19.12.7
' syhdhhdmdddmdy+sssoos+/+odmdysshdNMMNNmm
' hdddhhhdddddmmmhyyyysoooohhhhdmNMMMNmmmd  VERSION: 1.0
' dmmdddhhhdddddmNmdhyysyydddmmNNMNNmmddmm
' mmdddddddhhddddddmmmmdmmmmNNNmmmmmmmddmN  NOTE: Only 37% complete.
' NddhdmmdddddddddddddddmmmmmmmdmdmmmdmmNM

' ===> [ INITIALIZATION ] <=====================================================

Global wrongNum, test$
Q$ = chr$(34)+chr$(34)
R$ = chr$(13)+chr$(13)
myHeight = DisplayHeight
myWidth = DisplayWidth
myPlatform$ = Platform$
wrongNum = 0
'ON ERROR GOTO [OOPS]   ' ON ERROR not yet implemented.
'CALL causeOops
MAINWIN 200 200 ' MAINWIN no longer accepts "* *" for fullscreen.

' ===> [ MAIN PROGRAM ] <=======================================================

'GOTO [SKIP3]
CLS
PRINT
INPUT "Sorry for the next few silly prompts. Gotta test 'em somehow!"_
    + " [Press ENTER] ";A$
NOTICE "Howdy!" + chr$(13) + "Press ENTER or click the OK button."_
    + R$ + "(This is testing the NOTICE command.)"
PROMPT "Gimme a ""C""." + R$ + "Type the letter ""C"", then press ENTER or"_
    + " click the OK button."_
    + R$ +"(This is testing the PROMPT command.)"; responseVar$
IF upper$(responseVar$) = "C" then
    notice "Thank You" + chr$(13) + "I ...ouldn't have done it without you!"
else
    notice "WRONG!" + chr$(13) + "That was not a ""C""."
end if
CONFIRM "Are you sure you want to do this?" + chr$(13) + chr$(13)_
    + "(This is testing the CONFIRM dialog.)";A$
CLS
PRINT
PRINT "Liberty Basic Commands Test Program:"
PRINT "----------------------------------------"
PRINT
PRINT "I initially created this program to familiarize myself with all of the"
PRINT "Liberty Basic commands and constants. You unavoidably learn the little"
PRINT "details when you actually type the code out. Things like needing"
PRINT "parenthesis around logical comparisons and printing columns using commas"
PRINT "instead of tabs."
PRINT
PRINT "It occurred to me that this program might be useful to others for"
PRINT "testing. Or, at the very least, a mildly interesting thing to watch for"
PRINT "a few milliseconds."
PRINT
PRINT "You'll likely notice that I continually reset variables for each test"
PRINT "instead of using subroutines where the variables would be isolated. I"
PRINT "did this because it's actually less code than to include SUB {name}..."
PRINT "END SUB before and after each test." 
PRINT
PRINT "The code for the tests that fail is commented out currently since the"
PRINT "ON ERROR command has not yet been implemented. In these cases, the"
PRINT "response ""NO"" is hard coded. Also the code for some successful tests"
PRINT "is commented out because the processes involved don't easily lend"
PRINT "themselves to automated testing. In these cases, the response""YES"""
PRINT "is hard coded. A bit of a cheat, but not much else I could do. You can"
PRINT "fiddle with the commented code if you need to check something out."
PRINT
PRINT "Version 1.0 of this program is everything I've learned to date. It only"
PRINT "includes 119 commands and constants out of a total of 319. That's only"
PRINT "37%, so I've got a ways to go. The GUI design tests are next, but I've"
PRINT "never called a DLL or worked with random access files in my life, and I"
PRINT "don't even own a printer or joystick. So, the pace will likely be a tad"
PRINT "slower to reach verson 2.0. That is, assuming anyone even finds this"
PRINT "little program amusing enough to even warrant a second version."
PRINT
PRINT "Lemme know!  :@)"
PRINT
PRINT
PRINT "Observations for Liberty Basic v5.a-350:"
PRINT "----------------------------------------"
PRINT
PRINT "  * By my estimate, there appear to be 319 commands or constants in"
PRINT "    Liberty Basic, including Text, Graphics and Sprites commands."
PRINT
PRINT "  * LB 5 can now print double quotes instead of having to use"
PRINT "    chr$(34) + chr$(34)."
PRINT
PRINT "  * Processing speed is decidely slower on the Mac compared to the PC."
PRINT "    The PC can run this program in less than 3 seconds while the Mac"
PRINT "    can take up to 20."
PRINT
PRINT "  * Code editor scrolling has a bit of lag in Windows 10 but there is"
PRINT "    a much more severe lag in MacOS, with a concurrent spike in CPU"
PRINT "    use that engages the cooling fans."
PRINT
PRINT "  * LB 5 currently lacks command line parameters for using an external"
PRINT "    code editor."
PRINT
PRINT "  * The editor file Save function does not update the tab title." 
PRINT
PRINT "  * The toolbar icons on the Mac do not change during hover or click."
PRINT
PRINT "  * Printing commas for columnar display (e.g., PRINT ,,,""Column 4"")"
PRINT "    now requires a string or empty quotes before the comma(s)."
PRINT 
PRINT "  * Column sizes are different somehow. Running the same code;"
PRINT "    PRINT ""Column 1"",,,""Column 4"" yields different spacing between"
PRINT "    versions 4.5.1 and 5.0."
PRINT
PRINT
myStart = TIME$("ms")
PRINT "Command and Constants Tests (in alphabetical order by success):"
PRINT "---------------------------------------------------------------"
PRINT

'   `7MN.   `7MF'  .g8""8q.
'     MMN.    M  .dP'    `YM.
'     M YMb   M  dM'      `MM
'     M  `MN. M  MM        MM
'     M   `MM.M  MM.      ,MP
'     M     YMM  `Mb.    ,dP'
'   .JML.    YM    `"bmmd"'
'---------------------------------------------------------------------[ AFTER$ ]
PRINT "AFTER$",,,
'A$ = AFTER$("This is a yet another test, yet again.", "yet")
'IF A$ = " another test, yet again." THEN PRINT "YES" ELSE PRINT "NO"
PRINT "NO"
'-----------------------------------------------------------------[ AFTERLAST$ ]
PRINT "AFTERLAST$",,,
'A$ = AFTERLAST$("This is a yet another test, yet again.", "yet")
'IF A$ = " again." THEN PRINT "YES" ELSE PRINT "NO"
PRINT "NO"
'-----------------------------------------------------------------------[ BEEP ]
PRINT "BEEP",,,
'BEEP
PRINT "NO"
'----------------------------------------------------------------------[ BYREF ]
PRINT "BYREF",,,
x = 5.3
y = 7.2
result = myTest(x, y)
IF X = 5 THEN PRINT "YES" ELSE PRINT "NO"
'----------------------------------------------------------------[ COLORDIALOG ]
PRINT "COLORDIALOG",,,
'colordialog "red", chosen$
PRINT "NO"
'---------------------------------------------------------------[ commandline$ ]
PRINT "commandline$",,,
'IF CommandLine$ = "" THEN PRINT "YES ELSE PRINT "NO"
PRINT "NO"
'---------------------------------------------------------------------[ CURSOR ]
PRINT "CURSOR",,,
'CURSOR CROSSHAIR
PRINT "NO"
'-----------------------------------------------------------------[ FONTDIALOG ]
PRINT "FONTDIALOG",,,
'fontdialog "arial 10 italic", chosenFont$
PRINT "NO"
'------------------------------------------------------------------[ IdeaCode$ ]
PRINT "idecode$",,,
'A$ = idecode$()
'IF left$(a$,4) = "' //" THEN PRINT "YES" ELSE PRINT "NO"
PRINT "NO"
'---------------------------------------------------------------[ IdeFilename$ ]
PRINT "idefilename$",,,
'A$ = idefilename$()
'IF left$(A$,4) = "LB C" THEN PRINT "YES" ELSE PRINT "NO"
PRINT "NO"
'-----------------------------------------------------------------------[ KILL ]
PRINT "KILL",,,
'KILL "c:\myTest\test.txt"
PRINT "NO" ' Had to use RUN "del c:\myTest\test.txt" instead.
'-----------------------------------------------------------------------[ NAME ]
PRINT "NAME",,,
'NAME "c:\myTest\test.txt" as "c:\myTest\myTest.txt"
PRINT "NO"
'-----------------------------------------------------------------[ ONCOMERROR ]
PRINT "ONCOMERROR",,"NO"  ' Not implemented.
'-------------------------------------------------------------------[ ON ERROR ]
PRINT "ON ERROR GOTO.RESUME",,  ' Not implemented.
IF myGOOF = 1 THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------[ popupmenu ]
PRINT "popupmenu",,,
PRINT "NO"
'--------------------------------------------------------------[ printerdialog ]
PRINT "printerdialog",,,
PRINT "NO"
'-------------------------------------------------------------------[ REMCHAR$ ]
PRINT "REMCHAR$",,,
'A$ = REMCHAR$("Mississippi", "s")
'IF A$ = "Miiippi" THEN PRINT "YES" ELSE PRINT "NO"
PRINT "NO"
'-------------------------------------------------------------------[ REPLSTR$ ]
PRINT "REPLSTR$",,,
'A$ = REPLSTR$("Mississippi", "s", "x")
'IF A$ = "Mixxixxippi" THEN PRINT "YES" ELSE PRINT "NO"
PRINT "NO"
'----------------------------------------------------------------------[ STOP$ ]
PRINT "STOP",,,
'STOP
PRINT "NO"
'---------------------------------------------------------------------[ TRACE$ ]
PRINT "TRACE",,,
'TRACE 1
'FOR N = 1 TO 100
' PRINT N^2
'NEXT N
'TRACE 0
PRINT "NO"
'----------------------------------------------------------------------[ UPTO$ ]
PRINT "UPTO$",,,
'IF UPTO$("The dirty, ugly fox jumped over the fat, lazy dog."," jumped") =_
'     "The dirty, ugly fox" THEN PRINT "YES" ELSE PRINT "NO"
PRINT "NO"
PRINT


'  ,M"""b.  ,M"""b.  ,M"""b.
'  89'  `Mg 89'  `Mg 89'  `Mg
'       ,M9      ,M9      ,M9
'    mMMY'    mMMY'    mMMY'
'    MM       MM       MM
'    ,,       ,,       ,,
'    db       db       db

'-----------------------------------------------------------------------[ NONE ]
PRINT "NONE",,"??? Undocumented? Listed as a reserved command word."
' Unable to find documentation.
'-------------------------------------------------------------------[ PASSWORD ]
PRINT "PASSWORD",,"??? Undocumented? Listed as a reserved command word."
' Unable to find documentation.


'                                      q   p
'  `YMM'   `MM'`7MM"""YMM   .M"""bgd    \ /     `7MM"""Yp,
'    VMA   ,V    MM    `7  ,MI    "Y o=--*--=o    MM    Yb
'     VMA ,V     MM   d    `MMb.        / \       MM    dP `7MM  `7MM   .P"Ybmmm
'      VMMP      MMmmMM      `YMMNq.   d   b      MM"""bg.   MM    MM  :MI  I8
'       MM       MM   Y  , .     `MM              MM    `Y   MM    MM   WmmmP"
'       MM       MM     ,M Mb     dM              MM    ,9   MM    MM  8M
'     .JMML.   .JMMmmmmMMM P"Ybmmd"             .JMMmmmd9    `Mbod"YML. YMMMMMb
'                                                                      6'     dP
'                                                                      Ybmmmd'

PRINT
'-------------------------------------------------------------------[ httpget$ ]
PRINT "httpget$",,,
HTMLTEXT$ = httpget$("http://www.libertybasic.com")
IF WORD$(HTMLTEXT$,500) = "system" THEN
PRINT "YES* Bug: Command must be all lower case"
ELSE
PRINT "NO"
END IF
'--------------------------------------------------------------------[ MAINWIN ]
PRINT "MAINWIN",,"YES* Bug: No longer accepts " + Q$ + "* *" + Q$_
      + " for fullscreen."
        ' Made the screen as nearly fullscreen as possible.
'----------------------------------------------------------------------[ TIME$ ]
PRINT "TIME$",,,
'print time$()
'print time$("seconds") ' Not working properly, Displays ms.
'print time$("ms")
PRINT "YES* Bug: Seconds incorrectly display as ms."
PRINT
'  `YMM'   `MM'`7MM"""YMM   .M"""bgd
'    VMA   ,V    MM    `7  ,MI    "Y
'     VMA ,V     MM   d    `MMb.
'      VMMP      MMmmMM      `YMMNq.
'       MM       MM   Y  , .     `MM
'       MM       MM     ,M Mb     dM
'     .JMML.   .JMMmmmmMMM P"Ybmmd"
'------------------------------------------------------------------------[ ABS ]
PRINT "ABS",,,
IF ABS(-10) = 10 THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ ACS ]
PRINT "ACS",,,    ' Requires STR$ to make the test work.
IF STR$(ACS(0.2)) = "1.3694384060046" THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ AND ]
PRINT "AND",,,
N = 1
IF (0 AND 0) = 0 THEN N = N * 1 ELSE N = N * 0
IF (0 AND 1) = 0 THEN N = N * 1 ELSE N = N * 0
IF (1 AND 0) = 0 THEN N = N * 1 ELSE N = N * 0
IF (1 AND 1) = 1 THEN N = N * 1 ELSE N = N * 0
IF (11001101 AND 01010110) = 477452 THEN N = N * 1 ELSE N = N * 0
IF (10<20) AND (5=5) THEN N = N * 1 ELSE N = N * 0
IF NOT(N = 0) THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ ASC ]
PRINT "ASC",,,
IF ASC("A") = 65 THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ ASN ]
PRINT "ASN",,,    ' Requires STR$ to make the test work.
IF STR$(ASN(0.2)) = "0.20135792079033" THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ ATN ]
PRINT "ATN",,,    ' Requires STR$ to make the test work.
IF STR$(ATN(0.2)) = "0.19739555984988" THEN PRINT "YES" ELSE PRINT "NO"
'-----------------------------------------------------------------------[ CHR$ ]
PRINT "CHR$",,,
A$ = chr$(65)
IF A$ = "A" THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ COS ]
PRINT "COS",,,    ' Requires STR$ to make the test work.
IF STR$(COS(0.2)) = "0.98006657784124" THEN PRINT "YES" ELSE PRINT "NO"
'----------------------------------------------------------------------[ DATE$ ]
print "DATE$",,,
if int((val(date$("december 25, 2019")) - val(date$("july 30, 1998")))_
     / 365) = 21 then PRINT "YES" ELSE PRINT "NO"
'--------------------------------------------------------------------[ DECHEX$ ]
PRINT "DECHEX$",,,
IF DECHEX$(186)="BA" THEN PRINT "YES" ELSE PRINT "NO"
'----------------------------------------------------------------[ DefaultDir$ ]
PRINT "Defaultdir$",,,
if Platform$ = "unix" then
    IF left$(DefaultDir$,3) = "/Vo" THEN PRINT "YES" ELSE PRINT "NO"
else
    IF left$(DefaultDir$,3) = "C:\" THEN PRINT "YES" ELSE PRINT "NO"
end if
'------------------------------------------------------------------[ DIM.REDIM ]
PRINT "DIM.REDIM",,,
dim A(10)
for n = 1 to 10
    A(n) = n^2
next n
REDIM A(15)
A(11) = 11^2
if (A(11) = 121 and A(1) = 0) THEN PRINT "YES" ELSE PRINT "NO"
'-------------------------------------------------------------[ DisplayHeight$ ]
PRINT "DisplayHeight",,,    'Based on my display dimensions of course.
N = DisplayHeight
IF N = myHeight THEN PRINT "YES" ELSE PRINT "NO"
'--------------------------------------------------------------[ DisplayWidth$ ]
PRINT "DisplayWidth",,,     'Based on my display dimensions of course.
N = DisplayWidth
IF N = myWidth THEN PRINT "YES" ELSE PRINT "NO"
'----------------------------------------------------[ DO.{EXIT DO}.LOOP.UNTIL ]
PRINT "DO.{EXIT DO}.LOOP.UNTIL",
n = 0
do
    n = n + 1
    if n > 3 then exit do
loop until n = 5
if n = 4 then print "YES" ELSE PRINT "NO"
'--------------------------------------------------------------------[ Drives$ ]
PRINT "Drives$",,,
if Platform$ ="unix" then
    IF LEFT$(Drives$,1) = "/" THEN PRINT "YES" ELSE PRINT "NO"
else
    IF LEFT$(Drives$,1) = "C" THEN PRINT "YES" ELSE PRINT "NO"
end if

'------------------------------------------------------------------------[ END ]
PRINT "END",,"YES"
'-----------------------------------------------------------------------[ EVAL ]
PRINT "EVAL",,,
IF EVAL("2+1*3/4^5-6") = -3.9970703125 THEN PRINT "YES" ELSE PRINT "NO"
'----------------------------------------------------------------------[ EVAL$ ]
PRINT "EVAL$",,,
IF EVAL$("2+1*3/4^5-6") = "-3.9970703125" THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ EXP ]
PRINT "EXP",,,      ' Requires STR$ to make the test work.
IF STR$(EXP(5)) = "148.41315910258" THEN PRINT "YES" ELSE PRINT "NO"
'-----------------------------------------------------------------[ FILEDIALOG ]
PRINT "FILEDIALOG",,,
'FILEDIALOG "Open text file", "*.txt", fileName$
PRINT "YES"
'------------------------------------------------[ FOR.TO.{EXIT FOR}.STEP.NEXT ]
PRINT "FOR.TO.{EXIT FOR}.STEP.NEXT",
A$ = ""
for n = 1 to 10 step 2
    A$ = A$ + str$(n)
    if n > 5 then EXIT FOR     'This skips the 9.
next
IF A$ = "1357" THEN PRINT "YES" ELSE PRINT "NO"
'-------------------------------------------------------------------[ FUNCTION ]
PRINT "FUNCTION",,,
IF myFunc(2) = 20 THEN PRINT "YES" ELSE PRINT "NO"
'---------------------------------------------------------------------[ GLOBAL ]
PRINT "GLOBAL",,,
test$ = ""  ' test$ is a global variable set at the beginning of the program.
call my2ndSub
IF (test$ = "3.14" and other$ = "") THEN PRINT "YES" ELSE PRINT "NO"
'---------------------------------------------------------------[ GOSUB.RETURN ]
PRINT "GOSUB.RETURN",,,
A$ = ""
GOSUB [GRAB]
GOTO [NOPE]
[GRAB]
A$ = "test123"
RETURN
[NOPE]
IF A$ = "test123" THEN PRINT "YES" ELSE PRINT "NO"
'-----------------------------------------------------------------------[ GOTO ]
PRINT "GOTO",,,
A$ = "test123"
GOTO [NOPE2]
A$ = "nope321"
[NOPE2]
IF A$ = "test123" THEN PRINT "YES" ELSE PRINT "NO"
'---------------------------------------------------------------------[ HEXDEC ]
PRINT "HEXDEC",,,
IF HEXDEC("BA")=186 THEN PRINT "YES" ELSE PRINT "NO"
'--------------------------------------------------------[ IF.THEN.ELSE.END IF ]
PRINT "IF.THEN.ELSE.END IF",,
A$ = ""
FOR N = 1 TO 10
    IF N MOD 2 = 0 THEN
        A$ = A$ + STR$(N)
    ELSE
        B$ = B$ + STR$(N)
    END IF
NEXT N
IF (A$ = "246810" AND B$ = "13579") THEN PRINT "YES" ELSE PRINT "NO"
'----------------------------------------------------------------------[ INPUT ]
PRINT "INPUT",,"YES"   ' Very first question when run used the INPUT command.
'----------------------------------------------------------------------[ INSTR ]
PRINT "INSTR",,,
IF INSTR("THIS IS A TEST","IS",5) = 6 THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ INT ]
PRINT "INT",,,
IF INT(2+1*3/4^5-6) = -3 THEN PRINT "YES" ELSE PRINT "NO"
'----------------------------------------------------------------------[ LEFT$ ]
PRINT "LEFT$",,,
A$ = LEFT$("123456789",4)
IF A$ = "1234" THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ LEN ]
PRINT "LEN",,,
IF LEN("THIS IS A TEST.") = 15 THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ LET ]
PRINT "LET",,,
LET A$ = "test"
IF A$ = "test" THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ LOG ]
PRINT "LOG",,,      ' Requires STR$ to make the test work.
IF str$(LOG(7)) = "1.9459101490553" THEN PRINT "YES" ELSE PRINT "NO"
'---------------------------------------------------------------------[ LOWER$ ]
PRINT "LOWER$",,,
A$ = LOWER$("TEST")
IF A$ = "test" THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ MAX ]
PRINT "MAX",,,
IF MAX(0, 5) = 5 THEN PRINT "YES" ELSE PRINT "NO"
'-----------------------------------------------------------------------[ MID$ ]
PRINT "MID$",,,
A$ = MID$("123456789",4,3)
IF A$ = "456" THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ MIN ]
PRINT "MIN",,,
IF MIN(0, 5) = 0 THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ MOD ]
PRINT "MOD",,,
IF 10 MOD 3 = 1 THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------[ NOMAINWIN ]
PRINT "NOMAINWIN",,"YES"    ' It works, but we don't want to use it.
                            ' You wouldn't be able to see anything.
'------------------------------------------------------------------------[ NOT ]
PRINT "NOT",,,
IF NOT((21<20) AND (5=5)) THEN PRINT "YES" ELSE PRINT "NO"
'---------------------------------------------------------------------[ NOTICE ]
PRINT "NOTICE",,"YES"  ' Remember the silly notice at the beginning?
'-------------------------------------------------------------------------[ OR ]
PRINT "OR",,,
N = 1
IF (0 OR 0) = 0 THEN N = N * 1 ELSE N = N * 0
IF (0 OR 1) = 1 THEN N = N * 1 ELSE N = N * 0
IF (1 OR 0) = 1 THEN N = N * 1 ELSE N = N * 0
IF (1 OR 1) = 1 THEN N = N * 1 ELSE N = N * 0
IF (11001101 OR 01010110) = 11533759 THEN N = N * 1 ELSE N = N * 0
IF (10<20) OR (5=5) THEN N = N * 1 ELSE N = N * 0
IF NOT(N = 0) THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------[ Platform$ ]
PRINT "Platform$",,,
IF Platform$ = myPlatform$ THEN PRINT "YES" ELSE PRINT "NO"
'----------------------------------------------------------------------[ PRINT ]
PRINT "PRINT",,"YES"    ' Everything you see is using the PRINT command.
'---------------------------------------------------------------------[ PROMPT ]
PRINT "PROMPT",,"YES"  ' Remember the silly prompt for a "C" at the beginning?
'----------------------------------------------------------[ READ.DATA.RESTORE ]
PRINT "READ.DATA.RESTORE",,
N = 0 : A$ = ""
DO
    READ myNext
    A$ = A$ + str$(myNext)
LOOP UNTIL myNext = 9
RESTORE
READ myNext
A$ = A$ + str$(myNext)
DATA 1,2,3,4,5,6,7,8,9
IF A$ = "1234567891" THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ REM ]
PRINT "REM (or ')",,,   ' Yep, works.
N = 1 : REM N = 0
REM N = 0
' N = 0
IF N = 1 THEN PRINT "YES" ELSE PRINT "NO"
'---------------------------------------------------------------------[ RIGHT$ ]
PRINT "RIGHT$",,,
A$ = RIGHT$("123456789",4)
IF A$ = "6789" THEN PRINT "YES" ELSE PRINT "NO"
'--------------------------------------------------------------[ RND.RANDOMIZE ]
PRINT "RND.RANDOMIZE",,,
RANDOMIZE .5
IF INT(RND(0)*123) = 94 THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ RUN ]
PRINT "RUN",,,
'run "notepad"
PRINT "YES"
'----------------------------------------------------------------[ SELECT.CASE ]
PRINT "SELECT.CASE",,,
A$ = "" : N = 3
select case N
    case 3
        A$ = A$ + "first"
    case 5
        A$ = A$ + "second"
    case else
        'Nothing
end select
IF A$ = "first" THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ SIN ]
PRINT "SIN",,,    ' Requires STR$ to make the test work.
IF STR$(SIN(0.2)) = "0.19866933079506" THEN PRINT "YES" ELSE PRINT "NO"
'-----------------------------------------------------------------------[ SORT ]
PRINT "SORT",,,
for n = 1 to 5
    myArray$(n) = word$("One Two Three Four Five",n)
next n
sort myArray$(), 1, 5
IF myArray$(5) = "Two" THEN PRINT "YES" ELSE PRINT "NO"
'---------------------------------------------------------------------[ SPACE$ ]
PRINT "SPACE$",,,
A$ = SPACE$(4)
IF A$ = "    " THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ SQR ]
PRINT "SQR",,,
IF SQR(144) = 12 THEN PRINT "YES" ELSE PRINT "NO"
'----------------------------------------------------------------[ StartupDir$ ]
PRINT "Startupdir$",,,
if Platform$ = "unix" then
    IF left$(StartupDir$,3) = "/Vo" THEN PRINT "YES" ELSE PRINT "NO"
else
    IF left$(StartupDir$,3) = "C:\" THEN PRINT "YES" ELSE PRINT "NO"
end if

'-----------------------------------------------------------------------[ STR$ ]
PRINT "STR$",,,
A$ = str$(1234)
IF A$ = "1234" THEN PRINT "YES" ELSE PRINT "NO"
'--------------------------------------------------------------[ SUB.CALL SUB$ ]
PRINT "SUB.CALL SUB",,,
call mySub 10, "word"
if test$ = "10word" THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ TAB ]
PRINT "TAB",,,
'PRINT TAB(20);1234
PRINT "YES"
'------------------------------------------------------------------------[ TAN ]
PRINT "TAN",,,    ' Requires STR$ to make the test work.
IF STR$(TAN(0.2)) = "0.20271003550867" THEN PRINT "YES" ELSE PRINT "NO"
'----------------------------------------------------------------------[ TIMER ]
PRINT "TIMER",,,
myLocalStart = time$("ms")
timer 500, [myTrigger]
wait
[myTrigger]
timer 0
IF time$("ms") - myLocalStart >= 500 THEN PRINT "YES" ELSE PRINT "NO"
'----------------------------------------------------------------------[ TRIM$ ]
PRINT "TRIM$",,,
IF TRIM$(" This is a test. ") = "This is a test." THEN PRINT_
     "YES" ELSE PRINT "NO"
'---------------------------------------------------------------------[ UPPER$ ]
PRINT "UPPER$",,,
A$ = UPPER$("test")
IF A$ = "TEST" THEN PRINT "YES" ELSE PRINT "NO"
'----------------------------------------------------------------------[ USING ]
PRINT "USING",,,
N = 12.3489
A$ = using("##.##",N)
IF A$ = "12.35" THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ VAL ]
PRINT "VAL",,,
IF VAL("369")/9 = 41 THEN PRINT "YES" ELSE PRINT "NO"
'-------------------------------------------------------------------[ Version$ ]
PRINT "Version$",,,
IF Version$ = "5.0" THEN PRINT "YES" ELSE PRINT "NO"
'-----------------------------------------------------------------------[ WAIT ]
PRINT "WAIT",,,
myOtherStart = time$("ms")
timer 500, [myTrigger2]
wait
[myTrigger2]
timer 0
IF time$("ms") - myOtherStart >= 500 THEN PRINT "YES" ELSE PRINT "NO"
'----------------------------------------------------[ WHILE.{EXIT WHILE}.WEND ]
PRINT "WHILE.{EXIT WHILE}.WEND",
n = 0
while n < 5
    n = n + 1
    if n > 3 then exit while
wend
if n = 4 then print "YES" ELSE PRINT "NO"
'----------------------------------------------------------------------[ WORD$ ]
PRINT "WORD$",,,
IF WORD$("The dirty, ugly fox jumped over the fat, lazy dog.",3) =_
     "ugly" THEN PRINT "YES" ELSE PRINT "NO"
'------------------------------------------------------------------------[ XOR ]
PRINT "XOR",,,
N = 1
IF (0 XOR 0) = 0 THEN N = N * 1 ELSE N = N * 0
IF (0 XOR 1) = 1 THEN N = N * 1 ELSE N = N * 0
IF (1 XOR 0) = 1 THEN N = N * 1 ELSE N = N * 0
IF (1 XOR 1) = 0 THEN N = N * 1 ELSE N = N * 0
IF (11001101 XOR 01010110) = 11056307 THEN N = N * 1 ELSE N = N * 0
IF (21<20) XOR (5=5) THEN N = N * 1 ELSE N = N * 0
IF NOT(N = 0) THEN PRINT "YES" ELSE PRINT "NO"

[SKIP3]
PRINT
PRINT "File Function Tests (in chronological order):"
print "---------------------------------------------"
PRINT
myDir$ = DefaultDir$ + "myTest"
if Platform$ = "unix" then
    myFile$ = myDir$ + "/test.txt"
    myNextDir$ = myDir$ + "/again"
else
    myFile$ = myDir$ + "\test.txt"
    myNextDir$ = myDir$ + "\again"
end if
' Create a directory and sub-directory
'MKDIR
PRINT "MKDIR",,,

'print myDir$
'print myFile$
'print myNextDir$

result = mkdir(myDir$)
'print "MKDIR myDir$ result: ";result,
'input "Pause";p
if result <> 0 then
    print "Directory not created!"
    print "NO"
else
    print "YES"
end if
result = mkdir(myNextDir$)
'print "MKDIR myNextDIR$ result: ";result,
'input "Pause";p
if result <> 0 then
   print "Directory not created!"
'    print "NO"
'else
'    print "YES"
end if

' Delete the sub-directory
'RMDIR
PRINT "RMDIR",,,
result = rmdir(myNextDir$)
'print "RMDIR myNextDIR$ result: ";result,
'input "Pause";p
if result <> 0 then
    print "Directory not removed!"
    print "NO"
else
    print "YES"
end if

' Open a file
'OPEN
PRINT "OPEN",,"YES"
A$ = httpget$("http://www.google.com")
open myFile$ for output as #h
' Write to the file
'PRINT
print #h, A$
' Close the file
'CLOSE
PRINT "CLOSE",,"YES"
close #h
' Check if the file exists
PRINT "FILES",,,
dim info$(1,1)
files myDir$, "test.txt", info$()
If val(info$(0, 0)) > 0 then
'    print "yep, it's there."
    print "YES"
else
    print "NO"
end if
'input "Pause. File populated?";p
'Rename the file
PRINT "NAME",,,     ' Not yet implemented
'NAME
'If val(info$(0, 0)) > 0 then
'    name "c:\myTest\test.txt" as "c:\myTest\myTest.txt"
'end if
PRINT "NO"
' Read file to end
PRINT "LOF",,"YES* Bug: File size incorrect - Reported 4096 for a file"_
   + " 12197 bytes long."

open myFile$ for input as #h
myFileLen = lof(#h)
'print "File is ";myFileLen;" bytes long. Here's the first 100 bytes:"
'print
for n = 1 to 100
    A$ = input$(#h, 1)
'print A$;
next n
close #h
'print "..."
'Read file using INPUTTO$
'INPUTTO$
PRINT "INPUTTO$",,"YES"
'Print "Displaying each comma delimited item in a file on its own line."
'print
open myFile$ for input as #h
while eof(#h) = 0
    A$ = inputto$(#h, ",")
'    myCommas = myCommas + 1
'    if myCommas >= 10 then exit while
'print A$
wend
close #h
PRINT "LINE INPUT",,"YES"
' Read file using LINE INPUT
' LINE INPUT
'Print "Displaying the file one line at a time."
open myFile$ for input as #h
while eof(#h) = 0
   line input #h, item$
'   print item$
wend
close #h
PRINT "INPUTCSV",,"NO"      ' Not yet implemented.
'Print "Displaying each comma delimited bit of text from the file."
'INPUTCSV
'open "c:\myTest\test.txt" for input as #h
'INPUTCSV #h, a$, b$, c$, d$
'print a$
'print b$
'print c$
'print d$
'close #h
' Delete the file
PRINT "KILL",,"NO"      ' Not yet implemented.
'    KILL myFile$
if Platform$ = "unix" then
    RUN "rm " + myFile$
else
    RUN "cmd.exe /C del " + myFile$
end if
' Delete the directory
result = rmdir(myDir$)
'print "result: ";result
if result <> 0 then print "Directory not removed!"
PRINT
print "---------------------------------------------"
PRINT
myTotal = (TIME$("ms") - myStart)/1000
PRINT "Took "; using("##.#",myTotal);" seconds to do that."
END ' Well that happened.
FUNCTION myFunc(inNum)
    myFunc = inNum * 10
END FUNCTION
function myTest(byref a, byref b)
  a = int(a)
  b = int(b)
  myTest = a + b
end function
SUB mySub inNum, inString$
    test$ = str$(inNum)+inString$
END SUB
SUB my2ndSub
    test$ = "3.14"
    other$ = "nope"
END SUB
'sub causeOops          ' This was part of the ON ERROR test.
'    print 2/wrongNum
'end sub
'[OOPS]
'    myGoof = 1
'    wrongNum = 1
'    resume
