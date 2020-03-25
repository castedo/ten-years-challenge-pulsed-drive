'--- Global constants
Global Const TRUE = -1
Global Const FALSE = 0
Global Const NOSTEP = -32768
'  - Warning!
'  - MAX_ELEMENTS      must be <= 16.000
'  - MAX_STEP*MAX_FILE must be <= 16.000
Global Const MAX_ELEMENTS = 10000
Global Const MAX_STEP = 50
Global Const MAX_FILE = 100
'  - format strings
Global Const F3_0$ = "000"
Global Const F6_4$ = "0.0000"

'--- Global variables
Global CANCEL As Integer
Global BOTHDIR As Integer
Global NLIST As Integer

Global ALPHA_DC_MIN As Single
Global ALPHA_DC_MAX As Single
Global OMEGA_C As Single
Global EPS  As Single

Global STEP_HEIGHT() As Single
Global FILES() As String
Global CRLF$
Global T$

