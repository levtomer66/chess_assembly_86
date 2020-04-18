locals
sizeS equ 100d
Tool Struc
				typeTool	db	?		;R, H, B, K, Q, or P The Type Of The Tool
				toolColor	db	?		;Boolean - 00001011b = Player 1, 00001111b = Player 2
				xLoc		db	?		;DL Location (x location)
				yLoc		db	?		;DH Location (y location)
				isEaten		db	?		;Boolean - 0=Not Eaten Yet... , 1 = Eaten
Tool Ends

TableS struc
				NameP1 db	18d,20d dup('$')
				NameP2 db 	18d,20d dup('$')
				Winner db 	18d,20d dup('$')  	 ; winner's name
				DNTB   db 	18d,20d dup('$') 	 ; date & time in the begining
				DNTE   db 	18d,20d dup('$') 	 ; date & time in the end  ; DD/MM/YYYY HH:MM:SS		
TableS Ends

RankMove Struc
				offseTool			dw	?	;(The Tool That Movoing)
				cmpMovex			db	?	;(First move location x )
				cmpMovey			db	?	;(First move location y ) 	
				plrMovex			db	?	
				plrMovey			db	?
				arrOffsetsEatDanger	dw	9h dup('$')
				moveRank			db 	?	;The Rank For Moving 0 - 10
RankMove EndS

dseg segment
	sqStr	db	201d,205d,205d,205d,205d,205d,187d 											
	SQ_COLS	=	($-sqStr)/type sqStr
			db	186d, 0h, 0h, 0h,  0h, 0h ,186d	
			db	200d,205d,205d,205d,205d,205d, 188d	
	SQ_ROWS	=	($-sqStr)/SQ_COLS
	;Tls Tool = Strucs Array Of All The Tools In The Game
	Tls Tool 	<'R', 00001011b, 10h, 1h, 0h >, <'H', 00001011b, 17h, 1h, 0h >, <'B', 00001011b, 01eh, 1h, 0h >, <'Q', 00001011b, 25h, 1h, 0h > ,<'K', 00001011b, 02ch, 1h, 0h > ,<'B', 00001011b, 33h, 1h, 0h >,<'H', 00001011b, 03ah, 1h, 0h >,<'R', 00001011b, 41h, 1h, 0h >
		Tool	<'P', 00001011b, 10h, 4h, 0h >, <'P', 00001011b, 17h, 4h, 0h >, <'P', 00001011b, 01eh, 4h, 0h >, <'P', 00001011b, 25h, 4h, 0h > ,<'P', 00001011b, 02ch, 4h, 0h > ,<'P', 00001011b, 33h, 4h, 0h >,<'P', 00001011b, 03ah, 4h, 0h >,<'P', 00001011b, 41h, 4h, 0h >
		Tool	<'R', 00001111b, 10h, 16h, 0h >, <'H', 00001111b, 17h, 16h, 0h >, <'B', 00001111b, 01eh, 16h, 0h >, <'Q', 00001111b, 25h, 16h, 0h > ,<'K', 00001111b, 02ch, 16h, 0h > ,<'B', 00001111b, 33h, 16h, 0h >,<'H', 00001111b, 03ah, 16h, 0h >,<'R', 00001111b, 41h, 16h, 0h>
		Tool	<'P', 00001111b, 10h, 13h, 0h >, <'P', 00001111b, 17h, 13h, 0h >, <'P', 00001111b, 01eh, 13h, 0h >, <'P', 00001111b, 25h, 13h, 0h > ,<'P', 00001111b, 02ch, 13h, 0h > ,<'P', 00001111b, 33h, 13h, 0h >,<'P', 00001111b, 03ah, 13h, 0h >,<'P', 00001111b, 41h, 13h, 0h >
	TLS_LEN = ($-Tls)/type Tls	;The Length Of The Tools Array (32) Number of Players
	TOT_LEN = TLS_LEN*type Tls 
	
	xTurn	dw	00001111b, 00001011b
	
	
	openScr	db	'ChessTxt.txt', 0
	buffer1	db	1000h dup('$')	;OpenScr
	p1Winner db	'P1Win.txt', 0
	p2Winner db	'P2Win.txt', 0		
	buffer2	db	1000h dup('$')	;Winner Messeg
	instruction db	'InstTxt.txt', 0
	buffer3	db	1000h dup('$')	;Instruction
	mainMen	db	'MainM.txt', 0
	buffer4	db	500h dup('$') ;Main Menu 
		
	menuMsg	db	"Pick a Tool: "
	MENU_LEN = ($-menuMsg)/type menuMsg
	menuT	db	"Q - Queen ", "B - Bishop", "R - Rook  ", "H - Horse "		
	TOOL_LEN = ($-menuT)/ type menuT
	
	menu	db	" Player Vs Computer "
	MEN_LEN	=	($-menu)
	db	" Player1 Vs Player2 "
	db	"    Instruction     "	
	db	"    Record Table    "
	db	"        Quit        "
	
	tbl TableS SizeS dup(< , , , , >)
	temp TableS < , , , , > 
	TBL_LEN = type tbl
	
	p1Str db 10,13,"Player One, Enter Your Name, Please: $"
	p2Str db 10,13,"Player Two, Enter Your Name, Please: $"
	cmpName	db	"CmpPlayer $"	
	
	lineStr db "                                                                                $"
	countStr	dw (?)
	tempDNT	db "00/00/0000 00:00:00$"	
	msgKeletInst	db	"Enter 1 For Sort By Name Player 1",10,13,"Enter 2 For Sort By Name Player 2",10,13,"Enter 3 For Sort By Name Winner",10,13,"Enter 4 For Sort By Beggin Date",10,13,"Enter 5 For Sort By End Date",10,10,13,'$'
	keletMsg db "Please Enter Input Number(1...5): $"
	
	buffStr		db 6000h dup('$')	
	fNameStr	db 'Tbl.txt',0		
	fPStr		dw ?	
		
	buffR		db 600h dup('$')	
	fNameR		db 'Results.txt',0		
	fpr			dw ?
	
	keletArr db 5h dup(?)
	KELET_LEN = ($-keletArr)/type keletArr
	startAreas db 5h dup(?)
	endAreas db	5h dup (?)
	
	colorArray	db	00001111b,00001000b
	countCompGames	db	0
	
	msgColorSelect	db	"Please Select Your Side:$"
	
	subMenu db	" Blue "
	S_MEN_LEN = ($-subMenu)
	db		"White "
	
	rnk	RankMove	300d dup(< , , , , , >)
	
dseg ends

sseg segment stack	;Stack
	dw 	100h 	dup(?)
sseg ends

cseg segment
assume cs:cseg,ds:dseg,ss:sseg

ClrScr Proc	;Clearing The Screen
				push ax cx dx
				mov cl, 30h
				mov dl, 0ah
				mov ah, 2h
	Clr:		int 21h
				loop Clr
				push 0h
				call GoToXY
				pop dx cx ax
				ret
ClrScr	EndP

PaintStr	Proc
				push bp
				mov bp, sp
				push ax bx cx dx es bp 		 
				mov al, 1
				mov bh, 0
				mov bl, [bp+6] ;Color
				mov cx, [bp+8] ;Length
				mov dx, [bp+10];Location
				push ds
				pop es        
				mov bp, [bp+4] ;Offset String
				mov ah, 13h
				int 10h
				pop bp es dx cx bx ax bp
				ret 8
PaintStr	Endp

DNTtoTemp Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov di, [bp+4] ;offset tempDNT
				mov cx, [bp+6] ;number to write...
				;mov di, [bp+8] ;Counter of tempDNT
				mov bx, 10h
	PushDay:	mov ax, cx
				mov dx, 0h
				div bx
				mov cx, ax
				add dl, 30h
				push dx
				inc si
				cmp ax, 0h
				jnz PushDay				
				mov di, [bp+4]
				add di, [bp+8]
				cmp si, 1h
				jnz PopDay
				push 0h
				inc si				
	PopDay:		pop dx			
				mov [di], dl
				inc di
				dec si
				jnz PopDay	
				sub di, [bp+4]
				mov [bp+10], di
				pop di si dx cx bx ax bp
				ret 6
DNTtoTemp Endp

GetTnD 		Proc
				push ax bx cx dx si di	
				
				mov di, 0h
				mov si, 0h
				
				mov ah, 2ah ; Date || CX - Year || DH - Month || DL - Day
				int 21h	
				
				;Day
				push dx cx
				mov dh, 0h
				push 0h dx
				call ConHex
				pop cx
				push 0h di cx offset tempDNT
				call DNTtoTemp
				pop di
				pop cx dx
				
				inc di
				
				;Month:
				push dx cx
				mov dl, dh
				mov dh, 0h
				push 0h dx
				call ConHex
				pop cx
				push 0h di cx offset tempDNT
				call DNTtoTemp
				pop di
				pop cx dx
				
				inc di
				
				;Year:
				push dx cx
				push 0h cx
				call ConHex
				pop cx
				push 0h di cx offset tempDNT
				call DNTtoTemp
				pop di
				pop cx dx
				
				inc di
						
				mov ah, 2ch ; Time || CH - Hour || CL - Minitue || DH - Second
				int 21h
				
				;Hour:
				push dx cx
				mov cl, ch
				mov ch, 0h
				push 0h cx
				call ConHex
				pop cx
				push 0h di cx offset tempDNT
				call DNTtoTemp
				pop di
				pop cx dx
				
				inc di
				
				;Minitue:
				push dx cx
				mov ch, 0h
				push 0h cx
				call ConHex
				pop cx
				push 0h di cx offset tempDNT
				call DNTtoTemp
				pop di
				pop cx dx
				
				inc di
				
				;Second
				push dx cx
				mov dl, dh
				mov dh, 0h
				push 0h dx
				call ConHex
				pop cx
				push 0h di cx offset tempDNT
				call DNTtoTemp
				pop di
				pop cx dx
											
				pop di si dx cx bx ax
				ret 
GetTnD		EndP

DNTPusher Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]    ;offset DNTB or Offset DNTE
				mov di, [bp+6] 	  ;offset Temp DNT
				mov cx, [bp+8]    ;TempDNTLengh
	@@Again:	mov al, [di]
				mov [si], al
				inc si
				inc di
				loop @@Again
				pop di si dx cx bx ax bp 
				ret 6
DNTPusher Endp

ConHex 		Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov dx, 0h
				mov di, 0h
				mov ax, [bp+4]
				mov bx, 0ah
				mov si, 0
		Halek:	mov dx, 0h
				div bx ; חילוק
				push dx	
				inc si
				cmp ax, 0h	
				jnz Halek				
		OutPut:	pop ax								
				dec si
				push 0h si 10h  ;10h^si
				call Hezka
				pop bx				
				mul bx
				add di, ax 
				cmp si, 0h
				jnz OutPut						
				mov [bp+6], di
				pop di si dx cx bx ax bp
				ret 2
ConHex 		Endp

Hezka 		Proc
				push bp
				mov bp, sp				
				push ax bx cx dx si di
				mov ax, [bp+4] ;(al)^bl
				mov cx, ax
				mov bx, [bp+6] ;al^(bl)
				cmp bx, 1h
				jz @@Sof		
				cmp bx, 0h
				jnz @@Con
				mov cx, 1h
				jmp @@Sof
	@@Con:		mov si, 1h
	@@Again:	mul cx
				mov cx, ax
				mov ax, [bp+4]
				inc si
				cmp si, bx
				jnz @@Again
	@@Sof:		mov [bp+8], cx
				pop di si dx cx bx ax bp
				ret 4				
Hezka		Endp

SelectPlayerColorMenu	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di bp
				mov dx, 0823h
				mov bl, 00001111b
				mov cx, S_MEN_LEN
				mov si, 0h
				mov bp, offset subMenu
				push dx cx bx bp
				call PaintStr
				inc dh				
				add bp, cx
				push dx cx bx bp
				call PaintStr
				mov bp, offset subMenu
				mov bl, 00111111b
				mov dx, 0823h
				push dx cx bx bp
				call PaintStr				
	@@Kelet:	mov ah, 7h
				int 21h
				cmp al, 20h
				jnz @@Dkelet
				jmp @@Sof
	@@DKelet:	cmp al, 0
				jnz @@Kelet
				mov ah, 7h
				int 21h
				cmp al, 50h
				jz @@DownMove
				cmp al, 48h
				jz @@UpMove		
				jmp @@Kelet
	@@DownMove:	cmp si, 1h
				jz @@Kelet
				mov bl, 00001011b
				push dx cx bx bp
				call PaintStr						
				add bp, cx
				inc dh
				inc si				
				mov bl, 00111111b
				push dx cx bx bp
				call PaintStr
				jmp @@Kelet
	@@UpMove:	cmp si, 0h
				jz @@Kelet
				mov bl, 00001111b
				push dx cx bx bp
				call PaintStr				
				dec dh
				sub bp, cx
				dec si				
				mov bl, 00111111b
				push dx cx bx bp
				call PaintStr
				jmp @@Kelet
	@@Sof:		pop bp 
				mov [bp+4], si		
				pop di si dx cx bx ax bp
				ret
SelectPlayerColorMenu	EndP

MenuF	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di bp
	StartM:		mov dx, 0823h	;First Location
				mov bl, 00001111b	;First Color
				mov cx, MEN_LEN
				mov si, 0h
				mov bp, offset menu				
	@@Again:	push dx cx bx bp
				call PaintStr
				inc dh
				add bp, cx
				inc si
				cmp si, 5h				
				jnz @@Again
				mov si, 0h
				mov bp, offset menu
				mov bl, 00111111b
				mov dx, 0823h
				push dx cx bx bp
				call PaintStr
	@@Kelet:	mov ah, 7h
				int 21h
				cmp al, 20h
				jnz @@Dkelet
				jmp @@Sof
	@@DKelet:	cmp al, 0
				jnz @@Kelet
				mov ah, 7h
				int 21h
				cmp al, 50h
				jz @@DownMove
				cmp al, 48h
				jz @@UpMove		
				jmp @@Kelet
	@@DownMove:	cmp si, 4h
				jz @@Kelet
				mov bl, 00001111b
				push dx cx bx bp
				call PaintStr						
				add bp, cx
				inc dh
				inc si				
				mov bl, 00111111b
				push dx cx bx bp
				call PaintStr
				jmp @@Kelet
	@@UpMove:	cmp si, 0h
				jz @@Kelet
				mov bl, 00001111b
				push dx cx bx bp
				call PaintStr				
				dec dh
				sub bp, cx
				dec si				
				mov bl, 00111111b
				push dx cx bx bp
				call PaintStr
				jmp @@Kelet
	@@Sof:		pop bp 
				mov [bp+4], si
				pop di si dx cx bx ax bp				
				ret 
MenuF EndP

Kitov Proc
				push bp
				mov bp, sp
				push ax bx cx dx						
				mov dx, 0000h			
				mov ah, 2h
				int 10h			
				mov dx, [bp+4] ; Offset Path
				mov cx, 20h
				mov al, 0h
				mov ah, 3dh
				int 21h				   
				jc ErrorMsg
				mov bx, ax
				mov cx, 10h
				mov dx, [bp+6] ; Offset Buffer
	@@again:	mov ah, 3fh
				int 21h		
				jc ErrorMsg
				add dx, cx
				cmp ax, 0h
				jnz @@again	
				mov dx, [bp+6]
				mov ah, 9h
				int 21h			
				jmp @@Sof
	ErrorMsg:	mov dl, 'E'
				mov ah, 2h
				int 21h
	@@Sof:		
				pop dx cx bx ax bp
				ret 4     
Kitov EndP


Square proc 	;Get:  Offset Of Square, Cols, Location, Color, Rows - Drawing Square By Settings
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov di, [bp+4]											;Rows Length
				mov bl, [bp+6]											;Color			
				mov cx, [bp+10]											;Cols Length												
				mov si, [bp+12]											;offset sqStr
				mov dx, [bp+8]			
	WrShu:		push cx				
				mov al, [si]	
				mov cx, 1h
				mov bh, 0h
				mov ah, 2h
				int 10h
				mov ah, 9h
				int 10h	
				inc dl
				inc si
				pop cx
				cmp al, 0h
				jz SkipTabs
				loop WrShu
				mov cx, [bp+10]			
				inc dh			
				sub dl, cl
				dec di
				jnz WrShu			
				jmp @@Sof
	SkipTabs:	push ax
				mov ax, [bp+10]
				sub ax, 3h
				add si, ax			
				add dl, al
				pop ax
				mov cl, 1h
				jmp WrShu
	@@Sof:		pop di si dx cx bx ax bp
				ret	10
Square EndP

DrawSquareFunc proc		;Get : Square Rows, Square Cols - Drawing Board of 8x8 By Using Square Function
				push bp
				mov bp, sp
				push ax bx cx dx si di									
				mov ax, [bp+4]
				mov bx, [bp+6]
				push bx
				mov di, 1
				mov si, 8h											;Board Rows Length
				mov dh, 0
	DrawC:		mov dl, 0dh   										;First Square Location													
				mov cl, 8h											;Board Cols Length																	
	DrawR:		cmp di, 1											;Color Menager
				jnc @@Color2
	@@Color1:	mov bl, 00001000b
				mov di, 1
				jmp StSq
	@@Color2:	mov bl, 00001111b
				mov di, 0
	StSq:		push offset sqStr SQ_COLS dx bx SQ_ROWS
				call Square						
				add dl, al	
				loop DrawR
				dec di	
				pop bx
				push bx
				add dh, bl				
				dec si		
				jnz DrawC
				pop bx di si dx cx bx ax bp
				ret 4
DrawSquareFunc EndP

DrawSoldiers Proc ; Get: Offset of Tools, Tools Array Lenght- Draw The Tools By Their Settings
				push bp
				mov bp, sp	
				push ax bx cx dx si
				mov si, [bp+6]
				mov cx, [bp+4]
	NextTls:	push cx
				cmp [si].isEaten, 1h
				jz @@Continue			
				mov bl, [si].toolColor	
	LocNDraw:	mov dl, [si].xLoc
				mov dh, [si].yLoc
				mov ah, 2h
				int 10h
				mov al, [si].typeTool
				mov cx, 1h			
				mov bh, 0h
				mov ah, 9h
				int 10h			
	@@Continue:	pop cx
				add si, type Tls
				loop NextTls
				pop si dx cx bx ax bp
				ret 4
DrawSoldiers EndP

ToolExist Proc	;Get : Adress, Location, Length
				push bp
				mov bp, sp
				push ax cx dx si di
				mov si, [bp+4]
				mov dx, [bp+6]
				mov cx, [bp+8]
	@SearchArr:	mov al, [si].xLoc
				mov ah, [si].yLoc
				cmp dx, ax
				jz Exist
				add si, type Tls			
				loop @SearchArr
				jmp @@Sof
	Exist:		mov [bp+10], si							
	@@Sof:		pop di si dx cx ax bp
				ret 6			
ToolExist EndP	;Retunr: si= Adress - exist, 0 = Nothing

PaintSq	Proc
				push bp
				mov bp, sp
				push bx				
				mov bx, [bp+4] ;Paint
				mov bh, 0h
				push offset sqStr SQ_COLS dx bx SQ_ROWS
				call Square				
				pop bx bp
				ret 2
PaintSq	EndP

FirstSpace	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov bl, [bp+4] ; xTurn
				mov cl, 0h		;Selecter , 0 = didnt select yet, 1 = select success		
				;xor di, 1h			
				push dx
				add dl, 3h
				inc dh
				push 0h TLS_LEN dx offset Tls
				call ToolExist
				pop si
				pop dx
				cmp si, 0h			
				jnz TrySelect	;if Tool is exist					
				jmp SofFirstSpace
	TrySelect:	cmp [si].toolColor, bl
				jz SelectSuccess
				mov si, 0h
				jmp SofFirstSpace
	SelectSuccess:
				mov cl, 1h				
				push 04h
				call PaintSq			
				push bx dx
				mov bh, 0h
				add dl, 3h
				inc dh
				mov ah, 2h
				int 10h
				mov dl, ' '
				mov ah, 2h
				int 21h
				pop dx bx			
	SofFirstSpace:
				mov [bp+6], si
				pop di si dx cx bx ax bp
				ret 2
FirstSpace	EndP

Cheats	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
	@@Again:	mov ah, 7h
				int 21h
				cmp al, 'o'
				jz OneWin
				cmp al, 't'
				jz TwoWin
				cmp al, 'n'
				mov [bp+4], 1h
				jnz @@Jagain
				jmp @@Sof				
	@@Jagain:	jmp @@Again
	OneWin:		push xTurn[0]	
				pop ax
				jmp @@Con
	TwoWin:		push xTurn[1]
				pop ax
	@@Con:		cmp ax, 0bh
				jz @@P1Win				
	@@P2Win:	call ClrScr
				push ax bx cx si di
				mov cx, 0h				
				push ax cx dx
				mov si, offset tbl
				mov cx, countStr
				mov al, type tbl
				mul cl
				add si, ax
				pop dx cx ax
				lea di, [si].Winner
				lea bx, [si].NameP2
				mov cl, [bx]
	@@WriteWin1:	
				mov al, [bx]
				mov [di], al
				inc bx 
				inc di
				loop @@WriteWin1
				pop di si cx bx ax
				push offset buffer2 offset p2Winner 
				call Kitov
				mov ah, 7h
				int 21h
				jmp @@Sof
	@@P1Win:	call ClrScr
				push ax bx cx si di
				mov cx, 0h
				mov si, offset tbl
				push ax cx dx
				mov si, offset tbl
				mov cx, countStr
				mov al, type tbl
				mul cl
				add si, ax
				pop dx cx ax
				lea di, [si].Winner
				lea bx, [si].NameP1
				mov cl, [bx]
	@@WriteWin2:
				mov al, [bx]
				mov [di], al
				inc bx 
				inc di
				loop @@WriteWin2
				pop di si cx bx ax
				push offset buffer2 offset p1Winner  
				call Kitov
				mov ah, 7h
				int 21h
				mov [bp+4], 0h
	@@Sof:		pop di si dx cx bx ax bp
				ret
Cheats	EndP

TurnXTurn Proc
				push ax
				push xTurn[0]	;First xTurn
				pop ax			;temp = xTurn[0]    ax --> temp; 
				push xTurn[2]	;Second xTurn
				pop xTurn[0]	;xTurn[0] = xTurn[1]
				push ax			;temp
				pop xTurn[2]    ;xTurn[1] = temp
				pop ax
				ret				
TurnXTurn EndP

PlayChess Proc
				push bp  
				mov bp, sp
				push ax bx cx dx si di			
				mov ah, 0h					;indexer
				mov cl, 0h 					;SpaceCounter
				mov ch, 0h 					;xTurn indexer			
				
	Klita:		mov di, [bp+4]	;offset colorArray 	;Colour Manager	Idexer - 0 = white, 1 = Gray, 2+3 = Red
				mov bx, [bp+6]	;offset xTurn		;xTurn Manager 0 - P1, 1 = P2				
				mov al, ah
				mov ah, 0h
				add di, ax					;Indexer Color	
				mov ah, al	
				push cx
				cmp ch, 1h
				jnz DontIncIt
				inc ch
	DontIncIt:	add bl, ch					;Indexer xTurn		
				pop cx
				push ax						;1111
				mov ah, 7h
				int 21h
				cmp al, 0h				
				jz DoubleKey
				cmp al, 20h
				jnz @@Con
				pop ax						;1111
				jmp SpaceClicked				
	@@Con:		cmp al, 'e'
				pop ax						;1111				
				jz JmpSofi	
				push 0h
				call Cheats
				pop ax		;1 = N pressed, 0 = Done
				cmp al, 0h
				jz JmpSofi
				jmp Klita
	JmpSofi:	jmp @@Sof
	DoubleKey:	pop ax						;1111
				push ax
				mov ah, 7h
				int 21h				
				push [di]					;White or Blue
				call PaintSq
				cmp al, 04dh
				jz RightMove
				cmp al, 04bh
				jz LeftMove
				cmp al, 48h
				jz UpMove
				cmp al, 50h
				jz DownMove		
				pop ax
				jmp Klita
	RightMove:	add dl, 7h
				cmp dl, 045h
				jz RightByWall
				jmp PaintSqu
	RightByWall:
				mov dl, 0dh
				jmp PaintSqu
	LeftMove:	sub dl, 7h
				cmp dl, 6
				jz LeftByWall
				jmp PaintSqu
	LeftByWall:	mov dl, 03eh
				jmp PaintSqu
	UpMove:		sub dh, 3h
				cmp dh, -3h
				jz UpByWall
				jmp PaintSqu
	UpByWall:	mov dh, 15h
				jmp PaintSqu
	DownMove:	add dh, 3h
				cmp dh, 18h
				jz DownByWall
				jmp PaintSqu
	DownByWall:	mov dh, 0h
				jmp PaintSqu	
	PaintSqu:	pop ax
				push ax
				cmp cl, 1h
				jz RedPointer
				mov ax, 09h
				jmp PaintIt
	RedPointer:	mov ax, 04h
	PaintIt:	push ax			;Pointer-Saman
				call PaintSq
				pop ax
				xor ah, 1h
				jmp Klita				
	SpaceClicked:
				cmp cl, 1h
				jz SecSpace
				push 0h		;For Si Thats Come later... (stam)
				push [bx]
				call FirstSpace
				pop si
				;xor ah, 1h
				cmp si, 0h
				jnz Contin					;if ToolSelected
				jmp Klita		
	Contin:		mov cl, 1h					;FirstSpace Were Clicked
				jmp Klita
	SecSpace:	push ax
				push 0h ;StamYano
				push [bx] si dx				
				call SecondSpace
				pop ax
				push ax
				push 0h			;For ax.. (stam)				
				push bx			;xTrun Yano..
				call CheckMate
				pop ax			;1 = Mate! 0 = Nothing, Continue...
				cmp al, 1h
				pop ax
				jnz @@NoMate
				pop ax
				jmp @@Sof
	@@NoMate:	push offset Tls TLS_LEN
				call DrawSoldiers				
				cmp al, 0h					;if(al==1){Ho zaz}
				pop ax
				jnz ChangeTurn
				mov cl, 0h
				jmp Klita
	ChangeTurn:	xor ch, 1h
				mov cl, 0h					;Ipus Space Counter
				jmp Klita			
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
PlayChess EndP

SecondSpace Proc ; Get: Tool That had been Prresed, Location To Move, xTurn Idexer
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov dx, [bp+4]
				add dl, 3h
				inc dh
				mov si, [bp+6]
				mov bx, [bp+8]			;xTurn
				mov cl, [si].xLoc
				mov ch, [si].yLoc
				mov ax, dx			
				cmp [si].typeTool, 'P'
				jz IsPawn
				cmp [si].typeTool, 'R'
				jz IsRook
				cmp [si].typeTool, 'B'
				jz IsBishop
				cmp [si].typeTool, 'Q'
				jnz @@ConCheck
				jmp IsQueen
	@@ConCheck:	cmp [si].typeTool, 'H'
				jnz @@HSof
				jmp IsHorse
	@@HSof:		cmp [si].typeTool, 'K'
				jnz @@ToSof
				jmp IsKing
	@@ToSof:	jmp @@Sof
	IsPawn:		push dx si bx ;xTurn[bx]
				call PawnRules				
				jmp @@Con
	IsRook:		push dx si
				call RookRules			
				jmp @@Con	
	IsBishop:	push dx si 
				call BishopRules			
				jmp @@Con
	IsQueen:	push dx si
				call RookRules		
				push dx si
				call BishopRules			
				jmp @@Con
	IsHorse:	push dx si
				call HorseRules			
				jmp @@Con
	IsKing:		push dx si
				call KingRules	
				jmp SkipChess
	@@Con:		push si
				push 0h TLS_LEN bx offset Tls ;xTurn[bx]
				call KingLoc
				pop si
				push 0h si
				call CheckChess
				pop si
				cmp si, 1h
				pop si
				jz @@DontMove	
	SkipChess:	mov al, [si].xLoc
				mov ah, [si].yLoc
				cmp cx, ax
				jz @@DontMove
				mov [bp+10], 1h
				jmp @@Sof
	@@DontMove:	mov si, [bp+6]
				mov [si].xLoc, cl
				mov [si].yLoc, ch
				mov [bp+10], 0h
	@@Sof:		dec dh
				sub dl, 3h
				mov ah, 10h
				int 21h
				push 09h
				call PaintSq
				pop di si dx cx bx ax bp
				ret 6
SecondSpace EndP

LoadTbl proc
				push bp
				mov bp, sp
				push ax bx cx dx di si 
				mov si, [bp+4] ; offset tbl
				mov di, [bp+6] ; offset buffer
				mov cx ,[bp+8] ; countStr
				push dx
				mov ax, type tbl
				mul cx
				pop dx
				mov cx, ax    ; counter sofi
	@@ReDo:		mov al, [di]
				mov [si], al
				inc si
				inc di 
				loop @@ReDo
				pop si di dx cx bx ax bp
				ret 6
LoadTbl EndP
comment **
IsPawnGuard	Proc
				push bp
				mov bp, sp
				push ax bx si
				mov si, [bp+4] 	;tool adress to check if protecting				
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov bl, [si].toolColor	
				cmp bl, 0fh	;if tool is white(down)
				jz @@FromDown
				jmp @@FromUp
				
	@@FromDown:	add al, 7h
				add ah, 3h				
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si				
				cmp si, 0h
				jz @@LeftMFD
				cmp [si].toolColor, bl				
				jnz @@LeftMFD
				cmp [si].typeTool, 'P'
				jnz @@LeftMFD
				jmp @@PProtected
				
	@@LeftMFD:	mov si, [bp+4] ;offset adress to check if protecting
				mov al, [si].xLoc
				mov ah, [si].yLoc
				sub al, 7h
				add ah, 3h
	
				push 0h TLS_LEN ax offset tls
				call ToolExist
				pop si
				cmp si, 0h
				jz @@NoPP	
				cmp [si].toolColor, bl
				jnz @@NoPP
				cmp [si].typeTool, 'P'
				jnz @@NoPP
				jmp @@PProtected
	
	@@NoPP:		jmp @@NoPProtect	
	
	@@FromUp:	add al, 7h
				sub ah, 3h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si				
				cmp si, 0h
				jz @@LeftMFU
				cmp [si].toolColor, bl				
				jnz @@LeftMFU
				cmp [si].typeTool, 'P'
				jnz @@LeftMFU
				jmp @@ToolProtected
				
	@@LeftMFU:	mov si, [bp+4] ;offset adress to check if protecting
				mov al, [si].xLoc
				mov ah, [si].yLoc
				sub al, 7h
				sub ah, 3h	
				push 0h TLS_LEN ax offset tls
				call ToolExist
				pop si
				cmp si, 0h
				jz @@NoPProtect
				cmp [si].toolColor, bl
				jnz @@NoPProtect
				cmp [si].toolColor, 'P'
				jnz @@NoPProtect
				jmp @@ToolProtected
	@@NoPProtect:
				mov [bp+6], 0h
				jmp @@Sof
	@@ToolProtected:
				mov [bp+6], si
	@@Sof:		pop si bx ax bp
				ret 2
IsPawnGuard	Endp

IsHorseGuard Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] 	;tool adress to check if protecting							
				mov bl, [si].toolColor				
				mov di, 8h ; 0 - URR, 1 - URU, 2 - ULL, 3 - ULU, 4 - DLL, 5 - DLD, 6 - DRR, 7 - DRD
					
	@@DiDec:	dec di
				mov al, [si].xLoc
				mov ah, [si].yLoc				
				jmp @@WhereToJmp
				
	@@URR:		add al, 0eh
				sub ah, 3h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@URU:		add al, 7h
				sub ah, 6h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@ULL:		sub al, 0eh
				sub ah, 3h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@ULU:		sub al, 7h
				sub ah, 6h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@DLL:		sub al, 0eh
				add ah, 3h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@DLD:		sub al, 7h
				add ah, 6h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@DRR:		add al, 0eh
				add ah, 3h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
				
	@@DRD:		add al, 7h
				add ah, 6h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@Con:		push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h	; if(IsToolExist() == true/false) = 				
				jz @@JDecD
				cmp [si].toolColor, bl
				jnz @@JDecD
				cmp [si].toolColor, 'H'
				jnz @@JDecD
				jmp @@ToolProtected
				
	@@JDecD:	jmp @@DecDi
				
	@@WhereToJmp:
				cmp di, 7h
				jnz @@ConTo6
				jmp @@DRD
	@@ConTo6:	cmp di, 6h
				jnz @@ConTo1
				jmp @@DRR
	@@ConTo5:	cmp di, 5h
				jnz @@ConTo0
				jmp @@DLD
	@@ConTo4:	cmp di, 4h
				jnz @@ConTo3
				jmp @@DLL
	@@ConTo3:	cmp di, 3h
				jnz @@ConTo2
				jmp @@ULU
	@@ConTo2:	cmp di, 2h
				jnz @@ConTo1
				jmp @@ULL
	@@ConTo1:	cmp di, 1h
				jnz @@ConTo0
				jmp @@URU
	@@ConTo0:	cmp di, 0h
				jnz @@NoHProtectd
				jmp @@URR
	
	@@NoHProtectd:
				mov [bp+6], 0h
				jmp @@Sof
	@@ToolProtected:
				mov [bp+6], si				
	@@Sof:		pop di si dx cx bx ax bp
				ret 2
IsHorseGuard Endp

IsBishopGuard Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ; offset tool to check if guard		
				mov bl, [si].toolColor
				mov di, 4h ; 0 - HorizoR, 1 - HorizoL, 2 - VertiU, 3 - VertiD
				mov dx, 0h
	
	@@DiDec:	dec di
				mov al, [si].xLoc
				mov ah, [si].yLoc				
				jmp @@WhereToJmp
				
	@@RU:		add al, 7h
				sub ah, 3h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
	
	@@LU:		sub al, 7h
				sub ah, 3h
				cmp al, 0dh		;check Limits
				jc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
	
	@@RD:		add al, 7h
				add ah, 3h
				cmp al, 03ch 
				jnc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@LD:		sub al, 7h
				add ah, 3h
				cmp al, 0dh
				jc @@DecDi
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@Con:		push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h	; if(IsToolExist() == true/false) = 
				jz @@JDecD
				cmp [si].toolColor, bl
				jnz @@JDecD
				cmp [si].toolColor, 'B'
				jnz @@JDecD
				jmp @@ToolProtected
				
	@@JDecD:	jmp @@DecDi
	
	@@WhereToJmp:
				cmp di, 3h
				jnz @@ConTo2
				jmp @@RU
	@@ConTo2:	cmp di, 2h
				jnz @@ConTo1
				jmp @@LU
	@@ConTo1:	cmp di, 1h
				jnz @@ConTo0
				jmp @@RD
	@@ConTo0:	cmp di, 0h
				jnz @@NoBProtect
				jmp @@LD
				
	@@NoBProtect:
				mov [bp+6], 0h
				jmp @@Sof
	@@ToolProtected:	
				mov [bp+6], si
	@@Sof:		pop di si dx cx bx ax bp
				ret 2
IsBishopGuard Endp

IsRookGuard	EndP
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ; offset tool to check if protected				
				mov di, 4h ; 0 - HorizoR, 1 - HorizoL, 2 - VertiU, 3 - VertiD
	
	@@DiDec:	dec di
				mov al, [si].xLoc
				mov ah, [si].yLoc				
				jmp @@WhereToJmp
				
	@@HorizoR:	add al, 7h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				jmp @@Con
	
	@@HorizoL:	sub al, 7h
				cmp al, 0dh		;check Limits
				jc @@DiDec
				jmp @@Con
	
	@@VertiU:	sub ah, 3h
				cmp ah, 1h 
				jc @@DiDec
				jmp @@Con
				
	@@VertiD:	add ah, 3h
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@Con:		push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h	; if(IsToolExist() == true/false) = 
				jz @@JDecD
				cmp [si].toolColor, bl
				jnz @@JDecD
				cmp [si].toolColor, 'R'
				jnz @@JDecD
				jmp @@ToolProtected
				
	@@JDecD:	jmp @@DecDi
				
	@@WhereToJmp:
				cmp di, 3h
				jnz @@ConTo2
				jmp @@VertiD
	@@ConTo2:	cmp di, 2h
				jnz @@ConTo1
				jmp @@VertiU
	@@ConTo1:	cmp di, 1h
				jnz @@ConTo0
				jmp @@HorizoL
	@@ConTo0:	cmp di, 0h
				jnz @@NoRProtect
				jmp @@HorizoR			
					
	@@NoRProtect:
				mov [bp+6], 0h
				jmp @@Sof
	@@ToolProtected:	
				mov [bp+6], si
				
	@@Sof:		pop di si dx cx bx ax bp
				ret 2
IsRookGuard	Endp

IsKingGuard	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ; offset King				
				mov di, 8h ; 0 - R, 1 - UR, 2 - U, 3 - UL, 4 - L, 5 - LD, 6 - D, 7 - DR
	
	@@DiDec:	dec di
				mov al, [si].xLoc
				mov ah, [si].yLoc				
				jmp @@WhereToJmp
				
	@@R:		add al, 7h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				jmp @@Con
				
	@@UR:		add al, 7h
				sub ah, 3h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@U:		sub ah, 3h	 		;check Limits
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@UL:		sub al, 7h
				sub ah, 3h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@L:		sub al, 7h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				jmp @@Con
				
	@@DL:		sub al, 7h
				add ah, 3h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@D:		add ah, 3h 						
				cmp ah, 18h			;check Limits
				jnc @@DiDec		
				jmp @@Con
				
				
	@@DR:		add al, 7h
				add ah, 3h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@Con:		push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h	; if(IsToolExist() == true/false) = 
				jz @@JDecD
				cmp [si].toolColor, bl
				jnz @@JDecD
				cmp [si].toolColor, 'K'
				jnz @@JDecD
				jmp @@ToolProtected
				
	@@JDecD:	jmp @@DecDi			
	
	@@WhereToJmp:
				cmp di, 7h
				jnz @@ConTo6
				jmp @@DR
	@@ConTo6:	cmp di, 6h
				jnz @@ConTo1
				jmp @@D
	@@ConTo5:	cmp di, 5h
				jnz @@ConTo0
				jmp @@DL
	@@ConTo4:	cmp di, 4h
				jnz @@ConTo3
				jmp @@L
	@@ConTo3:	cmp di, 3h
				jnz @@ConTo2
				jmp @@UL
	@@ConTo2:	cmp di, 2h
				jnz @@ConTo1
				jmp @@U
	@@ConTo1:	cmp di, 1h
				jnz @@ConTo0
				jmp @@UR
	@@ConTo0:	cmp di, 0h
				jnz @@NoKProtect
				jmp @@R
	@@NoKProtect:
				mov [bp+6], 0h
				jmp @@Sof
	@@ToolProtected:
				mov [bp+6], si				
				pop di si dx cx bx ax bp
				ret 2				

IsKingGuard	EndP

IsToolProtected Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ;tool to check if have protect
				mov bx, 0h
				
				push 0h si
				call IsKingGuard
				pop ax
				cmp ax, 0h
				jz @@Con1
				add bl, 1h
				
	@@Con1:		push 0h si
				call IsQueenGuard
				pop ax
				cmp ax, 0h
				jz @@Con2
				add bl, 2h
				
	@@Con2:		push 0h si
				call IsRookGuard
				pop ax
				cmp ax, 0h
				jz @@Con3
				add bl, 3h

	@@Con3:		push 0h si
				call IsBishopGuard
				pop ax
				cmp ax, 0h
				jz @@Con4
				add bl, 4h
	
	@@Con4:		push 0h si
				call IsHorseGuard
				pop ax
				cmp ax, 0h
				jz @@Con5
				add bl, 4h
				
	@@Con5:		push 0h si
				call IsPawnGuard
				pop ax
				cmp ax, 0h
				jz @@Sof
				add bl, 5h
				
				
	@@Sof:		mov [bp+6], bl
				pop di si dx cx bx ax bp				
				ret 2
IsToolProtected EndP	;return : 0 - no tool protecting, tool adress - the weakest tool protecting

PawnRules Proc	;Get: Adress Of Tools Array, Location To Move
				push bp
				mov bp, sp
				push ax bx cx dx si di			
				mov si, [bp+6]
				mov dx, [bp+8]			
				mov bl, [si].toolColor
				mov al, [si].xLoc
				mov ah, [si].yLoc	
				mov cx, 1h				
				cmp bl, 00001011b
				jz P1Turn
				jmp P2Turn
	P1Turn:		cmp ah, 4h
				jnz TwoSt1
				mov cx, 2h			
	TwoSt1:		add ah, 3h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 1h
				jc @@Con1
				jmp @@EatTry1
	@@Con1:		cmp ax, dx
				jnz Con11
				cmp dh, 16h
				jnz DMjmp
				jmp Promot
	DMjmp:		jmp @@DoMove
	Con11:  	loop TwoSt1
				jmp @@EatTry1			
	P2Turn:		cmp ah, 13h
				jnz TwoSt2
				mov cx, 2h
	TwoSt2:		sub ah, 3h
				push 0h TLS_LEN dx offset Tls
				call ToolExist
				pop si
				cmp si, 1h
				jc @@Con2
				jmp @@EatTry2
	@@Con2:		cmp ax, dx			
				jnz Con21
				cmp dh, 1h
				jnz DMjmp2
				jmp Promot
	DMjmp2:		jmp @@DoMove
	Con21:		loop TwoSt2
				jmp @@EatTry2
	@@EatTry1:	mov si, [bp+6]
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov bx, ax
				add ah, 3h
				add al, 7h
				add bh, 3h
				sub bl, 7h
				cmp dx, ax
				jz CheckEatAX
				cmp dx, bx
				jz CheckEatBX
				jmp @@Sof
	@@EatTry2:	mov si, [bp+6]
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov bx, ax
				sub ah, 3h
				add al, 7h
				sub bh, 3h
				sub bl, 7h
				cmp dx, ax
				jz CheckEatAX
				cmp dx, bx
				jz CheckEatBX
				jmp @@Sof
	CheckEatAX:	mov cl, [si].toolColor			
				push 0h TLS_LEN ax offset Tls			
				jmp @@Contin
	CheckEatBX:	mov cl, [si].toolColor			
				push 0h  TLS_LEN bx offset Tls			
	@@Contin:	call ToolExist
				pop si
				cmp si, 0h	
				jnz IsEnemy
				jmp @@Sof
	IsEnemy:	cmp cl, [si].toolColor			
				jnz @@DoEat			
				jmp @@Sof	
	Promot:		mov si, [bp+6]
				push offset menuT si
				call Promotion
				jmp @@DoMove
	@@DoEat:	mov [si].xLoc, 0h
				mov [si].yLoc, 0h
				mov [si].isEaten, 1h
				cmp dh, 16h
				jz Promot
				cmp dh, 1h
				jz Promot
	@@DoMove:	mov si, [bp+6]	
				mov [si].xLoc, dl
				mov [si].yLoc, dh			
	@@Sof:		pop di si dx cx bx ax bp
				ret 6
PawnRules EndP ; Return: Move-yes/no, Eat-yes/no, di=Index, si=Adress

RookRules Proc	;Get: Adress Of Tools Array, Location To Move
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov cx, 7h
				mov di, 0h
				mov si, [bp+4]			
				mov dx, [bp+6]
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov bl, [si].toolColor			
	CheckR:		add al, 7h			
				inc di
				cmp ax, dx
				jz RightM
				loop CheckR
				mov di, 0h
				mov al, [si].xLoc
				mov cx, 7h
	CheckL:		sub al, 7h
				inc di
				cmp ax, dx
				jz LeftRM
				loop CheckL
				mov di, 0h
				mov al, [si].xLoc
				mov cx, 7h
	CheckU:		sub ah, 3h
				inc di
				cmp ax, dx
				jnz Lup
				jmp UpRM
	Lup:		loop CheckU
				mov di, 0h
				mov ah, [si].yLoc
				mov cx, 7h
	CheckD:		add ah, 3h
				inc di
				cmp ax, dx
				jnz Ldown
				jmp DownRM
	Ldown:		loop CheckD			
				jmp @@Sof
	RightM:		mov cx, di
				mov al, [si].xLoc
				mov ah, [si].yLoc
	IsEmpR:		add al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz LIsEm
				jmp @@Distrupt
	LIsEm:		loop IsEmpR
				jmp @@DoMove
	LeftRM:		mov cx, di
				mov al, [si].xLoc
				mov ah, [si].yLoc
	IsEmpL:		sub al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz @@Lleft
				jmp @@Distrupt
	@@Lleft:	loop IsEmpL
				jmp @@DoMove
	UpRM:		mov cx, di
				mov al, [si].xLoc
				mov ah, [si].yLoc
	IsEmpU:		sub ah, 3h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz @@Lup
				jmp @@Distrupt
	@@Lup:		loop IsEmpU
				jmp @@DoMove
	DownRM:		mov cx, di	
				mov al, [si].xLoc
				mov ah, [si].yLoc
	IsEmpD:		add ah, 3h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz @@Ldown
				jmp @@Distrupt
	@@Ldown:	loop IsEmpD
				jmp @@DoMove
	@@Distrupt:	cmp bl, [si].toolColor
				jnz @@EatTry
				jmp @@Sof
	@@EatTry:	mov al, [si].xLoc
				mov ah, [si].yLoc
				cmp ax, dx
				jnz @@Sof
				mov [si].xLoc, 0h
				mov [si].yLoc, 0h
				mov [si].isEaten, 1h			
	@@DoMove:	mov si, [bp+4]	
				mov [si].xLoc, dl
				mov [si].yLoc, dh
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
RookRules EndP	; Return: Move-yes/no, Eat-yes/no, di=Index, si=Adress

BishopRules	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]
				mov dx, [bp+6]			
				mov cx, 7h
				mov di, 1h
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov bl, [si].toolColor
	XUR:		add al, 7h
				sub ah, 3h
				cmp ax, dx
				jz URMove
				inc di
				loop XUR
				mov di, 1h
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov cx, 7h
	XUL:		sub ah, 3h
				sub al, 7h
				cmp ax, dx
				jnz LUL
				jmp ULMove
	LUL:		inc di
				loop XUL
				mov di, 1h
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov cx, 7h
	XDR:		add ah, 3h
				add al, 7h
				cmp ax, dx
				jnz LDR
				jmp DRMove
	LDR:		inc di
				loop XDR
				mov di, 1h
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov cx, 7h
	XDL:		add ah, 3h
				sub al, 7h
				cmp ax, dx
				jnz LDL
				jmp DLMove
	LDL:		inc di
				loop XDL
				jmp @@Sof
	URMove:		mov cx, di
				mov al, [si].xLoc
				mov ah, [si].yLoc
	IsEmpUR:	sub ah, 3h
				add al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz	LISEmpR
				jmp @@Distrupt
	LIsEmpR:	loop IsEmpUR
				jmp @@DoMove
	ULMove:		mov cx, di
				mov al, [si].xLoc
				mov ah, [si].yLoc
	IsEmpUL:	sub ah, 3h
				sub al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz LIsEmpUL
				jmp @@Distrupt
	LIsEmpUL:	loop IsEmpUL
				jmp @@DoMove
	DRMove:		mov cx, di
				mov al, [si].xLoc
				mov ah, [si].yLoc
	IsEmpDR:	add ah, 3h
				add al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jnz @@Distrupt
				loop IsEmpDR
				jmp @@DoMove
	DLMove:		mov cx, di
				mov al, [si].xLoc
				mov ah, [si].yLoc
	IsEmpDL:	add ah, 3h
				sub al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jnz @@Distrupt
				loop IsEmpDL
				jmp @@DoMove
	@@Distrupt:	cmp bl, [si].toolColor
				jnz @@EatTry
				jmp @@Sof
	@@EatTry:	mov al, [si].xLoc
				mov ah, [si].yLoc
				cmp ax, dx
				jnz @@Sof	
				mov [si].isEaten, 1h
				mov [si].xLoc, 0h
				mov [si].yLoc, 0h
	@@DoMove:	mov si, [bp+4]	
				mov [si].xLoc, dl
				mov [si].yLoc, dh
	@@Sof:		pop di si dx cx bx ax bp
				ret 4			
BishopRules	EndP

HorseRules	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]
				mov dx, [bp+6]				
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov bl, [si].toolColor
	RUUc:		add al, 7h
				add ah, 6h
				cmp ax, dx
				jz @@DoMove
	LUUc:		mov al, [si].xLoc
				mov ah, [si].yLoc
				sub al, 7h
				add ah, 6h
				cmp ax, dx
				jz @@DoMove
	ULLc:		mov al, [si].xLoc
				mov ah, [si].yLoc
				add ah, 3h
				sub al, 0eh
				cmp ax, dx
				jz @@DoMove
	URRc:		mov al, [si].xLoc
				mov ah, [si].yLoc
				add ah, 3h
				add al, 0eh
				cmp ax, dx
				jz @@DoMove
	DDLc:		mov al, [si].xLoc
				mov ah, [si].yLoc
				sub ah, 6h
				sub al, 7h
				cmp ax, dx
				jz @@DoMove
	DDRc:		mov al, [si].xLoc
				mov ah, [si].yLoc
				sub ah, 6h
				add al, 7h
				cmp ax, dx
				jz @@DoMove
	DRRc:		mov al, [si].xLoc
				mov ah, [si].yLoc
				sub ah, 3h
				add al, 0eh
				cmp ax, dx
				jz @@DoMove
	DLLc:		mov al, [si].xLoc
				mov ah, [si].yLoc
				sub ah, 3h
				sub al, 0eh
				cmp ax, dx
				jz @@DoMove
				jmp @@Sof
	@@DoMove:	push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si 
				cmp si, 0h
				jnz @@IsEnemy	
				jmp @@MoveH
	@@IsEnemy:	cmp bl, [si].toolColor
				jz @@Sof
				mov [si].xLoc, 0h
				mov [si].yLoc, 0h
				mov [si].isEaten, 1h
	@@MoveH:	mov si, [bp+4]	
				mov [si].xLoc, dl
				mov [si].yLoc, dh
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
HorseRules	EndP

KingRules Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]
				mov dx, [bp+6]				
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov bl, [si].toolColor
	Uc:			add ah, 3h
				cmp ax, dx
				jnz URc
				jmp @@DoMove
	URc:		mov al, [si].xLoc
				mov ah, [si].yLoc
				add ah, 3h
				add al, 7h
				cmp ax, dx
				jnz Rc
				jmp @@DoMove
	Rc:			mov al, [si].xLoc
				mov ah, [si].yLoc			
				add al, 7h
				cmp ax, dx
				jnz DRc
				jmp @@DoMove			
	DRc:		mov al, [si].xLoc
				mov ah, [si].yLoc
				sub ah, 3h
				add al, 7h
				cmp ax, dx
				jnz Dc
				jmp @@DoMove
	Dc:			mov al, [si].xLoc
				mov ah, [si].yLoc
				sub ah, 3h
				cmp ax, dx
				jnz DLc
				jmp @@DoMove
	DLc:		mov al, [si].xLoc
				mov ah, [si].yLoc
				sub ah, 3h
				sub al, 7h
				cmp ax, dx
				jnz Lc
				jmp @@DoMove
	Lc:			mov al, [si].xLoc
				mov ah, [si].yLoc
				sub al, 7h
				cmp ax, dx
				jnz LUc
				jmp @@DoMove
	LUc:		mov al, [si].xLoc
				mov ah, [si].yLoc
				add ah, 3h
				sub al, 7h
				cmp ax, dx
				jnz RCastling
				jmp @@DoMove
	RCastling:	mov al, [si].xLoc
				mov ah, [si].yLoc
				add al, 0eh
				cmp ax, dx
				jnz LCastling
				jmp DoRCast
	LCastling:	mov al, [si].xLoc
				mov ah, [si].yLoc
				sub al, 0eh
				cmp ax, dx			
				jz DoLCast
				jmp @@Sof
	DoRCast:	add al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp [si].typeTool, 'R'
				jz RCcon
				jmp @@Sof
	RCcon:		push dx bx
				mov dx, ax
				mov bh, 0h
				mov ah, 2h
				int 10h
				mov dl, ' '
				int 21h
				pop bx dx
				sub [si].xLoc, 0eh
				jmp @@MoveK	
	DoLCast:	sub al, 0eh
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp [si].typeTool, 'R'
				jz LCcon
				jmp @@Sof
	LCcon:		push dx bx
				mov dx, ax
				mov bh, 0h
				mov ah, 2h
				int 10h
				mov dl, ' '
				int 21h
				pop bx dx
				add [si].xLoc, 15h
				jmp @@MoveK
	@@DoMove:	push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si 
				cmp si, 0h
				jnz @@IsEnemy			
				jmp @@MoveK		
	@@IsEnemy:	cmp bl, [si].toolColor
				jz @@Sof
				mov [si].xLoc, 0h
				mov [si].yLoc, 0h
				mov [si].isEaten, 1h
	@@MoveK:	mov si, [bp+4]	
				mov [si].xLoc, dl
				mov [si].yLoc, dh
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
KingRules EndP

WriteStr Proc
				push bp
				mov bp, sp
				push ax dx 
				mov dx, [bp+4] ; offset str
				mov ah, 9h
				int 21h
				pop dx ax bp
				ret 2
WriteStr EndP
comment *
IsRookRitok Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ; offset Tool - that doing ritok
				mov di, [bp+6] ;offset "Who is merotak"
	@@SearchIndex:
				cmp [di], '$'
				jz @@IndexReached
				inc di
				jmp @@SearchIndex
	@@IndexReached:
				mov cx, 4h ; 0 - HorizoR, 1 - HorizoL, 2 - VertiU, 3 - VertiD
				
	@@CxDec:	dec cx
				mov al, [si].xLoc
				mov ah, [si].yLoc				
				jmp @@WhereToJmp
				
	@@HorizoR:	add al, 7h
				cmp al, 03ch 		;check Limits
				jnc @@CxDec
				jmp @@Con
	
	@@HorizoL:	sub al, 7h
				cmp al, 0dh		;check Limits
				jc @@CxDec
				jmp @@Con
	
	@@VertiU:	sub ah, 3h
				cmp ah, 1h 
				jc @@CxDec
				jmp @@Con
				
	@@VertiD:	add ah, 3h
				cmp ah, 18h
				jnc @@CxDec
				jmp @@Con
				
	@@Con:		push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h	; if(IsToolExist() == true/false) = 
				jz @@JDecD	
				cmp [si].toolColor, bl
				jnz @@JDecD
				cmp [si].typeTool, 'R'
				jnz @@JDecD
				mov [di], si
				inc di				;next Limbb
				
	@@JDecD:	jmp @@CxDec
					
	@@WhereToJmp:
				cmp cx, 3h
				jnz @@ConTo2
				jmp @@VertiD
	@@ConTo2:	cmp cx, 2h
				jnz @@ConTo1
				jmp @@VertiU
	@@ConTo1:	cmp cx, 1h
				jnz @@ConTo0
				jmp @@HorizoL
	@@ConTo0:	cmp cx, 0h
				jnz @@Sof
				jmp @@HorizoR
	@@Sof:		pop di si dx cx bx ax bp
				ret 4 
IsRookRitok EndP

IsEatDanger Proc
			
IsEatDanger EndP

IsKingEatDanger Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ; offset King
				mov di, [bp+6] ;offset "offsetArrayDangerEat"
	@@SearchIndex:
				cmp [di], '$'
				jz @@IndexReached
				inc di
				jmp @@SearchIndex
	@@IndexReached:
				mov cx, 8h ; 0 - R, 1 - UR, 2 - U, 3 - UL, 4 - L, 5 - LD, 6 - D, 7 - DR
				
	@@CxDec:	dec cx
				mov al, [si].xLoc
				mov ah, [si].yLoc				
				jmp @@WhereToJmp
				
	@@R:		add al, 7h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				jmp @@Con
				
	@@UR:		add al, 7h
				sub ah, 3h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@U:		sub ah, 3h	 		;check Limits
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@UL:		sub al, 7h
				sub ah, 3h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@L:		sub al, 7h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				jmp @@Con
				
	@@DL:		sub al, 7h
				add ah, 3h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@D:		add ah, 3h 						
				cmp ah, 18h			;check Limits
				jnc @@DiDec		
				jmp @@Con
				
				
	@@DR:		add al, 7h
				add ah, 3h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@Con:		push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h	; if(IsToolExist() == true/false) = 
				jz @@JDecD	
				cmp [si].toolColor, bl
				jnz @@JDecD				
				cmp [si].typeTool, 'K'
				jnz @@JDecD
				mov [di], si
				inc di	
	@@JDecD:	jmp @@CxDec
	@@WhereToJmp:
				cmp cx, 7h
				jnz @@ConTo6
				jmp @@DR
	@@ConTo6:	cmp cx, 6h
				jnz @@ConTo1
				jmp @@D
	@@ConTo5:	cmp cx, 5h
				jnz @@ConTo0
				jmp @@DL
	@@ConTo4:	cmp cx, 4h
				jnz @@ConTo3
				jmp @@L
	@@ConTo3:	cmp cx, 3h
				jnz @@ConTo2
				jmp @@UL
	@@ConTo2:	cmp cx, 2h
				jnz @@ConTo1
				jmp @@U
	@@ConTo1:	cmp cx, 1h
				jnz @@ConTo0
				jmp @@UR
	@@ConTo0:	cmp cx, 0h
				jnz @@Sof
				jmp @@R
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
IsKingEatDanger EndP

IsBishopEatDanger Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ; offset Bishop		
				mov di, [bp+6] ;offset "offsetArrayDangerEat"
	@@SearchIndex:
				cmp [di], '$'
				jz @@IndexReached
				inc di
				jmp @@SearchIndex
	@@IndexReached:				
				mov cx, 4h ; 0 - RU, 1 - LU, 2 - RD, 3 - LD
	
	@@CxDec:	dec cx
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov cx, 7h
				jmp @@WhereToJmp
				
	@@RU:		add al, 7h
				sub ah, 3h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
	
	@@LU:		sub al, 7h
				sub ah, 3h
				cmp al, 0dh		;check Limits
				jc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
	
	@@RD:		add al, 7h
				add ah, 3h
				cmp al, 03ch 
				jnc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@LD:		sub al, 7h
				add ah, 3h
				cmp al, 0dh
				jc @@DecDi
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@Con:		push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h	; if(IsToolExist() == true/false) = 
				jz @@JDecD	
				cmp [si].toolColor, bl
				jnz @@JDecD				
				cmp [si].typeTool, 'B'
				jnz @@JDecD
				mov [di], si
				inc di					
	@@JDecD:	jmp @@CxDec

	@@WhereToJmp:
				cmp cx, 3h
				jnz @@ConTo2
				jmp @@RU
	@@ConTo2:	cmp cx, 2h
				jnz @@ConTo1
				jmp @@LU
	@@ConTo1:	cmp cx, 1h
				jnz @@ConTo0
				jmp @@RD
	@@ConTo0:	cmp cx, 0h
				jnz @@Sof
				jmp @@LD
				
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
IsBishopEatDanger Endp

IsHorseEatDanger Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ; offset Horse		
				mov di, [bp+6] ;offset "offsetArrayDangerEat"
	@@SearchIndex:
				cmp [di], '$'
				jz @@IndexReached
				inc di
				jmp @@SearchIndex				
				mov cx, 8h ; 0 - URR, 1 - URU, 2 - ULL, 3 - ULU, 4 - DLL, 5 - DLD, 6 - DRR, 7 - DRD
				mov dx, 0h
	
	@@CxDec:	dec cx
				mov al, [si].xLoc
				mov ah, [si].yLoc				
				jmp @@WhereToJmp
				
	@@URR:		add al, 0eh
				sub ah, 3h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@URU:		add al, 7h
				sub ah, 6h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@ULL:		sub al, 0eh
				sub ah, 3h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@ULU:		sub al, 7h
				sub ah, 6h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@DLL:		sub al, 0eh
				add ah, 3h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@DLD:		sub al, 7h
				add ah, 6h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@DRR:		add al, 0eh
				add ah, 3h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
				
	@@DRD:		add al, 7h
				add ah, 6h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@Con:		push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h	; if(IsToolExist() == true/false) = 
				jz @@JDecD	
				cmp [si].toolColor, bl
				jnz @@JDecD				
				cmp [si].typeTool, 'H'
				jnz @@JDecD
				mov [di], si
				inc di	
	@@JDecD:	jmp @@CxDec

	@@WhereToJmp:
				cmp cx, 7h
				jnz @@ConTo6
				jmp @@DRD
	@@ConTo6:	cmp cx, 6h
				jnz @@ConTo1
				jmp @@DRR
	@@ConTo5:	cmp cx, 5h
				jnz @@ConTo0
				jmp @@DLD
	@@ConTo4:	cmp cx, 4h
				jnz @@ConTo3
				jmp @@DLL
	@@ConTo3:	cmp cx, 3h
				jnz @@ConTo2
				jmp @@ULU
	@@ConTo2:	cmp cx, 2h
				jnz @@ConTo1
				jmp @@ULL
	@@ConTo1:	cmp cx, 1h
				jnz @@ConTo0
				jmp @@URU
	@@ConTo0:	cmp cx, 0h
				jnz @@Sof
				jmp @@URR
				
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
IsHorseEatDanger EndP

IsRookEatDanger	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ; offset Tool Who can be in eat danger
				mov di, [bp+6] ;offset "offsetArrayDangerEat"
	@@SearchIndex:
				cmp [di], '$'
				jz @@IndexReached
				inc di
				jmp @@SearchIndex
	@@IndexReached:
				mov cx, 4h ; 0 - HorizoR, 1 - HorizoL, 2 - VertiU, 3 - VertiD
				mov dx, 0h
	
	@@CxDec:	dec cx
				mov al, [si].xLoc
				mov ah, [si].yLoc				
				jmp @@WhereToJmp
				
	@@HorizoR:	add al, 7h
				cmp al, 03ch 		;check Limits
				jnc @@CxDec
				jmp @@Con
	
	@@HorizoL:	sub al, 7h
				cmp al, 0dh		;check Limits
				jc @@CxDec
				jmp @@Con
	
	@@VertiU:	sub ah, 3h
				cmp ah, 1h 
				jc @@CxDec
				jmp @@Con
				
	@@VertiD:	add ah, 3h
				cmp ah, 18h
				jnc @@CxDec
				jmp @@Con
				
	@@Con:		push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h	; if(IsToolExist() == true/false) = 
				jz @@JDecD	
				cmp [si].toolColor, bl
				jnz @@JDecD
				cmp [si].typeTool, 'R'
				jnz @@JDecD
				mov [di], si
				inc di				;next Lim
				
	@@JDecD:	jmp @@CxDec
					
	@@WhereToJmp:
				cmp cx, 3h
				jnz @@ConTo2
				jmp @@VertiD
	@@ConTo2:	cmp cx, 2h
				jnz @@ConTo1
				jmp @@VertiU
	@@ConTo1:	cmp cx, 1h
				jnz @@ConTo0
				jmp @@HorizoL
	@@ConTo0:	cmp cx, 0h
				jnz @@Sof
				jmp @@HorizoR
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
IsRookEatDanger	Endp

IsPawnEatDanger	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ;offset tool - can be in danger
				mov di, [bp+6] ;offset "offsetArrayDangerEat"
	@@SearchIndex:
				cmp [di], '$'
				jz @@IndexReached
				inc di
				jmp @@SearchIndex
	@@IndexReached:
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov bl, [si].toolColor
				mov cx, 2h ; Left/Rigt
				
					
	@@DecCx:	dec cx
				mov al, [si].xLoc
				mov ah, [si].yLoc				
				jmp @@WhereToJmp
				
				
	@@EatFromD:	sub ah, 3h
				add al, 7h
				cmp al, 03ch	;checklimits
				jnc @@LeftEatD
				cmp ah, 1h
				jc @@LeftEatD
				jmp @@Con
	
	@@LeftEatD:	sub al, 7h
				sub ah, 3h
				cmp al, 0dh
				jc @@NoPEat
				cmp ah, 1h
				jc @@NoPEat
				jmp @@Con
	
	@@EatFromU:	add ah, 3h
				add al, 7h
				cmp al, 03ch
				jnc @@LeftEatU
				cmp ah, 18h
				jnc @@LeftEatU
				jmp @@Con
				
	@@LeftEatU:	sub al, 7h
				add ah, 3h
				cmp al, 0dh
				jc @@NoPEat
				cmp ah, 18h
				jnc @@NoPEat
				jmp @@Con
	
	@@Con:		push 0h TLS_LEN ax offset tls
				call ToolExist
				pop si
				cmp si, 0h
				jz @@JDecD	
				cmp [si].toolColor, bl
				jnz @@JDecD
				cmp [si].typeTool, 'P'
				jnz @@JDecD
				mov [di], si
				inc di					;Next lim
				
	@@JDecD:	jmp @@DecCx
	
	@@WhereToJmp:
				cmp cx, 1h
				jnz @@ConTo0
				jmp @@CmpColor
	@@ConTo0:	cmp cx, 0h
				jnz @@Sof
	@@CmpColor:	cmp bl, 0fh	;if si.toolcolor == down --> eat must be from up
				jz @@EatFromD
				jmp @@EatFromU		
	@@Sof:		pop di si dx cx bx ax bp
				ret 4				
IsPawnEatDanger EndP
*
CheckChess Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov bl, [si].toolColor
				cmp bl, 00001111b
				jz Plr2	
	Plr1:		add ah, 3h
				jmp @@ConCP
	Plr2:		sub ah, 3h
	@@ConCP:	sub al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz @@ConP
				cmp [si].typeTool, 'P'
				jnz @@ConP
				cmp [si].toolColor, bl
				jz @@ConP
				jmp @@IsChess
	@@ConP:		add al, 0eh
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz @@RookCC
				cmp [si].typeTool, 'P'
				jnz @@RookCC
				cmp [si].toolColor, bl
				jz @@RookCC
				jmp @@IsChess
	@@RookCC:	mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov cx, 7h			
	HorRCC:		add al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz LHorRCC
				cmp [si].typeTool, 'R'
				jnz QRm
				jmp CRC
	QRm:		cmp [si].typeTool, 'Q'
				jnz HorL
	CRC:		cmp [si].toolColor, bl
				jz HorL
				jmp @@IsChess
	LHorRCC:	loop HorRCC
	HorL:		mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov cx, 7h
	HorLCC:		sub al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz LHorL
				cmp [si].typeTool, 'R'			
				jnz QRm2
				jmp CRC2
	QRm2:		cmp [si].typeTool, 'Q'
				jnz VerU
	CRC2:		cmp [si].toolColor, bl
				jz VerU
				jmp @@IsChess
	LHorL:		loop HorLCC
	VerU:		mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov cx, 7h
	VerUCC:		add ah, 3h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz LVerU
				cmp [si].typeTool, 'R'			
				jnz QRm3
				jmp CRC3
	QRm3:		cmp [si].typeTool, 'Q'
				jnz VerD
	CRC3:		cmp [si].toolColor, bl
				jz VerD
				jmp @@IsChess
	LVerU:		loop VerUCC
	VerD:		mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov cx, 7h
	VerDCC:		sub ah, 3h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz LVerD
				cmp [si].typeTool, 'R'			
				jnz QRm4
				jmp CRC4
	QRm4:		cmp [si].typeTool, 'Q'
				jnz @@HorseCC			
	CRC4:		cmp [si].toolColor, bl
				jz @@HorseCC
				jmp @@IsChess
	LVerD:		loop VerDCC
	@@HorseCC:	mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
	RUUcc:		add ah, 6h
				add al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz LUUcc
				cmp [si].typeTool, 'H'
				jnz LUUcc
				cmp [si].toolColor, bl
				jz LUUcc
				jmp @@IsChess
	LUUcc:		mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				add ah, 6h
				sub al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz ULLcc
				cmp [si].typeTool, 'H'
				jnz ULLcc
				cmp [si].toolColor, bl
				jz ULLcc
				jmp @@IsChess
	ULLcc:		mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				add ah, 3h
				sub al, 0eh
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz URRcc
				cmp [si].typeTool, 'H'
				jnz URRcc
				cmp [si].toolColor, bl
				jz URRcc
				jmp @@IsChess
	URRcc:		mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				add ah, 3h
				add al, 0eh
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz DDLcc
				cmp [si].typeTool, 'H'
				jnz DDLcc
				cmp [si].toolColor, bl
				jz DDLcc
				jmp @@IsChess
	DDLcc:		mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				sub ah, 6h
				sub al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si 
				cmp si, 0h
				jz DDRcc
				cmp [si].typeTool, 'H'
				jnz DDRcc
				cmp [si].toolColor, bl
				jz DDRcc
				jmp @@IsChess
	DDRcc:		mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				sub ah, 6h
				add al, 7h
				push 0h  TLS_LEN ax offset Tls
				call ToolExist
				pop si 
				cmp si, 0h
				jz DRRcc
				cmp [si].typeTool, 'H'
				jnz DRRcc
				cmp [si].toolColor, bl
				jz DRRcc
				jmp @@IsChess
	DRRcc:		mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				sub ah, 3h
				add al, 0eh
				push 0h  TLS_LEN ax offset Tls
				call ToolExist
				pop si 
				cmp si, 0h
				jz DLLcc
				cmp [si].typeTool, 'H'
				jnz DLLcc
				cmp [si].toolColor, bl
				jz DLLcc
				jmp @@IsChess
	DLLcc:		mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				sub ah, 3h
				sub al, 0eh
				push 0h  TLS_LEN ax offset Tls
				call ToolExist
				pop si 
				cmp si, 0h
				jz BishCc
				cmp [si].typeTool, 'H'
				jnz BishCc
				cmp [si].toolColor, bl
				jz BishCc
				jmp @@IsChess
	BishCc:		mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov cx, 7h
	URcc:		add ah, 3h
				add al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si 
				cmp si, 0h
				jz @URloop
				cmp [si].typeTool, 'B'
				jnz QBm
				jmp CBC
	QBm:		cmp [si].typeTool, 'Q'
				jnz @ConBicc
	CBC:		cmp [si].toolColor, bl
				jz @ConBicc
				jmp @@IsChess
	@URloop:	loop URcc			
	@ConBicc:	mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov cx, 7h
	ULcc:		add ah, 3h
				sub al, 7h
				push 0h  TLS_LEN ax offset Tls
				call ToolExist
				pop si 
				cmp si, 0h
				jz @ULloop
				cmp [si].typeTool, 'B'
				jnz QBm2
				jmp CBC2
	QBm2:		cmp [si].typeTool, 'Q'
				jnz @ConBicc2
	CBC2:		cmp [si].toolColor, bl
				jz @ConBicc2
				jmp @@IsChess
	@ULloop:	loop ULcc			
	@ConBicc2:	mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov cx, 7h
	DRcc:		sub ah, 3h
				add al, 7h
				push 0h  TLS_LEN ax offset Tls
				call ToolExist
				pop si 
				cmp si, 0h
				jz @DRloop
				cmp [si].typeTool, 'B'
				jnz QBm3
				jmp CBC3
	QBm3:		cmp [si].typeTool, 'Q'
				jnz @ConBicc3
	CBC3:		cmp [si].toolColor, bl
				jz @ConBicc3
				jmp @@IsChess
	@DRloop:	loop DRcc
	@ConBicc3:	mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov cx, 7h
	DLcc:		sub al, 7h
				sub ah, 3h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si 
				cmp si, 0h
				jz DLloop
				cmp [si].typeTool, 'B'
				jnz QBm4
				jmp CBC4
	QBm4:		cmp [si].typeTool, 'Q'
				jnz @@KingCc
	CBC4:		cmp [si].toolColor, bl
				jz @@KingCc
				jmp @@IsChess
	DLloop:		loop DLcc
	@@KingCc:	mov si, [bp+4]	
				mov al, [si].xLoc
				mov ah, [si].yLoc
				add ah, 3h
				push 0h  TLS_LEN ax offset Tls
				call ToolExist
				pop si 
				cmp si, 0h
				jz @@ConKicc
				cmp [si].typeTool, 'K'
				jnz @@ConKicc
				cmp [si].toolColor, bl
				jz @@ConKicc
				jmp @@IsChess
	@@ConKicc:	add al, 7h
				push 0h  TLS_LEN ax offset Tls
				call ToolExist
				pop si 
				cmp si, 0h
				jz @@ConKicc2
				cmp [si].typeTool, 'K'
				jnz @@ConKicc2
				cmp [si].toolColor, bl
				jz @@ConKicc2
				jmp @@IsChess
	@@ConKicc2:	sub ah, 3h
				push 0h  TLS_LEN ax offset Tls
				call ToolExist
				pop si 
				cmp si, 0h
				jz @@ConKicc3
				cmp [si].typeTool, 'K'
				jnz @@ConKicc3
				cmp [si].toolColor, bl
				jz @@ConKicc3
				jmp @@IsChess
	@@ConKicc3:	sub ah, 3h
				push 0h  TLS_LEN ax offset Tls
				call ToolExist
				pop si 
				cmp si, 0h
				jz @@ConKicc4
				cmp [si].typeTool, 'K'
				jnz @@ConKicc4
				cmp [si].toolColor, bl
				jz @@ConKicc4
				jmp @@IsChess
	@@ConKicc4:	sub al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz @@ConKicc5
				cmp [si].typeTool, 'K'
				jnz @@ConKicc5
				cmp [si].toolColor, bl
				jz @@ConKicc5
				jmp @@IsChess
	@@ConKicc5:	sub al, 7h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz @@ConKicc6
				cmp [si].typeTool, 'K'
				jnz @@ConKicc6
				cmp [si].toolColor, bl
				jz @@ConKicc6
				jmp @@IsChess
	@@ConKicc6:	add ah, 3h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jz @@ConKicc7
				cmp [si].typeTool, 'K'
				jnz @@ConKicc7
				cmp [si].toolColor, bl
				jz @@ConKicc7
				jmp @@IsChess
	@@ConKicc7:	add ah, 3h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si 
				cmp si, 0h
				jz @@NoChess
				cmp [si].typeTool, 'K'
				jnz @@NoChess
				cmp [si].toolColor, bl
				jz @@NoChess
				jmp @@IsChess
	@@NoChess:	mov [bp+6], 0h ;Move Is Possible, The King Is Not Blocked 
				jmp @@Sof
	@@IsChess:	mov [bp+6], 1h ;Move Is Imposibble, The King Is Blocked
	@@Sof:		pop di si dx cx bx ax bp
				ret 2			
CheckChess EndP

KingLoc	Proc
				push bp
				mov bp, sp
				push bx cx dx si
				mov si, [bp+4]
				mov bl, [bp+6]
				mov cl, [bp+8]
	Fking:		cmp [si].typeTool, 'K'
				jnz LoopK
				cmp [si].toolColor, bl
				jz KingFound
	LoopK:		add si, Type Tls
				loop FKing
				jmp @@Sof
	KingFound:	mov [bp+10], si
	@@Sof:		pop si dx cx bx bp
				ret 6
KingLoc	EndP

OpenNewFile Proc near
				push bp
				mov bp, sp
				push ax cx dx
				mov dx, [bp+4]	; Name Of File
				mov cx, [bp+6]	; cx=0->normal, cx=1->read only
								; cx=2->hidden, cx=3->system
				mov ah,3ch
				int 21h
				jnc @@SofProc		
				mov dl, 'E'
				mov ah, 2h
				int 21h
				jmp @@SofProc
	@@SofProc:	mov [bp+8], ax									
				pop dx cx ax bp
				ret 4
OpenNewFile	EndP

ReadFile	Proc near
				push bp
				mov bp,sp
				push ax bx cx dx
				mov bx,[bp+4]	; File Pointer
				mov cx,[bp+6]	; Number of bytes that will be read
				mov dx,[bp+8]	; buffer
	NextReading:mov ah,3fh
				int 21h
				jc @@ShowError	; Fail
				add dx, cx
				cmp ax, 0
				jnz NextReading
				jmp @@SofProc
	@@ShowError:mov dl, 'E'
				mov ah, 2h
				int 21h
	@@SofProc:	pop dx cx bx ax bp
				ret 6
ReadFile	EndP

OpenExistFile Proc near
				push bp
				mov bp, sp
				push ax dx
				mov dx, [bp+4]	;Name of file
				mov ax, [bp+6]	;   al=0->read  al=1->write al=2->read/write
				mov ah,3dh
				int 21h
				jnc @@SofProc		
				mov dl, 'E'
				mov ah, 2h
				int 21h
				jmp @@SofProc
	@@SofProc:	mov [bp+8], ax
				pop dx ax bp
				ret 4
OpenExistFile	EndP

WriteToFile	Proc near
				push bp
				mov bp,sp
				push ax bx cx dx
				mov dx,[bp+4]	; Buffer
				mov cx,[bp+6]	; Number of bytes will be write
				mov bx,[bp+8]	; File Pointer
				mov ah, 40h
				int 21h
				jnc @@SofProc
	@@ShowError:mov dl, 'E'
				mov ah, 2h
				int 21h				
	@@SofProc:	pop dx cx bx ax bp
				ret 6
WriteToFile	EndP

CheckMate Proc
				push bp
				mov bp, sp
				push ax bx si
				mov bx, [bp+4]				
				cmp bx, offset xTurn
				jz @@Its0
				sub bx, 2h
				jmp SkipBX
	@@Its0:		add bx, 2h
	SkipBX:		push 0h TLS_LEN
				push [bx]
				push offset Tls
				call KingLoc
				pop si
				cmp [si].isEaten, 1h
				jz @@Contin	
				jmp @@Sof
	@@Contin:	mov [bp+6], 1h
				cmp [bx], 00001011b
				jz P1Win				
	P2Win:		call ClrScr
				push ax bx cx si di
				mov cx, 0h				
				push ax cx dx
				mov si, offset tbl
				mov cx, countStr
				mov al, type tbl
				mul cl
				add si, ax
				pop dx cx ax
				lea di, [si].Winner
				lea bx, [si].NameP2
				mov cl, [bx]
	WriteWin1:	mov al, [bx]
				mov [di], al
				inc bx 
				inc di
				loop WriteWin1
				pop di si cx bx ax
				push offset buffer2 offset p2Winner 
				call Kitov
				mov ah, 7h
				int 21h
				jmp @@Sof
	P1Win:		call ClrScr
				push ax bx cx si di
				mov cx, 0h
				mov si, offset tbl
				push ax cx dx
				mov si, offset tbl
				mov cx, countStr
				mov al, type tbl
				mul cl
				add si, ax
				pop dx cx ax
				lea di, [si].Winner
				lea bx, [si].NameP1
				mov cl, [bx]
	WriteWin2:	mov al, [bx]
				mov [di], al
				inc bx 
				inc di
				loop WriteWin2
				pop di si cx bx ax
				push offset buffer2 offset p1Winner  
				call Kitov
				mov ah, 7h
				int 21h
	@@Sof:		pop si bx ax bp
				ret 2
CheckMate EndP


Writting Proc
				push bp
				mov bp, sp
				push ax bx cx dx bp
				mov al, 1
				mov bh, 0
				mov bl, [bp+4]
				mov cx, [bp+6]
				mov dx, [bp+12]
				mov es, [bp+10]
				mov bp, [bp+8]
				mov ah, 13h
				int 10h
				pop bp dx cx bx ax bp
				ret	10
Writting EndP

Promotion	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si
				mov dx, 0000h
				mov si, [bp+4]
				mov bx, 00001111b
				mov cl, 4h
				push dx es offset menuMsg MENU_LEN bx
				call Writting
				mov ax, [bp+6]
	@Again:		inc dh
				push dx es ax 0ah bx
				call Writting
				add ax, 0ah
				loop @Again
	ReKel:		mov ah, 7h
				int 21h	
				cmp al, 'q'
				jz QProm
				cmp al, 'b'
				jz BProm
				cmp al, 'r'
				jz RProm
				cmp al, 'h'
				jz HProm
				jmp ReKel
	QProm:		mov [si].typeTool, 'Q'
				jmp @@Sof
	BProm:		mov [si].typeTool, 'B'
				jmp @@Sof
	RProm:		mov [si].typeTool, 'R'
				jmp @@Sof
	HProm:		mov [si].typeTool, 'H'
				jmp @@Sof
	@@Sof:		pop si dx cx bx ax bp
				ret 4
Promotion	EndP

ReadFile2	Proc near
				push bp
				mov bp,sp
				push ax bx cx dx di
				mov bx,[bp+4]	; המצביע לקובץ
				mov cx,[bp+6]	; מספר הבתים שיקראו בכל סבב קריאה
				mov dx,[bp+8]	; כתובת הזיכרון שבה יאוחסן המידע שנקרא מהקובץ
				mov di, 0
	@@NextReading1:
				mov ah,3fh
				int 21h
				inc di
				jc @@ShowError1	; כישלון בקריאה
				add dx, cx
				cmp ax, 0
				jnz @@NextReading1
				dec di
				jmp @@SofProc1
	@@ShowError1:
				mov dl, 'e'
				mov ah, 2h
				int 21h
	@@SofProc1:	mov [bp+10], di
				pop di dx cx bx ax bp
				ret 6
ReadFile2	EndP

WriteKeletClue	Proc
				push bp
				mov bp,sp
				push ax bx cx dx si di
				mov di, 0h
				mov cx, KELET_LEN
	OverAgain:	mov si, [bp+4] ;offset KeletArr
				add si, di
				mov bx, 0200h
				mov al, [si]
				cmp al, '1'
				jnz @@ConTo2
				add bl, 9h
				jmp DoneCheck
	@@ConTo2:	cmp al, '2'
				jnz @@ConTo3
				add bl, 14h
				jmp DoneCheck
	@@ConTo3:	cmp al, '3'
				jnz @@ConTo4			
				add bl, 20h
				jmp DoneCheck
	@@ConTo4:	cmp al, '4'
				jnz @@ConTo5
				add bl, 032h
				jmp DoneCheck
	@@ConTo5:	cmp al, '5'
				jnz DoneCheck
				add bl, 46h
	DoneCheck:  push bx
				call GoToXY
				sub si, [bp+4]
				inc si
				add si, 30h
				mov dx, si
				mov ah, 2h
				int 21h
				inc di
				loop OverAgain
				pop di si dx cx bx ax bp
				ret 2 
WriteKeletClue EndP

ShowTbl Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				
				mov si, [bp+4] ;Offset Tbl
				mov bx, [bp+6] ;Location
				mov ax, [bp+8] ; Where To Begin
				mov cx, [bp+10]
				cmp cl, 20d
				jc @@Contin
				mov cx, 20d
	@@Contin:	mov dx, type tbl
				mul dl
				add si, ax				
	@@NextRow:	mov bl, 0
				push bx
				call GoToXY				
				
				lea di, [si].NameP1
				add di, 2h
				lea dx, [di]
				mov ah, 9h
				int 21h				
				
				add bl, 0bh
				push bx 
				call GoToXY
							
				lea di, [si].NameP2
				add di, 2h
				lea dx, [di]
				mov ah, 9h
				int 21h
				
				add bl, 0ch
				push bx 
				call GoToXY
				
				lea di, [si].Winner
				add di, 2h
				lea dx, [di]
				mov ah, 9h
				int 21h
				
				add bl, 0dh
				push bx 
				call GoToXY
				
				lea di, [si].DNTB			
				lea dx, [di]
				mov ah, 9h
				int 21h
				
				add bl, 17h
				push bx 
				call GoToXY
				
				lea di, [si].DNTE			
				lea dx, [di]
				mov ah, 9h
				int 21h
				
				add si, type tbl
				inc bh
				loop @@NextRow
				pop di si dx cx bx ax bp
				ret 8
ShowTbl EndP 

GoToXY Proc
				push bp
				mov bp, sp
				push ax bx dx
				mov bh, 0h
				mov dx, [bp+4] ;Location				
				mov ah, 2h
				int 10h
				pop dx bx ax bp
				ret 2
GoToXY EndP

ClrScr2 Proc 				; cleaning the screan!
				push cx dx ax bx 
				mov bx, 0
				mov dh, 03d
				mov dl , 0d
				mov ah, 2h
				int 10h
				mov dx ,0d
				mov cl, 20d
		@@Clr: 	mov dx, offset lineStr
				mov ah, 9h
				int 21h
				loop @@Clr
				mov bl , 0h     
				mov dl, 0h
				mov dh, 0h
				mov ah, 2h
				int 10h
				pop bx ax dx cx
				ret
ClrScr2 EndP

RecordTable Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov ax, 0h
				mov cx, [bp+6] ;countStr
				dec cx
				mov dl, keletArr[0]
				push offset tbl dx cx 0h	;SortBy()
				call SortByLim
				
				cmp dl, '5'
				jnz Maybe4
				jmp @@EndWhile1
	Maybe4:		cmp dl, '4'
				jnz Mionim
				jmp @@EndWhile1
				
	Mionim:		mov di, [bp+8] ;startAreas
				mov bx, [bp+10] ;endAreas
				mov [di], 0				
				mov [bx], cx ;countStr
				inc [bx]
				
	While1:		push ax
				mov al, [di]
				cmp al, [bx]		;FirstWhile
				pop ax
				jc ConW1
				jmp @@EndWhile1
	ConW1:		mov al, [di]
				mov [di+1], al	;startAreas[1] = startAreas[0]
				
				mov dx, 0h
				mov dl, keletArr[0]				
				push 0h offset tbl ax dx ;ax = FirstLimit!
				call FindLimits
				pop dx				;SofZone				
				;inc  dl
				mov [bx+1], dl		;endAreas[1] = SearchRetsef()
				cmp dx, 0h
				jz SkipSortBy1
				
				push ax cx dx
				mov cx, 0h
				mov ax, 0h
				mov dx, 0h
				mov al, [bx+1]
				mov cl, [di+1]
				cmp al, cl
				jz SkipPre1
				mov dl, keletArr[1]
				push offset tbl dx ax cx	;SortBy()
				call SortByLim
	SkipPre1:	pop dx cx ax
	SkipSortBy1:			
				push ax
				mov al, [bx+1]
				inc al
				mov [di], al		;startAreas[0] = endAreas[1]
				pop ax		
				
	While2:		push ax
				mov al, [di+1]
				cmp al, [bx+1]			;SecondWhile				
				pop ax
				jc	ConW2
				jmp @@EndWhile2
				
	ConW2:		mov al, [di+1]
				mov [di+2], al		;startA[2] = StartA[1]
				
				mov dx, 0h
				mov dl, keletArr[1]				
				push 0h offset tbl ax dx ;ax = FirstLimit!
				call FindLimits
				pop dx				;SofZone
				;inc  dl
				mov [bx+2], dl		;endAreas[2] = SearchRetsef()
				cmp dx, 0h
				jz SkipSortBy2
				
				push ax cx dx
				mov cx, 0h
				mov ax, 0h
				mov dx, 0h
				mov al, [bx+2]
				mov cl, [di+2]
				cmp al, cl
				jz SkipPre2
				mov dl, keletArr[2]
				push offset tbl dx ax cx	;SortBy()
				call SortByLim
	SkipPre2:	pop dx cx ax
	SkipSortBy2:			
				push ax
				mov al, [bx+2]
				inc al
				mov [di+1], al		;startAreas[1] = endAreas[2]
				pop ax
				
	While3:		push ax
				mov al, [di+2]
				cmp al, [bx+2]			;ThirdWhile				
				pop ax
				jc	ConW3
				jmp @@EndWhile3
	ConW3:		mov al, [di+2]
				mov [di+3], al		;startA[3] = StartA[2]
				
				mov dx, 0h
				mov dl, keletArr[2]				
				push 0h offset tbl ax dx ;ax = FirstLimit!
				call FindLimits
				pop dx				;SofZone
				;inc  dl
				mov [bx+3], dl		;endAreas[3] = SearchRetsef()
				cmp dx, 0h
				jz SkipSortBy3
				
				push ax cx dx
				mov cx, 0h
				mov ax, 0h
				mov dx, 0h				
				mov al, [bx+3]
				mov cl, [di+3]
				cmp al, cl
				jz SkipPre3
				mov dl, keletArr[3]
				push offset tbl dx ax cx	;SortBy()
				call SortByLim
	SkipPre3:	pop dx cx ax
	SkipSortBy3:			
				push ax
				mov al, [bx+3]
				inc al
				mov [di+2], al		;startAreas[2] = endAreas[3]
				pop ax			
				
	while4:		push ax
				mov al, [di+3]
				cmp al, [bx+3]			;FourthWhile				
				pop ax
				jc	ConW4
				jmp @@EndWhile4
				
	ConW4:		mov al, [di+3]
				mov [di+4], al		;startA[4] = StartA[3]
				
				mov dx, 0h
				mov dl, keletArr[3]				
				push 0h offset tbl ax dx ;ax = FirstLimit!
				call FindLimits
				pop dx				;SofZone
				;inc  dl
				mov [bx+4], dl		;endAreas[4] = SearchRetsef()
				cmp dx, 0h
				jz SkipSortBy4
				
				push ax cx dx
				mov cx, 0h
				mov ax, 0h
				mov dx, 0h
				mov al, [bx+4]
				mov cl, [di+4]
				cmp al, cl
				jz SkipPre4
				mov dl, keletArr[4]
				push offset tbl dx ax cx	;SortBy()
				call SortByLim
	SkipPre4:	pop dx cx ax
	SkipSortBy4:			
				push ax
				mov al, [bx+4]
				inc al
				mov [di+3], al		;startAreas[3] = endAreas[4]
				pop ax
				jmp While4	
				
	@@EndWhile4:jmp While3	
	@@EndWhile3:jmp While2
	@@EndWhile2:jmp While1				
	@@EndWhile1:push offset buffR offset fNameR
				call Kitov
	@Ipus:		mov bx, 0300h
				call ClrScr2
				mov di, 0h ; (DownOnMe)
				mov si, [bp+4] ;offset tbl
				push countStr di bx si
				call ShowTbl							
				push offset keletArr
				call WriteKeletClue
				mov cx, 1h ; counter up +1
				mov dx , [bp+6] ;countStr				
				inc dx
				cmp dx, 20d
				jnc @UnConnD
				jmp @ConnD
	@UnConnD:	sub dx, 20d ; dx = counter down
	DecCl: 		dec cl	
	KoKelet:	mov ah, 7h
				int 21h
				cmp al, 'b'
				jnz DntBye
				jmp @Bye
	DntBye:		cmp al, 0
				jz @nnDD
				jmp @One
	@nnDD:		mov ah, 7h
				int 21h
				cmp dx, 1h
				jz @NotDown
				cmp al, 50h
				jz @IncDOM
				cmp al , 51h
				jz @PDown
	@NotDown:	cmp cl, 0
				jz KoKelet
				cmp al, 48h
				jz @DecDOM
				cmp al , 49h
				jz @PUp
				jmp KoKelet
	@PDown:		cmp dx, 19d
				jnc @DownP
				dec dx
				add di, dx
				add cx, dx		
				mov dx, 1d
				jmp @ConD
	@DownP:		add di, 20d
				add cl, 20d		
				sub dx, 20d
				jc KoKelet
				jmp @ConD
	@PUp:		cmp cx, 19d
				jnc @UpP
				sub di, cx
				add dx, cx
				mov cl, 0
				jmp @ConD
	@UpP:		sub di, 20
				sub cl, 20d
				add dx, 20
				jmp @ConD
	@IncDOM:	inc di
				inc cl		
				dec dx
				jz KoKelet
				jmp @ConD
	@DecDOM:	dec di
				dec cl
				inc dx
	@ConD:		mov bx, 0300h
				mov si, [bp+4]
				call ClrScr2
				push countStr di bx si
				call ShowTbl			
				jmp KoKelet	
	@ConnD:		mov ah, 7h
				int 21h
				cmp al, 0
				jnz @One
				mov ah, 7h
				int 21h
				jmp @ConnD
	@One:		mov dx, 0
				push bx
				mov bh, 0
				mov ah, 2h
				int 10h
				pop bx
				mov di, 0
				jmp @Ipus
	@Bye:		push KELET_LEN offset keletArr
				call IpusTbl
				pop di si dx cx bx ax bp
				ret 4
RecordTable EndP

IpusTbl Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ; offset keletArr
				mov cx, [bp+6] ;KELET_LEN
	@@Again:	mov [si], 0h
				inc si
				loop @@Again
				pop di si dx cx bx ax bp
				ret 4
IpusTbl Endp

SortByLim Proc
				push bp
				mov bp , sp
				push ax bx cx dx si di	
				mov ax, [bp+4] ;FirstLimit
				mov cx, [bp+6] ;LastLimit				
				sub cx, ax ;Hefresh = LastLimit-FirstLimit;			
				inc cx
				mov dx, [bp+8] ;What To Sort! 
				mov si, [bp+10] ; offset tbl 
				cmp dl, '1'
				jz @nDD1
				cmp dl, '2'
				jz @nDD2
				cmp dl, '3'
				jz @nDD3
				cmp dl, '4'
				jz @nDD4
				cmp dl, '5'
				jz @nDD5								
				jmp @ConnD
	@nDD1:   	lea di, [si].NameP1		
				jmp @@Mion				
	@nDD2:		lea di, [si].NameP2
				jmp @@Mion
	@nDD3:		lea di, [si].Winner
				jmp @@Mion
	@nDD4:		lea di, [si].DNTB
				jmp @@Mion
	@nDD5:		lea di, [si].DNTE				
	@@Mion:		mov dl, [bp+4]	;FirstLimit					
				mov ax, type tbl ; di = di+(typetbl*FirstLim)
				mul dl
				add di, ax					
				mov si, di ;si & di = FirstLimit	
				mov bx, 0h 				
				jmp Dswitch
	@@MionN1:	mov ax, [bp+4] ;FirstLimit
				mov cx, [bp+6] ;LastLimit				
				sub cx, ax ;Hefresh = LastLimit-FirstLimit;	
				inc cx
				push cx
				sub cx, 2h				
				cmp cx, bx
				pop cx
				jnz @@Cont
				jmp @@Sof
	@@Cont:		inc bx
				sub cx, bx
				mov si, di
				add si, type tbl
				add di, type tbl				
	Dswitch:	add si, type tbl
	@@Con:		dec cx
				jz @@MionN1
				push 0h di si
				call CompareTo
				pop dx
				cmp dl, 1h				
				jz DSwitch		
				push dx
				mov dx, [bp+8]
				push dx di si offset temp
				call SwitchIt
				pop dx
				jmp DSwitch					
	@@Sof:		pop di si dx cx bx ax bp
				ret 8
SortByLim EndP

FindLimits Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di 
				mov bx, [bp+4] ;What Limit To Find (namep1, namep2, namewinner, etc.)
				mov al, [bp+6] ;FirstLimit
				mov si, [bp+8] ;offset tbl
				mov cx, ax
				cmp bl, '1'
				jz @@nDD1
				cmp bl, '2'
				jz @@nDD2
				cmp bl, '3'
				jz @@nDD3
				cmp bl, '4'
				jz @@nDD4
				cmp bl, '5'
				jz @@nDD5								
				jmp @@Mion
	@@nDD1:   	lea di, [si].NameP1		
				jmp @@Mion				
	@@nDD2:		lea di, [si].NameP2
				jmp @@Mion
	@@nDD3:		lea di, [si].Winner
				jmp @@Mion
	@@nDD4:		lea di, [si].DNTB
				jmp @@Mion
	@@nDD5:		lea di, [si].DNTE				
	@@Mion:		mov dx, type tbl
				mul dl
				add di, ax ;di = di + (typetbl*firstlimit)  ===== Locating di into first string...
				mov si, di
				jmp Checkit
	@@ConCheck:	inc cx
				mov di, si
	Checkit:	add si, type tbl
				push 0h si di
				call IsEqual
				pop ax
				cmp al, 1h
				jz @@ConCheck
				mov [bp+10], cx ;LastLimit
				pop di si dx cx bx ax bp
				ret 6
FindLimits EndP

SwitchIt Proc
				push bp
				mov bp, sp
				push ax bx cx dx di si 
				mov ax, [bp+10] ;What to swich? ?!?!?! 
				mov di, [bp+8] ; offset str 1  = di
				mov si, [bp+6] ; offset str2 = si
				; replacing di and si, si is smaller, we put si into di and the opposet
				mov bx, [bp+4] ; offset str temp = keara
				cmp al, '1'
				jz @@Con
				cmp al, '2'
				jnz @@Check3
				sub si, 15h
				sub di, 15h
				jmp @@Con
	@@Check3:	cmp al, '3'
				jnz @@Check4
				sub si, 02ah
				sub di, 02ah
				jmp @@Con
	@@Check4:	cmp al, '4'
				jnz @@Check5
				sub si, 03fh
				sub di, 03fh
				jmp @@Con
	@@Check5:	sub si, 54h
				sub di, 54h
	@@Con:		mov cx, type tbl
				push si di
	DiToTemp:	mov al, [di] 
				mov [bx], al
				inc di
				inc bx
				loop DiToTemp
				mov cx, type tbl
			;	mov di, [bp+8] ; offset str 1  = di
			;	mov si, [bp+6] ; offset str2 = si
				pop di si
				push di si
	SiToDi	:	mov al, [si] 
				mov [di], al
				inc di
				inc si
				loop SiToDi
				mov cx, type tbl
			;	mov si, [bp+6] ; offset str2 = si
				pop si di				
				mov bx, [bp+4] ; offset str temp = keara
	TempToSi:	mov al, [bx] 
				mov [si], al
				inc si
				inc bx
				loop TempToSi
				pop si di dx cx bx ax bp
				ret 8
SwitchIt EndP

CompareTo Proc ;1 - Azov Shtoiot, Tashir KAHA;;;;;;;;;;;;;;;; 2 - Tahlif
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ;String 1 aba
				mov di, [bp+6] ;String 2 tomer
				mov cl, [si+1]
				mov ch, [di+1]
				mov al, [si]
				cmp al, 12h 
				jnz @@Shoov
				add si, 2h
				add di, 2h
	@@Shoov:	mov al, [si]
				cmp al, [di]
				jz @@Again
				jnc Ret1        ;if si>di
				jmp Ret2
	@@Again:	inc si
				inc di
				dec ch
				jz Ret1
				dec cl
				jz Ret2
				jmp @@Shoov	
	Ret1:		mov bx, 1h
				jmp @@Sof
	Ret2:		mov bx, 2h
	@@Sof:		mov [bp+8], bx
				pop di si dx cx bx ax bp
				ret 4
CompareTo EndP

MionKelet Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di				
				mov bx, [bp+4]
				mov si, [bp+6]		
				push offset msgKeletInst
				call WriteStr
	@@NextKelet:
				push offset keletMsg
				call WriteStr
	DntConn:	mov di, [bp+4]
				mov cx, [bp+6]
				mov ah, 7h
				int 21h
				cmp al, '1'
				jnc OkGood
				jmp DntConn
	OkGood:		cmp al, '6'
				jc NextChac
				jmp DntConn
	NextChac:	cmp al, [di]
				jz DntConn
				inc di
				loop NextChac				
	SofCheckKelet:
				mov dl, al
				mov ah, 2h
				int 21h
				mov [bx], al
				mov dl, 0ah
				mov ah, 2h
				int 21h
				mov dl, 0dh
				int 21h
				inc bx			
				dec si
				jnz @@NextKelet
				pop di si dx cx bx ax bp
				ret 4
MionKelet EndP

IpusNewGame	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ;offset tls
				mov cx, [bp+6] ;TLS_LEN				
				mov dl, 2h		;2 Rows
				mov bl, 10h		;First xLoc
				mov bh, 1h		;First yLoc
				mov cx, 8h					
	Row1A:		mov [si].xLoc, bl
				mov [si].yLoc, bh
				mov [si].isEaten, 0h
				add si, type tls				
				add bl, 7h
				loop Row1A
				mov cx, 8h
				add bh, 3h
				mov bl, 10h
				dec dl
				jnz Row1A
				mov cx, 8h
				mov dl, 2h
				mov bh, 16h
				mov bl, 10h
	Row2A:		mov [si].xLoc, bl
				mov [si].yLoc, bh
				mov [si].isEaten, 0h
				add si, type tls				
				add bl, 7h
				loop Row2A
				mov cx, 8h
				sub bh, 3h
				mov bl, 10h
				dec dl
				jnz Row2A
				push 00001111b
				pop xTurn[0]
				pop di si dx cx bx ax bp
				ret 4
IpusNewGame EndP

IsEqual		Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]
				mov di, [bp+6]
				mov cx, 14h
	@@NextChac: mov al, [si]
				mov bl, [di]
				inc di
				inc si
				cmp al, bl				
				jnz @@NoEq
				loop @@NextChac
				mov ax, 1h ; It Is Equal ! 
				jmp @@SofEq
	@@NoEq:		mov ax, 0h
	@@SofEq:	mov [bp+8], ax ; ax = 0 No Equal, ax = 1 Equal ! 
				pop di si dx cx bx ax bp
				ret 4
IsEqual		EndP

StartNewGameAgainstPlayer Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov dx, 000dh ;Start Location of First Square					
				push TLS_LEN offset tls
				call IpusNewGame
				call GetTnD
				push ax cx dx
				mov si, [bp+4] ;offset tbl
				mov cx, [bp+6] ;countStr
				mov al, [bp+8] ;type tbl
				mul cl
				add si, ax
				lea dx, [si].DNTB
				push 20d offset tempDNT dx
				call DNTPusher
				pop dx cx ax	
				
				mov dx, 0h
				mov bh, 0h
				mov ah, 2h
				int 10h
				push dx ax				
				push offset p1Str
				call WriteStr
				lea dx, [si].NameP1
				mov ah , 0ah
				int 21h
				push offset p2Str
				call WriteStr
				lea dx, [si].NameP2
				mov ah , 0ah
				int 21h
				pop ax dx
				mov dx, 000dh
				call ClrScr
				push SQ_ROWS SQ_COLS
				call DrawSquareFunc	
				push offset Tls TLS_LEN
				call DrawSoldiers
				push offset xTurn offset colorArray
				call PlayChess
				
				call GetTnD
				push ax cx dx 
				mov si, [bp+4] ;offset tbl
				mov cx, [bp+6] ;countStr
				mov al, [bp+8] ;type tbl
				mul cl
				add si, ax
				lea dx, [si].DNTE
				push 20d offset tempDNT dx
				call DNTPusher
				pop dx cx ax
				
				mov cx, [bp+6] ;countStr
				inc cx
				push cx
				pop countStr	
				
				push ax cx								
				mov ax, [bp+10] ;TBL_LEN
				mul cx
				pop cx
				push 0 ; stam
				push 0 offset fNameStr
				call OpenNewFile
				pop fPStr
				push fPStr ax offset TBL
				call WriteToFile
				pop ax				
	SofGameVsP:
				pop di si dx cx bx ax bp
				ret 8
StartNewGameAgainstPlayer Endp

LoadFirstFiles	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				push 0 ; stam
				push 2h offset fNameStr
				call OpenExistFile
				pop fPStr
				mov ax, fPStr
				cmp ax, 245h
				push dx
				jnz @UnCreate						
				mov dl, 1
	@UnCreate:	cmp dl, 1
				pop dx
				jz SofFilesLoad
				push 0		; stam
				push offset buffStr TBL_LEN fPStr
				call ReadFile2
				pop countStr				
				push countStr offset buffstr offset tbl
				call LoadTbl
	SofFilesLoad:
				pop di si dx cx bx ax bp
				ret 
LoadFirstFiles	EndP

PlayChessAC	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov ah, 0h					;indexer
				mov cl, 0h 					;SpaceCounter
				mov ch, 0h 					;xTurn indexer	

				
				
	@@Klita:	mov di, [bp+4]	;offset colorArray 	;Colour Manager	Idexer - 0 = white, 1 = Gray, 2 || 3 = Red
				mov bx, [bp+6]	;offset xTurn		;xTurn Manager 0 - Player, 1 = Computer				
				mov al, ah
				mov ah, 0h
				add di, ax					;Indexer Color	
				mov ah, al	
				push cx
				cmp ch, 1h
				jnz @@DontIncIt
				inc ch
	@@DontIncIt:	
				add bl, ch					;Indexer xTurn					
				pop cx
				cmp [bx], 00001111b			;if player turn
				jz @@PlayerTurn
				jmp @@ComputerTurn
	@@PlayerTurn:	
				push ax						;1111
				mov ah, 7h
				int 21h
				cmp al, 0h				
				jz @@DoubleKey
				cmp al, 20h
				jnz @@Con
				pop ax						;1111
				jmp @@SpaceClicked				
	@@Con:		cmp al, 'e'
				pop ax						;1111				
				jz @@JmpSofi	
				push 0h
				call Cheats
				pop ax		;1 = N pressed, 0 = Done
				cmp al, 0h
				jz @@JmpSofi
				jmp @@Klita
	@@JmpSofi:	jmp @@Sof
	@@DoubleKey:
				pop ax						;1111
				push ax
				mov ah, 7h
				int 21h				
				push [di]					;White or Blue
				call PaintSq
				cmp al, 04dh
				jz @@RightMove
				cmp al, 04bh
				jz @@LeftMove
				cmp al, 48h
				jz @@UpMove
				cmp al, 50h
				jz @@DownMove		
				pop ax
				jmp @@Klita
	@@RightMove:
				add dl, 7h
				cmp dl, 045h
				jz @@RightByWall
				jmp @@PaintSqu
	@@RightByWall:
				mov dl, 0dh
				jmp @@PaintSqu
	@@LeftMove:	sub dl, 7h
				cmp dl, 6
				jz @@LeftByWall
				jmp @@PaintSqu
	@@LeftByWall:	
				mov dl, 03eh
				jmp @@PaintSqu
	@@UpMove:	sub dh, 3h
				cmp dh, -3h
				jz @@UpByWall
				jmp @@PaintSqu
	@@UpByWall:	mov dh, 15h
				jmp @@PaintSqu
	@@DownMove:	add dh, 3h
				cmp dh, 18h
				jz @@DownByWall
				jmp @@PaintSqu
	@@DownByWall:
				mov dh, 0h
				jmp @@PaintSqu	
	@@PaintSqu:	pop ax
				push ax
				cmp cl, 1h
				jz @@RedPointer
				mov ax, 09h
				jmp @@PaintIt
	@@RedPointer:
				mov ax, 04h
	@@PaintIt:	push ax			;Pointer-Saman
				call PaintSq
				pop ax
				xor ah, 1h
				jmp @@Klita				
	@@SpaceClicked:
				cmp cl, 1h
				jz @@SecSpace
				push 0h		;For Si Thats Come later... (stam)
				push [bx]
				call FirstSpace
				pop si
				;xor ah, 1h
				cmp si, 0h
				jnz @@Contin					;if ToolSelected
				jmp @@Klita		
	@@Contin:	mov cl, 1h					;FirstSpace Were Clicked
				jmp @@Klita
	@@SecSpace:	push ax
				push 0h ;StamYano
				push [bx] si dx				
				call SecondSpace
				pop ax
				push ax
				push 0h			;For ax.. (stam)				
				push bx			;xTrun Yano..
				call CheckMate
				pop ax			;1 = Mate! 0 = Nothing, Continue...
				cmp al, 1h
				pop ax
				jnz @@NoMate
				pop ax
				jmp @@Sof
	@@NoMate:	push offset Tls TLS_LEN
				call DrawSoldiers				
				cmp al, 0h					;if(al==1){Ho zaz}
				pop ax
				jnz @@ChangeTurn
				mov cl, 0h
				jmp @@Klita
	@@ComputerTurn:
				push offset rnk offset tls
				call PawnStyleCMove
				
				push 0h offset rnk
				call GetHigherMove
				pop di
				mov si, [di].offseTool
				mov [si].xLoc, [di].CmpMovex
				mov [si].yLoc, [di].CmpMovey
				
	@@ChangeTurn:	
				xor ch, 1h
				mov cl, 0h					;Ipus Space Counter
				jmp @@Klita
	@@Sof:		pop di si dx cx bx ax bp
				ret
PlayChessAC	EndP

IsPawnGuard	Proc
				push bp
				mov bp, sp
				push ax bx si
				mov si, [bp+4] 	;tool adress to check if protecting				
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov bl, [si].toolColor	
				cmp bl, 0fh	;if tool is white(down)
				jz @@FromDown
				jmp @@FromUp
				
	@@FromDown:	add al, 7h
				add ah, 3h				
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si				
				cmp si, 0h
				jz @@LeftMFD
				cmp [si].toolColor, bl				
				jnz @@LeftMFD
				cmp [si].typeTool, 'P'
				jnz @@LeftMFD
				jmp @@ToolProtected
				
	@@LeftMFD:	mov si, [bp+4] ;offset adress to check if protecting
				mov al, [si].xLoc
				mov ah, [si].yLoc
				sub al, 7h
				add ah, 3h
	
				push 0h TLS_LEN ax offset tls
				call ToolExist
				pop si
				cmp si, 0h
				jz @@NoPP	
				cmp [si].toolColor, bl
				jnz @@NoPP
				cmp [si].typeTool, 'P'
				jnz @@NoPP
				jmp @@ToolProtected
	
	@@NoPP:		jmp @@NoPProtect	
	
	@@FromUp:	add al, 7h
				sub ah, 3h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si				
				cmp si, 0h
				jz @@LeftMFU
				cmp [si].toolColor, bl				
				jnz @@LeftMFU
				cmp [si].typeTool, 'P'
				jnz @@LeftMFU
				jmp @@ToolProtected
				
	@@LeftMFU:	mov si, [bp+4] ;offset adress to check if protecting
				mov al, [si].xLoc
				mov ah, [si].yLoc
				sub al, 7h
				sub ah, 3h	
				push 0h TLS_LEN ax offset tls
				call ToolExist
				pop si
				cmp si, 0h
				jz @@NoPProtect
				cmp [si].toolColor, bl
				jnz @@NoPProtect
				cmp [si].toolColor, 'P'
				jnz @@NoPProtect
				jmp @@ToolProtected
	@@NoPProtect:
				mov [bp+6], 0h
				jmp @@Sof
	@@ToolProtected:
				mov [bp+6], si
	@@Sof:		pop si bx ax bp
				ret 2
IsPawnGuard	Endp

GetHigherMove Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di				
				mov di, [bp+4] 			;offset rnk
				mov al, [di].moveRank	;ax - max
				mov si, di
	@@NextRnk:	add di, type rnk
				cmp [di].offseTool, 0h
				jz @@Sof
				mov bl, [di].moveRank		
				cmp al, bl
				jc @@Switch
				jmp @@NextRnk
				
	@@Switch:	mov al, bl
				mov si, di
				jmp @@NextRnk
	@@Sof:		mov [bp+6], si
				pop di si dx cx bx ax bp
				ret 2
GetHigherMove Endp

PawnStyleCMove Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]	;offset tls
				mov di, [bp+6]	;offset rnk				
				
	@@NextPawn:	mov cx, 1h
				mov [di].offseTool, si
				cmp [di].offseTool.yLoc, 4h
				jnz @@ConTin
				inc cx						;2 moves available
				
	@@ConTin:	mov al, [di].offseTool.xLoc
				mov ah, [di].offseTool.yLoc
				
				;Check Before
				push 0h si
				call IsToolProtected
				pop bx
				add [di].moveRank, bl
				
	@@SecStep:	add ah, 3h
				mov [di].cmpMovex, al
				mov [di].cmpMovey, ah
				
				;Check After
				push 0h offset tls offset rnk
				call NumOfOptionalMoves
				pop bx
				add [di].moveRank, bl
				
				add di, type rnk
				loop @@SecStep
				
				;Eating Check
				mov cx, 2h
	@@SecEat:	mov al, [di].offseTool.xLoc
				mov ah, [di].offseTool.yLoc
				add ah, 3h
				cmp cx, 1h
				jz @@Subit
				add al, 7h
				jmp @@SkipSub
	@@Subit:	sub al, 7h
	@@SkipSub:	mov [di].cmpMovex, al
				mov [di].cmpMovey, ah
				;Check After
				add di, type rnk
				loop @@SecEat
				
				add si, type tls
				cmp [si].typeTool, 'P'
				jnz @@Sof
				jmp @@NextPawn
				
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
PawnStyleCMove Endp

ComputerTurn Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ; offset tls
				mov cx, [bp+6] ;TLS_LEN
				mov al, countCompGames
				cmp al, 0h
				jnz UpToFirst
				;call FirstTurn
	UpToFirst:		
				pop di si dx cx bx ax bp
				ret
ComputerTurn Endp

StartNewGameAgainsComp Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov dx, 000dh ;Start Location of First Square
				mov cl, 0h	  ;Count Pressed Spaces
				mov di, 1h		;Color Menager For KELET Func
				push TLS_LEN offset tls
				call IpusNewGame
				call GetTnD
				push ax cx dx
				mov si, [bp+4] ;offset tbl
				mov cx, [bp+6] ;countStr
				mov al, [bp+8] ;type tbl
				mul cl
				add si, ax
				lea dx, [si].DNTB
				push 20d offset tempDNT dx
				call DNTPusher
				pop dx cx ax	
				
				call ClrScr
				push 0h
				call SelectPlayerColorMenu
				pop ax
				cmp al, 0h
				jnz DontChangeXTurn
	
				call TurnXTurn				;xTurn[0] --> xTurn[1], xTurn[1] --> xTurn[0]
				
	DontChangeXTurn:
				mov dx, 0h
				mov bh, 0h
				mov ah, 2h
				int 10h
				push dx ax				
				push offset p1Str
				call WriteStr
				lea dx, [si].NameP1
				mov ah , 0ah
				int 21h				
				lea bx, [si].NameP2
				push offset cmpName
				pop di
	CmpNameMover:
				mov al, [di]
				mov [bx], al
				inc di
				cmp al, '$'
				jnz CmpNameMover
				pop ax dx
				mov dx, 000dh
				call ClrScr
				push SQ_ROWS SQ_COLS
				call DrawSquareFunc	
				push offset Tls TLS_LEN
				call DrawSoldiers
				
				push offset xTurn offset colorArray
				call PlayChessAC
				
				pop di si dx cx bx ax bp
				ret 6
StartNewGameAgainsComp Endp

GetToolPoint Proc
				push bp
				mov bp, sp
				push ax bx
				mov al, [bp+4] ; Tool - 'P', 'R' , etc.
				mov bx, 10h		;If King
				cmp al, 'P'				
				jnz @@ConToR
				mov bx, 1h
				jmp @@Sof
	@@ConToR:	cmp al, 'R'
				jnz @@ConToB
				mov bx, 5h
				jmp @@Sof
	@@ConToB:	cmp al, 'B'
				jnz @@ConToH
				mov bx, 3h
				jmp @@Sof
	@@ConToH:	cmp al, 'H'
				jnz @@ConToQ
				mov bx, 3h
				jmp @@Sof
	@@ConToQ:	cmp al, 'Q'
				jnz @@Sof
				mov bx, 9h	
	@@Sof:		mov [bp+6], bx ; Return point of tool
				pop bx ax bp
				ret 2				
GetToolPoint EndP
comment **
KingCount	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ; offset King				
				mov di, 8h ; 0 - R, 1 - UR, 2 - U, 3 - UL, 4 - L, 5 - LD, 6 - D, 7 - DR
				mov dx, 0h
	
	@@DiDec:	dec di
				mov al, [si].xLoc
				mov ah, [si].yLoc				
				jmp @@WhereToJmp
				
	@@R:		add al, 7h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				jmp @@Con
				
	@@UR:		add al, 7h
				sub ah, 3h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@U:		sub ah, 3h	 		;check Limits
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@UL:		sub al, 7h
				sub ah, 3h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@L:		sub al, 7h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				jmp @@Con
				
	@@DL:		sub al, 7h
				add ah, 3h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@D:		add ah, 3h 						
				cmp ah, 18h			;check Limits
				jnc @@DiDec		
				jmp @@Con
				
				
	@@DR:		add al, 7h
				add ah, 3h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@Con:		push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h	; if(IsToolExist() == true/false) = 
				jz @@IncBM
				jmp @@DiDec
	@@IncBM:	inc dl				;inc bishop move
	@@WhereToJmp:
				cmp di, 7h
				jnz @@ConTo6
				jmp @@DR
	@@ConTo6:	cmp di, 6h
				jnz @@ConTo1
				jmp @@D
	@@ConTo5:	cmp di, 5h
				jnz @@ConTo0
				jmp @@DL
	@@ConTo4:	cmp di, 4h
				jnz @@ConTo3
				jmp @@L
	@@ConTo3:	cmp di, 3h
				jnz @@ConTo2
				jmp @@UL
	@@ConTo2:	cmp di, 2h
				jnz @@ConTo1
				jmp @@U
	@@ConTo1:	cmp di, 1h
				jnz @@ConTo0
				jmp @@UR
	@@ConTo0:	cmp di, 0h
				jnz @@Sof
				jmp @@R
				
	@@Sof:		mov [bp+6], dl
				pop di si dx cx bx ax bp
				ret 2	
KingCount	EndP

HorseCount	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ; offset Horse				
				mov di, 8h ; 0 - URR, 1 - URU, 2 - ULL, 3 - ULU, 4 - DLL, 5 - DLD, 6 - DRR, 7 - DRD
				mov dx, 0h
				jmp @@DiDec
	
				
	@@URR:		add al, 0eh
				sub ah, 3h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@URU:		add al, 7h
				sub ah, 6h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@ULL:		sub al, 0eh
				sub ah, 3h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@DiDec:	dec di
				mov al, [si].xLoc
				mov ah, [si].yLoc				
				jmp @@WhereToJmp			
				
	@@ULU:		sub al, 7h
				sub ah, 6h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
				
	@@DLL:		sub al, 0eh
				add ah, 3h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@DLD:		sub al, 7h
				add ah, 6h
				cmp al, 0dh 		;check Limits
				jc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@DRR:		add al, 0eh
				add ah, 3h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
				
	@@DRD:		add al, 7h
				add ah, 6h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@Con:		push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h	; if(IsToolExist() == true/false) = 
				jz @@IncBM
				jmp @@DiDec
	@@IncBM:	inc dl				;inc bishop move
	@@WhereToJmp:
				cmp di, 7h
				jnz @@ConTo6
				jmp @@DRD
	@@ConTo6:	cmp di, 6h
				jnz @@ConTo1
				jmp @@DRR
	@@ConTo5:	cmp di, 5h
				jnz @@ConTo0
				jmp @@DLD
	@@ConTo4:	cmp di, 4h
				jnz @@ConTo3
				jmp @@DLL
	@@ConTo3:	cmp di, 3h
				jnz @@ConTo2
				jmp @@ULU
	@@ConTo2:	cmp di, 2h
				jnz @@ConTo1
				jmp @@ULL
	@@ConTo1:	cmp di, 1h
				jnz @@ConTo0
				jmp @@URU
	@@ConTo0:	cmp di, 0h
				jnz @@Sof
				jmp @@URR
				
	@@Sof:		mov [bp+6], dl
				pop di si dx cx bx ax bp
				ret 2				
HorseCount	EndP

BishopCount	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ; offset Bishop				
				mov di, 4h ; 0 - RU, 1 - LU, 2 - RD, 3 - LD
				mov dx, 0h
	
	@@DiDec:	dec di
				mov al, [si].xLoc
				mov ah, [si].yLoc
				mov cx, 7h
				jmp @@WhereToJmp
				
	@@RU:		add al, 7h
				sub ah, 3h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
	
	@@LU:		sub al, 7h
				sub ah, 3h
				cmp al, 0dh		;check Limits
				jc @@DiDec
				cmp ah, 1h
				jc @@DiDec
				jmp @@Con
	
	@@RD:		add al, 7h
				add ah, 3h
				cmp al, 03ch 
				jnc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@LD:		sub al, 7h
				add ah, 3h
				cmp al, 0dh
				jc @@DiDec
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@Con:		push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h	; if(IsToolExist() == true/false) = 
				jz @@IncBM
				jmp @@DiDec
	@@IncBM:	inc dl				;inc bishop move
	@@WhereToJmp:
				cmp di, 3h
				jnz @@ConTo2
				jmp @@RU
	@@ConTo2:	cmp di, 2h
				jnz @@ConTo1
				jmp @@LU
	@@ConTo1:	cmp di, 1h
				jnz @@ConTo0
				jmp @@RD
	@@ConTo0:	cmp di, 0h
				jnz @@Sof
				jmp @@LD
	@@Sof:		mov [bp+6], dl
				pop di si dx cx bx ax bp
				ret 2
BishopCount	EndP

RookCount	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] ; offset Rook				
				mov di, 4h ; 0 - HorizoR, 1 - HorizoL, 2 - VertiU, 3 - VertiD
				mov dx, 0h
	
	@@DiDec:	dec di
				mov al, [si].xLoc
				mov ah, [si].yLoc				
				jmp @@WhereToJmp
				
	@@HorizoR:	add al, 7h
				cmp al, 03ch 		;check Limits
				jnc @@DiDec
				jmp @@Con
	
	@@HorizoL:	sub al, 7h
				cmp al, 0dh		;check Limits
				jc @@DiDec
				jmp @@Con
	
	@@VertiU:	sub ah, 3h
				cmp ah, 1h 
				jc @@DiDec
				jmp @@Con
				
	@@VertiD:	add ah, 3h
				cmp ah, 18h
				jnc @@DiDec
				jmp @@Con
				
	@@Con:		push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h	; if(IsToolExist() == true/false) = 
				jz @@IncRM
				jmp @@DiDec
	@@IncRM:	inc dl				;inc rook move
	@@WhereToJmp:
				cmp di, 3h
				jnz @@ConTo2
				jmp @@VertiD
	@@ConTo2:	cmp di, 2h
				jnz @@ConTo1
				jmp @@VertiU
	@@ConTo1:	cmp di, 1h
				jnz @@ConTo0
				jmp @@HorizoL
	@@ConTo0:	cmp di, 0h
				jnz @@Sof
				jmp @@HorizoR
	@@Sof:		mov [bp+6], dl
				pop di si dx cx bx ax bp
				ret 2
RookCount	EndP

PawnCount	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4] 		;offset Tools
				mov bx, [bp+6] 		;WhosTurn
				mov cx, 0h
				mov al, [si].xLoc
				mov ah, [si].yLoc
				cmp bl, 0fh 	;if Color To Check = White
				jz @@FromDown
				jmp @@FromUp
	@@FromDown:	cmp ah, 13h
				jnz @@1MovAvlb
	@@FromUp:	cmp ah, 4h		
				jnz @@1MovAvlb
				inc cx
	@@1MovAvlb:	inc cx
				cmp bl, 0fh 	;if Color To Check = White
				jz @@MFromD
				jmp @@MFromUp
				
	@@MFromD:	sub ah, 3h				
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h 		;if Tool Exist ? 
				jnz @@SofPawn
	@@IncPMove:	inc dl
				loop @@MFromD
				jmp @@SofPawn
	
	@@MFromUp:	add ah, 3h
				push 0h TLS_LEN ax offset Tls
				call ToolExist
				pop si
				cmp si, 0h
				jnz @@SofPawn
	@@IncPMFU:	inc dl
				loop @@MFromUp
				jmp @@SofPawn
				
	@@SofPawn:	mov [bp+8], dl
				pop di si dx cx bx ax bp
				ret 4
PawnCount	EndP

NumOfOptionalMoves	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di				
				mov di, [bp+4] ;offset rnk
				
				mov si, [di].offseTool
				push si
				mov dl, [si].xLoc
				mov dh, [si].yLoc
				mov bl, [si].toolColor
				
				mov [si].xLoc, [di].CmpMovex
				mov [si].yLoc, [di].CmpMovey
				
				mov si, [bp+6] ;offset tls
	@@Again:	cmp [si].toolColor, bl
				jz @@CheckWhoCount
				add si, type tls
				jmp @@Again
				
				mov cx, 0h 		;Counter Moves
								
	@@CheckWhoCount:
				cmp [si].typeTool, 'P'
				jnz @@MaybeR
				mov ax, 0h
				mov al, [si].toolColor
				
				push 0h ax si
				call PawnCount
				pop ax
				add cx, ax
			
	@@MaybeR:	cmp [si].typeTool, 'R'
				jnz @@MaybeH
				mov ax, 0h
				mov al, [si].toolColor
				
				push 0h ax si
				call RookCount
				pop ax
				add cx, ax
				
	@@MaybeH:	cmp [si].typeTool, 'H'
				jnz @@MaybeB
				mov ax, 0h
				mov al, [si].toolColor
				
				push 0h ax si
				call HorseCount
				pop ax
				add cx, ax
				
	@@MaybeB:	cmp [si].typeTool, 'B'
				jnz @@MaybeQ
				mov ax, 0h
				mov al, [si].toolColor
				
				push 0h ax si
				call BishopCount
				pop ax
				add cx, ax
				
	@@MaybeQ:	cmp [si].typeTool, 'Q'
				jnz @@MaybeK
				mov ax, 0h
				mov al, [si].toolColor
				
				push 0h ax si
				call BishopCount
				pop ax
				add cx, ax
				
				push 0h ax si
				call RookBishop
				pop ax
				add cx, ax
				
	@@MaybeK:	cmp [si].typeTool, 'K'
				jnz @@Sof
				mov ax, 0h
				mov al, [si].toolColor
				
				push 0h ax si
				call KingCount
				pop ax
				add cx, ax
				
				add si, type tls
				jmp @@CheckWhoCount
	
	@@Sof:		mov [bp+8], cx	
				pop si
				mov [si].xLoc, dl
				mov [si].yLoc, dh
				pop di si dx cx bx ax bp
				ret 4
NumOfOptionalMoves	EndP




	Start:		mov ax, dseg
				mov ds, ax		
				mov es, ax	
				
				call ClrScr	
				push offset buffer1	offset openScr 
				call Kitov
				mov ah, 7h 
				int 21h
				call ClrScr
				
	MMenu:		call LoadFirstFiles
				call ClrScr
				push offset buffer4 offset mainMen
				call Kitov				
				push 0h
				call MenuF
				pop si
				
				cmp si, 4h 				;Quit
				jnz @Con3
				jmp @Sof
				
	@Con3:		cmp si, 3h				;Table
				jnz @Con2
				call ClrScr
				push KELET_LEN offset keletArr
				call MionKelet
				push countStr offset tbl				
				call RecordTable	
				jmp MMenu
				
	@Con2:		cmp si, 2h				;Instruction
				jnz @Con1					
				call ClrScr
				push offset buffer3 offset instruction 
				call Kitov
				mov ah, 7h 
				int 21h					
				jmp MMenu
				
	@Con1:		cmp si, 1h				;Vs Player
				jnz @Con0
				call ClrScr
				push TBL_LEN type tbl countStr offset tbl
				call StartNewGameAgainstPlayer
				jmp MMenu
				
	@Con0:		push type tbl countStr offset tbl
				Call StartNewGameAgainsComp	;Vs Computer
				jmp MMenu	
				
	@Sof:		int 3h
cseg ends
end Start