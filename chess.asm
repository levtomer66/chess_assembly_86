locals
sizeS equ 100d
firstLoc equ 0dh

RankMove Struc
				RootAdrs	dw	0h					;Tool Source Offset(adress)
				DestAdrs	dw	0h					;Tool Destini Offset(adress)
				moveRank	dw 	0h					;The Rank For Moving				
RankMove EndS

TableS struc
				NameP1 db	18d,20d dup('$')
				NameP2 db 	18d,20d dup('$')
				Winner db 	18d,20d dup('$')  	 ; winner's name
				DNTB   db 	18d,20d dup('$') 	 ; date & time in the begining
				DNTE   db 	18d,20d dup('$') 	 ; date & time in the end  ; DD/MM/YYYY HH:MM:SS		
TableS Ends

dseg segment
				sqStr	db	201d,205d,205d,205d,205d,205d,187d 											
				SQ_COLS	=	($-sqStr)/type sqStr
						db	186d, 0h, 0h, 0h,  0h, 0h ,186d	
						db	200d,205d,205d,205d,205d,205d, 188d	
				SQ_ROWS	=	(($-sqStr)/type sqStr)/SQ_COLS
				
				brd 	db	'r','h','b','q','k','b','h','r'
				BRD_COLS =	($-brd)/type brd
						db	'p','p','p','p','p','p','p','p'
						db	'0','0','0','0','0','0','0','0'
						db	'0','0','0','0','0','0','0','0'
						db	'0','0','0','0','0','0','0','0'
						db	'0','0','0','0','0','0','0','0'
						db	'P','P','P','P','P','P','P','P'
						db	'R','H','B','Q','K','B','H','R'
				BRD_ROWS = (($-brd)/type brd)/BRD_COLS
				BRD_LEN	 =	BRD_ROWS*BRD_COLS
				
				xTurn		dw	'A', 'a'					;xTurn[0] = who's turn
				
				castlArr	db	0h,0h			;Big/Small Castling Available (0)
				
				promotionMsg	db	"Pick a Tool:",0ah,0dh,"Q - Queen",0ah,0dh,"R - Rook",0ah,0dh,"H - Horse",0ah,0dh, "B - Bishop$"
				promotionDel	db	"            ",0ah,0dh,"         ",0ah,0dh,"        ",0ah,0dh,"         ",0ah,0dh, "          $"
				
				SelPlrorCmp		db	"P - Player",0ah,0dh,"C - Cmp $"
				cmpOrPlr		dw	0000h			;0h = Player vs Player (else) 'a' = Computer First, 'A' = Player First
				selTurnByPlr	db	"W - White ",0ah,0dh,"B - Blue$"
				
				rnk	RankMove	SizeS*2	dup(< , , >)
				
				P_OPPMOV	db	4h		;1-up, 2-double up,3-eat left, 4- eat right.
				R_OPPMOV	db	01ch	;7 up + 7 down + 7 left + 7 right = 7*4 = 01ch
				B_OPPMOV = R_OPPMOV		;7 upright + 7 upleft + 7 downright + 7 downleft = 7*4 = 01ch = R_OPPMOV
				H_OPPMOV	db	8h		
				K_OPPMOV = H_OPPMOV
				Q_OPPMOV = R_OPPMOV				
				
				openScr	db	'ChessTxt.txt', 0
				buffer1	db	1000h dup('$')	;OpenScr
				
				buffStr		db 6000h dup('$')	
				fNameStr	db 'Tbl.txt',0		
				fPStr		dw ?	
					
				buffR		db 600h dup('$')	
				fNameR		db 'Results.txt',0		
				fpr			dw ?
				
				p1Winner db	'P1Win.txt', 0
				p2Winner db	'P2Win.txt', 0		
				buffer2	db	1000h dup('$')	;Winner Messeg
	
				
				
				
				menu	db	" Player Vs Computer "
				MEN_LEN	=	($-menu)
						db	" Player1 Vs Player2 "
						db	"    Instruction     "	
						db	"    Record Table    "
						db	"        Quit        "
						
				clrMenu	db	"		White		 "
				MEN_CLR_LEN= ($-clrMenu)
						db	"		Blue		 "
						
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
				
				
				
				keletArr db 5h dup(?)
				KELET_LEN = ($-keletArr)/type keletArr
				startAreas db 5h dup(?)
				endAreas db	5h dup (?)
	
	
	
	
dseg ends

sseg segment stack
				dw 100h dup(?)
sseg ends

cseg segment
assume cs:cseg, ds:dseg, ss:sseg

ClrScr	Proc
				push ax cx dx
				mov cx, 30h
				mov dl, 0ah
				mov ah, 2h
	@@Clr:		int 21h
				loop @@Clr
				push 0h
				call GoToXY
				pop dx cx ax
				ret
ClrScr	Endp

WriteStr Proc
				push bp
				mov bp, sp
				push ax dx 
				mov dx, [bp+4] ; offset str
				mov ah, 9h
				int 21h
				pop dx ax bp
				ret 2
WriteStr Endp

GoToXY	proc
				push bp
				mov bp, sp
				push ax bx dx 
				mov bh, 0h
				mov dx, [bp+4] ;Location				
				mov ah, 2h
				int 10h
				pop dx bx ax bp
				ret 2
GoToXY	endp

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

DrawBoard proc				
				push bx cx dx si di									
				mov di, 1
				mov si, 8h											;Board Rows Length
				mov dh, 0
	DrawC:		mov dl, firstLoc									;First Square Location													
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
				add dl, SQ_COLS	
				loop DrawR
				dec di					
				add dh, SQ_ROWS				
				dec si		
				jnz DrawC
				pop di si dx cx bx
				ret
DrawBoard EndP

DrawTools Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				xor bx, bx
				mov si, [bp+4]	;offset brd
				mov cx, BRD_COLS
				mov di, BRD_ROWS
				mov dx, 0h
				mov dx, firstLoc		;dl = first location of pointer
				add dl, 3h
				inc dh
	ColsTimes:  mov ah, 2h
				int 10h				
				mov al, [si]
				mov bl, 00001111b					
				cmp al, '0'		
				jnz @@DntDelTl
				push cx	ax		
				mov al, ' '
				mov cx, 1h
				;mov bh, 0h
				mov ah, 9h				
				int 10h
				pop ax cx
				jmp @@NextTool
	@@DntDelTl:	push dx			;~~~~1
				push 0h 60h ax 07bh
				call IfxBetweenAB
				pop dx
				cmp dx, 1h
				pop dx			;~~~~1
				jnz @@Con
				sub al, 20h			;make capital letter
				mov bl, 00001011b				
	@@Con:		push cx				
				mov cx, 1h
				;mov bh, 0h
				mov ah, 9h				
				int 10h
				pop cx
	@@NextTool:	add si, type brd
				add dl, 7h
				loop ColsTimes
				mov dx, firstLoc
				add dl, 3h
				inc dh
				dec di
				jz @@Sof
				mov cx, 8h
				push cx				
				sub cx, di
	SetRow:		add dh, 3h
				loop SetRow
				pop cx
				jmp ColsTimes
	@@Sof:		pop di si dx cx bx ax bp
				ret 2
DrawTools EndP

IfxBetweenAB Proc		;if(A<x<B){return 1} else 0
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov bl, [bp+4]	;the B
				mov al, [bp+6]	;the x
				mov bh, [bp+8]	;the A
				cmp al, bl
				jnc @@Sof
				cmp al, bh
				jc @@Sof
				mov [bp+10], 1h				
	@@Sof:		pop di si dx cx bx ax bp
				ret 6
IfxBetweenAB EndP

PlayChess Proc
				push bp  
				mov bp, sp
				push ax bx cx dx si di		
				mov ax, [bp+4]				;cmpOrPlr
				xor bl, bl					;Colour Index - 0 - White; 1 - Gray				
				xor cl, cl 					;SpaceCounter			
				mov dx, firstLoc
				
				cmp ax, xTurn[0]
				jnz Klita
				jmp @@CmpTurn
				
	Klita:		mov ah, 7h
				int 21h
				cmp al, 0h				
				jz DoubleKey
				cmp al, 20h
				jnz @@Con
				jmp SpaceClckd				
	@@Con:		cmp al, 'e'
				jz JmpSofi	
				;push 0h
				;call Cheats
				;pop ax		;1 = N pressed, 0 = Done
				;cmp al, 0h
				;jz JmpSofi
				;jmp Klita
	JmpSofi:	jmp @@Sof
	DoubleKey:	mov ah, 7h
				int 21h	
				push ax 				;~~~~PUSH~~~~
				mov ax, 00001111b		;White
				cmp bl, 1h
				jnz @@PntWhite				
				mov ax, 00001000b		;Gray				
	@@PntWhite:	push ax
				call PaintSq
				xor bl, 1h
				pop ax					;~~~~POP~~~~
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
				jz RByWall
				jmp PaintSqu
	RByWall:	mov dl, 0dh
				jmp PaintSqu
	LeftMove:	sub dl, 7h
				cmp dl, 6h
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
	
	PaintSqu:	mov ax, 00001001b			;Blue
				cmp cl, 1h
				jnz @@PntBlue
				mov ax, 00000100b			;paint red if space was clicked
	@@PntBlue:	push ax			
				call PaintSq
				jmp Klita
				
	SpaceClckd:	cmp cl, 1h
				jz SecSpace
				push 0h		;For Si Thats Come later... (stam)
				push xTurn[0]
				call FirstSpace
				pop si
				cmp si, 0h
				jnz Contin					;if ToolSelected
				jmp Klita		
	Contin:		xor cl, 1h					;FirstSpace Were Clicked
				jmp Klita
	
	SecSpace:	push xTurn[0] si dx				
				call SecondSpace
				xor cl, 1h
				
				push 0h xTurn[0]
				call CheckMate
				pop ax				
				cmp ax, 1h
				jz @@Winner
				
				mov ax, [bp+4]		;cmpOrPlr	
				cmp ax, xTurn[0]
				jz @@CmpTurn
				jmp Klita
	
	@@CmpTurn:	push xTurn[0] offset rnk offset brd
				call ComputerTurn
				comment *
				push ax			;save ax
				push 0h xTurn[2]
				call CheckMate
				pop ax
				cmp ax, 1h
				jz @@Winner
				pop ax			;load ax
				*
				call SwitchTurns
				jmp Klita
				;push 0h			;For ax.. (stam)				
				;push bx			;xTrun Yano..
				;call CheckMate
				;pop ax			;1 = Mate! 0 = Nothing, Continue...
				;cmp al, 1h
				;pop ax
				;jnz @@NoMate
				;pop ax
				;jmp @@Sof
	@@Winner:	
				
	@@Sof:		pop di si dx cx bx ax bp
				ret 2
PlayChess EndP

ToolExist Proc	;si(y-1) + x
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov dx, [bp+4]	;location
				mov si, [bp+6]	;offset brd
				mov cx, BRD_COLS
				mov di, BRD_ROWS
				mov bx, firstLoc
				add bl, 3h
				inc bh
				
	@@ColTimes:	cmp bx, dx
				jz DoneEAsearch
				add si, type brd				
				add bl, 7h
				loop @@ColTimes				
				dec di
				jz @@NoExist
				mov bx, firstLoc
				add bl, 3h
				inc bh
				mov cx, 8h
				push cx				
				sub cx, di
	@@SetRow:	add bh, 3h
				loop @@SetRow
				pop cx
				jmp @@ColTimes
				
	DoneEAsearch:
				mov bl, [si]
				cmp bl, '0'
				jz @@NoExist
				mov [bp+8], si
			
	@@NoExist:	pop di si dx cx bx ax bp
				ret 4
				
ToolExist Endp

FirstSpace	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov bl, [bp+4] ; xTurn[0]
				mov cl, 0h		;Selecter , 0 = didnt select yet, 1 = select success		
				;xor di, 1h			
				push dx
				add dl, 3h
				inc dh
				push 0h offset brd dx
				call ToolExist
				pop si
				pop dx
				cmp si, 0h		
				jnz TrySelect	;if Tool is exist					
				jmp SofFirstSpace
	TrySelect:	push 0h 
				push 0040h
				push [si]
				push 005bh
				call IfxBetweenAB
				pop ax
				cmp ax, 1h
				jz	@@CheckCapital
				cmp bl, 'a'
				jz SelectSuccess
				jmp @@Unsucc
	@@CheckCapital:
				cmp bl, 'A'
				jz SelectSuccess
	@@Unsucc:	mov si, 0h
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

SecondSpace Proc ; Get: Tool That had been Pressed, Location To Move, xTurn Idexer
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov dx, [bp+4]			;Location
				add dl, 3h
				inc dh
				mov si, [bp+6]			;tool had been pressed				
				mov ax, [si]
				push 0h
				push 0060h
				push ax 
				push 0007bh
				call IfxBetweenAB
				pop cx
				cmp cx, 1h
				jnz @@ConToP
				sub ax, 20h
	@@ConToP:	cmp al, 'P'
				jnz @@ConToR
				push dx si xTurn[0]
				call PawnRules
				jmp @@Con
	@@ConToR:	cmp al, 'R'
				jnz @@ConToB
				push dx si
				call RookRules			
				jmp @@Con
	@@ConToB:	cmp al, 'B'
				jnz @@ConToQ
				push dx si 
				call BishopRules			
				jmp @@Con
	@@ConToQ:	cmp al, 'Q'
				jnz @@ConToH	
				push dx si
				call RookRules		
				push dx si
				call BishopRules			
				jmp @@Con
	@@ConToH:	cmp al, 'H'
				jnz @@ConToK
				push dx si
				call HorseRules			
				jmp @@Con				
	@@ConToK:	cmp al, 'K'
				jnz @@ToSof
				push dx si
				call KingRules				
	@@ToSof:	jmp @@Con
						
	
	@@Con:		push offset brd	
				call DrawTools
				dec dh
				sub dl, 3h
				mov ah, 10h
				int 21h
				push 09h
				call PaintSq
				pop di si dx cx bx ax bp
				ret 6
SecondSpace EndP

GetLocByEA	Proc
				push bp 
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]	;The adress of tool by offset brd
				mov di, [bp+6]	;The adress of board
				mov [bp+8], 1h	;Error ! - can be crosss ! 
				mov cx, BRD_COLS	
				mov bx, 0h
				mov dx, firstLoc
				add dl, 3h
				inc dh
	@@CmpNow:	cmp di, si
				jz @@LocFound
				add di, type brd
				add dl, 7h
				loop @@CmpNow
				inc bx
				cmp bx, BRD_ROWS
				jz @@Sof
				push bx
				mov dx, firstLoc
				mov cx, BRD_COLS
				add dl, 3h
				inc dh
	@@BxTimes:	add dh, 3h
				dec bx
				jnz @@BxTimes
				pop bx
				jmp @@CmpNow				
	@@LocFound:	mov [bp+8], dx
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
GetLocByEA	EndP

QueenLoc	Proc
				push bp
				mov bp, sp
				push ax bx si
				xor bx, bx
				mov si, [bp+4]	;offset brd
				mov bx, [bp+6]	;xTurn[0]	
	FQuen:		mov al, [si]
				cmp bl, 'A'
				jnz FSQuen
				cmp al, 'Q'
				jz @@FindDest
				jmp LoopQ
	FSQuen:		cmp al, 'q'
				jz @@FindDest				
	LoopQ:		add si, type brd
				jmp FQuen
	@@FindDest:	mov [bp+8], si
	@@Sof:		pop si bx ax bp
				ret 4
QueenLoc	EndP

KingLoc	Proc
				push bp
				mov bp, sp
				push ax bx si
				xor bx, bx
				mov si, [bp+4]	;offset brd
				mov bx, [bp+6]	;xTurn[0]	
	Fking:		mov al, [si]
				cmp bl, 'A'
				jnz FSking
				cmp al, 'K'
				jz FindDest
				jmp LoopK
	FSking:		cmp al, 'k'
				jz FindDest				
	LoopK:		add si, type brd
				jmp FKing
	FindDest:	mov [bp+8], si
	@@Sof:		pop si bx ax bp
				ret 4
KingLoc	EndP
comment *
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
*
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

CheckIfAeatB	Proc
				push bp
				mov bp, sp
				push ax bx si di
				mov si, [bp+6]		;Offset tool Gonna eat	(A)				--if this			
				mov di, [bp+4]		;Offset tool gonna be eaten (B)			--eat this
				mov al, [di]
				cmp al, '0'
				jnz @@EatCheck
				jmp @@Sof
	@@EatCheck:	push 0h
				push 0040h
				push [si]
				push 005bh
				call IfxBetweenAB
				pop ax
				push 0h
				push 0040h
				push [di]
				push 005bh
				call IfxBetweenAB
				pop bx
				cmp ax, bx
				jz @@Sof
				mov [bp+8], 1h
	@@Sof:		pop di si bx ax bp
				ret 4
CheckIfAeatB	Endp

SwitchTurns		Proc				
				push xTurn[0]
				push xTurn[2]
				pop xTurn[0]
				pop xTurn[2]
				ret
SwitchTurns		EndP

CheckBorderCross	Proc
				push bp
				mov bp, sp
				push dx
				mov dx, [bp+4]		;Location to check
				mov [bp+6], 1h		;There is a CROSS!
				cmp dl, 42h
				jnc @@Sof
				cmp dl, 10h
				jc	@@Sof
				cmp dh, 1h
				jc @@Sof
				cmp dh, 17h
				jnc @@Sof
				mov [bp+6], 0h
	@@Sof:		pop dx bp
				ret 2
CheckBorderCross	EndP

CheckBorderCrossByEA	Proc
				push bp
				mov bp, sp
				push dx
				mov si, [bp+4]		;Location to check
				push 0h offset brd si
				call GetLocByEA
				pop dx
				mov [bp+6], 1h		;There is a CROSS!
				cmp dl, 42h
				jnc @@Sof
				cmp dl, 10h
				jc	@@Sof
				cmp dh, 1h
				jc @@Sof
				cmp dh, 17h
				jnc @@Sof
				mov [bp+6], 0h
	@@Sof:		pop dx bp
				ret 2
CheckBorderCrossByEA	EndP

Promotion	Proc
				push bp
				mov bp, sp
				push ax bx dx si
				mov si, [bp+4]	;offset tool that moved
				mov bl, [bp+6]	;xTurn[0]
				mov dx, 0h
				mov ah, 2h
				int 10h				
				push offset promotionMsg
				call WriteStr
	@@Klot:		mov ah, 7h
				int 21h
				cmp al, 'q'
				jz @@DoProm				
				cmp al, 'r'
				jz @@DoProm
				cmp al, 'b'
				jz @@DoProm
				cmp al, 'h'
				jz @@DoProm
				jmp @@Klot
	@@DoProm:	cmp bl, 'a'
				jz @@SmllLet
				sub al, 20h
	@@SmllLet:	mov [si], al
				mov dx, 0h
				mov ah, 2h
				int 10h	
				push offset promotionDel
				call WriteStr
				pop si dx bx ax bp
				ret 4
Promotion	Endp

PawnRules Proc	;Get: Adress Of Tools Array, Location To Move
				push bp
				mov bp, sp
				push ax bx cx dx si di			
				mov bl, [bp+4]	;xTurn[0]
				mov si, [bp+6]	;Offset of tool will be moved (?)
				mov dx, [bp+8]	;Location To Move
				mov di, si				
				mov cx, 1h
				push 0h offset brd si
				call GetLocByEA
				pop ax
				cmp bl, 'A'
				jz @@FromDown
				jmp @@FromUp
				
	@@FromDown:	cmp ah, 13h
				jnz @@MovChk1
				inc cx			;two move available
	@@MovChk1:	sub ah, 3h
				sub si, BRD_COLS
				mov bl, [si]
				cmp bl, '0'		
				jnz @@FromDEat
				cmp dx, ax
				jnz @@DoLoop
				jmp @@DoMove
	@@DoLoop:	loop @@MovChk1
	
	@@FromDEat:	mov si, di
				push 0h offset brd si
				call GetLocByEA
				pop ax
				sub ah, 3h			;Try Eat From Left
				sub si, BRD_COLS
				jmp @@LeftEat
				
	@@FromUp:	cmp ah, 4h
				jnz @@MovChk2
				inc cx			;two move available
	@@MovChk2:	add ah, 3h
				add si, BRD_COLS
				mov bl, [si]
				cmp bl, '0'
				jnz @@FromUEat
				cmp dx, ax
				jz @@DoMove
				loop @@MovChk2
				
	@@FromUEat:	mov si, di
				push 0h offset brd si
				call GetLocByEA
				pop ax
				add ah, 3h			;Try Eat From Left
				add si, BRD_COLS			
				
	@@LeftEat:	sub al, 7h
				sub si, type brd
				cmp ax, dx
				jnz @@RightEat
				push 0h di si
				call CheckIfAeatB
				pop ax
				cmp al, 1h
				jz @@DoMove
				
	@@RightEat:	add al, 0eh
				add si, type brd*2
				cmp ax, dx
				jnz @@Sof
				push 0h di si
				call CheckIfAeatB
				pop ax
				cmp al, 1h
				jz @@DoMove
				jmp @@Sof
	
	@@DoMove:	cmp dh, 1h
				jnz @@Opp				
				push xTurn[0] si
				call Promotion
				mov bh, '0'
				mov [di], bh				
				jmp @@SwchTurn
	@@Opp:		cmp dh, 16h
				jnz @@OrigiMov
				push xTurn[0] si
				call Promotion
				mov bh, '0'
				mov [di], bh				
				jmp @@SwchTurn
	@@OrigiMov:	mov bl, [di]
				mov bh, '0'
				mov [di], bh
				mov [si], bl			
	@@SwchTurn:	push ax				;save ax
				push 0h xTurn[0]
				call CheckChess
				pop ax
				cmp al, 1h
				pop ax				;load ax
				jz @@UndoMove
				call SwitchTurns
				jmp @@Sof
	@@UndoMove:	mov [si], bh	;clean the new area
				mov [di], bl	;return the tool to its place
	@@Sof:		pop di si dx cx bx ax bp
				ret 6
PawnRules EndP 

RookRules Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di	
				mov si, [bp+4]	;offset of tool will be moved (?)
				mov dx, [bp+6]	;Location To Move
				mov cx, 4h		;Up/Down/Left/Right
				mov di, si
				mov bl, [si]
				push 0h offset brd si
				call GetLocByEA
				pop ax
				
	@@WhereJmp:	cmp cx, 4h
				jnz @@ConTo3
				jmp @@RookUM
	@@ConTo3:	cmp cx, 3h
				jnz @@ConTo2
				jmp @@RookDM
	@@ConTo2:	cmp cx, 2h
				jnz @@ConTo1
				jmp @@RookRM
	@@ConTo1:	jmp @@RookLM
	
	@@RookUM:	sub ah, 3h		
				sub si, BRD_COLS				
				jmp @@Con
				
	@@RookDM:	add ah, 3h		
				add si, BRD_COLS
				jmp @@Con
				
	@@RookRM:	add al, 7h		
				add si, type brd
				jmp @@Con
				
	@@RookLM:	sub al, 7h		
				sub si, type brd
				jmp @@Con
	
	@@Con:		mov bl, [si]
				cmp bl, '0'
				jnz @@Etry		;Eat TRY				
				cmp ax, dx
				jz @@DoMove
				push 0h ax
				call CheckBorderCross
				pop bx
				cmp bl, 1h
				jz @@LoopWhr
				jmp @@WhereJmp
	
	@@LoopWhr:	mov si, di
				push 0h offset brd si
				call GetLocByEA
				pop ax
				loop @@WhereJmp
				jmp @@Sof
	
	@@Etry:		cmp ax, dx
				jnz @@LoopWhr
				push 0h di si
				call CheckIfAeatB
				pop ax
				cmp al, 1h
				jnz @@LoopWhr
	
	@@DoMove:	mov bl, [di]	;bl -> the move tool
				mov bh, '0'		;bh -> empty area
				mov [di], bh	;clean the area
				mov [si], bl	;mov the tool
				push ax				;save ax
				push 0h xTurn[0]
				call CheckChess
				pop ax
				cmp al, 1h
				pop ax				;load ax
				jz @@UndoMove
				push 0h offset brd di
				call GetLocByEA
				pop ax
				cmp ax, 1610h
				jnz @@ScstlChk				
				mov castlArr[0], 1h			
	@@ScstlChk:	cmp ax, 1641h
				jnz @@SwtchTrn
				mov castlArr[1], 1h
	@@SwtchTrn:	call SwitchTurns
				jmp @@Sof
	@@UndoMove:	mov [si], bh	;clean the new area
				mov [di], bl	;return the tool to its place				
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
RookRules EndP	

BishopRules Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di	
				mov si, [bp+4]	;offset of tool will be moved (?)
				mov dx, [bp+6]	;Location To Move
				mov cx, 4h		;Up/Down/Left/Right
				mov di, si
				mov bl, [si]
				push 0h offset brd si
				call GetLocByEA
				pop ax
				
	@@WhereJmp:	cmp cx, 4h
				jnz @@ConTo3
				jmp @@BishURM
	@@ConTo3:	cmp cx, 3h
				jnz @@ConTo2
				jmp @@BishULM
	@@ConTo2:	cmp cx, 2h
				jnz @@ConTo1
				jmp @@BishDRM
	@@ConTo1:	jmp @@BishDLM
	
	@@BishURM:	sub ah, 3h
				add al, 7h
				sub si, BRD_COLS	
				add si, type brd
				jmp @@Con
				
	@@BishULM:	sub ah, 3h
				sub al, 7h
				sub si, BRD_COLS
				sub si, type brd
				jmp @@Con
				
	@@BishDRM:	add ah, 3h
				add al, 7h		
				add si, BRD_COLS
				add si, type brd
				jmp @@Con
				
	@@BishDLM:	add ah, 3h
				sub al, 7h		
				add si, BRD_COLS
				sub si, type brd
				jmp @@Con
				
	@@LoopWhr:	mov si, di
				push 0h offset brd si
				call GetLocByEA
				pop ax
				loop @@WhereJmp
				jmp @@Sof
	
	@@Con:		mov bl, [si]
				cmp bl, '0'
				jnz @@Etry		;Eat TRY				
				cmp ax, dx
				jz @@DoMove
				push 0h ax
				call CheckBorderCross
				pop bx
				cmp bl, 1h
				jz @@LoopWhr
				jmp @@WhereJmp
	
	@@Etry:		cmp ax, dx
				jnz @@LoopWhr
				push 0h di si
				call CheckIfAeatB
				pop ax
				cmp al, 1h
				jnz @@LoopWhr
	
	@@DoMove:	mov bl, [di]	;bl -> the move tool
				mov bh, '0'		;bh -> empty area
				mov [di], bh	;clean the area
				mov [si], bl	;mov the tool
				push ax				;save ax
				push 0h xTurn[0]
				call CheckChess
				pop ax
				cmp al, 1h
				pop ax				;load ax
				jz @@UndoMove
				call SwitchTurns
				jmp @@Sof
	@@UndoMove:	mov [si], bh	;clean the new area
				mov [di], bl	;return the tool to its place
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
BishopRules EndP	

HorseRules Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di	
				mov si, [bp+4]	;offset of tool will be moved (?)
				mov dx, [bp+6]	;Location To Move
				mov cx, 8h		;Up/Down/Left/Right
				mov di, si
				mov bl, [si]
				push 0h offset brd si
				call GetLocByEA
				pop ax					
				jmp @@WhereJmp
	
	@@HorsUURM:	sub ah, 6h
				add al, 7h
				sub si, BRD_COLS*2	
				add si, type brd
				jmp @@Con
				
	@@HorsURRM:	sub ah, 3h
				add al, 0eh
				sub si, BRD_COLS
				add si, type brd*2
				jmp @@Con
				
	@@HorsDRRM:	add ah, 3h
				add al, 0eh		
				add si, BRD_COLS
				add si, type brd*2
				jmp @@Con
				
	@@HorsDDRM:	add ah, 6h
				add al, 7h		
				add si, BRD_COLS*2
				add si, type brd
				jmp @@Con
				
	@@HorsDDLM:	add ah, 6h
				sub al, 7h
				add si, BRD_COLS*2
				sub si, type brd
				jmp @@Con
				
	@@HorsDLLM:	add ah, 3h
				sub al, 0eh
				add si, BRD_COLS
				sub si, type brd*2
				jmp @@Con
				
	@@HorsULLM:	sub ah, 3h
				sub al, 0eh		
				sub si, BRD_COLS
				sub si, type brd*2
				jmp @@Con
				
	@@HorsUULM:	sub ah, 6h
				sub al, 7h		
				sub si, BRD_COLS*2
				sub si, type brd
				jmp @@Con
				
	@@WhereJmp:	cmp cx, 8h			
				jnz @@ConTo7
				jmp @@HorsUURM
	@@ConTo7:	cmp cx, 7h
				jnz @@ConTo6
				jmp @@HorsURRM
	@@ConTo6:	cmp cx, 6h
				jnz @@ConTo5
				jmp @@HorsDRRM
	@@ConTo5:	cmp cx, 5h
				jnz @@ConTo4
				jmp @@HorsDDRM
	@@ConTo4:	cmp cx, 4h
				jnz @@ConTo3
				jmp @@HorsDDLM
	@@ConTo3:	cmp cx, 3h
				jnz @@ConTo2
				jmp @@HorsDLLM
	@@ConTo2:	cmp cx, 2h
				jnz	@@ConTo1
				jmp @@HorsULLM
	@@ConTo1:	jmp @@HorsUULM
				
	@@LoopWhr:	mov si, di
				push 0h offset brd si
				call GetLocByEA
				pop ax
				loop @@WhereJmp
				jmp @@Sof
	
	@@Con:		mov bl, [si]
				cmp bl, '0'
				jnz @@Etry		;Eat TRY				
				cmp ax, dx
				jz @@DoMove
				push 0h ax
				call CheckBorderCross
				pop bx
				cmp bl, 1h
				jz @@LoopWhr
				jmp @@WhereJmp
	
	@@Etry:		cmp ax, dx
				jnz @@LoopWhr
				push 0h di si
				call CheckIfAeatB
				pop ax
				cmp al, 1h
				jnz @@LoopWhr
	
	@@DoMove:	mov bl, [di]	;bl -> the move tool
				mov bh, '0'		;bh -> empty area
				mov [di], bh	;clean the area
				mov [si], bl	;mov the tool
				push ax				;save ax
				push 0h xTurn[0]
				call CheckChess
				pop ax
				cmp al, 1h
				pop ax				;load ax
				jz @@UndoMove
				call SwitchTurns
				jmp @@Sof
	@@UndoMove:	mov [si], bh	;clean the new area
				mov [di], bl	;return the tool to its place
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
HorseRules EndP

Castling	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di				
				mov si, [bp+4]	;offset of tool will be moved (?)
				mov dx, [bp+6]	;Location To Move
				mov cx, 2h		;Big castling/Small castling
				mov di, si
				push 0h offset brd si
				call GetLocByEA
				pop ax					
				
	@@WhereJmp:	cmp cx, 2h
				jnz	@@ConTo1
				jmp @@CstlRR			;small castling
	@@ConTo1:	jmp @@CstlLL			;big castling
	
	@@CstlRR:	add al, 7h
				add si, type brd
				jmp @@Con
				
	@@CstlLL:	sub al, 7h
				sub si, type brd
				jmp @@Con
							
	@@Con:		mov bl, [si]
				cmp bl, '0'
				jnz @@LoopWhr						
				cmp ax, dx
				jz @@SrchRlvntR				
				jmp @@WhereJmp
	
	@@LoopWhr:	mov si, di
				push 0h offset brd si
				call GetLocByEA
				pop ax
				loop @@WhereJmp
				jmp @@Sof	
	
	@@SrchRlvntR:
				cmp cx, 2h
				jz @@SCstl
				sub si, type brd*2
				mov bl, 'R'
				cmp [si], bl
				jnz @@Opp
				jmp @@DoBCstl
	@@Opp:		mov bl, 'r'
				cmp [si], bl
				jnz @@Sof
				jmp @@DoBCstl
				
	@@SCstl:	add si, type brd
				mov bl, 'R'
				cmp [si], bl
				jnz @@Opp2
				jmp @@DoSCstl
	@@Opp2:		mov bl, 'r'
				cmp [si], bl
				jnz @@Sof
								;si = Rook,	di = King
	@@DoBCstl:	mov bl, [di]
				mov bh, [si]
				mov al, '0'
				mov [di], al
				mov [si], al
				sub di, type brd*2
				mov [di], bl
				mov si, di
				add si, type brd
				mov [si], bh
				jmp @@MovDone
								
	@@DoSCstl:	mov bl, [di]
				mov bh, [si]
				mov al, '0'
				mov [di], al
				mov [si], al
				add di, type brd*2
				mov [di], bl
				mov si, di
				sub si, type brd
				mov [si], bh
				
							
	@@MovDone:	mov castlArr[0], 1h	
				mov castlArr[1], 1h
				call SwitchTurns
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
Castling	Endp

KingRules Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di	
				mov si, [bp+4]	;offset of tool will be moved (?)
				mov dx, [bp+6]	;Location To Move
				mov cx, 8h		;Up/Down/Left/Right
				mov di, si
				mov bl, [si]
				push 0h offset brd si
				call GetLocByEA
				pop ax					
				jmp @@WhereJmp
	
	@@KingUM:	sub ah, 3h
				sub si, BRD_COLS
				jmp @@Con
				
	@@KingURM:	sub ah, 3h
				add al, 7h
				sub si, BRD_COLS
				add si, type brd
				jmp @@Con
				
	@@KingRM:	add al, 7h		
				add si, type brd
				jmp @@Con
				
	@@KingDRM:	add ah, 3h
				add al, 7h		
				add si, BRD_COLS
				add si, type brd
				jmp @@Con
				
	@@KingDM:	add ah, 3h
				add si, BRD_COLS
				jmp @@Con
				
	@@KingDLM:	add ah, 3h
				sub al, 7h
				add si, BRD_COLS
				sub si, type brd
				jmp @@Con
				
	@@KingLM:	sub al, 7h		
				sub si, type brd
				jmp @@Con
				
	@@KingULM:	sub ah, 3h
				sub al, 7h		
				sub si, BRD_COLS
				sub si, type brd
				jmp @@Con
				
	@@WhereJmp:	cmp cx, 8h			
				jnz @@ConTo7
				jmp @@KingUM
	@@ConTo7:	cmp cx, 7h
				jnz @@ConTo6
				jmp @@KingURM
	@@ConTo6:	cmp cx, 6h
				jnz @@ConTo5
				jmp @@KingRM
	@@ConTo5:	cmp cx, 5h
				jnz @@ConTo4
				jmp @@KingDRM
	@@ConTo4:	cmp cx, 4h
				jnz @@ConTo3
				jmp @@KingDM
	@@ConTo3:	cmp cx, 3h
				jnz @@ConTo2
				jmp @@KingDLM
	@@ConTo2:	cmp cx, 2h
				jnz	@@ConTo1
				jmp @@KingLM
	@@ConTo1:	jmp @@KingULM
				
	@@LoopWhr:	mov si, di
				push 0h offset brd si
				call GetLocByEA
				pop ax
				loop @@WhereJmp
				push dx si
				call Castling
				jmp @@Sof
	
	@@Con:		mov bl, [si]
				cmp bl, '0'
				jnz @@Etry		;Eat TRY				
				cmp ax, dx
				jz @@DoMove
				jmp @@LoopWhr
	
	@@Etry:		cmp ax, dx
				jnz @@LoopWhr
				push 0h di si
				call CheckIfAeatB
				pop ax
				cmp al, 1h
				jnz @@LoopWhr
	
	@@DoMove:	@@DoMove:	mov bl, [di]	;bl -> the move tool
				mov bh, '0'		;bh -> empty area
				mov [di], bh	;clean the area
				mov [si], bl	;mov the tool
				push ax				;save ax
				push 0h xTurn[0]
				call CheckChess
				pop ax
				cmp al, 1h
				pop ax				;load ax
				jz @@UndoMove
				call SwitchTurns
				mov castlArr[0], 1h
				mov castlArr[1], 1h
				jmp @@Sof
	@@UndoMove:	mov [si], bh	;clean the new area
				mov [di], bl	;return the tool to its place
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
KingRules EndP

SelectGameTypeMenu	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov dx, 0h
				mov ah, 2h 
				int 10h
				push offset SelPlrorCmp
				call WriteStr
				mov ah, 7h
				int 21h
				cmp al, 'c'
				jnz @@Sof
				call ClrScr
				push 0h
				call SelectColorMenu
				pop ax				
				mov [bp+4], al
	@@Sof:		pop di si dx cx bx ax bp
				ret 
SelectGameTypeMenu	Endp

SelectColorMenu	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di bp
	@@StartM:	mov dx, 0823h	;First Location
				mov bl, 00001111b	;First Color
				mov cx, MEN_CLR_LEN
				mov si, 0h
				mov bp, offset clrMenu				
	@@Again:	push dx cx bx bp
				call PaintStr
				inc dh
				add bp, cx
				inc si
				cmp si, 2h				
				jnz @@Again
				mov si, 0h
				mov bp, offset clrMenu
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
				mov [bp+4], 'A'
				cmp si, 0h
				jnz @@Sof2
				mov [bp+4], 'a'				
	@@Sof2:		pop di si dx cx bx ax bp				
				ret 
SelectColorMenu EndP

LoadCurrentTools	Proc	;Loading into [offset rnk].RootAdrs all Tools Adresses 
				push bp
				mov bp, sp
				push cx si di
				mov di, offset rnk			;offset rnk - Loading Into [offset rnk].RootAdrs
				mov si, offset brd			;offset brd - Loading From
				mov cx, BRD_LEN
	@@Process:	mov al, [si]
				cmp al, '0'
				jz @@Skip				
				mov [di].RootAdrs, si
				add si, type brd
				add di, type rnk
	@@Skip:		loop @@Process
				pop di si cx bp
				ret
LoadCurrentTools	EndP

IsMyEnemy	Proc
				push bp
				mov bp, sp
				push ax bx si
				mov si, [bp+4]	;offset tool to check if enemy
				
				push 0h 
				push 0040h
				push [si]
				push 005bh
				call IfxBetweenAB
				pop ax
				
				push 0h 
				push 0040h
				push xTurn[0]
				push 005bh
				call IfxBetweenAB
				pop bx
				
				cmp ax, bx			;if they are not the same , enemys ! 
				jz @@Sof	
				mov [bp+6], 1h
	@@Sof:		pop si bx ax bp
				ret 2
IsMyEnemy	EndP

IsABEnemys	Proc
				push bp
				mov bp, sp
				push ax bx si di
				mov si, [bp+4]	;offset tool A
				mov di, [bp+6]	;offset tool B
				
				push 0h 
				push 0040h
				push [si]
				push 005bh
				call IfxBetweenAB
				pop ax
				
				push 0h 
				push 0040h
				push [di]
				push 005bh
				call IfxBetweenAB
				pop bx
				
				cmp ax, bx			;if they are not the same , enemys ! 
				jz @@Sof	
				mov [bp+8], 1h
	@@Sof:		pop di si bx ax bp
				ret 4
IsABEnemys	EndP

PawnMovePC	Proc
				push bp 
				mov bp, sp
				push ax bx cx dx si di
				mov di, [bp+4]	; di -> rnk 		--> Load move to [di].DestAdrs
				mov si, [bp+6]	; si -> brd			-->	Offset of tool that will move				
				mov bl, [bp+8]	;xTurn[0]
				mov cl, [bp+10]	;if(cl = 1) ;pawn 1 move	if(cl == 2) ;pawn 2 moves 
								;if(cl = 3) pawn eat move left; if(cl = 4) pawn eat move right 	
				inc cl
				xor ch, ch
				
				push 0h offset brd si
				call GetLocByEA
				pop dx
				push si		;save si for move/dont move
				cmp bl, 2h
				jnz @@ConProg
				cmp dh, 13h
				jz @@ConProg
				cmp dh, 4h
				jz @@ConProg
				jmp @@DontMove
				
	@@ConProg:	mov al, [si]
				cmp bl, 'A'						
				jz @@FromDown
				jmp @@FromUp
				
	@@FromDown:	cmp cl, 3h
				jnc @@EatFD
				sub si, BRD_COLS
				sub dh, 3h
				push cx			;save cx
				push 0h dx
				call CheckBorderCross
				pop cx
				cmp cx, 1h
				pop cx			;load cx
				jnz @@Con1
				jmp @@DontMove
	@@Con1:		mov ah, [si]
				cmp ah, '0'
				jz @@ConLoop
				jmp @@DontMove				
	@@ConLoop:	loop @@FromDown
				jmp @@DoMove
				
	@@FromUp:	cmp cl, 3h
				jnc @@EatFU
				add si, BRD_COLS
				add dh, 3h
				push cx			;save cx
				push 0h dx
				call CheckBorderCross
				pop cx
				cmp cx, 1h
				pop cx			;load cx
				jz @@JmpDM
				mov ah, [si]
				cmp ah, '0'
				jnz @@JmpDM		
				loop @@FromUp
				jmp @@DoMove
				
	@@JmpDM:	jmp @@DontMove
				
	@@EatFD:	sub si, BRD_COLS
				sub dh, 3h
				jmp @@Con
	@@EatFU:	add si, BRD_COLS
				add dh, 3h
				jmp @@Con
				
	@@Con:		cmp cl, 3h
				jnz @@PawnEatR		
				sub si, type brd
				sub dl, 7h
				push ax			;save ax
				mov al, [si]
				cmp al, '0'
				pop ax			;load ax
				jnz @@ConEatL
				jmp @@DontMove
	@@ConEatL:	push cx			;save cx
				push 0h dx
				call CheckBorderCross
				pop cx
				cmp cx, 1h
				pop cx			;load cx
				jz @@DontMove
				push cx			;save cx2
				push 0h si
				call IsMyEnemy
				pop cx
				cmp cl, 0h
				pop cx			;load cx2				
				jz @@DontMove
				jmp @@DoMove
				
	@@PawnEatR:	add si, type brd	;if right eat
				add dl, 7h
				push ax		;save ax
				mov al, [si]
				cmp al, '0'
				pop ax		;load ax
				jnz @@ConEatR
				jmp @@DontMove
	@@ConEatR:	push cx			;save cx
				push 0h dx
				call CheckBorderCross
				pop cx
				cmp cx, 1h
				pop cx			;load cx
				jz @@DontMove
				push cx			;save cx2
				push 0h si
				call IsMyEnemy
				pop cx
				cmp cl, 0h
				pop cx			;load cx2		
				jz @@DontMove
				jmp @@DoMove
				
	@@DontMove:	pop si			;load old si 1
				jmp @@Sof
				
	@@DoMove:	mov [di].DestAdrs, si
				pop si			;load old si 2		
				mov [di].RootAdrs, si
				add di, type rnk
				
	@@Sof:		mov [bp+12], di
				pop di si dx cx bx ax bp
				ret 8
PawnMovePC	Endp

RookMovePC	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov di, [bp+4]	;offset rnk
				mov si, [bp+6]	;offset (tool) brd
				mov ax, [bp+8]	;Move instructor
				
				mov cl, 7h
				div cl			;al(full), ah(rest) = al/7
				mov cx, ax		;resault -> move instructor
								
				mov bl, [si]	;Letter of tool
				xor ax, ax
				push si		;save si to move/dont move
				mov al, cl	;al = where to move
				mov cl, ch	;cl = how much
				xor ch, ch	
				inc cl		;For the loop ! 
				inc cl		;For the first jump ! 
				;al = type jmp; cl = num of moves (+1 for first jump)
				
				push 0h si
				call GetIndexPointsByEA
				pop dx				;dl = y, dh = x
				
	@@WhrLoop:	cmp al, 0h
				jnz @@ConTo1
				loop @@RookUM
				jmp @@DoMove
				
	@@ConTo1:	cmp al, 1h
				jnz @@ConTo2
				loop @@RookDM
				jmp @@DoMove
				
	@@ConTo2:	cmp al, 2h
				jnz @@ConTo3
				loop @@RookRM
				jmp @@DoMove
				
	@@ConTo3:	loop @@RookLM
				jmp @@DoMove
				
	@@RookRM:	add si, type brd
				inc dh
				jmp @@ConMove
				
	@@RookLM:	sub si, type brd
				dec dh
				jmp @@ConMove
	
	@@RookDM:	add si, BRD_COLS
				inc dl
				jmp @@ConMove
	
	@@RookUM:	sub si, BRD_COLS
				dec dl
				
	@@ConMove:	push dx			;save points 1
				push 0h dx
				call IsBorderCrossByPoints			
				pop dx
				cmp dx, 1h					;Checking Border Cross
				pop dx			;load points 1
				jz @@DontMove
				push dx			;save points 2
				mov dl, [si]
				cmp dl, '0'
				pop dx			;load points 2
				jz @@WhrLoop ;Whree to looppp niggggggaaaaaaa ;loop @@RoomRM				
				
				cmp cl, 1h				;Eat check ! 
				jnz @@DontMove
				push dx				;save points 3
				push 0h si
				call IsMyEnemy
				pop dx
				cmp dl, 1h
				pop dx				;load points 3			
				jz @@DoMove

	@@DontMove:	pop si			;load old si 1
				jmp @@Sof
				
	@@DoMove:	mov [di].DestAdrs, si
				pop si			;load old si 2				
				mov [di].RootAdrs, si
				add di, type rnk
				
	@@Sof:		mov [bp+10], di
				pop di si dx cx bx ax bp
				ret 6
RookMovePC	Endp

BishopMovePC	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov di, [bp+4]	;offset rnk
				mov si, [bp+6]	;offset (tool) brd
				mov ax, [bp+8]	;Move instructor
				
				mov cl, 7h
				div cl			;al(full), ah(rest) = al/7
				mov cx, ax		;resault -> move instructor
								
				xor ax, ax
				push si		;save si to move/dont move
				mov al, cl	;al = where to move
				mov cl, ch	;cl = how much
				xor ch, ch	
				inc cl		;For the loop ! 
				inc cl		;For the first jump ! 
				;al = type jmp; cl = num of moves (+1 for first jump)
				
				push 0h si
				call GetIndexPointsByEA
				pop dx				;dl = y, dh = x
				
	@@WhrLoop:	cmp al, 0h
				jnz @@ConTo1
				loop @@BishURM
				jmp @@DoMove
				
	@@ConTo1:	cmp al, 1h
				jnz @@ConTo2
				loop @@BishULM
				jmp @@DoMove
				
	@@ConTo2:	cmp al, 2h
				jnz @@ConTo3
				loop @@BishDRM
				jmp @@DoMove
				
	@@ConTo3:	loop @@BishDLM
				jmp @@DoMove
				
	@@BishURM:	sub si, BRD_COLS
				add si, type brd
				dec dl
				inc dh
				jmp @@ConMove
				
	@@BishULM:	sub si, BRD_COLS
				sub si, type brd
				dec dl
				dec dh
				jmp @@ConMove
	
	@@BishDRM:	add si, BRD_COLS
				add si, type brd
				inc dl
				inc dh
				jmp @@ConMove
	
	@@BishDLM:	add si, BRD_COLS		
				sub si, type brd
				inc dl
				dec dh
				
	@@ConMove:	push dx				;save points 1
				push 0h dx
				call IsBorderCrossByPoints			
				pop dx
				cmp dx, 1h					;Checking Border Cross
				pop dx				;load points 1
				jz @@DontMove
				push dx			;save points 2
				mov dl, [si]
				cmp dl, '0'
				pop dx			;load points 2
				jz @@WhrLoop ;Whree to looppp niggggggaaaaaaa ;loop @@RoomRM				
				
				cmp cl, 1h				;Eat check ! 
				jnz @@DontMove
				push dx				;save points 3
				push 0h si
				call IsMyEnemy
				pop dx
				cmp dl, 1h
				pop dx				;load points 3
				jz @@DoMove
	
	@@DontMove:	pop si			;load old si 1
				jmp @@Sof
				
	@@DoMove:	mov [di].DestAdrs, si
				pop si			;load old si 2				
				mov [di].RootAdrs, si
				add di, type rnk
				
	@@Sof:		mov [bp+10], di
				pop di si dx cx bx ax bp
				ret 6
BishopMovePC	Endp

HorseMovePC	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov di, [bp+4]	;offset rnk
				mov si, [bp+6]	;offset (tool) brd
				mov cx, [bp+8]	;move selecter ; cl = where to		
				 							
				push si		;save si to move/dont move
				
				push 0h si
				call GetIndexPointsByEA
				pop dx				;dl = y, dh = x
				
	@@WhereMov:	cmp cl, 0h
				jnz @@ConTo1
				jmp @@HorsUURM
	@@ConTo1:	cmp cl, 1h
				jnz @@ConTo2
				jmp @@HorsURRM
	@@ConTo2:	cmp cl, 2h
				jnz @@ConTo3
				jmp @@HorsDRRM
	@@ConTo3:	cmp cl, 3h
				jnz @@ConTo4
				jmp @@HorsDDRM
	@@ConTo4:	cmp cl, 4h
				jnz @@ConTo5
				jmp @@HorsDDLM
	@@ConTo5:	cmp cl, 5h
				jnz @@ConTo6
				jmp @@HorsDLLM
	@@ConTo6:	cmp cl, 6h
				jnz @@ConTo7
				jmp @@HorsULLM
	@@ConTo7:	jmp @@HorsUULM
	
	@@HorsUURM:	sub si, BRD_COLS*2
				add si, type brd	
				sub dl, 2h
				inc dh
				jmp @@Con
				
	@@HorsURRM:	sub si, BRD_COLS
				add si, type brd*2	
				dec dl
				add dh, 2h
				jmp @@Con
				
	@@HorsDRRM:	add si, BRD_COLS
				add si, type brd*2
				inc dl
				add dh, 2h
				jmp @@Con
		
	@@HorsDDRM:	add si, BRD_COLS*2
				add si, type brd
				add dl, 2h
				inc dh
				jmp @@Con
		
	@@HorsDDLM:	add si, BRD_COLS*2
				sub si, type brd
				add dl, 2h
				dec dh
				jmp @@Con
				
	@@HorsDLLM:	add si, BRD_COLS
				sub si, type brd*2
				inc dl
				sub dh, 2h
				jmp @@Con
	
	@@HorsULLM:	sub si, BRD_COLS
				sub si, type brd*2
				dec dl
				sub dh, 2h
				jmp @@Con
				
	@@HorsUULM:	sub si, BRD_COLS*2
				sub si, type brd
				sub dl, 2h
				dec dh
				
	@@Con:		push 0h dx
				call IsBorderCrossByPoints			
				pop dx
				cmp dx, 1h					;Checking Border Cross
				jz @@DontMove
				mov dl, [si]
				cmp dl, '0'
				jz @@DoMove
				push 0h si
				call IsMyEnemy
				pop cx
				cmp cl, 1h
				jz @@DoMove
	
	@@DontMove:	pop si			;load old si 1
				jmp @@Sof
				
	@@DoMove:	mov [di].DestAdrs, si
				pop si			;load old si 2				
				mov [di].RootAdrs, si
				add di, type rnk				
				
	@@Sof:		mov [bp+10], di
				pop di si dx cx bx ax bp
				ret 6
HorseMovePC	Endp

KingMovePC	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov di, [bp+4]	;offset rnk
				mov si, [bp+6]	;offset (tool) brd
				mov cx, [bp+8]	;move selecter
				; cl = where to 
						
				push si		;save si to move/dont move
				
				push 0h si
				call GetIndexPointsByEA
				pop dx				;dl = y, dh = x
				
	@@WhereMov:	cmp cl, 0h
				jnz @@ConTo1
				jmp @@KingU
	@@ConTo1:	cmp cl, 1h
				jnz @@ConTo2
				jmp @@KingUR
	@@ConTo2:	cmp cl, 2h
				jnz @@ConTo3
				jmp @@KingR
	@@ConTo3:	cmp cl, 3h
				jnz @@ConTo4
				jmp @@KingDR
	@@ConTo4:	cmp cl, 4h
				jnz @@ConTo5
				jmp @@KingD
	@@ConTo5:	cmp cl, 5h
				jnz @@ConTo6
				jmp @@KingDL
	@@ConTo6:	cmp cl, 6h
				jnz @@ConTo7
				jmp @@KingL
	@@ConTo7:	jmp @@KingUL
	
	@@KingU:	sub si, BRD_COLS
				dec dl
				jmp @@Con
				
	@@KingUR:	sub si, BRD_COLS
				add si, type brd
				dec dl
				inc dh
				jmp @@Con
				
	@@KingR:	add si, type brd
				inc dh
				jmp @@Con
		
	@@KingDR:	add si, BRD_COLS
				add si, type brd
				inc dl
				inc dh
				jmp @@Con
		
	@@KingD:	add si, BRD_COLS
				inc dl
				jmp @@Con
				
	@@KingDL:	add si, BRD_COLS
				sub si, type brd
				inc dl
				dec dh
				jmp @@Con
	
	@@KingL:	sub si, type brd
				dec dh
				jmp @@Con
				
	@@KingUL:	sub si, BRD_COLS
				sub si, type brd
				dec dl
				dec dh
	
	@@Con:		push 0h dx
				call IsBorderCrossByPoints			
				pop dx
				cmp dx, 1h					;Checking Border Cross
				jz @@DontMove
				mov dl, [si]
				cmp dl, '0'
				jz @@DoMove
				push 0h si
				call IsMyEnemy
				pop cx
				cmp cl, 1h
				jz @@DoMove
	
	@@DontMove:	pop si			;load old si 1
				jmp @@Sof
				
	@@DoMove:	mov [di].DestAdrs, si
				pop si			;load old si 2				
				mov [di].RootAdrs, si
				add di, type rnk	
				
	@@Sof:		mov [bp+10], di
				mov [bp+12], si	
				pop di si dx cx bx ax bp
				ret 6
KingMovePC	EndP

GetIndexPointsByEA	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov ax, [bp+4]	;adress of tool to find index
				sub ax, offset brd
				mov bl, 8h
				div bl
				mov [bp+6], ax		;al = y ; ah = x
				pop di si dx cx bx ax bp
				ret 2
GetIndexPointsByEA	Endp

IsBorderCrossByPoints Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov dx, [bp+4]			;points
				mov [bp+6] ,1h			;there is a cross !!
				cmp dl, 0h
				jc @@Sof
				cmp dl, BRD_COLS
				jnc @@Sof
				cmp dh, 0h
				jc @@Sof
				cmp dh, BRD_COLS
				jnc @@Sof
				mov [bp+6] ,0h
	@@Sof:		pop di si dx cx bx ax bp
				ret 2
IsBorderCrossByPoints Endp

CheckMate	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di				
				mov cl, 1h					;is MATE!!!!
				call ClearRnk
				push offset brd offset rnk
				call DoAllPossibleMoves	
				lea di, rnk
				lea si, brd				
				
	@@NextRnk:	cmp [di].RootAdrs, 0h
				jnz @@Con1
				jmp @@Sof				;MATE!!!
	@@Con1:		mov bx, [di].RootAdrs
				mov al, [bx]			;save Original
				mov dl, '0'
				mov [bx], dl
				mov bx, [di].DestAdrs
				mov ah, [bx]			;save Original
				mov [bx], al
				
				mov dx, [bp+4]			;xTurn -> who to check mate...
				push 0h dx	
				call CheckChess
				pop dx
				
				mov bx, [di].RootAdrs
				mov [bx], al			;load Original
				mov bx, [di].DestAdrs
				mov [bx], ah			;load Original
				
				cmp dl, 1h			
				jz @@Contin
				xor cl, cl
				jmp @@Sof
				
	@@Contin:	add di, type rnk
				jmp @@NextRnk
								
	@@Sof:		mov [bp+6], cl
				pop di si dx cx bx ax bp
				ret 2
CheckMate	Endp

ClearRnk	Proc
				push di
				lea di, rnk
	@@NextRnk:	cmp [di].RootAdrs, 0h
				jz @@Sof
				mov [di].RootAdrs, 0000h
				mov [di].DestAdrs, 0000h
				mov [di].moveRank, 0h
				add di, type rnk
				jmp @@NextRnk
	@@Sof:		pop di
				ret
ClearRnk	Endp

DoAllPossibleMoves	Proc
				push bp 
				mov bp, sp
				push ax bx cx dx si di
				xor bx, bx		;bl = What move will be done dymanic by tool 
				mov di, [bp+4]	;offset rnk
				mov si, [bp+6]	;offset brd
				mov cx, BRD_LEN
				
	@@GetTool:	xor ax, ax
				mov al, [si]	;Letter of tool	
				cmp al, '0'
				jnz @@ConPoss
				jmp @@NextTool
	@@ConPoss:	push 0h si
				call IsMyEnemy
				pop dx
				cmp dl, 1h
				jnz @@ConPoss2
				jmp @@NextTool
	@@ConPoss2:	push 0h
				push 0060h
				push ax 
				push 007bh
				call IfxBetweenAB
				pop dx
				cmp dl, 1h
				jnz @@ConToP
				sub ax, 20h
				
	@@ConToP:	cmp al, 'P'
				jnz @@ConToR
				cmp bl, P_OPPMOV
				jc @@ConWP
				jmp @@NextTool
	@@ConWP:	push 0h bx xTurn[0] si di
				call PawnMovePC				
				pop di				
				jmp @@NextMove
				
	@@ConToR:	cmp al, 'R'
				jnz @@ConToB
				cmp bl, R_OPPMOV
				jc @@ConWR
				jmp @@NextTool
	@@ConWR:	push 0h bx si di
				call RookMovePC
				pop di
				jmp @@NextMove
				
	@@ConToB:	cmp al, 'B'
				jnz @@ConToQ
				cmp bl, B_OPPMOV
				jc @@ConWB
				jmp @@NextTool
	@@ConWB:	push 0h bx si di
				call BishopMovePC
				pop di
				jmp @@NextMove
				
	@@ConToQ:	cmp al, 'Q'
				jnz @@ConToH
				cmp bl, Q_OPPMOV
				jc @@ConWQ
				jmp @@NextTool
	@@ConWQ:	push 0h bx si di
				call BishopMovePC
				pop di
				push 0h bx si di
				call RookMovePC
				pop di
				jmp @@NextMove
				
	@@ConToH:	cmp al, 'H'
				jnz @@ConToK
				cmp bl, H_OPPMOV
				jnc @@NextTool
				push 0h bx si di
				call HorseMovePC
				pop di
				jmp @@NextMove
				
	@@ConToK:	cmp al, 'K'
				jnz @@Sof
				cmp bl, K_OPPMOV
				jnc @@NextTool
				push 0h bx si di
				call KingMovePC
				pop di
				jmp @@NextMove
				
	@@NextTool:	add si, type brd		;Next Adress in brd array
				dec cx
				jz @@Sof
				xor bl, bl				;Reset bl (using @@NextMove)
				dec bl
	@@NextMove:	inc bl
				jmp @@GetTool
				
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
DoAllPossibleMoves	Endp


FindHigherMoveRank	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov di, [bp+4]		;offset rnk
				mov ax, [di].moveRank			;al = max
				mov dx ,di
	@@AxMax:	cmp [di].RootAdrs, 0h
				jz @@Sof
				add di, type rnk
				mov bx, [di].moveRank
				cmp ax, bx
				jnc @@AxMax
				mov ax, bx
				mov dx ,di
				jmp @@AxMax
	@@Sof:		mov [bp+6], dx
				pop di si dx cx bx ax bp
				ret 2
FindHigherMoveRank	Endp

CountRook	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]		;offset (tool) brd				
				xor bx, bx			;counter
				xor cx, cx
				mov di, si			;save original
				
				xor ax, ax			;where to loop counter
				dec al				;for first move
				
	@@Ipus:		inc al
				mov cl, 7h	
				inc cl				;for first move
				mov si, di
				push 0h si
				call GetIndexPointsByEA
				pop dx				;dl = y, dh = x
				
				
	
	@@WhrLoop:	cmp al, 0h
				jnz @@Con1
				loop @@CountRM
				
				jmp @@Ipus
				
	@@Con1:		cmp al, 1h
				jnz @@Con2
				loop @@CountLM
				
				jmp @@Ipus
				
	@@Con2:		cmp al, 2h
				jnz @@Con3
				loop @@CountUM
				
				jmp @@Ipus
				
	@@Con3:		cmp al, 3h
				jnz @@Con4
				loop @@CountDM
				
	@@Con4:		jmp @@Sof
	
	@@CountRM:	add si, type brd	;count right move
				inc dh
				jmp @@Con
	@@CountLM:	sub si, type brd
				dec dh
				jmp @@Con
	@@CountUM:	sub si, BRD_COLS
				dec dl
				jmp @@Con
	@@CountDM:	add si, BRD_COLS
				inc dl
				
	@@Con:		push dx			;save dx
				push 0h dx
				call IsBorderCrossByPoints			
				pop dx
				cmp dx, 1h					;Checking Border Cross
				pop dx			;load dx
				jz @@DntCount	;jmp if cross
				push cx			;save cx
				mov cl, [si]
				cmp cl, '0'
				pop cx			;load cx
				jnz @@ChkEat
				inc bl
				jmp @@WhrLoop
				
	@@ChkEat:	push 0h si
				call IsMyEnemy
				pop dx
				cmp dx, 1h
				jnz @@DntCount
				inc bl				
	@@DntCount:	jmp @@Ipus
				
	@@Sof:		mov [bp+6], bl
				pop di si dx cx bx ax bp
				ret 2
CountRook	Endp

CountBishop	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]		;offset (tool) brd				
				xor bx, bx			;counter
				xor cx, cx
				mov di, si			;save original
				
				xor ax, ax			;where to loop counter
				dec al				;for first move
				
	@@Ipus:		inc al
				mov cl, 7h
				inc cl				;for first move
				mov si, di
				push 0h si
				call GetIndexPointsByEA
				pop dx				;dl = y, dh = x
				
	@@WhrLoop:	cmp al, 0h
				jnz @@Con1
				loop @@CountURM
				
				jmp @@Ipus
				
	@@Con1:		cmp al, 1h
				jnz @@Con2
				loop @@CountULM
				
				jmp @@Ipus
				
	@@Con2:		cmp al, 2h
				jnz @@Con3
				loop @@CountDRM
				
				jmp @@Ipus
				
	@@Con3:		cmp al, 3h
				jnz @@Con4
				loop @@CountDLM
				
	@@Con4:		jmp @@Sof
	
	@@CountURM:	sub si, BRD_COLS
				add si, type brd				
				dec dl
				inc dh				
				jmp @@Con
	@@CountULM:	sub si, BRD_COLS
				sub si, type brd
				dec dl
				dec dh
				jmp @@Con
	@@CountDRM:	add si, BRD_COLS
				add si, type brd
				inc dl
				inc dh
				jmp @@Con
	@@CountDLM:	add si, BRD_COLS
				sub si, type brd
				inc dl
				dec dh
				
	@@Con:		push dx			;save dx
				push 0h dx
				call IsBorderCrossByPoints			
				pop dx
				cmp dx, 1h					;Checking Border Cross
				pop dx			;load dx
				jz @@DntCount	;jmp if cross
				push cx			;save cx
				mov cl, [si]
				cmp cl, '0'
				pop cx			;load cx
				jnz @@ChkEat
				inc bl
				jmp @@WhrLoop
				
	@@ChkEat:	push 0h si
				call IsMyEnemy
				pop dx
				cmp dx, 1h
				jnz @@DntCount
				inc bl				
	@@DntCount:	jmp @@Ipus
				
	@@Sof:		mov [bp+6], bl
				pop di si dx cx bx ax bp
				ret 2
CountBishop	Endp

CountHorse	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]		;offset (tool) brd				
				xor bx, bx			;counter
				mov di, si			;save original
				
				xor ax, ax			;where to loop counter
				dec al				;for dirst move
				
	@@Ipus:		inc al
				mov si, di			;load original
				push 0h si
				call GetIndexPointsByEA
				pop dx				;dl = y, dh = x
				
	@@WhrJmp:	cmp al, 0h
				jnz @@ConTo1
				jmp @@HorsUURM
	@@ConTo1:	cmp al, 1h
				jnz @@ConTo2
				jmp @@HorsURRM
	@@ConTo2:	cmp al, 2h
				jnz @@ConTo3
				jmp @@HorsDRRM
	@@ConTo3:	cmp al, 3h
				jnz @@ConTo4
				jmp @@HorsDDRM
	@@ConTo4:	cmp al, 4h
				jnz @@ConTo5
				jmp @@HorsDDLM
	@@ConTo5:	cmp al, 5h
				jnz @@ConTo6
				jmp @@HorsDLLM
	@@ConTo6:	cmp al, 6h
				jnz @@ConTo7
				jmp @@HorsULLM
	@@ConTo7:	cmp al, 7h
				jnz @@ConTo8
				jmp @@HorsUULM
	@@ConTo8:	jmp @@Sof
	
	@@HorsUURM:	sub si, BRD_COLS*2
				add si, type brd	
				sub dl, 2h
				inc dh
				jmp @@Con
				
	@@HorsURRM:	sub si, BRD_COLS
				add si, type brd*2	
				dec dl
				add dh, 2h
				jmp @@Con
				
	@@HorsDRRM:	add si, BRD_COLS
				add si, type brd*2
				inc dl
				add dh, 2h
				jmp @@Con
		
	@@HorsDDRM:	add si, BRD_COLS*2
				add si, type brd
				add dl, 2h
				inc dh
				jmp @@Con
		
	@@HorsDDLM:	add si, BRD_COLS*2
				sub si, type brd
				add dl, 2h
				dec dh
				jmp @@Con
				
	@@HorsDLLM:	add si, BRD_COLS
				sub si, type brd*2
				inc dl
				sub dh, 2h
				jmp @@Con
	
	@@HorsULLM:	sub si, BRD_COLS
				sub si, type brd*2
				dec dl
				sub dh, 2h
				jmp @@Con
				
	@@HorsUULM:	sub si, BRD_COLS*2
				sub si, type brd
				sub dl, 2h
				dec dh
				
	@@Con:		push 0h dx
				call IsBorderCrossByPoints			
				pop cx
				cmp cx, 1h					;Checking Border Cross
				jz @@DntCount	;jmp if cross
				push cx			;save cx
				mov cl, [si]
				cmp cl, '0'
				pop cx			;load cx
				jnz @@ChkEat
				inc bl			;increase counter
				jmp @@Ipus
				
	@@ChkEat:	push 0h si
				call IsMyEnemy
				pop cx
				cmp cx, 1h
				jnz @@DntCount
				inc bl				
	@@DntCount:	jmp @@Ipus
				
	@@Sof:		mov [bp+6], bl
				pop di si dx cx bx ax bp
				ret 2
CountHorse	Endp

CountKing	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]		;offset (tool) brd				
				xor bx, bx			;counter
				mov di, si			;save original
				
				xor ax, ax			;where to loop counter
				dec al				;for dirst move
				
	@@Ipus:		inc al
				mov si, di			;load original
				push 0h si
				call GetIndexPointsByEA
				pop dx				;dl = y, dh = x
				
	@@WhereJmp:	cmp al, 0h
				jnz @@ConTo1
				jmp @@KingU
	@@ConTo1:	cmp al, 1h
				jnz @@ConTo2
				jmp @@KingUR
	@@ConTo2:	cmp al, 2h
				jnz @@ConTo3
				jmp @@KingR
	@@ConTo3:	cmp al, 3h
				jnz @@ConTo4
				jmp @@KingDR
	@@ConTo4:	cmp al, 4h
				jnz @@ConTo5
				jmp @@KingD
	@@ConTo5:	cmp al, 5h
				jnz @@ConTo6
				jmp @@KingDL
	@@ConTo6:	cmp al, 6h
				jnz @@ConTo7
				jmp @@KingL
	@@ConTo7:	cmp al, 7h
				jnz @@ConTo8
				jmp @@KingUL
	@@ConTo8:	jmp @@Sof
	
	@@KingU:	sub si, BRD_COLS
				dec dl
				jmp @@Con
				
	@@KingUR:	sub si, BRD_COLS
				add si, type brd
				dec dl
				inc dh
				jmp @@Con
				
	@@KingR:	add si, type brd
				inc dh
				jmp @@Con
		
	@@KingDR:	add si, BRD_COLS
				add si, type brd
				inc dl
				inc dh
				jmp @@Con
		
	@@KingD:	add si, BRD_COLS
				inc dl
				jmp @@Con
				
	@@KingDL:	add si, BRD_COLS
				sub si, type brd
				inc dl
				dec dh
				jmp @@Con
	
	@@KingL:	sub si, type brd
				dec dh
				jmp @@Con
				
	@@KingUL:	sub si, BRD_COLS
				sub si, type brd
				dec dl
				dec dh
				
	@@Con:		push 0h dx
				call IsBorderCrossByPoints			
				pop cx
				cmp cx, 1h					;Checking Border Cross
				jz @@DntCount	;jmp if cross
				push cx			;save cx
				mov cl, [si]
				cmp cl, '0'
				pop cx			;load cx
				jnz @@ChkEat
				inc bl			;increase counter
				jmp @@Ipus
				
	@@ChkEat:	push 0h si
				call IsMyEnemy
				pop cx
				cmp cx, 1h
				jnz @@DntCount
				inc bl				
	@@DntCount:	jmp @@Ipus
				
	@@Sof:		mov [bp+6], bl
				pop di si dx cx bx ax bp
				ret 2
CountKing	Endp

CountAvailableMoves	Proc	
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]		;offset brd
				xor dx, dx			;counter
				mov cx, BRD_LEN
				
	@@GetTool:	xor ax, ax	
				mov al, [si]		;Letter of tool
				cmp al, '0'
				jnz @@ConAvlb
				jmp @@NextTool
	@@ConAvlb:	push 0h si
				call IsMyEnemy
				pop bx
				cmp bx, 1h
				jnz @@ConAvlb2
				jmp @@NextTool
	@@ConAvlb2:	push 0h
				push 0060h
				push ax 
				push 007bh
				call IfxBetweenAB
				pop bx
				cmp bx, 1h
				jnz @@ConToP
				sub ax, 20h
				
	@@ConToP:	cmp al, 'P'
				jnz @@ConToR
				jmp @@NextTool
	@@ConToR:	cmp al, 'R'
				jnz @@ConToB
				push 0h si
				call CountRook
				pop bx
				add dx, bx
				jmp @@NextTool
	@@ConToB:	cmp al, 'B'
				jnz @@ConToQ
				push 0h si
				call CountBishop
				pop bx
				add dx, bx
				jmp @@NextTool
	@@ConToQ:	cmp al, 'Q'
				jnz @@ConToH
				push 0h si
				call CountRook
				pop bx
				add dx, bx
				push 0h si
				call CountBishop
				pop bx 
				add dx, bx				
				jmp @@NextTool
	@@ConToH:	cmp al, 'H'
				jnz @@ConToK
				push 0h si
				call CountHorse
				pop bx
				add dx, bx
				jmp @@NextTool
	@@ConToK:	push 0h si
				call CountKing
				pop bx
				add dx, bx
				jmp @@NextTool
				
	@@NextTool:	add si, type brd
				dec cx
				jz @@Sof
				jmp @@GetTool
				
	@@Sof:		mov [bp+6], dx
				pop di si dx cx bx ax bp
				ret 2
CountAvailableMoves Endp

CheckHorseThreat	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]		;king adress	
				mov di, si			;save original
				
				xor ax, ax			;where to loop counter
				dec al				;for dirst move
				
	@@Ipus:		inc al
				mov si, di			;load original
				push 0h si
				call GetIndexPointsByEA
				pop dx				;dl = y, dh = x
				
	@@WhrJmp:	cmp al, 0h
				jnz @@ConTo1
				jmp @@HorsUURM
	@@ConTo1:	cmp al, 1h
				jnz @@ConTo2
				jmp @@HorsURRM
	@@ConTo2:	cmp al, 2h
				jnz @@ConTo3
				jmp @@HorsDRRM
	@@ConTo3:	cmp al, 3h
				jnz @@ConTo4
				jmp @@HorsDDRM
	@@ConTo4:	cmp al, 4h
				jnz @@ConTo5
				jmp @@HorsDDLM
	@@ConTo5:	cmp al, 5h
				jnz @@ConTo6
				jmp @@HorsDLLM
	@@ConTo6:	cmp al, 6h
				jnz @@ConTo7
				jmp @@HorsULLM
	@@ConTo7:	cmp al, 7h
				jnz @@ConTo8
				jmp @@HorsUULM
	@@ConTo8:	jmp @@Sof
	
	@@HorsUURM:	sub si, BRD_COLS*2
				add si, type brd	
				sub dl, 2h
				inc dh
				jmp @@Con
				
	@@HorsURRM:	sub si, BRD_COLS
				add si, type brd*2	
				dec dl
				add dh, 2h
				jmp @@Con
				
	@@HorsDRRM:	add si, BRD_COLS
				add si, type brd*2
				inc dl
				add dh, 2h
				jmp @@Con
		
	@@HorsDDRM:	add si, BRD_COLS*2
				add si, type brd
				add dl, 2h
				inc dh
				jmp @@Con
		
	@@HorsDDLM:	add si, BRD_COLS*2
				sub si, type brd
				add dl, 2h
				dec dh
				jmp @@Con
				
	@@HorsDLLM:	add si, BRD_COLS
				sub si, type brd*2
				inc dl
				sub dh, 2h
				jmp @@Con
	
	@@HorsULLM:	sub si, BRD_COLS
				sub si, type brd*2
				dec dl
				sub dh, 2h
				jmp @@Con
				
	@@HorsUULM:	sub si, BRD_COLS*2
				sub si, type brd
				sub dl, 2h
				dec dh
				
	@@Con:		push 0h dx
				call IsBorderCrossByPoints			
				pop cx
				cmp cx, 1h		;Checking Border Cross
				jz @@Apes		;jmp if cross
				push cx			;save cx
				mov cl, [si]
				cmp cl, '0'
				pop cx			;load cx
				jnz @@ChkEat
				jmp @@Ipus
				
	@@ChkEat:	push 0h si di
				call IsABEnemys
				pop cx
				cmp cx, 1h
				jnz @@Apes
				
				mov cl, [si]
				cmp cl, 'h'
				jz @@Threat
				cmp cl, 'H'
				jz @@Threat
				
	@@Apes:		jmp @@Ipus
	@@Threat:	mov [bp+6], 1h
	@@Sof:		pop di si dx cx bx ax bp
				ret 2
CheckHorseThreat	Endp

CheckKingThreat	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]		;king adress	
				mov di, si			;save original
				
				xor ax, ax			;where to loop counter
				dec al				;for dirst move
				
	@@Ipus:		inc al
				mov si, di			;load original
				push 0h si
				call GetIndexPointsByEA
				pop dx				;dl = y, dh = x
				
	@@WhrJmp:	cmp al, 0h
				jnz @@ConTo1
				jmp @@KingU
	@@ConTo1:	cmp al, 1h
				jnz @@ConTo2
				jmp @@KingUR
	@@ConTo2:	cmp al, 2h
				jnz @@ConTo3
				jmp @@KingR
	@@ConTo3:	cmp al, 3h
				jnz @@ConTo4
				jmp @@KingDR
	@@ConTo4:	cmp al, 4h
				jnz @@ConTo5
				jmp @@KingD
	@@ConTo5:	cmp al, 5h
				jnz @@ConTo6
				jmp @@KingDL
	@@ConTo6:	cmp al, 6h
				jnz @@ConTo7
				jmp @@KingL
	@@ConTo7:	cmp al, 7h
				jnz @@ConTo8
				jmp @@KingUL
	@@ConTo8:	jmp @@Sof
	
	@@KingU:	sub si, BRD_COLS
				dec dl
				jmp @@Con
				
	@@KingUR:	sub si, BRD_COLS
				add si, type brd
				dec dl
				inc dh
				jmp @@Con
				
	@@KingR:	add si, type brd
				inc dh
				jmp @@Con
		
	@@KingDR:	add si, BRD_COLS
				add si, type brd
				inc dl
				inc dh
				jmp @@Con
		
	@@KingD:	add si, BRD_COLS
				inc dl
				jmp @@Con
				
	@@KingDL:	add si, BRD_COLS
				sub si, type brd
				inc dl
				dec dh
				jmp @@Con
	
	@@KingL:	sub si, type brd
				dec dh
				jmp @@Con
				
	@@KingUL:	sub si, BRD_COLS
				sub si, type brd
				dec dl
				dec dh
				
	@@Con:		push 0h dx
				call IsBorderCrossByPoints			
				pop cx
				cmp cx, 1h		;Checking Border Cross
				jz @@Apes		;jmp if cross
				push cx			;save cx
				mov cl, [si]
				cmp cl, '0'
				pop cx			;load cx
				jnz @@ChkEat
				jmp @@Ipus
				
	@@ChkEat:	push 0h si di
				call IsABEnemys
				pop cx
				cmp cx, 1h
				jnz @@Apes
				
				mov cl, [si]
				cmp cl, 'k'
				jz @@Threat
				cmp cl, 'K'
				jz @@Threat
				
	@@Apes:		jmp @@Ipus
	@@Threat:	mov [bp+6], 1h
	@@Sof:		pop di si dx cx bx ax bp
				ret 2
CheckKingThreat	Endp


CheckBishopThreat	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]		;king adress
				mov di, si			;save original
				xor cx, cx
				xor ax, ax			;where to loop counter
				dec al				;for first move
				
	@@Ipus:		inc al
				mov cl, 7h	
				inc cl				;for first move
				mov si, di
				push 0h si
				call GetIndexPointsByEA
				pop dx				;dl = y, dh = x
				
	@@WhrLoop:	cmp al, 0h
				jnz @@Con1
				loop @@CountURM
				
				jmp @@Ipus
				
	@@Con1:		cmp al, 1h
				jnz @@Con2
				loop @@CountULM
				
				jmp @@Ipus
				
	@@Con2:		cmp al, 2h
				jnz @@Con3
				loop @@CountDRM
				
				jmp @@Ipus
				
	@@Con3:		cmp al, 3h
				jnz @@Con4
				loop @@CountDLM
				
	@@Con4:		jmp @@Sof
	
	@@CountURM:	sub si, BRD_COLS
				add si, type brd				
				dec dl
				inc dh				
				jmp @@Con
	@@CountULM:	sub si, BRD_COLS
				sub si, type brd
				dec dl
				dec dh
				jmp @@Con
	@@CountDRM:	add si, BRD_COLS
				add si, type brd
				inc dl
				inc dh
				jmp @@Con
	@@CountDLM:	add si, BRD_COLS
				sub si, type brd
				inc dl
				dec dh
				
	@@Con:		push dx			;save dx
				push 0h dx
				call IsBorderCrossByPoints			
				pop dx
				cmp dx, 1h					;Checking Border Cross
				pop dx			;load dx
				jz @@Apes		;jmp if cross
				push cx			;save cx
				mov cl, [si]
				cmp cl, '0'
				pop cx			;load cx
				jnz @@ChkEat
				jmp @@WhrLoop
				
	@@ChkEat:	push 0h si di
				call IsABEnemys
				pop dx
				cmp dx, 1h
				jnz @@Apes
				
				mov cl, [si]
				cmp cl, 'b'
				jz @@Threat
				cmp cl, 'B'
				jz @@Threat
				cmp cl, 'q'
				jz @@Threat
				cmp cl, 'Q'
				jz @@Threat
				
	@@Apes:		jmp @@Ipus
	
	@@Threat:	mov [bp+6], 1h
	@@Sof:		pop di si dx cx bx ax bp
				ret 2
CheckBishopThreat	Endp

CheckRookThreat	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]		;king adress
				mov di, si			;save original di=First Location
				xor cx, cx
				xor ax, ax			;where to loop counter
				dec al				;for first move
				
	@@Ipus:		inc al
				mov cl, 7h	
				inc cl				;for first move
				mov si, di			
				push 0h si
				call GetIndexPointsByEA
				pop dx				;dl = y, dh = x
	
	@@WhrLoop:	cmp al, 0h
				jnz @@Con1
				loop @@CountRM
				
				jmp @@Ipus
				
	@@Con1:		cmp al, 1h
				jnz @@Con2
				loop @@CountLM
				
				jmp @@Ipus
				
	@@Con2:		cmp al, 2h
				jnz @@Con3
				loop @@CountUM
				
				jmp @@Ipus
				
	@@Con3:		cmp al, 3h
				jnz @@Con4
				loop @@CountDM
				
	@@Con4:		jmp @@Sof
	
	@@CountRM:	add si, type brd	;count right move
				inc dh
				jmp @@Con
	@@CountLM:	sub si, type brd
				dec dh
				jmp @@Con
	@@CountUM:	sub si, BRD_COLS
				dec dl
				jmp @@Con
	@@CountDM:	add si, BRD_COLS
				inc dl
				
	@@Con:		push dx			;save dx
				push 0h dx
				call IsBorderCrossByPoints			
				pop dx
				cmp dx, 1h					;Checking Border Cross
				pop dx			;load dx
				jz @@Apes		;jmp if cross
				push cx			;save cx
				mov cl, [si]
				cmp cl, '0'
				pop cx			;load cx
				jnz @@ChkEat
				jmp @@WhrLoop
				
	@@ChkEat:	push 0h si di
				call IsABEnemys
				pop dx
				cmp dx, 1h
				jnz @@Apes
				
				mov cl, [si]
				cmp cl, 'r'
				jz @@Threat
				cmp cl, 'R'
				jz @@Threat
				cmp cl, 'q'
				jz @@Threat
				cmp cl, 'Q'
				jz @@Threat
				
	@@Apes:		jmp @@Ipus
	
	@@Threat:	mov [bp+6], 1h
	@@Sof:		pop di si dx cx bx ax bp
				ret 2
CheckRookThreat	Endp

CheckPawnThreat	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov di, [bp+4]			;King adress
				mov bl, [bp+6]			;who chess to check
				cmp bl, 'A'
				jz @@FromDown
				jmp @@FromUp
				
	@@FromDown:	sub di, BRD_COLS
				sub di, type brd
				push ax 			;save ax
				mov al, 'p'
				cmp [di], al
				pop ax				;load ax
				jnz @@ChkRight
				jmp @@Threat
	@@ChkRight: add di, type brd*2
				push ax 			;save ax
				mov al, 'p'
				cmp [di], al
				pop ax				;load ax
				jnz @@NoThreat
				jmp @@Threat
	
	@@FromUp:	add di, BRD_COLS
				sub di, type brd
				push ax 			;save ax
				mov al, 'P'
				cmp [di], al
				pop ax				;load ax
				jnz @@ChkR			
				jmp @@Threat
	@@ChkR: 	add di, type brd*2
				push ax 			;save ax
				mov al, 'P'
				cmp [di], al
				pop ax				;load ax
				jnz @@NoThreat
				
	@@Threat:	mov [bp+8], 1h
	@@NoThreat:	pop di si dx cx bx ax bp
				ret 4
CheckPawnThreat	EndP

CheckQueenProtect	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				xor ax, ax
				push xTurn[0]
				pop bx
				push 0h bx offset brd
				call QueenLoc
				pop si
				
				push 0h si xTurn[0]
				call IsToolIsThreat
				pop ax
				
				mov [bp+4], al
				pop di si dx cx bx ax bp
				ret 
CheckQueenProtect	Endp

CheckChess	Proc	
				push bp
				mov bp, sp
				push ax bx cx dx si di
				xor bx, bx				
				mov bl, [bp+4]		;xTurn -> who chess to check
				push 0h bx offset brd
				call KingLoc
				pop si
				
				push 0h bx si
				call CheckPawnThreat
				pop ax
				cmp al, 1h
				jnz @@ConToR
				jmp	@@Sof
	
	@@ConToR:	push 0h si
				call CheckRookThreat	;Check Queen Too
				pop ax
				cmp al, 1h
				jnz @@ConToB
				jmp	@@Sof
	
	@@ConToB:	push 0h si
				call CheckBishopThreat	;Check Queen Too
				pop ax
				cmp al, 1h
				jnz @@ConToH
				jmp @@Sof
				
	@@ConToH:	push 0h si
				call CheckHorseThreat
				pop ax
				cmp al, 1h
				jnz @@ConToK
				jmp @@Sof
	
	@@ConToK:	push 0h si
				call CheckKingThreat
				pop ax
				
	@@Sof:		mov [bp+6], al
				pop di si dx cx bx ax bp
				ret 2
				
CheckChess	EndP

IsToolIsThreat	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				xor bx, bx
				xor ax, ax
				mov bl, [bp+4]	;xTurn[0] -> turn to check if threat
				mov si, [bp+6]	;offset tool to check threat
								
				push 0h bx si
				call CheckPawnThreat
				pop ax
				cmp al, 1h
				jnz @@ConToR
				jmp	@@Sof
	
	@@ConToR:	push 0h si
				call CheckRookThreat	;Check Queen Too
				pop ax
				cmp al, 1h
				jnz @@ConToB
				jmp	@@Sof
	
	@@ConToB:	push 0h si
				call CheckBishopThreat	;Check Queen Too
				pop ax
				cmp al, 1h
				jnz @@ConToH
				jmp @@Sof
				
	@@ConToH:	push 0h si
				call CheckHorseThreat
				pop ax
				cmp al, 1h
				jnz @@ConToK
				jmp @@Sof
	
	@@ConToK:	push 0h si
				call CheckKingThreat
				pop ax
	
	@@Sof:		mov [bp+8], al
				pop di si dx cx bx ax bp
				ret 4
IsToolIsThreat	Endp

RankTheMoves	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov di, [bp+4]	;offset rnk
				mov si, [bp+6]	;offset brd		
				xor cx, cx
				
				;check chess mode
				push ax					;save ax
				push 0h xTurn[0]		;Check If Enemy Chess
				call CheckChess
				pop ax
				cmp al, 1h
				pop ax					;load ax
				jnz @@CheckQProt
				mov cl, 1h				;chess mode on
				
				;check queen protect
	@@CheckQProt:
				push ax 			;save ax
				push 0h
				call CheckQueenProtect
				pop ax
				cmp al, 1h
				pop ax 				;load ax
				jnz @@NextRnk
				mov cl, 2h				;Q protect mode on
				
	@@NextRnk:	cmp [di].RootAdrs, 0h
				jnz @@Con1
				jmp @@Sof
	@@Con1:		mov bx, [di].RootAdrs
				mov al, [bx]			;save original
				mov dl, '0'
				mov [bx], dl
				mov bx, [di].DestAdrs
				mov ah, [bx]			;save original
				mov [bx], al
				
				cmp cl, 1h
				jz @@ChessMode
				
				cmp cl, 2h
				jz @@QProtect
				
				push 0h si
				call CountAvailableMoves
				pop bx
				
				push ax					;save ax
				push 0h xTurn[2]		;Check If Enemy Chess
				call CheckChess
				pop ax
				cmp al, 1h			
				pop ax					;load ax
				jnz @@Con
				mov bx, 0fff0h			;very good move ! 
				
				
				;check if tool will be threat!
	@@Con:		push ax si					;save ax si
				mov si, [di].DestAdrs
				push 0h si xTurn[0]
				call IsToolIsThreat
				pop ax
				cmp al, 1h
				pop si ax					;load ax si
				jnz @@Con2
				mov bx, 0000h
				jmp @@Con2
				
	@@ChessMode:		;check if king protect is neccesery
				push ax					;save ax
				push 0h xTurn[0]		;Check If Computer Chess
				call CheckChess
				pop ax
				cmp al, 0h				
				pop ax					;load ax
				jnz @@Con2
				mov bx, 0ffffh
				jmp @@Con2
	
	@@QProtect:	push ax 			;save ax
				push 0h
				call CheckQueenProtect
				pop ax
				cmp al, 0h
				pop ax 				;load ax
				jnz @@Con2
				mov bx, 0fff1h				
	
	@@Con2:		mov [di].moveRank, bx
				
				mov bx, [di].RootAdrs
				mov [bx], al
				mov bx, [di].DestAdrs
				mov [bx], ah			;load original
				
				add di, type rnk
				jmp @@NextRnk
				
	@@Sof:		pop di si dx cx bx ax bp
				ret 4
RankTheMoves	Endp

SetBoardAsChosenRank	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov di, [bp+4]		;highets rank
				mov bx, [di].RootAdrs
				mov al, [bx]
				mov ah, '0'
				mov [bx], ah
				mov bx, [di].DestAdrs
				mov [bx], al				
				pop  di si dx cx bx ax bp
				ret 2
SetBoardAsChosenRank	Endp

ComputerTurn	Proc
				push bp
				mov bp, sp
				push ax bx cx dx si di
				mov si, [bp+4]		;offset brd
				mov di, [bp+6]		;offset rnk
				mov bl, [bp+8]		;xTurn[0]
				call ClearRnk
				push offset brd offset rnk
				call DoAllPossibleMoves
				push offset brd offset rnk
				call RankTheMoves
				push 0h offset rnk
				call FindHigherMoveRank
				pop di
				push di
				call SetBoardAsChosenRank
				push offset brd
				call DrawTools
				
				
				
				; mov ax, [si]
				; push 0h
				; push 0060h
				; push ax 
				; push 0007bh
				; call IfxBetweenAB
				; pop cx
				; cmp cx, 1h
				; jnz @@ConToP
				; sub ax, 20h
	; @@ConToP:	cmp al, 'P'
				; jnz @@ConToR
				; jmp @@PStyle
	; @@ConToR:	cmp al, 'R'
				; jnz @@ConToH
				; jmp @@RStyle
	; @@ConToH:	cmp al, 'H'
				; jnz @@ConToB
				; jmp @@HStyle
	; @@ConToB:	cmp al, 'B'
				; jnz @@ConToK
				; jmp @@BStyle
	; @@ConToK:	cmp al, 'K'
				; jnz @@ConToQ
				; jmp @@KStyle
	; @@ConToQ:	jmp @@QStyle
				
	; @@PStyle:	;push si di xTurn[0]
				; ;call PawnStylePC
	; @@RStyle:	
	; @@HStyle:	
	; @@BStyle:	
	; @@KStyle:	
	; @@QStyle:
				pop di si dx cx bx ax bp
				ret 6
ComputerTurn	EndP

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




		Start:	mov ax,dseg
				mov ds, ax
				mov es, ax
				
				call ClrScr	
				push offset buffer1	offset openScr 
				call Kitov
				mov ah, 7h 
				int 21h
				call LoadFirstFiles
				
	@Menu:		call ClrScr
				push 0h
				call MenuF
				pop ax
				;plr vs comp
				cmp al, 0h
				jnz @ConTo1
				
				call GetTnD
				push ax cx dx
				mov si, offset tbl
				mov cx, countStr
				mov al, type tbl
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
				
				call ClrScr
				push 0h
				call SelectColorMenu
				pop cmpOrPlr
				jmp @Game
				
	@ConTo1:	cmp al, 1h
				jnz @ConTo2
				call ClrScr
				call GetTnD
				push ax cx dx
				mov si, offset tbl
				mov cx, countStr
				mov al, type tbl
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
				call ClrScr
				
				jmp @Game
				
	@ConTo2:	cmp al, 2h
				jnz @ConTo3
				
	@ConTo3:	cmp al, 3h
				jnz @ConTo4
				call ClrScr
				push KELET_LEN offset keletArr
				call MionKelet
				push countStr offset tbl				
				call RecordTable
				jmp @Menu
				
	@ConTo4:	cmp al, 4h
				
				
	@Game:		call DrawBoard
				push offset brd
				call DrawTools	
				push cmpOrPlr
				call PlayChess
				
				push xTurn[0]
				pop ax
				cmp al, 'A'
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
				jmp @Sof
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
				
				call GetTnD
				push ax cx dx 
				mov si, offset tbl
				mov cx, countStr
				mov al, type tbl
				mul cl
				add si, ax
				lea dx, [si].DNTE
				push 20d offset tempDNT dx
				call DNTPusher
				pop dx cx ax
				
				mov cx, countStr
				inc cx
				push cx
				pop countStr	
				
				push ax cx								
				mov ax, TBL_LEN
				mul cx
				pop cx
				push 0 ; stam
				push 0 offset fNameStr
				call OpenNewFile
				pop fPStr
				push fPStr ax offset TBL
				call WriteToFile
				pop ax			
				
		@Sof:	int 3h

cseg ends
end Start
