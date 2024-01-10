INCLUDE Irvine32.inc
INCLUDE Macros.inc

;-------------------------------------- PROTOTYPES OF FUNCTIONS

AppendingDataFile PROTO ,ptrMsg:PTR BYTE,ptrFileName:PTR BYTE,lenOfMsg:DWORD
Append PROTO ,ptrFileName:PTR BYTE,ptrMsg1:PTR BYTE,lenOfMsg1:DWORD,ptrMsg1:PTR BYTE,lenOfMsg2:DWORD
ConvertStringToNumber PROTO ,ptrString:PTR BYTE
IntToStr PROTO ,num:DWORD,ptrBuffer:PTR BYTE
CountDigits PROTO ,num:DWORD

SetCordinates PROTO ,X:BYTE,Y:BYTE

	Buffer_size = 500
.data
	ACC_NUMBER BYTE "12345678",0
	ACC_LENGTH DWORD 8

	PASSWORD byte "12345",0
	PASSWORD_LENGTH DWORD 5
	
	USERNAME byte "Shaheer",0
	USERNAME_LENGTH DWORD 5
	
	ACC_PIN BYTE "2468",0
	ACC_BALANCE DWORD 5000
	ACC_balanceString BYTE 100 DUP (?),0
	
	bufferusername BYTE Buffer_Size DUP (?)
	usernamesize DWORD 0
	bufferpassword BYTE Buffer_Size DUP (?)
	bufferAcc_no BYTE 20 DUP (?)
	bufferPIN BYTE 5 DUP (?)
	R_bufferAcc_no BYTE 20 DUP (?)
	
	filename BYTE "user.txt",0
	balancefile BYTE "balance.txt",0
	balancefileHandle HANDLE ?
	filehandle HANDLE ?
	stringlength DWORD ?
	lengthofinput DWORD ?
	temp_p byte 255 DUP(?)

	msg1 BYTE "Do yo want to Confirm the payement?",0
	msg2 BYTE "   ----  PAYMENT CONFIRMATION  ----  ",0
	newline BYTE 0Ah

.code
main PROC
	
	call clrscr
	;call WriteBalance
	call ReadBalance

	INVOKE SetCordinates ,24,6
	mwrite "      ------------- Welcome To Meezan Bank ATM System -------------           "
	call crlf
	call crlf
	call crlf
	mwrite "					        Press >> 1 to Log-in                       "
	call crlf
	mwrite "					        Press >> 2 to exit                         "
	call crlf
	mWrite "			           ______________________________________________"
	call crlf
	call crlf
	mWrite"					    Choice:    "
	call ReadDec
	.IF(EAX==1)
		CALL Login
	.ELSEIF(EAX==2)
		jmp quit
	.ELSE
		JMP quit2
	.ENDIF

	quit:
		INVOKE SetCordinates ,24,18
		mwrite "                   Thank You For Choosing Us.                  "
		INVOKE SetCordinates ,24,24
		call WaitMsg
		call clrscr
		jmp EXITT

	quit2:
		INVOKE SetCordinates ,24,18
		mwrite "                   Invalid Option Entered                 "
		INVOKE SetCordinates ,24,24
		call WaitMsg
	EXITT:
	exit
main ENDP

;---------------------------------------------- LOGIN FUNCTION ----------------------------------------------

Login PROC
	
	call clrscr
	INVOKE SetCordinates ,26,6
	mWrite"---------------------- ENTER DETAILS ------------------"
	call crlf
	call crlf
	call crlf
	mwrite "                            Enter Your Username      :    "
	mov edx,OFFSET bufferusername
	mov ecx,SIZEOF bufferusername
	call readstring
	mov usernamesize,eax
	call crlf
	mwrite "                            Enter Your Password      :    "
	mov edx,OFFSET bufferpassword
	mov ecx,SIZEOF bufferpassword
	call readstring

	INVOKE Str_compare,ADDR bufferpassword,ADDR PASSWORD
	Jne retu                                              ; if comparision is false jump to RETU

	call crlf
	call crlf
	INVOKE SetCordinates ,24,15
	call waitMsg
	call clrscr

	;INVOKE AppendingDataFile ,ADDR bufferusername,ADDR fileName,SIZEOF bufferusername
	;INVOKE AppendingDataFile ,ADDR newline,ADDR fileName,SIZEOF newline
	;INVOKE AppendingDataFile ,ADDR PASSWORD,ADDR fileName,SIZEOF PASSWORD

	INVOKE Append ,ADDR fileName,ADDR bufferusername,usernamesize,ADDR PASSWORD,SIZEOF PASSWORD
	call MENU

	jmp QuitLogin
	
	retu:
		call crlf
		INVOKE SetCordinates ,24,16
		mwrite "Password Donot Match Try Again."
		call crlf
	
	QuitLogin:
ret
Login ENDP

;------------------------------------------------- MAIN MENU FUNCTION -----------------------------------------

MENU PROC
	LOCAL choice:DWORD

	call clrscr
	INVOKE SetCordinates ,70,6
	call crlf
	mwrite  "                                  Please Choose an Option To Proceed With."
	call crlf
	call crlf
	mwrite "                                      Press >> 1 To With-Draw                       "
	call crlf
	mwrite "                                      Press >> 2 To Transfer                       "
	call crlf
	mwrite "                                      Press >> 3 To Check Balance                       "
	call crlf
	mwrite "                                      Press >> 4 To Deposit Money                       "
	call crlf
	mwrite "                                      Press >> 5 To Pay Bills                       "
	call crlf
	mwrite "                                      Press >> 6 Currency Exchange              "
	call crlf
	mwrite "                                      Press >> 7 To Exit                       "
	call crlf
	mWrite "                              __________________________________________"
	call crlf
	call crlf
	mWrite"                              Choice:    "
	call ReadDec
	mov choice,eax
	
	cmp choice,1
	je WithDraww

	cmp eax,2
	je Transferr

	cmp eax,3
	je CheckBalancee

	cmp eax,4
	je Deposit

	cmp eax,5
	je Paybill

	cmp eax,6
	je currency

	cmp eax,7
	je QuitMenu

	jmp return  ; for invalid Key press
	
	Withdraww:
		call WithDraw
		call Menu

	Transferr:
		call Transfer
		call Menu

	CheckBalancee:
		call CheckBalance
		call Menu
	
	Deposit:
		call DepositMoney
		call Menu

	Paybill:
		call PayBills
		call Menu

	checklimit:
		call CheckLimit
		call Menu

	currency:
		call ExchangeCurrency
		call Menu
	
	return:
	mwrite "   Wrong Option Selected "

	QuitMenu:

		mwrite "                                    Thank You For Choosing Us.                  "
		INVOKE SetCordinates ,24,24
		call WaitMsg
		call clrscr
	exit
ret
MENU ENDP
	
;------------------------------------------------- WITHDRAW FUNCTION -----------------------------------------

WithDraw PROC

	call clrscr 
	call ReadBalance

	INVOKE SetCordinates ,24,6
	mwrite "      ------------- WITHDRAW FUNDS -------------           "
	call crlf
	
	INVOKE SetCordinates ,24,8
	mWrite" Enter Your Account Number: "
	mov edx,OFFSET bufferAcc_no
	mov ecx,20
	call ReadString
	
	INVOKE Str_compare,ADDR ACC_NUMBER,ADDR bufferAcc_no
	jne INVALID

	call crlf
	call crlf
	INVOKE SetCordinates ,24,10
	mWrite" Enter Your Account PIN: "
	mov edx,OFFSET bufferPIN
	mov ecx,5
	call ReadString
	
	INVOKE Str_compare,ADDR ACC_PIN,ADDR bufferPIN
	jne INVALID

	INVOKE SetCordinates ,24,13
	mWrite"-- Enter the Amount to withdraw: "
	call ReadDec

	push eax

	mov edx,OFFSET msg1
	mov ebx,OFFSET msg2
	call MsgBoxAsk

	.IF(eax == 6)
		pop eax 
		.IF( eax < ACC_BALANCE )
		
			sub ACC_BALANCE,eax
			INVOKE SetCordinates ,24,16
			mWrite"FUNDS WITHDRAW DETAILS ----------------------------"
			call crlf
			INVOKE SetCordinates ,24,18
			mWrite"Withdraw Amount: "
			call WriteDec
			call crlf
			INVOKE SetCordinates ,24,20
			mWrite"Current Balance: "
			mov eax,ACC_BALANCE
			call WriteDec
			call crlf
			INVOKE SetCordinates ,24,24
			call WaitMsg
	
		.ELSE
			INVOKE SetCordinates ,24,16
			mWrite"Cannot Withdraw Amount as your Current Balance is Low! "
			call crlf
			INVOKE SetCordinates ,24,19
			call WaitMsg
		
		.ENDIF

	.ENDIF

	jmp QuitWithdraw
	
	INVALID:
		INVOKE SetCordinates ,24,13
		mWrite"                Your Details donot Match! "
		call Crlf
		call crlf
		INVOKE SetCordinates ,24,17
		call WaitMsg

	QuitWithDraw:
	call WriteBalance
	ret
WithDraw ENDP


;------------------------------------------------- TRANSFER MONEY FUNCTION -----------------------------------------

Transfer PROC
	LOCAL len:DWORD
	call clrscr 

	call ReadBalance

	INVOKE SetCordinates ,24,6
	mwrite "      ------------- TRANSFER FUNDS -------------           "
	call crlf
	
	INVOKE SetCordinates ,24,8
	mWrite" Enter Recipents Account Number: "
	mov edx,OFFSET R_bufferAcc_no
	mov ecx,20
	call ReadString
	
	INVOKE Str_length ,ADDR R_bufferAcc_no
	mov len,eax

	call crlf
	call crlf
	INVOKE SetCordinates ,24,10
	mWrite" Enter Your Account PIN: "
	mov edx,OFFSET bufferPIN
	mov ecx,5
	call ReadString
	
	

	mov eax,len										; restoring length of recipents account number to eax
										
	INVOKE Str_compare,ADDR ACC_PIN,ADDR bufferPIN	; Agr Zero Flag=1 hai iska matlab dono strings same hai
	jne INVALID

	.IF( eax == 8)						
			
		INVOKE SetCordinates ,24,13
		mWrite"-- Enter the Amount to Transfer: "
		call ReadDec

		push eax									; saving ammount to be transfered in stack

		mov edx,OFFSET msg1
		mov ebx,OFFSET msg2
		call MsgBoxAsk

		.IF(eax==6)

			pop eax

			.IF( eax < ACC_BALANCE )
		
				sub ACC_BALANCE,eax
				INVOKE SetCordinates ,24,16
				mWrite"FUNDS TRANSFER DETAILS ----------------------------"
				call crlf
				INVOKE SetCordinates ,24,18
				mwrite"Recipent Account # "
				mov edx,OFFSET R_bufferAcc_no
				call WriteString
				INVOKE SetCordinates ,24,20
				mWrite"TRANSFER Amount: "
				call WriteDec
				call crlf
				INVOKE SetCordinates ,24,22
				mWrite"Current Balance: "
				mov eax,ACC_BALANCE
				call WriteDec
				call crlf
				INVOKE SetCordinates ,24,26
				call WaitMsg
	
			.ELSE
				INVOKE SetCordinates ,24,16
				mWrite"Cannot Withdraw Amount as your Current Balance is Low! "
				call crlf
				INVOKE SetCordinates ,24,19
				call WaitMsg
		
			.ENDIF

		.ELSE	

			jne INVALID
		.ENDIF
	.ENDIF

	jmp QuitTransfer

	INVALID:
		INVOKE SetCordinates ,24,13
		mWrite"                Your Details donot Match! "
		call Crlf
		call crlf
		INVOKE SetCordinates ,24,17
		call WaitMsg
	jmp QuitTransfer
	
	QuitTransfer:

	call WriteBalance

	ret
Transfer ENDP


;------------------------------------------------- CHECK BALANCE FUNCTION -----------------------------------------

CheckBalance PROC

	call clrscr
	call ReadBalance

	INVOKE SetCordinates ,24,6
	mwrite "      ------------- ACCOUNT DETAILS -------------           "
	call crlf
	INVOKE SetCordinates ,24,8
	mWrite"            Account # "
	mov edx,OFFSET ACC_NUMBER
	mov ecx,SIZEOF ACC_NUMBER
	call WriteString
	INVOKE SetCordinates ,24,10
	mWrite"            Current Balance :"
	mov eax,ACC_BALANCE
	call WriteDec

	INVOKE SetCordinates ,24,14
	call WaitMsg
	ret
CheckBalance ENDP

;------------------------------------------------- DEPOSIT MONEY FUNCTION -----------------------------------------

DepositMoney PROC

	call clrscr
	call ReadBalance

	INVOKE SetCordinates ,24,6
	mwrite "      ------------- DEPOSIT MONEY -------------           "
	call crlf
	INVOKE SetCordinates ,24,8
	mWrite"Enter Amount to Deposit: "
	call ReadDec

	push eax									; saving ammount to be transfered in stack

	mov edx,OFFSET msg1
	mov ebx,OFFSET msg2
	call MsgBoxAsk

	.IF(eax==6)

		pop eax
		add ACC_BALANCE,eax
	
	.ENDIF
	
	INVOKE SetCordinates ,24,12
	mwrite "      ------------- FUNDS DETAILS -------------           "
	call crlf
	INVOKE SetCordinates ,24,14
	mWrite"            Account # "
	mov edx,OFFSET ACC_NUMBER
	mov ecx,SIZEOF ACC_NUMBER
	call WriteString
	INVOKE SetCordinates ,24,16
	mWrite"            Current Balance :"
	mov eax,ACC_BALANCE
	call WriteDec

	call WriteBalance

	INVOKE SetCordinates ,24,20
	call WaitMsg	
	ret
DepositMoney ENDP


;------------------------------------------------- PAY BILLS FUNCTION -----------------------------------------

PayBills PROC
	
	call clrscr
	call ReadBalance

	INVOKE SetCordinates ,24,6
	mwrite "      ------------- PAY BILLS -------------           "
	call crlf
	INVOKE SetCordinates ,24,8
	mWrite"Enter Account Number of Bill : "
	call ReadDec
	INVOKE SetCordinates ,24,10
	mWrite"Enter Amount to Pay : "
	call ReadDec

	push eax									; saving ammount to be transfered in stack

	mov edx,OFFSET msg1
	mov ebx,OFFSET msg2
	call MsgBoxAsk

	.IF(eax==6)

		pop eax
	
		.IF( eax < ACC_BALANCE )
			sub ACC_BALANCE ,eax
			INVOKE SetCordinates ,24,14
			mWrite"PAY DETAILS ----------------------------"
			call crlf
			INVOKE SetCordinates ,24,16
			mWrite"PAYED Amount: "
			call WriteDec
			call crlf
			INVOKE SetCordinates ,24,18
			mWrite"Current Balance: "
			mov eax,ACC_BALANCE
			call WriteDec
			call crlf
			INVOKE SetCordinates ,24,22
			call WaitMsg
	
		.ELSE
			INVOKE SetCordinates ,24,14
			mWrite"Cannot Pay the Bill as your Current Balance is Low! "
			call crlf
			INVOKE SetCordinates ,24,17
			call WaitMsg
		.ENDIF

	.ENDIF
	
	call WriteBalance

	ret
PayBills ENDP


;------------------------------------------------- Currency Exchange FUNCTION -----------------------------------------
ExchangeCurrency PROC
	
	Local val: DWORD
	call clrscr
	call ReadBalance

	INVOKE SetCordinates ,70,6
	call crlf
	mwrite  "                                  Please Choose an Option To Proceed With."
	call crlf
	call crlf
	mwrite "                                      Press >> 1 Convert To USD $                     "
	call crlf
	mwrite "                                      Press >> 2 Convert To Canadian $                     "
	call crlf
	mwrite "                                      Press >> 3 Convert To Euro €                     "
	call crlf
	mwrite "                                      Press >> 4 Convert To Riyal                    "
	call crlf
	mwrite "                                      Press >> 5 To Exit                       "
	call crlf
	mWrite "                              __________________________________________"
	call crlf
	call crlf
	mWrite"                              Choice:    "
	call ReadDec
	mov val,eax

	cmp val,1
	je usd

	cmp eax,2
	je cad

	cmp eax,3
	je eru

	cmp eax,4
	je sr

	cmp eax,5
	je retur

	jmp inval

	usd:
	call USDconvert
	jmp retur

	cad:
	call CADconvert
	jmp retur

	eru:
	call ERUconvert
	jmp retur

	sr:
	call SARconvert
	jmp retur

	inval:
	mwrite "Invalid Option Choosen"

	retur:
	ret
ExchangeCurrency ENDP

;------------------------------------------------- USD EXCHANGE FUNCTION -----------------------------------------
;will divide the account balance by the usd rate and store the data in the balance

USDconvert PROC
	LOCAL usdequal  :DWORD

	call clrscr 
	call ReadBalance

	INVOKE SetCordinates ,24,6
	mwrite "      ------------- USD DOLLAR CONVERTOR -------------           "
	call crlf
	mov edx,0
	mov eax,ACC_BALANCE
	mov ebx,285
	div ebx
	mov usdequal, eax
	INVOKE SetCordinates ,24,8
	mwrite "The Balance IN UDS Is:  "
	mov eax,usdequal
	call writedec
	call crlf
	INVOKE SetCordinates ,24,12
	call WaitMsg
	ret 
USDconvert ENDP

;------------------------------------------------- CANADIAN EXCHANGE FUNCTION -----------------------------------------
;will divide the account balance by the canadian rate and store the data in the balance

CADconvert PROC
	LOCAL cadequal  :DWORD
	call clrscr 
	call ReadBalance

	INVOKE SetCordinates ,24,6
	mwrite "      ------------- CAD DOLLAR CONVERTOR -------------           "
	call crlf
	mov edx,0
	mov eax,ACC_BALANCE
	mov ebx,207
	div ebx
	mov cadequal, eax
	INVOKE SetCordinates ,24,8
	mwrite "The Balance IN CAD Is:  "
	mov eax,cadequal
	call writedec
	call crlf
	INVOKE SetCordinates ,24,12
	call WaitMsg

	ret 
CADconvert ENDP

;------------------------------------------------- EURO EXCHANGE FUNCTION -----------------------------------------
;will divide the account balance by the euro rate and store the data in the balance

ERUconvert PROC
	LOCAL eurequal  :DWORD
	call clrscr 
	call ReadBalance

	INVOKE SetCordinates ,24,6
	mwrite "      ------------- CAD DOLLAR CONVERTOR -------------           "
	call crlf
	mov edx,0
	mov eax,ACC_BALANCE
	mov ebx,311
	div ebx
	mov eurequal, eax
	INVOKE SetCordinates ,24,8
	mwrite "The Balance IN EURO Is:  "
	mov eax,eurequal
	call writedec
	call crlf
	INVOKE SetCordinates ,24,12
	call WaitMsg

ret 
ERUconvert ENDP

;------------------------------------------------- RIYAL EXCHANGE FUNCTION -----------------------------------------
;will divide the account balance by the rial rate and store the data in the balance

SARconvert PROC
	LOCAL riyalequal  :DWORD
	
	call clrscr 
	call ReadBalance

	INVOKE SetCordinates ,24,6
	mwrite "      ------------- RIYAL DOLLAR CONVERTOR -------------           "
	call crlf
	mov edx,0
	mov eax,ACC_BALANCE
	mov ebx,76
	div ebx
	mov RIYALequal, eax
	INVOKE SetCordinates ,24,8
	mwrite "The Balance IN RIYAL Is:  "
	mov eax,riyalequal
	call writedec
	call crlf
	INVOKE SetCordinates ,24,12
	call WaitMsg

	ret 
SARconvert ENDP
;------------------------------------------------- SETTING CORDINATES FUNCTION -----------------------------------------
; Recieves X and Y cordinates

SetCordinates PROC ,X:BYTE,Y:BYTE

	mov dl,X
	mov dh,Y

	call gotoxy

	ret
SetCordinates ENDP

;------------------------------------------- APPENDING DATA IN THE FILE FUNCTION ----------------------------------------
; Recieves Text message , File Name , Length of the Message

AppendingDataFile PROC USES  eax ,ptrMsg:PTR BYTE,ptrFileName:PTR BYTE,lenOfMsg:DWORD

	
	push 0							; template file
	push FILE_ATTRIBUTE_NORMAL
	push OPEN_ALWAYS
	push 0							;security attribute
	push 0							; share node
	push FILE_SHARE_WRITE
	push ptrFileName
	call CreateFileA
	mov fileHandle,eax

	INVOKE SetFilePointer, fileHandle, 0, 0, FILE_END   ; ye mene file pointer ko end mei le gaya taake mei append
														; kr sakoon

	push 0					;overlap structure
	push 0					; recieve the num bytes written
	push LenOfMsg
	push ptrMsg
	push fileHandle
	call WriteFile

	mov eax,fileHandle
	call CloseFile

	ret
AppendingDataFile ENDP


Append PROC USES  eax ,ptrFileName:PTR BYTE,ptrMsg1:PTR BYTE,lenOfMsg1:DWORD,ptrMsg2:PTR BYTE,lenOfMsg2:DWORD



	mov edx,ptrFilename
	call CreateOutputFile
	mov fileHandle,eax

	mov edx,ptrMsg1
	mov ecx,lenOfMsg1
	mov eax,fileHandle
	call WriteToFile

	mov edx,OFFSET newline
	mov ecx,1
	mov eax,fileHandle
	call WriteToFile

	mov edx,ptrMsg2
	mov ecx,lenOfMsg2
	mov eax,fileHandle
	call WriteToFile

	ret
Append ENDP



ConvertStringToNumber PROC ,ptrString:PTR BYTE

    mov esi,ptrString
    mov eax,0
    mov ebx,0

    beginWhile:
        movzx eax, byte ptr [esi] 
        cmp  eax, 0               
        je   EndWhile          
        
				
		cmp eax, 32
		je EndWhile

        cmp  eax, '0'
        jl   InvalidInput             ; If less than '0', it's not a digit
        cmp  eax, '9'
        jg   InvalidInput             ; If greater than '9', it's not a digit

        sub  eax, '0'             
        imul ebx, ebx, 10              
        add  ebx, eax        
        

        inc esi
        jmp beginWhile

        
    EndWhile:
		mov eax,ebx
        jmp endd

    InvalidInput:
        mWrite"Invalid Input"

    endd:
        ret
ConvertStringToNumber ENDP

CountDigits PROC num:DWORD

	mov eax, num             ; Example: Replace this with your number
	mov ecx, 0            ; Initialize digit count to zero

	countDigitsLoop:
		mov edx,0
		cmp eax, 0            ; Check if the number is zero
		je  endCount          ; If it is, exit the loop

		inc ecx               ; Increment digit count
		mov ebx, 10           ; Set divisor to 10
		div ebx               ; Divide eax by 10, result in eax, remainder in edx
		jmp countDigitsLoop  ; Repeat the loop

	endCount:
		mov eax,ecx

	ret
CountDigits ENDP


IntToStr PROC ,num:DWORD,ptrBuffer:PTR BYTE 
	

	mov edi,ptrBuffer
	INVOKE CountDigits ,num
	add edi,eax					; eax contains number of digits
	
    mov ecx, 10                ; Set divisor to 10
	mov eax,num

	reverseLoop:
		xor edx, edx               ; Clear any previous remainder
		div ecx                    ; Divide EAX by 10, result in EAX, remainder in EDX

		add dl, '0'                ; Convert remainder to ASCII
		dec edi                    ; Move buffer pointer backwards
		mov [edi], dl              ; Store ASCII character in the buffer

		test eax, eax              ; Check if quotient is zero
		jnz reverseLoop           ; If not, continue the loop

    ret
IntToStr ENDP

WriteBalance PROC USES edx eax
	
	INVOKE IntToStr ,ACC_BALANCE,OFFSET ACC_balanceString
	mov edx,OFFSET balancefile
	call CreateOutputFile
	mov balancefileHandle,eax
	
	mov edx,OFFSET ACC_balanceString
	mov ecx,SIZEOF ACC_balanceString
	mov eax,balancefileHandle
	call WriteToFile

	mov eax,balancefileHandle
	call CloseFile

	ret
WriteBalance ENDP

ReadBalance PROC USES edx eax

		
	mov edx,OFFSET balancefile
	call OpenInputFile
	mov balancefileHandle,eax

	mov edx,OFFSET ACC_balanceString
	mov ecx,SIZEOF ACC_balanceString
	mov eax,balancefileHandle
	call ReadFromFile

	mov edx,OFFSET ACC_balanceString

	
	INVOKE ConvertStringToNumber ,OFFSET ACC_balanceString
	mov ACC_BALANCE,eax

	mov eax,balancefileHandle
	call CloseFile

	ret
ReadBalance ENDP

END main