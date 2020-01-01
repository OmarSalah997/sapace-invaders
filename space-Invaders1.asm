include status.inc   
include clearpowerup.inc 
include drawSPACE.INC
include drawFirst.inc
include drawSecond.inc 
include clearFirstPlayerUP.inc
include clearSecondPlayerDOWN.inc 
include clearFirstPlayerDOWN.inc
include clearSecondPlayerUP.inc
include clearbullet.inc
.model small
.data
x  db  4fh   
buf dw 0
buf2 dw 0 
player1  db 15,?,15 dup('$')
player2  db 15,?,15 dup('$')   
welcome  db "please enter 1st player name:","$" 
wel2  db "please enter 2nd player name:","$" 
startmes db "*to start the game press A   ","$"
endmes   db "*to exit the game press ESC","$" 
repeat   db "please enter a valid key!press any key and try again","$" 
invitation db " has sent you game request,to accept press A","$" 
score db " score =","$" 
hlth db  "/health=","$"
status   db "----------------------------------------","$"
p1score  db 00h ,"$" 
p1health db 2h  ,"$"
p2score  db 00h ,"$"
p2health db 2h  ,"$" 
p2win db "wins!! press any key to continue","$"
S dw 0
D dw 0
y dw 0
plus dw 20
color db ?
x1 dw ?
x2 dw ? 
ind  db 1
xb1 dw 306 
minus1 dw 310 ;starting position of bullet for player one
xb2 dw 14 
minus2 dw 10   ;starting position of bullet for player two
bullet1 db 0 
by1 dw 0
bullet2 db 0 
by2 dw 0
flag db 0 

start1 dw 80
start2 dw 80

end1 dw 90
end2 dw 90 


refb1 dw ?
refb2 dw ?
refbullet1 db 0
refy1 dw ?

refb3 dw ?
refb4 dw ?
refbullet2 db 0
refy2 dw ?


.code 
.main proc far 
mov ax,@data
mov ds,ax 
do:    
mov ah,0
mov al,13h
int 10h     
    
mov ah,2          ;Move Cursor
mov dx,0A08h      ;X,Y Position
int 10h


mov ah, 9
mov dx, offset welcome
int 21h                   
     
mov ah,2          ;Move Cursor
mov dx,0C0Ch      ;Y,X Position
int 10h     

mov ah,0AH        ;Read the name from keyboard
mov dx,offset player1                  
int 21h 
 
MOV AX,0600H    ;06 TO SCROLL & 00 FOR FULLJ SCREEN
MOV BH,00H    ;ATTRIBUTE 7 FOR BACKGROUND AND 1 FOR FOREGROUND
MOV CX,0000H    ;STARTING COORDINATES
MOV DX,184FH    ;ENDING COORDINATES
INT 10H        ;FOR VIDEO DISPLAY


mov ah,2
mov dh,0Ah
mov dl,08h          ;Move Cursor 
int 10h


mov ah, 9
mov dx, offset wel2
int 21h                   
     

mov ah,2          ;Move Cursor
mov dx,0C0Ch      ;Y,X Position
int 10h     

mov ah,0AH        ;Read the name from keyboard
mov dx,offset player2                  
int 21h  
;---------------------------------------------------------------
screen1:
MOV AX,0600H    ;06 TO SCROLL & 00 FOR FULLJ SCREEN
MOV BH,00H    ;ATTRIBUTE 7 FOR BACKGROUND AND 1 FOR FOREGROUND
MOV CX,0000H    ;STARTING COORDINATES
MOV DX,184FH    ;ENDING COORDINATES
INT 10H        ;FOR VIDEO DISPLAY 


mov ah,2          ;Move Cursor
mov dh,0ah     ;X,Y Position 
mov dl,0ah
mov bh,0
int 10h


mov ah, 9
mov dx, offset startmes      ;start game=A,exit=esc
int 21h 
     
mov ah,2          ;Move Cursor
mov dh,11h      ;row 
mov dl,0dh       ;column
mov bh,0
int 10h 

mov ah, 9
mov dx, offset endmes
int 21h               

mov ah,07         ;Read one char and put in al without echo
int 21h

cmp al,1bh        
jz endgame       ;ESC is entered,ending game,terminating
cmp al,61h        
jz strt        ;A is entered,starting game, :"(       
jmp screen1         
         
strt:             ;starting the game
MOV AX,0600H    ;06 TO SCROLL & 00 FOR FULLJ SCREEN
MOV BH,00H    ;ATTRIBUTE 7 FOR BACKGROUND AND 1 FOR FOREGROUND
MOV CX,0000H    ;STARTING COORDINATES
MOV DX,184FH    ;ENDING COORDINATES
INT 10H        ;FOR VIDEO DISPLAY
  
;status bar
mov ah,2
mov dh,14h
mov dl,00h
mov bh,0
int 10h       ;move cursor

mov ah,9
mov dx,offset status
int 21h 
   
mov ah,2
mov dh,15h
mov dl,00h
mov bh,0
int 10h       ;move cursor  

mov dx,offset player2+2
mov ah,9
int 21h   

mov ah,2
mov dh,15h
mov dl,06h
mov bh,0
int 10h

mov ah,9
mov dx,offset invitation    ;showing invitation notification
int 21h    


     
mov ah,07         ;Read one char and put in al without echo
int 21h           ;A--->game accepted

cmp al,61h   
jz startgame
jmp strt


startgame:             ; f2 is pressed
MOV AX,0600H    ;06 TO SCROLL & 00 FOR FULLJ SCREEN
MOV BH,00H    ;ATTRIBUTE 7 FOR BACKGROUND AND 1 FOR FOREGROUND
MOV CX,0000H    ;STARTING COORDINATES
MOV DX,184FH    ;ENDING COORDINATES
INT 10H        ;FOR VIDEO DISPLAY

mov ah,2
mov dh,12h
mov dl,00h
mov bh,0
int 10h       ;move cursor

mov ah,9
mov dx,offset status
int 21h 

;----------------p1 score---------
mov ah,2
mov dh,13h
mov dl,00h
mov bh,0
int 10h       ;move cursor 

mov ah,9
mov dx,offset player1+2
int 21h    

mov ah,2
mov dh,13h
mov dl,05h
mov bh,0
int 10h       ;move cursor 
                            
mov ah,9
mov dx,offset score
int 21h

mov ah,2
mov dh,13h
mov dl,0dh
mov bh,0
int 10h       ;move cursor 
                            
mov al, p1score
aam
add ax, 3030h
push ax
mov dl, ah
mov ah, 02h
int 21h
pop dx
mov ah, 02h
int 21h   

mov ah,2
mov dh,13h
mov dl,10h
mov bh,0
int 10h       ;move cursor 

mov ah,9
mov dx,offset hlth
int 21h
 
mov al, p1health
aam
add ax, 3030h
push ax
mov dl, ah
mov ah, 02h
int 21h
pop dx
mov ah, 02h
int 21h  

;----------p2 score------------
;printing score
mov ah,2
mov dh,13h
mov dl,30h
mov bh,0
int 10h       ;move cursor 

mov ah,9
mov dx,offset player2+2
int 21h    

mov ah,2
mov dh,13h
mov dl,35h
mov bh,0
int 10h       ;move cursor 
                            
mov ah,9
mov dx,offset score
int 21h

mov ah,2
mov dh,13h
mov dl,3dh
mov bh,0
int 10h       ;move cursor 
                            
mov al, p2score
aam
add ax, 3030h
push ax
mov dl, ah
mov ah, 02h
int 21h
pop dx
mov ah, 02h
int 21h

mov ah,2
mov dh,13h
mov dl,40h
mov bh,0
int 10h       ;move cursor         
        
mov ah,9
mov dx,offset hlth
int 21h   

mov al, p2health
aam
add ax, 3030h
push ax
mov dl, ah
mov ah, 02h
int 21h
pop dx
mov ah, 02h
int 21h  
 
    
;------------------------------------------------------------------      
;------------------------------------------------------------------    
 
;intilize cursor and display 0  
mov ah,0
mov al,13h
int 10h
;clear screen
mov ax,0600h
mov bh,0
mov cx,0
mov dx,184Fh
int 10h
    
mov si,064fh 
mov di,0600h
   
mov ah,2
mov dx,064fh
int 10h 
   
; player
drawSPACE
mov si,start1 ;store the pos of player1
mov di,start2 ;store the pos of player2
  
statusBAR    ;draw status bar      
  
;-------------
;-------------------power ups ---------------------------------------------
;draw power up with white color 

     
drawpower: 
;push dx
;push ax
mov dx,start2
mov ax, dx  ;this code is used to generate random number
xor dx, dx
mov cx, 10    
div cx        
cmp dl,4
jae red
mov cl,1111b
jmp draw
red:
mov cl,0100b  
draw:
mov y,0  
mov x1,150
mov x2,155
mov color,cl

mov cx,x1
mov dx,y
mov al,color  
mov ah,0ch

back:int 10h
inc cx
cmp cx,x2
jnz back

 inc y
mov dx,y
mov cx,x1
cmp y,40
jnz back

mov plus,0
;--------------------------------------------------------------------

;----------------clear buffer and start game-------------  
 
  
check:  
mov ah,0ch
mov al,0
int 21h    ;clear buffer 
  

moverefb1: ;check if there any reflectef bullet to mov it
 
 cmp refbullet1,1
 jnz  b1 
 invertbullet1
 statusBAR  
 
b1:
cmp  bullet1,1   ;flag to check if there is a bullet or not
jnz  moverefb2         
;-------------------------------------MOVING THE BULLETS-------------------------------------------------------------------------------
  
;moving the bullet for player one

mov cx,xb1
mov dx,by1     ;y position of player 1    
mov al,1111b   ;draw white pixel
mov ah,0ch
int 10h


;-------------------------check if the bullet hit the powerup

cmp xb1,150
jb  clearb1
cmp xb1,155
ja  clearb1

mov bx,plus 
dec bx
cmp bx,by1
ja  clearb1
mov bx,y
dec bx
cmp bx,by1 
jb  clearb1
cmp color,0100b   ;check which power did it hit
jnz wall1
cmp p2health,3
ja clear
inc p2health
statusBAR

clear:             ;clear the bullet and powerup after hitting each other
      dec xb1
      clearbullet1
      jmp stop
      
wall1:
      cmp refbullet1,1
      jz  clearb1
      mov bx,xb1
      mov refb1,bx
      mov bx,minus1
      mov refb2,bx
      mov refbullet1,1 
      mov bx,by1
      mov refy1,bx
      clearbullet1
      drawinvbullet1
      jmp moverefb2
      
      
      
clearb1: 
     
mov cx,minus1
mov dx,by1     ;y position of player 1
mov al,0000b   ;draw black pixel
mov ah,0ch
int 10h
dec minus1
dec xb1

cmp xb1,10
jnz moverefb2 
clearbullet1                   
 ;;;;;;;;;;;;;;;;;;;;;;;;;;HERE IM SURE ITS IN THE SAME COLUMN WITH the other player;;;;;;     
                
push dx
push cx           ;keep thier values in a safe place
mov dx,start2     ;position of the top of the player
mov cx,by1        ;position of the bullet

cmp cx,dx       ;comparing other player y pos with bullet y pos
jb clr1
mov cx,by1        ;no collapse as the bullet is above the player->clearbullet
mov dx,end2    ;;position of the bottom of the player
cmp cx,dx       ;comparing other player y pos with bullet y pos
ja clr1        ;;no collapse as the bullet is under the player->clearbullet 

inc p2score             ;collapse!!!!!!,inc score dec health
dec p1health               
mov dh,p1health                  
cmp dh,0                ;check if he was dead (RIP)
jz gameover1            ;ending game
clr1:
pop cx          ;bring back their values from the safe place
pop dx
clearbullet1       ;erasing the bullet
statusBAR          ;updating the status bar

;---------------------------------moving the bullet for player two

moverefb2:                    ;check if there any reflecitng bullet to move it 
 cmp refbullet2,1
 jnz bulletp2 
 invertbullet2 
 statusBAR

bulletp2:
cmp bullet2,1   ;flag to check if there is a bullet or not
jnz rec 

mov cx,xb2
mov dx,by2     ;player2 y position
mov al,1111b   ;draw white pixel
mov ah,0ch
int 10h

;-------------------------check if the bullet hit the powerup
 
cmp xb2,150
jb  clearb2
cmp xb2,155
ja  clearb2

mov bx,plus 
dec bx
cmp bx,by2
ja  clearb2
mov bx,y
dec bx
cmp bx,by2 
jb  clearb2
cmp color,0100b
jnz wall2
cmp p1health,3
jz  clear2
inc p1health
statusBAR  

clear2:
      inc xb2
      clearbullet2
      jmp stop
       
wall2:
      cmp refbullet2,1
      jz  clearb2
      mov bx,xb2
      mov refb3,bx
      mov bx,minus2
      mov refb4,bx
      mov refbullet2,1 
      mov bx,by2
      mov refy2,bx
      clearbullet2
      drawinvbullet2
      jmp rec

      
clearb2:
inc xb2      
mov cx,minus2
mov dx,by2     ;player2 y position
mov al,0000b   ;draw black pixel
mov ah,0ch
int 10h
inc minus2

cmp xb2,310
jnz rec

clearbullet2 

 ;;;;;;;;;;;;;;;;;;;;;;;;;;HERE IM SURE ITS IN THE SAME COLUMN WITH the other player;;;;;;     

push dx
push cx         ;keep thier values in a safe place
mov dx,start1   ;position of the top of the player
mov cx,by2      ;position of the bullt

cmp cx,dx       ;comparing p2 y pos with bullet y pos
jb clr2 
mov cx,by2       ;no collapse as the bullet is above the player->clearbullet
mov dx,end1      ;position of the bottom of the player
cmp cx,dx
ja clr2        ;;no collapse as the bullet is under the player->clearbullet
inc p1score             ;collapse,inc score dec health
dec p2health               
mov dh,p2health         ;         
cmp dh,0                 ;check if he is dead   (RIP)
jz gameover2             ;ending game
clr2:
pop cx
pop dx                 ;bring back their values from the safe place

clearbullet2          ;erasing the bullet
statusBAR             ;updating the status bar

;---------------movig power up 
rec:
cmp y,90h
jz stop
fall:
mov cx,x1
mov dx,plus 
mov al,0000b ;draw line with black color
mov ah,0ch

black:int 10h
inc cx
cmp cx,x2
jnz black
      
mov cx,x1
mov dx,y
mov al,color ;draw line with white color
mov ah,0ch

colored:int 10h
inc cx
cmp cx,x2
jnz colored

inc plus 
inc y
cmp y,200 
jmp delay
stop:
mov plus,20
clearpower
jmp drawpower

delay:
;delay
MOV     CX, 01H
MOV     DX, 3240H
mov ah, 86h    ;WAIT.
int 15h 

;----
mov ah,1      ;do not wait for key press
int 16h       ;key press  
jz check  



;------------------------------------------end power ups ----------------------------- 
   
 
jnz isUp       ;did the user press up arrow
   
mov bx,064fh 
jmp check       ;jmp if not press
isUp:
cmp ah,72         ;check if the user press up arrow
jnz isDown        ;if not the jmp else if 

cmp start1,0      ;if the first player arrive to the top of the screen
jz check          ;stop any movement
;---------------


; exchange the end1 and start1 so we can draw the playerpush end1          
  dec start1
  dec end1
  
  push end1
  mov bx,start1
  
  pop start1
  mov end1,bx
  ;-----------
  
jump:
clearFirstPlayerUP  ;clear      
jmp display     ;jmp to draw first player with new pos and update statusbar


isDown:        ;check if the user press down arrow     
cmp ah,80
jnz isUpW      ;if not then jmp else 
cmp end1,90h   ;check if the player arrive to the statusbar so it can't mov 
jz check       ;if yes then don't complete and jmp to check if the user press any key
push start1    ;store the previous pos so we can clear it 
push end1
;--------
 
jump3:
              
clearFirstPlayerDOWN    ;clear first player
;after clear first player with the previos pos pop the valuse to inc it 
pop end1
pop start1
inc end1
inc start1
   
jmp display    ;jmp to dispaly player with new pos and status bar
   
   
isUpW:           ;did the user press W
  cmp ah,11h
  jnz isDownS    ;jmp if not press
   
  cmp start2,0    ;check if the second player arrive to the top of the screen
  
  
  jz check        ;if yes,then stop any movement
   
  dec start2        ;if the user press W then dec the pos to move up
  dec end2
 ; exchange the end2 and start2 so we can draw the player 
  push end2
  mov bx,start2
  
  pop start2
  mov end2,bx
   
  
   

jump2:
     
clearSecondPlayerUP    ;clear second player with previous pos 
jmp display2           ;jmp to display second player with new pos
   
isDownS:        ; check if the user press S
  cmp ah,1FH   
  jnz isD        ;check if the user  press D to start FIGHT
  
  cmp end2,90h   ;check if the player arrive the status bar
  jz check       ;if yes the stop any movement
 
 ;store the previous pos so we can clear it 
  push start2
  push end2

 
  


jump4:
    
clearSecondPlayerDOWN      ;clear second player with previous pos 
;retrive the pos to inc it 
pop end2
pop start2
inc end2
inc start2
  
jmp display2
   
   
   
;-----------------------------fight ------left player press D----------------------
   
isD:
  cmp ah,20h
  jnz isLeft
   
;-------Write code here-----this code make left player to start fight--------------
 cmp  bullet2,1
 jz   isleft  
    
 
 mov bx,start2
 mov by2,bx 
  
 ;draw bullet for second player                                     
mov cx,10
mov dx,by2   ;TO DO exchange value of dx with the player2 y position
mov al,1111b
mov ah,0ch

backb2: int 10h
        inc cx
        cmp cx,14
        jnz backb2  
   
mov bullet2,1      
 
   
;-----------------end fight code for left player -----------------
   
 
 
;**********************************************************************************************************************  
   
;------------------fight ----------right player press left arrow-----------
   
isLeft:
  cmp ah,4Bh 
  jnz check
   
;-----Write code here !!!-------this code make right player to start fight--------- 
  
cmp  bullet1,1
jz   display
  
  
;------SAVING THE POSTION OF THE PLAYR WHEN HE SHOT THE BULLET

mov bx, start1

mov by1,bx
  
;---------------------DRAWING BULLET--------------------------------
  
mov cx,310       
mov dx,by1   
mov al,1111b
mov ah,0ch

backb1: int 10h
        dec cx
        cmp cx,306
        jnz backb1
          
mov bullet1,1   ;TURN THE FLAG ON              
    
   
;---------------------end fight code for right player---------------- 
     
display:
     
;clear
 
drawFirst    ;draw first player after clear it to mov it 

;-----------------------

statusBAR     ;draw status
 
jmp check      ;return to check if the user press key
    
display2:      ;display the second plaer
               
   
;-----------
;clear

drawSecond        ;draw second player after mov it 

    
  
;------------
statusBAR       ;draw status
      
jmp check  
    
gameover1:
MOV AX,0600H    ;06 TO SCROLL & 00 FOR FULLJ SCREEN
MOV BH,00H    ;ATTRIBUTE 7 FOR BACKGROUND AND 1 FOR FOREGROUND
MOV CX,0000H    ;STARTING COORDINATES
MOV DX,184FH    ;ENDING COORDINATES
INT 10H        ;FOR VIDEO DISPLAY

mov ah,2          ;Move Cursor
mov dx,0A08h      ;X,Y Position
int 10h

mov ah,9
mov dx,offset player2+2
int 21h 

mov ah,2          ;Move Cursor
mov dx,1408h      ;X,Y Position
int 10h

mov ah, 9
mov dx, offset p2win
int 21h 

mov ah,07         ;Read one char and put in al without echo
int 21h           ;A--->game accepted
jmp endgame



gameover2:

MOV AX,0600H    ;06 TO SCROLL & 00 FOR FULLJ SCREEN
MOV BH,00H    ;ATTRIBUTE 7 FOR BACKGROUND AND 1 FOR FOREGROUND
MOV CX,0000H    ;STARTING COORDINATES
MOV DX,184FH    ;ENDING COORDINATES
INT 10H        ;FOR VIDEO DISPLAY

mov ah,2          ;Move Cursor
mov dx,0A08h      ;X,Y Position
int 10h

mov ah,9
mov dx,offset player1+2
int 21h 

mov ah,2          ;Move Cursor
mov dx,0c08h      ;X,Y Position
int 10h

mov ah, 9
mov dx, offset p2win
int 21h 

mov ah,07         ;Read one char and put in al without echo
int 21h           
jmp endgame

    
endgame:
MOV AX,0600H    ;06 TO SCROLL & 00 FOR FULLJ SCREEN
MOV BH,00H    ;ATTRIBUTE 7 FOR BACKGROUND AND 1 FOR FOREGROUND
MOV CX,0000H    ;STARTING COORDINATES
MOV DX,184FH    ;ENDING COORDINATES
INT 10H        ;FOR VIDEO DISPLAY

MOV AH, 0
INT 21H
hlt 
    
    