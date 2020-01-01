;-------------------clear bullet for first player-------------------
clearbullet1 macro
    
local c1


mov cx,minus1
mov dx,by1     ;TO DO exchange value 100 with the player2 y position
mov al,0000b   ;draw black pixel
mov ah,0ch
c1:     int 10h
        dec cx
        cmp cx,xb1
        jnz c1

mov xb1,306 
mov minus1,310
mov bullet1,0

endm clearbullet1

;-------------------clear bullet for second player-----------------

clearbullet2 macro 
    
local c2
  
mov cx,minus2
mov dx,by2     ;TO DO exchange value 100 with the player2 y position
mov al,0000b   ;draw black pixel
mov ah,0ch
c2:    int 10h
       inc cx
       cmp cx,xb2
       jnz c2

mov xb2,14 
mov minus2,10
mov bullet2,0    

endm clearbullet2

;---------------------------moving the reflected bullet for right player-----------------------------
                  
invertbullet1 macro

local back2,en,cl2,clrbultr
                   
mov cx,refb2
mov dx,refy1     ;player1 y position
mov al,1111b     ;draw white pixel
mov ah,0ch
int 10h
inc refb2

      
mov cx,refb1
mov dx,refy1     ;player1 y position
mov al,0000b     ;draw black pixel
mov ah,0ch
int 10h
inc refb1
cmp refb2,310 
jnz en
  ;;;;;;;;;;;;;;;HERE IM SURE ITS IN SAME COLUMN WITH THE PLAYER -_- ;
        push dx
        push cx           ;keep thier values in a safe place
        mov cx,refy1      ;POSITION OF THE BULLET
        mov dx,start1     ;position of the top of the player
        cmp cx,dx       ;comparing other player y pos with bullet y pos
        jb clrbultr        ;no collapse as the bullet is above the player->clearbullet
        mov cx,refy1
        mov dx,end1    ;;position of the bottom of the player
        cmp cx,dx       ;comparing other player y pos with bullet y pos
        ja clrbultr        ;;no collapse as the bullet is under the player->clearbullet 
        dec p2health 
        mov dh,p2health                  
        cmp dh,0                ;check if he was dead (RIP)
        jz gameover2            ;ending game
        pop cx
        pop dx      
clrbultr:
mov cx,refb1
mov dx,refy1     
mov al,0000b     ;draw black pixel
mov ah,0ch
cl2:   int 10h
       inc cx
       cmp cx,310 
       jnz cl2
             
mov refb1,0 
mov refb2,0
mov refbullet1,0

                               
en:                       
endm invertbullet1

;-----------------------draw reflected bullet for right player------------------------------

drawinvbullet1 macro

local backb2
    
mov cx,refb1
mov dx,refy1   ;player2 y position
mov al,1111b
mov ah,0ch

backb2: int 10h
        inc cx
        cmp cx,refb2
        jnz backb2
         
endm drawinvbullet1 

;-----------------------moving reflected bullet for left player----------------------------------------
invertbullet2 macro

local back2,en,cl2 ,clrbuletl
                   
mov cx,refb4
mov dx,refy2     ;player2 y position
mov al,1111b     ;draw white pixel
mov ah,0ch
int 10h
dec refb4

      
mov cx,refb3
mov dx,refy2     ;player2 y position
mov al,0000b     ;draw black pixel
mov ah,0ch
int 10h
dec refb3
cmp refb4,10 
jnz en
         ;;;;;;;;;;;;here im sure its in the same column -_-;;;;;;;;;
         
        push dx
        push cx           ;keep thier values in a safe place
        mov cx,refy2     ;POSITION OF THE BULLET
        mov dx,start2     ;position of the top of the player
        cmp cx,dx       ;comparing other player y pos with bullet y pos 
        
        jb clrbuletl        ;no collapse as the bullet is above the player->clearbullet 
        mov cx,refy2
        mov dx,end2    ;;position of the bottom of the player
        cmp cx,dx       ;comparing other player y pos with bullet y pos
        ja clrbuletl        ;;no collapse as the bullet is under the player->clearbullet 
        dec p1health 
        mov dh,p1health                  
        cmp dh,0                ;check if he was dead (RIP)
        jz gameover1            ;ending game
        pop cx
        pop dx 
        
        
clrbuletl:              
mov cx,refb3
mov dx,refy2     ;the player2 y position
mov al,0000b     ;draw black pixel
mov ah,0ch
cl2:   int 10h
       dec cx
       cmp cx,10
       jnz cl2

mov refb3,0 
mov refb4,0
mov refbullet2,0

                               
en:                       
endm invertbullet2

;--------------------------drawing the reflected bullet for left player----------------------------------

drawinvbullet2 macro

local backb2

dec refb3
dec refb4
    
mov cx,refb3
mov dx,refy2           ;player2 y position
mov al,1111b
mov ah,0ch

backb2: int 10h
        dec cx
        cmp cx,refb4
        jnz backb2
         
endm drawinvbullet2