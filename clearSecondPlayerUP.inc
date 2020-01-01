clearSecondPlayerUP macro
local clearplayer2
local player2c
    
clearplayer2:
mov cx,2      ;put the player x position
mov dx,start2     ;put the player y position
mov al,0000b   ;player color 
mov ah,0ch

player2c:int 10h
         inc cx
         cmp cx,10
         jnz player2c

dec start2      
mov dx,start2
mov cx,2
cmp dx,end2 
jnz player2c
add end2,10

;-------
  

endm