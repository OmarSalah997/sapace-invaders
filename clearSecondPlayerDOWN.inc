clearSecondPlayerDOWN macro
    
clearplayer2DOWN:
mov cx,2      ;put the player x position
mov dx,start2     ;put the player y position
mov al,0000b   ;player color 
mov ah,0ch

player2cDOWN:int 10h
         inc cx
         cmp cx,10
         jnz player2cDOWN

inc start2      
mov dx,start2
mov cx,2
cmp dx,end2 
jnz player2cDOWN
add end2,10

;-------
  

endm