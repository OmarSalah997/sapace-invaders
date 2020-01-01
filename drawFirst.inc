drawFirst macro
    
local  drawplayer1
local  back1
    
drawplayer1:

;the first player
mov cx,318        ;put the player x position(don't change this)
mov dx,start1     ;put the player y position
mov al,0001b ;player color 
mov ah,0ch

back1:int 10h
      dec cx
      cmp cx,310
      jnz back1


inc dx
mov cx,318
cmp dx,end1
jnz back1


endm
