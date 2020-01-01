drawSecond macro

local drawplayer2
local player22

drawplayer2:

;the first player
mov cx,2      ;put the player x position(don't change this)
mov dx,start2     ;put the player y position
mov al,1011b ;player color 
mov ah,0ch

player22:int 10h
        inc cx
        cmp cx,10
        jnz player22


inc dx
mov cx,2
cmp dx,end2
jnz player22 

endm