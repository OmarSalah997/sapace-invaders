drawSPACE macro
local  drawplayer1
local  back1
local  drawplayer2
local  player22


drawplayer1:

;the first player
mov cx,318   ;put the player x position(don't change this)
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




drawplayer2:

;the first player
mov cx,2   ;put the player x position(don't change this)
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