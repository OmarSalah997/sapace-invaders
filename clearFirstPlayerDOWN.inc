clearFirstPlayerDOWN macro
    
    
    clearplayer1DOWN:
mov cx,318   ;put the player x position
mov dx,start1     ;put the player y position
mov al,0000b ;player color 
mov ah,0ch

back11DOWN:int 10h
      dec cx
      cmp cx,310
      jnz back11DOWN
      
;inc start1
inc start1
mov dx,start1
mov cx,318
cmp dx,end1 
jnz back11DOWN
add end1,10 



endm