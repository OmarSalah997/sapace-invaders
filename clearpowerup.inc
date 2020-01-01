clearpower macro  
mov cx,150
mov dx,plus
mov al,0000b ;draw line with black color
mov ah,0ch

clearp:int 10h
       inc cx
       cmp cx,155
       jnz clearp
       
inc plus
mov dx,plus
mov cx,150
cmp plus,90h
jnz clearp
endm
    