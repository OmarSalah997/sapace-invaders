
statusBAR  macro 
local startgame    
 


startgame:             
mov ah,2
mov dh,15h
mov dl,00h
mov bh,0
int 10h       ;move cursor

mov ah,9
mov dx,offset status
int 21h 
 



 
;status bar
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
mov dh,14h
mov dl,00h
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
mov dl,40h
mov bh,0
int 10h       ;move cursor 

mov ah,9
mov dx,offset player2+2
int 21h    

mov ah,2
mov dh,13h
mov dl,45h
mov bh,0
int 10h       ;move cursor 
                            
mov ah,9
mov dx,offset score
int 21h

mov ah,2
mov dh,13h
mov dl,4dh
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
mov dh,14h
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
    
    
    

endm    statusBAR
    
;------------------------------------------------------------------    
