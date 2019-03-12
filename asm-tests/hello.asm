section .data
    text db "Hello, World",0,10
 
section .text
    global _start

_start:
	;; This code will be used for injection eventually, so we want
	;; to back up all registers to the stack and restore them when the program
        ;; leaves the injected code
;; BACKUP registers:
	push rax
	push rdi
	push rsi
	push rdx
	;; 
;;; 
	push cx
	mov rax, 0
	mov rcx, 0x1 	;looping this already takes 4 seconds!
loop:
	inc rax
	dec rcx
	jnz loop
	call test
done:	
;;; RESTORE registers
	pop rax
	pop rdi
	pop rsi
	pop rdx
	pop cx
	;; exit:
	mov rax, 60			;sys_exit
	mov rdi, 0
	syscall

test:	
	mov rax, 1     		; sys_write syscall
	mov rdi, 1
	mov rsi, text		;TODO how to find string?
	mov rdx, 14
	syscall
	ret
