; R1 is the stack pointer
; R31 holds the return address after a CALL
; R3, R4 etc hold parameters.
;
; execution starts here.
addi    1,0,1024        ; initialise stack pointer
call    0,0,18          ; call main
call    0,0,25          ; call exit
;
; function FAC: parameter N comes in R3.
;
st      31,1,-1         ; save return address
st      3,1,-2          ; save parameter N
subi    1,1,2           ; decrement stack pointer
seqi    4,3,1           ; R4 = N == 1
bf      0,4,11          ; branch if N != 1
; return 1
addi    3,0,1           ; R3 = 1
addi    1,1,2           ; increment stack pointer
jmp     0,31,0          ; jump to return address
;
; return n * fac(n-1)
subi    3,3,1           ; N-1
call    0,0,3           ; recursive call. result in R3
ld      4,1,0           ; reload parameter
mul     3,3,4           ; R3 = N * fac(N-1)
ld      5,1,1           ; reload return address
addi    1,1,2           ; increment stack pointer
jmp     0,5,0           ; jump to return address
;
; function MAIN
;
st      31,1,-1         ; save return address
subi    1,1,1           ; decrement stack pointer
addi    3,0,12          ; N = 12
call    0,0,3           ; call fac
ld      5,1,0           ; reload return address
addi    1,1,1           ; increment stack pointer
jmp     0,5,0           ; jump to return address
;
; function EXIT
;
halt    0,0,0           ; halt machine (in unix: do exit system call)