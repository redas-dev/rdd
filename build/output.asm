%macro print 1 ; macro with one argument
 push dword %1 ; %1 means first argument
call printf
add  esp, 4
%endmacro
section .bss
section .text
    global _start
    extern printf
_start:
    mov rax, 60
    xor rdi, rdi
    syscall
section .data
    format_int: db "%d", 0
    format_string: db "%s", 0
    format_float: db "%f", 0
