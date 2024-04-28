.section .text
.globl _syscall

#   +8              +4               +0
#   +---------------+----------------+ -32
#   |             arg1               |
#   +--------------------------------+ -24
#   |             arg2               |
#   +--------------------------------+ -16
#   |             arg3               |
#   +--------------------------------+ -8
#   |          sys_number            |
#   +--------------------------------+ <== rbp
#   |           old rbp              |
#   +--------------------------------+
#   |        return address          |
#   +--------------------------------+

_syscall:
    pushq %rbp
    movq  %rsp, %rbp
    subq  $64, %rsp

    movq %rdi, -32(%rbp)
    movq %rsi, -24(%rbp)
    movq %rdx, -16(%rbp)
    movq %rcx,  -8(%rbp)


    movq -32(%rbp), %rdi    # rdi = arg1
    movq -24(%rbp), %rsi    # rsi = arg2
    movq -16(%rbp), %rdx    # rdx = arg3
    movq  -8(%rbp), %rax    # rax = sys_number
    
    syscall

    leave
    ret

