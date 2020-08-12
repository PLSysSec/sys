; RUN: %S/ConcreteTest.py --klee='%klee' --lli=%lli %s

declare void @print_i32(i32)

define i32 @main() {
entry:
	br label %test
test:
	%a = phi i32 [10, %entry], [%b, %test] 
	%b = phi i32 [%a, %test], [20, %entry]
	%c = phi i32 [0, %entry], [1, %test]
	%d = icmp eq i32 %c, 1
	%dep1 = add i32 %a, %b 
	br i1 %d, label %exit, label %test
exit:
	call void @print_i32(i32 %b)
	ret i32 0
}

; 4 blocks 
; a = 10 X
; b = 20 X
; c = 0 X 
; d = cmp eq 0 1 = false
; br test
; a_1 = 20 X
; b_1 = 20 X
; c_1 = 1 X
; d_1 = cmp eq 1 1 = true
; br exit
; print 10
; ret 0

; one block oks are
; "coming from entry, going to test" and
; "coming from test, going to exit"

; coming from entry, going to test
; a = 10
; b = 20
; c = 0
; cmp c (only see c because its the only one that used)

; coming from test going to exit
; a = prev_b
; b = prev_a
; c = 1


