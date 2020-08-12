; RUN: %S/ConcreteTest.py --klee='%klee' --lli=%lli %s

declare void @print_i32(i32)

define i32 @main() {
entry:
	br label %test
test:
	%a = phi i32 [10, %entry], [%b, %test]
	%depa = add i32 %a, %a
	%b = phi i32 [20, %entry], [%a, %test]
	%c = phi i32 [0, %entry], [1, %test]
	%d = icmp eq i32 %c, 1
	br i1 %d, label %exit, label %test
exit:
	call void @print_i32(i32 %b)
	ret i32 0
}

; entry
; test
; a = 10
; b = 20
; c = 0
; d = cmp c 1
; br d exit test
; test
; a_1 = 20
; b_1 = 20
; c_1 = 1
; d = cmp c_1 1
; br exit