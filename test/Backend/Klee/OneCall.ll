; RUN: %S/ConcreteTest.py --klee='%klee' --lli=%lli %s

declare void @print_i32(i32)

; wtf did you guys call this sum? 
define i32 @sum(i32 %a, i32 %b) {
	%c = sub i32 %a, %b
	ret i32 %c
}

define i32 @main() {
	%a = call i32 @sum(i32 54, i32 2)
	%dep = add i32 %a, %a 
	ret i32 0
}
