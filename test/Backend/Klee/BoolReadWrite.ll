; LLVM 3.7 requires a type as the first argument to 'load'
; REQUIRES: geq-llvm-3.7
; RUN: %S/ConcreteTest.py --klee='%klee' --lli=%lli %s

declare void @print_i1(i1)

define i32 @main() {
        %mem = alloca i1
        store i1 1, i1* %mem
        %v = load i1, i1* %mem
	%res = zext i1 %v to i32
	ret i32 %res
	
;        br i1 %v, label %ok, label %exit	
; ok:
; 	call void @print_i1(i1 %v)
;         br label %exit
; exit:
; 	ret i32 0
}
