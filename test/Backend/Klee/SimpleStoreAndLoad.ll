; LLVM 3.7 requires a type as the first argument to 'getelementptr'
; LLVM 3.7 requires a type as the first argument to 'load'
; REQUIRES: geq-llvm-3.7
; RUN: %S/ConcreteTest.py --klee='%klee' --lli=%lli %s

declare void @print_i32(i32)

define i32 @main() {
entry:
	%a = alloca i32, i32 4
	%tmp1 = getelementptr i32, i32* %a, i32 0
	store i32 0, i32* %tmp1
	%tmp2 = load i32, i32* %tmp1
	%tmp3 = icmp eq i32 %tmp2, 0
	%retres = zext i1 %tmp3 to i32
        ret i32 %retres		  
; 	br i1 %tmp3, label %exitTrue, label %exitFalse
; exitTrue:
; 	call void @print_i32(i32 1)
; 	ret i32 0
; exitFalse:
; 	call void @print_i32(i32 0)
; 	ret i32 0
}
