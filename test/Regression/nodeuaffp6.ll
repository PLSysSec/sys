; ModuleID = 'free.bc'
source_filename = "free.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%"class.std::__cxx11::basic_string" = type { i64, i64, %union.anon.92 }
%union.anon.92 = type { i64, [8 x i8] }

; Function Attrs: noinline nounwind sspstrong uwtable
define i8 @twopointers() local_unnamed_addr {

; we do this so that the definitions of the pointers
; will be out of scope and the tool will need to
; lazily allocate locations for the pointers
setup:
  %x = call i8* @getpointer()
  %y = call i8* @getpointer()
  br label %branchblock

branchblock:
  %branch = icmp eq i8* %x, %y
  br i1 %branch, label %true, label %false

; force the uaf if the two pointers are equal.
; if path constraints aren't working, it will assume pointers are unequal 
true:
  call void @free(i8* %x)
  %result = load i8, i8* %y
  ret i8 %result 

false:
  ret i8 0  
}

; Function Attrs: nounwind
declare void @free(i8* nocapture) local_unnamed_addr #1

; Function Attrs: nounwind
declare i8* @getpointer() 

declare void @llvm.lifetime.end.p0i8(i64, i8*) 

