; ModuleID = 'src/loop.c'
source_filename = "src/loop.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

define i32 @myFree(i32** nocapture readonly) {
  br label %2

; <label>:2:                                      ; preds = %2, %1
  %3 = phi i64 [ 0, %1 ], [ %7, %2 ]
  %4 = getelementptr inbounds i32*, i32** %0, i64 %3
  %5 = bitcast i32** %4 to i8**
  %6 = load i8*, i8** %5, align 8
  tail call void @free(i8* %6)
  %7 = add nuw nsw i64 %3, 1
  %8 = icmp eq i64 %7, 2
  br i1 %8, label %9, label %2

; <label>:9:                                      ; preds = %2
  %10 = load i32*, i32** %0, align 8
  %11 = load i32, i32* %10, align 4
  ret i32 %11
}

declare void @free(i8* nocapture) local_unnamed_addr #1
