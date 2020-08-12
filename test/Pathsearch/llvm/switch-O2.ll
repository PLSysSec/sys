; ModuleID = 'switch.c'
source_filename = "switch.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: norecurse nounwind readnone sspstrong uwtable
define dso_local i32 @foo(i32, i32, i32) local_unnamed_addr #0 {
  switch i32 %0, label %5 [
    i32 3, label %7
    i32 4, label %4
  ]

; <label>:4:                                      ; preds = %3
  br label %7

; <label>:5:                                      ; preds = %3
  %6 = mul nsw i32 %1, 3
  ret i32 %6

; <label>:7:                                      ; preds = %3, %4
  %8 = phi i32 [ %2, %4 ], [ %0, %3 ]
  ret i32 %8
}

attributes #0 = { norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 7.0.0 (tags/RELEASE_700/final)"}
