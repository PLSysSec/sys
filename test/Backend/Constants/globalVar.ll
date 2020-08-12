; ModuleID = 'free.bc'
source_filename = "free.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.apoint = type { i32, i32 }
@foo = local_unnamed_addr constant %struct.apoint { i32 10, i32 20 }, align 4
@bar = local_unnamed_addr constant <2 x i32> <i32 0, i32 1>, align 4
@magicnum = local_unnamed_addr constant i32 440, align 4

; Function Attrs: noinline norecurse nounwind sspstrong uwtable
define i32 @simple(%struct.apoint* nocapture) local_unnamed_addr #0 {
  %2 = load %struct.apoint, %struct.apoint * @foo
  %3 = extractvalue %struct.apoint %2, 0
  %4 = extractvalue %struct.apoint %2, 1
  %5 = add i32 %3, %4
  %6 = load i32, i32 * @magicnum
  %7 = add i32 %5, %6
  %8 = load <2 x i32>, <2 x i32> * @bar
  %9 = extractelement <2 x i32> %8, i32 0
  %10 = extractelement <2 x i32> %8, i32 1
  %11 = add i32 %9, %10
  ret i32 %3
}

attributes #0 = { noinline norecurse nounwind sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
!4 = !{!5, !6, i64 0}
!5 = !{!"apoint", !6, i64 0, !6, i64 4}
!6 = !{!"int", !7, i64 0}
!7 = !{!"omnipotent char", !8, i64 0}
!8 = !{!"Simple C/C++ TBAA"}
!9 = !{!5, !6, i64 4}
