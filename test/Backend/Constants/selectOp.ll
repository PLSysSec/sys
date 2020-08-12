; ModuleID = 'free.bc'
source_filename = "free.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@sel1 = local_unnamed_addr constant i32 select (i1 1, i32 1, i32 0), align 4
@sel2 = local_unnamed_addr constant i32 select (i1 0, i32 1, i32 0), align 4
@sel3 = local_unnamed_addr constant <1 x i32> select (i1 1, <1 x i32> <i32 1>, <1 x i32> <i32 0>), align 4
@sel4 = local_unnamed_addr constant <1 x i32> select (<1 x i1> <i1 0>, <1 x i32> <i32 1>, <1 x i32> <i32 0>), align 4
@sel5 = local_unnamed_addr constant <1 x i32> select (<1 x i1> <i1 1>, <1 x i32> add (<1 x i32> <i32 1>, <1 x i32> <i32 1>), <1 x i32> <i32 0>), align 4



; Function Attrs: noinline norecurse nounwind sspstrong uwtable
define i32 @simple(i32 nocapture) local_unnamed_addr #0 {
  %2 = load i32, i32 * @sel1
  %3 = load i32, i32 * @sel2
  %4 = or i32 %2, %3
  %5 = load <1 x i32>, <1 x i32> * @sel3
  %6 = extractelement <1 x i32> %5, i64 0
  %7 = load <1 x i32>, <1 x i32> * @sel4
  %8 = extractelement <1 x i32> %7, i64 0
  %9 = add i32 %6, %8
  %10 = load <1 x i32>, <1 x i32> * @sel5
  %11 = extractelement <1 x i32> %10, i64 0
  ret i32 %11
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
