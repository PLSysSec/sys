; ModuleID = 'free.bc'
source_filename = "free.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@cmp1 = local_unnamed_addr constant i1 icmp eq (i32 1, i32 1), align 4

@cmp2 = local_unnamed_addr constant i1 icmp ne (i32 add (i32 1, i32 1), i32 1), align 4

@cmp3 = local_unnamed_addr constant <2 x i1> icmp eq (<2 x i32> <i32 1, i32 1>, <2 x i32> <i32 0, i32 1>)

; Function Attrs: noinline norecurse nounwind sspstrong uwtable
define i1 @simple(i32 nocapture) local_unnamed_addr #0 {
  %2 = load i1, i1 * @cmp1
  %3 = load i1, i1 * @cmp2
  %4 = or i1 %2, %3
  %5 = load <2 x i1>, <2 x i1> * @cmp3
  %6 = extractelement <2 x i1> %5, i32 0
  %7 = extractelement <2 x i1> %5, i32 1
  %8 = or i1 %6, %7
  ret i1 %6
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
