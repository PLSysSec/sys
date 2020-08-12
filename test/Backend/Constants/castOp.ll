; ModuleID = 'free.bc'
source_filename = "free.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.apoint = type { i32, i32 }
%struct.bpoint = type { i32, i32 }

@trunc = local_unnamed_addr constant i8 trunc (i32 511 to i8), align 4
@zext = local_unnamed_addr constant i8 zext (i1 1 to i8), align 4
@sext = local_unnamed_addr constant i8 sext (i1 1 to i8), align 4
@ptrtoint = local_unnamed_addr constant i8 ptrtoint (i32 * null to i8), align 4
@inttoptr = local_unnamed_addr constant i32 * inttoptr (i32 5 to i32 *), align 4
@bitcast = local_unnamed_addr constant %struct.bpoint * bitcast (%struct.apoint * null to %struct.bpoint *), align 4



; Function Attrs: noinline norecurse nounwind sspstrong uwtable
define i8 @simple(i32 nocapture) local_unnamed_addr #0 {
  %2 = load i8, i8 * @trunc
  %3 = load i8, i8 * @zext
  %4 = add i8 %2, %3
  %5 = load i8, i8 * @sext
  %6 = load i8, i8 * @ptrtoint
  %7 = add i8 %5, %6
  %8 = load i32 *, i32 ** @inttoptr
  %9 = load i32, i32 * %8
  %10 = load %struct.bpoint *, %struct.bpoint ** @bitcast
  %11 = load %struct.bpoint, %struct.bpoint * %10 
  ret i8 %5
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
