; ModuleID = 'free.bc'
source_filename = "free.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@addsimple = local_unnamed_addr constant i32 add (i32 0, i32 5), align 4
@addcomplex = local_unnamed_addr constant i32 add (i32 add (i32 5, i32 5), i32 5), align 4
@subsimple = local_unnamed_addr constant i8 sub (i8 0, i8 1), align 4
@subcomplex = local_unnamed_addr constant i8 sub (i8 0, i8 sub (i8 1, i8 1)), align 4
@mulsimple = local_unnamed_addr constant i8 mul (i8 5, i8 5), align 4
@mulcomplex = local_unnamed_addr constant i8 mul (i8 5, i8 sub (i8 1, i8 1)), align 4
@udivsimple = local_unnamed_addr constant i8 udiv (i8 254, i8 2), align 4
@udivcomplex = local_unnamed_addr constant i8 udiv (i8 254, i8 sub (i8 4, i8 2)), align 4
@sdivsimple = local_unnamed_addr constant i8 sdiv (i8 254, i8 2), align 4
@sdivcomplex = local_unnamed_addr constant i8 sdiv (i8 254, i8 sub (i8 4, i8 2)), align 4
@uremcomplex = local_unnamed_addr constant i8 urem (i8 255, i8 sub (i8 4, i8 2)), align 4
@sremcomplex = local_unnamed_addr constant i8 srem (i8 255, i8 sub (i8 4, i8 2)), align 4
@shlcomplex = local_unnamed_addr constant i8 shl (i8 1, i8 sub (i8 5, i8 4)), align 4
@lshrcomplex = local_unnamed_addr constant i8 lshr (i8 255, i8 sub (i8 14, i8 7)), align 4
@ashrcomplex = local_unnamed_addr constant i8 ashr (i8 255, i8 sub (i8 14, i8 7)), align 4
@andcomplex = local_unnamed_addr constant i1 and (i1 and (i1 0, i1 1), i1 0), align 4
@orcomplex = local_unnamed_addr constant i1 or (i1 sub (i1 1, i1 1), i1 or (i1 1, i1 0)), align 4
@xorcomplex = local_unnamed_addr constant i1 xor (i1 1, i1 xor (i1 1, i1 1)), align 4

; Function Attrs: noinline norecurse nounwind sspstrong uwtable
define i1 @simple(i32 nocapture) local_unnamed_addr #0 {
  %2 = load i32, i32 * @addsimple
  %3 = load i32, i32 * @addcomplex
  %4 = add i32 %2, %3
  %5 = load i8, i8 * @subsimple
  %6 = load i8, i8 * @subcomplex
  %7 = add i8 %5, %6
  %8 = load i8, i8 * @mulsimple
  %9 = load i8, i8 * @mulcomplex
  %10 = add i8 %8, %9
  %11 = load i8, i8 * @udivsimple
  %12 = load i8, i8 * @udivcomplex
  %13 = add i8 %11, %12
  %14 = load i8, i8 * @sdivsimple
  %15 = load i8, i8 * @sdivcomplex
  %16 = add i8 %14, %15
  %17 = load i8, i8 * @uremcomplex
  %18 = load i8, i8 * @sremcomplex
  %19 = add i8 %17, %18
  %20 = load i8, i8 * @shlcomplex
  %21 = load i8, i8 * @lshrcomplex
  %22 = add i8 %20, %21
  %23 = load i8, i8 * @ashrcomplex
  %24 = add i8 %23, %23
  %25 = load i1, i1 * @andcomplex
  %26 = load i1, i1 * @orcomplex
  %27 = add i1 %25, %26
  %28 = load i1, i1 * @xorcomplex
  ret i1 %28
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
