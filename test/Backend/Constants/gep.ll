; ModuleID = 'free.bc'
source_filename = "free.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.apoint = type { i32, i32 }

@greeting = local_unnamed_addr global [7 x i8] c"Hello!\00", align 1
@foo = local_unnamed_addr constant %struct.apoint { i32 10, i32 20 }, align 4
@.str.33 = local_unnamed_addr constant [10 x i8] c"FSReqWrap\00", align 1
@gepconst = local_unnamed_addr constant i8 * getelementptr ([10 x i8], [10 x i8] * @.str.33, i32 0, i32 2)

; Function Attrs: noinline norecurse nounwind readonly sspstrong uwtable
define i32 @simple(i32 nocapture readonly) local_unnamed_addr #0 {
  %gep1 = getelementptr %struct.apoint, %struct.apoint * @foo, i32 0, i32 0
  %val1 = load i32, i32 * %gep1
  %dep1 = add i32 %val1, %val1

  %gep2 = getelementptr [7 x i8], [7 x i8] * @greeting, i32 0, i32 5
  %val2 = load i8, i8 * %gep2
  %dep2 = add i8 %val2, %val2

  %ptr = load i8 *, i8 ** @gepconst
  %val3 = load i8, i8 * %ptr
  %dep3 = add i8 %val3, %val3 
  
  ret i32 4
}

attributes #0 = { noinline norecurse nounwind readonly sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
!4 = !{!5, !5, i64 0}
!5 = !{!"int", !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
