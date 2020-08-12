; ModuleID = 'free.bc'
source_filename = "free.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@bob = local_unnamed_addr constant { { i64 } } insertvalue ( { { i64 } } undef, i64 200, 0, 0)

; Function Attrs: noinline norecurse nounwind readonly sspstrong uwtable
define i64 @simple(i32* nocapture readonly) local_unnamed_addr #0 {

  %tostore = load { { i64 } }, { { i64 } } * @bob
  %ptr = alloca { { i64 } }
  store { { i64 } } %tostore, { { i64 } } * %ptr
  %second = load { { i64 } }, { { i64 } } * %ptr
  %result = extractvalue { { i64 } } %second, 0, 0
  %dep = add i64 %result, %result

  %insert = insertvalue { { i64 } } undef, i64 400, 0, 0
  %extract = extractvalue { { i64 } } %insert, 0, 0

  %store = insertvalue { i8, { { i64 * } } } undef, i64 * null, 1, 0, 0
  %load = extractvalue { i8, { { i64 * } } } %store, 1, 0, 0
  %dep2 = load i64, i64 * %load

  ret i64 %extract
}

attributes #0 = { noinline norecurse nounwind readonly sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
!4 = !{!5, !6, i64 8}
!5 = !{!"my_struct", !6, i64 0, !6, i64 8}
!6 = !{!"any pointer", !7, i64 0}
!7 = !{!"omnipotent char", !8, i64 0}
!8 = !{!"Simple C/C++ TBAA"}
