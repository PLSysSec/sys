; ModuleID = 'add.bc'
source_filename = "add.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline norecurse nounwind readnone sspstrong uwtable
define i32* @alloca(i32, i32) local_unnamed_addr #0 {
  %ptr1 = alloca i32
  %ptr2 = alloca i64
  %ptr3 = alloca { { { i32 } } }
  %ptr4 = alloca i16
  %ptr5 = alloca i1
  %ptr6 = alloca i8
  %ptr7 = alloca i64
  %ptr8 = alloca i1
  %ptr9 = alloca i64
  %dep1 = load i32, i32 * %ptr1
  %dep2 = load i64, i64 * %ptr2
  %dep3 = load { { { i32 } } }, { { { i32 } } } * %ptr3
  %dep4 = load i16, i16 * %ptr4
  %dep5 = load i1, i1 * %ptr5
  %dep6 = load i8, i8 * %ptr6
  %dep7 = load i64, i64 * %ptr7
  %dep8 = load i1, i1 * %ptr8
  %dep9 = load i64, i64 * %ptr9
  ret i32* null
}


attributes #0 = { noinline norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
