; ModuleID = 'add.bc'
source_filename = "add.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline norecurse nounwind readnone sspstrong uwtable
define i8 @add(i64) local_unnamed_addr #0 {

  %struct1 = insertvalue { i32, i32, i32 } undef, i32 1, 0
  %dep1 = extractvalue { i32, i32, i32 } %struct1, 1

  %struct2 = insertvalue { i32, i32, i32 } undef, i32 2, 2
  %dep2 = extractvalue { i32, i32, i32 } %struct2, 1

  %struct3 = insertvalue { i16, i64 } undef, i64 2, 1
  %dep3 = extractvalue { i16, i64 } %struct3, 1

  %struct4 = insertvalue { i16, i16 } undef, i16 2, 1
  %dep4 = extractvalue { i16, i16 } %struct4, 1

  %struct5 = insertvalue { { i16 }, { i16 } } undef, { i16 } undef, 1
  %dep5 = extractvalue { { i16 }, { i16 } } %struct5, 1

  %struct6 = insertvalue { { i32, i16 }, { i32, i16 } } undef, { i32, i16 } undef, 1
  %dep6 = extractvalue { { i32, i16 }, { i32, i16 } } %struct6, 1

  %struct7 = insertvalue { { i64, i16 }, { i64, i16 } } undef, { i64, i16 } undef, 1
  %dep7 = extractvalue { { i64, i16 }, { i64, i16 } } %struct7, 1

  %struct8 = insertvalue { <3 x i16>, <3 x i16> } undef, <3 x i16> undef, 1
  %dep8 = extractvalue { <3 x i16>, <3 x i16> } %struct8, 1

  %struct9 = insertvalue { i16, i64, i16 } undef, i16 0, 2
  %dep9 = extractvalue { i16, i64, i16 } %struct9, 1

  %array10 = insertvalue [5 x i16] undef, i16 1, 4
  %dep10 = extractvalue [5 x i16] %array10, 1

  %array11 = insertvalue [7 x i16] undef, i16 1, 6
  %dep11 = extractvalue [7 x i16] %array11, 1

  %array12 = insertvalue [3 x { i16, i32 }] undef, { i16, i32 } undef, 2
  %dep12 = extractvalue [3 x { i16, i32 }] %array12, 0

  %array13 = insertvalue [2 x <3 x i16>] undef, <3 x i16> undef, 1
  %dep13 = extractvalue [2 x <3 x i16>] %array13, 1

  %array14 = insertvalue [4 x <2 x i16>] undef, <2 x i16> undef, 3
  %dep14 = extractvalue [4 x <2 x i16>] %array14, 1

  ret i8 5
}

attributes #0 = { noinline norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
