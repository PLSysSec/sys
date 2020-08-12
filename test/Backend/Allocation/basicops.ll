; ModuleID = 'add.bc'
source_filename = "add.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline norecurse nounwind readnone sspstrong uwtable
define i8 @add(i64) local_unnamed_addr #0 {

  %simplestruct = insertvalue { i64, i64, i64 } { i64 1, i64 2, i64 3 }, i64 6, 0
  %simpleextract = extractvalue { i64, i64, i64 } %simplestruct, 0
  %simpledep = add i64 %simpleextract, %simpleextract

  %padstruct = extractvalue { i64, i32 } { i64 4, i32 8 }, 1
  %paddep = add i32 %padstruct, %padstruct

  %padvect = extractelement <3 x i32> <i32 1, i32 2, i32 3>, i32 2
  %padvecdep = add i32 %padvect, %padvect

  %add = add i8 4, 4
  %depadd = add i8 %add, %add

  %add1 = add i1 1, 1
  %depadd1 = add i1 %add1, %add1

  %insert2 = insertvalue { i64, i64, i64 } undef, i64 5, 1
  %insert3 = insertvalue { i64, i64, i64 } %insert2, i64 8, 0
  %insert4 = insertvalue { i64, i64, i64 } %insert3, i64 9, 2  	     
  %extract2 = extractvalue { i64, i64, i64 } %insert4, 1 
  %extract3 = extractvalue { i64, i64, i64 } %insert4, 0
  %extract4 = extractvalue { i64, i64, i64 } %insert4, 2
  %dep2 = add i64 %extract3, %extract2
  %dep3 = add i64 %dep2, %extract4

  %einsert1 = insertelement <2 x i32> undef, i32 4, i64 1
  %eextract1 = extractelement <2 x i32> %einsert1, i64 1
  %edep1 = add i32 %eextract1, %eextract1

  %insert5 = insertvalue { { i64 } } undef, i64 7, 0, 0
  %extract5 = extractvalue { { i64 } } %insert5, 0, 0
  %insert6 = insertvalue { { i64 }, { { { i16, i16 } } } } undef, i16 9, 1, 0, 0, 0
  %insert7 = insertvalue { { i64 }, { { { i16, i16 } } } } %insert6, i16 12, 1, 0, 0, 1
  %extract6 = extractvalue { { i64 }, { { { i16, i16 } } } } %insert7, 1, 0, 0, 0
  %extract7 = extractvalue { { i64 }, { { { i16, i16 } } } } %insert7, 1, 0, 0, 1
  %dep5 = add i64 %extract5, %extract5
  %dep6 = add i16 %extract6, %extract7

  %extract8 = extractvalue [2 x i64] [i64 32, i64 48], 1
  %extract9 = extractvalue [2 x i64] [i64 32, i64 48], 0
  %dep8 = add i64 %extract8, %extract9

  ret i8 5
}

attributes #0 = { noinline norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
