; ModuleID = 'add.bc'
source_filename = "add.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline norecurse nounwind readnone sspstrong uwtable
define i8 @add(i64) local_unnamed_addr #0 {

  ; integer sizes
  
  %one = trunc i64 %0 to i1
  %depone = or i1 %one, %one 
  %eight = trunc i64 %0 to i8
  %depeight = and i8 %eight, %eight
  %twelve = trunc i64 %0 to i12
  %deptwelve = add i12 %twelve, %twelve
  %sixteen = trunc i64 %0 to i16
  %depsixteen = add i16 %sixteen, %sixteen
  %twenty = trunc i64 %0 to i20
  %deptwenty = add i20 %twenty, %twenty
  %thirtytwo = trunc i64 %0 to i32
  %depthirtytwo = add i32 %thirtytwo, %thirtytwo
  %forty = trunc i64 %0 to i40
  %depforty = add i40 %forty, %forty
  %sixtyfour = add i64 %0, %0
  %depsixtyfour = add i64 %sixtyfour, %sixtyfour

  ; ; arrays, structs, and vectors of int types

  %array = insertvalue [4 x i32] undef, i32 1, 3
  %dep = extractvalue [4 x i32] %array, 2

  %array2 = insertvalue [5 x i35] undef, i35 1, 3
  %dep2 = extractvalue [5 x i35] %array2, 2

  %vector = insertelement <2 x i1> undef, i1 0, i32 0
  %depv = extractelement <2 x i1> %vector, i32 1

  %struct = insertvalue { i32, i32 } undef, i32 1, 0
  %deps = extractvalue { i32, i32 } %struct, 0

  %struct2 = insertvalue { { { { { i1 } } } } } undef, i1 0, 0, 0, 0, 0, 0
  %deps2 = extractvalue { { { { { i1 } } } } } %struct2, 0, 0, 0, 0, 0

  %struct3 = insertvalue { i8, i64 } undef, i64 0, 1
  %deps3 = extractvalue { i8, i64 } %struct3, 1

  %struct4 = insertvalue { i8, i64, i32, i64 } undef, i64 0, 1
  %deps4 = extractvalue { i8, i64, i32, i64 } %struct4, 1

  ; richer arrays, structs, etc

  %struct5 = insertvalue { { i32, i32, i32 } } undef, i32 4, 0, 1
  %deps5 = extractvalue { { i32, i32, i32 } } %struct5, 0, 2 	  

  %struct6 = insertvalue { { i32 }, { i32 } } undef, i32 4, 0, 0
  %dep6 = extractvalue { { i32 }, { i32 } } %struct6, 0, 0

  %struct7 = insertvalue { <2 x i10>, <2 x i10> } undef, <2 x i10> undef, 0
  %dep7 = extractvalue { <2 x i10>, <2 x i10> } %struct7, 0

  %struct8 = insertvalue { <5 x i32> } undef, <5 x i32> undef, 0
  %dep8= extractvalue { <5 x i32> } %struct8, 0

  %struct9 = insertvalue { i32, i16 } undef, i16 1, 1
  %dep9 = extractvalue { i32, i16} %struct9, 0

  %vector10 = insertelement < 3 x i16 > undef, i16 2, i32 2
  %dep10 = extractelement < 3 x i16 > %vector10, i32 1

  %array11 = insertvalue [2 x { i32, i16 }] undef, { i32, i16 } undef, 0
  %dep11 = extractvalue [2 x { i32, i16 }] %array11, 0

  %struct12 = insertvalue { [2 x { i32, i16 } ] } undef, [2 x { i32, i16 } ] undef, 0
  %dep12 = extractvalue { [2 x { i32, i16 } ] } %struct12, 0

  %array13 = insertvalue [2 x { i32, i32, i32}] undef, { i32, i32, i32 } undef, 0
  %dep13 = extractvalue [2 x { i32, i32, i32}] %array13, 1

  %struct14 = insertvalue { < 3 x i16 >, <3 x i16> } undef, <3 x i16> undef, 0
  %dep14 = extractvalue { < 3 x i16 >, <3 x i16> } %struct14, 1

  ; more from klee tests
  %struct15 = insertvalue { i8, i16, i32, i64 } undef, i8 0, 0
  %dep15 = extractvalue { i8, i16, i32, i64 } %struct15, 0

  %struct16 = insertvalue { i8, i16, i16, i8, i64 } undef, i8 0, 0
  %dep16 = extractvalue { i8, i16, i16, i8, i64 } %struct16, 0  

  %struct17 = insertvalue { i8, i16, i16, i8, i64 } undef, i8 0, 0
  %dep17 = extractvalue { i8, i16, i16, i8, i64 } %struct16, 0  

  ret i8 5
}

attributes #0 = { noinline norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
