; ModuleID = 'free.bc'
source_filename = "free.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%str = type { i32 , i32 }
%str3 = type { i8 , i8 * }

@foo = local_unnamed_addr constant %str { i32 1000 , i32 900 }

%str1 = type { i8 , i8 * }

@baz = local_unnamed_addr constant i8 50
@bar = local_unnamed_addr constant %str1 { i8 200 , i8 * @baz }

%str2 = type { i32, %str * }

@qux = local_unnamed_addr constant %str { i32 200, i32 500 }
@pugs = local_unnamed_addr constant %str2 { i32 200,  %str * @qux }

%vect = type <1 x i5>

%vstruct = type { %vect }

; Function Attrs: noinline norecurse nounwind readonly sspstrong uwtable
define i32 @simple(i32 nocapture readonly) local_unnamed_addr #0 {
  %gep = getelementptr %str, %str * @foo, i32 0, i32 1
  %value = load i32, i32 * %gep
  %dep = add i32 %value, %value

  %bad = alloca { i8, i64, i8, i64 }
  %bada = getelementptr { i8, i64, i8, i64 }, { i8, i64, i8, i64 }* %bad, i32 0, i32 0
  %badb = getelementptr { i8, i64, i8, i64 }, { i8, i64, i8, i64 }* %bad, i32 0, i32 1
  %badc = getelementptr { i8, i64, i8, i64 }, { i8, i64, i8, i64 }* %bad, i32 0, i32 2
  %badd = getelementptr { i8, i64, i8, i64 }, { i8, i64, i8, i64 }* %bad, i32 0, i32 3
  store i8 18, i8* %bada
  store i8 2, i8* %badc
  store i64 9, i64* %badb
  store i64 1, i64* %badd
  %badl = load i8, i8* %bada
  %badll = load i8, i8* %badc
  %badlll = load i64, i64* %badb
  %badllll = load i64, i64* %badd 
  %baddep1 = add i8 %badl, %badl
  %baddep2 = add i8 %badll, %badll  
  %baddep3 = add i64 %badlll, %badlll
  %baddep4 = add i64 %badllll, %badllll  

  %gep1 = getelementptr %str1, %str1 * @bar, i32 0, i32 1
  %ptr1 = load i8*, i8 ** %gep1
  %value1 = load i8, i8 * %ptr1
  %dep1 = add i8 %value1, %value1

  ; ; note that this is NOT a nested gep
  %gep2 = getelementptr %str2, %str2 * @pugs, i32 0, i32 1
  %gep3 = getelementptr %str, %str * @qux, i32 0, i32 0
  %value2 = load i32, i32* %gep3
  %dep2 = add i32 %value2, %value2

  %iv = insertvalue { i32 } undef, i32 25, 0
  %ivptr = alloca { i32 }
  store { i32 } %iv, { i32 } * %ivptr
  %gep4 = getelementptr { i32 }, { i32 } * %ivptr, i32 0, i32 0
  %value3 = load i32, i32 * %gep4
  %dep3 = add i32 %value3, %value3

  %ivptr2 = alloca { i9 }
  %gep5 = getelementptr { i9 }, { i9 } * %ivptr2, i32 0, i32 0
  store i9 50, i9 * %gep5
  %ev = load { i9 }, { i9 } * %ivptr2
  %value4 = extractvalue { i9 } %ev, 0
  %dep4 = add i9 %value4, %value4

  %dptr = alloca { { i4 } } 
  %gep6 = getelementptr { { i4 } }, { { i4 } } * %dptr, i32 0, i32 0, i32 0
  store i4 8, i4 * %gep6
  %dload = load { { i4 } }, { { i4 } } * %dptr
  %dev = extractvalue { { i4 } } %dload, 0
  %value5 = extractvalue { i4 } %dev, 0
  %dep5 = add i4 %value5, %value5

  %gep7 = getelementptr { { i4 } }, { { i4 } } * %dptr, i32 0, i32 0
  %gep8 = getelementptr { i4 }, { i4 } * %gep7, i32 0, i32 0
  store i4 2, i4 * %gep8
  %dload2 = load { { i4 } }, { { i4 } } * %dptr
  %dev2 = extractvalue { { i4 } } %dload2, 0
  %value6 = extractvalue { i4 } %dev2, 0
  %dep6 = add i4 %value6, %value6

  %q = alloca { i32 * }
  %innerp = alloca i32 
  %gep9 = getelementptr { i32 * }, { i32 * } * %q, i32 0, i32 0
  store i32 * %innerp, i32 ** %gep9
  %w = load i32 *, i32 ** %gep9
  store i32 1000, i32 * %w
  %t = load i32 *, i32 ** %gep9
  %y = load i32, i32 * %t
  %dep7 = add i32 %y, %y
  %m = load { i32 * }, { i32 * } * %q
  %r = extractvalue { i32 * } %m, 0
  %k = load i32, i32 * %r
  %dep8 = add i32 %k, %k

  %vs = alloca %vstruct
  %ogvect = insertelement <1 x i5> undef, i5 4, i32 0
  %gep10 = getelementptr %vstruct, %vstruct * %vs, i32 0, i32 0
  store <1 x i5> %ogvect, <1 x i5> * %gep10
  %vresult = load <1 x i5>, <1 x i5> * %gep10
  %val = extractelement <1 x i5> %vresult, i32 0
  %dep9 = add i5 %val, %val 

  %arr = alloca i7
  %idx = getelementptr i7, i7 * %arr, i32 4
  store i7 50, i7 * %idx
  %arrval = load i7, i7 * %idx
  %dep10 = add i7 %arrval, %arrval

  ret i32 100
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
