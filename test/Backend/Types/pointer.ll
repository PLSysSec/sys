; ModuleID = 'add.bc'
source_filename = "add.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline norecurse nounwind readnone sspstrong uwtable
define i8 @add(i32, i32) local_unnamed_addr #0 {

  ; correct rounding
  %alloca = alloca i8
  store i8 257, i8 * %alloca
  %result_0 = load i8, i8 * %alloca
  %dummy_0 = add i8 %result_0, %result_0

  ; ; triple pointers
  %ptr_0 = alloca i8 **
  %ptr_1 = alloca i8 *
  %ptr_2 = alloca i8
  store i8 100, i8 * %ptr_2
  store i8 * %ptr_2, i8 ** %ptr_1
  store i8 ** %ptr_1, i8 *** %ptr_0
  %val_0 = load i8 **, i8 *** %ptr_0
  %val_1 = load i8 *, i8 ** %ptr_1
  %val_2 = load i8, i8 * %ptr_2
  %dummy_1 = add i8 %val_2, %val_2

  ; ; pointers to vectors
  %vect_ptr = alloca <2 x i32>
  store <2 x i32> <i32 100, i32 200>, <2 x i32> * %vect_ptr
  %vect_vals = load <2 x i32>, <2 x i32> * %vect_ptr
  %vect_one = extractelement <2 x i32> %vect_vals, i64 1
  %dummy_2 = add i32 %vect_one, %vect_one 

  ; ; double pointer to vectors
  %vptr_0 = alloca <2 x i32> *
  %vptr_1 = alloca <2 x i32>
  store <2 x i32> <i32 4, i32 6>, <2 x i32> * %vptr_1
  store <2 x i32> * %vptr_1, <2 x i32> ** %vptr_0
  %vval_0 = load <2 x i32> *, <2 x i32> ** %vptr_0
  %vval_1 = load <2 x i32>, <2 x i32> * %vptr_1
  %result_3 = extractelement <2 x i32> %vval_1, i64 0
  %dummy_3 = add i32 %result_3, %result_3 
  
  ; ; pointer to struct 
  %struct_ptr = alloca { i16 }
  store { i16 } { i16 200 }, { i16 } * %struct_ptr
  %struct_vals = load { i16 }, { i16 } * %struct_ptr
  %sval = extractvalue { i16 } %struct_vals, 0
  %dummy_4 = add i16 %sval, %sval

  ; ; double pointer to struct
  %sptr_0 = alloca { i8 }
  %sptr_1 = alloca { i8 } *
  store { i8 } { i8 8 }, { i8 } * %sptr_0
  store { i8 } * %sptr_0, { i8 } ** %sptr_1
  %sval_1 = load { i8 } *, { i8 } ** %sptr_1
  %sval_0 = load { i8 }, { i8 } * %sptr_0 
  %innerval = extractvalue { i8 } %sval_0, 0
  %dummy_5 = add i8 %innerval, %innerval

  ; ; pointers in structures
  %p = alloca i16
  store i16 100, i16 * %p
  %q = insertvalue { i8, { i16 * } } undef, i16 * %p, 1, 0
  %e = extractvalue { i8, { i16 * } } %q, 1, 0
  %v = load i16, i16 * %e
  %dummy_6 = add i16 %v, %v

  ; ; pointers in vectors
  %a = alloca i8 
  store i8 200, i8 * %a
  %b = insertelement <2 x i8 *> undef, i8 * %a, i64 0
  %c = extractelement <2 x i8 *> %b, i64 0
  %d = load i8, i8 * %c 
  %dummy_7 = add i8 %d, %d

  ; ; null
  %nptr = alloca i64 *
  store i64 * null, i64 ** %nptr
  %null = load i64 *, i64 ** %nptr
  %nullval = ptrtoint i64 * %null to i32
  %dummy_8 = add i32 %nullval, %nullval

  ret i8 5
}

attributes #0 = { noinline norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
