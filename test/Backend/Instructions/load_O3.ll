; ModuleID = 'add.bc'
source_filename = "add.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline norecurse nounwind readnone sspstrong uwtable
define i32 @load(i32, i32) local_unnamed_addr #0 {
  ; basic load and store
  %w3 = alloca i32
  store i32 3, i32* %w3 ; store to memory version 1 = memory version 0 
  %w4 = load i32, i32* %w3 ; load from memory version 0
  %depw = add i32 %w4, %w4
  ; vector load and store
  %w5 = insertelement <2 x i32> undef, i32 4, i32 0
  %w6 =  alloca <2 x i32>
  store <2 x i32> %w5, <2 x i32>* %w6
  %w7 = load <2 x i32>, <2 x i32>* %w6
  %w8 = extractelement <2 x i32> %w7, i32 0
  %w9 = and i32 %w8, %w8
  %w10 = alloca { i64, i64 }
  store { i64, i64 } { i64 12, i64 14 }, { i64, i64 } * %w10
  %w11 = load { i64, i64 }, { i64, i64 } * %w10
  %w12 = extractvalue { i64, i64 } %w11, 0
  %w13 = extractvalue { i64, i64 } %w11, 1
  %w14 = add i64 %w12, %w13

  ; make sure we arent stamping
  ; |...64....|..16.......|..16......|
  ; store to the i16 is stamping out the struct memory
  %w15 = alloca { i64, i16 }
  %w16 = alloca i8
  store { i64, i16 } { i64 5, i16 8 }, { i64, i16 } * %w15
  store i8 7, i8 * %w16
  %w17 = load i8, i8 * %w16 ; should be 7
  %w18 = load { i64, i16 }, { i64, i16 } * %w15
  %w19 = extractvalue { i64, i16 } %w18, 0
  %w20 = extractvalue { i64, i16 } %w18, 1
  %w21 = add i16 %w20, %w20
  %w22 = add i64 %w19, %w19
  %w300 = add i8 %w17, %w17

  ; make sure we can handle structs with two different size elements in em
  %w23 = alloca { i8, i16 }
  %w24 = getelementptr { i8, i16 }, { i8, i16} * %w23, i32 0, i32 0
  %w25 = getelementptr { i8, i16 }, { i8, i16 } *  %w23, i32 0, i32 1
  store i8 5, i8 * %w24
  store i16 5, i16 * %w25
  %w26 = load i8, i8 * %w24
  %w27 = load i16, i16 * %w25
  %w28 = add i8 %w26, %w26
  %w29 = add i16 %w27, %w27
  %ev1 = extractvalue { i64, i8 } { i64 5, i8 9 }, 1
  %ev2 = add i8 %ev1, %ev1 
  %a1 = alloca { i64, i8 }
  store { i64, i8 } { i64 5, i8 9 }, { i64, i8 }* %a1
  %a2 = load { i64, i8 }, { i64, i8 }* %a1
  %a3 = extractvalue { i64, i8 } %a2, 1
  %a4 = extractvalue { i64, i8 } %a2, 0  
  %a5 = add i8 %a3, %a3
  %a6 = add i64 %a4, %a4  
  ret i32 0
}


attributes #0 = { noinline norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}

;  store i32 3, i32* %3
;  %4 = load i32, i32* %3
;  ret i32 %4
