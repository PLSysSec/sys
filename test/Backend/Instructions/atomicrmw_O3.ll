; ModuleID = 'add.bc'
source_filename = "add.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline norecurse nounwind readnone sspstrong uwtable
define i64 @or(i32, i32) local_unnamed_addr #0 {
  %3 = alloca i64
  store i64 0, i64* %3

  ; xchg: *ptr = val
  %4 = atomicrmw xchg i64* %3, i64 100 acquire ; old value = 0
  %5 = load i64, i64* %3 ; %5 = xchg 0 100 = 100
  %6 = and i64 %4, %5

  ; add: *ptr = *ptr + val
  %7 = atomicrmw add i64* %3, i64 100 acquire ; old value = 100
  %8 = load i64, i64* %3 ; %8 = add 100 100 = 200
  %9 = and i64 %7, %8

  ; sub: *ptr = *ptr - val
  %10 = atomicrmw sub i64* %3, i64 200 acquire ; old value = 200
  %11 = load i64, i64* %3 ; %11 = sub 200 200 = 0
  %12 = and i64 %10, %11
  
  ; and: *ptr = *ptr & val
  %13 = atomicrmw and i64* %3, i64 1 acquire ; old value = 0
  %14 = load i64, i64* %3 ; %14 = and 0 1 = 0
  %15 = and i64 %13, %14
    
  ; nand: *ptr = ~(*ptr & val)
  %16 = atomicrmw nand i64* %3, i64 0 acquire ; old value = 0
  %17 = load i64, i64* %3 ; %17 = nand i64 0 i64 0 = 1 
  %18 = and i64 %16, %17
  
  ; or: *ptr = *ptr | val
  %19 = atomicrmw or i64* %3, i64 0 acquire ; old value = 18446744073709551615
  %20 = load i64, i64* %3 ; %20 = or 18446744073709551615 0 = 18446744073709551615)
  %21 = and i64 %19, %20
  
  ; xor: *ptr = *ptr ^ val
  %22 = atomicrmw or i64* %3, i64 0 acquire ; old value = 18446744073709551615
  %23 = load i64, i64* %3 ; %23 = xor 18446744073709551615 0 = 18446744073709551615
  %24 = and i64 %22, %23

  ; max: *ptr = *ptr > val ? *ptr : val (using a signed comparison)
  %25 = atomicrmw max i64* %3, i64 1 acquire ; old value = 18446744073709551615
  %26 = load i64, i64* %3 ; %26 = max 18446744073709551615 1 = 1
  %27 = and i64 %25, %26

  ; min: *ptr = *ptr < val ? *ptr : val (using a signed comparison)
  %28 = atomicrmw min i64* %3, i64 18446744073709551615  acquire ; old value = 1
  %29 = load i64, i64* %3 ; %29 = min 18446744073709551615 1 = 18446744073709551615
  %30 = and i64 %28, %29

  ; umax: *ptr = *ptr > val ? *ptr : val (using an unsigned comparison)
  %31 = atomicrmw umax i64* %3, i64 1 acquire ; old value = 18446744073709551615
  %32 = load i64, i64* %3 ; %32 = umax 1 18446744073709551615 = 18446744073709551615
  %33 = and i64 %32, %31

  ; umin: *ptr = *ptr < val ? *ptr : val (using an unsigned comparison)
  %34 = atomicrmw umin i64* %3, i64 1 acquire ; old value = 18446744073709551615
  %35 = load i64, i64* %3 ; %32 = umin 1 18446744073709551615 = 1
  %36 = and i64 %34, %35

  ret i64 %4
}


attributes #0 = { noinline norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
