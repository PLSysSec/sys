; ModuleID = 'src/simple1.c'
source_filename = "src/simple1.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @foo(i32, i32, i32) #0 {
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca i32, align 4
  store i32 %0, i32* %5, align 4
  store i32 %1, i32* %6, align 4
  store i32 %2, i32* %7, align 4
  %9 = load i32, i32* %5, align 4
  %10 = load i32, i32* %6, align 4
  %11 = icmp sgt i32 %9, %10
  br i1 %11, label %12, label %13

; <label>:12:                                     ; preds = %3
  store i32 3, i32* %4, align 4
  br label %25

; <label>:13:                                     ; preds = %3
  %14 = load i32, i32* %5, align 4
  %15 = icmp sgt i32 %14, 55
  br i1 %15, label %16, label %19

; <label>:16:                                     ; preds = %13
  %17 = load i32, i32* %6, align 4
  %18 = mul nsw i32 %17, 3
  store i32 %18, i32* %4, align 4
  br label %25

; <label>:19:                                     ; preds = %13
  %20 = load i32, i32* %7, align 4
  %21 = add nsw i32 %20, 53
  store i32 %21, i32* %8, align 4
  %22 = load i32, i32* %8, align 4
  %23 = load i32, i32* %5, align 4
  %24 = mul nsw i32 %22, %23
  store i32 %24, i32* %4, align 4
  br label %25

; <label>:25:                                     ; preds = %19, %16, %12
  %26 = load i32, i32* %4, align 4
  ret i32 %26
}

attributes #0 = { noinline nounwind optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.1 (tags/RELEASE_601/final)"}
