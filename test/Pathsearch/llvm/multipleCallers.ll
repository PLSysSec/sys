; ModuleID = 'src/multipleCallers.c'
source_filename = "src/multipleCallers.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @foo(i32, i32, i32) #0 {
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  store i32 %0, i32* %5, align 4
  store i32 %1, i32* %6, align 4
  store i32 %2, i32* %7, align 4
  %8 = load i32, i32* %5, align 4
  %9 = load i32, i32* %6, align 4
  %10 = icmp slt i32 %8, %9
  br i1 %10, label %11, label %15

; <label>:11:                                     ; preds = %3
  %12 = load i32, i32* %5, align 4
  %13 = load i32, i32* %7, align 4
  %14 = add nsw i32 %12, %13
  store i32 %14, i32* %4, align 4
  br label %19

; <label>:15:                                     ; preds = %3
  %16 = load i32, i32* %5, align 4
  %17 = load i32, i32* %7, align 4
  %18 = sub nsw i32 %16, %17
  store i32 %18, i32* %4, align 4
  br label %19

; <label>:19:                                     ; preds = %15, %11
  %20 = load i32, i32* %4, align 4
  ret i32 %20
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @caller1(i32) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  %4 = load i32, i32* %3, align 4
  %5 = icmp sgt i32 %4, 10
  br i1 %5, label %6, label %11

; <label>:6:                                      ; preds = %1
  %7 = load i32, i32* %3, align 4
  %8 = load i32, i32* %3, align 4
  %9 = srem i32 %8, 17
  %10 = call i32 @foo(i32 %7, i32 %9, i32 2)
  store i32 %10, i32* %2, align 4
  br label %16

; <label>:11:                                     ; preds = %1
  %12 = load i32, i32* %3, align 4
  %13 = load i32, i32* %3, align 4
  %14 = sub nsw i32 10, %13
  %15 = call i32 @foo(i32 %12, i32 %14, i32 3)
  store i32 %15, i32* %2, align 4
  br label %16

; <label>:16:                                     ; preds = %11, %6
  %17 = load i32, i32* %2, align 4
  ret i32 %17
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @caller2(i32) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  %4 = load i32, i32* %3, align 4
  %5 = icmp sgt i32 %4, 12
  br i1 %5, label %6, label %13

; <label>:6:                                      ; preds = %1
  %7 = load i32, i32* %3, align 4
  %8 = load i32, i32* %3, align 4
  %9 = and i32 %8, 64206
  %10 = load i32, i32* %3, align 4
  %11 = sdiv i32 %10, 2
  %12 = call i32 @foo(i32 %7, i32 %9, i32 %11)
  store i32 %12, i32* %2, align 4
  br label %20

; <label>:13:                                     ; preds = %1
  %14 = load i32, i32* %3, align 4
  %15 = load i32, i32* %3, align 4
  %16 = sub nsw i32 12, %15
  %17 = load i32, i32* %3, align 4
  %18 = mul nsw i32 %17, 3
  %19 = call i32 @foo(i32 %14, i32 %16, i32 %18)
  store i32 %19, i32* %2, align 4
  br label %20

; <label>:20:                                     ; preds = %13, %6
  %21 = load i32, i32* %2, align 4
  ret i32 %21
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @callercaller(i32, i32) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 %0, i32* %4, align 4
  store i32 %1, i32* %5, align 4
  %6 = load i32, i32* %4, align 4
  %7 = call i32 @caller1(i32 %6)
  %8 = icmp sgt i32 %7, 5
  br i1 %8, label %9, label %13

; <label>:9:                                      ; preds = %2
  %10 = load i32, i32* %5, align 4
  %11 = call i32 @caller2(i32 %10)
  %12 = add nsw i32 %11, 3
  store i32 %12, i32* %3, align 4
  br label %17

; <label>:13:                                     ; preds = %2
  %14 = load i32, i32* %5, align 4
  %15 = call i32 @caller2(i32 %14)
  %16 = mul nsw i32 %15, 5
  store i32 %16, i32* %3, align 4
  br label %17

; <label>:17:                                     ; preds = %13, %9
  %18 = load i32, i32* %3, align 4
  ret i32 %18
}

attributes #0 = { noinline nounwind optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 7.0.0 (tags/RELEASE_700/final)"}
