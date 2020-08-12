; ModuleID = 'src/multiCallsInBlock.c'
source_filename = "src/multiCallsInBlock.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @diffCalls(i32, i32) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 %0, i32* %4, align 4
  store i32 %1, i32* %5, align 4
  %6 = load i32, i32* %4, align 4
  %7 = load i32, i32* %5, align 4
  %8 = icmp slt i32 %6, %7
  br i1 %8, label %9, label %20

; <label>:9:                                      ; preds = %2
  %10 = load i32, i32* %4, align 4
  %11 = call i32 @callee1(i32 %10)
  %12 = load i32, i32* %5, align 4
  %13 = call i32 @callee2(i32 %12)
  %14 = add nsw i32 %11, %13
  %15 = load i32, i32* %4, align 4
  %16 = load i32, i32* %5, align 4
  %17 = sub nsw i32 %15, %16
  %18 = call i32 @callee3(i32 %17)
  %19 = sub nsw i32 %14, %18
  store i32 %19, i32* %3, align 4
  br label %23

; <label>:20:                                     ; preds = %2
  %21 = load i32, i32* %5, align 4
  %22 = call i32 @callee1(i32 %21)
  store i32 %22, i32* %3, align 4
  br label %23

; <label>:23:                                     ; preds = %20, %9
  %24 = load i32, i32* %3, align 4
  ret i32 %24
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @callee1(i32) #0 {
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = load i32, i32* %2, align 4
  %4 = sdiv i32 %3, 2
  ret i32 %4
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @callee2(i32) #0 {
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = load i32, i32* %2, align 4
  %4 = srem i32 %3, 2
  ret i32 %4
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @callee3(i32) #0 {
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = load i32, i32* %2, align 4
  %4 = mul nsw i32 %3, 3
  ret i32 %4
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @sameCalls(i32, i32) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 %0, i32* %4, align 4
  store i32 %1, i32* %5, align 4
  %6 = load i32, i32* %4, align 4
  %7 = load i32, i32* %5, align 4
  %8 = icmp sgt i32 %6, %7
  br i1 %8, label %9, label %28

; <label>:9:                                      ; preds = %2
  %10 = load i32, i32* %4, align 4
  %11 = call i32 @callee1(i32 %10)
  %12 = load i32, i32* %5, align 4
  %13 = call i32 @callee1(i32 %12)
  %14 = add nsw i32 %11, %13
  %15 = load i32, i32* %4, align 4
  %16 = call i32 @callee2(i32 %15)
  %17 = add nsw i32 %14, %16
  %18 = load i32, i32* %4, align 4
  %19 = load i32, i32* %5, align 4
  %20 = add nsw i32 %18, %19
  %21 = call i32 @callee1(i32 %20)
  %22 = add nsw i32 %17, %21
  %23 = load i32, i32* %4, align 4
  %24 = load i32, i32* %5, align 4
  %25 = mul nsw i32 %23, %24
  %26 = call i32 @callee1(i32 %25)
  %27 = add nsw i32 %22, %26
  store i32 %27, i32* %3, align 4
  br label %33

; <label>:28:                                     ; preds = %2
  %29 = load i32, i32* %5, align 4
  %30 = load i32, i32* %4, align 4
  %31 = sub nsw i32 %29, %30
  %32 = call i32 @callee1(i32 %31)
  store i32 %32, i32* %3, align 4
  br label %33

; <label>:33:                                     ; preds = %28, %9
  %34 = load i32, i32* %3, align 4
  ret i32 %34
}

attributes #0 = { noinline nounwind optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 7.0.0 (tags/RELEASE_700/final)"}
