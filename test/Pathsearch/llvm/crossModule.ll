; ModuleID = 'src/crossModule.c'
source_filename = "src/crossModule.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @simpleCrossModuleCaller(i32, i32) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  store i32 %1, i32* %4, align 4
  %6 = load i32, i32* %3, align 4
  %7 = load i32, i32* %4, align 4
  %8 = load i32, i32* %3, align 4
  %9 = load i32, i32* %4, align 4
  %10 = add nsw i32 %8, %9
  %11 = call i32 @foo(i32 %6, i32 %7, i32 %10)
  store i32 %11, i32* %5, align 4
  %12 = load i32, i32* %5, align 4
  %13 = add nsw i32 %12, 3
  ret i32 %13
}

declare i32 @foo(i32, i32, i32) #1

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @multipleCrossModuleCaller(i32, i32) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 %0, i32* %4, align 4
  store i32 %1, i32* %5, align 4
  %6 = load i32, i32* %4, align 4
  %7 = load i32, i32* %5, align 4
  %8 = load i32, i32* %4, align 4
  %9 = load i32, i32* %5, align 4
  %10 = sub nsw i32 %8, %9
  %11 = call i32 @foo(i32 %6, i32 %7, i32 %10)
  %12 = icmp sgt i32 %11, 11
  br i1 %12, label %13, label %24

; <label>:13:                                     ; preds = %2
  %14 = load i32, i32* %4, align 4
  %15 = load i32, i32* %5, align 4
  %16 = add nsw i32 %14, %15
  %17 = load i32, i32* %4, align 4
  %18 = load i32, i32* %5, align 4
  %19 = sub nsw i32 %17, %18
  %20 = load i32, i32* %4, align 4
  %21 = load i32, i32* %5, align 4
  %22 = mul nsw i32 %20, %21
  %23 = call i32 @foo(i32 %16, i32 %19, i32 %22)
  store i32 %23, i32* %3, align 4
  br label %49

; <label>:24:                                     ; preds = %2
  %25 = load i32, i32* %5, align 4
  %26 = load i32, i32* %4, align 4
  %27 = call i32 @foo(i32 %25, i32 %26, i32 2)
  %28 = icmp sgt i32 %27, 13
  br i1 %28, label %29, label %40

; <label>:29:                                     ; preds = %24
  %30 = load i32, i32* %4, align 4
  %31 = load i32, i32* %5, align 4
  %32 = sub nsw i32 %30, %31
  %33 = load i32, i32* %4, align 4
  %34 = load i32, i32* %5, align 4
  %35 = add nsw i32 %33, %34
  %36 = load i32, i32* %4, align 4
  %37 = load i32, i32* %5, align 4
  %38 = srem i32 %36, %37
  %39 = call i32 @foo(i32 %32, i32 %35, i32 %38)
  store i32 %39, i32* %3, align 4
  br label %49

; <label>:40:                                     ; preds = %24
  %41 = load i32, i32* %5, align 4
  %42 = add nsw i32 %41, 2
  %43 = load i32, i32* %4, align 4
  %44 = mul nsw i32 %43, 2
  %45 = load i32, i32* %5, align 4
  %46 = load i32, i32* %4, align 4
  %47 = sdiv i32 %45, %46
  %48 = call i32 @foo(i32 %42, i32 %44, i32 %47)
  store i32 %48, i32* %3, align 4
  br label %49

; <label>:49:                                     ; preds = %40, %29, %13
  %50 = load i32, i32* %3, align 4
  ret i32 %50
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @crossModuleCallerCaller(i32, i32) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  store i32 %1, i32* %4, align 4
  %5 = load i32, i32* %3, align 4
  %6 = load i32, i32* %4, align 4
  %7 = call i32 @simpleCrossModuleCaller(i32 %5, i32 %6)
  %8 = load i32, i32* %4, align 4
  %9 = load i32, i32* %3, align 4
  %10 = call i32 @multipleCrossModuleCaller(i32 %8, i32 %9)
  %11 = sub nsw i32 %7, %10
  ret i32 %11
}

attributes #0 = { noinline nounwind optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 7.0.0 (tags/RELEASE_700/final)"}
