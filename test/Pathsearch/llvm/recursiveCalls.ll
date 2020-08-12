; ModuleID = 'test/Pathsearch/src/recursiveCalls.c'
source_filename = "test/Pathsearch/src/recursiveCalls.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @simple(i32) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  %5 = load i32, i32* %3, align 4
  %6 = add nsw i32 %5, 2
  store i32 %6, i32* %4, align 4
  %7 = load i32, i32* %4, align 4
  %8 = icmp sgt i32 %7, 25
  br i1 %8, label %9, label %11

; <label>:9:                                      ; preds = %1
  %10 = load i32, i32* %4, align 4
  store i32 %10, i32* %2, align 4
  br label %14

; <label>:11:                                     ; preds = %1
  %12 = load i32, i32* %4, align 4
  %13 = call i32 @simple(i32 %12)
  store i32 %13, i32* %2, align 4
  br label %14

; <label>:14:                                     ; preds = %11, %9
  %15 = load i32, i32* %2, align 4
  ret i32 %15
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @moreBlocks(i32) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  %7 = load i32, i32* %3, align 4
  %8 = add nsw i32 %7, 2
  store i32 %8, i32* %4, align 4
  %9 = load i32, i32* %4, align 4
  %10 = srem i32 %9, 3
  %11 = icmp eq i32 %10, 0
  br i1 %11, label %12, label %13

; <label>:12:                                     ; preds = %1
  store i32 1, i32* %5, align 4
  br label %20

; <label>:13:                                     ; preds = %1
  %14 = load i32, i32* %4, align 4
  %15 = srem i32 %14, 3
  %16 = icmp eq i32 %15, 1
  br i1 %16, label %17, label %18

; <label>:17:                                     ; preds = %13
  store i32 5, i32* %5, align 4
  br label %19

; <label>:18:                                     ; preds = %13
  store i32 11, i32* %5, align 4
  br label %19

; <label>:19:                                     ; preds = %18, %17
  br label %20

; <label>:20:                                     ; preds = %19, %12
  store i32 0, i32* %6, align 4
  br label %21

; <label>:21:                                     ; preds = %27, %20
  %22 = load i32, i32* %6, align 4
  %23 = icmp slt i32 %22, 2
  br i1 %23, label %24, label %30

; <label>:24:                                     ; preds = %21
  %25 = load i32, i32* %4, align 4
  %26 = add nsw i32 %25, 1
  store i32 %26, i32* %4, align 4
  br label %27

; <label>:27:                                     ; preds = %24
  %28 = load i32, i32* %6, align 4
  %29 = add nsw i32 %28, 1
  store i32 %29, i32* %6, align 4
  br label %21

; <label>:30:                                     ; preds = %21
  %31 = load i32, i32* %4, align 4
  %32 = load i32, i32* %5, align 4
  %33 = mul nsw i32 %31, %32
  %34 = icmp sgt i32 %33, 242
  br i1 %34, label %35, label %39

; <label>:35:                                     ; preds = %30
  %36 = load i32, i32* %4, align 4
  %37 = load i32, i32* %5, align 4
  %38 = sdiv i32 %36, %37
  store i32 %38, i32* %2, align 4
  br label %42

; <label>:39:                                     ; preds = %30
  %40 = load i32, i32* %4, align 4
  %41 = call i32 @moreBlocks(i32 %40)
  store i32 %41, i32* %2, align 4
  br label %42

; <label>:42:                                     ; preds = %39, %35
  %43 = load i32, i32* %2, align 4
  ret i32 %43
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @notTailRec(i32) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  %5 = load i32, i32* %3, align 4
  %6 = icmp sgt i32 %5, 7
  br i1 %6, label %7, label %10

; <label>:7:                                      ; preds = %1
  %8 = load i32, i32* %3, align 4
  %9 = add nsw i32 %8, 10
  store i32 %9, i32* %2, align 4
  br label %22

; <label>:10:                                     ; preds = %1
  %11 = load i32, i32* %3, align 4
  %12 = sub nsw i32 %11, 2
  %13 = call i32 @notTailRec(i32 %12)
  store i32 %13, i32* %4, align 4
  %14 = load i32, i32* %4, align 4
  %15 = icmp sgt i32 %14, 0
  br i1 %15, label %16, label %19

; <label>:16:                                     ; preds = %10
  %17 = load i32, i32* %4, align 4
  %18 = mul nsw i32 %17, 2
  store i32 %18, i32* %2, align 4
  br label %22

; <label>:19:                                     ; preds = %10
  %20 = load i32, i32* %4, align 4
  %21 = add nsw i32 %20, 5
  store i32 %21, i32* %2, align 4
  br label %22

; <label>:22:                                     ; preds = %19, %16, %7
  %23 = load i32, i32* %2, align 4
  ret i32 %23
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @mutRecursiveA(i32) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  %4 = load i32, i32* %3, align 4
  %5 = icmp sgt i32 %4, 100
  br i1 %5, label %6, label %8

; <label>:6:                                      ; preds = %1
  %7 = load i32, i32* %3, align 4
  store i32 %7, i32* %2, align 4
  br label %12

; <label>:8:                                      ; preds = %1
  %9 = load i32, i32* %3, align 4
  %10 = add nsw i32 %9, 11
  %11 = call i32 @mutRecursiveB(i32 %10)
  store i32 %11, i32* %2, align 4
  br label %12

; <label>:12:                                     ; preds = %8, %6
  %13 = load i32, i32* %2, align 4
  ret i32 %13
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @mutRecursiveB(i32) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  %4 = load i32, i32* %3, align 4
  %5 = icmp slt i32 %4, 66
  br i1 %5, label %6, label %8

; <label>:6:                                      ; preds = %1
  %7 = load i32, i32* %3, align 4
  store i32 %7, i32* %2, align 4
  br label %12

; <label>:8:                                      ; preds = %1
  %9 = load i32, i32* %3, align 4
  %10 = sdiv i32 %9, 2
  %11 = call i32 @mutRecursiveA(i32 %10)
  store i32 %11, i32* %2, align 4
  br label %12

; <label>:12:                                     ; preds = %8, %6
  %13 = load i32, i32* %2, align 4
  ret i32 %13
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @threeWayMutRecursiveA(i32) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  %4 = load i32, i32* %3, align 4
  %5 = srem i32 %4, 5
  %6 = icmp eq i32 %5, 0
  br i1 %6, label %7, label %9

; <label>:7:                                      ; preds = %1
  %8 = load i32, i32* %3, align 4
  store i32 %8, i32* %2, align 4
  br label %13

; <label>:9:                                      ; preds = %1
  %10 = load i32, i32* %3, align 4
  %11 = add nsw i32 %10, 1
  %12 = call i32 @threeWayMutRecursiveB(i32 %11)
  store i32 %12, i32* %2, align 4
  br label %13

; <label>:13:                                     ; preds = %9, %7
  %14 = load i32, i32* %2, align 4
  ret i32 %14
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @threeWayMutRecursiveB(i32) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  %4 = load i32, i32* %3, align 4
  %5 = icmp sgt i32 %4, 124
  br i1 %5, label %6, label %9

; <label>:6:                                      ; preds = %1
  %7 = load i32, i32* %3, align 4
  %8 = sub nsw i32 %7, 2
  store i32 %8, i32* %2, align 4
  br label %13

; <label>:9:                                      ; preds = %1
  %10 = load i32, i32* %3, align 4
  %11 = add nsw i32 %10, 4
  %12 = call i32 @threeWayMutRecursiveC(i32 %11)
  store i32 %12, i32* %2, align 4
  br label %13

; <label>:13:                                     ; preds = %9, %6
  %14 = load i32, i32* %2, align 4
  ret i32 %14
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @threeWayMutRecursiveC(i32) #0 {
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = load i32, i32* %2, align 4
  %4 = add nsw i32 %3, 2
  %5 = call i32 @threeWayMutRecursiveA(i32 %4)
  ret i32 %5
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @notRecursive(i32, i32) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  store i32 %1, i32* %4, align 4
  %5 = load i32, i32* %3, align 4
  %6 = load i32, i32* %4, align 4
  %7 = mul nsw i32 %6, 3
  %8 = add nsw i32 %5, %7
  ret i32 %8
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @nestedRecursive(i32) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  %5 = load i32, i32* %3, align 4
  %6 = call i32 @notRecursive(i32 3, i32 %5)
  store i32 %6, i32* %4, align 4
  %7 = load i32, i32* %4, align 4
  %8 = icmp sgt i32 %7, 100
  br i1 %8, label %9, label %11

; <label>:9:                                      ; preds = %1
  %10 = load i32, i32* %4, align 4
  store i32 %10, i32* %2, align 4
  br label %14

; <label>:11:                                     ; preds = %1
  %12 = load i32, i32* %4, align 4
  %13 = call i32 @nestedRecursive(i32 %12)
  store i32 %13, i32* %2, align 4
  br label %14

; <label>:14:                                     ; preds = %11, %9
  %15 = load i32, i32* %2, align 4
  ret i32 %15
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @doubleRecursive(i32, i32) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  store i32 %0, i32* %4, align 4
  store i32 %1, i32* %5, align 4
  %8 = load i32, i32* %4, align 4
  %9 = load i32, i32* %5, align 4
  %10 = sub nsw i32 %8, %9
  %11 = icmp sgt i32 %10, 100
  br i1 %11, label %12, label %16

; <label>:12:                                     ; preds = %2
  %13 = load i32, i32* %4, align 4
  %14 = load i32, i32* %5, align 4
  %15 = add nsw i32 %13, %14
  store i32 %15, i32* %3, align 4
  br label %29

; <label>:16:                                     ; preds = %2
  %17 = load i32, i32* %4, align 4
  %18 = load i32, i32* %5, align 4
  %19 = sub nsw i32 %18, 1
  %20 = call i32 @doubleRecursive(i32 %17, i32 %19)
  %21 = mul nsw i32 2, %20
  store i32 %21, i32* %6, align 4
  %22 = load i32, i32* %6, align 4
  %23 = load i32, i32* %4, align 4
  %24 = call i32 @doubleRecursive(i32 %22, i32 %23)
  %25 = add nsw i32 3, %24
  store i32 %25, i32* %7, align 4
  %26 = load i32, i32* %6, align 4
  %27 = load i32, i32* %7, align 4
  %28 = sub nsw i32 %26, %27
  store i32 %28, i32* %3, align 4
  br label %29

; <label>:29:                                     ; preds = %16, %12
  %30 = load i32, i32* %3, align 4
  ret i32 %30
}

attributes #0 = { noinline nounwind optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.1 (tags/RELEASE_601/final)"}
