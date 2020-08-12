; ModuleID = 'add.bc'
source_filename = "add.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%s = type <{ i8, i32, i64 }> ; 0, 1, 5
%t = type { i32, %s } ; 0, 4

; Function Attrs: noinline norecurse nounwind readnone sspstrong uwtable
define i32 @add(i32, i32) local_unnamed_addr #0 {
   %a = alloca %t
   
   %t = getelementptr %t, %t* %a, i32 0, i32 1, i32 1
   store i32 45, i32* %t
   %v = load i32, i32* %t
   %dep1 = add i32 %v, %v
   
   %x = getelementptr %t, %t* %a, i32 0, i32 1, i32 0
   store i8 127, i8* %x
   %w = load i8, i8* %x
   %dep2 = add i8 %w, %w

   %f = getelementptr %t, %t* %a, i32 0, i32 1, i32 2
   store i64 12345, i64* %f
   %b = load i64, i64* %f
   %dep3 = add i64 %b, %b
   
   ret i32 %v
}

attributes #0 = { noinline norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
