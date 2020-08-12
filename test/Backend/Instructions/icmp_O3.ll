; ModuleID = 'add.bc'
source_filename = "add.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline norecurse nounwind readnone sspstrong uwtable
define i1 @or(i32, i32) local_unnamed_addr #0 {
  %3 = icmp eq i32 0, 0
  %4 = icmp ne i32 0, 0
  %5 = icmp ugt i8 255, 1
  %6 = icmp uge i8 256, 0
  %7 = icmp ult i8 255, 1
  %8 = icmp ule i8 256, 1
  %9 = icmp sgt i8 255, 1
  %10 = icmp sge i8 240, 240
  %11 = icmp slt i8 254, 1
  %12 = icmp sle i8 254, 1
  %13 = or i1 %3, %4
  %14 = or i1 %5, %6
  %15 = or i1 %7, %8
  %16 = or i1 %9, %10 
  %17 = or i1 %11, %12
  ret i1 %3
}


attributes #0 = { noinline norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
