; ModuleID = 'free2.bc'
source_filename = "free2.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.apoint = type { i32, %struct.inner* }
%struct.inner = type { i32 }

; Function Attrs: noinline norecurse nounwind sspstrong uwtable

define i32 @storeToApoint(%struct.apoint* nocapture) local_unnamed_addr #0 {
  %2 = getelementptr inbounds %struct.apoint, %struct.apoint* %0, i64 0, i32 0
  store i32 0, i32* %2, align 8, !tbaa !4
  %3 = getelementptr inbounds %struct.apoint, %struct.apoint* %0, i64 0, i32 1
  %4 = load %struct.inner*, %struct.inner** %3, align 8, !tbaa !10
  %5 = getelementptr inbounds %struct.inner, %struct.inner* %4, i64 0, i32 0
  store i32 1, i32* %5, align 4, !tbaa !4
  ret i32 0
  }

attributes #0 = { noinline norecurse nounwind sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
!4 = !{!5, !6, i64 0}
!5 = !{!"apoint", !6, i64 0, !6, i64 4, !6, i64 8}
!6 = !{!"int", !7, i64 0}
!7 = !{!"omnipotent char", !8, i64 0}
!8 = !{!"Simple C/C++ TBAA"}
!9 = !{!5, !6, i64 4}
!10 = !{!5, !6, i64 8}
