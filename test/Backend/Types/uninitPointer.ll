; ModuleID = 'add.bc'
source_filename = "add.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline norecurse nounwind readnone sspstrong uwtable
define i8 @add(i32 *, i32 *) local_unnamed_addr #0 {

  %alias = icmp eq i32 * %0, %1
  %dep = or i1 %alias, %alias

  %gep10 = getelementptr i32, i32 * %0, i32 1
  %gep11 = getelementptr i32, i32 * %1, i32 1
  %alias1 = icmp eq i32 * %gep10, %gep11
  %dep1 = or i1 %alias1, %alias1 

  %gep20 = getelementptr i32, i32 * %0, i32 2
  %gep21 = getelementptr i32, i32 * %1, i32 2
  %alias2 = icmp eq i32 * %gep20, %gep21
  %dep2 = or i1 %alias2, %alias2   

  %gep30 = getelementptr i32, i32 * %0, i32 3
  %gep31 = getelementptr i32, i32 * %1, i32 3
  %alias3 = icmp eq i32 * %gep30, %gep31
  %dep3 = or i1 %alias3, %alias3   

  %gep40 = getelementptr i32, i32 * %0, i32 4
  %gep41 = getelementptr i32, i32 * %1, i32 4
  %alias4 = icmp eq i32 * %gep40, %gep41
  %dep4 = or i1 %alias4, %alias4     

  %gep50 = getelementptr i32, i32 * %0, i32 5
  %gep51 = getelementptr i32, i32 * %1, i32 5
  %alias5 = icmp eq i32 * %gep50, %gep51
  %dep5 = or i1 %alias5, %alias5       
  
  ret i8 5
}

attributes #0 = { noinline norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
