; ModuleID = 'add.bc'
source_filename = "add.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%t = type <1 x i8>

@foo = local_unnamed_addr constant %t <i8 17>, align 4

; Function Attrs: noinline norecurse nounwind readnone sspstrong uwtable
define i8 @add(i32, i32) local_unnamed_addr #0 {
  %x = insertelement %t undef, i8 6, i32 0
  %y = bitcast %t %x to <1 x i8>
  %z = extractelement <1 x i8> %y, i64 0
  %w = add i8 %z, %z 

  %a = load %t, %t * @foo
  %b = bitcast %t %a to <1 x i8>
  %c = extractelement <1 x i8> %b, i64 0
  %d = add i8 %c, %c

  ret i8 5
}

attributes #0 = { noinline norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
