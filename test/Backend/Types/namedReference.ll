; ModuleID = 'add.bc'
source_filename = "add.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%a = type { i32 }

%b = type { %a, i64 }

%c = type i10

%d = type <2 x %c>

%e = type i8 *

@foo = local_unnamed_addr constant %a { i32 100 }

; Function Attrs: noinline norecurse nounwind readnone sspstrong uwtable
define i8 @add(i32, i32) local_unnamed_addr #0 {
  %astr = insertvalue %a undef, i32 5, 0
  %aval = extractvalue %a %astr, 0
  %d0 = add i32 %aval, %aval

  %bstr0 = insertvalue %b undef, %a %astr, 0
  %bstr1 = insertvalue %b %bstr0, i64 500, 1
  %aresult = extractvalue %b %bstr1, 0
  %bresult_0 = extractvalue %a %aresult, 0
  %bresult_1 = extractvalue %b %bstr1, 1
  %d1 = add i32 %bresult_0, %bresult_0
  %d2 = add i64 %bresult_1, %bresult_1

  %x = add %c 0, 50
  %y = add %c %x, %x
  %z = add %c %y, %y

  %d_0 = insertelement %d undef, %c 0, i32 0
  %d_1 = insertelement %d %d_0, %c 1, i32 1
  %d_r0 = extractelement %d %d_1, i64 0
  %d_r1 = extractelement %d %d_1, i64 1
  %result_d = add %c %d_r0, %d_r1

  %p = alloca i8
  store i8 1, %e %p
  %v = load i8, %e %p
  %w = add i8 %v, %v

  ret i8 6
}

attributes #0 = { noinline norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
