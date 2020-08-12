; ModuleID = 'free.bc'
source_filename = "free.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%a = type { <1 x i32> }
%b = type { { i32 } }

%struct.apoint = type { i32, i32 }

%c = type { i32 }

@iv1 = local_unnamed_addr constant %struct.apoint insertvalue (%struct.apoint { i32 0, i32 0}, i32 1, 1), align 4

@iv2 = local_unnamed_addr constant %a insertvalue (%a { <1 x i32> <i32 1>}, <1 x i32> insertelement (<1 x i32> undef, i32 100, i32 0), 0), align 4

@iv3 = local_unnamed_addr constant %b insertvalue (%b undef, { i32 } insertvalue ({i32 } undef, i32 100, 0), 0)

@iv4 = local_unnamed_addr constant { i32 } insertvalue ({i32 } undef, i32 100, 0)

@iv5 = local_unnamed_addr constant %c insertvalue (%c undef, i32 100, 0)

; Function Attrs: noinline norecurse nounwind sspstrong uwtable
define i32 @simple(i32 nocapture) local_unnamed_addr #0 {

  %load_one = load %struct.apoint, %struct.apoint * @iv1
  %val_one = extractvalue %struct.apoint %load_one, 1  ; ; 1
  
  %load_two = load %a, %a * @iv2
  %val_two_0 = extractvalue %a %load_two, 0
  %val_two_1 = extractelement <1 x i32> %val_two_0, i32 0  ; ; 100
  
  %load_three = load %b, %b * @iv3
  %val_three = extractvalue %b %load_three, 0, 0  ; ; 100
  
  %load_four = load { i32 }, { i32 } * @iv4
  %val_four = extractvalue { i32 } %load_four, 0  ; ; 100
  
  %load_five = load %c, %c * @iv5
  %val_five = extractvalue %c %load_five, 0  ; ; 100
  
  %dummy_one = add i32 %val_one, %val_two_1
  %dummy_two = add i32 %val_three, %val_four 
  ret i32 %val_five
}

attributes #0 = { noinline norecurse nounwind sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 6.0.0 (tags/RELEASE_600/final)"}
!4 = !{!5, !6, i64 0}
!5 = !{!"apoint", !6, i64 0, !6, i64 4}
!6 = !{!"int", !7, i64 0}
!7 = !{!"omnipotent char", !8, i64 0}
!8 = !{!"Simple C/C++ TBAA"}
!9 = !{!5, !6, i64 4}
