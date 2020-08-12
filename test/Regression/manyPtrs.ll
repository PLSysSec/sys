; ModuleID = 'add.bc'
source_filename = "add.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"


; Function Attrs: noinline norecurse nounwind readnone sspstrong uwtable
define i8 @add(i8******) local_unnamed_addr #0 {
  %ptr1 = load i8*****, i8****** %0
  %ptr2 = load i8****, i8***** %ptr1
  %ptr3 = load i8***, i8**** %ptr2
  %ptr4 = load i8**, i8*** %ptr3
  %ptr5 = load i8*, i8** %ptr4
  %val = load i8, i8* %ptr5        
  ret i8 %val
}

declare void @free(i8* nocapture) local_unnamed_addr #1   

attributes #0 = { noinline norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
