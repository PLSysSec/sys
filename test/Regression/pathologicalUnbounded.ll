; ModuleID = 'add.bc'
source_filename = "add.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"


%struct.hostent = type { i8*, i8**, i32, i32, i8** }

; Function Attrs: noinline norecurse nounwind readnone sspstrong uwtable
define i1 @add(%struct.hostent*, i1, i8***, i8**) local_unnamed_addr #0 {

five:

  %six = load i8*, i8** %3
  %seven = icmp eq i8* %six, null
  br i1 %seven, label %end, label %eight  

eight:

  br label %nine

nine:

  %ten = bitcast i64 0 to i64 
  %eleven = bitcast i8* %six to i8*
  %twelve = add nuw i64 %ten, 1
  tail call void @free(i8* nonnull %eleven) 
  %thirteen = load i8**, i8*** %2
  %fourteen = getelementptr inbounds i8*, i8** %thirteen, i64 %twelve
  %fifteen = load i8*, i8** %fourteen
  %sixteen = icmp eq i8* %fifteen, null
  br i1 %sixteen, label %seventeen, label %end

seventeen:

  %eighteen = bitcast i8** %thirteen to i8**
  %nineteen = bitcast i8** %eighteen to i8*
  tail call void @free(i8* %nineteen) 
  store i8** null, i8*** %2
  br label %twenty

twenty:

  %twentyone = getelementptr inbounds %struct.hostent, %struct.hostent* %0, i64 0, i32 1
  %twentytwo = load i8**, i8*** %twentyone
  %twentythree = icmp eq i8** %twentytwo, null
  ret i1 %twentythree

end:

  ret i1 0
}

declare void @free(i8* nocapture) local_unnamed_addr #1   

attributes #0 = { noinline norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
