
; Function Attrs: noinline nounwind sspstrong uwtable
define i8* @myFree() local_unnamed_addr {

start:
  %startptr = alloca i8
  br label %loopstart  

loopstart:
  %pointer = phi i8* [%startptr, %start], [%nextpointer, %loopend]
  %nextpointer = getelementptr i8, i8* %pointer, i32 1
  br label %loopend 

loopmiddle:
  tail call void @free(i8* %nextpointer)
  %result = load i8, i8* %nextpointer
  %dep = add i8 %result, %result 
  br label %loopend

loopend:
  br label %loopstart 

untakenbr:
  ret i8* null

}

; Function Attrs: nounwind
declare void @free(i8* nocapture) local_unnamed_addr #1
