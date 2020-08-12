
%"class.graphite2::vm::Machine::Code" = type <{ i8**, i8*, i64, i64, i8, [3 x i8], i32, i8, i8, i8, i8, [4 x i8] }>  

define i8 @fp (i32, i8*) {

entry:
  %fake1535 = alloca %"class.graphite2::vm::Machine::Code"
  %fake1533 = alloca %"class.graphite2::vm::Machine::Code"*
  %fake1536 = alloca i8
  br label %fake1539

fake1539:
  ; 1540 = 192, 1535 = 192 
  %fake1540 = bitcast %"class.graphite2::vm::Machine::Code"* %fake1535 to i8**
  ; 1541 = 576 
  %fake1541 = load i8*, i8** %fake1540
  tail call void @free(i8* %fake1541) 
  br label %fake1542

fake1542:
  store i8 0, i8* %fake1536
  %fake1543 = bitcast %"class.graphite2::vm::Machine::Code"* %fake1535 to i8*
  %fake1544 = getelementptr inbounds %"class.graphite2::vm::Machine::Code", %"class.graphite2::vm::Machine::Code"* %fake1535, i64 0, i32 6
  store i32 4, i32* %fake1544
  br label %fake1545
  
fake1545:
  %fake1546 = getelementptr inbounds i8, i8* %1, i64 2
  %fake1547 = load i8, i8* %fake1546
  %fake1548 = load %"class.graphite2::vm::Machine::Code"*, %"class.graphite2::vm::Machine::Code"** %fake1533
  %fake1549 = getelementptr inbounds %"class.graphite2::vm::Machine::Code", %"class.graphite2::vm::Machine::Code"* %fake1548, i64 0, i32 7
  %fake1550 = load i8, i8* %fake1549
  ret i8 %fake1550
}

declare void @free(i8*)
declare void @llvm.memset.p0i8.i64(i8*, i8, i64, i1)