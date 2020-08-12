define i8 @add_O3(i32, i32) local_unnamed_addr #0 {
  %3 = add nsw i8 1, 1
  %4 = add nsw i8 255, 1
  %5 = add nsw i8 %3, %4
  
  ret i8 %5
}

define i32* @alloca_O3(i32, i32) local_unnamed_addr #0 {
  %ptr = alloca i32
  ret i32* %ptr
}

define i32 @call_O3(i32, i32) local_unnamed_addr #0 {
  %3 = or i32 %0, %1
  ret i32 %3
}

define i64 @atomicrmw_O3(i32, i32) local_unnamed_addr #0 {
  %3 = alloca i64
  store i64 0, i64* %3

  ; xchg: *ptr = val
  %4 = atomicrmw xchg i64* %3, i64 100 acquire ; old value = 0
  %5 = load i64, i64* %3 ; %5 = xchg 0 100 = 100
  %6 = and i64 %4, %5

  ret i64 1
}

define i32 @vector_add_O3(i32, i32) local_unnamed_addr #0 {
  %3 = add nsw <4 x i32> <i32 0, i32 1, i32 2, i32 3>, <i32 0, i32 1, i32 2, i32 3>
  %4 = extractelement <4 x i32> %3, i32 0
  %5 = extractelement <4 x i32> %3, i32 1	
  %6 = extractelement <4 x i32> %3, i32 2
  %7 = extractelement <4 x i32> %3, i32 3
  %8 = add i32 %7, %6
  %9 = add i32 %5, %4
  ret i32 %4
}

define i32 @foo(i32, i32) local_unnamed_addr #0 {
  %3 = add i32 7, 1
  %4 = call i32 @bar(i32 %1)
  ret i32 %4
}

define i32 @bar(i32) {
  %2 = add i32 %0, 3
  ret i32 %2
}
