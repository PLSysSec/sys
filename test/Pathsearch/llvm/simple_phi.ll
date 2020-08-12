define i64 @foo(i64, i64) {
  %3 = icmp slt i64 %0, 3
  br i1 %3, label %4, label %5

; <label>:4:
  br label %5

; <label>:5:
  %6 = phi i64 [ 22, %2 ], [ 33, %4 ]
  ret i64 %6
}
