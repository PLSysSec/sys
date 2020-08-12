


define i8 @dumbarray (i8) {
  %getelem = extractvalue { i8 } zeroinitializer, 0
  %sum = add i8 %0, %getelem
  ret i8 %sum
}