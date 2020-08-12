

%big = type { i64, i64, i64, i64 }
%adversarial = type { %big, %big, %big, %big, %big, i8, %big }
%adversarial2 = type { %big, %big, %big, %big, %big, i16, %big }
%adversarial3 = type { %big, %big, %big, %big, %big, i32, %big }
%adversarial4 = type { %big, %big, %big, %big, %big, i64, %big }

%adversarial5 = type { i32, i8, i64 }


define i64 @offset (%adversarial*, %adversarial2*, %adversarial3*, %adversarial4*, %adversarial5*) {
  %gepd = getelementptr %adversarial, %adversarial * %0, i32 0, i32 6, i32 0
  %result = load i64, i64* %gepd

  %gep2 = getelementptr %adversarial2, %adversarial2 * %1, i32 0, i32 6, i32 0
  %result2 = load i64, i64* %gep2
  %dep2 = add i64 %result2, %result2

  %gep3 = getelementptr %adversarial3, %adversarial3 * %2, i32 0, i32 6, i32 0
  %result3 = load i64, i64* %gep3
  %dep3 = add i64 %result3, %result3

  %gep4 = getelementptr %adversarial4, %adversarial4 * %3, i32 0, i32 6, i32 0
  %result4 = load i64, i64* %gep4
  %dep4 = add i64 %result4, %result4

  %gep5 = getelementptr %adversarial5, %adversarial5 * %4, i32 0, i32 2
  %result5 = load i64, i64* %gep5
  %dep5 = add i64 %result5, %result5

  %gep6 = getelementptr %adversarial5, %adversarial5 * %4, i32 0, i32 1
  %result6 = load i8, i8* %gep6
  %dep6 = add i8 %result6, %result6

  ret i64 %result
}