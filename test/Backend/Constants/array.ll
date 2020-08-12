
@greeting = local_unnamed_addr global [7 x i8] c"Hello!\00", align 1

define i32 @simple(i32 nocapture readonly) local_unnamed_addr #0 {
  %arr = load [7 x i8], [7 x i8] * @greeting
  %letter = extractvalue [7 x i8] %arr, 1 
  %dep = add i8 %letter, %letter

  %letter1 = extractvalue [2 x i8] c"a\00", 0
  %dep1 = add i8 %letter1, %letter1

  ret i32 1
}