

define i32 @simple(i32 nocapture readonly) local_unnamed_addr #0 {

  %arr = insertvalue [2 x i8] c"a\00", i8 122, 0
  %letter = extractvalue [2 x i8] %arr, 0
  %term = extractvalue [2 x i8] %arr, 1
  %dep = add i8 %letter, %term

  %arr1 = insertvalue [1 x [2 x i8 ] ] undef, [2 x i8] c"a\00", 0
  %letter1 = extractvalue [1 x [2 x i8]] %arr1, 0, 0
  %dep1 = add i8 %letter1, %letter1;

  %arr2 = insertvalue { [2 x i8] } undef, [2 x i8] c"d\00", 0
  %letter2 = extractvalue { [2 x i8] } %arr2, 0, 0
  %dep2 = add i8 %letter2, %letter2

  %ptr = alloca { [2 x i8] }
  store { [2 x i8] } %arr2 , { [2 x i8] } * %ptr
  %gep = getelementptr { [2 x i8] }, { [2 x i8] } * %ptr, i32 0, i32 0, i32 0
  %letter3 = load i8, i8 * %gep
  %dep3 = add i8 %letter3, %letter3

  %arr4 = insertvalue  [2 x { i8 } ] undef, { i8 } { i8 50 }, 0
  %arr5 = insertvalue  [2 x { i8 } ] %arr4, { i8 } { i8 100 }, 1
  %letter4 = extractvalue [2 x { i8 } ] %arr5, 0, 0
  %letter5 = extractvalue [2 x { i8 } ] %arr5, 1, 0
  %dep4 = add i8 %letter4, %letter5

  %ptr2 = alloca [2 x { i8 } ]
  store [2 x { i8 } ] %arr5, [2 x { i8 } ] * %ptr2
  %gep2 = getelementptr [2 x { i8 } ], [2 x { i8 } ] * %ptr2, i32 0, i32 0, i32 0
  %letter6 = load i8, i8 * %gep2
  %dep5 = add i8 %letter6, %letter6

  %arr6 = insertvalue [2 x <1 x i8>] undef, <1 x i8> <i8 9>, 0
  %arr7 = insertvalue [2 x <1 x i8>] %arr6, <1 x i8> <i8 11>, 1
  %inter1 = extractvalue [2 x <1 x i8>] %arr7, 0
  %letter7 = extractelement <1 x i8> %inter1, i32 0
  %inter2 = extractvalue [2 x <1 x i8>] %arr7, 1
  %letter8 = extractelement <1 x i8> %inter2, i32 0  
  %dep6 = add i8 %letter7, %letter8

  ret i32 1
}