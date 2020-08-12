; RUN: %S/ConcreteTest.py --klee='%klee' --lli=%lli %s

; %i0 = 111, %j0 = 100
define i8 @"test_simple_arith"(i16 %i0, i16 %j0) {

arith:
  %t1 = add i16 %i0, %j0 ; 211
  %t2 = sub i16 %i0, %j0 ; 11
  %t3 = mul i16 %t1, %t2 ; 

  call void @print_i16(i16 %t3)

  ret i8 0
}

    ; %dnm1 = call i8 @test_div_and_mod(i16 63331, i16 3123)
    ; %dnm2 = call i8 @test_div_and_mod(i16 1000, i16 55444)
    ; %dnm3 = call i8 @test_div_and_mod(i16 49012, i16 55444)
    ; %dnm4 = call i8 @test_div_and_mod(i16 1000, i16 25)

define i8 @"test_div_and_mod"(i16 %op1, i16 %op2) {

divnmod:
  %t1 = udiv i16 %op1, %op2
  %t2 = urem i16 %op1, %op2  
  %t3 = sdiv i16 %op1, %op2  
  %t4 = srem i16 %op1, %op2  

  call void @print_i16(i16 %t1)
  call void @print_i16(i16 %t2)
  call void @print_i16(i16 %t3)
  call void @print_i16(i16 %t4)

  ret i8 0      
}

;    %cmp1 = call i8 @test_cmp(i16 63331, i16 3123)
;    %cmp2 = call i8 @test_cmp(i16 1000, i16 55444)
; call void @test_cmp(i16 49012, i16 55444)

define i8 @test_cmp(i16 %op1, i16 %op2) {

cmp:
  %t1 = icmp ule i16 %op1, %op2
  %t2 = icmp ult i16 %op1, %op2  
  %t3 = icmp uge i16 %op1, %op2  
  %t4 = icmp ugt i16 %op1, %op2  
  %t6 = icmp slt i16 %op1, %op2  
  %t5 = icmp sle i16 %op1, %op2
  %t7 = icmp sge i16 %op1, %op2  
  %t8 = icmp sgt i16 %op1, %op2  
  %t9 = icmp eq i16 %op1, %op2  
  %t10 = icmp ne i16 %op1, %op2  

  call void @print_i1(i1 %t1)
  call void @print_i1(i1 %t2)
  call void @print_i1(i1 %t3)
  call void @print_i1(i1 %t4)
  call void @print_i1(i1 %t5)
  call void @print_i1(i1 %t6)
  call void @print_i1(i1 %t7)
  call void @print_i1(i1 %t8)
  call void @print_i1(i1 %t9)
  call void @print_i1(i1 %t10)

  ret i8 0
}

define i32 @main() {
    %simple = call i8 @test_simple_arith(i16 111, i16 100)
    %simpledep = add i8 %simple, %simple

    %dnm1 = call i8 @test_div_and_mod(i16 63331, i16 3123)
   ; %dnm2 = call i8 @test_div_and_mod(i16 1000, i16 55444)
    ; %dnm3 = call i8 @test_div_and_mod(i16 49012, i16 55444)
    %dnm4 = call i8 @test_div_and_mod(i16 1000, i16 25)
    %dnmdep1 = add i8 %dnm1, %dnm4
;    %dnmdep2 = add i8 %dnm3, %dnm4

    %cmp1 = call i8 @test_cmp(i16 63331, i16 3123)
    %cmp2 = call i8 @test_cmp(i16 1000, i16 55444)
    %cmp3 = call i8 @test_cmp(i16 49012, i16 55444)
    ; call void @test_cmp(i16 1000, i16 25)
    %depcmp1 = add i8 %cmp1, %cmp2
    %depcmp2 = add i8 %cmp3, %cmp3 
        
    ret i32 0
}

; defined in print_int.c
declare void @print_i1(i1)
declare void @print_i8(i8)
declare void @print_i16(i16)
declare void @print_i32(i32)
declare void @print_i64(i64)
