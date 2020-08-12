; ModuleID = 'malloc.bc'
source_filename = "malloc.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind sspstrong uwtable
define noalias i8 @dynamicarr() local_unnamed_addr #0 {

  %malloc1 = tail call noalias i8* @malloc(i64 8) #2
  
  ; i8 array
  
  %gep0 = getelementptr inbounds i8, i8* %malloc1, i64 0
  store i8 2, i8 * %gep0
  %gep1 = getelementptr inbounds i8, i8* %malloc1, i64 1
  store i8 50, i8 * %gep1
  %gep2 = getelementptr inbounds i8, i8* %malloc1, i64 2
  store i8 4, i8 * %gep2
  %gep3 = getelementptr inbounds i8, i8* %malloc1, i64 3
  store i8 35, i8 * %gep3
  %gep4 = getelementptr inbounds i8, i8* %malloc1, i64 4
  store i8 9, i8 * %gep4
  %gep5 = getelementptr inbounds i8, i8* %malloc1, i64 5
  store i8 18, i8 * %gep5
  %gep6 = getelementptr inbounds i8, i8* %malloc1, i64 6
  store i8 53, i8 * %gep6
  %gep7 = getelementptr inbounds i8, i8* %malloc1, i64 7
  store i8 7, i8 * %gep7
  %gep8 = getelementptr inbounds i8, i8* %malloc1, i64 8
  store i8 7, i8 * %gep8

  %r0 = load i8, i8* %gep0
  %dep0 = add i8 %r0, %r0
  %r1 = load i8, i8* %gep1
  %dep1 = add i8 %r1, %r1
  %r2 = load i8, i8* %gep2
  %dep2 = add i8 %r2, %r2
  %r3 = load i8, i8* %gep3
  %dep3 = add i8 %r3, %r3
  %r4 = load i8, i8* %gep4
  %dep4 = add i8 %r4, %r4
  %r5 = load i8, i8* %gep5
  %dep5 = add i8 %r5, %r5
  %r6 = load i8, i8* %gep6
  %dep6 = add i8 %r6, %r6
  %r7 = load i8, i8* %gep7
  %dep7 = add i8 %r7, %r7
  %r8 = load i8, i8* %gep8
  %dep8 = add i8 %r8, %r8

  ; i16 array
  
  %mallocuncast = tail call noalias i8* @malloc(i64 8) #2
  %malloc2 = bitcast i8* %mallocuncast to i16*
  %geps1 = getelementptr inbounds i16, i16* %malloc2, i64 0
  store i16 30, i16 * %geps1
  %geps2 = getelementptr inbounds i16, i16* %malloc2, i64 1
  store i16 48, i16 * %geps2
  %geps3 = getelementptr inbounds i16, i16* %malloc2, i64 2
  store i16 99, i16 * %geps3
  %geps4 = getelementptr inbounds i16, i16* %malloc2, i64 3
  store i16 4, i16 * %geps4
  %geps5 = getelementptr inbounds i16, i16* %malloc2, i64 4
  store i16 4, i16 * %geps5

  %rs1 = load i16, i16* %geps1
  %deps1 = add i16 %rs1, %rs1
  %rs2 = load i16, i16* %geps2
  %deps2 = add i16 %rs2, %rs2
  %rs3 = load i16, i16* %geps3
  %deps3 = add i16 %rs3, %rs3
  %rs4 = load i16, i16* %geps4
  %deps4 = add i16 %rs4, %rs4  
  %rs5 = load i16, i16* %geps5
  %deps5 = add i16 %rs5, %rs5        

  ; i32 array

  %mallocuncast3 = tail call noalias i8* @malloc(i64 8) #2
  %malloc3 = bitcast i8* %mallocuncast3 to i32*
  %gept0 = getelementptr inbounds i32, i32* %malloc3, i64 0
  store i32 2, i32 * %gept0
  %gept1 = getelementptr inbounds i32, i32* %malloc3, i64 1  
  store i32 4, i32 * %gept1

  %rt0 = load i32, i32* %gept0
  %dept0 = add i32 %rt0, %rt0
  %rt1 = load i32, i32* %gept1
  %dept1 = add i32 %rt1, %rt0  

  ; making sure math works

  %malloc4 = tail call noalias i8* @malloc(i64 8) #2

  ; make all 32 of the last bits of the %malloc4 cell ones

  %castthirtytwo = bitcast i8* %malloc4 to i32*
  %gepthirtytwo = getelementptr inbounds i32, i32* %castthirtytwo, i64 1
  store i32 4294967295, i32* %gepthirtytwo

  ; set up the other pointers

  %castsixteen = bitcast i8* %malloc4 to i16*
  %gepsixteen = getelementptr inbounds i16, i16* %castsixteen, i64 2
  %gepeight = getelementptr inbounds i8, i8* %malloc4, i64 4

  ; test the load of all pointers

  %rthirtytwo = load i32, i32* %gepthirtytwo
  %depthirtytwo = add i32 %rthirtytwo, %rthirtytwo

  %rsixteen = load i16, i16* %gepsixteen
  %depsixteen = add i16 %rsixteen, %rsixteen

  %reight = load i8, i8* %gepeight
  %depeight = add i8 %reight, %reight  	 

  ret i8 0
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) local_unnamed_addr #1

attributes #0 = { nounwind }
attributes #1 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }

