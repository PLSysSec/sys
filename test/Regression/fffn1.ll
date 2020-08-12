; ModuleID = 'free.bc'
source_filename = "free.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @uaf(i32*) #0 !dbg !9 {
  ; 64
  %2 = alloca i32*, align 8
  ; 128 
  %3 = alloca i32, align 4
  ; load %2 = %0
  store i32* %0, i32** %2, align 8
  
  call void @llvm.dbg.declare(metadata i32** %2, metadata !14, metadata !DIExpression()), !dbg !15
  call void @llvm.dbg.declare(metadata i32* %3, metadata !16, metadata !DIExpression()), !dbg !17

  ; %4 = %0
  %4 = load i32*, i32** %2, align 8, !dbg !18
  ; %5 = *%0
  %5 = load i32, i32* %4, align 4, !dbg !19
  ; load %3 = *%0
  store i32 %5, i32* %3, align 4, !dbg !17
  ; %6 = %0
  %6 = load i32*, i32** %2, align 8, !dbg !20
  ; %7 = cast %0 to i8
  %7 = bitcast i32* %6 to i8*, !dbg !20
  ; free %0
  call void @free(i8* %7) #3, !dbg !21
  ; %8 = *%0
  %8 = load i32, i32* %3, align 4, !dbg !22
  ; return *%0
  ret i32 %8, !dbg !23
}

; Function Attrs: nounwind readnone speculatable
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nounwind
declare void @free(i8*) #2

attributes #0 = { noinline nounwind optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable }
attributes #2 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5, !6, !7}
!llvm.ident = !{!8}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 7.0.0 (tags/RELEASE_700/final)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2)
!1 = !DIFile(filename: "free.c", directory: "/home/mlfbrown/hateonwards/attack")
!2 = !{}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{i32 7, !"PIC Level", i32 2}
!7 = !{i32 7, !"PIE Level", i32 2}
!8 = !{!"clang version 7.0.0 (tags/RELEASE_700/final)"}
!9 = distinct !DISubprogram(name: "uaf", scope: !1, file: !1, line: 4, type: !10, isLocal: false, isDefinition: true, scopeLine: 4, flags: DIFlagPrototyped, isOptimized: false, unit: !0, retainedNodes: !2)
!10 = !DISubroutineType(types: !11)
!11 = !{!12, !13}
!12 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!13 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !12, size: 64)
!14 = !DILocalVariable(name: "p", arg: 1, scope: !9, file: !1, line: 4, type: !13)
!15 = !DILocation(line: 4, column: 15, scope: !9)
!16 = !DILocalVariable(name: "result", scope: !9, file: !1, line: 5, type: !12)
!17 = !DILocation(line: 5, column: 7, scope: !9)
!18 = !DILocation(line: 5, column: 17, scope: !9)
!19 = !DILocation(line: 5, column: 16, scope: !9)
!20 = !DILocation(line: 6, column: 8, scope: !9)
!21 = !DILocation(line: 6, column: 3, scope: !9)
!22 = !DILocation(line: 7, column: 10, scope: !9)
!23 = !DILocation(line: 7, column: 3, scope: !9)
