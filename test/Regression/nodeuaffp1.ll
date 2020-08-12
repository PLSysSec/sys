; ModuleID = 'free.bc'
source_filename = "free.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%"class.std::__cxx11::basic_string" = type { i64, i64, %union.anon.92 }
%union.anon.92 = type { i64, [8 x i8] }

; Function Attrs: noinline nounwind sspstrong uwtable
define i8* @myFree() local_unnamed_addr {

fakepreamble:
  %fake143 = alloca i8*
  %fake9 = alloca %"class.std::__cxx11::basic_string"
  %fake130 = alloca i8
  %fake125 = alloca i8*
  %fake124 = alloca i8 	 
  br label %entry207

entry207:
  %load = load i8*, i8** %fake143
  %gep = getelementptr inbounds %"class.std::__cxx11::basic_string", %"class.std::__cxx11::basic_string"* %fake9, i64 0, i32 2
  %cast = bitcast %union.anon.92* %gep to i8*
  %compare = icmp eq i8* %load, %cast
  br i1 %compare, label %untakenbr, label %next212

next212:
  call void @free(i8* %load)
  br label %next213

next213:
  call void @llvm.lifetime.end.p0i8(i64 32, i8* nonnull %fake130) 
  %otherload = load i8*, i8** %fake125
  %loadcmp = icmp eq i8* %otherload, %fake124 
  br i1 %loadcmp, label %untakenbr, label %untakenbr

untakenbr:
  ret i8* null
  
}

; Function Attrs: nounwind
declare void @free(i8* nocapture) local_unnamed_addr #1

declare void @llvm.lifetime.end.p0i8(i64, i8*) 

