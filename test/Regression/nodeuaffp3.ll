
%"class.v8::platform::tracing::TraceWriter" = type { i32 (...)** }

define i8 @myFree() local_unnamed_addr {

dummy:
  %fake29 = alloca i8
  %fake11 = alloca i8
  %fake5 = alloca %"class.v8::platform::tracing::TraceWriter"*
  %bob = getelementptr i8, i8* %fake11, i32 0
  %dep = load i8, i8* %bob
  br label %fake31

fake31: 
  call void @free(i8* %fake29) 
  br label %fake32

fake32:
  call void @llvm.lifetime.end.p0i8(i64 32, i8* nonnull %fake11) 
  %new33 = load %"class.v8::platform::tracing::TraceWriter"*, %"class.v8::platform::tracing::TraceWriter"** %fake5
  ret i8 0
  
}  

; Function Attrs: nounwind
declare void @free(i8* nocapture) local_unnamed_addr #1
declare void @llvm.lifetime.end.p0i8(i64, i8*) 
