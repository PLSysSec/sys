

@Line = internal unnamed_addr global i8* null
@Yanked = internal unnamed_addr global i8* null




declare void @free(i8* nocapture)
declare i8* @malloc(i64)
declare i32 @getvalue32()
declare void @llvm.memcpy.p0i8.p0i8.i64(i8*, i8*, i64, i1)


define i32 @bug (i8* %fake48) {

preamble:
  %fake47 = call i32 @getvalue32()
  %fake26 = call i32 @getvalue32()
  br label %entry

entry:

  tail call void @free(i8* nonnull %fake48) 
  store i8* null, i8** @Yanked
  br label %fake51

fake51:

  %fake52 = icmp slt i32 %fake47, 1
  br i1 %fake52, label %exit, label %fake53

fake53:

  %fake54 = add i32 %fake47, 1
  %fake55 = zext i32 %fake54 to i64
  %fake56 = tail call noalias i8* @malloc(i64 %fake55) 
  store i8* %fake56, i8** @Yanked
  %fake57 = icmp eq i8* %fake56, null
  br i1 %fake57, label %exit, label %fake58

fake58:

  %fake59 = load i8*, i8** @Line
  %fake60 = sext i32 %fake26 to i64
  ;; this aliases. %fake59 = 0. This is the issue 
  %fake61 = getelementptr inbounds i8, i8* %fake59, i64 %fake60
  %fake62 = sext i32 %fake47 to i64
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* %fake56, i8* %fake61, i64 %fake62, i1 false) 
  %fake63 = getelementptr inbounds i8, i8* %fake56, i64 %fake62
  store i8 0, i8* %fake63
  br label %exit

exit:
  ret i32 4

}

