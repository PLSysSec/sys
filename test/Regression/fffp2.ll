
%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, %struct._IO_codecvt*, %struct._IO_wide_data*, %struct._IO_FILE*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type opaque
%struct._IO_codecvt = type opaque
%struct._IO_wide_data = type opaque
%struct.MarFile_ = type { %struct._IO_FILE*, [256 x %struct.MarItem_*], %struct.SeenIndex_*, i32 }
%struct.MarItem_ = type { %struct.MarItem_*, i32, i32, i32, [1 x i8] }
%struct.SeenIndex_ = type { %struct.SeenIndex_*, i32, i32 }
%struct.ProductInformationBlock = type { i8*, i8* }

define i32 @fp (%struct.MarFile_*) {

fake1:
  br label %fake11

fake11:
  %fake12 = phi i64 [ 0, %fake1 ], [ %fake23, %fake22 ]
  %fake13 = getelementptr inbounds %struct.MarFile_, %struct.MarFile_* %0, i64 0, i32 1, i64 %fake12
  %fake14 = load %struct.MarItem_*, %struct.MarItem_** %fake13
  %fake15 = icmp eq %struct.MarItem_* %fake14, null
  br i1 %fake15, label %untaken, label %fake16

fake16:
  %fake17 = phi %struct.MarItem_* [ %fake14, %fake11 ]
  %fake18 = bitcast %struct.MarItem_* %fake17 to i8*
  %fake19 = getelementptr inbounds %struct.MarItem_, %struct.MarItem_* %fake17, i64 0, i32 0
  %fake20 = load %struct.MarItem_*, %struct.MarItem_** %fake19
  tail call void @free(i8* %fake18) 
  %fake21 = icmp eq %struct.MarItem_* %fake20, null
  br i1 %fake21, label %fake22, label %untaken

fake22:
  %fake23 = add nuw nsw i64 %fake12, 1
  %fake24 = icmp eq i64 %fake23, 256
  br i1 %fake24, label %untaken, label %fake11

untaken:
  ret i32 0
}

declare void @free(i8*)