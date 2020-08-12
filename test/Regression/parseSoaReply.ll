

; <label>:103:                                    ; preds = %96
  %104 = load i64, i64* %8, align 8, !tbaa !256
  %105 = getelementptr inbounds i8, i8* %85, i64 %104
  %106 = getelementptr inbounds %struct.ares_soa_reply, %struct.ares_soa_reply* %9, i64 0, i32 1
  %107 = call i32 @ares_expand_name(i8* nonnull %105, i8* %1, i32 %2, i8** nonnull %106, i64* nonnull %8) #13
  %108 = icmp eq i32 %107, 0
  br i1 %108, label %112, label %109
; <label>:112:                                    ; preds = %103
  %113 = load i64, i64* %8, align 8, !tbaa !256
  %114 = getelementptr inbounds i8, i8* %105, i64 %113
  %115 = getelementptr inbounds i8, i8* %114, i64 20
  %116 = icmp ugt i8* %115, %66
  br i1 %116, label %117, label %121  