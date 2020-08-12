; ModuleID = 'float.bc'
source_filename = "float.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%floatvector = type <2 x float> 

; Function Attrs: noinline norecurse nounwind readnone sspstrong uwtable
define dso_local float @floaty(%floatvector) {
  %floatelem = extractelement %floatvector %0, i32 0
  ret float %floatelem
}
