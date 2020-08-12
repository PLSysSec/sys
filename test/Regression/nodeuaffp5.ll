
%"class.node::tracing::InternalTraceBuffer" = type { %"class.node::MutexBase", i8, i64, %"class.node::tracing::Agent"*, %"class.std::vector.89", i64, i32, i32 }
%"class.node::MutexBase" = type { %union.pthread_mutex_t }
%union.pthread_mutex_t = type { %struct.__pthread_mutex_s }
%struct.__pthread_mutex_s = type { i32, i32, i32, i32, i32, i16, i16, %struct.__pthread_internal_list }
%struct.__pthread_internal_list = type { %struct.__pthread_internal_list*, %struct.__pthread_internal_list* }
%"class.std::vector.89" = type { %"struct.std::_Vector_base.90" }
%"struct.std::_Vector_base.90" = type { %"struct.std::_Vector_base<std::unique_ptr<v8::platform::tracing::TraceBufferChunk, std::default_delete<v8::platform::tracing::TraceBufferChunk> >, std::allocator<std::unique_ptr<v8::platform::tracing::TraceBufferChunk, std::default_delete<v8::platform::tracing::TraceBufferChunk> > > >::_Vector_impl" }
%"struct.std::_Vector_base<std::unique_ptr<v8::platform::tracing::TraceBufferChunk, std::default_delete<v8::platform::tracing::TraceBufferChunk> >, std::allocator<std::unique_ptr<v8::platform::tracing::TraceBufferChunk, std::default_delete<v8::platform::tracing::TraceBufferChunk> > > >::_Vector_impl" = type { %"class.std::unique_ptr.94"*, %"class.std::unique_ptr.94"*, %"class.std::unique_ptr.94"* }
%"class.std::unique_ptr.94" = type { %"class.std::__uniq_ptr_impl.95" }
%"class.std::__uniq_ptr_impl.95" = type { %"class.std::tuple.96" }
%"class.std::tuple.96" = type { %"struct.std::_Tuple_impl.97" }
%"struct.std::_Tuple_impl.97" = type { %"struct.std::_Head_base.102" }
%"struct.std::_Head_base.102" = type { %"class.v8::platform::tracing::TraceBufferChunk"* }
%"class.v8::platform::tracing::TraceBufferChunk" = type <{ i64, [64 x %"class.v8::platform::tracing::TraceObject"], i32, [4 x i8] }>
%"class.v8::platform::tracing::TraceObject" = type { i32, i32, i8, i8*, i8*, i8*, i64, i64, i32, [2 x i8*], [2 x i8], [2 x %"union.v8::platform::tracing::TraceObject::ArgValue"], [2 x %"class.std::unique_ptr.103"], i8*, i32, i64, i64, i64, i64 }
%"union.v8::platform::tracing::TraceObject::ArgValue" = type { i64 }
%"class.std::unique_ptr.103" = type { %"class.std::__uniq_ptr_impl.104" }
%"class.std::__uniq_ptr_impl.104" = type { %"class.std::tuple.105" }
%"class.std::tuple.105" = type { %"struct.std::_Tuple_impl.106" }
%"struct.std::_Tuple_impl.106" = type { %"struct.std::_Head_base.111" }
%"struct.std::_Head_base.111" = type { %"class.v8::ConvertableToTraceFormat"* }
%"class.v8::ConvertableToTraceFormat" = type { i32 (...)** }
%"class.node::tracing::Agent" = type { i64, %struct.uv_loop_s, i8, i32, %"class.std::unordered_map", %"class.std::unordered_map.8", %"class.std::unique_ptr", %"class.node::MutexBase", %"class.node::ConditionVariableBase", %struct.uv_async_s, %"class.std::set", %"class.std::__cxx11::list" }
%struct.uv_loop_s = type { i8*, i32, [2 x i8*], %union.anon, i32, i64, i32, [2 x i8*], [2 x i8*], %struct.uv__io_s**, i32, i32, [2 x i8*], %union.pthread_mutex_t, %struct.uv_async_s, %union.pthread_rwlock_t, %struct.uv_handle_s*, [2 x i8*], [2 x i8*], [2 x i8*], [2 x i8*], [2 x i8*], void ()*, %struct.uv__io_s, i32, %struct.anon, i64, i64, [2 x i32], %struct.uv__io_s, %struct.uv_signal_s, i32, %struct.uv__io_s, i8*, i32 }
%union.anon = type { [2 x i8*] }
%union.pthread_rwlock_t = type { %struct.__pthread_rwlock_arch_t }
%struct.__pthread_rwlock_arch_t = type { i32, i32, i32, i32, i32, i32, i32, i32, i8, [7 x i8], i64, i32 }
%struct.uv_handle_s = type { i8*, %struct.uv_loop_s*, i32, {}*, [2 x i8*], %union.anon.0, %struct.uv_handle_s*, i32 }
%union.anon.0 = type { [4 x i8*] }
%struct.anon = type { i8*, i32 }
%struct.uv_signal_s = type { i8*, %struct.uv_loop_s*, i32, void (%struct.uv_handle_s*)*, [2 x i8*], %union.anon.2, %struct.uv_handle_s*, i32, void (%struct.uv_signal_s*, i32)*, i32, %struct.anon.3, i32, i32 }
%union.anon.2 = type { [4 x i8*] }
%struct.anon.3 = type { %struct.uv_signal_s*, %struct.uv_signal_s*, %struct.uv_signal_s*, i32 }
%struct.uv__io_s = type { void (%struct.uv_loop_s*, %struct.uv__io_s*, i32)*, [2 x i8*], [2 x i8*], i32, i32, i32 }
%"class.std::unordered_map" = type { %"class.std::_Hashtable" }
%"class.std::_Hashtable" = type { %"struct.std::__detail::_Hash_node_base"**, i64, %"struct.std::__detail::_Hash_node_base", i64, %"struct.std::__detail::_Prime_rehash_policy", %"struct.std::__detail::_Hash_node_base"* }
%"struct.std::__detail::_Hash_node_base" = type { %"struct.std::__detail::_Hash_node_base"* }
%"struct.std::__detail::_Prime_rehash_policy" = type { float, i64 }
%"class.std::unordered_map.8" = type { %"class.std::_Hashtable.9" }
%"class.std::_Hashtable.9" = type { %"struct.std::__detail::_Hash_node_base"**, i64, %"struct.std::__detail::_Hash_node_base", i64, %"struct.std::__detail::_Prime_rehash_policy", %"struct.std::__detail::_Hash_node_base"* }
%"class.std::unique_ptr" = type { %"class.std::__uniq_ptr_impl" }
%"class.std::__uniq_ptr_impl" = type { %"class.std::tuple" }
%"class.std::tuple" = type { %"struct.std::_Tuple_impl" }
%"struct.std::_Tuple_impl" = type { %"struct.std::_Head_base.23" }
%"struct.std::_Head_base.23" = type { %"class.node::tracing::TracingController"* }
%"class.node::tracing::TracingController" = type { %"class.v8::platform::tracing::TracingController.base", [4 x i8] }
%"class.v8::platform::tracing::TracingController.base" = type <{ %"class.v8::TracingController", %"class.std::unique_ptr.24", %"class.std::unique_ptr.33", %"class.std::unique_ptr.49", %"class.std::unordered_set", i32 }>
%"class.v8::TracingController" = type { i32 (...)** }
%"class.std::unique_ptr.24" = type { %"class.std::__uniq_ptr_impl.25" }
%"class.std::__uniq_ptr_impl.25" = type { %"class.std::tuple.26" }
%"class.std::tuple.26" = type { %"struct.std::_Tuple_impl.27" }
%"struct.std::_Tuple_impl.27" = type { %"struct.std::_Head_base.32" }
%"struct.std::_Head_base.32" = type { %"class.v8::platform::tracing::TraceBuffer"* }
%"class.v8::platform::tracing::TraceBuffer" = type { i32 (...)** }
%"class.std::unique_ptr.33" = type { %"class.std::__uniq_ptr_impl.34" }
%"class.std::__uniq_ptr_impl.34" = type { %"class.std::tuple.35" }
%"class.std::tuple.35" = type { %"struct.std::_Tuple_impl.36" }
%"struct.std::_Tuple_impl.36" = type { %"struct.std::_Head_base.41" }
%"struct.std::_Head_base.41" = type { %"class.v8::platform::tracing::TraceConfig"* }
%"class.v8::platform::tracing::TraceConfig" = type { i32, i8, %"class.std::vector" }
%"class.std::vector" = type { %"struct.std::_Vector_base" }
%"struct.std::_Vector_base" = type { %"struct.std::_Vector_base<std::__cxx11::basic_string<char>, std::allocator<std::__cxx11::basic_string<char> > >::_Vector_impl" }
%"struct.std::_Vector_base<std::__cxx11::basic_string<char>, std::allocator<std::__cxx11::basic_string<char> > >::_Vector_impl" = type { %"class.std::__cxx11::basic_string"*, %"class.std::__cxx11::basic_string"*, %"class.std::__cxx11::basic_string"* }
%"class.std::__cxx11::basic_string" = type { %"struct.std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Alloc_hider", i64, %union.anon.48 }
%"struct.std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Alloc_hider" = type { i8* }
%union.anon.48 = type { i64, [8 x i8] }
%"class.std::unique_ptr.49" = type { %"class.std::__uniq_ptr_impl.50" }
%"class.std::__uniq_ptr_impl.50" = type { %"class.std::tuple.51" }
%"class.std::tuple.51" = type { %"struct.std::_Tuple_impl.52" }
%"struct.std::_Tuple_impl.52" = type { %"struct.std::_Head_base.57" }
%"struct.std::_Head_base.57" = type { %"class.v8::base::Mutex"* }
%"class.v8::base::Mutex" = type opaque
%"class.std::unordered_set" = type { %"class.std::_Hashtable.58" }
%"class.std::_Hashtable.58" = type { %"struct.std::__detail::_Hash_node_base"**, i64, %"struct.std::__detail::_Hash_node_base", i64, %"struct.std::__detail::_Prime_rehash_policy", %"struct.std::__detail::_Hash_node_base"* }
%"class.node::ConditionVariableBase" = type { %union.pthread_cond_t }
%union.pthread_cond_t = type { %struct.__pthread_cond_s }
%struct.__pthread_cond_s = type { %union.anon.78, %union.anon.80, [2 x i32], [2 x i32], i32, i32, [2 x i32] }
%union.anon.78 = type { i64 }
%union.anon.80 = type { i64 }
%struct.uv_async_s = type { i8*, %struct.uv_loop_s*, i32, void (%struct.uv_handle_s*)*, [2 x i8*], %union.anon.1, %struct.uv_handle_s*, i32, void (%struct.uv_async_s*)*, [2 x i8*], i32 }
%union.anon.1 = type { [4 x i8*] }
%"class.std::set" = type { %"class.std::_Rb_tree" }
%"class.std::_Rb_tree" = type { %"struct.std::_Rb_tree<node::tracing::AsyncTraceWriter *, node::tracing::AsyncTraceWriter *, std::_Identity<node::tracing::AsyncTraceWriter *>, std::less<node::tracing::AsyncTraceWriter *>, std::allocator<node::tracing::AsyncTraceWriter *> >::_Rb_tree_impl" }
%"struct.std::_Rb_tree<node::tracing::AsyncTraceWriter *, node::tracing::AsyncTraceWriter *, std::_Identity<node::tracing::AsyncTraceWriter *>, std::less<node::tracing::AsyncTraceWriter *>, std::allocator<node::tracing::AsyncTraceWriter *> >::_Rb_tree_impl" = type { %"struct.std::_Rb_tree_key_compare", %"struct.std::_Rb_tree_header" }
%"struct.std::_Rb_tree_key_compare" = type { %"struct.std::less" }
%"struct.std::less" = type { i8 }
%"struct.std::_Rb_tree_header" = type { %"struct.std::_Rb_tree_node_base", i64 }
%"struct.std::_Rb_tree_node_base" = type { i32, %"struct.std::_Rb_tree_node_base"*, %"struct.std::_Rb_tree_node_base"*, %"struct.std::_Rb_tree_node_base"* }
%"class.std::__cxx11::list" = type { %"class.std::__cxx11::_List_base" }
%"class.std::__cxx11::_List_base" = type { %"struct.std::__cxx11::_List_base<std::unique_ptr<v8::platform::tracing::TraceObject, std::default_delete<v8::platform::tracing::TraceObject> >, std::allocator<std::unique_ptr<v8::platform::tracing::TraceObject, std::default_delete<v8::platform::tracing::TraceObject> > > >::_List_impl" }
%"struct.std::__cxx11::_List_base<std::unique_ptr<v8::platform::tracing::TraceObject, std::default_delete<v8::platform::tracing::TraceObject> >, std::allocator<std::unique_ptr<v8::platform::tracing::TraceObject, std::default_delete<v8::platform::tracing::TraceObject> > > >::_List_impl" = type { %"struct.std::__detail::_List_node_header" }
%"struct.std::__detail::_List_node_header" = type { %"struct.std::__detail::_List_node_base", i64 }
%"struct.std::__detail::_List_node_base" = type { %"struct.std::__detail::_List_node_base"*, %"struct.std::__detail::_List_node_base"* }
%"class.node::tracing::NodeTraceBuffer" = type { %"class.v8::platform::tracing::TraceBuffer", %struct.uv_loop_s*, %struct.uv_async_s, %struct.uv_async_s, i8, %"class.node::MutexBase", %"class.node::ConditionVariableBase", %"struct.std::atomic", %"class.node::tracing::InternalTraceBuffer", %"class.node::tracing::InternalTraceBuffer" }
%"struct.std::atomic" = type { %"struct.std::__atomic_base" }
%"struct.std::__atomic_base" = type { %"class.node::tracing::InternalTraceBuffer"* }

$_ZN2v88platform7tracing16TraceBufferChunkD2Ev = comdat any

$_ZNSt6vectorISt10unique_ptrIN2v88platform7tracing16TraceBufferChunkESt14default_deleteIS4_EESaIS7_EE17_M_default_appendEm = comdat any

$_ZZN4node9MutexBaseINS_16LibuvMutexTraitsEEC1EvE4args = comdat any

$_ZZN4node21ConditionVariableBaseINS_16LibuvMutexTraitsEEC1EvE4args = comdat any

@.str = private unnamed_addr constant [36 x i8] c"../src/tracing/node_trace_buffer.cc\00", align 1
@.str.1 = private unnamed_addr constant [4 x i8] c"105\00", align 1
@.str.2 = private unnamed_addr constant [13 x i8] c"(err) == (0)\00", align 1
@__PRETTY_FUNCTION__._ZN4node7tracing15NodeTraceBufferC2EmPNS0_5AgentEP9uv_loop_s = private unnamed_addr constant [93 x i8] c"node::tracing::NodeTraceBuffer::NodeTraceBuffer(size_t, node::tracing::Agent *, uv_loop_t *)\00", align 1
@_ZZN4node7tracing15NodeTraceBufferC1EmPNS0_5AgentEP9uv_loop_sE4args_0 = internal constant [4 x i8*] [i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.3, i32 0, i32 0), i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.2, i32 0, i32 0), i8* getelementptr inbounds ([93 x i8], [93 x i8]* @__PRETTY_FUNCTION__._ZN4node7tracing15NodeTraceBufferC2EmPNS0_5AgentEP9uv_loop_s, i32 0, i32 0)], align 16
@.str.3 = private unnamed_addr constant [4 x i8] c"109\00", align 1
@_ZZN4node9MutexBaseINS_16LibuvMutexTraitsEEC1EvE4args = linkonce_odr constant [4 x i8*] [i8* getelementptr inbounds ([20 x i8], [20 x i8]* @.str.4, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.5, i32 0, i32 0), i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.6, i32 0, i32 0), i8* getelementptr inbounds ([87 x i8], [87 x i8]* @__PRETTY_FUNCTION__._ZN4node9MutexBaseINS_16LibuvMutexTraitsEEC2Ev, i32 0, i32 0)], comdat, align 16
@.str.4 = private unnamed_addr constant [20 x i8] c"../src/node_mutex.h\00", align 1
@.str.5 = private unnamed_addr constant [4 x i8] c"143\00", align 1
@.str.6 = private unnamed_addr constant [37 x i8] c"(0) == (Traits::mutex_init(&mutex_))\00", align 1
@__PRETTY_FUNCTION__._ZN4node9MutexBaseINS_16LibuvMutexTraitsEEC2Ev = private unnamed_addr constant [87 x i8] c"node::MutexBase<node::LibuvMutexTraits>::MutexBase() [Traits = node::LibuvMutexTraits]\00", align 1
@.str.7 = private unnamed_addr constant [26 x i8] c"vector::_M_default_append\00", align 1
@_ZZN4node21ConditionVariableBaseINS_16LibuvMutexTraitsEEC1EvE4args = linkonce_odr constant [4 x i8*] [i8* getelementptr inbounds ([20 x i8], [20 x i8]* @.str.4, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.8, i32 0, i32 0), i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.9, i32 0, i32 0), i8* getelementptr inbounds ([111 x i8], [111 x i8]* @__PRETTY_FUNCTION__._ZN4node21ConditionVariableBaseINS_16LibuvMutexTraitsEEC2Ev, i32 0, i32 0)], comdat, align 16
@.str.8 = private unnamed_addr constant [4 x i8] c"118\00", align 1
@.str.9 = private unnamed_addr constant [35 x i8] c"(0) == (Traits::cond_init(&cond_))\00", align 1
@__PRETTY_FUNCTION__._ZN4node21ConditionVariableBaseINS_16LibuvMutexTraitsEEC2Ev = private unnamed_addr constant [111 x i8] c"node::ConditionVariableBase<node::LibuvMutexTraits>::ConditionVariableBase() [Traits = node::LibuvMutexTraits]\00", align 1


define i8 @buggy() {

fakeintro:
  %fake18 = alloca %"class.std::unique_ptr.94"
  %fake20 = alloca %"class.std::unique_ptr.94"
  br label %fake21

fake21:

  %fake22 = phi %"class.std::unique_ptr.94"* [ %fake29, %fake28 ], [ %fake20, %fakeintro ]
  %fake23 = getelementptr %"class.std::unique_ptr.94", %"class.std::unique_ptr.94"* %fake22, i64 0, i32 0, i32 0, i32 0, i32 0, i32 0
  %fake24 = load %"class.v8::platform::tracing::TraceBufferChunk"*, %"class.v8::platform::tracing::TraceBufferChunk"** %fake23
  %fake25 = icmp eq %"class.v8::platform::tracing::TraceBufferChunk"* %fake24, null
  br i1 %fake25, label %fake28, label %fake26

fake26:

  %fake27 = bitcast %"class.v8::platform::tracing::TraceBufferChunk"* %fake24 to i8*
  tail call void @free(i8* %fake27)
  br label %fake28
  
fake28:

  store %"class.v8::platform::tracing::TraceBufferChunk"* null, %"class.v8::platform::tracing::TraceBufferChunk"** %fake23
  %fake29 = getelementptr inbounds %"class.std::unique_ptr.94", %"class.std::unique_ptr.94"* %fake22, i64 1
  %fake30 = icmp eq %"class.std::unique_ptr.94"* %fake29, %fake18
  br i1 %fake30, label %fakeend, label %fake21

fakeend:

  ret i8 0

}

declare void @free(i8* nocapture) local_unnamed_addr #1
