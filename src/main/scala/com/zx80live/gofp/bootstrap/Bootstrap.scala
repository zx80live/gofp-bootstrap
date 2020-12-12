package com.zx80live.gofp.bootstrap

import com.zx80live.gofp.bootstrap.functions._
import com.zx80live.gofp.bootstrap.types._

object Bootstrap extends App {

  import IoUtils._

  toFile(dir = "", filename = "bootstrap_predef.go", imports = Seq("fmt"), content = Seq(FuncPredef.funcRequire))
  toFile(dir = "", filename = "bootstrap_predef_reflect.go", imports = Seq("reflect"), content = FuncPredef.funcReflectTypes)

  toFile(dir = "", filename = "bootstrap_base.go", content = BaseType.boxedDeclarations)
  toFile(dir = "", filename = "bootstrap_base_cons.go", content = BaseType.functionsCons)
  toFile(dir = "", filename = "bootstrap_base_underlined.go", content = BaseType.functionsUnderlined)
  toFile(dir = "", filename = "bootstrap_base_converters.go", imports = Seq("fmt", "strconv"), content = BaseType.functionsConverters)
  toFile(dir = "", filename = "bootstrap_base_numeric.go", content = BaseType.functionsNumeric)
  toFile(dir = "", filename = "bootstrap_base_string.go", imports = Seq("fmt"), content = BaseType.functionsString)

  toFile(dir = "", filename = "bootstrap_func_equal.go", content = FuncEquals.functions)
  toFile(dir = "", filename = "bootstrap_func_tostring.go", imports = Seq("fmt", "reflect"), content = FuncToString.functions)
  toFile(dir = "", filename = "bootstrap_func_println.go", imports = Seq("fmt"), content = FuncPrintln.functionsPrintln)

  toFile(dir = "", filename = "bootstrap_predicate.go", content = Predicate.declarations)
  toFile(dir = "", filename = "bootstrap_predicate_empty.go", content = Predicate.emptyDeclarations)
  toFile(dir = "", filename = "bootstrap_predicate_and.go", content = Predicate.functionsAnd)
  toFile(dir = "", filename = "bootstrap_predicate_or.go", content = Predicate.functionsOr)
  toFile(dir = "", filename = "bootstrap_predicate_xor.go", content = Predicate.functionsXor)
  toFile(dir = "", filename = "bootstrap_predicate_neg.go", content = Predicate.functionsNeg)
  toFile(dir = "", filename = "bootstrap_predicate_math.go", content = Predicate.mathPredicates)
  toFile(dir = "", filename = "bootstrap_predicate_string.go", imports = Seq("regexp"), content = Predicate.stringPredicates)

  //  toFile(dir = "", filename = "bootstrap_transformer.go", content = Transformer.declarations)
  //  toFile(dir = "", filename = "bootstrap_transformer_empty.go", content = Transformer.emptyDeclarations)
  toFile(dir = "", filename = "bootstrap_transformer_identity.go", content = Transformer.identities)
  toFile(dir = "", filename = "bootstrap_transformer_string.go", imports = Seq("regexp"), content = Transformer.stringTransformers)

  toFile(dir = "", filename = "bootstrap_array.go", content = ArrayType.declarations)
  toFile(dir = "", filename = "bootstrap_array_count.go", content = ArrayType.functionsCount)
  toFile(dir = "", filename = "bootstrap_array_drop.go", content = ArrayType.functionsDrop)
  toFile(dir = "", filename = "bootstrap_array_dropright.go", content = ArrayType.functionsDropRight)
  toFile(dir = "", filename = "bootstrap_array_dropwhile.go", content = ArrayType.functionsDropWhile)
  toFile(dir = "", filename = "bootstrap_array_take.go", content = ArrayType.functionsTake)
  toFile(dir = "", filename = "bootstrap_array_takewhile.go", content = ArrayType.functionsTakeWhile)
  toFile(dir = "", filename = "bootstrap_array_takeright.go", content = ArrayType.functionsTakeRight)
  toFile(dir = "", filename = "bootstrap_array_filter.go", content = ArrayType.functionsFilter)
  toFile(dir = "", filename = "bootstrap_array_foreach.go", content = ArrayType.functionsForeach)
  toFile(dir = "", filename = "bootstrap_array_head.go", content = ArrayType.functionsHead)
  toFile(dir = "", filename = "bootstrap_array_headoption.go", content = ArrayType.functionsHeadOption)
  toFile(dir = "", filename = "bootstrap_array_map.go", content = ArrayType.functionsMap)
  toFile(dir = "", filename = "bootstrap_array_size.go", content = ArrayType.functionsSize)
  toFile(dir = "", filename = "bootstrap_array_tail.go", content = ArrayType.functionsTail)
  toFile(dir = "", filename = "bootstrap_array_tolist.go", content = ArrayType.functionsToList)
  toFile(dir = "", filename = "bootstrap_array_mkstring.go", imports = Seq("fmt"), content = ArrayType.functionsMkString)
  toFile(dir = "", filename = "bootstrap_array_tostring.go", content = ArrayType.functionsToString)
  toFile(dir = "", filename = "bootstrap_array_equals.go", content = ArrayType.functionsEquals)
  toFile(dir = "", filename = "bootstrap_array_find.go", content = ArrayType.functionsFind)
  toFile(dir = "", filename = "bootstrap_array_zip.go", content = ArrayType.functionsZip)
  toFile(dir = "", filename = "bootstrap_array_zipall.go", content = ArrayType.functionsZipAll)
  toFile(dir = "", filename = "bootstrap_array_zipwithindex.go", content = ArrayType.functionsZipWithIndex)

  toFile(dir = "", filename = "bootstrap_option.go", content = OptionType.declarations)
  toFile(dir = "", filename = "bootstrap_option_none.go", content = OptionType.emptyDeclarations)
  toFile(dir = "", filename = "bootstrap_option_cons.go", content = OptionType.functionsCons)
  toFile(dir = "", filename = "bootstrap_option_shortcons.go", content = OptionType.functionsShortCons)
  toFile(dir = "", filename = "bootstrap_option_isdefined.go", content = OptionType.functionsIsDefined)
  toFile(dir = "", filename = "bootstrap_option_isempty.go", content = OptionType.functionsIsEmpty)
  toFile(dir = "", filename = "bootstrap_option_equals.go", content = OptionType.functionsEquals)
  toFile(dir = "", filename = "bootstrap_option_foreach.go", content = OptionType.functionsForeach)
  toFile(dir = "", filename = "bootstrap_option_filter.go", content = OptionType.functionsFilter)
  toFile(dir = "", filename = "bootstrap_option_map.go", content = OptionType.functionsMap)
  toFile(dir = "", filename = "bootstrap_option_flatmap.go", content = OptionType.functionsFlatMap)
  toFile(dir = "", filename = "bootstrap_option_flatten.go", content = OptionType.functionsFlatten)
  toFile(dir = "", filename = "bootstrap_option_tostring.go", imports = Seq("fmt"), content = OptionType.functionsToString)
  toFile(dir = "", filename = "bootstrap_option_foldleft.go", content = OptionType.functionsFoldLeft)

  toFile(dir = "", filename = "bootstrap_list.go", content = ListType.declarations)
  toFile(dir = "", filename = "bootstrap_list_nil.go", content = ListType.emptyDeclarations)
  toFile(dir = "", filename = "bootstrap_list_prepend.go", content = ListType.functionsPrepend)
  toFile(dir = "", filename = "bootstrap_list_cons.go", content = ListType.functionsCons)
  toFile(dir = "", filename = "bootstrap_list_isempty.go", content = ListType.functionsIsEmpty)
  toFile(dir = "", filename = "bootstrap_list_nonempty.go", content = ListType.functionsNonEmpty)
  toFile(dir = "", filename = "bootstrap_list_head.go", content = ListType.functionsHead)
  toFile(dir = "", filename = "bootstrap_list_headoption.go", content = ListType.functionsHeadOption)
  toFile(dir = "", filename = "bootstrap_list_tail.go", content = ListType.functionsTail)
  toFile(dir = "", filename = "bootstrap_list_foreach.go", content = ListType.functionsForeach)
  toFile(dir = "", filename = "bootstrap_list_reverse.go", content = ListType.functionsReverse)
  toFile(dir = "", filename = "bootstrap_list_copy.go", content = ListType.functionsCopy)
  toFile(dir = "", filename = "bootstrap_list_filter.go", content = ListType.functionsFilter)
  toFile(dir = "", filename = "bootstrap_list_map.go", content = ListType.functionsMap)
  toFile(dir = "", filename = "bootstrap_list_flatmap.go", content = ListType.functionsFlatMap)
  toFile(dir = "", filename = "bootstrap_list_flatten.go", content = ListType.functionsFlatten)
  toFile(dir = "", filename = "bootstrap_list_size.go", content = ListType.functionsSize)
  toFile(dir = "", filename = "bootstrap_list_mkstring.go", imports = Seq("fmt"), content = ListType.functionsMkString)
  toFile(dir = "", filename = "bootstrap_list_tostring.go", content = ListType.functionsToString)
  toFile(dir = "", filename = "bootstrap_list_equals.go", content = ListType.functionsEquals)
  toFile(dir = "", filename = "bootstrap_list_toarray.go", content = ListType.functionsToArray)
  toFile(dir = "", filename = "bootstrap_list_reduce.go", content = ListType.functionsReduce)
  toFile(dir = "", filename = "bootstrap_list_foldleft.go", content = ListType.functionsFoldLeft)
  toFile(dir = "", filename = "bootstrap_list_find.go", content = ListType.functionsFind)
  toFile(dir = "", filename = "bootstrap_list_groupby.go", content = ListType.functionsGroupBy)
  toFile(dir = "", filename = "bootstrap_list_count.go", content = ListType.functionsCount)
  toFile(dir = "", filename = "bootstrap_list_take.go", content = ListType.functionsTake)
  toFile(dir = "", filename = "bootstrap_list_takewhile.go", content = ListType.functionsTakeWhile)
  toFile(dir = "", filename = "bootstrap_list_takeright.go", content = ListType.functionsTakeRight)
  toFile(dir = "", filename = "bootstrap_list_drop.go", content = ListType.functionsDrop)
  toFile(dir = "", filename = "bootstrap_list_dropright.go", content = ListType.functionsDropRight)
  toFile(dir = "", filename = "bootstrap_list_dropwhile.go", content = ListType.functionsDropWhile)
  toFile(dir = "", filename = "bootstrap_list_zip.go", content = ListType.functionsZip)
  toFile(dir = "", filename = "bootstrap_list_zipall.go", content = ListType.functionsZipAll)
  toFile(dir = "", filename = "bootstrap_list_zipwithindex.go", content = ListType.functionsZipWithIndex)

  toFile(dir = "", filename = "bootstrap_queue.go", content = QueueType.declarations)
  toFile(dir = "", filename = "bootstrap_queue_nil.go", content = QueueType.emptyDeclarations)
  toFile(dir = "", filename = "bootstrap_queue_enqueue.go", content = QueueType.functionsEnqueue)
  toFile(dir = "", filename = "bootstrap_queue_swap.go", content = QueueType.functionsSwap)
  toFile(dir = "", filename = "bootstrap_queue_dequeue.go", content = QueueType.functionsDequeue)
  toFile(dir = "", filename = "bootstrap_queue_dequeueoption.go", content = QueueType.functionsDequeueOption)
  toFile(dir = "", filename = "bootstrap_queue_head.go", content = QueueType.functionsHead)
  toFile(dir = "", filename = "bootstrap_queue_headoption.go", content = QueueType.functionsHeadOption)
  toFile(dir = "", filename = "bootstrap_queue_tail.go", content = QueueType.functionsTail)
  toFile(dir = "", filename = "bootstrap_queue_nonempty.go", content = QueueType.functionsNonEmpty)
  toFile(dir = "", filename = "bootstrap_queue_isempty.go", content = QueueType.functionsIsEmpty)
  toFile(dir = "", filename = "bootstrap_queue_foreach.go", content = QueueType.functionsForeach)
  toFile(dir = "", filename = "bootstrap_queue_tolist.go", content = QueueType.functionsToList)

  toFile(dir = "mutable", filename = "bootstrap_linkedlist.go", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = LinkedListType.declarations)
  toFile(dir = "mutable", filename = "bootstrap_linkedlist_empty.go", content = LinkedListType.emptyDeclarations)
  toFile(dir = "mutable", filename = "bootstrap_linkedlist_head.go", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = LinkedListType.functionsHead)
  toFile(dir = "mutable", filename = "bootstrap_linkedlist_foreach.go", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = LinkedListType.functionsForeach)
  toFile(dir = "mutable", filename = "bootstrap_linkedlist_foreachright.go", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = LinkedListType.functionsForeachRight)
  toFile(dir = "mutable", filename = "bootstrap_linkedlist_append.go", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = LinkedListType.functionsAppend)
  toFile(dir = "mutable", filename = "bootstrap_linkedlist_tolist.go", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = LinkedListType.functionsToList)

  toFile(dir = "", filename = "bootstrap_tuple2.go", content = Tuple2Type.declarations)
  toFile(dir = "", filename = "bootstrap_tuple2_cons.go", content = Tuple2Type.functionsCons)
  toFile(dir = "", filename = "bootstrap_tuple2_equals.go", content = Tuple2Type.functionsEquals)
  toFile(dir = "", filename = "bootstrap_tuple2_tostring.go", imports = Seq("fmt"), content = Tuple2Type.functionsToString)

  toFile(dir = "concurrent", filename = "bootstrap_future.go", pack = "concurrent", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = FutureType.declarations)
  toFile(dir = "concurrent", filename = "bootstrap_future_cons.go", pack = "concurrent", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = FutureType.functionsCons)
  toFile(dir = "concurrent", filename = "bootstrap_future_success.go", pack = "concurrent", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = FutureType.functionsSuccess)
  toFile(dir = "concurrent", filename = "bootstrap_future_map.go", pack = "concurrent", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = FutureType.functionsMap)
  toFile(dir = "concurrent", filename = "bootstrap_future_flatmap.go", pack = "concurrent", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = FutureType.functionsFlatMap)
  toFile(dir = "concurrent", filename = "bootstrap_future_result.go", pack = "concurrent", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = FutureType.functionsResult)


}