package com.zx80live.gofp.bootstrap

import com.zx80live.gofp.bootstrap.functions._
import com.zx80live.gofp.bootstrap.types._

//TODO zipWithIndex
//TODO zip
//TODO zipWith
object Bootstrap extends App {

  import IoUtils._

  toFile("bootstrap_predef.go", imports = Seq("fmt"), content = Seq(FuncPredef.funcRequire))
  toFile("bootstrap_predef_reflect.go", imports = Seq("reflect"), content = FuncPredef.funcReflectTypes)

  toFile("bootstrap_base.go", content = BaseType.boxedDeclarations)
  toFile("bootstrap_base_cons.go", content = BaseType.functionsCons)
  toFile("bootstrap_base_underlined.go", content = BaseType.functionsUnderlined)
  toFile("bootstrap_base_converters.go", imports = Seq("fmt", "strconv"), content = BaseType.functionsConverters)
  toFile("bootstrap_base_numeric.go", content = BaseType.functionsNumeric)
  toFile("bootstrap_base_string.go", imports = Seq("fmt"), content = BaseType.functionsString)

  toFile("bootstrap_func_equal.go", content = FuncEquals.functions)
  toFile("bootstrap_func_tostring.go", imports = Seq("fmt", "reflect"), content = FuncToString.functions)
  toFile("bootstrap_func_println.go", imports = Seq("fmt"), content = FuncPrintln.functionsPrintln)

  toFile("bootstrap_predicate.go", content = Predicate.declarations)
  toFile("bootstrap_predicate_empty.go", content = Predicate.emptyDeclarations)
  toFile("bootstrap_predicate_and.go", content = Predicate.functionsAnd)
  toFile("bootstrap_predicate_or.go", content = Predicate.functionsOr)
  toFile("bootstrap_predicate_xor.go", content = Predicate.functionsXor)
  toFile("bootstrap_predicate_neg.go", content = Predicate.functionsNeg)
  toFile("bootstrap_predicate_math.go", content = Predicate.mathPredicates)
  toFile("bootstrap_predicate_string.go", imports = Seq("regexp"), content = Predicate.stringPredicates)

  //  toFile("bootstrap_transformer.go", content = Transformer.declarations)
  //  toFile("bootstrap_transformer_empty.go", content = Transformer.emptyDeclarations)
  toFile("bootstrap_transformer_identity.go", content = Transformer.identities)
  toFile("bootstrap_transformer_string.go", imports = Seq("regexp"), content = Transformer.stringTransformers)

  toFile("bootstrap_array.go", content = ArrayType.declarations)
  toFile("bootstrap_array_count.go", content = ArrayType.functionsCount)
  toFile("bootstrap_array_drop.go", content = ArrayType.functionsDrop)
  toFile("bootstrap_array_dropright.go", content = ArrayType.functionsDropRight)
  toFile("bootstrap_array_dropwhile.go", content = ArrayType.functionsDropWhile)
  toFile("bootstrap_array_take.go", content = ArrayType.functionsTake)
  toFile("bootstrap_array_takewhile.go", content = ArrayType.functionsTakeWhile)
  toFile("bootstrap_array_takeright.go", content = ArrayType.functionsTakeRight)
  toFile("bootstrap_array_filter.go", content = ArrayType.functionsFilter)
  toFile("bootstrap_array_foreach.go", content = ArrayType.functionsForeach)
  toFile("bootstrap_array_head.go", content = ArrayType.functionsHead)
  toFile("bootstrap_array_headoption.go", content = ArrayType.functionsHeadOption)
  toFile("bootstrap_array_map.go", content = ArrayType.functionsMap)
  toFile("bootstrap_array_size.go", content = ArrayType.functionsSize)
  toFile("bootstrap_array_tail.go", content = ArrayType.functionsTail)
  toFile("bootstrap_array_tolist.go", content = ArrayType.functionsToList)
  toFile("bootstrap_array_mkstring.go", imports = Seq("fmt"), content = ArrayType.functionsMkString)
  toFile("bootstrap_array_tostring.go", content = ArrayType.functionsToString)
  toFile("bootstrap_array_equals.go", content = ArrayType.functionsEquals)
  toFile("bootstrap_array_find.go", content = ArrayType.functionsFind)
  toFile("bootstrap_array_zip.go", content = ArrayType.functionsZip)
  toFile("bootstrap_array_zipwithindex.go", content = ArrayType.functionsZipWithIndex)

  toFile("bootstrap_option.go", content = OptionType.declarations)
  toFile("bootstrap_option_none.go", content = OptionType.emptyDeclarations)
  toFile("bootstrap_option_cons.go", content = OptionType.functionsCons)
  toFile("bootstrap_option_shortcons.go", content = OptionType.functionsShortCons)
  toFile("bootstrap_option_isdefined.go", content = OptionType.functionsIsDefined)
  toFile("bootstrap_option_isempty.go", content = OptionType.functionsIsEmpty)
  toFile("bootstrap_option_equals.go", content = OptionType.functionsEquals)
  toFile("bootstrap_option_foreach.go", content = OptionType.functionsForeach)
  toFile("bootstrap_option_filter.go", content = OptionType.functionsFilter)
  toFile("bootstrap_option_map.go", content = OptionType.functionsMap)
  toFile("bootstrap_option_flatmap.go", content = OptionType.functionsFlatMap)
  toFile("bootstrap_option_flatten.go", content = OptionType.functionsFlatten)
  toFile("bootstrap_option_tostring.go", imports = Seq("fmt"), content = OptionType.functionsToString)
  toFile("bootstrap_option_foldleft.go", content = OptionType.functionsFoldLeft)

  toFile("bootstrap_list.go", content = ListType.declarations)
  toFile("bootstrap_list_nil.go", content = ListType.emptyDeclarations)
  toFile("bootstrap_list_prepend.go", content = ListType.functionsPrepend)
  toFile("bootstrap_list_cons.go", content = ListType.functionsCons)
  toFile("bootstrap_list_isempty.go", content = ListType.functionsIsEmpty)
  toFile("bootstrap_list_nonempty.go", content = ListType.functionsNonEmpty)
  toFile("bootstrap_list_head.go", content = ListType.functionsHead)
  toFile("bootstrap_list_headoption.go", content = ListType.functionsHeadOption)
  toFile("bootstrap_list_tail.go", content = ListType.functionsTail)
  toFile("bootstrap_list_foreach.go", content = ListType.functionsForeach)
  toFile("bootstrap_list_reverse.go", content = ListType.functionsReverse)
  toFile("bootstrap_list_copy.go", content = ListType.functionsCopy)
  toFile("bootstrap_list_filter.go", content = ListType.functionsFilter)
  toFile("bootstrap_list_map.go", content = ListType.functionsMap)
  toFile("bootstrap_list_flatmap.go", content = ListType.functionsFlatMap)
  toFile("bootstrap_list_flatten.go", content = ListType.functionsFlatten)
  toFile("bootstrap_list_size.go", content = ListType.functionsSize)
  toFile("bootstrap_list_mkstring.go", imports = Seq("fmt"), content = ListType.functionsMkString)
  toFile("bootstrap_list_tostring.go", content = ListType.functionsToString)
  toFile("bootstrap_list_equals.go", content = ListType.functionsEquals)
  toFile("bootstrap_list_toarray.go", content = ListType.functionsToArray)
  toFile("bootstrap_list_reduce.go", content = ListType.functionsReduce)
  toFile("bootstrap_list_foldleft.go", content = ListType.functionsFoldLeft)
  toFile("bootstrap_list_find.go", content = ListType.functionsFind)
  toFile("bootstrap_list_groupby.go", content = ListType.functionsGroupBy)
  toFile("bootstrap_list_count.go", content = ListType.functionsCount)
  toFile("bootstrap_list_take.go", content = ListType.functionsTake)
  toFile("bootstrap_list_takewhile.go", content = ListType.functionsTakeWhile)
  toFile("bootstrap_list_takeright.go", content = ListType.functionsTakeRight)
  toFile("bootstrap_list_drop.go", content = ListType.functionsDrop)
  toFile("bootstrap_list_dropright.go", content = ListType.functionsDropRight)
  toFile("bootstrap_list_dropwhile.go", content = ListType.functionsDropWhile)
  toFile("bootstrap_list_zip.go", content = ListType.functionsZip)
  toFile("bootstrap_list_zipall.go", content = ListType.functionsZipAll)
  toFile("bootstrap_list_zipwithindex.go", content = ListType.functionsZipWithIndex)

  toFile("bootstrap_tuple2.go", content = Tuple2Type.declarations)
  toFile("bootstrap_tuple2_cons.go", content = Tuple2Type.functionsCons)
  toFile("bootstrap_tuple2_equals.go", content = Tuple2Type.functionsEquals)
  toFile("bootstrap_tuple2_tostring.go", imports = Seq("fmt"), content = Tuple2Type.functionsToString)

  toFile(dir = "concurrent", filename = "bootstrap_future.go", pack = "concurrent", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = FutureType.declarations)
  toFile(dir = "concurrent", filename = "bootstrap_future_cons.go", pack = "concurrent", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = FutureType.functionsCons)
  toFile(dir = "concurrent", filename = "bootstrap_future_success.go", pack = "concurrent", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = FutureType.functionsSuccess)
  toFile(dir = "concurrent", filename = "bootstrap_future_map.go", pack = "concurrent", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = FutureType.functionsMap)
  toFile(dir = "concurrent", filename = "bootstrap_future_flatmap.go", pack = "concurrent", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = FutureType.functionsFlatMap)
  toFile(dir = "concurrent", filename = "bootstrap_future_result.go", pack = "concurrent", imports = Seq(""". "github.com/zx80live/gofp/fp" """), content = FutureType.functionsResult)


}