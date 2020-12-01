package com.zx80live.gofp.bootstrap

import com.zx80live.gofp.bootstrap.functions._
import com.zx80live.gofp.bootstrap.types._

//TODO zipWithIndex
//TODO zip
//TODO zipWith
//TODO count, take, dropRight
//TODO base predicates (find by regexp)
object Bootstrap extends App {

  import IoUtils._

  toFile("bootstrap_base.go", content = BaseType.boxedDeclarations)
  toFile("bootstrap_base_cons.go", content = BaseType.functionsCons)
  toFile("bootstrap_base_underlined.go", content = BaseType.functionsUnderlined)
  toFile("bootstrap_base_converters.go", imports = Seq("fmt", "strconv"), content = BaseType.functionsConverters)
  toFile("bootstrap_base_math.go", content = BaseType.functionsMath)
  toFile("bootstrap_base_range.go", content = BaseType.functionsRange)

  toFile("bootstrap_func_equal.go", content = FuncEquals.functions)
  toFile("bootstrap_func_tostring.go", imports = Seq("fmt"), content = FuncToString.functions)

  toFile("bootstrap_predicate.go", content = Predicate.declarations)
  toFile("bootstrap_predicate_empty.go", content = Predicate.emptyDeclarations)
  toFile("bootstrap_predicate_and.go", content = Predicate.functionsAnd)
  toFile("bootstrap_predicate_or.go", content = Predicate.functionsOr)
  toFile("bootstrap_predicate_xor.go", content = Predicate.functionsXor)
  toFile("bootstrap_predicate_neg.go", content = Predicate.functionsNeg)

  //  toFile("bootstrap_transformer.go", content = Transformer.declarations)
  //  toFile("bootstrap_transformer_empty.go", content = Transformer.emptyDeclarations)
  toFile("bootstrap_identity.go", content = Transformer.identities)

  toFile("bootstrap_array.go", content = ArrayType.declarations)
  toFile("bootstrap_array_drop.go", content = ArrayType.functionsDrop)
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

}