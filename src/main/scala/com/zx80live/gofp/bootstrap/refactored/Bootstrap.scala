package com.zx80live.gofp.bootstrap.refactored

import com.zx80live.gofp.bootstrap.refactored.functions.{FuncEquals, FuncMkString, FuncToString}
import com.zx80live.gofp.bootstrap.refactored.types.{ListType, OptionType, Transformer}


object Bootstrap extends App {
  import IoUtils._

  toFile("bootstrap_equal.go", content = FuncEquals.functions)
  toFile("bootstrap_tostring.go", imports = Seq("fmt"), content = FuncToString.functions)
  toFile("bootstrap_mkstring.go", imports = Seq("fmt"), content = FuncMkString.functions)

  toFile("bootstrap_transformer.go", content = Transformer.declarations)
  toFile("bootstrap_transformer_empty.go", content = Transformer.emptyDeclarations)

  toFile("bootstrap_option.go", content = OptionType.declarations ++ OptionType.noneDeclarations)
  toFile("bootstrap_option_cons.go", content = OptionType.functionsCons)
  toFile("bootstrap_option_isdefined.go", content = OptionType.functionsIsDefined)
  toFile("bootstrap_option_isempty.go", content = OptionType.functionsIsEmpty)
  toFile("bootstrap_option_equals.go", content = OptionType.functionsEquals)
  toFile("bootstrap_option_foreach.go", content = OptionType.functionsForeach)

  toFile("bootstrap_list.go", content = ListType.declarations ++ ListType.nilDeclarations)
  toFile("bootstrap_list_prepend.go", content = ListType.functionsPrepend)
  toFile("bootstrap_list_cons.go", content = ListType.functionsCons)
  toFile("bootstrap_list_isempty.go", content = ListType.functionsIsEmpty)
  toFile("bootstrap_list_nonempty.go", content = ListType.functionsNonEmpty)
  toFile("bootstrap_list_foreach.go", content = ListType.functionsForeach)
  toFile("bootstrap_list_reverse.go", content = ListType.functionsReverse)
  toFile("bootstrap_list_copy.go", content = ListType.functionsCopy)
}
