package com.zx80live.gofp.bootstrap.refactored

import com.zx80live.gofp.bootstrap.refactored.functions.FuncEquals
import com.zx80live.gofp.bootstrap.refactored.types.{ListType, OptionType}


object Bootstrap extends App {
  import IoUtils._

  toFile("bootstrap_equal.go", content = FuncEquals.functions)

  toFile("bootstrap_option.go", content = OptionType.declarations ++ OptionType.noneDeclarations)
  toFile("bootstrap_option_isdefined.go", content = OptionType.functionsIsDefined)
  toFile("bootstrap_option_isempty.go", content = OptionType.functionsIsEmpty)
  toFile("bootstrap_option_equals.go", content = OptionType.functionsEquals)

  toFile("bootstrap_list.go", content = ListType.declarations ++ ListType.nilDeclarations)
  toFile("bootstrap_list_isempty.go", content = ListType.functionsIsEmpty)
  toFile("bootstrap_list_nonempty.go", content = ListType.functionsNonEmpty)
}
