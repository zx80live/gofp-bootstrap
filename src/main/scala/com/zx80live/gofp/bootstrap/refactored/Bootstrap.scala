package com.zx80live.gofp.bootstrap.refactored

import com.zx80live.gofp.bootstrap.refactored.functions.FuncEquals
import com.zx80live.gofp.bootstrap.refactored.types.OptionType


object Bootstrap extends App {
  import IoUtils._

  toFile("bootstrap_equal.go", content = FuncEquals.functions)

  toFile("bootstrap_option.go", content = OptionType.declarations ++ OptionType.nones)
  toFile("bootstrap_option_isdefined.go", content = OptionType.functionsIsDefined)
  toFile("bootstrap_option_equals.go", content = OptionType.functionsEquals)
}
