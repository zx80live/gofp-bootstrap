package com.zx80live.gofp.bootstrap.refactored

object Bootstrap extends App {

  BaseType.types foreach println
  println("--------------------------------------------")

  OptionType.types foreach println
  println("--------------------------------------------")

  ArrayType.types foreach println
  println("--------------------------------------------")

  ListType.types foreach println
  println("--------------------------------------------")
}
