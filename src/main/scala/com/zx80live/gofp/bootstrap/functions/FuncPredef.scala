package com.zx80live.gofp.bootstrap.functions

object FuncPredef {
  def funcRequire: String =
    s"""
       |func Require(e bool, msg string) {
       |  if !e { panic(fmt.Sprintf("Assertion error: %v", msg)) }
       |}
       |""".stripMargin
}
