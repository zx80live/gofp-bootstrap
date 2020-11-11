package com.zx80live.gofp.bootstrap

object ToStrings {
  def toName(goType: String): String = s"ToString${GoTypes.toName(goType)}"

  val baseToStrings: Seq[String] = GoTypes.baseTypes.map { goType =>
    s"""
       |func ${toName(goType)}(e $goType) string { return fmt.Sprintf("%v", e)}""".stripMargin
  }

  val arraysToStrings: Seq[String] = GoTypes.arrayTypes.map { goType =>
    s"""
       |func ${toName(goType)}(e $goType) string { return ${MkStrings.toName(goType)}(e, "[", ",", "]")}""".stripMargin
  }

  val allToStrings: Seq[String] = baseToStrings ++ arraysToStrings
}
