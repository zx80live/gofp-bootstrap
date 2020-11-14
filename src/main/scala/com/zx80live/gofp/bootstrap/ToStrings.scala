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

  val optionToStrings: Seq[String] = Optional.names.map { case (t, tOpt) =>
    s"""
       |func ${toName(tOpt)}(e $tOpt) string {
       |  if e.IsDefined() { return fmt.Sprintf("Some(%v)", ${toName(t)}(*e.value)) } else { return "None" }
       |}
       |""".stripMargin
  }

  val listToStrings: Seq[String]= Lists.names.map { case (t, tList) =>
  s"""
       |func ${toName(tList)}(l $tList) string { return ${MkStrings.toName(tList)}(l, "List(", ",", ")") }""".stripMargin
  }

  val allToStrings: Seq[String] = baseToStrings ++ arraysToStrings ++ optionToStrings ++ listToStrings
}
