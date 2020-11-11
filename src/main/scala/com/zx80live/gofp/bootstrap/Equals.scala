package com.zx80live.gofp.bootstrap

object Equals {
  def toName(goType: String): String = s"Equal${GoTypes.toName(goType)}"

  val baseEquals: Seq[String] = GoTypes.baseTypes.map { goType =>
    s"""func ${toName(goType)}(a, b $goType) bool { return a == b }"""
  }

  val arraysEquals: Seq[String] = GoTypes.arrayTypes.map { goType =>
    s"""
       |func ${toName(goType)}(a, b $goType) bool {
       |  if len(a) != len(b) {
       |    return false
       |  }
       |  for i, v := range a {
       |    if v != b[i] {
       |      return false
       |    }
       |  }
       |  return true
       |}
       |""".stripMargin
  }

  val allEquals: Seq[String] = baseEquals ++ arraysEquals
}
