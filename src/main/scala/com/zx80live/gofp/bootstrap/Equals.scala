package com.zx80live.gofp.bootstrap

@deprecated("use com.zx80live.gofp.bootstrap.refactored")
object Equals {
  def toName(goType: String): String = s"Equal${GoTypes.toName(goType)}"

  val baseEquals: Seq[String] = GoTypes.baseTypes.map { goType =>
    s"""
       |func ${toName(goType)}(a, b $goType) bool { return a == b }""".stripMargin
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

  val optionalEquals: Seq[String] = Optional.names.map { case (t, optT) =>
    s"""
       |func ${toName(optT)}(a, b $optT) bool {
       |  if a.IsDefined() {
       |    if b.IsDefined() {
       |      return ${Equals.toName(t)}(*a.value, *b.value)
       |    } else {
       |      return false
       |    }
       |  } else if b.IsDefined() {
       |    return false
       |  } else {
       |    return true
       |  }
       |}
       |""".stripMargin
  }

  val listEquals: Seq[String] = Lists.names.map { case (t, tList) =>
    s"""
       |func ${toName(tList)}(a, b $tList) bool {
       |  if a.Size() != b.Size() { return false }
       |
       |  xs1 := a
       |  xs2 := b
       |  for xs1.IsNotEmpty() {
       |    if !${Equals.toName(t)}(*xs1.head, *xs2.head) { return false }
       |    xs1 = *xs1.tail
       |    xs2 = *xs2.tail
       |  }
       |  return true
       |}
       |""".stripMargin
  }

  val allEquals: Seq[String] = baseEquals ++ arraysEquals ++ optionalEquals ++ listEquals
}
