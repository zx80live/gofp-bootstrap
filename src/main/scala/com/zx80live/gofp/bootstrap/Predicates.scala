package com.zx80live.gofp.bootstrap

object Predicates {

  def toName(goType: String): String = s"Predicate${GoTypes.toName(goType)}"

  def predicateTypeDeclaration(goType: String): String = {
    s"""
       |type ${toName(goType)} func(e $goType) bool""".stripMargin
  }

  val predicateTypeDeclarations: Seq[String] = GoTypes.allTypes.map(predicateTypeDeclaration)

  val predicateEq: Seq[String] = GoTypes.allTypes.map { goType =>
    val pt = toName(goType)
    s"""
       |func Eq$pt(e $goType) $pt {
       |	return func(o $goType) bool { return ${Equals.toName(goType)}(e, o) }
       |}""".stripMargin
  }
}
