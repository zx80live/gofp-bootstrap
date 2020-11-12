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

  val predicateAnd: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
       |func (p1 ${toName(t)}) And(p2 ${toName(t)}) ${toName(t)} { return func(e $t) bool { return p1(e) && p2(e) } }
       |""".stripMargin
  }

  val predicateOr: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
       |func (p1 ${toName(t)}) Or(p2 ${toName(t)}) ${toName(t)} { return func(e $t) bool { return p1(e) || p2(e) } }
       |""".stripMargin
  }

  val predicateXor: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
       |func (p1 ${toName(t)}) Xor(p2 ${toName(t)}) ${toName(t)} { return func(e $t) bool { x := p1(e); y := p2(e); return (x || y) && !(x && y) } }
       |""".stripMargin
  }

  val predicateNeg: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
       |func (p ${toName(t)}) Neg() ${toName(t)} { return func(e $t) bool { return !p(e) } }
       |""".stripMargin
  }
}
