package com.zx80live.gofp.bootstrap.refactored.types

case class Predicate(t: Type) {
  def name: String = Predicate.name(t)

  def emptyName: String = s"${t.view}EmptyPredicate"

  def declaration: String =
    s"""
       |type $name func(t ${t.raw}) bool""".stripMargin

  def emptyDeclaration: String =
    s"""
       |var $emptyName $name = func(t ${t.raw}) bool { return true }""".stripMargin

  def funcAnd: String =
    s"""
       |func (p1 $name) And(p2 $name) $name { return func(e ${t.raw}) bool {  return p1(e) && p2(e)  } }""".stripMargin

  def funcOr: String =
    s"""
       |func (p1 $name) Or(p2 $name) $name { return func(e ${t.raw}) bool { return p1(e) || p2(e) } }""".stripMargin

  def funcXor: String =
    s"""
       |func (p1 $name) Xor(p2 $name) $name { return func(e ${t.raw}) bool { x := p1(e); y := p2(e); return (x || y) && !(x && y) } }""".stripMargin

  def funcNeg: String =
    s"""
       |func (p $name) Neg() $name { return func(e ${t.raw}) bool { return !p(e) } }""".stripMargin
}


object Predicate {
  def name(t: Type): String = s"${t.view}Predicate"

  def underlinedTypes: Seq[Type] = BaseType.types ++ ArrayType.types ++ OptionType.types ++ ListType.types

  def declarations: Seq[String] = underlinedTypes.map(Predicate.apply).map(_.declaration)
  def emptyDeclarations: Seq[String] = underlinedTypes.map(Predicate.apply).map(_.emptyDeclaration)
  def functionsAnd: Seq[String] = underlinedTypes.map(Predicate.apply).map(_.funcAnd)
  def functionsOr: Seq[String] = underlinedTypes.map(Predicate.apply).map(_.funcOr)
  def functionsXor: Seq[String] = underlinedTypes.map(Predicate.apply).map(_.funcXor)
  def functionsNeg: Seq[String] = underlinedTypes.map(Predicate.apply).map(_.funcNeg)
}
