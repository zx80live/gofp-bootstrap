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
}


object Predicate {
  def name(t: Type): String = s"${t.view}Predicate"

  def underlinedTypes: Seq[Type] = BaseType.types ++ ArrayType.types ++ OptionType.types ++ ListType.types

  def declarations: Seq[String] = underlinedTypes.map(Predicate.apply).map(_.declaration)
  def emptyDeclarations: Seq[String] = underlinedTypes.map(Predicate.apply).map(_.emptyDeclaration)
}
