package com.zx80live.gofp.bootstrap.types

import BaseType._

case class Predicate(t: Type) {
  def name: String = Predicate.name(t)

  def emptyName: String = s"${t.view}EmptyPredicate"

  def declaration: String =
    s"""
       |type $name func(t ${t.raw}) bool""".stripMargin

  def emptyDeclaration: String =
    s"""
       |var $emptyName $name = func(t ${t.raw}) bool { return true }""".stripMargin

  // predicate composition
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

  // numeric predicates
  def funcEven: String = if (t.isInteger) {
    s"""
       |var Even${t.view} $name = func(t ${t.raw}) bool { return t % 2 == 0 }""".stripMargin
  } else ""

  def funcOdd: String = if (t.isInteger) {
    s"""
       |var Odd${t.view} $name = func(t ${t.raw}) bool { return t % 2 != 0 }""".stripMargin
  } else ""

  def funcNegNum: String = if (t.isInteger) {
    s"""
       |var Neg${t.view} $name = func(t ${t.raw}) bool { return t < 0 }""".stripMargin
  } else ""

  def funcPosNum: String = if (t.isInteger) {
    s"""
       |var Pos${t.view} $name = func(t ${t.raw}) bool { return t >= 0 }""".stripMargin
  } else ""

  def funcZeroNum: String = if (t.isInteger) {
    s"""
       |var Zero${t.view} $name = func(t ${t.raw}) bool { return t == 0 }""".stripMargin
  } else ""

  def funcOneNum: String = if (t.isInteger) {
    s"""
       |var One${t.view} $name = func(t ${t.raw}) bool { return t == 1 }""".stripMargin
  } else ""

  // string predicates
  def funcMatchRegex: String = if (t == GoString) {
    s"""
       |func MatchRegexp(r *regexp.Regexp) StringPredicate {
       |	return func(s string) bool {
       |		return r.MatchString(s)
       |	}
       |}""".stripMargin
  } else ""

  def funcMatchRegexString: String = if (t == GoString) {
    s"""
       |func MatchRegexpString(pattern string) StringPredicate {
       |	return MatchRegexp(regexp.MustCompile(pattern))
       |}""".stripMargin
  } else ""
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

  def mathPredicates: Seq[String] =
    BaseType.integerTypes.map(Predicate.apply).map(_.funcEven) ++
      BaseType.integerTypes.map(Predicate.apply).map(_.funcOdd) ++
      BaseType.integerTypes.map(Predicate.apply).map(_.funcNegNum) ++
      BaseType.integerTypes.map(Predicate.apply).map(_.funcPosNum) ++
      BaseType.integerTypes.map(Predicate.apply).map(_.funcZeroNum) ++
      BaseType.integerTypes.map(Predicate.apply).map(_.funcOneNum)

  def stringPredicates: Seq[String] = Seq(
    Predicate(GoString).funcMatchRegex,
    Predicate(GoString).funcMatchRegexString
  )
}
