package com.zx80live.gofp.bootstrap.types

import com.zx80live.gofp.bootstrap.functions.{FuncEquals, FuncPredef, FuncToString}

case class Tuple2Type(e1: Type, e2: Type) extends TupleType {
  override def raw: String = s"Tuple2"

  override def alias: String = raw

  override def view: String = raw

  override def declaration: String =
    s"""
       |type $raw struct {
       |  E1 ${e1.alias}
       |  E2 ${e2.alias}
       |}""".stripMargin

  override def funcEquals: String =
    s"""
       |func (a $alias) Equals(b $alias) bool {
       |  return ${FuncEquals.name(e1)}(a.E1, b.E1) && ${FuncEquals.name(e2)}(a.E2, b.E2)
       |}""".stripMargin

  override def funcToString: String = {
    FuncPredef.underlinedTypes.map { t =>
      s"""
         |if reflect.TypeOf(t) == ${FuncPredef.reflectTypeName(t)} {
         |  s1 = ${FuncToString.name(t)}()
         |}
         |""".stripMargin
    }

    s"""
       |func (t $alias) ToString() String {
       |
       |  return String(fmt.Sprintf("Tuple(%v, %v)", ${FuncToString.name(e1)}(t.E1),${FuncToString.name(e2)}(t.E2)))
       |}""".stripMargin
  }

  def consName: String = s"MkTuple2"

  def funcCons: String =
    s"""
       |func $consName(e1, e2 Any) $raw { return $raw { e1, e2} }""".stripMargin
}

object Tuple2Type {

  def defaultType: Tuple2Type = Tuple2Type(BaseType.GoAny, BaseType.GoAny)

  def types: Seq[Tuple2Type] = Seq(defaultType)

  def declarations: Seq[String] = types.map(_.declaration)

  def functionsCons: Seq[String] = types.map(_.funcCons)

  def functionsEquals: Seq[String] = types.map(_.funcEquals)

  def functionsToString: Seq[String] = types.map(_.funcToString)
}