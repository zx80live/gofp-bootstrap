package com.zx80live.gofp.bootstrap.types

case class LazyType(t: Type) extends Type {
  override def raw: String = s"Lazy${t.view}"

  override def alias: String = raw

  override def view: String = raw

  override def declaration: String =
    s"""
       |type $raw struct {
       |	eval   func() ${t.raw}
       |	cached *${t.raw}
       |}""".stripMargin

  override def funcEquals: String = ???

  override def funcToString: String =
    s"""
       |func (n $raw) ToString() string {
       |  if n.cached != nil {
       |    return fmt.Sprintf("${raw}(%v)", *n.cached)
       |  } else {
       |    return fmt.Sprintf("${raw}(?)")
       |  }
       |}""".stripMargin

  def funcValue: String =
    s"""
       |func (n $raw) Value() ${t.raw} {
       |	if n.cached != nil {
       |		//fmt.Println(" ${t.alias}.cached", *n.cached)
       |		return *n.cached
       |	} else {
       |		//fmt.Println("*${t.alias}.eval", n.eval())
       |		return n.eval()
       |	}
       |}""".stripMargin

  def funcEval: String =
    s"""
       |func (n $raw) Eval() $raw {
       |	if n.cached != nil {
       |		//fmt.Println(" ${t.alias}.cached", *n.cached)
       |		return n
       |	} else {
       |		cached := n.eval()
       |		//fmt.Println("*${t.alias}.eval", cached)
       |		return $raw{n.eval, &cached}
       |	}
       |}""".stripMargin

  def funcCached: String =
    s"""
       |func (n $raw) Cached() ${t.raw} { return *n.cached }
       |""".stripMargin

  def funcCons: String =
    s"""
       |func Mk$view(f func() ${t.raw}) $raw { return $raw { f , nil }}
       |""".stripMargin
}

object LazyType {
  def underlined: Seq[Type] = BaseType.reducedTypes

  def types: Seq[LazyType] = underlined.map(LazyType.apply)

  def declarations: Seq[String] = types.map(_.declaration)

  def functionsValue: Seq[String] = types.map(_.funcValue)

  def functionsEval: Seq[String] = types.map(_.funcEval)

  def functionsToString: Seq[String] = types.map(_.funcToString)

  def functionsCons: Seq[String] = types.map(_.funcCons)

  def functionsCached: Seq[String] = types.map(_.funcCached)
}
