package com.zx80live.gofp.bootstrap

object List {

  def toName(goType: String): String = s"List${GoTypes.toName(goType)}"

  def listDeclaration(goType: String): String =
    s"""
      |type ${toName(goType)} struct {
      |	head    $goType
      |	tail    *${toName(goType)}
      |	functor ${Functors.toName(goType, goType)}
      |}
      |""".stripMargin

  val lists: Seq[String] = GoTypes.allTypes map listDeclaration
}
