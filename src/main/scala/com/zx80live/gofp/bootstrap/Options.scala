package com.zx80live.gofp.bootstrap

object Options {
  def toName(t: String): String = s"Option${GoTypes.toName(t)}"

  val options: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
      |type ${toName(t)} struct {
      |}""".stripMargin
  }

}
