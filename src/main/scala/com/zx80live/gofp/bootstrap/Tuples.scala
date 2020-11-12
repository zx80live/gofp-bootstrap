package com.zx80live.gofp.bootstrap

object Tuples {
  def toName(size: Int): String = s"Tuple$size"

  val tuples: Seq[String] = (2 to 32).map {i =>
    s"""
       |type ${toName(i)} struct {
       |${(1 to i).map(id => "E"+id + " Any").mkString("  ","\n  ", "")}
       |}
       |""".stripMargin
  }

}
