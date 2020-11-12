package com.zx80live.gofp.bootstrap

object Optional {
  def toName(t: String): String = s"Option${GoTypes.toName(t)}"

  def toNoneName(t: String): String = s"None${GoTypes.toName(t)}"

  val optionalDeclarations: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
       |type ${toName(t)} struct { value *$t }""".stripMargin
  }

  val optionalNones: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
       |var None${GoTypes.toName(t)} ${toName(t)} = ${toName(t)} { nil } """.stripMargin
  }

  val optionalCons: Seq[String] = GoTypes.allTypes.map { t =>
    val name = if (t == GoTypes.GoAny) "AnyOpt" else GoTypes.toName(t)

    s"""
       |func $name(e $t) ${toName(t)} { return ${toName(t)} { &e } }
       |""".stripMargin
  }

  val optionalToString: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
       |func (o ${toName(t)}) ToString() string { if o == ${toNoneName(t)} { return "None" } else { return fmt.Sprintf("Some(%v)", ${ToStrings.toName(t)}(*o.value)) } }
       |""".stripMargin
  }
}
