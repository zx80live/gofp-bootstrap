package com.zx80live.gofp.bootstrap

object Optional {
  val types : Seq[String] = GoTypes.allTypes
  val names: Seq[(String, String)] = types.map (t => (t, toName(t)))

  def toName(t: String): String = s"Option${GoTypes.toName(t)}"

  def toNoneName(t: String): String = s"None${GoTypes.toName(t)}"

  def toConsName(t: String): String = if (t == GoTypes.GoAny) "AnyOpt" else GoTypes.toName(t)

  val optionalDeclarations: Seq[String] = types.map { t =>
    s"""
       |type ${toName(t)} struct { value *$t }""".stripMargin
  }

  val optionalNones: Seq[String] = types.map { t =>
    s"""
       |var None${GoTypes.toName(t)} ${toName(t)} = ${toName(t)} { nil } """.stripMargin
  }

  val optionalCons: Seq[String] = types.map { t =>
    s"""
       |func ${toConsName(t)}(e $t) ${toName(t)} { return ${toName(t)} { &e } }
       |""".stripMargin
  }

  val optionalToString: Seq[String] = types.map { t =>
    s"""
       |func (o ${toName(t)}) ToString() string { if o == ${toNoneName(t)} { return "None" } else { return fmt.Sprintf("Some(%v)", ${ToStrings.toName(t)}(*o.value)) } }
       |""".stripMargin
  }

  val optionalIsDefined: Seq[String] = types.map { t =>
    s"""
       |func (o ${toName(t)}) IsDefined() bool { return o != ${toNoneName(t)} }
       |""".stripMargin
  }

  val optionalIsEmpty: Seq[String] = types.map { t =>
    s"""
       |func (o ${toName(t)}) IsEmpty() bool { return o == ${toNoneName(t)} }
       |""".stripMargin
  }

  val optionalFilter: Seq[String] = types.map { t =>
    s"""
       |func (o ${toName(t)}) Filter(p ${Predicates.toName(t)}) ${toName(t)} {
       |  if o.IsDefined() && p(*o.value) {
       |    return o
       |  } else {
       |    return ${toNoneName(t)}
       |  }
       |}
       |""".stripMargin
  }

  val optionalMap: Seq[String] = for {
    t1 <- types
    t2 <- types
  } yield {
    s"""
       |func (o ${toName(t1)}) Map${GoTypes.toName(t2)}(f ${Functors.toName(t1, t2)}) ${toName(t2)} {
       |  if o.IsDefined() {
       |    return ${toConsName(t2)}(f(*o.value))
       |  } else {
       |    return ${toNoneName(t2)}
       |  }
       |}""".stripMargin
  }
}
