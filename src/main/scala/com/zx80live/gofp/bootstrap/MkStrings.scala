package com.zx80live.gofp.bootstrap

object MkStrings {
  def toName(goType: String): String = s"MkString${GoTypes.toName(goType)}"

  val arraysMkString: Seq[String] = GoTypes.arrayTypes.map { goType =>
    s"""
      |func ${toName(goType)}(arr $goType, left, sep, right string) string {
      |	 content := ""
      |	 for _, e := range arr {
      |	   content = fmt.Sprintf("%v%v%v", content, e, sep)
      |	 }
      |	 l := len(content)
      |	 if l > 0 {
      |		 content = content[:l-1]
      |	 }
      |	 return fmt.Sprintf("%v%v%v", left, content, right)
      |}
      |""".stripMargin
  }

  val nestedArraysMkString: Seq[String] = GoTypes.nestedArrayTypes.map { goType =>
    s"""
       |func ${toName(goType)}(arr $goType, left, sep, right string) string {
       |	 content := ""
       |	 for _, e := range arr {
       |	   content = fmt.Sprintf("%v%v%v", content, ${toName(goType).dropRight("Arr".length)}(e, left, sep, right), sep)
       |	 }
       |	 l := len(content)
       |	 if l > 0 {
       |		 content = content[:l-1]
       |	 }
       |	 return fmt.Sprintf("%v%v%v", left, content, right)
       |}
       |""".stripMargin
  }

  val listMkString: Seq[String] = Lists.names.map { case (t, tList) =>
    s"""
       |func ${toName(tList)}(l $tList, left, sep, right string) string { return ${toName(t)}Arr(l.ToArray(), left, sep, right) }""".stripMargin
  }
}
