package com.zx80live.gofp.bootstrap.functions

import com.zx80live.gofp.bootstrap.types.{ArrayType, ListType, MonadType, Type}

@deprecated
object FuncMkString {

  def name(t: MonadType): String = s"MkString${t.view}"

  def contract(t: MonadType): String = s"func ${name(t)}(o ${t.raw}, start, sep, end string) string"

  def body(t: MonadType): String = t match {
    case a: ArrayType =>
      s"""
         |	 content := ""
         |	 for _, e := range o {
         |	   content = fmt.Sprintf("%v%v%v", content, ${FuncToString.name(t.underlined)}(e), sep)
         |	 }
         |	 l := len(content)
         |	 if l > 0 {
         |		 content = content[:l-1]
         |	 }
         |	 return fmt.Sprintf("%v%v%v", start, content, end)
         |""".stripMargin
    case _ =>
      s"""
         |   content := ""
         |   xs := o
         |   for xs.NonEmpty() {
         |     content = fmt.Sprintf("%v%v%v", content, ${FuncToString.name(t.underlined)}(*xs.head), sep)
         |     xs = *xs.tail
         |   }
         |	 l := len(content)
         |	 if l > 0 {
         |		 content = content[:l-1]
         |	 }
         |	 return fmt.Sprintf("%v%v%v", start, content, end)
         |""".stripMargin
  }

  def func(t: MonadType): String =
    s"""
       |${contract(t)} {
       |  ${body(t)}
       |}""".stripMargin

  def functions: Seq[String] = (ArrayType.types ++ ListType.types).map(func)
}
