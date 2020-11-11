package com.zx80live.gofp.bootstrap

import java.io._

object Bootstrap extends App {

  def fileHeader(name: String, pack: String = "fp", imports: Seq[String] = Nil) =
    println(
      s"""// $name.go
         |// DO NOT EDIT THIS FILE WAS GENERATED AUTOMATICALLY BY gofp-bootstrap
         |
         |package $pack
         |
         |${imports.map(i => "import \"" + i + "\"").mkString("", "\n", "")}
         |""".stripMargin)


  def toFile(filename: String,
            pack: String = "fp",
            imports: Seq[String] = Nil,
             content: Seq[String] = Nil):Unit = {
    val writer = new BufferedWriter(new FileWriter(new File(filename)))

    try {
      writer.write(
        s"""// $filename
           |// DO NOT EDIT THIS FILE WAS GENERATED AUTOMATICALLY BY gofp-bootstrap
           |
           |package $pack
           |
           |${imports.map(i => "import \"" + i + "\"").mkString("", "\n", "")}
           |""".stripMargin
      )
      content foreach writer.write
    } finally {
      writer.close()
    }
  }


  toFile("fpredicate.go", content = Predicates.predicateTypeDeclarations)
  toFile("fequal.go", content = Equals.allEquals)
  toFile("fpredicate_eq.go", content = Predicates.predicateEq)
  toFile("fmkstring.go", imports = Seq("fmt"), content = MkStrings.arraysMkString)
  toFile("ftostring.go", imports = Seq("fmt"), content = ToStrings.allToStrings)
  toFile("ffunctor.go", content =
    Functors.functorTypeDeclarations ++ Functors.emptyFunctors
  )
  toFile("ffunctor_map.go", content = Functors.functorMaps)
  toFile("flist.go", content = List.lists)
}







