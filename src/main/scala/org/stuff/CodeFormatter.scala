package org.stuff

import java.nio.file._
//import org.scalafmt.interfaces.Scalafmt
import org.scalafmt.cli.Scalafmt210

object CodeFormatter {
  //val scalafmt = Scalafmt.create(this.getClass.getClassLoader)
  //val config = Paths.get(".scalafmt.conf")
  //val file = Paths.get("Main.scala")
  def apply(code: String): String =
    new Scalafmt210().format(code, "Main.scala")

  def applyInFoo(code: String): String =
    new Scalafmt210().format("object Foo {" + code + "}", "Main.scala")

  def main(args: Array[String]):Unit = {
    CodeFormatter.apply("object A  {  }")
  }
}