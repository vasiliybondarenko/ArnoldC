package org.arnoldc

import org.scalatest._

abstract  class ArnoldGeneratorTest extends FlatSpec with Matchers {

  val arnoldGenerator = new ArnoldGenerator
  val byteCodeExecutor = new ByteCodeExecutor
  var className = "Hello"

  def getOutput(arnoldCode: String): String = {
    val bytecode = arnoldGenerator.generate(arnoldCode, className)
    byteCodeExecutor.getOutput(bytecode, className)
  }

  def evaluatingError(func: => Any): String = {
    (evaluating{
      func
    } should produce [Exception]).getMessage
  }

}