package org.arnoldc.parsers

import org.parboiled.errors.{ErrorUtils, DefaultInvalidInputErrorFormatter, InvalidInputError}
import scala.collection.JavaConverters._
import java.util
import org.parboiled.support.Chars
import org.parboiled.common.StringUtils
import org.parboiled.matchers.StringMatcher


class ArnoldInvalidInputErrorFormatter extends DefaultInvalidInputErrorFormatter{

  override def format(error: InvalidInputError): String = {
    if (error == null) return ""
    val len: Int = error.getEndIndex - error.getStartIndex
    val sb: StringBuilder = new StringBuilder
    if (len > 0) {
      val c: Char = error.getInputBuffer.charAt(error.getStartIndex)
      if (c == Chars.EOI) {
        sb.append("Unexpected end of input")
      }
      else {
        sb.append("Invalid input '").append(StringUtils.escape(String.valueOf(c)))
        if (len > 1) sb.append("...")
        sb.append('\'')
      }
    }
    else {
      sb.append("Invalid input")
    }

    formatExpectedMessage(sb, error)
    formatExpectedExpressionMessage(sb, error)
    return sb.toString
  }

  def formatExpectedMessage(sb:StringBuilder, error:InvalidInputError):StringBuilder = {
    val expectedString: String = getExpectedString(error)
    if (StringUtils.isNotEmpty(expectedString)) {
      sb.append(", expected ").append(expectedString)
    }
    sb
  }

  def formatExpectedExpressionMessage(sb:StringBuilder, error:InvalidInputError):StringBuilder = {
    val expectedExpression = getExpectedExpression(error)    
    if(StringUtils.isNotEmpty(expectedExpression)){
      sb.append("\nDid you mean ").append(expectedExpression).append("?\n")
    }else{
      sb.append("\nI AM NOT A MIND READER. YOU HAVE NO RESPECT FOR LOGIC\n")
    }
  }
  
  def getExpectedExpression(error: InvalidInputError): String = {
    val pathStartIndex: Int = error.getStartIndex - error.getIndexDelta
    val allLabels = new util.ArrayList[String]()
    for(path <- error.getFailedMatchers.asScala.toList){
      val matcher = ArnoldErrorUtils.findProperLabelMatcher(path, pathStartIndex)
      if(matcher.isInstanceOf[StringMatcher]){
        val labels = getLabels(matcher)
        for(label <- labels){
          if (label != null && !allLabels.contains(label))
            allLabels.add(label)
        }
      }
    }
    join(allLabels)
  }

  override def join(labelList: util.List[String]):String = {
    val sb = new StringBuilder()
    var i:Int = 0
    for(i <- 0 until labelList.size){
      val label = labelList.get(i)
      if(ArnoldErrorUtils.printableSequence(label)){
        if(i > 0) sb.append(if (i < labelList.size - 1) ", " else " or ")
        sb.append(labelList.get(i))
      }
    }
    sb.toString()
  }
}
