package org.arnoldc.parsers

import org.parboiled.errors.{ErrorUtils, DefaultInvalidInputErrorFormatter, InvalidInputError}
import scala.collection.JavaConverters._
import java.util
import org.parboiled.support.{MatcherPath, Chars}
import org.parboiled.common.StringUtils
import org.parboiled.matchers.{Matcher, StringMatcher}


class ArnoldInvalidInputErrorFormatter extends DefaultInvalidInputErrorFormatter{

  override def format(error: InvalidInputError): String = {
    if (error == null) return ""
    val len: Int = error.getEndIndex - error.getStartIndex
    val sb: StringBuilder = new StringBuilder
    error.getInputBuffer.charAt(error.getStartIndex) match {
      case Chars.EOI => sb.append("Unexpected end of input")
      case c => {
        sb.append("Invalid input '").append(StringUtils.escape(String.valueOf(c)))
        if (len > 1) sb.append("...")
        sb.append('\'')
      }
    }

    formatExpectedMessage(sb, error)
    formatExpectedExpressionMessage(sb, error)
    return sb.toString
  }

  private def formatExpectedMessage(sb:StringBuilder, error:InvalidInputError):StringBuilder = {
    getExpectedString(error) match {
      case "" | null => sb
      case exp => sb.append(", expected ").append(exp)
    }
  }

  private def formatExpectedExpressionMessage(sb:StringBuilder, error:InvalidInputError):StringBuilder = {
    getExpectedExpression(error) match {
      case "" | null => sb.append("\nI AM NOT A MIND READER. YOU HAVE NO RESPECT FOR LOGIC\n")
      case exp => sb.append("\nDid you mean ").append(exp).append("?\n")
    }
  }
  
  def getExpectedExpression(error: InvalidInputError): String = {
    val pathStartIndex: Int = error.getStartIndex - error.getIndexDelta
    val allLabels = new util.ArrayList[String]()
    val paths = error.getFailedMatchers.asScala.toList
    paths.toStream.map(
      (mp:MatcherPath) => ArnoldErrorUtils.findProperLabelMatcher(mp:MatcherPath, pathStartIndex))
      .filter(_.isInstanceOf[StringMatcher])
      .foreach(collectLabels(_, allLabels))
    join(allLabels)
  }

  private def collectLabels(matcher:Matcher, allLabels:util.List[String]):util.List[String] = {
    val labels = getLabels(matcher)
    labels.foreach((label:String) => if (label != null && !allLabels.contains(label)) allLabels.add(label))
    allLabels
  }

  override def join(labelList: util.List[String]):String = {
    val sb = new StringBuilder()
    iterate(labelList, ArnoldErrorUtils.printableSequence, 
      (index:Int, label:String) => appendMatcherLabel(sb, index, label, labelList.size()))
    sb.toString()
  }

  private def appendMatcherLabel(sb:StringBuilder, i:Int, label:String, count:Int){
    sb.append(labelsSplitter(i, count))
    sb.append(label)
  }
  
  private def labelsSplitter(index:Int, count:Int):String = {
    if(index > 0) if (index < count - 1) ", " else " or "
    else ""
  }

  private def iterate(list:util.List[String], filter:(String => Boolean), func: ((Int, String) => Unit)){
    for(i <- 0 until list.size){
      val label = list.get(i)
      if(filter(label)){
        func(i, label)
      }
    }
  }
}
