package org.arnoldc.parsers


import org.parboiled.errors.{InvalidInputError, ParseError, ErrorUtils}
import org.parboiled.scala.ParsingResult
import org.parboiled.common.Preconditions._
import org.parboiled.support.{Chars, MatcherPath}
import org.parboiled.matchers.{StringMatcher, TestNotMatcher, Matcher}

object ArnoldErrorUtils{
  def printParseErrors(parsingResult: ParsingResult[_]):String ={
    checkArgNotNull(parsingResult, "parsingResult")
    printParseErrors(parsingResult.parseErrors)
  }

  def printParseErrors(errors: List[ParseError]):String = {
    checkArgNotNull(errors, "errors")
    val sb = new StringBuilder

    errors.foreach{ (e:ParseError) =>
      if(sb.length > 0) sb.append("---\n")
      sb.append(printParseError(e))
    }
    sb.toString()
  }

  def printParseError(error: ParseError): String = {
    checkArgNotNull(error, "error")
    checkArgNotNull(formatter, "formatter")
    val message: String = getErrorMessage(error)
    ErrorUtils.printErrorMessage("%s(line %s, pos %s):", message, error.getStartIndex, error.getEndIndex, error.getInputBuffer)
  }

  def getErrorMessage(error:ParseError):String = {
    if (error.getErrorMessage != null)
      error.getErrorMessage
    else if (error.isInstanceOf[InvalidInputError])
      formatter.format(error.asInstanceOf[InvalidInputError])
    else
      error.getClass.getSimpleName
  }

  def findProperLabelMatcher(path:MatcherPath, pathStartIndex:Int): Matcher = {
    checkArgNotNull(path, "path")
    val found: Matcher = if (path.parent != null) findProperLabelMatcher(path.parent, pathStartIndex) else null
    if(found != null){
      found
    }else{
      val m = path.element.matcher
      if (path.element.startIndex == pathStartIndex && m.hasCustomLabel){
        path.parent.element.matcher
      }
      else null
    }
  }

  def printableSequence(str:String):Boolean = {
    !str.contains("INDENT") && !str.contains("DEDENT")
  }

  def formatter = new ArnoldInvalidInputErrorFormatter

}
