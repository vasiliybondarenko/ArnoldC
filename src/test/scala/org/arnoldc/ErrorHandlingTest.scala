package org.arnoldc


class ErrorHandlingTest extends ArnoldGeneratorTest{

  it should "throw an exception with proper expression if there is a typo in expression" in {
    evaluatingError {
      val code =
        "ITS SHOWTIME\n" +
        "YOU HAVE BEEN TERMINATED\n"
      getOutput(code)
    } should startWith (
        "WHAT THE FUCK DID I DO WRONG:\n" +
        "Invalid input 'S', expected '''\n" +
        "Did you mean \"IT'S SHOWTIME\"?\n" +
        "(line 1, pos 3):\n" +
        "ITS SHOWTIME\n" +
        "  ^"
    )
  }

  it should "throw an exception with proper suggestions if there is no expression found started with the given string" in {
    evaluatingError {
      val code =
        "IT'S SHOWTIME\n" +
        "A\n"
      getOutput(code)
    } should startWith (
        "WHAT THE FUCK DID I DO WRONG:\n" +
        "Invalid input 'A', expected '\\t', '\\r', ' ', '\\n', Statement or \"YOU HAVE BEEN TERMINATED\"\n" +
        "I AM NOT A MIND READER. YOU HAVE NO RESPECT FOR LOGIC\n" +
        "(line 2, pos 1):\n" +
        "A\n" +
        "^"
    )
  }

  it should "throw an exception with Unexpected end of input message" in {
    evaluatingError {
      val code =
        "IT'S SHOWTIME\n" +
        "TALK TO THE HAND \"A\n"
      getOutput(code)
    } should startWith (
      "WHAT THE FUCK DID I DO WRONG:\n" +
      "Unexpected end of input, expected apply or '\"'\n" +
      "I AM NOT A MIND READER. YOU HAVE NO RESPECT FOR LOGIC\n"
    )
  }

}
