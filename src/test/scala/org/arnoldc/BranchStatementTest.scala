package org.arnoldc

class BranchStatementTest extends ArnoldGeneratorTest{
  it should "function using simple if statements" in {
    val code =
      "IT'S SHOWTIME\n" +
        "HEY CHRISTMAS TREE vartrue\n" +
        "YOU SET US UP @NO PROBLEMO\n" +
        "BECAUSE I'M GOING TO SAY PLEASE vartrue\n" +
        "TALK TO THE HAND \"this branch should be reached\"\n" +
        "YOU HAVE NO RESPECT FOR LOGIC\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("this branch should be reached\n")
  }

  it should "function using simple if statements vol2" in {
    val code =
      "IT'S SHOWTIME\n" +
        "HEY CHRISTMAS TREE vartrue\n" +
        "YOU SET US UP @I LIED\n" +
        "BECAUSE I'M GOING TO SAY PLEASE vartrue\n" +
        "TALK TO THE HAND \"this branch should not be reached\"\n" +
        "YOU HAVE NO RESPECT FOR LOGIC\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("")
  }

  it should "function using simple if else statements" in {
    val code =
      "IT'S SHOWTIME\n" +
        "HEY CHRISTMAS TREE vartrue\n" +
        "YOU SET US UP @NO PROBLEMO\n" +
        "BECAUSE I'M GOING TO SAY PLEASE vartrue\n" +
        "TALK TO THE HAND \"this branch should be reached\"\n" +
        "BULLSHIT\n" +
        "TALK TO THE HAND \"this branch should not be reached\"\n" +
        "YOU HAVE NO RESPECT FOR LOGIC\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("this branch should be reached\n")
  }

  it should "function using simple if else statements vol2" in {
    val code =
      "IT'S SHOWTIME\n" +
        "HEY CHRISTMAS TREE varfalse\n" +
        "YOU SET US UP @I LIED\n" +
        "BECAUSE I'M GOING TO SAY PLEASE varfalse\n" +
        "TALK TO THE HAND \"this branch should not be reached\"\n" +
        "BULLSHIT\n" +
        "TALK TO THE HAND \"this branch should be reached\"\n" +
        "YOU HAVE NO RESPECT FOR LOGIC\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("this branch should be reached\n")
  }

  it should "function using assigning variables in if statements" in {
    val code =
      "IT'S SHOWTIME\n" +
        "HEY CHRISTMAS TREE var\n" +
        "YOU SET US UP 0\n" +
        "HEY CHRISTMAS TREE vartrue\n" +
        "YOU SET US UP @NO PROBLEMO\n" +
        "BECAUSE I'M GOING TO SAY PLEASE vartrue\n" +
        "GET TO THE CHOPPER var\n" +
        "HERE IS MY INVITATION 3\n" +
        "ENOUGH TALK\n" +
        "YOU HAVE NO RESPECT FOR LOGIC\n" +
        "TALK TO THE HAND var\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("3\n")
  }


  it should "function using stub while statement" in {
    val code =
      "IT'S SHOWTIME\n" +
        "HEY CHRISTMAS TREE varfalse\n" +
        "YOU SET US UP @I LIED\n" +
        "STICK AROUND varfalse\n" +
        "CHILL\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("")
  }


  it should "function using stub while statement vol2" in {
    val code =
      "IT'S SHOWTIME\n" +
        "STICK AROUND @I LIED\n" +
        "CHILL\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("")
  }



  it should "function when while loop executed once" in {
    val code =
      "IT'S SHOWTIME\n" +
        "HEY CHRISTMAS TREE varfalse\n" +
        "YOU SET US UP @NO PROBLEMO\n" +
        "STICK AROUND varfalse\n" +
        "GET TO THE CHOPPER varfalse\n" +
        "HERE IS MY INVITATION @I LIED\n" +
        "ENOUGH TALK\n" +
        "TALK TO THE HAND \"while statement printed once\"\n" +
        "CHILL\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("while statement printed once\n")
  }

  it should "function when while loop executed consequently" in {
    val code =
      "IT'S SHOWTIME\n" +
        "HEY CHRISTMAS TREE isLessThan10\n" +
        "YOU SET US UP @NO PROBLEMO\n" +
        "HEY CHRISTMAS TREE n\n" +
        "YOU SET US UP 0\n" +
        "STICK AROUND isLessThan10\n" +
        "GET TO THE CHOPPER n\n" +
        "HERE IS MY INVITATION n\n" +
        "GET UP 1\n" +
        "ENOUGH TALK\n" +
        "TALK TO THE HAND n\n" +
        "GET TO THE CHOPPER isLessThan10\n" +
        "HERE IS MY INVITATION 10\n" +
        "LET OFF SOME STEAM BENNET n\n" +
        "ENOUGH TALK\n" +
        "CHILL\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n")
  }

  it should "break while loop" in {
    val code =
      "IT'S SHOWTIME\n" +
        "STICK AROUND @NO PROBLEMO\n" +
        "TALK TO THE HAND \"ONE\"\n" +
        "GET OUT @NO PROBLEMO\n" +
        "CHILL\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("ONE\n")
  }

  it should "break while loop with alternative break" in {
    val code =
      "IT'S SHOWTIME\n" +
        "STICK AROUND @NO PROBLEMO\n" +
        "TALK TO THE HAND \"ONE\"\n" +
        "I NEED A VACATION @NO PROBLEMO\n" +
        "CHILL\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("ONE\n")
  }

  it should "break while loop if condition is satisfied" in {
    val code =
      "IT'S SHOWTIME\n" +
        "HEY CHRISTMAS TREE var\n" +
        "YOU SET US UP @NO PROBLEMO\n" +
        "STICK AROUND var\n" +
        "TALK TO THE HAND \"ONE\"\n" +
        "GET OUT var\n" +
        "CHILL\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("ONE\n")
  }

  it should "break while loop if condition is satisfied on second iteration" in {
    val code =
      "IT'S SHOWTIME\n" +
        "HEY CHRISTMAS TREE i\n" +
        "YOU SET US UP 0\n" +
        "HEY CHRISTMAS TREE isEqual2\n" +
        "YOU SET US UP @I LIED\n" +
        "STICK AROUND @NO PROBLEMO\n" +
        "TALK TO THE HAND i\n" +
        "GET OUT isEqual2\n" +
        "GET TO THE CHOPPER i\n" +
        "HERE IS MY INVITATION i\n" +
        "GET UP 1\n" +
        "ENOUGH TALK\n" +
        "GET TO THE CHOPPER isEqual2\n" +
        "HERE IS MY INVITATION @NO PROBLEMO\n" +
        "ENOUGH TALK\n" +
        "CHILL\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("0\n1\n")
  }

  it should "break while loop if i equal 2" in {
    val code =
      "IT'S SHOWTIME\n" +
        "HEY CHRISTMAS TREE i\n" +
        "YOU SET US UP 0\n" +
        "HEY CHRISTMAS TREE isEqual2\n" +
        "YOU SET US UP @I LIED\n" +
        "STICK AROUND @NO PROBLEMO\n" +
        "TALK TO THE HAND i\n" +
        "GET OUT isEqual2\n" +
        "GET TO THE CHOPPER i\n" +
        "HERE IS MY INVITATION i\n" +
        "GET UP 1\n" +
        "ENOUGH TALK\n" +
        "GET TO THE CHOPPER isEqual2\n" +
        "HERE IS MY INVITATION i\n" +
        "YOU ARE NOT YOU YOU ARE ME 2\n" +
        "ENOUGH TALK\n" +
        "CHILL\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("0\n1\n2\n")
  }

  it should "throw an exception if break is outside of while loop" in {
    evaluatingError {
      val code =
        "IT'S SHOWTIME\n" +
        "GET OUT @NO PROBLEMO\n" +
        "YOU HAVE BEEN TERMINATED\n"
      getOutput(code)
    } should startWith (
      "WHAT THE FUCK DID I DO WRONG:\n" +
      "Break statement cannot be placed outside of loop\n"
    )
  }

  it should "function when for loop executed consequently from 1 to 2" in {
    val code =
        "IT'S SHOWTIME\n" +
        "FLY OR DIE n 1 2\n" +
        "TALK TO THE HAND n\n" +
        "RELAX\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("1\n2\n")
  }

  it should "function when for loop executed consequently from 1 to max variable" in {
    val code =
        "IT'S SHOWTIME\n" +
        "HEY CHRISTMAS TREE max\n" +
        "YOU SET US UP 2\n" +
        "FLY OR DIE n 1 max\n" +
        "TALK TO THE HAND n\n" +
        "RELAX\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("1\n2\n")
  }

  it should "function when for loop executed consequently from min variable to max variable" in {
    val code =
        "IT'S SHOWTIME\n" +
        "HEY CHRISTMAS TREE max\n" +
        "YOU SET US UP 2\n" +
        "HEY CHRISTMAS TREE min1\n" +
        "YOU SET US UP 1\n" +
        "FLY OR DIE n min1 max\n" +
        "TALK TO THE HAND n\n" +
        "RELAX\n" +
        "YOU HAVE BEEN TERMINATED\n"
    getOutput(code) should equal("1\n2\n")
  }

}
