package example

object ReversePolishNotation {

  import cats.data.State

  /*
   * You can think of State monad of something like this:
   *
   * type State[S, A] = S => (S, A)
   *
   * For example, you can create instances of state with the following structure:
   *
   * val s = State[String, String] { originalString =>
   *   (originalString, originalString.toUpperCase)
   * }
   * s.runA("hello").value === "HELLO"
   *
   * */

  type CalcState[A] = State[List[Int], A]

  /*
   * TODO 06
   * This function must support the symbols {+,-,*,/}
   *
   * If the symbol is a number
   *   stack-it
   * otherwise
   *   pop the last two symbols
   *   execute the operation
   *   stack the result
   *
   * Note: for simplicity do not implement any error handling!
   *
   */
  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operation(_ + _)
    case "-" => operation(_ - _)
    case "*" => operation(_ * _)
    case "/" => operation(_ / _)
    case a   => State(s => (a.toInt :: s, a.toInt))
  }

  private def operation(f: (Int, Int) => Int): State[List[Int], Int] =
    State.apply {
      case a :: b :: t =>
        val c = f(a, b)
        (c :: t, c)
      case _ => throw new IllegalArgumentException("Missing operands for given operator")
    }

  /*
   * TODO 06: Fold over the list calling evalOne
   *
   * Your empty element could be calculated like the 'pure state 0':
   * import cats.syntax.applicative._
   * 0.pure[CalcState]
   */
  def evalAll(sym: List[String]): CalcState[Int] =
    sym match {
      case h :: Nil => evalOne(h)
      case h :: t   => evalOne(h).flatMap(_ => evalAll(t))
      case _        => throw new IllegalArgumentException("empty list")
    }

}
