package jsy.student

import jsy.lab1._

object Lab1 extends jsy.util.JsyApplication with jsy.lab1.Lab1Like {
  import jsy.lab1.Parser
  import jsy.lab1.ast._

  /*
   * CSCI 3155: Lab 1
   * Spencer Milbrandt
   *
   * Partner: Ryan Whitmer
   * Collaborators: In-class walkthroughs and Prof. Hammer (mostly with correcting test errors)
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function. The
   * '???' expression is a Scala expression that throws a NotImplementedError
   * exception.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   */

  /*
   * Example: Test-driven development of plus
   *
   * A convenient, quick-and-dirty way to experiment, especially with small code
   * fragments, is to use the interactive Scala interpreter. The simplest way
   * to use the interactive Scala interpreter in IntelliJ is through a worksheet,
   * such as Lab1Worksheet.sc. A Scala Worksheet (e.g., Lab1Worksheet.sc) is code
   * evaluated in the context of the project with results for each expression
   * shown inline.
   *
   * Step 0: Sketch an implementation in Lab1.scala using ??? for unimmplemented things.
   * Step 1: Do some experimentation in Lab1Worksheet.sc.
   * Step 2: Write a test in Lab1Spec.scala, which should initially fail because of the ???.
   * Step 3: Fill in the ??? to finish the implementation to make your test pass.
   */

  //def plus(x: Int, y: Int): Int = ???
  def plus(x: Int, y: Int): Int = x + y

  /* Exercises */

  def abs(n: Double): Double =
    if (n > 0) n else -n // If the number is a real number return it, otherwise apply a negative value to return the absolute value.

  def xor(a: Boolean, b: Boolean): Boolean =
    if (a) if (b) false else true else if (b) true else false // Basic truth table for the xor boolean function.

  def repeat(s: String, n: Int): String =
  {
    require (n >= 0) // If no require then there would need to be a catch exception argument to deal with negative integers.
    n match
    { // Pattern matching to deal with all possible cases.
      case 0 =>
        "" // Base case, returns an empty string if there are no repeats n.
      case 1 =>
        s // Returns the string value s if there is only one repeat n.
      case _ if n > 1 =>
        s.concat(repeat(s, n-1)) // For anything greater than one repeat, concatenate to the string and recursively call until n == 0.
    }
  }

  def sqrtStep(c: Double, xn: Double): Double =
    xn - (((xn*xn)-c)/(2*xn)) // Calculating using Newton's Method.

  def sqrtN(c: Double, x0: Double, n: Int): Double =
  {
    require (n >= 0) // Can't be less than or equal to 0 when using the sqrt function, must be a real number.
    n match // Pattern matching to recursively call sqrtStep and return approximation of the nth degree.
    {
      case 0 => x0 // Base case, if n == 0 then return the initial guess, x0.
      case _ => sqrtN(c, sqrtStep(c,x0), n-1) /* Recursive call to sqrtStep where one iteration
      of Newton's Method is calculated and then reduce iteration by one until n == 0. */
    }
  }

  def sqrtErr(c: Double, x0: Double, epsilon: Double): Double =
  {
    require (epsilon > 0) // Can't be less than or equal to 0 when using the sqrt function, must be a real number.
    if (abs((x0*x0)-c) < epsilon) // This will terminate the recursive call when the approximation error is within epsilon and return x0.
      x0
    else
      sqrtErr(c, sqrtStep(c, x0), epsilon) // Recursive call of the sqrtStep function to continuously get a smaller x0 value.
  }

  def sqrt(c: Double): Double =
  {
    require(c >= 0)
    if (c == 0) 0 else sqrtErr(c, 1.0, 0.0001)
  }

  /* Search Tree */

  // Defined in Lab1Like.scala:
  //
  // sealed abstract class SearchTree
  // case object Empty extends SearchTree
  // case class Node(l: SearchTree, d: Int, r: SearchTree) extends SearchTree

  def repOk(t: SearchTree): Boolean =
  {
    def check(t: SearchTree, min: Int, max: Int): Boolean =
      t match
      {
        case Empty =>
          true
        case Node(l, d, r) => /* Since tMin and tMax are being passed through, when the traversal is left,
        all the nodes must be greater than or equal to the minimum value node and greater than the value of the
        previous node because d was passed in as the max. In doing so, when the traversal is right, all the values
        must be less than tMax, but still greater or equal to the max value. */

        check(l, min, d) && check(r, d, max) && (d >= min) && (d < max)

        /* Only two places this fails is on an unbalanced tree where the root is tMin and tMin was placed to
        the left and right of the root and if tMax is the root and a 2nd tMax is placed to the left or right. */
      }
    check(t, Int.MinValue, Int.MaxValue)
  }

  def insert(t: SearchTree, n: Int): SearchTree =
  {
    t match // Pattern matching to ensure an empty tree case and a true insert case.
    {
      case Empty =>
        Node(Empty, n, Empty) // Base case, if tree is empty insert n as the root.
      case Node(l, d, r) => // Check the current node.
      {
        if (n >= d) /* All values greater than or equal to the current node will be inserted right.
         Since there needs to be a linked node to the right, add a new current node that is connected
         to the new node with the new value n. */
          Node(l, d, insert(r, n))
        else
          Node(insert(l, n), d, r) // If n is less than the root node, go left.
      }
    }
  }

  def deleteMin(t: SearchTree): (SearchTree, Int) =
  {
    require(t != Empty)
    (t: @unchecked)
    match
    {
      case Node(Empty, d, r) =>
        (r, d)
      case Node(l, d, r) =>
        val (l1, m) = deleteMin(l)
        (Node(l1, d, r), m) /* Since the leftmost node will be the minimum value within the tree,
        delete the node and return m. */
    }
  }

  def delete(t: SearchTree, n: Int): SearchTree =
  {
    t match // Pattern matching to ensure empty tree case and node cases.
    {
      case Empty =>
        Empty // Empty tree results in no deletion.
      case Node(Empty, d, Empty) =>
        if (n == d) Empty // If tree is not empty but has no children nodes, return empty.
        else Node(Empty, d, Empty) // Else return the node since the value is not in the tree.
      case Node(Empty, d, r) =>
      {
        if (n > d) Node(Empty, d, delete(r, n)) // If greater than, traverse right and delete the node data.
        else if (n < d) Node(Empty, d, r) // If less than, the value is not in the tree, thus return the node.
        else r // Return the right node leaf value.
      }
      case Node(l, d, Empty) => // Same as traversing right except in reverse and for the left side.
      {
        if (n > d) Node(l, d, Empty)
        else if (n < d) Node(delete(l, n), d, Empty)
        else l
      }
      case Node(l, d, r) =>
      {
        if (n > d) Node(l, d, delete(r, n)) // If greater than, traverse right.
        else if (n < d) Node(delete(l, n), d, r) // If less than, traverse left.
        else // If equal, call deleteMin on the right side to replace the node that is about to be deleted.
        {
          val (r1, m) = deleteMin(r)
          Node(l, m, r1)
        }
      }

    }
  }
  /* JavaScripty */

  def eval(e: Expr): Double =
  {
    e match // Pattern matching for case classes N expression, Unary expression, and Binary expression with a catch exception case.
    {
      case N(n) => n // Return the numerical value of the expression, n.
      case Unary(Neg, e1) =>
        if (eval(e1) >= 0) -eval(e1) // If the exp is positive, then return a negative number.
        else eval(e1) // If e1 is negative, then negate and return the positive number.
      case Binary(Plus, e1, e2) =>
        eval(e1) + eval(e2) // Obtain the numerical expression through an eval recursive call, then add the values together.
      case Binary(Minus, e1, e2) =>
        eval(e1) - eval(e2) // Same as Binary Plus, except for difference.
      case Binary(Times, e1, e2) =>
        eval(e1) * eval(e2) // Same as Binary Plus, except for multiplication.
      case Binary(Div, e1, e2) =>
        eval(e1) / eval(e2) /* Same as Binary Plus, except for division. The eval function takes care
      of negative and positive infinity already as well as dividing into 0. */
      case _ => throw new UnsupportedOperationException
    }
  }

 // Interface to run your interpreter from a string.  This is convenient
 // for unit testing.
 def eval(s: String): Double = eval(Parser.parse(s))



 /* Interface to run your interpreter from the command-line.  You can ignore the code below. */

 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

    println(prettyNumber(v))
  }

}
