/**IFT512ZCode331/recurseSum
 * practicing the recursion routine
 * 2023-09-09 rr
 *
 * Code  --- Algorithms ----- Infrastructure ( Spark & Lakehouse)
 */

type I = Int; type D = Double ; type V = Vector[D]
type S =String
import scala.annotation.tailrec
import scala.math.pow
// A Unicode example , just to show the unicode code!
// USING the Euro symbol \u20ac ( IF MY EDITOR HAS THIS?
val s = f"\u20ac a unicode symbol for the euro"
//here is a function that needs it...
def fn1(amount:D)= f" \u20ac $amount%.2f "
fn1{ 3.4 * 10 }
//************ Basic recursion examples *************




/**IFT512ZCode331/recurseSum.sc
 * This is the imperative approach with
 * mutated variable "sum"
 * @param is
 * @return
 * r.r 2023-09-18
 */
def sumImperative( is: List[I]):I =
  var sum:I = 0
   for i <- is   do
     sum = sum + i
   sum
sumImperative(List(1,2,3))

/** Functional approach using immutable variables
 *
 * but,  I want 3 outcomes,
 * not just the two arguments I put
 * into sumA,
 * for whatever reason!
 *
 * @param is // this is the list of ints, i.e., "is"
 * @return (iter, accum, testvalue)
 *  So, I  wanted to return more from the helper then
 *  just iter and accum:
 *  So I declare the types I plan to return
 *  e.g.def sumA(is : List[I]):(I,I,I ,D)
 *  then at the very end, instead of
 * (iter,accum, a), I could put
 *  (iter,accum, a   , somethingD-like)
 *  *** just edited sumA to do this
 *  and returned an arbitrary "I" of 42
 *   I also added an arbitrary element to helper
 *    but that may be more useful since helper can calc
 *  iteratively
 */
def sumA(is : List[I]):(I,I,I)= {
  /**
   * the "is" keeps getting shrunk so,  it has to be
   *  an argument in helper,
   *
    * @param is
   * @param iter
   * @param accum
   * @return  iter, accum , arbitrary value, your choice
   */
  @tailrec
  def helper(is: List[I], iter: I, accum: I): (I, I,I) = {
     is match
      case Nil => (iter, accum,42)
      case i :: is => {
        println(s"\n iter = $iter accum = $accum")
        helper(is, iter + 1, accum + i)
      }
   }//end helper
  val ( iter,accum, a) = helper(is, 0, 0)
  (iter,accum, 42)
}
val (iters,total,junk) = sumA(List(1,2,3,4,5))

val list = List(1,2).head
/**
 * sum a list using recursion and if-else
 *
 *
 */
def sumB(is : List[I]):(I,I,I)= {
  // is  keeps getting shrunk so:it has to be an argument in helper
  def helper(is: List[I], iter: I, accum: I): (I, I,I) = {
    if is == Nil then
      ( iter, accum, 42)
    else helper(is.tail, iter + 1, accum + is.head)
  }//end helper
  val ( iter,accum, a) = helper(is, 0, 0)
  (iter,accum, a)
}
val (iterB,totalB,junkB) = sumB(List(1,2,3,4,5))

/** Recursion examples : using if-else or match expresskons */
import scala.math.*
def power(x:I, n:I):Long=
  if n>= 1 then x * power(x,n-1)
  else 1
power(2,3)

val p = (z:D) => pow(z,2)
//def nest(x:D,power:Function1[D,D]
//return ( x, total, iter) ={
def nest(x:D,p:(z:D) =>D,total:D,maxIter:I,step:I):(D,D,I)={
  def helper(x:D, total:D, iter:I):(D,D,I)= {
    if iter< maxIter then {
      if iter%step == 0 then
        print(s" iter = $iter")
      helper(x, total + pow(x, 2), iter + 1)
      }
    else
      (x,  total, iter)
  }//end helper
  helper(2.0,0.0,0 )
}//end nest
nest(2.0,p,0,10,2)

