/**IFTZCode331/linRegRecurseModA
 * USING ONLY  Scala 3.3.1
 * using
 * pure functions & immutable variables
 * ** NOTE I put the needed stat functions
 * in a separate object,"StatUtils",  that I then imported
 * I read in the dataset from local file
 * (  a homework assignment data set = A3.2SparkScala)
 * 2023-09-13 r.r   ( this worksheet is a clone of recurseGradientDescent.sc
 * I hope to get this one to work
 * trouble in printing out intermediate stuff with the other one
 */
import StatUtils.*

import scala.annotation.tailrec
import scala.math.*
println(s"new try at recursion1 linRegRecurseModA2023-09-13")
//read in local file used for A3.2SparkScala Assignment
val baseFnPath = "/Users/rob/Desktop/4IFTAdvBigDataAI/"
val fnPath = baseFnPath + "IFT512ExA32SparkScala.txt"
/**this returns a vector of strings, one string per line
I have to parse each line into the X features and Y labels*/
val data = io.Source.fromFile(fnPath).getLines.toVector
val X = data.map{str => str.split(",")(0).toDouble}
val Y = data.map{str => str.split(",")(1).toDouble}
val x = X.center
val y = Y.center
val ts = x zip y   //training set of x,y pairs, feature and label
/* cost Jcost = Sum(y - (w x+b))^2 -- this is simply SSE
return when iter ==  maxIters
as a check,

1. I want to return the below 6 element tuple at the end of the parent function
(w,b,avgJcost, alpha, maxIter,step)
2. the helper function iterates over 3 elements ( w, b, iter)
3. helper returns however, 4 elements (w,b,avgJcost,iter)
   where avgJcost is computed from the training set and w, b

I know there is one "w" and one "b" in this case  then I can calc avgJcost,
 given the training set
At intermediate iterations, I want to return:
(w,b, avgJcost,iter)
*/
// D(Z^2(/DZ = 2 Z
//D( Y-(WX +B) )^2 == > 2(Y-(WX+B))*-X
def LinearRegressionGDModA(ts:Vector[(D,D)],
                    alpha:D, maxIter:I,step:I ):(D,D,D,D,I,I)= {
  val m = ts.length.toDouble// number of training examples
  println(s" training set length = $m ")
  @tailrec
  def helperModA( w: D, b: D,
              iter: I): (D, D, D, I) = {
    val avgJcost = ts.map{case(x,y)=>pow(y-(w*x +b),2.0)}.sum/m   // SSE/m = MSE
    val avgdJdw =  ts.map { case (x, y) => 2.0 * (y - (w * x + b)) *(-x)}.sum/m
    val avgdJdb = ts.map{case(x,y) => (y -(w * x + b)) *(-1)}.sum/m

    if iter < maxIter then {
      if iter%step ==0 then
        println(s" YO ,  in recursion = $maxIter iter = $iter")
        println(s"w= $w, b= $b, avgJcost =$avgJcost,avgdJdw= $avgdJdw iter= $iter")
      helperModA(w - alpha * avgdJdw, b - alpha * avgdJdb, iter + 1)
    }
    else {
    println(s" At end of recursion")
    (w, b, avgJcost, iter)
    }
  }  // end helper
  // val (w,b,avgJcost,iter)=
  // final output from the Parent function, LinearRegressionGDModA
  // (w,b,avgJcost, alpha, maxIter,step) ( avgJcost should be 1.5/3
   val (w,b,avgJCost,iter) = helperModA(w=0.0, b= 0.0, 0)
  (w,b,avgJCost,alpha,maxIter,step)
}//end LinearRegressionGDModA

LinearRegressionGDModA(Vector((3.0,6.0),(4.0,9.0),(5.0, 15.0)),
 0.039,2500, 500)


/*val res0: (StatUtils.D, StatUtils.D, StatUtils.D, StatUtils.D, StatUtils.I,
  StatUtils.I) = (4.062642266318771,-6.179801612369885,0.6325292238645428,0.039,1000,100)
  */

