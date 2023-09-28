/**IFT512Zcode331LogReg09-20/LogRegGDTest.scala
 * Logistic Regression Gradient Descent,
 * with tail-recursion, using only Scala 3.3.1,no libraries
 * Running In IntelliJ 2.2.3
 * For Binary classification I only need "w" and "b"
 * slope & intercept
 * Training set= ts: Use Hosmer(1989) 100 pairs of
 * (age,cardiacDefect) clients:
 * e.g. (age = 68,1) means: person was 68 and
 * diagnosed with cardiac defect = 1. While ( 23,0) means
 * person was 23 and no defect, therefore 0.
 * First: I read in (x,y) pairs and do preprocessing to calculate
 * an interim value,the  sigmoid function(z)=1/(1+exp(-z))
 * z = w * x + b, and then
 * a= sigmoid[z] , I bundle this up with the (x,y) and provide
 * a triple for subsequent processing (a,x,y)
 * The average cost is  avgJcost
 * avgJcost= Sum( - y log(a) - (1-y log(a))/m  where m = number of training examples
 * where [ -y log(a) - (1-y) log(1-a)]  is the  BinaryCrossEntropy function
 * minimizing avgJcost equates to finding the best w, b parameters.
 * Gradient descent is the algorithm to do this
 * The client function is LogRegGD(ts,alpha,maxIter,step)
 * ts : training set of pairs(x,y)=  ( age, cardiacDefect)
 * alpha: learning rate
 * maxIter: is client specified number of iterations ( epochs)
 * step: is client specified number of iterations between printouts
 * ** knowing the structure of "ts", I know I need a
 * single "w" and a single "b", so I can
 * initialize and update those in my "helper" function( client
 * doesn't need to supply these).
 * The helper(w,b,iter) function goes thru the whole training set
 * to update w and b using gradient descent ( and bumps the
 * iter value by "1"). Then it goes thru the
 * whole training set again to update w and b again.
 * helper goes through the training set "maxIter" number of times.
 * ( e.g.maxIter= 30000  for one of my runs).
 * 2023-09-20rr
 */

import scala.math.*
import scala.annotation.tailrec
object LogRegGDTest extends App{
  type D = Double;type S = String;type I = Int  //aliases
  val baseFnPath = "/Users/rob/Desktop/4IFTAdvBigDataAI/"
  val fnPath = baseFnPath + "HosmerCHD.txt"  //**Local file**
  val data = io.Source.fromFile(fnPath).getLines.toVector
  val dataTotal = data.take(100)    // I may not use all of ts
  val ts:Vector[(Double,Double)] = dataTotal.map { line => {
    val Array(id, x, y) = line.split(" ")
    (x.toDouble, y.toDouble)
  }}
println(s" ts.take(2)  ${ts.take(2)}")// preview of file content

  def LogRegGD(ts: Vector[(D, D)], alpha: D, maxIter: I, step: I) = {
    println(s"LogRegGD ${new java.util.Date()}")
    val m = ts.length
    @tailrec
    def helper(w: D, b: D, iter: I): (D, D, I, D) = {
      val tss: Vector[(D, D, D)] = ts.map { case (x: D, y: D) => {
        val z: D = w * x + b
        val a: D = 1.0 / (1.0 + exp(-z))   // this IS the sigmoid(z) function
        (a: D, y: D, x: D)
      }
      }   // avgJ is like the MSE = SSE/m in linear regression
      val avgJ = tss.map { case (a, y, x) => -y * log(a) - (1 - y) * log(1 - a) }.sum / m
      val avgdw = tss.map { case (a, y, x) => (a - y) * x }.sum / m
      val avgdb = tss.map { case (a, y, x) => (a - y) }.sum / m
      if iter < maxIter then {
        if iter % step == 0 then
          println(f" w = $w, b = $b, iter  = $iter, av2gJ = $avgJ")
        helper(w - alpha * avgdw, b - alpha * avgdb, iter + 1)
      }
      else {
        println(f"w = $w, b = $b, iter = $iter, avgJ = $avgJ")
        (w, b, iter, avgJ)
      }
    } // end helper

    val (w, b, iter, avgJ) = helper(w = 0.0, b = 0.0, iter = 0)
    (w, b, avgJ, alpha, maxIter, step)
  } // end LogRegGD

  LogRegGD(ts, alpha = 0.005, maxIter = 30000, step =3000)
}
/*
/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/bin/java -javaagent:/Users/rob/Library/Application Support/JetBrains/Toolbox/apps/IDEA-U/ch-0/232.9921.47/IntelliJ IDEA.app/Contents/lib/idea_rt.jar=50269:/Users/rob/Library/Application Support/JetBrains/Toolbox/apps/IDEA-U/ch-0/232.9921.47/IntelliJ IDEA.app/Contents/bin -Dfile.encoding=UTF-8 -classpath /Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/charsets.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/deploy.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/ext/cldrdata.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/ext/dnsns.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/ext/jaccess.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/ext/jfxrt.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/ext/localedata.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/ext/nashorn.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/ext/sunec.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/ext/sunjce_provider.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/ext/sunpkcs11.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/ext/zipfs.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/javaws.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/jce.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/jfr.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/jfxswt.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/jsse.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/management-agent.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/plugin.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/resources.jar:/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/rt.jar:/Users/rob/workspace/IFT512ZCode331LogReg09_20/target/scala-3.3.1/classes:/Users/rob/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.10/scala-library-2.13.10.jar:/Users/rob/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.1/scala3-library_3-3.3.1.jar LogRegGDTest
  ts.take(2)  Vector((20.0,0.0), (23.0,0.0))
LogRegGD Fri Sep 22 18:23:47 MST 2023
w = 0.0, b = 0.0, iter  = 0, av2gJ = 0.6931471805599458
w = 0.0643452036984258, b = -1.0988506010803325, iter  = 3500, av2gJ = 1.0051913473460516
w = 0.08306015411129856, b = -2.0301157399463707, iter  = 7000, av2gJ = 0.936058797530909
w = 0.09680162196759493, b = -2.8012005467077525, iter  = 10500, av2gJ = 0.8624778448939836
w = 0.10632543905822808, b = -3.4221946250306123, iter  = 14000, av2gJ = 0.7916821022884883
w = 0.11251911532259488, b = -3.9090583427280268, iter  = 17500, av2gJ = 0.7293650129839371
w = 0.11624536768100346, b = -4.282149464423273, iter  = 21000, av2gJ = 0.6781648202625525
w = 0.11823514048019512, b = -4.562982027289769, iter  = 24500, av2gJ = 0.6381372612668282
w = 0.11905270298883436, b = -4.771559476618411, iter  = 28000, av2gJ = 0.6078946853934473
w = 0.11910737754479214, b = -4.924970466772974, iter  = 31500, av2gJ = 0.5855578457519183
w = 0.11868325609870614, b = -5.037019724288381, iter = 35000, avgJ = 0.5693012215765565

Process finished with exit code 0
*/
