/**
 * Created by Korhan on 29/05/2015.
 */
import scala.collection.mutable.HashSet
object EulerProblems {

  def sumOfAllFiveOrThree(limit : Int) : Int = {
    val numbers = (1 until limit) toList

    numbers.filter(x => x%3==0 || x%5==0 ) sum
  }

  def main (args: Array[String]) {
    println("sum: " + sumOfAllFiveOrThree(1000))

    fibEven(700)

    println(fibList filter(a=> a<4000000 && a%2==0) sum)

    println("max prime factor: " + findLargestPrimeFactor2(BigInt(600851475143L)))
  }

  var fibList = List[BigInt]()
  def fibEven(x:Int, prev: BigInt = 0, next: BigInt = 1):BigInt = x match {
    case 0 => prev
    case 1 => next
    case _ =>
        fibList ::= next
        fibEven(x-1, next, (next + prev))
  }

  def findLargestPrimeFactor(input: BigInt) : Int ={
    var copyInp = input
    var primeNumbers = HashSet[Int]()
    var x = 2
    while(x <= input/2){
      if(copyInp%x==0){
        primeNumbers += x
        copyInp/=x
        x-=1
      }
      x+=1
    }
    primeNumbers max
  }

  def findLargestPrimeFactor2(input: BigInt) : Int ={
    var copyInp = input
    var primeNumbers = HashSet[Int]()
    var x = 2
    while(copyInp > 1){
      while(copyInp%x==0){
        primeNumbers += x
        copyInp/=x
      }
      x+=1
      if (x*x > copyInp){
        if(copyInp > 1) {
          primeNumbers += copyInp intValue()
          copyInp = 1
        }
      }
    }
    primeNumbers max
  }

}
