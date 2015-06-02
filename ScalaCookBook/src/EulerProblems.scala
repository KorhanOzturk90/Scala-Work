/**
 * Created by Korhan on 29/05/2015.
 */
import scala.collection.mutable.HashSet
/*
 * @author  Korhan Ozturk
 * Each method in this class is meant to answer a corresponding problem in http://projecteuler.net.
 */
object EulerProblems {

  def sumOfAllFiveOrThree(limit : Int) : Int = {
    val numbers = (1 until limit) toList

    numbers.filter(x => x%3==0 || x%5==0 ) sum
  }

  def main (args: Array[String]) {
    /*println("sum: " + sumOfAllFiveOrThree(1000))

    fibEven(700)

    println(fibList filter(a=> a<4000000 && a%2==0) sum)

    println("max prime factor: " + findLargestPrimeFactor(BigInt(600851475143L)))

    println("GCD 252 and 105: " + GCD(252, 105))

    var numbers = List.range(1,21)
    println("LCDM 1-20: " + LCM(numbers))

    numbers = List.range(1,101)
    println("Sum square difference of 1-100: " + sumSquareDiff(numbers))*/

    println("largest palindrome : " + findLargestPalindrome())
  }

  var fibList = List[BigInt]()
  def fibEven(x:Int, prev: BigInt = 0, next: BigInt = 1):BigInt = x match {
    case 0 => prev
    case 1 => next
    case _ =>
        fibList ::= next
        fibEven(x-1, next, (next + prev))
  }


  //Finds the largest prime factor of a given number
  def findLargestPrimeFactor(input: BigInt) : Int ={
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

  //Finds the greatest common divisor of two numbers
  def GCD(a: Int, b: Int): Int = b match {
    case 0 => a
    case _ => GCD(b, a%b)

  }

  //Finds the least common multiple of two numbers
  def findLCM(a: Int, b:Int): Int = {
    a * b / GCD(a, b)
  }

  //Finds the least common multiple that is evenly divisible by all of the numbers in the list
  def LCM(numbers: List[Int]) : Int = {
    var result = numbers head

    for (i <- 1 until numbers.length - 1){
      result = findLCM(result, numbers(i))
    }
    result
  }

  //Finds the difference between the sum of the squares of the first n natural numbers and the square of the sum
  def sumSquareDiff(input: List[Int]) : Int = {
    var tempSum = (input sum) - (input head)
    var totalSum = 0

    for(i<-0 until input.length - 1){
      totalSum += input(i) * tempSum * 2
      tempSum -= input(i+1)
    }
    totalSum
  }

  //Finds whether given number is a palindrome.
  def isPalindrome(number:Int) : Boolean = {
    var p = number
    var reverse = 0
    while(p > 0){
      reverse *= 10
      reverse += p%10
      p /= 10

    }
    reverse == number
  }

  //Finds the largest palindrome made from the product of two 3-digit numbers.
  def findLargestPalindrome() : Int ={
    var result = 0
    for(i<-999 to 100 by -1; j <- 100 to i){
      val mult = i * j
      if(isPalindrome(mult) && mult > result ){
        result = mult
      }
    }
    result
  }


}
