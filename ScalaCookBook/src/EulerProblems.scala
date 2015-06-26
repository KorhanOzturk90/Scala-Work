/**
 * Created by Korhan on 29/05/2015.
 */

import scala.collection.mutable.HashSet

/*
 * @author  Korhan Ozturk
 * Each method in this class is meant to answer a corresponding problem in http://projecteuler.net.
 */
object EulerProblems {

  def sumOfAllFiveOrThree(limit: Int): Int = {
    val numbers = (1 until limit) toList

    numbers.filter(x => x % 3 == 0 || x % 5 == 0) sum
  }

  def main(args: Array[String]) {
    val sum = sumOfAllFiveOrThree(1000);
    println(s"sum: $sum")

    fibEven(700)

    println(fibList filter (a => a < 4000000 && a % 2 == 0) sum)

    println("max prime factor: " + findLargestPrimeFactor(BigInt(600851475143L)))

    println("GCD 252 and 105: " + GCD(252, 105))

    var numbers = List.range(1, 21)
    println("LCDM 1-20: " + LCM(numbers))

    numbers = List.range(1, 101)
    println("Sum square difference of 1-100: " + sumSquareDiff(numbers))

    println("largest palindrome : " + findLargestPalindrome())

    println("10001st prime number is " + findNthPrimeNumber(10001))

    println("Largest multiple of 13 adjacent number is " + findLargestProductInSeries(13))
  }

  var fibList = List[BigInt]()

  def fibEven(x: Int, prev: BigInt = 0, next: BigInt = 1): BigInt = x match {
    case 0 => prev
    case 1 => next
    case _ =>
      fibList ::= next
      fibEven(x - 1, next, (next + prev))
  }


  //Finds the largest prime factor of a given number
  def findLargestPrimeFactor(input: BigInt): Int = {
    var copyInp = input
    var primeNumbers = HashSet[Int]()
    var x = 2
    while (copyInp > 1) {
      while (copyInp % x == 0) {
        primeNumbers += x
        copyInp /= x
      }
      x += 1
      if (x * x > copyInp) {
        if (copyInp > 1) {
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
    case _ => GCD(b, a % b)

  }

  //Finds the least common multiple of two numbers
  def findLCM(a: Int, b: Int): Int = {
    a * b / GCD(a, b)
  }

  //Finds the least common multiple that is evenly divisible by all of the numbers in the list
  def LCM(numbers: List[Int]): Int = {
    var result = numbers head

    for (i <- 1 until numbers.length - 1) {
      result = findLCM(result, numbers(i))
    }
    result
  }

  //Finds the difference between the sum of the squares of the first n natural numbers and the square of the sum
  def sumSquareDiff(input: List[Int]): Int = {
    var tempSum = (input sum) - (input head)
    var totalSum = 0

    for (i <- 0 until input.length - 1) {
      totalSum += input(i) * tempSum * 2
      tempSum -= input(i + 1)
    }
    totalSum
  }

  //Finds whether given number is a palindrome.
  def isPalindrome(number: Int): Boolean = {
    var p = number
    var reverse = 0
    while (p > 0) {
      reverse *= 10
      reverse += p % 10
      p /= 10

    }
    reverse == number
  }

  //Finds the largest palindrome made from the product of two 3-digit numbers.
  def findLargestPalindrome(): Int = {
    var result = 0
    for (i <- 999 to 100 by -1; j <- 100 to i) {
      val mult = i * j
      if (isPalindrome(mult) && mult > result) {
        result = mult
      }
    }
    result
  }

  //Finds whether the given number is a prime number
  def isPrime(input: Int): Boolean = {
    var prime = true
    if (input == 2) {
      prime = true
    }
    else
      for (x <- 2 to Math.sqrt(input).toInt) {
        if (input % x == 0) {
          prime = false
        }
      }
    prime
  }

  //Finds n'th prime number
  def findNthPrimeNumber(n: Int): Int = {
    var count = 0
    for (i <- 2 to 400000) {
      if (isPrime(i)) {
        count += 1
      }
      if (count == n) {
        return i
      }
    }
    0
  }

  //Finds the value of n adjacent digits in 1000-digit number.
  def findLargestProductInSeries(n: Int): BigInt = {
    val input = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    var result = BigInt(0)
    for (i <- 0 to input.length - n - 1) {
      var temp = BigInt(1)
      for (y <- i to i + n - 1){
        temp *= input.charAt(y).asDigit
      }
      if (temp > result) {
        result = temp
      }
    }
    result

  }


}
