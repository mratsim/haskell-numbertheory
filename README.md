# Haskell math for Primes, Factorization, and the first few problems of Project Euler

6 years ago I started learning Haskell
This is a collection of programs I did while solving Project Euler problems and also programming praxis exercises
I often far beyond what was asked to optimize compute time

What you will find :
* For primes :
    * Prime Generation
        * Sieve of Atkin, speed comparable to C ([primegen 0.97](https://cr.yp.to/primegen.html) and [primesieve v4.0](primesieve.org))
        * Sieve of Erathosthene
        * Trial division
    * Factoring Function including
        * Trial division
        * Continued Fraction Canditates method
        * Elliptic Curve Factorization method (non-finished)
    * Random Function
        * Miller-Rabin Primality test
    * Helper Functions
        * Merge Sort
        * Matrix
            * Scalar Multiplication
            * Polynomial Multiplication
            * dot product
            * tensor product
            * Matrix product
            * scalar Multiplication
        * Number Theory
            * x^n in a (semi)group
            * Diophantine ? (see extended Euclide)
            * Integer square root
            * Modulo square root
            * Modulo Exponentiation
            * Legendre symbol
            * Kronecker Symbol
        * Arithmetic Function
            * Euler Totient
            * Moebius
            * Sigma Function
            * Harmonic Function

Unfortunately I probably lost the actual code used in Project Euler and other stuff like Pollard p-1, and pollard rho factorization methods

## List of solved problems
1. Add all the natural numbers below 1000 that are multiples of 3 or 5.
2. Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed one million.
3. Find the largest prime factor of 600851475143.
4. Find the largest palindrome made from the product of two 3-digit numbers.
5. What is the smallest number divisible by each of the numbers 1 to 20?
6. What is the difference between the sum of the squares and the square of the sums?
7. Find the 10001st prime.
8. Discover the largest product of thirteen consecutive digits in the 1000-digit number.
9. There is only one Pythagorean triplet, {a, b, c}, for which a + b + c = 1000. Find the product abc.
10. Calculate the sum of all the primes below one million.

** **
12. What is the first triangle number to have over five-hundred divisors?
13. Find the first ten digits of the sum of one-hundred 50-digit numbers.
14. Find the longest sequence using a starting number under one million.
15. Starting in the top left corner in a 20 by 20 grid, how many routes are there to the bottom right corner?
16. What is the sum of the digits of the number 21000?

** **
19. You are given the following information, but you may prefer to do some research for yourself.

    1 Jan 1900 was a Monday.
    Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine. And on leap years, twenty-nine.

    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
    How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

** **
20. Find the sum of digits in 100!

** **
25. The Fibonacci sequence is defined by the recurrence relation:

    Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
    Hence the first 12 terms will be:

    F1 = 1
    F2 = 1
    F3 = 2
    F4 = 3
    F5 = 5
    F6 = 8
    F7 = 13
    F8 = 21
    F9 = 34
    F10 = 55
    F11 = 89
    F12 = 144
    The 12th term, F12, is the first term to contain three digits.

    What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

** **
27. Euler discovered the remarkable quadratic formula:

    n^2+n+41

    It turns out that the formula will produce 40 primes for the consecutive integer values 0≤n≤390≤n≤39. However, when n=40,402+40+41=40(40+1)+41n=40,402+40+41=40(40+1)+41 is divisible by 41, and certainly when n=41,412+41+41n=41,412+41+41 is clearly divisible by 41.

    The incredible formula n^2−79n+1601 was discovered, which produces 80 primes for the consecutive values 0≤n≤790≤n≤79. The product of the coefficients, −79 and 1601, is −126479.

    Considering quadratics of the form:

    n^2+an+b, where |a|<1000|a|<1000 and |b|≤1000|b|≤1000

    where |n||n| is the modulus/absolute value of nn
    e.g. |11|=11|11|=11 and |−4|=4|−4|=4
    Find the product of the coefficients, aa and bb, for the quadratic expression that produces the maximum number of primes for consecutive values of nn, starting with n=0n=0.


** **
29. Consider all integer combinations of ab for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:

    2^2=4, 2^3=8, 2^4=16, 2^5=32
    3^2=9, 3^3=27, 3^4=81, 3^5=243
    4^2=16, 4^3=64, 4^4=256, 4^5=1024
    5^2=25, 5^3=125, 5^4=625, 5^5=3125
    If they are then placed in numerical order, with any repeats removed, we get the following sequence of 15 distinct terms:

    4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125

    How many distinct terms are in the sequence generated by ab for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?


** **
48. The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

    Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
