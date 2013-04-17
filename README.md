scala-matrix-util
=================

A small matrix class, written in Scala, with lightweight multiplication and exponentiation for matrices of 
all numeric types and booleans, as well as a bunch of utility methods I've found useful in my projects. 
Use if you need to work with matrices but don't need the full power of a linear algebra library. 

==========
Features
==========

The Matrix object has a number of methods for quick matrix creation, e.g

val m1 = Matrix(2,2,1)

 creates a 2x2 integer matrix filled with 1's, and

val m2 = Matrix.unflatten(2, ArraySeq(1,1,0,1))

creates the following matrix: 
1 1
0 1



Individual matrix entries can be accessed and updated in any of the following ways:

m1(1,2) = 3 OR m1(1)(2) = 3 OR m1((1,2)) = 3 where (1,2) is a Tuple[Int,Int]



Matrices can be multiplied and exponentiated using the standard syntax: 

val product = m1 * m2
val power = m1 ^ 10


And have two convenient toString methods for pretty-printing:

println(m1.toString) or just println(m1)
println(m1.toString(3)) pads each entry to 3 characters 

See the provided "main" method for more sample code. 
