/*
 * Ivan Vendrov, 2013 
 * 
 * No license : Use however you want. 
 */



import scala.collection.mutable.Cloneable
import scala.collection.mutable.ArraySeq

/**
 * Interface for the MatrixMultiply object; used to automatically infer type arguments to "*" and "^".
 */
trait MatrixMultiply[T]{
  def times: Function2[T,T,T] 
  def plus: Function2[T,T,T] 
  val plusid: T
}

/**
 * Stores the standard times, plus, and plusid for particular types;
 * this allows the use of "*" and "^" on matrices without the need t type parameters
 */
object MatrixMultiply{
  
  implicit object mmInt extends MatrixMultiply[Int] {
    def times = _*_
    def plus = _+_
    val plusid = 0
  }
  
  implicit object mmLong extends MatrixMultiply[Long] {
    def times = _*_
    def plus = _+_
    val plusid: Long = 0
  }
  
  implicit object mmBigInt extends MatrixMultiply[BigInt] {
    def times = _*_
    def plus = _+_
    val plusid:BigInt = 0
  }
  
  implicit object mmDouble extends MatrixMultiply[Double] {
    def times = _*_
    def plus = _+_
    val plusid: Double = 0
  }
  
  implicit object mmBoolean extends MatrixMultiply[Boolean] {
    def times = _&&_
    def plus = _||_
    val plusid = false
  }
}



/**
 * A matrix, internally represented with a two-dimensional ArraySeq
 */
class Matrix[T](val array: ArraySeq[ArraySeq[T]],val rows: Int,val columns: Int) 
  extends Traversable[T] with java.lang.Cloneable {
  
  
    
  /****************
   * Constructors
   ****************/
  
  /**
   * Constructor with array of arrays  
   */
  def this(m: ArraySeq[ArraySeq[T]]){    
    this(m, m.length, m(0).length)
  } 
  
  
  /****************
   * Accessors
   ****************/
  
  /**
   * @return dimensions of matrix as a tuple (rows, columns)
   */
  def dim: Tuple2[Int,Int] = (rows,columns)
  
  /**
   * @return true if the matrix is square
   */
  def square: Boolean = (rows == columns)  
  
  /**
   * Converts the matrix to a string, with adjacent entries in a row separated by spaces,
   * and rows separated by newlines
   */
  override def toString() = {
    array.map(_.map(_.toString).mkString(" ")).mkString("\n")
  }
  
  /**
   * Same as toString(), but each entry is padded with whitespace to be of length fieldSize;
   * this improves the appearance for e.g. numerical matrices
   */
  def toString(fieldSize: Int) = {
    val format = "%" + fieldSize + "s"   
    array.map(_.map(_.toString).map(s => format.format(s)).mkString(" ")).mkString("\n")
  }
  
  /****************
   * Entrywise Operations
   ****************/
  /*
   * methods that manipulate a single entry - allow indexing like
   * matrix(1)(2) or matrix(1,2) = 5 or even matrix((1,2)) = 3 where (1,2) is a tuple.
   */  
  def apply(p: (Int, Int)) = array(p._1)(p._2)
  def apply(r: Int, c: Int) = array(r)(c)
  def apply(r: Int) = array(r)
  def update(p: (Int, Int), v: T) {
    array(p._1)(p._2) = v
  }
  def update(r: Int, c:Int, v: T) {
    array(r)(c) = v
  }
  
  
  
  /**
   * @return the rth row of the matrix as an ArraySeq
   */
  def row(r:Int) = array(r)
  
  /**
   * @return the cth column of the matrix as an ArraySeq
   */
  def column(c:Int) = {
    ArraySeq.range[Int](0,rows).map( i => array(i)(c))
  }
  
  /**
   * @return the submatrix with top left and bottom right corners as indicated.
   */
  def submatrix(topleft: Tuple2[Int,Int], bottomright: Tuple2[Int,Int]) = {
    val dx = bottomright._1 - topleft._1
    val dy = bottomright._2 - topleft._2
    val result = Matrix[T](dx+1,dy+1)
    for (i <- 0 to dx; j <- 0 to dy){
      result(i,j) = this(i + topleft._1, j + topleft._2)
    }
    result
  }  
  
  
  /****************
   * Operations with Indices
   ****************/
  
  /**
   * @return the first index (as a pair of integers) that satisfies the given predicate
   * The matrix is searched in row-first order.
   */
  def findIndex(p : T => Boolean): Option[(Int, Int)] = {
    for (i <- 0 until rows; j <- 0 until columns){      
      if (p(this(i,j))) return Some ((i,j))
    }
    None
  }
  
  /**
   * @return true if the given point is a valid index of the matrix
   */
  def valid (p : (Int, Int)): Boolean = {
    val (x,y) = p
    (x >= 0) && (x < rows) && (y >= 0) && (y < columns)
  }
  /**
   * @returns the neighbors of a point in a 4-connected grid 
   */
  def neighbours4(p: (Int, Int))= {
    val (x,y) = p
    Iterator((x+1,y),(x-1,y),(x,y+1),(x,y-1)).filter(valid)
  }
  
   /**
   * @return the neighbors of a point in a 8-connected grid  
   */
  def neighbours8(p: (Int, Int))= {
    val (x,y) = p
    val pts =  for (i <- x-1 to x+1; j <- y-1 to y+1) yield (i,j)
    pts.filter(pt => valid(pt) && pt != p).iterator    
  }
  
  /**
   * @return all the indices of the matrix in row-first order
   */
  def allIndices = {
    for (i <- 0 until rows; j <- 0 until columns) yield (i,j)
  }
  
  
  
  
  /****************
   * Optimizations to the Traversable operations
   ****************/
  def foreach[U](f: T => U): Unit  = {
    for (i <- 0 until rows; j <- 0 until columns){
      f(this(i,j))
    }
  }
  def iterator: Iterator[T] = array.map(_.iterator).iterator.flatten
  
  def map[A](f: Function[T,A]) = {
    val m2 = array.map(_.map(f))  
    new Matrix(m2, rows, columns)
  }
  
  def flatten = array.flatten
  
  def transpose = new Matrix(array.transpose, columns, rows)
  
  
  
  /****************
   * Operations involving other matrices
   ****************/
  
  /**
   * applies a binary operation entrywise on two matrices of equal dimension, generating
   * a third matrix 
   * 
   * e.g. entrywise(_+_) is matrix addition 
   */
  def entrywise[U,V](f: Function2[T,U,V])(other: Matrix[U]): Matrix[V] = {
    if (this.dim != other.dim) throw new Error("Invalid dimensions for Matrix.entrywise()")
    val result = Matrix[V](rows,columns)
    for (i <- 0 until rows; j <- 0 until columns){
      result(i,j) = f(this(i,j), other(i,j))      
    }
    result
  }
  
    
  /**
   * Performs matrix multiplication using textbook algorithm and the given times, plus, and plusId function
   * Very general; usually the numerical / boolean specializations given below are sufficient.
   */
  def multGen[U,V, W](times: Function2[T,U,V], plus: Function2[W,V,W], plusId: W)
  	(other: Matrix[U]) = {   
    val (rows, columns) = dim
    val (orows, ocolumns) = other.dim
    if (columns != orows) throw new Error("Invalid dimensions for Matrix.mult()")
    
    val result = Matrix[W](rows,ocolumns)
    
    for (i <- 0 until rows; j <- 0 until ocolumns){
      var accum = plusId
      for (k <- 0 until columns){
        val p = times(this(i,k), other(k,j))
        accum = plus(accum, p)
      }
      result(i)(j) = accum      
    }
    result 
  }
  
    
      
  /**
   * multiplication where the times, plus, and plusID are inferred automatically based on the
   * matrix's type (see the MatrixMultiply object)
   */
  def mult (other: Matrix[T]) (implicit value: MatrixMultiply[T]) = 
  		     multGen[T,T,T] (value.times, value.plus, value.plusid) (other)
  
  /**
   * synonym for mult
   */
  def * (other: Matrix[T]) (implicit value: MatrixMultiply[T]) = mult(other)
  
  
  /**
   * Uses the standard exponentiation algorithm to take a matrix to a given power,
   * implicitly inferring the multiplication desired based on the matrix type
   * Number of multiplication is logarithmic in exp.
   */
  def pow(exp: Long)(implicit value: MatrixMultiply[T]): Matrix[T] = {
    if (exp == 1) this
    else if (exp % 2 == 1) this.mult(pow(exp-1))
    else {
      val half = pow(exp/2)
      half * half
    }
  }
  
  /**
   * synonym for pow
   */
  def ^ (exp: Long) (implicit value: MatrixMultiply[T]) = pow(exp)
  
  // plumbing that allows Scala to infer the types for matrix multiplication
  private def times[A](implicit value: MatrixMultiply[A]) = value.times
  private def plus[A](implicit value: MatrixMultiply[A]) = value.plus
  private def plusid[A](implicit value: MatrixMultiply[A]) = value.plusid
  
  
}

/**
 * Provides various ways to construct matrices
 */
object Matrix {
  
  /**
   * Constructor with rows, columns and default value :
   * Creates a new matrix of size rows x columns and fills each entry with (default)
   */
  def apply[T](rows:Int, columns:Int, default: => T) : Matrix[T] =   
    new Matrix(ArraySeq.fill(rows,columns)(default), rows, columns)    
  
  
  /**
   * Constructor with rows and columns 
   * Creates a new matrix of size rows x columns and fills each entry with null
   */
  def apply[T](rows:Int, columns:Int) : Matrix[T] = 
    apply(rows, columns, null.asInstanceOf[T])    
  
  
  /**
   * Constructs a matrix given dimensions and a flat array
   */  
  def unflatten[T](columns: Int, array: ArraySeq[T]) = {    
    val rows = array.length/columns
    val it = array.grouped(columns)
    new Matrix(ArraySeq.fill(rows)(it.next), rows, columns)    
  }
  
  // some sample code to show how the Matrix class is used
  def main(args : Array[String]){
    // create a 3x3 matrix of 1's
    val m1 = Matrix(3,3,1)
    
    // create a 3x3 matrix of 2's
    val m2 = Matrix(3,3,2)    
    
    // set an entry of the matrix
    m1(1,1) = 3;
    
    // compute products and powers
    val product = m1*m2
    val power = m1 ^ 10
    
    // default print
    println(product)
    // pretty-print by padding each entry with whitespace
    println(product.toString(3)) 
    
    println(Matrix.unflatten(2, ArraySeq(1,1,0,1)))
  }
 
}
