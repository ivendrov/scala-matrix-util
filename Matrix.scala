/*
 * written by Ivan Vendrov, 2013 
 *
 * No license - use however you want.
 */

package matrix

import scala.collection.mutable.Cloneable
import scala.collection.mutable.ArraySeq




class Matrix[T](val array: ArraySeq[ArraySeq[T]],val rows: Int,val columns: Int) 
  extends Traversable[T] with java.lang.Cloneable {
  
  
    
  /****************
   * CONSTRUCTORS
   ****************/
  
  /**
   * Constructor with array of arrays
   */
  def this(m: ArraySeq[ArraySeq[T]]){    
    this(m, m.length, m(0).length)
  } 
  
  
  /****************
   * MATRIX INFORMATION
   ****************/
  
  /**
   * returns dimensions of matrix
   */
  def dim: Tuple2[Int,Int] = (rows,columns)
  /**
   * returns true if the matrix is square
   */
  def square: Boolean = (rows == columns)  
  
  override def toString() = {
    array.map(_.map(_.toString).mkString(" ")).mkString("\n")
  }
  def toString(fieldSize: Int) = {
    val format = "%" + fieldSize + "s"   
    array.map(_.map(_.toString).map(s => format.format(s)).mkString(" ")).mkString("\n")
  }
  
  /****************
   * ENTRYWISE manipulation
   ****************/
  // single-entry functions
  // allow indexing like matrix(1)(2) or matrix(1,2) = 5
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
   * Parts of matrix
   */
  def row(r:Int) = array(r)
  def column(c:Int) = {
    ArraySeq.range[Int](0,rows).map( i => array(i)(c))
  }
  // add sanitation
  def submatrix(topleft: Tuple2[Int,Int], bottomright: Tuple2[Int,Int]) = {
    val dx = bottomright._1 - topleft._1
    val dy = bottomright._2 - topleft._2
    val result = Matrix[T](dx+1,dy+1)
    for (i <- 0 to dx; j <- 0 to dy){
      result(i,j) = this(i + topleft._1, j + topleft._2)
    }
    result
  }
  
  
  // all - entry functions
  def map[A](f: Function[T,A]) = {
    val m2 = array.map(_.map(f))  
    new Matrix(m2, rows, columns)
  }
  
  //// Operations with indices 
  /**
   * return first index (as a pair of integers) that satisfies the given predicate
   */
  def findIndex(p : T => Boolean): Option[(Int, Int)] = {
    for (i <- 0 until rows; j <- 0 until columns){      
      if (p(this(i,j))) return Some ((i,j))
    }
    None
  }
  
  /**
   * True if the given point is a valid index of the matrix
   */
  def valid (p : (Int, Int)): Boolean = {
    val (x,y) = p
    (x >= 0) && (x < rows) && (y >= 0) && (y < columns)
  }
  /**
   * returns the neighbors of a point in a 4-connected grid
   * TODO: Optimize
   */
  def neighbours4(p: (Int, Int))= {
    val (x,y) = p
    Iterator((x+1,y),(x-1,y),(x,y+1),(x,y-1)).filter(valid)
  }
  
   /**
   * returns the neighbors of a point in a 8-connected grid
   * TODO: Optimize
   */
  def neighbours8(p: (Int, Int))= {
    val (x,y) = p
    val pts =  for (i <- x-1 to x+1; j <- y-1 to y+1) yield (i,j)
    pts.filter(pt => valid(pt) && pt != p).iterator    
  }
  
  /**
   * return all the indices of the matrix in row-first order
   */
  def allIndices = {
    for (i <- 0 until rows; j <- 0 until columns) yield (i,j)
  }
  
  
  
  
  /**
   * Collection Manipulation
   */
  def foreach[U](f: T => U): Unit  = {
    for (i <- 0 until rows; j <- 0 until columns){
      f(this(i,j))
    }
  }
  def iterator: Iterator[T] = array.map(_.iterator).iterator.flatten
  
  def flatten = array.flatten
  
  def transpose = new Matrix(array.transpose, columns, rows)
  
  
  /**
   * Operations on/with other matrices
   */
  
  // apply a binary operation entrywise on two matrices of equal dimension
  def entrywise[U,V](f: Function2[T,U,V])(other: Matrix[U]): Matrix[V] = {
    if (this.dim != other.dim) throw new Error("Invalid dimensions for Matrix.entrywise()")
    val result = Matrix[V](rows,columns)
    for (i <- 0 until rows; j <- 0 until columns){
      result(i,j) = f(this(i,j), other(i,j))      
    }
    result
  }
  
  
  
  
  
  
  def times[A](implicit value: MatrixMultiply[A]) = value.times
  def plus[A](implicit value: MatrixMultiply[A]) = value.plus
  def plusid[A](implicit value: MatrixMultiply[A]) = value.plusid
  
  
  
  // given an inner product function, let the (i,j)th entry of the result be the inner product
  // of the ith row and jth column
  def multGen[U,V](ip: Function2[ArraySeq[T],ArraySeq[U],V])(other: Matrix[U]) = {
    val (orows, ocolumns) = other.dim
    if (columns != orows) throw new Error("Invalid dimensions for Matrix.mult()")
    
    val result = Matrix[V](rows,ocolumns)
    for (i <- 0 until rows; j <- 0 until ocolumns){
      result(i,j) = ip(this.row(i), other.column(j))
    }
  }  

  // simplification of multGen - applies the times and plus operations just like the 
  // textbook algorithm
  def multImp[U,V, W](times: Function2[T,U,V], plus: Function2[W,V,W], plusId: W)(other: Matrix[U]) =    
    Matrix.multGen[T,U,V,W](times, plus, plusId) (this, other)
  
  // less general form of multGen, where all intermediate results have the same type
  def mult (other:Matrix[T]) (implicit value: MatrixMultiply[T]) = Matrix.mult[T](value) (this, other)
  def * (other:Matrix[T]) (implicit value: MatrixMultiply[T]) = Matrix.mult[T](value) (this, other)
  
  
  def powGen(mult: Function2[Matrix[T], Matrix[T], Matrix[T]])(exp: Long): Matrix[T] = {
    if (exp == 1) this
    else if (exp % 2 == 1) mult(this, this.powGen(mult)(exp-1))
    else {
      val half = this.powGen(mult)(exp/2)
      mult(half,half)
    }
  }
  
  def pow (exp: Long)(implicit value: MatrixMultiply[T]) = powGen(Matrix.mult[T](value)) (exp)
  def ^ (exp: Long) (implicit value: MatrixMultiply[T]) = powGen(Matrix.mult[T](value)) (exp)
  
 
  
  
  
   
  
}

object Matrix {
  
  /**
   * Constructor with rows, columns and default value
   */
  def apply[T](rows:Int, columns:Int, default: => T) : Matrix[T] =   
    new Matrix(ArraySeq.fill(rows,columns)(default), rows, columns)    
  
  
  /**
   * Constructor with rows and columns
   */
  def apply[T](rows:Int, columns:Int) : Matrix[T] = 
    apply(rows, columns, null.asInstanceOf[T])    
  
  
  /**
   * Constructs a matrix given dimensions and a flat array
   */
  // needs verification
  def unflatten[T](columns: Int, array: ArraySeq[T]) = {    
    val rows = array.length/columns
    val it = array.grouped(columns)
    new Matrix(ArraySeq.fill(rows)(it.next), rows, columns)    
  }
  
  /**
   *  Constructs a matrix given a "matrix" of characters
   *  e.g.
   *  
   *  ABA
   *  BAA
   *  BAC
   */
  def parseMatrix[T](lines: Iterator[String], parseChar: Char => T)= {
    def seqToArraySeq[T](s : Seq[T]) = ArraySeq(s : _ *)
    
    val entities = lines.map(_.map(parseChar))
    new Matrix(seqToArraySeq(entities.map(seqToArraySeq).toSeq))   
  }
  
  // matrix multiplication, textbook algorithm
  def multGen[T,U,V, W](times: Function2[T,U,V], plus: Function2[W,V,W], plusId: W)
  	(a: Matrix[T], b: Matrix[U]) = {   
    val (rows, columns) = a.dim
    val (orows, ocolumns) = b.dim
    if (columns != orows) throw new Error("Invalid dimensions for Matrix.mult()")
    
    val result = Matrix[W](rows,ocolumns)
    
    for (i <- 0 until rows; j <- 0 until ocolumns){
      var accum = plusId
      for (k <- 0 until columns){
        val p = times(a(i,k), b(k,j))
        accum = plus(accum, p)
      }
      result(i)(j) = accum      
    }
    result 
  }
  
  def mult[A] (implicit value: MatrixMultiply[A]) = 
    multGen[A,A,A,A](value.times, value.plus, value.plusid) _
  
  def main(args : Array[String]){
    
  }
 
}


trait MatrixMultiply[T]{
  def times: Function2[T,T,T] 
  def plus: Function2[T,T,T] 
  val plusid: T
}

object MatrixMultiply{
  //
  // MATRIX MULTIPLICATION
  //
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
    def plus = (x,y) => (x+y)%1000
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



