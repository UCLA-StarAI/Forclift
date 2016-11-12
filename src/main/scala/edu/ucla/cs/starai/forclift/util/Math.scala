/*
 * Copyright 2016 Guy Van den Broeck and Wannes Meert (UCLA and KU Leuven)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.ucla.cs.starai.forclift.util

import scala.language.implicitConversions

/**
 * This is a value class whose runtime type is Double -- no boxing/unboxing overhead
 */
final class LogDouble(val v: Double) extends AnyVal with Ordered[LogDouble]{
  
  @inline def isZero = v.isNegInfinity
  @inline def isOne = (v == 0)
  @inline def isNaN = v.isNaN
  @inline def isInfinite = v.isPosInfinity
  @inline def isNegInfinite = v.isNegInfinity
  
  @inline def compare(that: LogDouble) =  if (this.v > that.v) 1 else if (this.v < that.v) -1 else 0
  // override Ordered for performance
  @inline override def < (that:LogDouble) = (this.v < that.v)
  @inline override def > (that:LogDouble) = (this.v > that.v)
  @inline override def <= (that:LogDouble) = (this.v <= that.v)
  @inline override def >= (that:LogDouble) = (this.v >= that.v)
  
  // no equals or hashcode definitions allowed (defaults to proxy for v)
  // (see https://docs.google.com/document/d/10TQKgMiJTbVtkdRG53wsLYwWM2MkhtmdV25-NZvLLMA/edit?hl=en_US)
  
  /**
   * http://lingpipe-blog.com/2009/06/25/log-sum-of-exponentials/
   * https://gist.github.com/scalala/Scalala/blob/master/src/main/scala/scalala/library/Numerics.scala
   */
  @inline def + (that: LogDouble): LogDouble = {
    if (this.v < that.v) {
      if (this.isZero) that
      else new LogDouble(that.v + math.log1p(math.exp(this.v - that.v)))
    }else{
      if (that.isZero) this
      else new LogDouble(this.v + math.log1p(math.exp(that.v - this.v)))
    }
  }

  /**
   * https://gist.github.com/scalala/Scalala/blob/master/src/main/scala/scalala/library/Numerics.scala
   */
  @inline def - (that: LogDouble): LogDouble = {
    require(this >= that, s"Cannot subtract $that from $this" )
    if (that.isZero) this
    else if (this == that) LogDouble.zero
    else new LogDouble(this.v + math.log(1.0 - math.exp(that.v - this.v)))
  }
  
  @inline def * (that: LogDouble): LogDouble = new LogDouble(this.v + that.v)
  @inline def / (that: LogDouble): LogDouble = new LogDouble(this.v - that.v)
  
  @inline def pow(exp: Int): LogDouble = new LogDouble(this.v * exp)
  @inline def pow(exp: Long): LogDouble = new LogDouble(this.v * exp)
  @inline def pow(exp: Double): LogDouble = new LogDouble(this.v * exp)
  @inline def root(exp: Int): LogDouble = {
    require(exp >= 0)
    new LogDouble(this.v / exp)
  }
  @inline def root(exp: Long): LogDouble = {
    require(exp >= 0)
    new LogDouble(this.v / exp)
  }
  @inline def root(exp: Double): LogDouble = {
    require(exp >= 0)
    new LogDouble(this.v / exp)
  }

  @inline def log: LogDouble = new LogDouble(math.log(this.v))
  @inline def logToDouble: Double = this.v
  @inline def exp: LogDouble = new LogDouble(math.exp(this.v))
  @inline def inv: LogDouble = new LogDouble(-this.v)

  @inline override def toString = s"exp($v)"
  @inline def toFloat = math.exp(v).toFloat
  @inline def toDouble = math.exp(v)
  @inline def toSignDouble = new SignLogDouble(true,this)
  
}

object LogDouble{
  
   implicit def doubleToLogDouble(d: Double): LogDouble = {
     require(d >= 0) // math.log does not throw an exception
     new LogDouble(math.log(d))
   }
   
   def fromLog(d: Double) = new LogDouble(d)
   
	// not safe! has to be manually
	//   implicit def signLogDoubleToLogDouble(d: SignLogDouble): LogDouble = {
	//     d.toLogDouble
   	//   }
  
   val zero: LogDouble = 0
   val one: LogDouble = 1
   val NaN: LogDouble = new LogDouble(Double.NaN)
   
}


/**
 * Extend LogDouble towards negative numbers
 * Positive numbers have pos == true, negative have pos == false
 */
final class SignLogDouble(_pos: Boolean, val ld: LogDouble) extends Ordered[SignLogDouble]{

  // be careful adding @inline in this class: can cause compiler to crash
  
  // set -0 to 0
  val pos = (_pos || ld.isZero || ld.isNaN)
  def neg = !pos  
  
  def isZero = ld.isZero
  def isOne = (pos && ld.isOne)
  def isNaN = ld.isNaN
  def isInfinite = ld.isInfinite
  
  
  def compare(that: SignLogDouble) =  {
    if(this.pos && that.pos){
      if (this.ld > that.ld) 1 else if (this.ld < that.ld) -1 else 0
    }else if(this.pos && that.neg){
      1
    }else if(this.neg && that.pos){
      -1
    }else /*(this.neg && that.neg)*/ {
       if (this.ld > that.ld) -1 else if (this.ld < that.ld) 1 else 0
    }
  }
  
  override def equals(other: Any) = other match {
 	   case that: SignLogDouble => (this.pos == that.pos) && (this.ld == that.ld)
 	   case _ => false
  }
  
  override def hashCode = pos.hashCode * 47 + ld.hashCode

  def unary_- = new SignLogDouble(neg,ld)
  def abs = new SignLogDouble(true,ld)
  def toLogDouble = {
    require(pos)
    ld
  }

  def + (that: SignLogDouble): SignLogDouble = {
    if (this.isZero) that
    else if (that.isZero) this
    else if(this.pos && that.pos){
      new SignLogDouble(true, this.ld + that.ld)
    }else if(this.pos && that.neg){
      if(this.ld > that.ld) new SignLogDouble(true, this.ld - that.ld)
      else /*(this.ld < that.ld)*/ new SignLogDouble(false, that.ld - this.ld)
    }else if(this.neg && that.pos){
      if(this.ld > that.ld) new SignLogDouble(false, this.ld - that.ld)
      else /*(this.ld < that.ld)*/ new SignLogDouble(true, that.ld - this.ld)
    }else /*(this.neg && that.neg)*/ {
       new SignLogDouble(false, this.ld + that.ld)
    }
  }
  
  def - (that: SignLogDouble): SignLogDouble = {
    this + (-that)
  }
  
  def * (that: SignLogDouble): SignLogDouble = {
    new SignLogDouble((this.pos == that.pos),(this.ld * that.ld))
  }
  def / (that: SignLogDouble): SignLogDouble = {
    new SignLogDouble((this.pos == that.pos),(this.ld / that.ld))
  }
  
  def pow(exp: Int): SignLogDouble = {
    val newSign = (pos || (exp%2 == 0)) // positive number or even power
    new SignLogDouble(newSign,ld.pow(exp))
  }
  
  def pow(exp: Long): SignLogDouble = {
    require(exp >= 0)
    val newSign = (pos || (exp%2 == 0)) // positive number or even power
    new SignLogDouble(newSign,ld.pow(exp))
  }
  
  def root(exp: Long): SignLogDouble = {
    require(exp >= 0)
    require(this.pos)
    new SignLogDouble(true,ld.root(exp))
  }
  
  def root(exp: Double): SignLogDouble = {
    require(exp >= 0)
    require(this.pos)
    new SignLogDouble(true,ld.root(exp))
  }

  def log: SignLogDouble = {
    require(pos)
    if (ld.v < 0.0) { // TODO: can be implemented more principled
      new SignLogDouble(false, new LogDouble(math.log(-ld.v)))
    } else {
      new SignLogDouble(pos,ld.log)
    }
  }
  
  def logToDouble: Double = {
    require(pos, s"Expected a positive number, got $ld")
    ld.logToDouble
  }
  
  def exp: SignLogDouble = {
    if(pos) new SignLogDouble(true,ld.exp) 
    else (new SignLogDouble(true,ld.exp.inv))
  }

  override def toString = (if(pos) "" else "-") + ld
  def toDouble = if(pos) ld.toDouble else -ld.toDouble
  def toFloat = if(pos) ld.toFloat else -ld.toFloat
  
}

object SignLogDouble{
  
  import LogDouble._
  
   implicit  def doubleToSignLogDouble(d: Double): SignLogDouble = {
     if(d<0) new SignLogDouble(false, -d)
     else new SignLogDouble(true, d)
   }
  
   implicit def logDoubleToSignLogDouble(ld: LogDouble): SignLogDouble = {
     new SignLogDouble(true, ld)
   }
  
   def fromLog(d: Double) = new SignLogDouble(true, LogDouble.fromLog(d))
   
   val zero: SignLogDouble = 0
   val one: SignLogDouble = 1
   val NaN: SignLogDouble = new SignLogDouble(true, LogDouble.NaN)
}


object KLD {

  import SignLogDouble._
  
  def asymmetricKld(p: SignLogDouble, q: SignLogDouble): SignLogDouble = {
    if (p.isZero) {
      zero
    } else if (q.isZero) {
    	// smooth
        asymmetricKld(p, 0.000001)
    } else {
    	p * (p / q).log
    }
  }

  def symmetricKld(p: SignLogDouble, q: SignLogDouble): SignLogDouble = {
    val r1 = asymmetricKld(p, q)
    val r2 = asymmetricKld(q, p)
    (r1 + r2)
  }
  
  def symmetricKld(p: SignLogDouble, q: SignLogDouble, r: SignLogDouble): SignLogDouble = {
    val r1 = symmetricKld(p, q)
    val r2 = symmetricKld(q, r)
    val r3 = symmetricKld(p, r)
    (r1 + r2 + r3)
  }

}

object Binomial {

  private[this] val factorialCache = new collection.mutable.ArrayBuffer[LogDouble] ++ List(LogDouble.one, LogDouble.one)

  def factorial(n: Int): LogDouble = {
    if (n < factorialCache.length) factorialCache(n)
    else {
      for (i <- factorialCache.length to n) {
        factorialCache += (factorialCache(i - 1) * i)
      }
      factorialCache.last
    }
  }

  def coeff(n: Int, k: Int): LogDouble = factorial(n) / factorial(k) / factorial(n - k)

}
