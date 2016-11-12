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

/**
 *  Factory object for producing generalized memoized functions of up to four
 *  arguments.
 *
 *  Usage:
 *  <code><pre>
 *  val circArea =
 *      Memoize((r:Double)=>Math.Pi * Math.pow(r,2))
 *
 *  val area = circArea(2)
 *
 *  val cylVol = Memoize {
 *      (r:Double, h:Double) =>
 *        val vol = Math.Pi * Math.pow(r,2) * h
 *        println("Radius: " + r + " Height: " +
 *            h + " Volume: " + vol)
 *        vol
 *  }
 *
 *  val vol = cylVol(2, 3.5)
 *  val twoRCyl = cylVol.curry(2) //We can partially apply as normal
 *  val vol2 = twoRCyl(3.5)       //...and the memoization cache is shared
 *
 *  def sphereVol(r: Double) = Math.Pi * Math.pow(r,3)
 *
 *  val memoSV = Memoize(sphereVol _)
 *  </pre></code>
 *
 *  @see http://www.itl.nist.gov/div897/sqg/dads/HTML/memoize.html
 *  @see http://www.uncarved.com/blog/memoization.mrk
 *
 */
object Memoize {

  ////  println("====================")
  ////  println("MEMOIZATION DISABLED - replace later by cache hashmap")
  ////  println("====================")
  //  def apply[T, R](f: T => R) = f
  //  def apply[T1, T2, R](f: (T1, T2) => R) = f
  //  def apply[T1, T2, T3, R](f: (T1, T2, T3) => R) = f
  //  def apply[T1, T2, T3, T4, R](f: (T1, T2, T3, T4) => R) = f
  //  def apply[T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => R) = f

  //  println("====================")
  //  println("FULL MEMOIZATION")
  //  println("====================")
  //  def apply[T, R](f: T => R) = new MemoizedFunction1(f)
  //  def apply[T1, T2, T3, R](f: (T1, T2, T3) => R) = new MemoizedFunction3(f)
  //  def apply[T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => R) = new MemoizedFunction5(f)

  //  println("====================")
  //  println("SOFT MEMOIZATION")
  //  println("====================")
  //  def apply[T, R](f: T => R) = new SoftMemoizedFunction1(f)
  //  def apply[T1, T2, T3, R](f: (T1, T2, T3) => R) = new SoftMemoizedFunction3(f)
  //  def apply[T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => R) = new SoftMemoizedFunction5(f)

  /**
   * A memoized single-argument function
   */
  class MemoizedFunction1[-T1, +R](f: T1 => R) extends (T1 => R) {

    private[this] val cache = new java.util.HashMap[T1, R]

    def apply(x: T1): R = {
      val key = x
      val result = cache.get(key)
      if (result != null) result
      else {
        val res = f(x)
        cache.put(key, res)
        res
      }
    }

  }

  /**
   * A memoized 3-argument function
   */
  class MemoizedFunction3[-T1, -T2, -T3, +R](f: ((T1, T2, T3) => R))
    extends ((T1, T2, T3) => R) {

    private[this] val cache = new java.util.HashMap[(T1, T2, T3), R]

    def apply(x: T1, y: T2, z: T3): R = {
      val key = (x, y, z)
      val result = cache.get(key)
      if (result != null) result
      else {
        val res = f(x, y, z)
        cache.put(key, res)
        res
      }
    }
  }

  /**
   * A memoized 5-argument function
   */
  class MemoizedFunction5[-T1, -T2, -T3, -T4, -T5, +R](f: ((T1, T2, T3, T4, T5) => R))
    extends ((T1, T2, T3, T4, T5) => R) {

    private[this] val cache = new java.util.HashMap[(T1, T2, T3, T4, T5), R]

    def apply(x: T1, y: T2, z: T3, a: T4, b: T5): R = {
      val key = (x, y, z, a, b)
      val result = cache.get(key)
      if (result != null) result
      else {
        val res = f(x, y, z, a, b)
        cache.put(key, res)
        res
      }
    }
  }

  /**
   * A memoized single-argument function
   */
  class SoftMemoizedFunction1[-T1, +R](f: T1 => R) extends (T1 => R) {

    private[this] val cache = new SoftMemCache[T1, R]

    def apply(x: T1): R = {
      val key = x
      val result = cache.get(key)
      if (result.nonEmpty) result.get
      else {
        val res = f(x)
        cache.update(key, res)
        res
      }
    }

  }

  /**
   * A memoized 3-argument function
   */
  class SoftMemoizedFunction3[-T1, -T2, -T3, +R](f: ((T1, T2, T3) => R))
    extends ((T1, T2, T3) => R) {

    private[this] val cache = new SoftMemCache[(T1, T2, T3), R]

    def apply(x: T1, y: T2, z: T3): R = {
      val key = (x, y, z)
      val result = cache.get(key)
      if (result.nonEmpty) result.get
      else {
        val res = f(x, y, z)
        cache.update(key, res)
        res
      }
    }
  }

  /**
   * A memoized 5-argument function
   */
  class SoftMemoizedFunction5[-T1, -T2, -T3, -T4, -T5, +R](f: ((T1, T2, T3, T4, T5) => R))
    extends ((T1, T2, T3, T4, T5) => R) {

    private[this] val cache = new SoftMemCache[(T1, T2, T3, T4, T5), R]

    def apply(x: T1, y: T2, z: T3, a: T4, b: T5): R = {
      val key = (x, y, z, a, b)
      val result = cache.get(key)
      if (result.nonEmpty) result.get
      else {
        val res = f(x, y, z, a, b)
        cache.update(key, res)
        res
      }
    }
  }
}
