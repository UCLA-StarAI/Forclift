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

import java.io.File
import java.io.FileWriter
import java.io.FileNotFoundException

import scala.collection.mutable
import scala.io.BufferedSource
import scala.language.implicitConversions

abstract class NameSpace[-I, O] {

  private[this] val names = new mutable.HashMap[I, O]

  protected def createName(obj: I): O

  def getName(obj: I): O = {
    names.getOrElseUpdate(obj, createName(obj))
  }

  def forceName(obj: I, name: O): O = {
    require(!names.contains(obj) || names(obj) == name)
    names(obj) = name
    name
  }

}

object ToStringNameSpace extends NameSpace[Any, String] {

  override protected def createName(obj: Any) = obj.toString

  override def getName(obj: Any) = obj.toString

}

object XNameSpace extends NameSpace[Any, String] {

  override protected def createName(obj: Any) = "X"

  override def getName(obj: Any) = "X"

}

object Timer{
  
  def apply[A](f: => A)(output: Long => String): A = {
    val start = System.currentTimeMillis()
    val ret = f
    println(output(System.currentTimeMillis()-start))
    ret
  }
  
}

object Output{
    
  def writeToFile(file: File, content:String, report: String = ""){
    val out = new FileWriter(file)
    out.write(content)
    if(report.nonEmpty) println(report);
    out.close
  }
  
}

object CacheStats {

  var hitCount = 0
  var missCount = 0

  def print = {
    val nb: Int = (hitCount + missCount)
    if (nb % 1000 == 0) {
      println("Hits: " + hitCount + " Misses: " + missCount)
    }
  }

  def hit = {
    hitCount += 1
    print
  }

  def miss = {
    missCount += 1
    print
  }
}

object Resource{
  
  def fromFile(p: String): BufferedSource =
    Option(getClass.getResourceAsStream(p)).map(scala.io.Source.fromInputStream)
      .getOrElse(throw new FileNotFoundException("Resource " + p))

}