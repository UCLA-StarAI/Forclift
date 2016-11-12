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

package edu.ucla.cs.starai.forclift.util.extracollection

import scala.collection._
import scala.language.implicitConversions 

final class MultiMap[K, V](
  final val self: Map[K, Set[V]] = Map.empty[K, Set[V]]) extends MapProxy[K, Set[V]] {

  import MultiMap._

  def contains(key: K, value: V) = this(key).contains(value)

  override def apply(key: K) = get(key).getOrElse(Set())

  def tuples = iterator.flatMap { case (k, set) => set.iterator.map { (k, _) } }

  def +(key: K, value: V): MultiMap[K, V] = {
    val oldSet = this(key)
    val newEntry = (key -> (oldSet + value))
    new MultiMap(this + newEntry)
  }

  def -(key: K, value: V): MultiMap[K, V] = {
    val oldSet = this(key)
    val newEntry = (key -> (oldSet - value))
    new MultiMap(this + newEntry)
  }

  override lazy val hashCode = super.hashCode

}

object MultiMap {

  def empty[K, V]: MultiMap[K, V] = new MultiMap()

  implicit def map2MultiMap[K, V](map: Map[K, Set[V]]): MultiMap[K, V] = {
    new MultiMap(map)
  }

}
