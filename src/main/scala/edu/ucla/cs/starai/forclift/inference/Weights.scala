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

package edu.ucla.cs.starai.forclift.inference

import edu.ucla.cs.starai.forclift._
import scala.language.implicitConversions
import edu.ucla.cs.starai.forclift.compiler._
import edu.ucla.cs.starai.forclift.conditioning._

import edu.ucla.cs.starai.forclift.propositional._
import scala.collection._
import util._


/**
 * Keep track of weights, both natural and log space.
 */
abstract class Weights {
  
  def posW: SignLogDouble
  def negW: SignLogDouble
  def negWPlusPosW: SignLogDouble
  
  def posWDouble: Double
  def negWDouble: Double
  def negWPlusPosWDouble: Double
  
  def posWLogDouble: LogDouble
  def negWLogDouble: LogDouble
  def negWPlusPosWLogDouble: LogDouble
  
  override def toString = "[" + posWDouble + "," + negWDouble + "]"
}

object Weights {

  def apply(posW: Double, negW: Double) = WeightsFromExp(posW, negW)

}

final case class WeightsFromExp(val posWDouble: Double, val negWDouble: Double) extends Weights {
  // cache everything for performance!
  val posW: SignLogDouble = posWDouble
  val negW: SignLogDouble = negWDouble
  val negWPlusPosW: SignLogDouble = negW + posW
  val negWPlusPosWDouble: Double = negWDouble + posWDouble
  val posWLogDouble: LogDouble = if(posW.pos) posW.toLogDouble else LogDouble.NaN
  val negWLogDouble: LogDouble = if(negW.pos) negW.toLogDouble else LogDouble.NaN
  val negWPlusPosWLogDouble: LogDouble = if(negWPlusPosW.pos) negWPlusPosW.toLogDouble  else LogDouble.NaN
}

final case class WeightsFromLog(val posW: SignLogDouble, val negW: SignLogDouble) extends Weights {
  // cache everything for performance!
  val posWDouble = posW.toDouble
  val negWDouble = negW.toDouble
  val negWPlusPosW: SignLogDouble = negW + posW
  val negWPlusPosWDouble: Double = negWDouble + posWDouble
  val posWLogDouble: LogDouble = if(posW.pos) posW.toLogDouble else LogDouble.NaN
  val negWLogDouble: LogDouble = if(negW.pos) negW.toLogDouble else LogDouble.NaN
  val negWPlusPosWLogDouble: LogDouble = if(negWPlusPosW.pos) negWPlusPosW.toLogDouble  else LogDouble.NaN
}

class PredicateWeights(val self: Map[Predicate, Weights] = Map.empty) extends MapProxy[Predicate, Weights] {

  override def toString = self.iterator.map {
    case (p, w) =>
      val domains = if (p.domains.nonEmpty) p.domains.mkString("(", ",", ")") else ""
      "predicate " + p + domains + " " + w.posWDouble + " " + w.negWDouble
  }.mkString("\n")

  def predicates = keySet

  def logInterpretationWeight(literals: Set[UnitClause]): SignLogDouble = {
    //println("weight of "+literals.mkString(" & ")+ " for "+this)
    literals.foldLeft(SignLogDouble.one) { (weight, literal) =>
      literal match {
        case _: PositiveUnitClause => weight * self(literal.atom.predicate).posW
        case _: NegativeUnitClause => weight * self(literal.atom.predicate).negW
        case _ => throw new IllegalStateException("Unit clause pattern failed")
      }
    }
  }

  def +(p: Predicate, w: Weights): PredicateWeights = {
    require(!contains(p))
    new PredicateWeights(self + (p -> w))
  }

  override def -(p: Predicate): PredicateWeights = {
    require(contains(p))
    new PredicateWeights(self - p)
  }

  def update(p: Predicate, w: Weights) = {
    new PredicateWeights(self + (p -> w))
  }

}

object PredicateWeights {

  val empty = new PredicateWeights(Map.empty)

  implicit def map2PredicateWeights(self: Map[Predicate, Weights]): PredicateWeights = new PredicateWeights(self)

}
