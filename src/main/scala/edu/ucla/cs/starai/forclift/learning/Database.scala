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

package edu.ucla.cs.starai.forclift.learning

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.compiler._
import edu.ucla.cs.starai.forclift.languages.mln._

class Database(val structure: MLN, val db: MLN) {

  val posEvidence: Set[Atom] = {
    db.posEvidence
  }

  require(posEvidence.forall { _.isGround }, "All evidence should be ground.")

  // set up data structure to speed up counting
  val groupedDb: Map[Predicate, Set[Atom]] = posEvidence.groupBy { _.predicate }

  val predicates = groupedDb.keySet

  /** Return the atoms in the database for the predicates in the given set. */
  def subDb(predicates: Set[Predicate]) = predicates.flatMap { groupedDb.getOrElse(_, Set.empty) }

  // the MLN parser creates new copies of the domains
  // wannes: I do not understand this completely
  val domainTranslation = db.domainSizes.keySet.map { d1 =>
    (d1 -> db.domainSizes.keySet.find { d2 =>
      d1.asInstanceOf[RootDomain].name == d2.asInstanceOf[RootDomain].name
    }.get)
  }.toMap

  val domainSizes: DomainSizes = {
    new DomainSizes(db.domainSizes.map { case (k, v) => (domainTranslation(k), v) }, useExplicitConstants = true)
  }

  lazy val vocabularySize = {
    (predicates union structure.predicates).toList.map { p =>
      p.toAtom.nbGroundings(domainSizes)
    }.sum
  }

  //val additionalConstants = db.evConsts.map {
  //case (dCopy, cs) =>
  //val d = domainTranslation(dCopy)
  //(d.asInstanceOf[RootDomain] -> (cs -- d.knownConstants.toSet))
  //}.toMap

  //def addAdditionalConstants() {
  //println("---\nWarning: adding database constants to dynamicConstants\n---")
  //for ((d, cs) <- additionalConstants) d.dynamicConstants ++= cs
  //}

  //def removeAdditionalConstants() {
  //for ((d, cs) <- additionalConstants) d.dynamicConstants --= cs
  //}

  override def toString = "db" + hashCode //posEvidence.toString

}

object Databases {

  def fromMLNs(structure: MLN, dbs: IndexedSeq[MLN], nbtest: Int = 0) = {
    new Databases(structure,
      dbs.map { new Database(structure, _) },
      nbtest)
  }

}

/**
 * Collection of databases for a particular MLN
 *
 * @param structure
 * @param dbs
 * @param nbtest
 *        Number that indicates how many of the databases (from the end)
 *        should be used for testing. First length-testdbs are training
 *        databases
 */
class Databases(
  structure: MLN,
  val dbs: IndexedSeq[Database],
  val nbtest: Int = 0) {

  require(dbs.forall { _.structure == structure })

  def predicates = dbs.flatMap { _.predicates }.toSet

  // dangerous! dont use! will be bigger than you think: val unionDomainSizes: DomainSizes = dbs.map { _.domainSizes }.reduce { _.combine(_) }

  /**
   * Create a domainSize which guarantees to have enough elements wrt
   * the arity of the predicates in the databases.
   */
  val minimalDomainSize = {
    println(structure)
    val wmc = structure.toWeightedCNF(false)
    // count all predicates, also those not appearing in a complex formula
    val allPredicates = (wmc.cnf.predicates union this.predicates)
    val maxArity = allPredicates.map { _.arity }.max
    val domains = dbs.flatMap { _.domainSizes.keySet }.toSet
    var minDomainSizes = DomainSizes.empty
    for (d <- domains) {
      val constants = d.knownConstants
      minDomainSizes = minDomainSizes + (d -> DomainSize(maxArity.max(constants.size), d, constants.toSet))
    }
    minDomainSizes
  }

  def size = dbs.size
  
  def trainDbIds = (0 until (dbs.size - nbtest))
  def testDbIds = ((dbs.size - nbtest) until dbs.size)

  def vocabularySize = dbs.map(_.vocabularySize).sum * 1.0 / dbs.size
  
  lazy val traindbs = dbs.slice(0, dbs.length - nbtest)
  lazy val testdbs = dbs.slice(dbs.length - nbtest, dbs.length)

  override def toString = {
    dbs.foldLeft("Databases:\n")((str, db) => str + db.toString + "\n")
  }
}
