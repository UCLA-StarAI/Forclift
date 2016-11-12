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

package edu.ucla.cs.starai.forclift.conditioning

import scala.util.Random._

import edu.ucla.cs.starai.forclift.examples.models._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.languages._

object TestConditioning {

  def main(args: Array[String]): Unit = {

    val nbPeople = 7
    val people = (1 to nbPeople).map { i: Int => "p" + i.toString }
    println("people = " + people)

    val model = new FriendsSmokerModel(nbPeople, people.toSeq, Nil)

    val smokes = model.catom("smokes(P)")
    val cancer = model.catom("cancer(P)")

    val smokers = shuffle(people).take(3)
    println("smoker yes = " + smokers)
    val smokersEvidence = smokers.map { smoker => model.catom("smokes(" + smoker + ")") }.toSet

    val nonSmokers = shuffle(people.toList filterNot (smokers.toList.contains(_))).take(2)
    println("smoker no = " + nonSmokers)
    val nonSmokersEvidence = nonSmokers.map { p => model.catom("!smokes(" + p + ")") }.toSet

    val posCancer = shuffle(people).take(2)
    println("cancer yes = " + posCancer)
    val posCancerEvidence = posCancer.map { p => model.catom("cancer(" + p + ")") }.toSet

    val negCancer = shuffle(people.toList filterNot (posCancer.toList.contains(_))).take(3)
    println("cancer no = " + negCancer)
    val negCancerEvidence = negCancer.map { p => model.catom("!cancer(" + p + ")") }.toSet

    val posEvidence = posCancerEvidence ++ smokersEvidence
    val negEvidence = negCancerEvidence ++ nonSmokersEvidence
    val interpretation = posEvidence ++ negEvidence.map { _.toNegativeUnitClause }

    val wmc = model.theory

    val cnf = wmc.cnf
    println("Original CNF")
    println(cnf)
    println

    //		val (fullWmc,fullDomains) = wmc.conditionableFull(List(smokes,cancer))
    //		
    //		println("Full Conditionable CNF")
    //		println(fullWmc.cnf)
    //		println
    //		println("Full Conditionable Domains")
    //		println(fullDomains.conditioningDomains.mkString("\n"))
    //		println()
    //		
    //		val domainElems = fullDomains.addEvidence(fullWmc.domainSizes,posEvidence)
    //		
    //		println("Domain Elements")
    //		println(domainElems.mkString("\n"))
    //		println()
    //		
    //		val fullWmc2 = fullWmc.addDomainSizes(ConditioningDomainSet.domainElements2SubDomainSizes(domainElems))
    //		
    //		println("Domain Sizes")
    //		println(fullWmc2.domainSizes.mkString("\n"))
    //		println()
    //		
    //		println("conditioned logSmoothWmc = "+fullWmc2.logSmoothWmc)
    ////		fullWmc2.showSmoothNnfPdf(true,20,"theory.smooth.nnf", false)

    val (parWmc, parDomains) = wmc.conditionablePartial(List(smokes, cancer))

    println("Partial Conditionable CNF")
    println(parWmc.cnf)
    println
    println("Partial Conditionable Domains")
    println(parDomains.conditioningDomains.mkString("\n"))
    println()

    val domainElems2 = parDomains.addEvidence(parWmc.domainSizes, posEvidence, negEvidence)

    println("Domain Elements")
    println(domainElems2.toArray.sortBy { _.toString }.mkString("\n"))
    println()

    val parWmc2 = parWmc.addDomainSizes(ConditioningDomainSet.domainElements2SubDomainSizes(domainElems2))

    println("Domain Sizes")
    println(parWmc2.domainSizes.toArray.sortBy { _.toString }.mkString("\n"))
    println()

    println
    //		parWmc2.verifylogSmoothWmc
    //		parWmc2.showSmoothNnfPdf(true,20,"theory.smooth.nnf", false)

    //	    println("interpretation = "+interpretation)

    //		val interpretationWeight = parWmc2.predicateWeights.logInterpretationWeight(interpretation)
    //		println("term logSmoothWmc = "+interpretationWeight)
    //		
    //		
    //		println("total logSmoothWmc = "+logprodexp(interpretationWeight,parWmc2.logSmoothWmc))

    //		println("ground conditioned CNF")
    //		println(parWmc2.ground.cnf)
    //		println
    //		
    ////		println("ground naive CNF")
    ////		println(naiveModel.theory.ground.cnf)
    ////		println
    //		
    //		println("simplified ground naive CNF")
    //		println(naiveModel.theory.ground.cnf.simplify())
    //		println

    println("conditioned logSmoothWmc = " + parWmc2.logSmoothWmc)
    println("conditioned logPropWMC = " + parWmc2.logSmoothPropWmc)

    val naiveModel = new FriendsSmokerModel(nbPeople, people.toSeq, interpretation.map { _.toString }.toSeq)
    println("naive logSmoothWmc = " + naiveModel.theory.logSmoothWmc)
    println("naive logPropWMC = " + naiveModel.theory.logSmoothPropWmc)

    println("simplified ground naive logPropWMC = " + naiveModel.theory.copy(cnf = naiveModel.theory.groundCnf.simplify()).logSmoothPropWmc)
    println("ground simplified naive logPropWMC = " + naiveModel.theory.copy(cnf = naiveModel.theory.cnf.simplify()).ground.logSmoothPropWmc)
    println

    //		println("conditioned conditioned on")
    //		println(parWmc2.conditionedAtoms.toArray.sortBy{_.toString}.mkString("\n"))
    //		println
    //		println("conditioned conditioned on")
    //		println(parWmc2.conditionedGroundings.toArray.sortBy{_.toString}.mkString("\n"))
    //		println
    //		
    //		println("smoothing conditioned on")
    //		println(parWmc2.groundAtomsToSmoothInCNF.toArray.sortBy{_.toString}.mkString("\n"))
    //		println
    //		
    //		println("smoothing naive on")
    //		println(naiveModel.theory.groundAtomsToSmoothInCNF.toArray.sortBy{_.toString}.mkString("\n"))
    //		println

  }

  //	def main(args : Array[String]) : Unit = {
  //		
  //	    val start = System.currentTimeMillis()
  //	    
  //	    assume(false)
  //	    
  //		val model = new WebKBModel(3,3,3)
  //		val wmc = model.theory
  //		
  //		var cnf = wmc.cnf
  //		println("Original CNF")
  //		println(cnf)
  //		println
  //		
  //		val pageclass = model.catom("pageclass(P,C)")
  //		val has = model.catom("has(Page,Word)")
  //		
  //		val conditionableWmc = wmc.conditionableFull(List(has)).conditionablePartial(List(pageclass))
  //		println("Conditionable CNF ("+ conditionableWmc.cnf.size +" clauses)")
  //		println(conditionableWmc.cnf)
  //		println
  //		
  //		println("Runtime: "+((System.currentTimeMillis()-start)/1000F) +"s")
  //		
  //	}

}

object TestMixedConditioning {

  def main(args: Array[String]): Unit = {

    val nbPeople = 100
    val people = (1 to nbPeople).map { i: Int => "p" + i.toString }
    println("people = " + people)

    val pairs = people.flatMap { p1 => people.map { p2 => "friends(" + p1 + "," + p2 + ")" } }

    val friends = shuffle(pairs).take(2)
    val noFriends = shuffle(pairs.toList filterNot (friends.toList.contains(_))).take(1).map { "!" + _ }

    val model = new FriendsSmokerModel(nbPeople, people.toSeq, friends ++ noFriends)

    val smokes = model.catom("smokes(P)")
    val cancer = model.catom("cancer(P)")

    val smokers = shuffle(people).take(3)
    println("smoker yes = " + smokers)
    val smokersEvidence = smokers.map { smoker => model.catom("smokes(" + smoker + ")") }.toSet

    val nonSmokers = shuffle(people.toList filterNot (smokers.toList.contains(_))).take(2)
    println("smoker no = " + nonSmokers)
    val nonSmokersEvidence = nonSmokers.map { p => model.catom("!smokes(" + p + ")") }.toSet

    val posCancer = shuffle(people).take(2)
    println("cancer yes = " + posCancer)
    val posCancerEvidence = posCancer.map { p => model.catom("cancer(" + p + ")") }.toSet

    val negCancer = shuffle(people.toList filterNot (posCancer.toList.contains(_))).take(3)
    println("cancer no = " + negCancer)
    val negCancerEvidence = negCancer.map { p => model.catom("!cancer(" + p + ")") }.toSet

    val posEvidence = posCancerEvidence ++ smokersEvidence
    val negEvidence = negCancerEvidence ++ nonSmokersEvidence
    val interpretation = posEvidence ++ negEvidence.map { _.toNegativeUnitClause }

    val wmc = model.theory

    val cnf = wmc.cnf
    println("Original CNF")
    println(cnf)
    println

    val (parWmc, parDomains) = wmc.conditionablePartial(List(smokes, cancer))

    println("Partial Conditionable CNF")
    println(parWmc.cnf)
    println
    println("Partial Conditionable Domains")
    println(parDomains.conditioningDomains.mkString("\n"))
    println()

    val domainElems2 = parDomains.addEvidence(parWmc.domainSizes, posEvidence, negEvidence)

    println("Domain Elements")
    println(domainElems2.toArray.sortBy { _.toString }.mkString("\n"))
    println()

    val parWmc2 = parWmc.addDomainSizes(ConditioningDomainSet.domainElements2SubDomainSizes(domainElems2))

    println("Domain Sizes")
    println(parWmc2.domainSizes.toArray.sortBy { _.toString }.mkString("\n"))
    println()

    println
    //		parWmc2.verifylogSmoothWmc
    //		parWmc2.showSmoothNnfPdf(true,20,"theory.smooth.nnf", false)

    //	    println("interpretation = "+interpretation)

    //		val interpretationWeight = parWmc2.predicateWeights.logInterpretationWeight(interpretation)
    //		println("term logSmoothWmc = "+interpretationWeight)
    //		
    //		
    //		println("total logSmoothWmc = "+logprodexp(interpretationWeight,parWmc2.logSmoothWmc))

    //		println("ground conditioned CNF")
    //		println(parWmc2.ground.cnf)
    //		println
    //		
    ////		println("ground naive CNF")
    ////		println(naiveModel.theory.ground.cnf)
    ////		println
    //		
    //		println("simplified ground naive CNF")
    //		println(naiveModel.theory.ground.cnf.simplify())
    //		println

    println("conditioned logSmoothWmc = " + parWmc2.logSmoothWmc)
    println("conditioned logPropWMC = " + parWmc2.logSmoothPropWmc)

    val naiveModel = new FriendsSmokerModel(nbPeople, people.toSeq, interpretation.map { _.toString }.toSeq)
    println("naive logSmoothWmc = " + naiveModel.theory.logSmoothWmc)
    println("naive logPropWMC = " + naiveModel.theory.logSmoothPropWmc)

    println("simplified ground naive logPropWMC = " + naiveModel.theory.copy(cnf = naiveModel.theory.groundCnf.simplify()).logSmoothPropWmc)
    println("ground simplified naive logPropWMC = " + naiveModel.theory.copy(cnf = naiveModel.theory.cnf.simplify()).ground.logSmoothPropWmc)
    println

    //		println("conditioned conditioned on")
    //		println(parWmc2.conditionedAtoms.toArray.sortBy{_.toString}.mkString("\n"))
    //		println
    //		println("conditioned conditioned on")
    //		println(parWmc2.conditionedGroundings.toArray.sortBy{_.toString}.mkString("\n"))
    //		println
    //		
    //		println("smoothing conditioned on")
    //		println(parWmc2.groundAtomsToSmoothInCNF.toArray.sortBy{_.toString}.mkString("\n"))
    //		println
    //		
    //		println("smoothing naive on")
    //		println(naiveModel.theory.groundAtomsToSmoothInCNF.toArray.sortBy{_.toString}.mkString("\n"))
    //		println

  }
}
