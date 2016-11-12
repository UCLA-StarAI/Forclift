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

import scala.collection._
import scala.language.implicitConversions
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.compiler._
import edu.ucla.cs.starai.forclift.conditioning._
import edu.ucla.cs.starai.forclift.propositional._
import edu.ucla.cs.starai.forclift.util._
import edu.ucla.cs.starai.forclift.nnf.visitors.SignLogDoubleWmc
import edu.ucla.cs.starai.forclift.nnf.visitors.VerifyWmcVisitor
import edu.ucla.cs.starai.forclift.nnf.visitors.WmcVisitor

case class WeightedCNF(
  cnf: CNF,
  domainSizes: DomainSizes,
  predicateWeights: PredicateWeights,
  conditionedAtoms: List[PositiveUnitClause] = Nil,
  compilerBuilder: Compiler.Builder = Compiler.Builder.default
  ) {
  
  def setCompiler(newCompiledBuilder: Compiler.Builder) = {
    WeightedCNF(cnf, domainSizes, predicateWeights, conditionedAtoms, newCompiledBuilder)
  }

  def addDomainSizes(extraDomainSizes: DomainSizes) = {
    WeightedCNF(cnf, domainSizes ++ extraDomainSizes, predicateWeights, conditionedAtoms, compilerBuilder)
  }

  def eqToConstraints(eq: Predicate): WeightedCNF = {
    copy(cnf = cnf.eqToConstraints)
  }

  def ++(other: WeightedCNF): WeightedCNF = {
    val newCnf = cnf ++ other.cnf
    val newDomainSizes = domainSizes ++ other.domainSizes
    val newPredicateWeights = predicateWeights ++ other.predicateWeights
    require(other.compilerBuilder == compilerBuilder)
    WeightedCNF(newCnf, newDomainSizes, newPredicateWeights, conditionedAtoms, other.compilerBuilder)
  }

  def addConstraint(clause: Clause) = {
    val newCnf = cnf + clause
    WeightedCNF(newCnf, domainSizes, predicateWeights, conditionedAtoms, compilerBuilder)
  }
  
  def vocabularyPredicates = cnf.predicates union predicateWeights.predicates

  lazy val wmcVisitor = WmcVisitor(predicateWeights)
  
  lazy val logSmoothWmc: SignLogDouble = {
    //TODO test sign of weights and optimize
    wmcVisitor.wmc(smoothNnf,domainSizes, predicateWeights)
  }

  lazy val logPropWmc: SignLogDouble = {
    val propCnf = toDimacsCNF
    val c2d = new LogC2D
    c2d.weightedModelCount(propCnf)
  }

  lazy val logSmoothPropWmc: SignLogDouble = {
    val propCnf = toSmoothDimacsCNF
    val c2d = new LogC2D
    c2d.weightedModelCount(propCnf)
  }

  def verifyLogWmc {
	VerifyWmcVisitor.verify(smoothNnf,domainSizes, predicateWeights)
    val correct = ((logSmoothPropWmc - logSmoothWmc).abs.logToDouble < 0.0000001
    				|| (logSmoothPropWmc.logToDouble - logSmoothWmc.logToDouble).abs < 0.0000001)
    if (!correct) {
      throw new VerifyWmcVisitor.VerificationFailedException
    }
  }

  def logPropProbability(query: Atom) = {
    val cnfBuilder = toDimacsCNFBuilder
    val queryIndex = cnfBuilder.getVar(query)
    val propCnf = cnfBuilder.toDimacsCNF
    val c2d = new LogC2D
    c2d.probability(propCnf, queryIndex)
  }

  lazy val groundCnf = cnf.ground(domainSizes)

  def ground = WeightedCNF(groundCnf, domainSizes, predicateWeights, conditionedAtoms, compilerBuilder)

  lazy val toDimacsCNFBuilder: DimacsCNFBuilder[Atom] = {
    def atomWeights(atom: Atom): (Double, Double) = {
      val w = predicateWeights(atom.predicate).posWDouble
      val negw = predicateWeights(atom.predicate).negWDouble
      (w, negw)
    }
    //	    println("new builder")
    val builder = new DimacsCNFBuilder[Atom](atomWeights)
    for (groundClause <- groundCnf.clauses) {
      //		    println("adding ground clause "+groundClause)
      builder.addClause(groundClause.posLits, groundClause.negLits)
    }
    builder
  }

  lazy val conditionedGroundings = conditionedAtoms.flatMap { _.ground(domainSizes) }

  lazy val groundAtomsToSmoothInCNF = {
    (vocabularyPredicates.flatMap { _.toAtom.ground(domainSizes) } -- conditionedGroundings).map { _.toPositiveUnitClause }
  }

  lazy val toSmoothDimacsCNFBuilder: DimacsCNFBuilder[Atom] = {
    val builder = toDimacsCNFBuilder
    // add every predicate grounding to the theory, for smoothing
    for (groundAtom <- groundAtomsToSmoothInCNF) {
      assume(groundAtom.posLits.size == 1)
      assume(groundAtom.negLits.isEmpty)
      //		    println("adding ground smoothing clause for "+groundAtom)
      builder.addClause(groundAtom.posLits, groundAtom.posLits)
    }
    builder
  }

  lazy val toDimacsCNF: DimacsCNF = toDimacsCNFBuilder.toDimacsCNF

  lazy val toSmoothDimacsCNF: DimacsCNF = toSmoothDimacsCNFBuilder.toDimacsCNF

  lazy val smoothNnf = nnf.smoothWithPredicates(vocabularyPredicates, conditionedAtoms.toSet)

  def sizeHint(d: Domain) = domainSizes(d.root).size

  lazy val nnf = {
    val compiler = compilerBuilder(sizeHint)
    compiler.compile(cnf)
  }

  def showNnfPdf(compact: Boolean, maxDepth: Int, file: String, verbose: Boolean = false) = {
    nnf.showPDF(domainSizes, predicateWeights, compact, maxDepth = maxDepth, file = file, verbose = verbose)
  }

  def showSmoothNnfPdf(compact: Boolean, maxDepth: Int, file: String, verbose: Boolean = false) = {
    smoothNnf.showPDF(domainSizes, predicateWeights, compact, maxDepth = maxDepth, file = file, verbose = verbose)
  }

  override def toString = List[Any](domainSizes, predicateWeights, cnf).mkString("\n")

  def atomEquivalenceClasses = {
    val shatteredCNF = cnf.shatter

    val atoms = shatteredCNF.toPositiveUnitClauses
    println("cAtoms:")
    println(atoms.mkString("\n"))
    println

    assume(atoms.forall { atom1 => atoms.forall { atom2 => (atom1 equivalent atom2) || (atom1 independent atom2) } })

    def filterEquivalent(atoms: List[PositiveUnitClause]): List[PositiveUnitClause] = atoms match {
      case Nil => Nil
      case head :: tail => head :: filterEquivalent(tail.filter(head.independent))
    }

    val indepAtoms = filterEquivalent(atoms.toList)
    println("Independent cAtoms:")
    println(indepAtoms.mkString("\n"))
    println

    for (atom1 <- indepAtoms; atom2 <- indepAtoms; if !(atom1 eq atom2)) {
      assume(atom1 != atom2, atom1 + " and " + atom2 + " are equal")
      assume(atom1 independent atom2, atom1 + " and " + atom2 + " are not independent")
    }
    indepAtoms
  }

  def conditionablePartial(conditionableAtoms: List[PositiveUnitClause]): (WeightedCNF, PartialConditioningDomainSet) = {

    //	    println("CONDITIONABLIZING ("+ cnf.size +" clauses)")
    //	    println(cnf)
    //	    println("ON PARTIAL")
    //	    println(conditionableAtoms.mkString("\n"))
    //	    println

    //	    println("(Shattering)")
    val shatteredCnf = cnf.shatter

    //	    println("(Finding unary atoms)")
    val firstToConditionablize = shatteredCnf.toPositiveUnitClauses.view.filter { catom =>
      catom.atom.isSingleton && conditionableAtoms.exists { conditionableAtom =>
        conditionableAtom.subsumes(catom) || conditionableAtom.equivalent(catom)
      }
    }.headOption

    def subdomainsForCAtom(catom: PositiveUnitClause): PartialConditioningDomain = {
      assume(catom.constrs.variables.size == 1)
      val logVar = catom.literalVariables.head
      val domain = catom.constrs.domainFor(logVar)
      val excludedConstants = catom.constrs.ineqConstrs(logVar).map { _.asInstanceOf[Constant] }
      val evidenceSubdoman = domain.subdomain("±" + catom.atom.toString(XNameSpace), "?" + catom.atom.toString(XNameSpace), "", "", excludedConstants)
      val posEvidenceSubdoman = evidenceSubdoman.subdomain("+" + catom.atom.toString(XNameSpace), "-" + catom.atom.toString(XNameSpace), "", "", excludedConstants)
      PartialConditioningDomain(catom, posEvidenceSubdoman, evidenceSubdoman)
    }

    if (firstToConditionablize.isEmpty) {
      // still add conditionableAtoms subset to conditionedAtoms, for smoothing later on
      val domainAndAtoms = conditionableAtoms.filter { atom => !atom.atom.isGround }.map { atom =>
        val logVar = atom.literalVariables.head
        val conditioningDomain = subdomainsForCAtom(atom)
        val conditioningAtom = atom.setDomain(logVar, conditioningDomain.knownDomain).toPositiveUnitClause
        (conditioningDomain, conditioningAtom)
      }
      val (conditioningDomains, newConditioningAtoms) = domainAndAtoms.unzip
      (this.copy(conditionedAtoms = newConditioningAtoms ::: conditionedAtoms), PartialConditioningDomainSet(conditioningDomains))
    } else {
      val catom = firstToConditionablize.get
      val logVar = catom.literalVariables.head
      val newCondDomains = subdomainsForCAtom(catom)
      val PartialConditioningDomain(_, posEvidenceSubdoman, evidenceSubdoman) = newCondDomains
      val negEvidenceSubdoman = newCondDomains.negativeDomain
      //            println("(Conditioning)")
      val conditionedCNF = cnf.condition(catom.setDomain(logVar, posEvidenceSubdoman).toPositiveUnitClause).condition(catom.setDomain(logVar, negEvidenceSubdoman).toNegativeUnitClause)
      // simplify to reduce number of clauses!
      //            println("(Simplifying)")
      val simplifiedConditionedCNF = conditionedCNF.simplify()
      val remainingConditionableAtoms = conditionableAtoms.flatMap { _.minus(catom) }
      val conditionedCAtom = catom.setDomain(logVar, evidenceSubdoman).toPositiveUnitClause
      val (wcnf, PartialConditioningDomainSet(condDomains)) = WeightedCNF(
        simplifiedConditionedCNF,
        domainSizes,
        predicateWeights,
        conditionedCAtom :: conditionedAtoms,
        compilerBuilder).conditionablePartial(remainingConditionableAtoms)
      (wcnf, PartialConditioningDomainSet(newCondDomains :: condDomains))
    }
  }

  //	def conditionableFull(conditionableAtoms: List[PositiveUnitClause]): (WeightedCNF, FullConditioningDomainSet) = {
  //	    
  //	    println("CONDITIONABLIZING ("+ cnf.size +" clauses)")
  //	    println(cnf)
  //	    println("ON FULL")
  //	    println(conditionableAtoms.mkString("\n"))
  //	    println
  //	    
  ////	    println("(Shattering)")
  //	    val shatteredCnf = cnf.shatter
  //	    
  ////	    println("(Finding unary atoms)")
  //	    val firstToConditionablize = shatteredCnf.toPositiveUnitClauses.view.filter{catom => 
  //	        catom.atom.isSingleton && conditionableAtoms.exists{conditionableAtom => 
  //	            conditionableAtom.subsumes(catom) || conditionableAtom.equivalent(catom)
  //            }
  //        }.headOption
  //        
  //        if(firstToConditionablize.isEmpty){
  //            fix like above
  //            (this,FullConditioningDomainSet(Nil))
  //        }else{
  //            val catom = firstToConditionablize.get
  ////            println("(Found "+catom+")")
  //            assume(catom.elemConstrs.variables.size == 1)
  //            val logVar = catom.variables.first
  //            val domain = catom.elemConstrs(logVar)
  //			val excludedConstants = catom.ineqConstrs(logVar).map{_.asInstanceOf[Constant]}
  //            val posEvidenceSubdoman = domain.subdomain(catom.atom.toString,catom.atom.toString, "+", "-",excludedConstants)
  //            val negEvidenceSubdoman = posEvidenceSubdoman.complement
  ////            println("(Conditioning)")
  //            val conditionedCNF = cnf.condition(catom.setDomain(logVar,posEvidenceSubdoman).toPositiveUnitClause).condition(catom.setDomain(logVar,negEvidenceSubdoman).toNegativeUnitClause)
  //            // simplify to reduce number of clauses!
  ////            println("(Simplifying)")
  //            val simplifiedConditionedCNF = conditionedCNF.simplify()
  //            val remainingConditionableAtoms = conditionableAtoms.flatMap{_.minus(catom)}
  //            val (wcnf,FullConditioningDomainSet(condDomains)) = WeightedCNF(
  //                    simplifiedConditionedCNF, 
  //                    domainSizes, 
  //                    predicateWeights, 
  //                    catom :: conditionedAtoms,
  //                    compiledBuilder).conditionableFull(remainingConditionableAtoms)
  //            val newCondDomains = FullConditioningDomain(catom,posEvidenceSubdoman)
  //            (wcnf,FullConditioningDomainSet(newCondDomains :: condDomains))
  //        }
  //    }

}
