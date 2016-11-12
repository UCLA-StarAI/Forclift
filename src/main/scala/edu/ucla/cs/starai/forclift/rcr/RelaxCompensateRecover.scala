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

package edu.ucla.cs.starai.forclift.rcr

import scala.collection._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.compiler._
import edu.ucla.cs.starai.forclift.examples.models._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.util.SignLogDouble._
import scala.util.Random._
import util.KLD._
import edu.ucla.cs.starai.forclift.util.SignLogDouble

class RelaxCompensateRecover(
  weightedCNF: WeightedCNF,
  compilerBuilder: Compiler.Builder = Compiler.Builder.default,
  verbose: Boolean = false) {

  // require that the CNF is compiled from an MLN with only soft clauses.
  require(weightedCNF.cnf.clauses.forall { _.predicates.exists { _.name.name.startsWith("f_") } },
    ("The RCR implementation currently only supports soft formulas (no hard clauses).\n"
      + "(problem clause: " + weightedCNF.cnf.clauses.find { !_.predicates.exists { _.name.name.startsWith("f_") } }.get + ")"))

  val equip = new EquiprobableAtoms(weightedCNF, verbose)
  import equip._

  /**
   * Generate equivalences and copy atoms
   */
  def fullRelaxation(): (PredicateWeights, List[Compensation], MarginalCircuitsSet) = {
    onFullRelaxationStart()
    val compiler = compilerBuilder(weightedCNF.domainSizes.toInts)
    val (coShatteredEquivalences: List[CoShatteredEquivalence], relaxedCNF: CNF, relaxedWeightFunction) = {
      val coshatteredFactors = coshatteredCnf.distinctPositiveUnitClauses.filter { catom => factors(catom.atom.predicate) }
      val clausesByFactor = coshatteredFactors.map { factor => (factor, new CNF(coshatteredCnf.clauses.filter { _.dependent(factor) })) }
      if (verbose) {
        println("Coshattered Factor CNFs")
        for ((f, cnf) <- clausesByFactor) {
          println("-Factor " + f)
          println(cnf)
        }
        println
      }
      val (coShatteredEquivalencesPerFactor, relaxedCNFs) = clausesByFactor.map {
        case (factor, factorCNF) =>
          // check if factor is a unit factor, then don't copy nodes
          if (factorCNF.distinctPositiveUnitClauses.size == 2) {
            (Nil, factorCNF)
          } else {
            var coshatteredEquivalences: List[CoShatteredEquivalence] = Nil
            val relaxedClauses = factorCNF.clauses.map { clause =>
              val factorLiteral = clause.atoms.find(_.predicate == factor.predicate).get
              def copyAtomAndRecord(atom: Atom): Atom = {
                if (atom == factorLiteral) factorLiteral
                else {
                  // filter constants and duplicate logvars
                  val factorVars = factorLiteral.args.collect { case v: Var => v }.toList.distinct
                  var subscript = atom.args.map {
                    _ match {
                      case Constant(c) => "c" + c
                      case v: Var => factorVars.indexOf(v).toString
                    }
                  }.mkString("<", ",", ">")
                  if (subscript == "<0>" && factorVars.size == 1) subscript = ""
                  // not only give a new name for each factor, but also for each coshattered factor, so include
                  // use factor from higher up to avoid issues with different names for identical predicates
                  val copyName = Symbol(atom.predicate + "_{" + factor.toString.replaceAll(" ", "") + subscript + "}")
                  val copyArity = factorVars.size
                  val copyDomains = factorVars.map { v => factorLiteral.predicate.domains(factorLiteral.args.indexOf(v)) }
                  val copyPredicate = Predicate(copyName, copyArity, copyDomains)
                  val copyAtom = copyPredicate(factorVars: _*)
                  if (!coshatteredEquivalences.exists { _.copy.predicate.name == copyName }) {
                    val newEquivalence = new CoShatteredEquivalence(copyAtom, atom, clause.constrs, domainSizes)
                    coshatteredEquivalences = newEquivalence :: coshatteredEquivalences
                  }
                  copyAtom
                }
              }
              val posLiterals = clause.posLits.map { copyAtomAndRecord(_) }
              val negLiterals = clause.negLits.map { copyAtomAndRecord(_) }
              new Clause(posLiterals, negLiterals, clause.constrs)
            }
            // to create a (almost) spanning tree, already relax 1 equivalence
            val maxArityEquivalence = coshatteredEquivalences.maxBy { _.nbGroundOrigAtoms }
            val spanningTreeCnf = maxArityEquivalence.substituteCopy(new CNF(relaxedClauses))
            //                        (coshatteredEquivalences, new CNF(relaxedClauses))
            (coshatteredEquivalences filterNot (_ == maxArityEquivalence), spanningTreeCnf)
          }
      }.unzip
      val coShatteredEquivalencesWithTransl = coShatteredEquivalencesPerFactor.flatten
      val translationEquivalences = coShatteredEquivalencesWithTransl.filter { eq1 =>
        // find equivalences that maintain arity and appear only once
        (eq1.maintainsArity
          && !relaxedCNFs.exists { cnf =>
            // not a unit clause and containing the translated atom
            cnf.distinctPositiveUnitClauses.size > 2 && cnf.dependent(eq1.origCAtom)
          }
          && !coShatteredEquivalencesWithTransl.exists { eq2 =>
            !(eq1 eq eq2) && eq1.origCAtom.dependent(eq2.origCAtom)
          })
      }
      val relaxedCNFWithTransl = relaxedCNFs.foldLeft(new CNF(Nil)) { _ ++ _ }
      val coShatteredEquivalences = coShatteredEquivalencesWithTransl filterNot (translationEquivalences.contains(_))
      val relaxedCNF = translationEquivalences.foldLeft(relaxedCNFWithTransl) { (cnf, equiv) => equiv.substituteCopy(cnf) }
      val copyPredicates = {
        val copyPredicates = coShatteredEquivalences.map { _.copy.predicate }.toSet
        if (verbose) {
          println("Copy Predicates")
          println(copyPredicates.mkString("\n"))
          println
        }
        copyPredicates
      }
      // copy predicates get weight 1,1, to keep partition function computations correct!
      val relaxedWeightFunction = copyPredicates.foldLeft(originalPredicateWeights) { _.+(_, Weights(1, 1)) }
      // propagate hard factors
      val hardFactors = factors.filter { f => relaxedWeightFunction(f).posWDouble == 1 && relaxedWeightFunction(f).negWDouble == 0 }
      val relaxedCNFWithHardFactors = hardFactors.foldLeft(relaxedCNF) { (cnf, f) =>
        cnf.condition(f.toAtom)
      }
      if (verbose) {
        {
          println("Hard Factors")
          println(hardFactors.mkString("\n"))
          println
        }
        {
          println("Translations Undone")
          println(translationEquivalences.mkString("\n"))
          println
        }
        {
          println("Relaxed CNF")
          println(relaxedCNFWithHardFactors.mkString("\n"))
          println
        }
        {
          println("Equivalences")
          println(coShatteredEquivalences.mkString("\n"))
          println
        }
        {
          val fullyRelaxedZ = new CNFCircuit(compiler, coShatteredEquivalences.foldLeft(relaxedCNFWithHardFactors) { _ ++ _.smoothingDef })
          fullyRelaxedZ.cacheWmc(domainSizes, relaxedWeightFunction)
          println("Fully relaxed log partition function = exp(" + fullyRelaxedZ.cachedWmc + ") = " 
        		  	+ fullyRelaxedZ.cachedWmc.toDouble)
          println
          val fullyRelaxedMarginalCircuits = coshatteredFgCAtoms.map { catom =>
            val marginalCircuit = new MarginalCircuits(compiler, fullyRelaxedZ, catom, domainSizes)
            marginalCircuit.cacheQueryWmc(relaxedWeightFunction)
            marginalCircuit
          }
          println("Fully relaxed Marginals")
          println(fullyRelaxedMarginalCircuits.map { c => "P(" + c.queryClass + ") = " + c.marginal.toDouble }.mkString("\n"))
          println
        }
      }
      (coShatteredEquivalences, relaxedCNFWithHardFactors, relaxedWeightFunction)
    }

    val compensationsWeightFunction = {
      val thetas = coShatteredEquivalences.map { _.thetaCopy } ++ coShatteredEquivalences.map { _.thetaOrig }
      thetas.foldLeft(relaxedWeightFunction) { _.+(_, Weights(0.5, 0.5)) }
    }

    val initialMarginalCircuitsSet = {
      val relaxedCompensateCNF = coShatteredEquivalences.foldLeft(relaxedCNF) { _ ++ _.thetaDefs }
      if (verbose) {
        println("Relaxed Compensate CNF")
        println(relaxedCompensateCNF)
        println
      }
      val independentZs = relaxedCompensateCNF.independentSubtheories.map { new CNFCircuit(compiler, _) }
      if (verbose) {
        println("Relaxed Compensate CNF Independent Subtheories")
        println(independentZs.map { _.cnf }.mkString("\n--------------\n"))
        println
      }
      val originalMarginals = coshatteredFgCAtoms.map { catom =>
        val dependentZ = independentZs.find { _.cnf.dependent(catom) }.get
        new MarginalCircuits(compiler, dependentZ, catom, domainSizes)
      }
      val copyMarginals = coShatteredEquivalences.map { equiv =>
        val catom = equiv.copyCAtom
        val dependentZ = independentZs.find { _.cnf.dependent(catom) }.get
        new MarginalCircuits(compiler, dependentZ, catom, domainSizes)
      }
      val mcs = new MarginalCircuitsSet(independentZs, originalMarginals, copyMarginals, compiler, domainSizes)
      //	  if(verbose){
      //	    println("MarginalCircuitsSet")
      //	    println(mcs)
      //	    println()
      //	  }
      mcs
    }

    val initialCompensations = {
      coShatteredEquivalences.map { eq =>
        val origMarginal = initialMarginalCircuitsSet.origMarginals.find(_.queryClass.equivalent(eq.origCAtom)).get
        val copyMarginal = initialMarginalCircuitsSet.copyMarginals.find(_.queryClass.equivalent(eq.copyCAtom)).get
        new Compensation(eq, origMarginal, copyMarginal, 0.0)
      }
    }

    (compensationsWeightFunction, initialCompensations, initialMarginalCircuitsSet)
  }

  //
  // COMPENSATE
  //
  val defaultPrecision = 0.0000000001
  val defaultMaxNbIterations = 1000
  val defaultDamping = 0.5
  val defaultKeepParams = true

  def compensateFullRelaxation(damping: Double = defaultDamping, precision: Double = defaultPrecision, maxNbIterations: Int = defaultMaxNbIterations) = {
    val (compensationsWeightFunction, initialCompensations, initialMarginalCircuitsSet) = fullRelaxation
    onCompensateRecoverStart(
      compensationsWeightFunction,
      initialCompensations,
      initialMarginalCircuitsSet)
    compensate(
      compensationsWeightFunction,
      initialCompensations,
      initialMarginalCircuitsSet,
      damping,
      precision,
      maxNbIterations)
  }

  def compensate(initialWeightFunction: PredicateWeights,
    compensations: List[Compensation],
    marginalCircuitsSet: MarginalCircuitsSet,
    damping: Double = defaultDamping,
    precision: Double = defaultPrecision,
    maxNbIterations: Int = defaultMaxNbIterations) = {

    onStartCompensation(initialWeightFunction, compensations, marginalCircuitsSet)

    if (verbose) {

      println("Compensating " + compensations.size + " first-order equivalences")
    }

    var weights = initialWeightFunction

    def updateCachedWmcs() {
      for (zCircuit <- marginalCircuitsSet.independentZs) zCircuit.cacheWmc(domainSizes, weights)
      for (marginal <- marginalCircuitsSet.copyMarginals) marginal.cacheQueryWmc(weights)
      for (marginal <- marginalCircuitsSet.origMarginals) marginal.cacheQueryWmc(weights)
    }

    def clearCachedWmcs() {
      for (zCircuit <- marginalCircuitsSet.independentZs) zCircuit.clearCache
      for (marginal <- marginalCircuitsSet.copyMarginals) marginal.clearQueryCache
      for (marginal <- marginalCircuitsSet.origMarginals) marginal.clearQueryCache
    }

    clearCachedWmcs()

    val logPrecision = math.log(precision)
    val logDamping: (SignLogDouble,SignLogDouble) = (damping, (1-damping))

    var converged = false
    var i = 0
    while (!converged && i < maxNbIterations) {
      if (verbose) {
        println
        println("ITERATION " + i)
      }
      i += 1
      converged = true

      // update weighted model counts
      if (i == 1 && verbose) {
        val start = System.currentTimeMillis()
        println("Starting compilation")
        updateCachedWmcs()
        println("Compilation done in " + ((System.currentTimeMillis() - start) / 1000F) + "s")
        for (zCircuit <- marginalCircuitsSet.independentZs) {
          println("z circuit size = " + zCircuit.smoothNNF.size)
          println("z circuit size evaluation complexity = O(n^" + zCircuit.smoothNNF.evalOrder + ")")
        }
        for (marginal <- marginalCircuitsSet.copyMarginals) {
          println(marginal.queryClass + " circuit size = " + marginal.queryCNFCircuit.smoothNNF.size)
          println(marginal.queryClass + " circuit size evaluation complexity = O(n^" + marginal.queryCNFCircuit.smoothNNF.evalOrder + ")")
        }
        for (marginal <- marginalCircuitsSet.origMarginals) {
          println(marginal.queryClass + " circuit size = " + marginal.queryCNFCircuit.smoothNNF.size)
          println(marginal.queryClass + " circuit size evaluation complexity = O(n^" + marginal.queryCNFCircuit.smoothNNF.evalOrder + ")")
        }
      } else {
        updateCachedWmcs()
      }

      if (verbose) {
        val sCompKLDs = compensations.map { _.singleCompensationKLD(weights) }
        println("Single Compensation KLD = " + sCompKLDs.map { _.toFloat }.mkString(" + "))
        println("                        = " + sCompKLDs.sum)
        val tCompKLDs = compensations.map { _.totalCompensationsKLD(weights) }
        println("Total Compensation KLD = " + tCompKLDs.map { _.toFloat }.mkString(" + "))
        println("                       = " + tCompKLDs.sum)
      }
      onCompensationIteration(i, compensations, marginalCircuitsSet, weights)

      for (compensation <- compensations) {
        val (compWeights, compConverged) = compensation.stepThetaParams(weights, logDamping, logPrecision)
        if (verbose) {
          print(compensation.eq.thetaCopy + "=" + weights(compensation.eq.thetaCopy))
          print(" and ")
          print(compensation.eq.thetaOrig + "=" + weights(compensation.eq.thetaOrig))
          if (compConverged) println("  (Converged)")
          else println
        }
        //        //TODO REMOVE TEST
        //        compensations.groupBy()
        //        // END TEST
        weights = compWeights
        converged = converged && compConverged
      }

      // reset cached weighted model counts
      clearCachedWmcs()

      if (verbose && converged) {
        println("CONVERGED!")
        println
      }

    }
    if (i == maxNbIterations) {
      // did not converge, reset parameters
      val thetas = compensations.map { _.eq.thetaCopy } ++ compensations.map { _.eq.thetaOrig }
      weights = thetas.foldLeft(weights) { _.update(_, Weights(.5, .5)) }
    }
    updateCachedWmcs
    // done

    onCompensationIteration(i + 1, compensations, marginalCircuitsSet, weights)
    onEndCompensation(weights, compensations, marginalCircuitsSet)

    if (verbose) {
      println
      val sCompKLDs = compensations.map { _.singleCompensationKLD(weights) }
      println("Single Compensation KLD = " + sCompKLDs.map { _.toFloat }.mkString(" + "))
      println("                        = " + sCompKLDs.sum)
      val tCompKLDs = compensations.map { _.totalCompensationsKLD(weights) }
      println("Total Compensation KLD = " + tCompKLDs.map { _.toFloat }.mkString(" + "))
      println("                       = " + tCompKLDs.sum)
      val unnormalizedZ = marginalCircuitsSet.cachedZ(weights)
      val logPfFactor = compensations.foldLeft(one) { (f, c) => (f * c.partitionFunctionFactor(weights)) }
      val logPf = (unnormalizedZ / logPfFactor)
      println("Unnomalized Partition function = exp(" + unnormalizedZ + ") = " + unnormalizedZ.toDouble)
      println("Offset Partition function = exp(" + logPfFactor + ") = " + logPfFactor.toDouble)
      println("Partition function = exp(" + logPf + ") = " + logPf.toDouble)
      println("Compensations")
      for (compensation <- compensations) {
        println(" - Approx P(" + compensation.eq.origCAtom + ") = " + compensation.origMarginal.marginal.toDouble)
        println("   Approx P(" + compensation.eq.copyCAtom + ") = " + compensation.copyMarginal.marginal.toDouble)
        println("   Single Compensation KLD = " + compensation.singleCompensationKLD(weights))
        println("   Total Compensation  KLD = " + compensation.totalCompensationsKLD(weights))
        println("   Accumulated Total Compensation  KLD = " + compensation.accumulatedTotalCompensationsKLD)
      }
      println("Marginals")
      for (m <- marginalCircuitsSet.origMarginals ++ marginalCircuitsSet.copyMarginals) {
        println(" - P(" + m.queryClass + ") = " + m.marginal.toDouble)
      }
    }
    weights
  }

  //
  // RECOVER
  //

  def compensateFullRelaxationAndRecover(
    maxNbRecoveries: Int = Integer.MAX_VALUE, keepParams: Boolean = defaultKeepParams,
    damping: Double = defaultDamping, precision: Double = defaultPrecision, maxNbIterations: Int = defaultMaxNbIterations) = {
    val (compensationsWeightFunction, initialCompensations, initialMarginalCircuitsSet) = fullRelaxation()
    onCompensateRecoverStart(compensationsWeightFunction,
      initialCompensations,
      initialMarginalCircuitsSet)
    compensateAndRecover(
      compensationsWeightFunction,
      initialCompensations,
      initialMarginalCircuitsSet,
      maxNbRecoveries,
      keepParams,
      damping,
      precision,
      maxNbIterations)
  }

  def compensateAndRecover(
    weightFunctionArg: PredicateWeights,
    compensationsArg: List[Compensation],
    marginalCircuitsSetArg: MarginalCircuitsSet,
    maxNbRecoveriesArg: Int = Integer.MAX_VALUE,
    keepParams: Boolean = defaultKeepParams,
    damping: Double = defaultDamping,
    precision: Double = defaultPrecision,
    maxNbIterations: Int = defaultMaxNbIterations): (MarginalCircuitsSet, PredicateWeights) = {
    var compensations = compensationsArg
    var maxNbRecoveries = maxNbRecoveriesArg
    var marginalCircuitsSet = marginalCircuitsSetArg
    var weightFunction = compensate(weightFunctionArg, compensations, marginalCircuitsSet, damping, precision, maxNbIterations)
    var canRecoverWithCompiler = true
    while (maxNbRecoveries > 0 && compensations.nonEmpty && canRecoverWithCompiler) {
      val orderedCompensations = {
        compensations.foreach { _.addToAccumulatedTotalCompensationsKLD(weightFunction) }
        val orderedCompensations = compensations.sortBy { -_.accumulatedTotalCompensationsKLD }
        if (verbose) {
          println("Worst compensation " + orderedCompensations.head.eq)
          println
        }
        orderedCompensations
      }
      canRecoverWithCompiler = orderedCompensations.exists { worstCompensation =>
        val (mappedCircuitsSet, mappedOrigMarginals, mappedCopyMarginals) = marginalCircuitsSet.recover(worstCompensation)
        if (verbose) {
          println("Recovered")
          println(worstCompensation.origMarginal.Z.cnf)
          println("and")
          println(worstCompensation.copyMarginal.Z.cnf)
          println("into")
          println(mappedOrigMarginals(worstCompensation.origMarginal).Z.cnf)
          println
        }
        val newCompensations = (compensations filterNot (_ == worstCompensation)).map { _.mapMarginals(mappedOrigMarginals, mappedCopyMarginals) }
        val newWeightFunction = {
          var newWeightFunction = weightFunction - worstCompensation.eq.thetaCopy - worstCompensation.eq.thetaOrig
          if (!keepParams) {
            val thetas = newCompensations.map { _.eq.thetaCopy } ++ newCompensations.map { _.eq.thetaOrig }
            newWeightFunction = thetas.foldLeft(newWeightFunction) { _.update(_, Weights(.5, .5)) }
          }
          newWeightFunction
        }

        try {
          // do compensation
          weightFunction = compensate(newWeightFunction, newCompensations, mappedCircuitsSet, damping, precision, maxNbIterations)
          compensations = newCompensations
          weightFunction = newWeightFunction
          maxNbRecoveries -= 1
          marginalCircuitsSet = mappedCircuitsSet
          true
        } catch {
          case e: IllegalArgumentException => {
            if (verbose) {
              println("Could not recover " + worstCompensation + ", trying other equivalence.")
            }
            false
          }
        }
      }

    }
    if (verbose && !canRecoverWithCompiler) {
      compensations.foreach { compensation => println("Could not recover " + compensation + ".") }
    }
    (marginalCircuitsSet, weightFunction)
  }

  def onFullRelaxationStart() {}

  def onCompensateRecoverStart(
    initialWeightFunction: PredicateWeights,
    compensations: List[Compensation],
    marginalCircuitsSet: MarginalCircuitsSet) {}

  def onStartCompensation(
    initialWeightFunction: PredicateWeights,
    compensations: List[Compensation],
    marginalCircuitsSet: MarginalCircuitsSet) {}

  def onEndCompensation(
    weights: PredicateWeights,
    compensations: List[Compensation],
    marginalCircuitsSet: MarginalCircuitsSet) {}

  def onCompensationIteration(
    i: Int,
    compensations: List[Compensation],
    marginalCircuitsSet: MarginalCircuitsSet,
    weights: PredicateWeights) {}

  def println { println("") }

  def println(str: Object) {
    System.out.println(str.toString)
  }

  def print(str: Object) {
    System.out.print(str.toString)
  }

}
