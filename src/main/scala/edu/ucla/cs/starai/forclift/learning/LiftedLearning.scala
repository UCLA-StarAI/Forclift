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
import edu.ucla.cs.starai.forclift.languages.mln._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.compiler._
import collection._
import breeze.optimize._
import breeze.linalg._
import breeze.math._
import LiftedLearning._

/**
 * Class for likelihood calculations and learning.
 *
 * For now, only learn from a single interpretation
 *
 * Warning: Learning adds dynamic new constants to domainsizes. Therefore,
 *          you can't use the same mln or parser objects afterwards for
 *          something else.
 */
class LiftedLearning(
  structure: MLN,
  traindbMLNs: Seq[MLN],
  normalizeLH: Boolean = false,
  normalizepll: Boolean = false,
  mu: Double = 0.0, // alchemy default 0
  sigma: Double = 100.0, // alchemy default 100
  compiler: Compiler = Compiler.default,
  optimizer: FirstOrderMinimizer[DenseVector[Double], DiffFunction[DenseVector[Double]]] 
		  = new LBFGS[DenseVector[Double]](tolerance = 1E-15),
  verbose: Boolean = false,
  testdbMLNs: Seq[MLN] = Seq(),
  skolemize: Boolean = true) {
  
  // learning messes with the constants in the domain: 
  // it cannot run when the domain already has anonymous constants
  require(structure.domains.forall { _.dynamicConstants.isEmpty },
      "Learning cannot run when the domain already has anonymous constants: "
      +"do not run multiple calls of learning of likelihood evaluation on the same parsed MLN")
  
  if (verbose && normalizeLH) println("Using normalization for learning")
  if (verbose && normalizepll) println("Using normalization for pseudo-likelihood")
  require(!normalizepll || !normalizeLH,
    "Normalization for PLL and learning is not yet compatible")

  val structure2 = if(skolemize){
      if(verbose) println("Using Skolemizing")
      structure.toMLNSkolemized
    } else {
      if(verbose) println("Not using Skolemization")
      structure
    }

  val dbs: Databases = Databases.fromMLNs(structure2,
    (traindbMLNs ++ testdbMLNs).toIndexedSeq,
    testdbMLNs.length)

  // Check whether there are empty domains. This is not allowed.
  for (db <- dbs.dbs) {
    db.domainSizes.emptyDomains match {
      case Some(doms) => {
        throw new IllegalStateException("Some domains have size 0 and have also no know constants in the theory or databases: " + doms.mkString(", "))
      }
      case _ => {}
    }
  }
  // Check that all domainSizes use explicitConstants
  require(dbs.dbs.forall { _.domainSizes.useExplicitConstants },
    "Not all domainSizes from databases are using explicit constants")

  lazy val hardFormulas = structure.wformulas.filter(_.hard)

  // mlnFormulas      = All complex mln formulas that are used for learning
  // learnableClauses = All formulas (complex and unit) that are present in
  //                    the theory, implicitely or explicitely
  // indepUnitPreds   = Predicates that only appear as unit clauses and
  //                    not in any other formula
  // hardCNF          = CNF for all hard clauses in the MLN
  // countTime		  = Time spent counting number of true groundings
  lazy val (mlnFormulas, learnableClauses, indepUnitPreds, hardCNF, countTime): 
	  (IndexedSeq[LearningMLNFormula], IndexedSeq[LearningFormula], Set[Predicate], CNF, Long) = {

    var start = System.currentTimeMillis()
    val nameSpace = new WFNameSpace

    // partition into weighted unit clauses and weighted complex formulas
    val (weightedPredicates, formulas) = structure2.toMLNasAlchemyCNF.wformulas.partition { wf =>
      wf.formula match {
        case LiteralFormula(atom, true) => {
          // Check whether the atom has all different variables as
          // arguments. If, for example, an atom has the same variable
          // on two different positions, it is not a basic unit 
          // clause representing a weightedPredicate.
          // For example: 0.25 myPredicate(Var1, Var2, Var1)
          atom.variables.size == atom.predicate.arity
        }
        case _ => false
      }
    }

    // set of learnable mln formulas
    // assumes MLN splitting
    val (mlnHardFormulas, mlnSoftFormulas) = formulas.flatMap { _.expandAlchemy }.flatMap { _.splitClauses }.partition { _.hard }
    if (verbose) println("Hard non-unit formulas:\n" + mlnHardFormulas.mkString("\n"))
    if (verbose) println("Weighted non-unit formulas:")
    val mlnLearnFormulas = mlnSoftFormulas.map {
      LearningMLNFormula(this, _, nameSpace, dbs, mu, sigma, verbose)
    }.toIndexedSeq

    // set of learnable unit clauses
    if (verbose) println("Weighted unit formulas:")

    val mlnPredicatesInFormulas = mlnLearnFormulas.flatMap { _.mlnPredicates }.toSet
    val mlnPredicates = (
      mlnPredicatesInFormulas
      union weightedPredicates.flatMap { _.predicates }.toSet
      union dbs.predicates)
    val unitClauses = {
      val unitClauseMap = mlnPredicates.map { pred =>
        (pred ->
          new LearningUnitClause(this, pred, nameSpace, dbs, mu, sigma, verbose))
      }.toMap
      // initialize weights of unit clauses
      for (wPred <- weightedPredicates) {
        unitClauseMap(wPred.predicates.head).logWeight = wPred.weight
      }
      unitClauseMap.values.toIndexedSeq
    }

    // all learnable clauses: formulas and unit clauses
    val learnableFormulas = (unitClauses ++ mlnLearnFormulas).toIndexedSeq

    // Figure out which predicates are only in unit clauses and not in
    // mlnLearnFormulas. For those we should add a unit clause to the CNF such
    // that the CNF contains all predicates in the MLN.
    val indepUnitPreds = unitClauses.map { _.res }.toSet.diff(mlnPredicatesInFormulas)

    val cnfFromHardMLN = mlnHardFormulas.foldLeft(CNF()) { (cnf, formula) => cnf ++ formula.toWeightedCNF(nameSpace).cnf }

    //if (verbose) {
    val countTime = (System.currentTimeMillis() - start) 
    println("Initializing counts took " + (countTime/ 1000F) + "s")
    if(verbose) println
    //}

    (mlnLearnFormulas, learnableFormulas, indepUnitPreds, cnfFromHardMLN, countTime)
  }
  
  def priorDensity() = learnableClauses.map(_.priorDensity).reduce(_ * _)
  def perVariablePriorDensity() = priorDensity().root(dbs.vocabularySize)
  def gradientLogPriorDensity(i: Int) = learnableClauses(i).gradientLogPriorDensity
  def gradientPerVariableLogPriorDensity(i: Int) = gradientLogPriorDensity(i) / dbs.vocabularySize

  lazy val (zs,compileTime) : 
	  (IndexedSeq[(Database, PrecompiledCNFCircuit)],Long) = {
    var start = System.currentTimeMillis()
    val cnfFromMLN = mlnFormulas.foldLeft(hardCNF) { (cnf, formula) => cnf ++ formula.cnf }
    val cnf = cnfFromMLN
    if (verbose) println("Compiling partition function")
    val vocabularyPredicates = learnableClauses.map { _.res }.toSet
    assume((cnf.predicates ++ indepUnitPreds).subsetOf(vocabularyPredicates))
    val circuit = compiler.compile(cnf).smoothWithPredicates(vocabularyPredicates)
    //        circuit.showPDF(DomainSizes.empty, PredicateWeights.empty, false, maxDepth=7)
    if (verbose) {
      println("Partition function has size " + circuit.size)
      println("Partition function has order " + circuit.evalOrder)
    }
    val zs = dbs.dbs.map { db => (db, new PrecompiledCNFCircuit(circuit)) }
    // construct CNF holding theory and the most general atom of every res, so that every grounding is covered somewhere
    val allcoveringCNF = new CNF(vocabularyPredicates.map { _.toAtom }.toList ::: cnf.clauses)
    val queryClassesInCNF = allcoveringCNF.equiprobableClasses
    val queryClasses = queryClassesInCNF.groupBy { _.predicate }
    for (learnable <- learnableClauses) {
      assume(queryClasses.contains(learnable.res), "Problem: CNF does not contain " + learnable.res)
      learnable.initializeCircuits(compiler, queryClasses(learnable.res).toIndexedSeq, zs, vocabularyPredicates)
    }
    //if (verbose) {
    val compileTime = (System.currentTimeMillis() - start)
    println("Compiling circuits took " + (compileTime / 1000F) + "s")
    if(verbose) println
    //}
    (zs,compileTime)
  }

  lazy val trainZs = zs.slice(0, dbs.size - dbs.nbtest)
  lazy val testZs = zs.slice(dbs.size - dbs.nbtest, dbs.size)
  
  
  def buildDatabaseLikelihood(id: Int): DatabaseLikelihood = {
      val db = dbs.dbs(id)
      require(zs(id)._1 == db)
      val formulaCircuits = learnableClauses.map{_.circuitsForDatabases(id)}
      require(formulaCircuits.forall(_.db == db))
      val z = zs(id)._2 
      DatabaseLikelihood(db,formulaCircuits,z)
  }
  
  lazy val trainDatabaseLikelihoods: IndexedSeq[DatabaseLikelihood] = dbs.trainDbIds.map(buildDatabaseLikelihood(_))
  lazy val testDatabaseLikelihoods: IndexedSeq[DatabaseLikelihood] = dbs.testDbIds.map(buildDatabaseLikelihood(_))
  
  def getPredicateWeights(): PredicateWeights = {
    val predws = learnableClauses.map { l => (l.res -> l.weights) }
    structure2.weights ++ predws
  }

  var cachedMarginals = false
  var cachedZs = false

  def reevaluateZ() {
    if (!cachedZs) {
      if(verbose) println("Reevaluating partition function circuit")
      val weights = getPredicateWeights()
      for ((db, z) <- zs) {
        z.clearCache
        z.cacheWmc(db.domainSizes, weights)
      }
      cachedZs = true
    }
  }

  def reevaluateQueryCircuits() {
    if (!cachedMarginals) {
      if(verbose) println("Reevaluating query circuits")
      val weights = getPredicateWeights()
      for (clause <- learnableClauses) clause.reevaluateQueryCircuits(weights)
      cachedMarginals = true
    }
  }

  def numOptimizableParameters: Int = learnableClauses.size

  lazy val logLikelihoodFunction = new DiffFunction[DenseVector[Double]] {

    // cache results to avoid repeated computations
    var lastWeights: DenseVector[Double] = null
    var lastResult: (Double, DenseVector[Double]) = null;

    def calculate(weights: DenseVector[Double]): (Double, DenseVector[Double]) = {
      if (weights == lastWeights) {
        if(verbose) println("Reusing computed likelihood and gradient for previous weights.")
        return lastResult;
      } else {
        updateParameters(weights.valuesIterator.toArray)
        if(verbose) println("Computing likelihood and gradient for weights:")
        if(verbose) println(getPredicateWeights())
        // negate value and gradient to maximize
        val value = -calculateObjective
        val gradient = -calculateGradient;
        lastWeights = weights.copy;
        lastResult = (value, gradient);
        return lastResult;
      }
    }

    def updateParameters(weights: Array[Double]) {
      for (i <- 0 until numOptimizableParameters) {
        learnableClauses(i).logWeight = weights(i)
      }
      cachedZs = false
      cachedMarginals = false
    }

    def calculateObjective: Double = {
      reevaluateZ()
      // We don't need to update the query circuits, they are only used in the gradient. 
      
      if(verbose) println("Computing likelihood")
      val totalLikelihood = trainDatabaseLikelihoods.map{ dbLh =>
        val likelihood = if(normalizeLH){
          dbLh.perVariableLikelihood
        }else{
          dbLh.likelihood()
        }
        if(verbose) println(s"  Database ${dbLh.db} has likelihood $likelihood (normalization = $normalizeLH)")
        likelihood
      }.reduce(_ * _)
      if(verbose) println(s"Likelihood of all db is $totalLikelihood")
      
      val totalPriorDensity = if(normalizeLH){
        perVariablePriorDensity()
      }else{
        priorDensity()
      }
      if(verbose) println(s"Prior density is $totalPriorDensity")
      val regularizedTotalLikelihood = totalLikelihood * totalPriorDensity
      if(verbose) println(s"Regularized likelihood of all db is $regularizedTotalLikelihood")
      if(verbose) println
      
      // convert to logspace double at the very end!
      regularizedTotalLikelihood.logToDouble
    }

    /** If argument is null, an array will be allocated for you and returned. */
    def calculateGradient(): DenseVector[Double] = {
      numGradientComputations+=1
      reevaluateZ()
      reevaluateQueryCircuits()
      
      if(verbose) println("Computing gradient")
      	
      val weights = new Array[Double](numOptimizableParameters)
      for (i <- 0 until numOptimizableParameters) {
	        val res = learnableClauses(i).res
	      val totalDerivativeLikelihood = trainDatabaseLikelihoods.map{ dbLh =>
	        val derivative = if(normalizeLH){
	          dbLh.gradientPerVariableLogLikelihood(i)
	        }else{
	          dbLh.gradientLogLikelihood(i)
	        }
	        if(verbose) println(s"  Database ${dbLh.db} has derivative $derivative towards weight of $res (normalization = $normalizeLH)")
	        derivative
	      }.reduce(_ + _)
	      if(verbose) println(s"Derivative of all db is $totalDerivativeLikelihood towards weight of $res")
	      
	      val derivativeLogPriorDensity = if(normalizeLH){
	        gradientPerVariableLogPriorDensity(i)
	      }else{
	        gradientLogPriorDensity(i)
	      }
	      if(verbose) println(s"Derivative of logprior is $derivativeLogPriorDensity towards weight of $res")
	      val derivativeLogRegularizedLikelihood = totalDerivativeLikelihood + derivativeLogPriorDensity
	      if(verbose) println(s"Derivative of regularized likelihood is $derivativeLogRegularizedLikelihood towards weight of $res")
	      weights(i) = derivativeLogRegularizedLikelihood
      }
      if(verbose) println
      
      return DenseVector(weights)
    }
  }

  var numGradientComputations = 0
  
  def learnParameters(): (MLN, Double) = {
    numGradientComputations = 0
    val learnedParameters = optimizer.minimize(logLikelihoodFunction, DenseVector.zeros(numOptimizableParameters))
    val ll = logLikelihoodFunction.calculate(learnedParameters)._1
    if(verbose) println("Final weight vector: " + learnedParameters);
    if(verbose) println("Final loglikelihood: " + ll);
    if(verbose) println
    (learnedMLN(), ll)
  }

  def learnedMLN(): MLN = {
    val learnedFormulas = learnableClauses.map { _.learnedFormula }
    structure.copy(wformulas = learnedFormulas.toList ++ hardFormulas)
  }

  /**
   * Index structure for pseudo likelihood.
   * @todo   Improve efficiency (only looks at pred for now)
   */
  lazy val predToLClause: immutable.HashMap[Predicate, List[LearningFormula]] = {
    val hash = new mutable.HashMap[Predicate, mutable.ListBuffer[LearningFormula]]
    learnableClauses.foreach { clause =>
      clause.mlnPredicates.foreach { pred =>
        hash.getOrElseUpdate(pred, new mutable.ListBuffer()).append(clause)
      }
    }
    immutable.HashMap(hash.map { case (k, v) => (k, v.toList) }.toSeq: _*)
  }

  /**
   * The pseudo likelihood per database
   *
   * The formula used is
   * LPLL = \sum_a -1\cdot\log(1 +e^{f_{a \in}(\bar{a})-f_{a \in}({a})})
   *
   * @todo Very inefficient at the moment (grounds), meant for comparisons.
   */
  def logPseudoLikelihood: List[Double] = logPseudoLikelihoodWithDbs(dbs.traindbs)
  def testLogPseudoLikelihood: List[Double] = logPseudoLikelihoodWithDbs(dbs.testdbs)

  def logPseudoLikelihoodWithDbs(dbs: IndexedSeq[Database]): List[Double] = {
    dbs.toList.zipWithIndex.map {
      case (db, idx) =>

        require(db.domainSizes.useExplicitConstants)

        //db.addAdditionalConstants()
        //println("domainSizes="+db.domainSizes)
        //var grndcnt = 0

        val lpll = structure.predicates.toList.map { pred =>
          val pc = pred.toAtom.toPositiveUnitClause
          // Could be optimized by shattering and using those atoms instead
          // of grounding everything.
          val groundings = pc.ground(db.domainSizes)
          //println("Grounding predicate %s (#%d)" format(pred.toString, groundings.size))

          predToLClause.get(pred) match {
            case Some(lclauses) => {
              val predlpll = groundings.map { grounding =>
                //println("Grounding: "+grounding)
                //grndcnt += 1
                val evidenceAtom = grounding.posLits.head
                val isPosEvidence = db.posEvidence.contains(evidenceAtom)
                val lnllp = lclauses.map { _.customCircuitsForDatabase(db, Some((evidenceAtom, true))).numeratorLikelihood }
                					.reduce { _ * _ }.logToDouble
                val lnlln = lclauses.map { _.customCircuitsForDatabase(db, Some((evidenceAtom, false))).numeratorLikelihood }
                					.reduce { _ * _ }.logToDouble
				// could this computation continue with SignLogDouble?
                val diff = if (isPosEvidence) {
                  lnlln - lnllp
                } else {
                  lnllp - lnlln
                }
                val lplla = -1 * math.log(1 + math.exp(diff))
                //println("lnllp = "+lnllp+" , lnlln = "+lnlln+" -> lplla = "+lplla)
                lplla
              }.reduce { _ + _ }
              if (normalizepll) {
                predlpll / groundings.size
              } else {
                predlpll
              }
            }
            case None => {
              // This is a predicate that does not appear in any formula.
              // Therefore, diff will always be zero.
              //-1*groundings.size*math.log(1+math.exp(0))
              val predlpll = -1 * math.log(2)
              if (normalizepll) {
                predlpll
              } else {
                predlpll * groundings.size
              }
            }
          }
        }.reduce { _ + _ }

        //db.removeAdditionalConstants()
        //println("#%d groundings" format grndcnt)

        //println("lpll = "+lpll)
        lpll
    }
  }
}
