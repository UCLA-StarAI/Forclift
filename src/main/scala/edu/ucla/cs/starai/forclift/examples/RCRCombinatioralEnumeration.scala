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

package edu.ucla.cs.starai.forclift.examples

import java.io._
import scala.language.postfixOps
import scala.util.Random._
import edu.ucla.cs.starai.forclift.compiler._
import edu.ucla.cs.starai.forclift.examples.models._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.rcr.Compensation
import edu.ucla.cs.starai.forclift.rcr.MarginalCircuitsSet
import edu.ucla.cs.starai.forclift.rcr.RelaxCompensateRecover
import scala.sys.process.Process

object RCRCombinatorialEnumeration {

  def main(args: Array[String]): Unit = {
    //
    //        val theoryr(Y,Z)tring =
    //          """d = {X1,X2,X3}
    //q
    //r(X,Y)d,d
    //1 q <=> r(X,Y)x,y ^ r(X,Y)y,z <=> r(X,Y)x,z"""
    //    
    //        val parser = new MLNParser
    //        val theory = parser.parseMLNtheoryr(Y,Z)tring.toWeightedCNf(X,Y,Z)false
    //        printlntheory
    //
    /* def model(n: Int) = new WeightedCNFModel {
      def theoryString = "domain D " + n + """ {x1,x2,x3}
predicate q 1.0 1.0
predicate r(D,D) 1.0 1.0
predicate f_{1}(D,D,D) 1.0 0.0
    """
    }*/

    val trueModelCount = Array[Double](1, 2, 13, 171, 3994, 154303, 9415189, 878222530, 122207703623.0, 24890747921947.0, 7307450299510288.0,
      3053521546333103057.0, 1797003559223770324237.0, 1476062693867019126073312.0,
      1679239558149570229156802997.0,
      2628225174143857306623695576671.0,
      5626175867513779058707006016592954.0,
      16388270713364863943791979866838296851.0,
      64662720846908542794678859718227127212465.0)

    //                def model(n: Int) = new WeightedCNFModel {
    //                    def theoryString = "domain D " + n + """ {x1,x2,x3}
    //        predicate r(D,D) 1.0 1.0
    //        predicate f_{1}(D,D,D) 1.0 0.0
    //        ¬r(X,Y) v ¬r(Y,Z) v r(X,Z) v ¬f_{1}(X,Y,Z)
    //        r(X,Y) v f_{1}(X,Y,Z)
    //        r(Y,Z) v f_{1}(X,Y,Z)
    //        ¬r(X,Z) v f_{1}(X,Y,Z)
    //            """
    //                }
    //        def model(n: Int) = new WeightedCNFModel {
    //            def theoryString = "domain D " + n + """ {x1,x2,x3}
    //predicate q 1.0 1.0
    //predicate r(D,D) 1.0 1.0
    //predicate f_{1}(D,D,D) 1 0
    //¬f_{1}(X,Y,Z) v ¬q v ¬r(X,Y) v ¬r(Y,Z) v r(X,Z)
    //¬f_{1}(X,Y,Z) v q v r(X,Y)
    //¬f_{1}(X,Y,Z) v q v r(Y,Z)
    //¬f_{1}(X,Y,Z) v q v ¬r(X,Z)
    //f_{1}(X,Y,Z) v ¬q v r(X,Y)
    //f_{1}(X,Y,Z) v ¬q v r(Y,Z)
    //f_{1}(X,Y,Z) v ¬q v ¬r(X,Z)
    //f_{1}(X,Y,Z) v q v ¬r(X,Y) v ¬r(Y,Z) v r(X,Z)
    //    """
    //        }
    def model(n: Int) = new WeightedCNFModel {
      def theoryString = "domain D " + n + """ {x1,x2,x3}
predicate q 1.0 1.0
predicate r(D,D) 1.0 1.0
predicate f_{1}(D,D,D) 1 0
¬f_{1}(X,Y,Z) v ¬q v ¬r(X,Y) v ¬r(Y,Z) v r(X,Z)
f_{1}(X,Y,Z) v q
f_{1}(X,Y,Z) v r(X,Y)
f_{1}(X,Y,Z) v r(Y,Z)
f_{1}(X,Y,Z) v ¬r(X,Z)
    """
    }

    // 

    for (n <- 3 to 10) {

      println("Nb elements = " + n)
      println("True model count = exp(" + math.log(trueModelCount(n)) + ") = " + trueModelCount(n))
      println("P(transitive) = exp(" + (math.log(trueModelCount(n)) - math.log(math.pow(2, n * n))) + ") = " + trueModelCount(n) / math.pow(2, n * n))
      println("P(q => transitive) = " + trueModelCount(n) / (trueModelCount(n) + math.pow(2, n * n)))
      println

      val wmc = model(n).theory

      val rootDir = new File("experiments/rcr-combenum/test/")
      rootDir.mkdir()

      def replot() {
        import scala.sys.process._
        println(Process("sh makeallplots.sh", rootDir)!!)
      }

      val dir = new File(rootDir, "t")
      dir.mkdir()

      //        val rcr = new LoggingGroundTruthRCR(wmc, dir, Compiler.Builder.default, Compiler.Builder.defaultWithGrounding, true) {
      //
      //            override def onEndCompensation(
      //                weights: PredicateWeights,
      //                compensations: List[Compensation],
      //                marginalCircuitsSet: MarginalCircuitsSet) {
      //                super.onEndCompensation(
      //                    weights: PredicateWeights,
      //                    compensations: List[Compensation],
      //                    marginalCircuitsSet: MarginalCircuitsSet)
      //                replot()
      //            }
      //
      //        }
      val consoleLog = new PrintWriter(new FileWriter(new File(dir, "t" + 3)));
      val rcr = new RelaxCompensateRecover(wmc, Compiler.Builder.default, false) {

        override def println(str: Object) {
          super.println(str)
          consoleLog.println(str)
          consoleLog.flush
        }

        override def print(str: Object) {
          super.print(str)
          consoleLog.print(str)
          consoleLog.flush
        }

        override def onEndCompensation(
          weights: PredicateWeights,
          compensations: List[Compensation],
          marginalCircuitsSet: MarginalCircuitsSet) {
          super.onEndCompensation(weights, compensations, marginalCircuitsSet)
          val qMarginal = marginalCircuitsSet.origMarginals.find { _.queryClass.atom.predicate.toString() == "q" }.get
          val qP = qMarginal.marginal.toDouble
          val N = math.pow(2, n * n)
          val nbTransitiveApprox = qP * N / (1 - qP)
          println("NB RELAXED EQUIVALENCES = " + compensations.map { _.eq.nbGroundEquivalences }.sum)
          println("NB TRANSITIVE RELATIONS ESTIMATE = " + nbTransitiveApprox)
          println("ERROR = " + (nbTransitiveApprox - trueModelCount(n)).abs)
        }

      }
      val weights = rcr.compensateFullRelaxationAndRecover()
      //        consoleLog.close()

      //        rcr.closeAll

      //        println("TOTAL RUNTIME: " + ((System.currentTimeMillis - rcr.start) / 1000.0F) + "s")

      //        println
      //        println("Weight Function")
      //        println(weights)
      //        println
    }

  }
}
