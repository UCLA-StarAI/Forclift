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
import scala.language.postfixOps
import edu.ucla.cs.starai.forclift.compiler._

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.examples.models._
import edu.ucla.cs.starai.forclift.examples.models.mln._
import edu.ucla.cs.starai.forclift.inference._
import scala.util.Random._


import java.io._

object RelaxCompensateExperimentExact {

  def main(args: Array[String]): Unit = {

    val rootDir = new File("experiments/rcr/test/")
    rootDir.mkdir()

    def replot() {
      import scala.sys.process._
      println(Process("sh makeallplots.sh", rootDir)!!)
    }

    //TODO check OriginalSymmetricFriendsSmokerModel correct rewrite
    //TODO increase precision!!!!! to avoid convergence at step 0 do minimal number of steps???? do reset of params?
    val experiments = List(
      //      ("braz2-10", (new Braz2Model(10)).theory), //too long
      //      ("braz2-30", (new Braz2Model(30)).theory), //too long
      //      ("sickdeath-5", (new SickDeathModel(5)).theory),
      //      ("sickdeath-20", (new SickDeathModel(20)).theory),
      //      ("competingworkshops-10-3", (new CompetingWorkshopsModel(10, 3)).theory),
      //      ("competingworkshops-200-10", (new CompetingWorkshopsModel(200, 10)).theory),
      //      ("workshopattributes-5", (new Workshop6AttributesModel(5)).theory),
      //      ("workshopattributes-50", (new Workshop6AttributesModel(50)).theory),
      //      ("smokers-10", (new OriginalFriendsSmokerModel(10)).theory),
      //      ("smokers-100", (new OriginalFriendsSmokerModel(100)).theory),
      //      ("smokersdrinkers-10", (new OriginalFriendsSmokerDrinkerModel(10)).theory), //too long
      //      ("smokersdrinkers-50", (new OriginalFriendsSmokerDrinkerModel(50)).theory), //too long
      ("smokerssymmetric-10", (new OriginalSymmetricFriendsSmokerModel(10)).theory), //too long
      ("smokerssymmetric-50", (new OriginalSymmetricFriendsSmokerModel(50)).theory), //too long
      ("webkb-5", (new WebKBModel(5)).theory),
      ("webkb-20", (new WebKBModel(20)).theory))

    for ((name, wmc) <- experiments) {

      val dir = new File(rootDir, name)
      dir.mkdir()

      //      val groundingsDir = new File("experiments/rcr/groundings/")
      //      groundingsDir.mkdir()
      //      MLNGrounder.ground(wmc,groundingsDir,name)

      val rcr = new LoggingGroundTruthRCR(wmc, dir,
        Compiler.Builder.default,
        Compiler.Builder.default, true) {

        override def onEndCompensation(
          weights: PredicateWeights,
          compensations: List[Compensation],
          marginalCircuitsSet: MarginalCircuitsSet) {
          super.onEndCompensation(
            weights: PredicateWeights,
            compensations: List[Compensation],
            marginalCircuitsSet: MarginalCircuitsSet)
          replot()
        }

      }

      val weights = rcr.compensateFullRelaxationAndRecover(keepParams = true, damping = 0.5)

      rcr.closeAll()

      replot()
    }
  }

}

object RelaxCompensateExperimentApprox {

  def main(args: Array[String]): Unit = {

    val rootDir = new File("experiments/rcr/test/")
    rootDir.mkdir()

    def replot() {
      import scala.sys.process._
      println(Process("sh makeallplots.sh", rootDir)!!)
    }

    def model(n: Int) = new WeightedCNFModel {
      def theoryString = "domain D " + n + """ {a,b,c}
predicate r(D,D) 1.0 1.0
predicate f_{1}(D,D,D) 1 0
¬f_{1}(X,Y,Z) v ¬r(X,Y) v ¬r(Y,Z) v r(X,Z)
f_{1}(X,Y,Z) v r(X,Y)
f_{1}(X,Y,Z) v r(Y,Z)
f_{1}(X,Y,Z) v ¬r(X,Z)
    """
    }

    //TODO check OriginalSymmetricFriendsSmokerModel correct rewrite
    //TODO increase precision!!!!! to avoid convergence at step 0 do minimal number of steps???? do reset of params?
    val experiments = List(
      ("tfriends-3", (model(3)).theory),
      ("tfriends-10", (model(10)).theory),
      ("tsmokers-3", (new TransitiveFriendsSmokerModel(3)).theory),
      ("tsmokersdrinkers-3", (new TransitiveFriendsSmokerDrinkerModel(3)).theory),
      ("tsmokerssymmetric-3", (new TransitiveSymmetricFriendsSmokerModel(3)).theory),
      ("tsmokers-10", (new TransitiveFriendsSmokerModel(10)).theory),
      ("tsmokersdrinkers-10", (new TransitiveFriendsSmokerDrinkerModel(10)).theory),
      ("tsmokerssymmetric-10", (new TransitiveSymmetricFriendsSmokerModel(10)).theory))

    val exactCompiler = GroundCompiler.builder

    for ((name, wmc) <- experiments) {

      val dir = new File(rootDir, name)
      dir.mkdir()

      //      val groundingsDir = new File("experiments/rcr/groundings/")
      //      groundingsDir.mkdir()
      //      MLNGrounder.ground(wmc,groundingsDir,name)

      val rcr = new LoggingGroundTruthRCR(wmc, dir,
        Compiler.Builder.default,
        exactCompiler, true) {

        override def onEndCompensation(
          weights: PredicateWeights,
          compensations: List[Compensation],
          marginalCircuitsSet: MarginalCircuitsSet) {
          super.onEndCompensation(
            weights: PredicateWeights,
            compensations: List[Compensation],
            marginalCircuitsSet: MarginalCircuitsSet)
          replot()
        }

      }

      val weights = rcr.compensateFullRelaxationAndRecover(keepParams = true, damping = 0.5)

      rcr.closeAll()

      replot()
    }
  }

}
