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
import edu.ucla.cs.starai.forclift.compiler._

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.examples.models._
import edu.ucla.cs.starai.forclift.examples.models.mln._
import edu.ucla.cs.starai.forclift.inference._
import scala.util.Random._


import java.io._

object TestRelaxCompensate {

  def main(args: Array[String]): Unit = {

    /*

    val keepParams = true

    //TODO relax equivalences by increasing arities!!!!!!!!!!!!!!!!!
    // only useful when there are unary atoms in clauses with 3 vars!

    //TODO compare to closed form equation for number of transitive relations!!!

    val nbPeople = 200

//    val wmc = (new OriginalFriendsSmokerModel(200)).theory
    val wmc = (new BuiModel(3)).theory

    //    println("Nb ground atoms = "+ wmc.groundCnf.atoms.filter{!_.predicate.name.name.startsWith("f_")}.size)
    //    println("Nb ground factors = "+ wmc.groundCnf.atoms.filter{_.predicate.name.name.startsWith("f_")}.size)

//    val dir = new File("experiments/rcr/test/smokers-" + nbPeople + "-" + keepParams)
    val dir = new File("experiments/rcr/test/bui-" + nbPeople + "-" + keepParams)
    dir.mkdir()
//    val rcr = new LoggingGroundTruthRCR(wmc, dir, Compiler.Builder.default, Compiler.Builder.defaultWithGrounding, true)
    val rcr = new LoggingGroundTruthRCR(wmc, dir, Compiler.Builder.default, GroundCompiler.builder, true)

    val weights = rcr.compensateFullRelaxationAndRecover(keepParams = keepParams, precision = 0.001)

    rcr.closeAll()

    println("TOTAL RUNTIME: " + ((System.currentTimeMillis() - rcr.start) / 1000.0F) + "s")

    println
    println("Weight Function")
    println(weights)
    println

    */
  }
}
