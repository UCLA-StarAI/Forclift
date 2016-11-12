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

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.inference._
import compiler._

//object GroundSymmetricExperiment {
//
//  def main(args: Array[String]): Unit = {
//    val domainSizes = 2.to(200, 1)
//
//    import java.io._
//    val file = new File("data/GroundSymmetricExperiment.dat")
//    val writer = new java.io.PrintWriter(file)
//    writer.println("domainsize\t\tprob\t\ttotaltime")
//
//    for (domainSize <- domainSizes) {
//      val model = new models.SymmetricFriendsSmokerModel(domainSize, List("guy"))
//      val stats = LiftedInference.factorGraphInference(
//        model.theoryString,
//        "smokes(guy)", propositionalWmc = true, compiledBuilder = Compiler.Builder.default)
//        val row = domainSize + "\t\t" + stats.propositionalProb + "\t\t" + stats.propositionalTotalTime
//      println(stats)
//      writer.println(row)
//      writer.flush
//    }
//    writer.close
//  }
//
//}

//object IJCAI11SymmetricExperiment {
//
//  def main(args: Array[String]): Unit = {
//    val domainSizes = 61.to(200, 3)
//
//    val compilerBuilder = IJCAI11Compiler.builder
//
//    import java.io._
//    val file = new File("data/IJCAI11SymmetricExperiment.dat")
//    val writer = new java.io.PrintWriter(file)
//    writer.println("domainsize\t\tprob\t\tcompilationtime\t\tinferencetime\t\ttotaltime")
//
//    for (domainSize <- domainSizes) {
//      val model = new models.SymmetricFriendsSmokerModel(domainSize, List("guy"))
//      val stats = LiftedInference.factorGraphInference(
//        model.theoryString,
//        "smokes(guy)", compiledBuilder = IJCAI11Compiler.builder)
//      val row = domainSize + "\t\t" + stats.liftedProb + "\t\t" + stats.compilationTime + "\t\t" + stats.inferenceTime + "\t\t" + stats.totalTime
//      println(stats)
//      writer.println(row)
//      writer.flush
//    }
//    writer.close
//  }
//
//}

//object NIPS11SymmetricExperiment {
//
//  def main(args: Array[String]): Unit = {
//    val domainSizes = 2.to(300, 5)
//
//    val compilerBuilder = NIPS11Compiler.builder
//
//    import java.io._
//    val file = new File("data/NIPS11SymmetricExperiment.dat")
//    val writer = new java.io.PrintWriter(file)
//    writer.println("domainsize\t\tprob\t\tcompilationtime\t\tinferencetime\t\ttotaltime")
//
//    for (domainSize <- domainSizes) {
//      val model = new models.SymmetricFriendsSmokerModel(domainSize, List("guy"))
//      val stats = LiftedInference.factorGraphInference(
//        model.theoryString,
//        "smokes(guy)", compiledBuilder = compilerBuilder)
//      val row = domainSize + "\t\t" + stats.liftedProb + "\t\t" + stats.compilationTime + "\t\t" + stats.inferenceTime + "\t\t" + stats.totalTime
//      println(stats)
//      writer.println(row)
//      writer.flush
//    }
//    writer.close
//  }
//
//}

//object SymmetricFriendsSmokerWithEvidenceExample {
//  
//	def main(args : Array[String]) : Unit = {
//		
//		val model = new models.SymmetricFriendsSmokerModel(10,List("guy","luc","bert"),List("friends(guy,luc)", "smokes(luc)")) 
//		
//		LiftedInference.factorGraphInference(
//				model.theoryString,
//				"smokes(guy)"
////				,maxDepth = 13
////				,showTheorySmoothNnf = true
//				,propositionalWmc = true
////				,verifyLogWmc = true
//				
//		)
//		println
//		LiftedInference.factorGraphInference(
//				model.theoryString,
//				"smokes(bert)"
//				,propositionalWmc = true
////				,verifyLogWmc = true
//		)
//		
//	}
//
//}
