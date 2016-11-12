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

import java.io.File
import util.Random
import java.io.PrintWriter
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.learning.LiftedLearning
import edu.ucla.cs.starai.forclift.languages.mln.MLNParser
import java.io.OutputStream
import java.io.PrintStream

object MljSmokersExperiment extends App {

  val dir = new File("experiments/mlj-smokers")
  dir.mkdirs()

  val data = new PrintWriter(new File(dir, "timings.dat"))
  data.printf("%10s %10s %10s %10s %10s %10s\n", "DomSize", "GradCalls", 
      "CountTime", "CompTime", "InfTime", "LearnTime")

  
  for (size <- List(30000,2,2,2,2, 3, 4, 10, 20, 50, 100, 500, 1000, 5000, 10000, 15000, 20000, 25000, 30000)) {
    println(s"Domain size is $size")

    val structStr =
s"""
person = $size {}
Smokes(person)
Friends(person,person)

!Friends(x,y) v !Smokes(x) v Smokes(y)
"""
  
    val structFile = new PrintWriter(new File(dir, s"struct-$size.mln"))
    structFile.println(structStr)
    structFile.close

    val r = new Random
    val trainingDBStr = {
      val people = (1 to size)
      // generate smokers
      val smokers = r.shuffle(people).take((size*0.4).toInt)
      val nonsmokers = people diff smokers
      val smokesDb = smokers.map(s => {s"Smokes(P$s)"}).toSet
      // generate friends
      val friendsOfSmokersDb = smokers.flatMap{s =>
        // create sparse database 
      	val nbFriends = 1+r.nextInt(3)
      	for(i <- 1 to nbFriends) yield {
      	  if(smokers.nonEmpty && r.nextDouble<0.6 || nonsmokers.isEmpty ){
      	    val friend = smokers(r.nextInt(smokers.size))
      	    s"Friends(P$s,P$friend)"
      	  }else{
      	    val friend = nonsmokers(r.nextInt(nonsmokers.size))
      	    s"Friends(P$s,P$friend)"
      	  }
      	}
      }.toSet
      val friendsOfNonSmokersDb = nonsmokers.flatMap{s =>
      	val nbFriends = 1+r.nextInt(2)
      	for(i <- 1 to nbFriends) yield {
      	  if(smokers.nonEmpty && r.nextDouble<0.35  || nonsmokers.isEmpty){
      	    val friend = smokers(r.nextInt(smokers.size))
      	    s"Friends(P$s,P$friend)"
      	  }else{
      	    val friend = nonsmokers(r.nextInt(nonsmokers.size))
      	    s"Friends(P$s,P$friend)"
      	  }
      	}
      }.toSet
      smokesDb.mkString("\n") + friendsOfSmokersDb.mkString("\n") + friendsOfNonSmokersDb.mkString("\n")
    }

    println(s"Database has ${trainingDBStr.lines.size} facts")
//    val dbFile = new PrintWriter(new File(dir, s"db-$size.db"))
//    dbFile.println(trainingDBStr)
//    dbFile.close
    
    
    val parser = new MLNParser
    parser.isLearnModus = true
    val structure = parser.parseMLN(structStr)
    val db = parser.parseDB(trainingDBStr)

    val start = System.currentTimeMillis()
    val learner = new LiftedLearning(structure, Seq(db), verbose = true, skolemize=false)
    val learnedMLN = learner.learnParameters()
    val learningTime = System.currentTimeMillis() - start
    val compileTime = learner.compileTime
    val countTime = learner.countTime
    val inferenceTime = learningTime-learner.countTime
    val numGradientComputations = learner.numGradientComputations
    
    data.println(f"$size%10s $numGradientComputations%10s $countTime%10s $compileTime%10s $inferenceTime%10s $learningTime%10s")
    data.flush
    println(f"$size%10s $numGradientComputations%10s $countTime%10s $compileTime%10s $inferenceTime%10s $learningTime%10s")
    
  }

  data.close
  println("done")
}

//private class NullOutputStream extends OutputStream {
//    override def write(b: Int){
//         return;
//    }
//    override def write(b: Array[Byte]){
//         return;
//    }
//    override def write(b: Array[Byte], off: Int, len: Int){
//         return;
//    }
//}
