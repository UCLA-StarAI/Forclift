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

trait ConditioningSmokersExperiment {
  val nbPeople = 50
  val step = 2
}

object ConditioningSmokersExperiment1 extends ConditioningSmokersExperiment {

  def main(args: Array[String]): Unit = {
    runExperiment(2)
    runExperiment(2)
    runExperiment(2)
  }

  def runExperiment(number: Int) {
    def pad(s: Any) = s.toString.padTo(25, " ").mkString
    def time = System.currentTimeMillis();

    val people = (1 to nbPeople).map { i: Int => "p" + i.toString }

    val model = new FriendsSmokerModel(nbPeople, people.toSeq, Nil)
    val smokes = model.catom("smokes(P)")
    val cancer = model.catom("cancer(P)")

    val wmc = model.theory
    val startTheoryModification = time
    val (parWmc, parDomains) = wmc.conditionablePartial(List(smokes, cancer))
    val theoryModificationTime = time - startTheoryModification

    val file = new java.io.PrintWriter("experiments/smokersexperiment-conditioned-" + number + ".dat")
    def output(str: String) {
      println(str)
      file.println(str)
      file.flush
    }
    output(pad("%evidence") +
      pad("theoryModificationTime") +
      pad("compilationTime") +
      pad("inferenceTime") +
      pad("totalTime") +
      pad("Z"))
    try {
      for (evidence <- 0.to(nbPeople, step)) yield {
        val smokers = shuffle(people).take((evidence / 2.0).ceil.toInt)
        val smokersEvidence = smokers.map { smoker => model.catom("smokes(" + smoker + ")") }.toSet

        val nonSmokers = shuffle(people.toList filterNot (smokers.toList.contains(_))).take(evidence / 2)
        val nonSmokersEvidence = nonSmokers.map { p => model.catom("!smokes(" + p + ")") }.toSet

        val posCancer = shuffle(people).take((evidence / 2.0).ceil.toInt)
        val posCancerEvidence = posCancer.map { p => model.catom("cancer(" + p + ")") }.toSet

        val negCancer = shuffle(people.toList filterNot (posCancer.toList.contains(_))).take(evidence / 2)
        val negCancerEvidence = negCancer.map { p => model.catom("!cancer(" + p + ")") }.toSet

        val posEvidence = posCancerEvidence ++ smokersEvidence
        val negEvidence = negCancerEvidence ++ nonSmokersEvidence
        assume((posEvidence ++ negEvidence).size == 2 * evidence)

        val domainElems2 = parDomains.addEvidence(parWmc.domainSizes, posEvidence, negEvidence)
        val parWmc2 = parWmc.addDomainSizes(ConditioningDomainSet.domainElements2SubDomainSizes(domainElems2))

        val startCompilation = time
        parWmc2.smoothNnf
        val compilationTime = time - startCompilation

        val startInference = time
        val Z = parWmc2.logSmoothWmc
        val inferenceTime = time - startInference

        val str = pad((evidence * 100F / nbPeople)) +
          pad((theoryModificationTime / 1000F)) +
          pad((compilationTime / 1000F)) +
          pad((inferenceTime / 1000F)) +
          pad(((theoryModificationTime + compilationTime + inferenceTime) / 1000F)) +
          pad(Z.logToDouble.toFloat)

        output(str)
      }
    } finally { file.close() }

  }

}

object ConditioningSmokersExperiment2 extends ConditioningSmokersExperiment {

  def main(args: Array[String]): Unit = {
    runExperiment(2, true)
  }

  def runExperiment(number: Int, direction: Boolean) {
    def pad(s: Any) = s.toString.padTo(25, " ").mkString
    def time = System.currentTimeMillis();

    val people = (1 to nbPeople).map { i: Int => "p" + i.toString }

    val model = new FriendsSmokerModel(nbPeople, people.toSeq, Nil)

    val file = new java.io.PrintWriter("experiments/smokersexperiment-naive-" + number ++ "-" + direction + ".dat")
    def output(str: String) {
      println(str)
      file.println(str)
      file.flush
    }
    output(pad("%evidence") +
      pad("compilationTime") +
      pad("inferenceTime") +
      pad("totalTime") +
      pad("Z"))
    val iterations = if (direction) 0.to(nbPeople, step) else nbPeople.to(0, -step)
    try {
      for (evidence <- iterations) yield {
        val smokers = shuffle(people).take((evidence / 2.0).ceil.toInt)
        val smokersEvidence = smokers.map { smoker => model.catom("smokes(" + smoker + ")") }.toSet

        val nonSmokers = shuffle(people.toList filterNot (smokers.toList.contains(_))).take(evidence / 2)
        val nonSmokersEvidence = nonSmokers.map { p => model.catom("!smokes(" + p + ")") }.toSet

        val posCancer = shuffle(people).take((evidence / 2.0).ceil.toInt)
        val posCancerEvidence = posCancer.map { p => model.catom("cancer(" + p + ")") }.toSet

        val negCancer = shuffle(people.toList filterNot (posCancer.toList.contains(_))).take(evidence / 2)
        val negCancerEvidence = negCancer.map { p => model.catom("!cancer(" + p + ")") }.toSet

        val posEvidence = posCancerEvidence ++ smokersEvidence
        val negEvidence = negCancerEvidence ++ nonSmokersEvidence
        val interpretation = posEvidence ++ negEvidence.map { _.toNegativeUnitClause }
        assume((interpretation).size == 2 * evidence)

        val naiveModel = new FriendsSmokerModel(nbPeople, people.toSeq, interpretation.map { _.toString }.toSeq)
        val wmc = naiveModel.theory

        val startCompilation = time
        wmc.smoothNnf
        val compilationTime = time - startCompilation

        val startInference = time
        val Z = wmc.logSmoothWmc
        val inferenceTime = time - startInference

        val str = pad((evidence * 100F / nbPeople)) +
          pad((compilationTime / 1000F)) +
          pad((inferenceTime / 1000F)) +
          pad(((compilationTime + inferenceTime) / 1000F)) +
          pad(Z.logToDouble.toFloat)

        output(str)
      }
    } finally { file.close() }

  }

}
object ConditioningSmokersExperiment3 extends ConditioningSmokersExperiment {

  def main(args: Array[String]): Unit = {
    runExperiment(2, false)
  }

  def runExperiment(number: Int, direction: Boolean) {
    def pad(s: Any) = s.toString.padTo(25, " ").mkString
    def time = System.currentTimeMillis();

    val people = (1 to nbPeople).map { i: Int => "p" + i.toString }

    val model = new FriendsSmokerModel(nbPeople, people.toSeq, Nil)

    val file = new java.io.PrintWriter("experiments/smokersexperiment-ground-" + number ++ "-" + direction + ".dat")
    def output(str: String) {
      println(str)
      file.println(str)
      file.flush
    }
    output(pad("%evidence") +
      pad("totalTime") +
      pad("Z"))
    val iterations = if (direction) 0.to(nbPeople, step) else nbPeople.to(0, -step)
    try {
      for (evidence <- iterations) yield {
        val smokers = shuffle(people).take((evidence / 2.0).ceil.toInt)
        val smokersEvidence = smokers.map { smoker => model.catom("smokes(" + smoker + ")") }.toSet

        val nonSmokers = shuffle(people.toList filterNot (smokers.toList.contains(_))).take(evidence / 2)
        val nonSmokersEvidence = nonSmokers.map { p => model.catom("!smokes(" + p + ")") }.toSet

        val posCancer = shuffle(people).take((evidence / 2.0).ceil.toInt)
        val posCancerEvidence = posCancer.map { p => model.catom("cancer(" + p + ")") }.toSet

        val negCancer = shuffle(people.toList filterNot (posCancer.toList.contains(_))).take(evidence / 2)
        val negCancerEvidence = negCancer.map { p => model.catom("!cancer(" + p + ")") }.toSet

        val posEvidence = posCancerEvidence ++ smokersEvidence
        val negEvidence = negCancerEvidence ++ nonSmokersEvidence
        val interpretation = posEvidence ++ negEvidence.map { _.toNegativeUnitClause }
        assume((interpretation).size == 2 * evidence)

        val naiveModel = new FriendsSmokerModel(nbPeople, people.toSeq, interpretation.map { _.toString }.toSeq)
        val wmc = naiveModel.theory

        val start = time
        val Z = wmc.logSmoothPropWmc
        val runtime = time - start

        val str = pad((evidence * 100F / nbPeople)) +
          pad((runtime / 1000F)) +
          pad(Z.logToDouble.toFloat)

        output(str)
      }
    } finally { file.close() }

  }

}

object ConditioningSmokersExperiment4 {

  def main(args: Array[String]): Unit = {
    runExperiment(1)
    runExperiment(1)
  }

  def runExperiment(number: Int) {
    def pad(s: Any) = s.toString.padTo(25, " ").mkString
    def time = System.currentTimeMillis();

    val file = new java.io.PrintWriter("experiments/smokersexperiment2-conditioned-" + number + ".dat")
    def output(str: String) {
      println(str)
      file.println(str)
      file.flush
    }
    output(pad("nbPeople") +
      pad("theoryModificationTime") +
      pad("compilationTime") +
      pad("inferenceTime") +
      pad("totalTime") +
      pad("Z"))
    try {
      for (nbPeople <- 1.to(100, 2)) yield {

        val people = (1 to nbPeople).map { i: Int => "p" + i.toString }

        val model = new FriendsSmokerModel(nbPeople, people.toSeq, Nil)
        val smokes = model.catom("smokes(P)")
        val cancer = model.catom("cancer(P)")

        val wmc = model.theory
        val startTheoryModification = time
        val (parWmc, parDomains) = wmc.conditionablePartial(List(smokes, cancer))
        val theoryModificationTime = time - startTheoryModification

        val smokers = shuffle(people).take(nbPeople / 4)
        val smokersEvidence = smokers.map { smoker => model.catom("smokes(" + smoker + ")") }.toSet

        val nonSmokers = shuffle(people.toList filterNot (smokers.toList.contains(_))).take(nbPeople / 4)
        val nonSmokersEvidence = nonSmokers.map { p => model.catom("!smokes(" + p + ")") }.toSet

        val posCancer = shuffle(people).take(nbPeople / 4)
        val posCancerEvidence = posCancer.map { p => model.catom("cancer(" + p + ")") }.toSet

        val negCancer = shuffle(people.toList filterNot (posCancer.toList.contains(_))).take(nbPeople / 4)
        val negCancerEvidence = negCancer.map { p => model.catom("!cancer(" + p + ")") }.toSet

        val posEvidence = posCancerEvidence ++ smokersEvidence
        val negEvidence = negCancerEvidence ++ nonSmokersEvidence

        val domainElems2 = parDomains.addEvidence(parWmc.domainSizes, posEvidence, negEvidence)
        val parWmc2 = parWmc.addDomainSizes(ConditioningDomainSet.domainElements2SubDomainSizes(domainElems2))

        val startCompilation = time
        parWmc2.smoothNnf
        val compilationTime = time - startCompilation

        val startInference = time
        val Z = parWmc2.logSmoothWmc
        val inferenceTime = time - startInference

        val str = pad((nbPeople)) +
          pad((theoryModificationTime / 1000F)) +
          pad((compilationTime / 1000F)) +
          pad((inferenceTime / 1000F)) +
          pad(((theoryModificationTime + compilationTime + inferenceTime) / 1000F)) +
          pad(Z.logToDouble.toFloat)

        output(str)
      }
    } finally { file.close() }

  }

}

object ConditioningSmokersExperiment5 {

  def main(args: Array[String]): Unit = {
    runExperiment(2)
  }

  def runExperiment(number: Int) {
    def pad(s: Any) = s.toString.padTo(25, " ").mkString
    def time = System.currentTimeMillis();

    val file = new java.io.PrintWriter("experiments/smokersexperiment2-naive-" + number + ".dat")
    def output(str: String) {
      println(str)
      file.println(str)
      file.flush
    }
    output(pad("nbPeople") +
      pad("compilationTime") +
      pad("inferenceTime") +
      pad("totalTime") +
      pad("Z"))
    try {
      for (nbPeople <- 1.to(100, 2)) yield {

        val people = (1 to nbPeople).map { i: Int => "p" + i.toString }

        val model = new FriendsSmokerModel(nbPeople, people.toSeq, Nil)
        val smokers = shuffle(people).take(nbPeople / 4)
        val smokersEvidence = smokers.map { smoker => model.catom("smokes(" + smoker + ")") }.toSet

        val nonSmokers = shuffle(people.toList filterNot (smokers.toList.contains(_))).take(nbPeople / 4)
        val nonSmokersEvidence = nonSmokers.map { p => model.catom("!smokes(" + p + ")") }.toSet

        val posCancer = shuffle(people).take(nbPeople / 4)
        val posCancerEvidence = posCancer.map { p => model.catom("cancer(" + p + ")") }.toSet

        val negCancer = shuffle(people.toList filterNot (posCancer.toList.contains(_))).take(nbPeople / 4)
        val negCancerEvidence = negCancer.map { p => model.catom("!cancer(" + p + ")") }.toSet

        val posEvidence = posCancerEvidence ++ smokersEvidence
        val negEvidence = negCancerEvidence ++ nonSmokersEvidence
        val interpretation = posEvidence ++ negEvidence.map { _.toNegativeUnitClause }

        val naiveModel = new FriendsSmokerModel(nbPeople, people.toSeq, interpretation.map { _.toString }.toSeq)
        val wmc = naiveModel.theory

        val startCompilation = time
        wmc.smoothNnf
        val compilationTime = time - startCompilation

        val startInference = time
        val Z = wmc.logSmoothWmc
        val inferenceTime = time - startInference

        val str = pad((nbPeople)) +
          pad((compilationTime / 1000F)) +
          pad((inferenceTime / 1000F)) +
          pad(((compilationTime + inferenceTime) / 1000F)) +
          pad(Z.logToDouble.toFloat)

        output(str)
      }
    } finally { file.close() }

  }

}
object ConditioningSmokersExperiment6 {

  def main(args: Array[String]): Unit = {
    runExperiment(1)
  }

  def runExperiment(number: Int) {
    def pad(s: Any) = s.toString.padTo(25, " ").mkString
    def time = System.currentTimeMillis();

    val file = new java.io.PrintWriter("experiments/smokersexperiment2-ground-" + number + ".dat")
    def output(str: String) {
      println(str)
      file.println(str)
      file.flush
    }
    output(pad("nbPeople") +
      pad("totalTime") +
      pad("Z"))
    try {
      for (nbPeople <- 1.to(100, 2)) yield {

        val people = (1 to nbPeople).map { i: Int => "p" + i.toString }

        val model = new FriendsSmokerModel(nbPeople, people.toSeq, Nil)
        val smokers = shuffle(people).take(nbPeople / 4)
        val smokersEvidence = smokers.map { smoker => model.catom("smokes(" + smoker + ")") }.toSet

        val nonSmokers = shuffle(people.toList filterNot (smokers.toList.contains(_))).take(nbPeople / 4)
        val nonSmokersEvidence = nonSmokers.map { p => model.catom("!smokes(" + p + ")") }.toSet

        val posCancer = shuffle(people).take(nbPeople / 4)
        val posCancerEvidence = posCancer.map { p => model.catom("cancer(" + p + ")") }.toSet

        val negCancer = shuffle(people.toList filterNot (posCancer.toList.contains(_))).take(nbPeople / 4)
        val negCancerEvidence = negCancer.map { p => model.catom("!cancer(" + p + ")") }.toSet

        val posEvidence = posCancerEvidence ++ smokersEvidence
        val negEvidence = negCancerEvidence ++ nonSmokersEvidence
        val interpretation = posEvidence ++ negEvidence.map { _.toNegativeUnitClause }

        val naiveModel = new FriendsSmokerModel(nbPeople, people.toSeq, interpretation.map { _.toString }.toSeq)
        val wmc = naiveModel.theory

        val start = time
        val Z = wmc.logSmoothPropWmc
        val runtime = time - start

        val str = pad(nbPeople) +
          pad((runtime / 1000F)) +
          pad(Z.logToDouble.toFloat)

        output(str)
      }
    } finally { file.close() }

  }

}
