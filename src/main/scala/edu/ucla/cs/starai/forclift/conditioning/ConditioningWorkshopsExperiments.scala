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

import java.util.Date

import scala.util.Random._

import edu.ucla.cs.starai.forclift.examples.models._
import edu.ucla.cs.starai.forclift._

trait ConditioningWorkshopsExperiment {
  val nbPeople = 1000
  val nbWorkshops = 100
  val step = 15
}

object ConditioningWorkshopsExperiment1 extends ConditioningWorkshopsExperiment {

  def main(args: Array[String]): Unit = {
    runExperiment(1)
    runExperiment(1)
    runExperiment(1)
  }

  def runExperiment(number: Int) {
    def pad(s: Any) = s.toString.padTo(25, " ").mkString
    def time = System.currentTimeMillis();

    val people = (1 to nbPeople).map { i: Int => "p" + i.toString }
    val workshops = (1 to nbWorkshops).map { i: Int => "w" + i.toString }

    val model = new CompetingWorkshopsModel(nbPeople, nbWorkshops, people.toSeq, Nil, Nil)
    val attends = model.catom("attends(P)")

    val wmc = model.theory
    val startTheoryModification = time
    val (parWmc, parDomains) = wmc.conditionablePartial(List(attends))
    val theoryModificationTime = time - startTheoryModification

    val file = new java.io.PrintWriter("experiments/workshopsexperiment-conditioned-" + number + ".dat")
    def output(str: String) {
      print((new Date()) + ": ")
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
        val wsEvidence = evidence / 10

        val attendees = shuffle(people).take((evidence / 2.0).ceil.toInt)
        val attendeesEvidence = attendees.map { attendee => model.catom("attends(" + attendee + ")") }.toSet

        val nonAttendees = shuffle(people.toList filterNot (attendees.toList.contains(_))).take(evidence / 2)
        val nonAttendeesEvidence = nonAttendees.map { p => model.catom("!attends(" + p + ")") }.toSet

        val hotW = shuffle(workshops).take((wsEvidence / 2.0).ceil.toInt)
        val hotWEvidence = hotW.map { w => model.catom("hot(" + w + ")") }.toSet

        val nonhotW = shuffle(workshops.toList filterNot (hotW.toList.contains(_))).take(wsEvidence / 2)
        val nonhotWEvidence = nonhotW.map { w => model.catom("!hot(" + w + ")") }.toSet

        val domainElems2 = parDomains.addEvidence(parWmc.domainSizes, attendeesEvidence ++ hotWEvidence, nonAttendeesEvidence ++ nonhotWEvidence)
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

object ConditioningWorkshopsExperiment2 extends ConditioningWorkshopsExperiment {

  def main(args: Array[String]): Unit = {
    runExperiment(1, false)
  }

  def runExperiment(number: Int, direction: Boolean) {
    def pad(s: Any) = s.toString.padTo(25, " ").mkString
    def time = System.currentTimeMillis();

    val people = (1 to nbPeople).map { i: Int => "p" + i.toString }
    val workshops = (1 to nbWorkshops).map { i: Int => "w" + i.toString }

    val model = new CompetingWorkshopsModel(nbPeople, nbWorkshops, people.toSeq, Nil, Nil)

    val file = new java.io.PrintWriter("experiments/workshopsexperiment-naive-" + number ++ "-" + direction + ".dat")
    def output(str: String) {
      print((new Date()) + ": ")
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
        val wsEvidence = evidence / 10

        val attendees = shuffle(people).take((evidence / 2.0).ceil.toInt)
        val attendeesEvidence = attendees.map { attendee => model.catom("attends(" + attendee + ")") }.toSet

        val nonAttendees = shuffle(people.toList filterNot (attendees.toList.contains(_))).take(evidence / 2)
        val nonAttendeesEvidence = nonAttendees.map { p => model.catom("!attends(" + p + ")") }.toSet

        val hotW = shuffle(workshops).take((wsEvidence / 2.0).ceil.toInt)
        val hotWEvidence = hotW.map { w => model.catom("hot(" + w + ")") }.toSet

        val nonhotW = shuffle(workshops.toList filterNot (hotW.toList.contains(_))).take(wsEvidence / 2)
        val nonhotWEvidence = nonhotW.map { w => model.catom("!hot(" + w + ")") }.toSet

        val interpretation = attendeesEvidence ++ hotWEvidence ++ nonAttendeesEvidence.map { _.toNegativeUnitClause } ++ nonhotWEvidence.map { _.toNegativeUnitClause }
        assume((interpretation).size == 2 * evidence)

        val naiveModel = new CompetingWorkshopsModel(nbPeople, nbWorkshops, people.toSeq, workshops.toSeq, interpretation.map { _.toString }.toSeq)
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
object ConditioningWorkshopsExperiment3 extends ConditioningWorkshopsExperiment {

  def main(args: Array[String]): Unit = {
    runExperiment(1, false)
  }

  def runExperiment(number: Int, direction: Boolean) {
    def pad(s: Any) = s.toString.padTo(25, " ").mkString
    def time = System.currentTimeMillis();

    val people = (1 to nbPeople).map { i: Int => "p" + i.toString }
    val workshops = (1 to nbWorkshops).map { i: Int => "w" + i.toString }

    val model = new CompetingWorkshopsModel(nbPeople, nbWorkshops, people.toSeq, Nil, Nil)

    val file = new java.io.PrintWriter("experiments/workshopsexperiment-ground-" + number ++ "-" + direction + ".dat")
    def output(str: String) {
      print((new Date()) + ": ")
      println(str)
      file.println(str)
      file.flush
    }
    output(pad("%evidence") +
      pad("totalTime") +
      pad("Z"))
    val iterations = 820.to(0, -10) //if(direction) 0.to(nbPeople,step) else nbPeople.to(0,-step)
    try {
      for (evidence <- iterations) yield {
        val wsEvidence = evidence / 10

        val attendees = shuffle(people).take((evidence / 2.0).ceil.toInt)
        val attendeesEvidence = attendees.map { attendee => model.catom("attends(" + attendee + ")") }.toSet

        val nonAttendees = shuffle(people.toList filterNot (attendees.toList.contains(_))).take(evidence / 2)
        val nonAttendeesEvidence = nonAttendees.map { p => model.catom("!attends(" + p + ")") }.toSet

        val hotW = shuffle(workshops).take((wsEvidence / 2.0).ceil.toInt)
        val hotWEvidence = hotW.map { w => model.catom("hot(" + w + ")") }.toSet

        val nonhotW = shuffle(workshops.toList filterNot (hotW.toList.contains(_))).take(wsEvidence / 2)
        val nonhotWEvidence = nonhotW.map { w => model.catom("!hot(" + w + ")") }.toSet

        val interpretation = attendeesEvidence ++ hotWEvidence ++ nonAttendeesEvidence.map { _.toNegativeUnitClause } ++ nonhotWEvidence.map { _.toNegativeUnitClause }
        assume((interpretation).size == 2 * evidence)

        val naiveModel = new CompetingWorkshopsModel(nbPeople, nbWorkshops, people.toSeq, workshops.toSeq, interpretation.map { _.toString }.toSeq)
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

object ConditioningWorkshopsExperiment4 {

  def main(args: Array[String]): Unit = {
    runExperiment(1)
    runExperiment(1)
  }

  def runExperiment(number: Int) {
    def pad(s: Any) = s.toString.padTo(25, " ").mkString
    def time = System.currentTimeMillis();

    val file = new java.io.PrintWriter("experiments/workshopsexperiment2-conditioned-" + number + ".dat")
    def output(str: String) {
      print((new Date()) + ": ")
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
      for (nbPeople <- 1.to(1501, 20)) yield {
        val nbWorkshops = nbPeople / 10

        val people = (1 to nbPeople).map { i: Int => "p" + i.toString }
        val workshops = (1 to nbWorkshops).map { i: Int => "w" + i.toString }

        val model = new CompetingWorkshopsModel(nbPeople, nbWorkshops, people.toSeq, workshops.toSeq, Nil)
        val attends = model.catom("attends(P)")
        val hot = model.catom("hot(P)")

        val wmc = model.theory
        val startTheoryModification = time
        val (parWmc, parDomains) = wmc.conditionablePartial(List(attends))
        val theoryModificationTime = time - startTheoryModification

        val attendees = shuffle(people).take((nbPeople / 4))
        val attendeesEvidence = attendees.map { attendee => model.catom("attends(" + attendee + ")") }.toSet

        val nonAttendees = shuffle(people.toList filterNot (attendees.toList.contains(_))).take(nbPeople / 4)
        val nonAttendeesEvidence = nonAttendees.map { p => model.catom("!attends(" + p + ")") }.toSet

        val hotW = shuffle(workshops).take((nbWorkshops / 4))
        val hotWEvidence = hotW.map { w => model.catom("hot(" + w + ")") }.toSet

        val nonhotW = shuffle(workshops.toList filterNot (hotW.toList.contains(_))).take(nbWorkshops / 4)
        val nonhotWEvidence = nonhotW.map { w => model.catom("!hot(" + w + ")") }.toSet

        val domainElems2 = parDomains.addEvidence(parWmc.domainSizes, attendeesEvidence ++ hotWEvidence, nonAttendeesEvidence ++ nonhotWEvidence)
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
          pad((inferenceTime / 1000d)) +
          pad(((theoryModificationTime + compilationTime + inferenceTime) / 1000F)) +
          pad(Z.logToDouble.toFloat)

        output(str)
      }
    } finally { file.close() }

  }

}

object ConditioningWorkshopsExperiment5 {

  def main(args: Array[String]): Unit = {
    runExperiment(1)
  }

  def runExperiment(number: Int) {
    def pad(s: Any) = s.toString.padTo(25, " ").mkString
    def time = System.currentTimeMillis();

    val file = new java.io.PrintWriter("experiments/workshopsexperiment2-naive-" + number + ".dat")
    def output(str: String) {
      print((new Date()) + ": ")
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
      for (nbPeople <- 1.to(20001, 30)) yield {
        val nbWorkshops = nbPeople / 10

        val people = (1 to nbPeople).map { i: Int => "p" + i.toString }
        val workshops = (1 to nbWorkshops).map { i: Int => "w" + i.toString }

        val model = new CompetingWorkshopsModel(nbPeople, nbWorkshops, people.toSeq, Nil, Nil)

        val attendees = shuffle(people).take((nbPeople / 4))
        val attendeesEvidence = attendees.map { attendee => model.catom("attends(" + attendee + ")") }.toSet

        val nonAttendees = shuffle(people.toList filterNot (attendees.toList.contains(_))).take(nbPeople / 4)
        val nonAttendeesEvidence = nonAttendees.map { p => model.catom("!attends(" + p + ")") }.toSet

        val hotW = shuffle(workshops).take((nbWorkshops / 4))
        val hotWEvidence = hotW.map { w => model.catom("hot(" + w + ")") }.toSet

        val nonhotW = shuffle(workshops.toList filterNot (hotW.toList.contains(_))).take(nbWorkshops / 4)
        val nonhotWEvidence = nonhotW.map { w => model.catom("!hot(" + w + ")") }.toSet

        val interpretation = hotWEvidence ++ attendeesEvidence ++ nonAttendeesEvidence.map { _.toNegativeUnitClause } ++ nonhotWEvidence.map { _.toNegativeUnitClause }

        val naiveModel = new CompetingWorkshopsModel(nbPeople, nbWorkshops, people.toSeq, workshops.toSeq, interpretation.map { _.toString }.toSeq)
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
object ConditioningWorkshopsExperiment6 {

  def main(args: Array[String]): Unit = {
    runExperiment(2)
  }

  def runExperiment(number: Int) {
    def pad(s: Any) = s.toString.padTo(25, " ").mkString
    def time = System.currentTimeMillis();

    val file = new java.io.PrintWriter("experiments/workshopsexperiment2-ground-" + number + ".dat")
    def output(str: String) {
      print((new Date()) + ": ")
      println(str)
      file.println(str)
      file.flush
    }
    output(pad("nbPeople") +
      pad("totalTime") +
      pad("Z"))
    try {
      for (nbPeople <- 250.to(20001, 15)) yield {
        val nbWorkshops = nbPeople / 10

        val people = (1 to nbPeople).map { i: Int => "p" + i.toString }
        val workshops = (1 to nbWorkshops).map { i: Int => "w" + i.toString }

        val model = new CompetingWorkshopsModel(nbPeople, nbWorkshops, people.toSeq, workshops.toSeq, Nil)

        val attendees = shuffle(people).take((nbPeople / 4))
        val attendeesEvidence = attendees.map { attendee => model.catom("attends(" + attendee + ")") }.toSet

        val nonAttendees = shuffle(people.toList filterNot (attendees.toList.contains(_))).take(nbPeople / 4)
        val nonAttendeesEvidence = nonAttendees.map { p => model.catom("!attends(" + p + ")") }.toSet

        val hotW = shuffle(workshops).take((nbWorkshops / 4))
        val hotWEvidence = hotW.map { w => model.catom("hot(" + w + ")") }.toSet

        val nonhotW = shuffle(workshops.toList filterNot (hotW.toList.contains(_))).take(nbWorkshops / 4)
        val nonhotWEvidence = nonhotW.map { w => model.catom("!hot(" + w + ")") }.toSet

        val interpretation = hotWEvidence ++ attendeesEvidence ++ nonAttendeesEvidence.map { _.toNegativeUnitClause } ++ nonhotWEvidence.map { _.toNegativeUnitClause }

        val naiveModel = new CompetingWorkshopsModel(nbPeople, nbWorkshops, people.toSeq, workshops.toSeq, interpretation.map { _.toString }.toSeq)
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
