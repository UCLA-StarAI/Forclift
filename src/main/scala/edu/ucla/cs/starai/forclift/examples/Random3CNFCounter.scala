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

package edu.ucla.cs.starai.forclift.examples;
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

///*
// * Copyright (C) 2013 Guy Van den Broeck (guy.vandenbroeck@cs.kuleuven.be)
// * 
// * This file is part of WFOMC (http://dtai.cs.kuleuven.be/ml/systems/wfomc).
// *
// * WFOMC is free software: you can redistribute it and/or modify
// * it under the terms of the GNU Lesser General Public License as published by
// * the Free Software Foundation, either version 3 of the License, or
// * (at your option) any later version.
// *
// * WFOMC is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU Lesser General Public License for more details.
// *
// * You should have received a copy of the GNU Lesser General Public License
// * along with WFOMC.  If not, see <http://www.gnu.org/licenses/>.
// * 
// */
//
//package liftedinference.examples
//
//import edu.ucla.cs.starai.forclift._
//import java.io._
//import edu.ucla.cs.starai.forclift.examples._
//
//object Random3CNFCounter {
//
//  def main(args: Array[String]): Unit = {
//
//    val rootDir = new File("experiments/3cnf/")
//    rootDir.mkdir()
//
//    val output: PrintWriter = new PrintWriter(new FileWriter(new File(rootDir, "counts.dat")));
//
//    def logData(line: String) {
//      System.out.print(line)
//      output.print(line)
//      output.flush
//    }
//
//    def logDataln(line: String = "") {
//      System.out.println(line)
//      output.println(line)
//      output.flush
//    }
//
//    def pad(s: Any) = s.toString.padTo(25, " ").mkString
//
//    val domains = Stream.from(5, 5)
//    val ratios = List(4, 4.24, 5, 5.25, 5.5, 5.75, 6, 7)
//
//    logDataln(pad("NbVars") + ratios.map { r => pad("Ratio " + r) }.mkString)
//
//    for (domain <- domains) {
//
//      logData(pad(domain))
//
//      for (ratio <- ratios) {
//
//        println("nbVars = " + domain)
//        println("ratio = " + ratio)
//        println("logNbInterpretations = " + (domain * math.log(2)) / math.log(2))
//
//        val domains = (
//          "domain Variable " + domain + " {} " + "\n" +
//          "domain Clause " + (math.round(ratio * domain).toInt) + " {} " + "\n")
//        val denominator = new models.WeightedCNFModel {
//          def theoryString = (domains + """
//	        
//	        predicate p(Variable) 0.5 0.5
//	                                
//	        predicate cv1(Clause,Variable) 0.5 0.5
//	        predicate cv2(Clause,Variable) 0.5 0.5
//	        predicate cv3(Clause,Variable) 0.5 0.5
//	                                
//	        predicate cs1(Clause) 0.5 0.5
//	        predicate cs2(Clause) 0.5 0.5
//	        predicate cs3(Clause) 0.5 0.5
//	        
//	        !cv1(C,V1) v !cv1(C,V2), V1!=V2
//	        !cv2(C,V1) v !cv2(C,V2), V1!=V2
//	        !cv3(C,V1) v !cv3(C,V2), V1!=V2
//	                                
//	        """)
//        }
//
//        val denomWmc = denominator.theory.logSmoothWmc
//        println("logDenominator = " + denomWmc.logToDouble / math.log(2))
//
//        val numerator = new models.WeightedCNFModel {
//          def theoryString = (domains + """
//	        
//	        predicate p(Variable) 0.5 0.5
//	                                
//	        predicate cv1(Clause,Variable) 0.5 0.5
//	        predicate cv2(Clause,Variable) 0.5 0.5
//	        predicate cv3(Clause,Variable) 0.5 0.5
//	                                
//	        predicate cs1(Clause) 0.5 0.5
//	        predicate cs2(Clause) 0.5 0.5
//	        predicate cs3(Clause) 0.5 0.5
//	        
//	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v !cs1(C) v !cs2(C) v !cs3(C) v p(V1) v p(V2) v p(V3)
//	        
//	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v !cs1(C) v !cs2(C) v cs3(C) v p(V1) v p(V2) v !p(V3)
//	        
//	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v !cs1(C) v cs2(C) v !cs3(C) v p(V1) v !p(V2) v p(V3)
//	        
//	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v !cs1(C) v cs2(C) v cs3(C) v p(V1) v !p(V2) v !p(V3)
//	        
//	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v cs1(C) v !cs2(C) v !cs3(C) v !p(V1) v p(V2) v p(V3)
//	        
//	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v cs1(C) v !cs2(C) v cs3(C) v !p(V1) v p(V2) v !p(V3)
//	        
//	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v cs1(C) v cs2(C) v !cs3(C) v !p(V1) v !p(V2) v p(V3)
//	        
//	        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v cs1(C) v cs2(C) v cs3(C) v !p(V1) v !p(V2) v !p(V3)
//	            
//	        !cv1(C,V1) v !cv1(C,V2), V1!=V2
//	        !cv2(C,V1) v !cv2(C,V2), V1!=V2
//	        !cv3(C,V1) v !cv3(C,V2), V1!=V2
//	                                
//	        """)
//        }
//
//        val numWmc = numerator.theory.logSmoothWmc
//        println("logNumerator = " + numWmc.logToDouble / math.log(2))
//        println("logModelProb = " + (numWmc / denomWmc).logToDouble / math.log(2))
//        println("modelProb = " + (numWmc / denomWmc).toDouble)
//
//        val logExpectedNbModels = (numWmc / denomWmc).logToDouble / math.log(2) + domain
//        println("logNbModels = " + logExpectedNbModels)
//
//        logData(pad(logExpectedNbModels))
//
//        println()
//
//      }
//
//      logDataln()
//    }
//
//    output.close
//
//  }
//
//}
