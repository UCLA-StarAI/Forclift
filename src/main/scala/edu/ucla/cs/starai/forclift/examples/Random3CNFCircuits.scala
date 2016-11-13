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
//import edu.ucla.cs.starai.forclift.examples._
//
//object Random3CNFCircuits {
//
//  def main(args: Array[String]): Unit = {
//
//    //        {
//    //            val model = new models.WeightedCNFModel {
//    //                def theoryString = (
//    //                    """
//    //domain Variable 10 {}
//    //
//    //predicate v(Variable) 0.5 0.5
//    //predicate c0(Variable,Variable,Variable) 0.5 0.5
//    //predicate c1(Variable,Variable,Variable) 0.5 0.5
//    //predicate c2(Variable,Variable,Variable) 0.5 0.5
//    //predicate c3(Variable,Variable,Variable) 0.5 0.5
//    //            
//    //v(X) v v(Y) v v(Z) v !c0(X,Y,Z), X!=Y, X!=Z, Y!=Z
//    //v(X) v v(Y) v !v(Z) v !c1(X,Y,Z), X!=Y, X!=Z, Y!=Z
//    //v(X) v !v(Y) v !v(Z) v !c2(X,Y,Z), X!=Y, X!=Z, Y!=Z
//    //!v(X) v !v(Y) v !v(Z) v !c3(X,Y,Z), X!=Y, X!=Z, Y!=Z
//    //""")
//    //            }
//    //
//    //            //ALTERNATIVE: !c0(X,Y,Z) v !c0(Y,Z,Z), etc.
//    //            println(model.theory)
//    //
//    //            //			model.theory.showSmoothNnfPdf(true,Integer.MAX_VALUE,"random3sat")
//    //            model.theory.showNnfPdf(true, Integer.MAX_VALUE, "random3sat")
//    //
//    //            println("LogNbRelations = " + model.theory.logWmc)
//    //            println("NbRelations = " + math.exp(model.theory.logWmc))
//    //        }
//
//    //                {
//    //                    val model = new models.WeightedCNFModel {
//    //                        def theoryString = (
//    //                            """
//    //        domain Variable 10 {}
//    //        domain Clause 10 {}
//    //        
//    //        predicate p(Variable) 1 1
//    //                                
//    //        predicate c1v1(Clause,Variable) 0.5 0.5
//    //        predicate c1v2(Clause,Variable) 0.5 0.5
//    //        predicate c1v3(Clause,Variable) 0.5 0.5
//    //                                
//    //        predicate c2v1(Clause,Variable) 0.5 0.5
//    //        predicate c2v2(Clause,Variable) 0.5 0.5
//    //        predicate c2v3(Clause,Variable) 0.5 0.5
//    //                                
//    //        predicate c3v1(Clause,Variable) 0.5 0.5
//    //        predicate c3v2(Clause,Variable) 0.5 0.5
//    //        predicate c3v3(Clause,Variable) 0.5 0.5
//    //                                
//    //        predicate c4v1(Clause,Variable) 0.5 0.5
//    //        predicate c4v2(Clause,Variable) 0.5 0.5
//    //        predicate c4v3(Clause,Variable) 0.5 0.5
//    //        
//    //        !c1v1(C,V) v !c1v1(C,W), V!=W
//    //        !c1v2(C,V) v !c1v2(C,W), V!=W
//    //        !c1v3(C,V) v !c1v3(C,W), V!=W
//    //                                
//    //        !c1v1(C,V) v !c1v2(C,V)
//    //        !c1v1(C,V) v !c1v3(C,V)
//    //        !c1v2(C,V) v !c1v3(C,V)
//    //        
//    //        !c1v1(C,V1) v !c1v2(C,V2) v !c1v3(C,V3) v p(V1) v p(V2) v p(V3)
//    //        !c1v1(C,V1) v !c1v2(C,V2) v !c1v3(C,V3) v p(V1) v p(V2) v p(V3)
//    //        !c1v1(C,V1) v !c1v2(C,V2) v !c1v3(C,V3) v p(V1) v p(V2) v p(V3)
//    //        
//    //        !c2v1(C,V) v !c2v1(C,W), V!=W
//    //        !c2v2(C,V) v !c2v2(C,W), V!=W
//    //        !c2v3(C,V) v !c2v3(C,W), V!=W
//    //                                
//    //        !c2v1(C,V) v !c2v2(C,V)
//    //        !c2v1(C,V) v !c2v3(C,V)
//    //        !c2v2(C,V) v !c2v3(C,V)
//    //        
//    //        !c2v1(C,V1) v !c2v2(C,V2) v !c2v3(C,V3) v p(V1) v p(V2) v !p(V3)
//    //        !c2v1(C,V1) v !c2v2(C,V2) v !c2v3(C,V3) v p(V1) v p(V2) v !p(V3)
//    //        !c2v1(C,V1) v !c2v2(C,V2) v !c2v3(C,V3) v p(V1) v p(V2) v !p(V3)
//    //        
//    //        !c3v1(C,V) v !c3v1(C,W), V!=W
//    //        !c3v2(C,V) v !c3v2(C,W), V!=W
//    //        !c3v3(C,V) v !c3v3(C,W), V!=W
//    //                                
//    //        !c3v1(C,V) v !c3v2(C,V)
//    //        !c3v1(C,V) v !c3v3(C,V)
//    //        !c3v2(C,V) v !c3v3(C,V)
//    //        
//    //        !c3v1(C,V1) v !c3v2(C,V2) v !c3v3(C,V3) v p(V1) v !p(V2) v !p(V3)
//    //        !c3v1(C,V1) v !c3v2(C,V2) v !c3v3(C,V3) v p(V1) v !p(V2) v !p(V3)
//    //        !c3v1(C,V1) v !c3v2(C,V2) v !c3v3(C,V3) v p(V1) v !p(V2) v !p(V3)
//    //        
//    //        !c4v1(C,V) v !c4v1(C,W), V!=W
//    //        !c4v2(C,V) v !c4v2(C,W), V!=W
//    //        !c4v3(C,V) v !c4v3(C,W), V!=W
//    //                                
//    //        !c4v1(C,V) v !c4v2(C,V)
//    //        !c4v1(C,V) v !c4v3(C,V)
//    //        !c4v2(C,V) v !c4v3(C,V)
//    //        
//    //        !c4v1(C,V1) v !c4v2(C,V2) v !c4v3(C,V3) v !p(V1) v !p(V2) v !p(V3)
//    //        !c4v1(C,V1) v !c4v2(C,V2) v !c4v3(C,V3) v !p(V1) v !p(V2) v !p(V3)
//    //        !c4v1(C,V1) v !c4v2(C,V2) v !c4v3(C,V3) v !p(V1) v !p(V2) v !p(V3)
//    //                                
//    //        """)
//    //                    }
//    //        
//    //                    //PROBLEM: THERE ARE MULTIPLE INTERPRETATIONS FOR v IN A GIVEN INTERPRETATION FOR c!
//    //                    println(model.theory)
//    //        
//    //                    //			model.theory.showSmoothNnfPdf(true,Integer.MAX_VALUE,"random3sat")
//    //                    model.theory.showNnfPdf(true, Integer.MAX_VALUE, "random3sat")
//    //        
//    //                    println("LogNbRelations = " + model.theory.logWmc)
//    //                    println("NbRelations = " + math.exp(model.theory.logWmc))
//    //                }
//
//    {
//      val model = new models.WeightedCNFModel {
//        def theoryString = (
//          """
//        domain Variable 2 {}
//        domain Clause 2 {}
//        
//        predicate p(Variable) 1 1
//                                
//        predicate cv1(Clause,Variable) 0.5 0.5
//        predicate cv2(Clause,Variable) 0.5 0.5
//        predicate cv3(Clause,Variable) 0.5 0.5
//                                
//        predicate cs1(Clause) 0.5 0.5
//        predicate cs2(Clause) 0.5 0.5
//        predicate cs3(Clause) 0.5 0.5
//        
//        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v !cs1(C) v !cs2(C) v !cs3(C) v p(V1) v p(V2) v p(V3)
//        
//        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v !cs1(C) v !cs2(C) v cs3(C) v p(V1) v p(V2) v !p(V3)
//        
//        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v !cs1(C) v cs2(C) v !cs3(C) v p(V1) v !p(V2) v p(V3)
//        
//        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v !cs1(C) v cs2(C) v cs3(C) v p(V1) v !p(V2) v !p(V3)
//        
//        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v cs1(C) v !cs2(C) v !cs3(C) v !p(V1) v p(V2) v p(V3)
//        
//        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v cs1(C) v !cs2(C) v cs3(C) v !p(V1) v p(V2) v !p(V3)
//        
//        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v cs1(C) v cs2(C) v !cs3(C) v !p(V1) v !p(V2) v p(V3)
//        
//        !cv1(C,V1) v !cv2(C,V2) v !cv3(C,V3) v cs1(C) v cs2(C) v cs3(C) v !p(V1) v !p(V2) v !p(V3)
//            
//        !cv1(C,V1) v !cv1(C,V2), V1!=V2
//        !cv2(C,V1) v !cv2(C,V2), V1!=V2
//        !cv3(C,V1) v !cv3(C,V2), V1!=V2
//                                
//        """)
//      }
//
//      //PROBLEM: THERE ARE MULTIPLE INTERPRETATIONS FOR v IN A GIVEN INTERPRETATION FOR c!
//      println(model.theory)
//
//      //			model.theory.showSmoothNnfPdf(true,Integer.MAX_VALUE,"random3sat")
//      model.theory.showNnfPdf(true, Integer.MAX_VALUE, "random3sat")
//
//      println("expectedCount = " + model.theory.logSmoothWmc)
//      println("nbRelations = " + model.theory.logSmoothWmc)
//    }
//  }
//
//}
