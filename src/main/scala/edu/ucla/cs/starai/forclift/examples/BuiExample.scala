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
// * Copyright (C) 2011 Guy Van den Broeck (guy.vandenbroeck@cs.kuleuven.be)
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
//import models._
//
//object BuiExample {
//
//  def main(args: Array[String]): Unit = {
//
//    //		val model =  new MLNModel {
//    //
//    //    def theoryString = (
//    //"""
//    //d = {D1,D2,D3}
//    //P(d,d)
//    //1.4 P(x,y) ^ P(y,z)
//    //""")
//    //
//    //}
//
//    //		val model =  new WeightedCNFModel {
//    //
//    //    def theoryString = (
//    //"""
//    //domain D 4
//    //                
//    //predicate f(D,D,D) 1 1.4
//    //predicate p(D,D) 1 1
//    //            
//    //f(X,Y,Z) ∨ p(X,Y)
//    //f(X,Y,Z) ∨ p(Z,X)
//    //!f(X,Y,Z) ∨ !p(X,Y) ∨ !p(Z,X)
//    //""")
//    //
//    //}
//
//    //		val model =  new WeightedCNFModel {
//    //
//    //    def theoryString = (
//    //"""
//    //domain D 4
//    //               
//    //predicate p(D,D) 1 1
//    //
//    //p(X,Y) v p(Y,Z)
//    //""")
//    //
//    //}
//    val model = new WeightedCNFModel {
//
//      def theoryString = (
//        """
//domain D 4
//               
//predicate p(D,D) 1 1
//
//p(X,Y) v p(Y,Z)
//            
//""")
//
//    }
//
//    println(model.theory)
//
//    model.theory.showNnfPdf(false, Integer.MAX_VALUE, "bui")
//
//    println("LogWMC = " + model.theory.logSmoothWmc)
//  }
//
//}
