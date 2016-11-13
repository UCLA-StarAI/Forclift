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
//object ComplexSymmetryExample extends App {
//
//  /*
//    {
//			
//			val symmetricModel = new models.WeightedCNFModel {
//
//    def theoryString = (
//		"""
//domain Person 5 {guy}
//
//predicate enemies(Person,Person) 1.0 1.0
//predicate friends(Person,Person) 1.0 1.0
//
//!friends(X,Y) v !enemies(X,Y)
//!friends(Y,X) v !enemies(X,Y)
//"""
//	)
//				}
//	
//			println(symmetricModel.theory)
//				    
////			symmetricModel.theory.showSmoothNnfPdf(true,Integer.MAX_VALUE,"symmetric-smooth.nnf")
//			symmetricModel.theory.showNnfPdf(true,Integer.MAX_VALUE,"symmetric.nnf")
//    }
//    */
//  //    /*
//  {
//
//    val symmetricModel = new models.WeightedCNFModel {
//
//      def theoryString = (
//        """
//domain Person 5 {guy}
//
//predicate friends(Person,Person) 1.0 1.0
//
//!friends(X,Y) v friends(Y,X)
//friends(X,Y) v !friends(Y,X)
//""")
//    }
//
//    println(symmetricModel.theory)
//
//    //			symmetricModel.theory.showSmoothNnfPdf(true,Integer.MAX_VALUE,"symmetric-smooth.nnf")
//    symmetricModel.theory.showNnfPdf(true, Integer.MAX_VALUE, "symmetric.nnf")
//  }
//  //    */
//  /*
//     {
//			
//			val symmetricModel = new models.WeightedCNFModel {
//
//    def theoryString = (
//		"""
//domain Person 5 {guy}
//
//predicate parent(Person,Person) 1.0 1.0
//
//!parent(X,Y) v !parent(Y,X), X!=Y
//parent(X,Y) v parent(Y,X), X!=Y
//!parent(X,X)
//"""
//	)
//	
//	BUUUUG
//				}
//	
//			println(symmetricModel.theory)
//				    
////			symmetricModel.theory.showSmoothNnfPdf(true,Integer.MAX_VALUE,"symmetric-smooth.nnf")
//			symmetricModel.theory.showNnfPdf(false,Integer.MAX_VALUE,"symmetric.nnf")
//    }
//    */
//  /*
//     {
//			
//			val symmetricModel = new models.WeightedCNFModel {
//
//    def theoryString = (
//		"""
//domain D 5
//
//predicate lteq(D,D) 1.0 1.0
//
//lteq(X,Y) v lteq(Y,X)
//"""
//	)
//				}
//	
//			println(symmetricModel.theory)
//				    
////			symmetricModel.theory.showSmoothNnfPdf(true,Integer.MAX_VALUE,"symmetric-smooth.nnf")
//			symmetricModel.theory.showNnfPdf(false,Integer.MAX_VALUE,"symmetric.nnf")
//    }
//    */
//}
