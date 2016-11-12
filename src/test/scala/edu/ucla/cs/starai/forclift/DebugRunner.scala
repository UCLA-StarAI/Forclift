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

package edu.ucla.cs.starai.forclift

import edu.ucla.cs.starai.forclift.bugs.Bug1Model
import edu.ucla.cs.starai.forclift.compiler.IJCAI11Compiler

/**
 * Useful for running a test with the Eclipse Scala IDE debugger
 */
object DebugRunner extends App{

	val model = new Bug1Model(5)
    model.compilerBuilder = IJCAI11Compiler.builder
    model.theory.nnf
    println("done")
    
}
