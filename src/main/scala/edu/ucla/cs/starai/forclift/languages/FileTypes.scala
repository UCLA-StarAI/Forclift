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

package edu.ucla.cs.starai.forclift.languages

import scala.language.implicitConversions

sealed class FileFormat(val name: String, val extension: String){
  
  override def toString = name
  
}

object FileFormat{
  
  case object MLN extends FileFormat("MLN","mln")
  case object FOCNF extends FileFormat("FO-CNF","focnf")
  case object FactorGraph extends FileFormat("Factor Graph","fg")
  case object WeightedGroupLogic extends FileFormat("Weighted Group Logic","wgl")

  implicit def stringToFileFormat(s:String): Option[FileFormat] = s match {
    case FOCNF.extension => Some(FOCNF)
    case MLN.extension => Some(MLN)
    case FactorGraph.extension => Some(FactorGraph)
    case WeightedGroupLogic.extension => Some(WeightedGroupLogic)
    case _ =>  None
  }
  
}

  
