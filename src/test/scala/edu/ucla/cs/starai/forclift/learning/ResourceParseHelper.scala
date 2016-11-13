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

package edu.ucla.cs.starai.forclift.learning

import edu.ucla.cs.starai.forclift.languages.mln.MLNParser
import edu.ucla.cs.starai.forclift.languages.mln.MLN
import edu.ucla.cs.starai.forclift.util.Resource

/**
 * Helper trait to parse MLNs and Databases from the resources directory.
 */
trait ResourceParseHelper {
  
  def parse(mlnfile: String, dbfile: String): (MLN,MLN) = {
    val parser = new MLNParser
    parser.setLearnModus(true)
    val mlnstring = Resource.fromFile(mlnfile).mkString
    val dbstring = Resource.fromFile(dbfile).mkString

    val mln = parser.parseMLN(mlnstring)
    val db = parser.parseDB(dbstring)
    (mln,db)
  }
  
}
