package edu.ucla.cs.starai.forclift.learning

import edu.ucla.cs.starai.forclift.languages.mln.MLNParser
import edu.ucla.cs.starai.forclift.languages.mln.MLN
import scala.io.Source
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