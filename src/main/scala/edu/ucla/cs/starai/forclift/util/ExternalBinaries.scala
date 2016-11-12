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

package edu.ucla.cs.starai.forclift.util

import java.io._
import scala.io._
import System._
//import ProcessBuilder._
import scala.sys.process._
import collection.mutable.ListBuffer

case class ExternalBinary(
  name: String,
  cmd: String,
  availableCmd: String,
  availableError: String,
  availableExpectedReturnValue: Int = 0) {
}

/**
 * Standardized code to call external binaries uniformly.
 *
 * Known external binaries:
 * - c2d
 * - pdflatex
 * - dot (graphviz)
 */
object ExternalBinaries {

  // Binaries
  lazy val c2d = ExternalBinary(
    "C2D",
    (if (getenv("C2DCMD") == null) "./external/c2d_linux" else getenv("C2DCMD")),
    (if (getenv("C2DCMD") == null) "./external/c2d_linux" else getenv("C2DCMD")),
    """Cannot find or execute c2d compiler. Make sure you are on a linux system 
       and the executable is installed as ./c2d_linux or set the C2DCMD environment 
       variable to a correct path.""",
    1)
  lazy val pdflatex = ExternalBinary(
    "pdflatex",
    "pdflatex",
    "pdflatex --version",
    "Cannot find pdflatex in your path.")
  lazy val dot = ExternalBinary(
    "dot",
    "dot",
    "dot -V",
    "Cannot find Graphviz dot in your path.")
  lazy val dot2tex = ExternalBinary(
    "dot2tex",
    "dot2tex",
    "dot2tex -V",
    "Cannot find dot2tex in your path.")
  lazy val dot2texi = ExternalBinary(
    "dot2texi",
    "echo 'dot2texi is a latex package and not executable from the prompt.'",
    "kpsewhich dot2texi.sty",
    "Cannot find dot2texi LaTeX package in your LaTeX source tree.")

  // lazy vals so that the check happens only once
  lazy val checkC2DAvailable = checkAvailable(c2d)
  lazy val checkPdfLatexAvailable = checkAvailable(pdflatex)
  lazy val checkDotAvailable = checkAvailable(dot)
  lazy val checkDot2TexAvailable = checkAvailable(dot2tex)
  lazy val checkDot2TexiAvailable = checkAvailable(dot2texi)

  lazy val c2dCmd = c2d.cmd
  lazy val pdflatexCmd = pdflatex.cmd
  lazy val dotCmd = dot.cmd
  lazy val dot2texCmd = dot2tex.cmd

  lazy val logger = ProcessLogger(
    (o: String) => {}, //println("out: " + o),
    (e: String) => {}) //println("err: " + e))

  def stringLogger(out: ListBuffer[String], err: ListBuffer[String]): ProcessLogger = {
    ProcessLogger(
      (o: String) => out += o,
      (e: String) => err += e)
  }

  /**
   * Check whether the given command is available.
   * Assumes that the given command terminates with exitcode 0.
   */
  def checkAvailable(binary: ExternalBinary) {
    val cmd = binary.availableCmd
    //print("\nTrying cmd: "+cmd+"\n")
    //print("C2DCMD="+getenv("C2DCMD")+"\n")
    //val pb = new ProcessBuilder(cmd.split(" "):_*)
    val output = new ListBuffer[String]
    val pb = Process(cmd)
    var exitcode = 0
    try {
      //print("\nStarting process\n")
      //val prcs = pb.run()
      //print("\nRunning process\n")
      //exitcode = prcs.exitValue()
      exitcode = pb ! stringLogger(output, output)
      //print("\nExitcode = "+exitcode+"\n")
      //prcs.destroy()
    } catch {
      case e: IOException => {
        //print(e);
        exitcode = 127
      }
      case e: Throwable => {
        //print(e);
        exitcode = 1
      }
    }
    if (exitcode != binary.availableExpectedReturnValue) {
      throw new IllegalStateException(binary.availableError + "\nTried the following command: " + cmd + "\nOutput: " + output.mkString("\n"))
    } 
  }
}
