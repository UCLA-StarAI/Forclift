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

import sbt._
import Keys._

import java.util.jar.{Attributes, Manifest}
import Path.makeString
import System._
import java.util.Date

import sbtassembly.Plugin._
import AssemblyKeys._
import scoverage.ScoverageSbtPlugin._
  
import com.typesafe.sbt.SbtProguard
import com.typesafe.sbt.SbtProguard._
import ProguardKeys.{ mergeStrategies, merge, options, proguard }
import ProguardOptions.keepMain
import ProguardMerge.append

import de.heikoseeberger.sbtheader.HeaderPlugin
import de.heikoseeberger.sbtheader.license.Apache2_0
import de.heikoseeberger.sbtheader.HeaderPattern

import com.typesafe.sbteclipse.plugin.EclipsePlugin._
import laika.sbt.LaikaSbtPlugin.LaikaPlugin


object BuildSettings {
  
  val buildName         = "forclift"
  val buildOrganization = "edu.ucla.cs.starai"
  val buildScalaVersion = "2.11.8"
  val buildScalaVersionMajor = "2.11"
  val jreTargetVersion  = "1.7"
  val buildVersion      = "3.1"
  val buildMainClass    = "edu.ucla.cs.starai.forclift.cli.CLI"
  val buildJarName      = buildName+".jar"
  val buildJarNameDebug = buildName+"-debug"+".jar"
  val javacFlags = Seq("-source", jreTargetVersion, "-target", jreTargetVersion)
  val productionScalacFlags = Seq(
          "-target:jvm-"+jreTargetVersion,   
          "-encoding", "UTF8",
          "-optimise", 
          "-Xelide-below", "3000", 
          "-Xdisable-assertions"
  )
  val testScalacFlags = Seq(
          "-target:jvm-"+jreTargetVersion,
          "-encoding", "UTF8",
          "-optimise"
  )

  lazy val appSettings = Seq (
      name         := buildName,
      organization := buildOrganization,
      scalaVersion := buildScalaVersion,
      version      := buildVersion
  )
  
  lazy val compileSettings = Seq(
      // Compile options
      mainClass in Compile := Some(buildMainClass),
      // disable assertions and optimize in final binaries
      javacOptions ++= javacFlags,
      scalacOptions in Compile ++= productionScalacFlags
  )
  
  lazy val testSettings = Seq(
      // show durations and full strack traces (scalatest options)
      testOptions in Test += Tests.Argument("-oDF"),
      javacOptions ++= javacFlags,
      // enable assertions and optimize in test binaries
      scalacOptions in Test := testScalacFlags,
      // do not run tests tagged as 'Slow'
      testOptions in Test += Tests.Argument("-l", "org.scalatest.tags.Slow"),
      // disable parallel testing in an atempt to avoid spurious test errors
      parallelExecution in Test := false
  )
    
  lazy val sCoverageSettings = instrumentSettings ++ Seq(
    ScoverageKeys.excludedPackages in ScoverageCompile := "udy.ucla.cs.starai.forclift.examples.*;",
    // re-enable highlighting when running scala > 2.11.0 (was disabled because of bug)
    ScoverageKeys.highlighting := true,
    // here, add flags (++=) because otherwise instrumentation fails?
    scalacOptions in ScoverageCompile ++= testScalacFlags
  )

  // Settings for Assembly plugin (https://github.com/eed3si9n/sbt-assembly)
  lazy val assemblySettings = sbtassembly.Plugin.assemblySettings ++ Seq (
    jarName in assembly := buildJarName,
    assembleArtifact in packageBin := true, // if false: Exclude source files
    test in assembly := {}, // Skip the test during assembly
    excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
    cp.filter{el => el.data.getName.toLowerCase.indexOf("junit") != -1 ||
                    el.data.getName.toLowerCase.indexOf("scalatest") != -1 }
    }
  )

  lazy val proguardSettings =  SbtProguard.proguardSettings ++ Seq(
      ProguardKeys.proguardVersion in Proguard := "5.3.1",
      options in Proguard += keepMain(buildMainClass),
      options in Proguard += "-dontnote",
      options in Proguard += "-dontwarn",
      options in Proguard += "-ignorewarnings",
      options in Proguard += "-dontoptimize",
      options in Proguard += "-dontobfuscate",
      options in Proguard += "-keepattributes *Annotation*",
      options in Proguard += "-keepattributes Signature",
      options in Proguard += "-keepattributes InnerClasses",
      options in Proguard += "-keepattributes EnclosingMethod",
      ProguardKeys.inputFilter in Proguard := { file =>
        // remove embedded documentation and manifests
        Some("!**.html,!**.css,!**.gif,!META-INF/**")
      },
      // Make the jar first such that a manifest is generated
      exportJars := true,
       // Explicitely include generated jar file
      ProguardKeys.inputs in Proguard := (dependencyClasspath in Compile).value.files,
      ProguardKeys.filteredInputs in Proguard ++= ProguardOptions.noFilter((packageBin in Compile).value),
      javaOptions in (Proguard, proguard) := Seq("-Xmx2G")
  )

  lazy val headerSettings = List(
      HeaderPlugin.autoImport.headers := Map(
        "scala" -> Apache2_0("2016", "Guy Van den Broeck and Wannes Meert (UCLA and KU Leuven)"),
        "java" -> Apache2_0("2016", "Jan Van Haaren (KU Leuven)")
      )
    )

  lazy val docSettings = LaikaPlugin.defaults ++ Seq()
   
  import ExtraCommands._
  lazy val customCommands = Seq(dist,stats)

  lazy val createAllHeaders = addCommandAlias("createAllHeaders",       ";compile:createHeaders;test:createHeaders")

}

object ForcliftBuild extends Build {

  import BuildSettings._

  val dependencies = Seq (
	  // Scalatest for testing
	  "org.scalatest" % "scalatest_2.11" % "2.2.2" % "test",
	  // Mix in junit layer on top on scalatest (for eclipse)
	  "com.novocode" % "junit-interface" % "0.10-M4" % "test",
	  // Command line argument parser
	  "org.clapper" %% "argot" % "1.0.3",
	  // Optimization algorithm
    	  "org.scalanlp" %% "breeze" % "0.8.1",
          // New Breeze version slows down learning experiments a lot
          //"org.scalanlp" %% "breeze" % "0.11.2",
          //"org.scalanlp" %% "breeze-natives" % "0.11.2",
	  // Parser-combinators (used to be in the scala library)
	  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
  	  // fast math library is not really faster for us
	  // "net.jafama" % "jafama" % "2.1.0"
	  // macro for generating equals/hashcode/canEqual
	  // val scalaEquals =  "org.scalaequals" %% "scalaequals-core" % "1.2.0"
  )

  lazy val main = Project("main", file("."))
    .settings(Defaults.defaultSettings:_*)
    .settings(libraryDependencies := dependencies)
    .settings(commands ++= customCommands)
    .settings(appSettings:_*)
    .settings(compileSettings:_*)
    .settings(testSettings:_*)
    .settings(sCoverageSettings: _*)
    .settings(proguardSettings: _*)
    .settings(assemblySettings: _*)
    .settings(headerSettings: _*)
    .settings(headerSettings: _*)
    .settings(createAllHeaders: _*)
    .settings(docSettings: _*)
    
}


object ExtraCommands {

  /** Print the active project and current Git branch **/
  def stats = Command.command("stats") { state =>
    object devnull extends ProcessLogger {
      def info (s: => String) {}
      def error (s: => String) { }
      def buffer[T] (f: => T): T = f
    }

    val current = """\*\s+(\w+)""".r
    def gitBranches = ("git branch --no-color" lines_! devnull mkString)
    val currBranch = current findFirstMatchIn gitBranches map (_ group(1)) getOrElse "-"

    val currProject = Project.extract (state).currentProject.id

    println("Project: %s\nBranch: %s".format(currProject,currBranch))

    state
  }

  /** Package the jar-files, documentation and examples in a Zip-file that
    * can be uploaded to the website.
   **/
  def dist = Command.command("dist") { state =>
    val distdir = BuildSettings.buildName+"-"+BuildSettings.buildVersion
    //val curDir = System.getProperty("user.dir")
    val curdir:String = ("""pwd""" !!)
    println(curdir)
    println("Deleting "+distdir+". " + ("rm -rf "+distdir !!))
    println("Creating "+distdir+". " + ("mkdir -p "+distdir+"/models" !!))
    println("Copying sbt. " + ("mkdir -p "+distdir+"/project" !!))
    println("Copying sbt. " + ("cp project/Build.scala "+distdir+"/project/" !!))
    println("Copying sbt. " + ("cp project/plugin.sbt " +distdir+"/project/" !!))
    println("Copying jar. " + ("cp target/scala-"+BuildSettings.buildScalaVersionMajor+"/proguard/"+BuildSettings.buildName+"_"+BuildSettings.buildScalaVersionMajor+"-"+BuildSettings.buildVersion+".jar "+distdir+"/"+BuildSettings.buildJarName !!))
    println("Copying README.md. "  + ("cp README.md " +distdir+"/" !!))
    println("Copying README_dev.md. "  + ("cp README_dev.md " +distdir+"/" !!))
    println("Copying LICENSE. " + ("cp LICENSE "+distdir+"/" !!))
    println("Copying models. " + ("cp -R models  "+distdir+"/" !!))
    println("Copying source code. " + ("cp -R src  "+distdir+"/" !!))
    println("Making zip. " + ("zip -r "+distdir+".zip "+distdir !!))
    println("Created "+distdir+".zip")
    state
  }

  /* is this still used? */
  def dist_debug = Command.command("dist") { state =>
    println("Distribution is not allowed in debug mode. Switch to the main project ('project main')")
    state
  }

}

