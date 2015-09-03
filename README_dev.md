% WFOMC development instructions

SBT
===

Last tested with Java 7, Scala 2.11.2, and SBT 0.13.6.

Compiling
---------
- `sbt clean`   - Clear target directory
- `sbt compile` - Compile code
- `sbt run`     - Run code interactively in SBT

Testing and documentation
-------------------------
- `sbt test`               - Run tests
- `sbt test-only liftedinference.TestMLNParser` - Run a single test
- `sbt test-only *Parser*` - Run all tests that contain 'Parser'
- `sbt doc`                - Run scaladoc on sources
- `sbt scoverage:test`     - Run code coverage of tests

Note: `sbt test` has assertions disabled; `sbt scoverage:test` has assertions enabled.

Packaging
---------
- `sbt createAllHeaders`  - Updates all source file headers (copyright)
- `sbt assembly`          - Create jar file to distribute
- `sbt proguard:proguard` - Create jar file to distribute using Proguard
- `sbt dist`              - Create zip file to distribute

Eclipse IDE
------------
- `sbt eclipse` - Create Eclipse .project files


---

vim:ft=markdown:expandtab

