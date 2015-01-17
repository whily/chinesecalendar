chinesecalendar
===============

Chinesecalendar is a Scala library for Chinese Calendar.

Features to include:

* Chinese Calendar between AD 1 to 317.

For more information about Chinesecalendar, please go to
  <https://github.com/whily/chinesecalendar>

Wiki pages can be found at
  <https://wiki.github.com/whily/chinesecalendar>

Development
-----------

The following tools are needed to build Chinesecalendar from source:

* JDK version 6/7 from <http://www.java.com> if Java is not available. 
  Note that JDK is preinstalled on Mac OS X and available via package manager
  on many Linux systems. 
* Scala (2.11.0)
* sbt (0.13.5)
  
The project follows general sbt architecture, therefore normal sbt
commands can be used to build the library: compile, doc, test,
etc. For details, please refer
<http://scala.micronauticsresearch.com/sbt/useful-sbt-commands>.

Currently the library is not published to any public repository
yet. To use this library with your project, you need to download the
source code, and run `sbt publish-local` in your command line. Then,
include following line in your sbt configuration file.

          libraryDependencies += "net.whily" %% "chinesecalendar" % "0.0.1-SNAPSHOT"
