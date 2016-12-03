chinesecalendar
===============

[![Build Status](https://secure.travis-ci.org/whily/chinesecalendar.png)](http://travis-ci.org/whily/chinesecalendar)

Chinesecalendar is a Scala library for Chinese Calendar.

Features to include:

* Chinese Calendar between BCE 841 to CE 1911.

For more information about Chinesecalendar, please go to
  <https://github.com/whily/chinesecalendar>

Wiki pages can be found at
  <https://wiki.github.com/whily/chinesecalendar>

Development
-----------

The following tools are needed to build Chinesecalendar from source:

* JDK version 6/7/8 from <http://www.java.com> if Java is not available.
  Note that JDK is preinstalled on Mac OS X and available via package manager
  on many Linux systems.
* Scala (2.11.8)
* sbt (0.13.13)

The project follows general sbt architecture, therefore normal sbt
commands can be used to build the library: compile, doc, test,
etc. For details, please refer
<http://scala.micronauticsresearch.com/sbt/useful-sbt-commands>.

Currently the library is not published to any public repository
yet. To use this library with your project, you need to download the
source code, and run `sbt publish-local` in your command line. Then,
include following line in your sbt configuration file.

          libraryDependencies += "net.whily" %% "chinesecalendar" % "0.2.1-SNAPSHOT"
