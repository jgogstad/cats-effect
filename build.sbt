/*
 * Copyright 2018 Daniel Spiewak
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

name := "ce3"

baseVersion in ThisBuild := "0.1"

organization in ThisBuild := "com.codecommit"
publishGithubUser in ThisBuild := "djspiewak"
publishFullName in ThisBuild := "Daniel Spiewak"

val CatsVersion = "2.1.1"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
addCompilerPlugin("io.tryp" % "splain" % "0.5.5" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % CatsVersion,
  "org.typelevel" %% "cats-free" % CatsVersion,
  "com.codecommit" %% "coop" % "0.4.0",
  "org.typelevel" %% "cats-laws" % CatsVersion % Test,
  "org.typelevel" %% "discipline-specs2" % "1.0.0" % Test,
  "org.specs2" %% "specs2-scalacheck" % "4.8.1" % Test
)
