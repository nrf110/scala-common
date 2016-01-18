name := "common"

scalaVersion := Common.scalaVersion

organization := Common.organization

version := Common.version

lazy val util = (project in file("util"))

lazy val io = (project in file("io")).dependsOn(util)
