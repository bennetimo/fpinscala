package io.coderunner.fpinscala

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

abstract class UnitTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks
