/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.webapptest

import _root_.org.specs._
import _root_.org.specs.runner.JUnit3
import _root_.org.specs.runner.ConsoleRunner
import _root_.net.sourceforge.jwebunit.junit.WebTester

class ToHeadUsagesTest extends JUnit3(ToHeadUsages)
object ToHeadUsagesRunner extends ConsoleRunner(ToHeadUsages)


object ToHeadUsages extends Specification {
  JettyTestServer.start()

  "lift <head> merger" should {
    "merge <head> from html fragment" >> {
      JettyTestServer.browse(
        "/htmlFragmentWithHead",
         _.assertElementPresentByXPath("/html/head/script[@id='fromFrag']")
      )
    }

    "merge <head> from html fragment does not include head element in body" >> {
      JettyTestServer.browse(
        "/htmlFragmentWithHead",
         _.assertElementNotPresentByXPath("/html/body/script[@id='fromFrag']")
      )
    }

    "merge <head> from snippet" >> {
      JettyTestServer.browse(
        "/htmlSnippetWithHead",
         _.assertElementPresentByXPath("/html/head/script[@src='snippet.js']")
      )
    }
    
    "not merge for bodyless html" >> {
      JettyTestServer.browse(
        "/basicDiv",html => {
          html.assertElementPresent("fruit")
          html.assertElementPresent("bat")
        }
      )
    }

    "not merge for headless bodyless html" >> {
      JettyTestServer.browse(
        "/h1",html => {
          html.assertElementPresent("h1")
        }
      )
    }

    "not merge for headless body html" >> {
      JettyTestServer.browse(
        "/body_no_head",html => {
          html.assertElementPresentByXPath("/html/body/head/div")
        }
      )
    }

    "not merge non-html" >> {
      JettyTestServer.browse(
        "/non_html",html => {
          html.assertElementPresent("frog")
        }
      )
    }

  }

  "deferred snippets" should {
    "render" >> {
      JettyTestServer.browse(
        "/deferred",html => {
          html.assertElementPresent("second")
        }
      )
    }

    "not deferred not in actor" >> {
      JettyTestServer.browse(
        "/deferred",html => {
          html.assertElementPresentByXPath("/html/body/span[@id='whack1']/span[@id='actor_false']")
        }
      )
    }

    "deferred in actor" >> {
      JettyTestServer.browse(
        "/deferred",html => {
          html.assertElementPresentByXPath("/html/body/span[@id='whack2']/span[@id='actor_true']")
        }
      )
    }
  }

  //  JettyTestServer.stop()
}
