module Examples.First where

import Parser
import Syntax
import Examples.Waffles
import Examples.OldFashioned
import Examples.Cookies

import Test.HUnit
import Text.Parsec.Error


instance Eq ParseError where
  (==) e1 e2 = (errorPos e1 == errorPos e2) &&
               (errorMessages e1 == errorMessages e2)

test = runTestTT tests

tests = TestList[ TestLabel "waffles"       waffles_test
                , TestLabel "oldFashioned"  oldFashioned_test
                , TestLabel "cookies"       cookies_test
                ]

mkTestCase s expected seen = TestCase(assertEqual s expected seen)

--Test for Waffles recipe
waffles_result  = parse parseProgram "" wafflesText
waffles_expects = Right wafflesAST
waffles_test    = mkTestCase "waffles" waffles_expects waffles_result

--Test for OldFashioned recipe
oldFashioned_result  = parse parseProgram "" oldFashionedText
oldFashioned_expects = Right oldFashionedAST
oldFashioned_test    = mkTestCase "oldFashioned" oldFashioned_expects oldFashioned_result

--Test for Cookies recipe
cookies_result  = parse parseProgram "" cookiesText
cookies_expects = Right cookiesAST
cookies_test    = mkTestCase "cookies" cookies_expects cookies_result

