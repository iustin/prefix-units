{-

Copyright 2012, 2014, Google Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following disclaimer
in the documentation and/or other materials provided with the
distribution.
    * Neither the name of Google Inc. nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

{- | Tests for the Data.Prefix.Units module

-}

module Main (main) where

import Data.Char (toUpper)
import Data.List
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.HUnit hiding (Test)

import Data.Prefix.Units

-- * Test helpers

failTest :: String -> Property
failTest msg = printTestCase msg False

-- | Checks for equality with proper annotation.
(==?) :: (Show a, Eq a) => a -> a -> Property
(==?) x y = printTestCase
            ("Expected equality, but '" ++
             show x ++ "' /= '" ++ show y ++ "'") (x == y)
infix 3 ==?

-- | Formats a failed-to-parse-unit message.
failParseUnit :: Unit -> String -> Property
failParseUnit unit err =
  failTest $ "Failed to parse unit '" ++ unitName unit ++ "': " ++ err

-- | Fails unless we received an error message that passes a given
-- condition.
expectParseFailure :: (Show a) =>
                      (String -> Bool)
                   -> Either String a
                   -> Property
expectParseFailure fn (Left err) =
  printTestCase "Unexpected error message" $ fn err
expectParseFailure _  (Right v)  =
  failTest $ "Unexpected parse with result " ++ show v

-- | Expect a parse result to be a given value.
expectParse :: (Real a, Real b) => a -> Unit -> Either String b -> Property
expectParse _ unit (Left err) = failParseUnit unit err
expectParse v unit (Right v') =
  printTestCase "Parsed wrong value: " $
  toRational v' ==? toRational v * unitMultiplier unit

-- * Instances

instance Arbitrary Unit where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary FormatMode where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary ParseMode where
  arbitrary = elements [minBound..maxBound]

allUnits :: [Unit]
allUnits = [minBound..maxBound]

-- * Actual tests

-- ** Definitions

testUniqueNames :: Assertion
testUniqueNames =
  let names = map unitName allUnits
  in nub names @=? names

testUniqueSymbols :: Assertion
testUniqueSymbols =
  let symbols = map unitSymbol allUnits
  in  nub symbols @=? symbols

testUniqueFancySymbols :: Assertion
testUniqueFancySymbols =
  let symbols = map fancySymbol allUnits
  in nub symbols @=? symbols

testOrdering :: Assertion
testOrdering = do
  let si_mult = map unitMultiplier $ sort siUnits
      bin_mult = map unitMultiplier $ sort binaryUnits
      all_mult = map unitMultiplier allUnits -- no sorting here!
  sort si_mult @=? si_mult
  sort bin_mult @=? bin_mult
  sort all_mult @=? all_mult

testSIBinary :: Assertion
testSIBinary =
  assertBool "SI unit lists contain binary prefixes" $
               null (siUnits `intersect` binaryUnits) &&
               null (siKMGT `intersect` binaryUnits) &&
               null (siSupraunitary `intersect` binaryUnits)

-- ** Parsing

testNullUnitInt :: Int -> ParseMode -> Property
testNullUnitInt v pmode =
  case parseValue pmode (show v) of
    Left err -> failTest $ "Failed to parse empty unit: " ++ err
    Right v' -> printTestCase "Parsed wrong value:" (v ==? v')

testNullUnitFrac :: Double -> ParseMode -> Property
testNullUnitFrac d pmode =
  case parseValue pmode (show d) of
    Left err -> failTest $ "Failed to parse empty unit: " ++ err
    Right d' -> printTestCase "Parsed wrong value:" (d ==? d')

testSymbolParsingExact :: Unit -> Property
testSymbolParsingExact unit =
  case parseExactSymbol (unitSymbol unit) of
    Left err -> failParseUnit unit err
    Right unit' -> printTestCase "Parsed wrong unit: " (unit ==? unit')

-- | Binary units should parse themselves in explicit binary mode.
testSymbolParsingBinary :: Property
testSymbolParsingBinary =
  forAll (elements binaryUnits) $ \unit ->
  case parseBinarySymbol (unitSymbol unit) of
    Left err -> failParseUnit unit err
    Right unit' -> printTestCase "Parsed wrong unit: " (unit ==? unit')

-- | Binary units should parse themselves in all parsing modes.
testSymbolParsingBinaryAbbrev :: Property
testSymbolParsingBinaryAbbrev =
  forAll (elements binaryUnits) $ \unit ->
  case parseSymbol ParseBinary (take 1 (unitSymbol unit)) of
    Left err -> failParseUnit unit err
    Right unit' -> printTestCase "Parsed wrong unit: " (unit ==? unit')

-- | Binary units should parse themselves in all parsing modes.
testSymbolParsingBinaryAll :: ParseMode -> Property
testSymbolParsingBinaryAll mode =
  forAll (elements binaryUnits) $ \unit ->
  case parseSymbol mode (unitSymbol unit) of
    Left err -> failParseUnit unit err
    Right unit' -> printTestCase "Parsed wrong unit: " (unit ==? unit')

-- | Fail to parse invalid symbols in any mode.
testSymbolParsingFail :: ParseMode -> Property
testSymbolParsingFail mode =
  expectParseFailure (("NO-SUCH" `isInfixOf`) . map toUpper) $
    parseSymbol mode "no-such"

-- | Parsed values should be correct.
testParsingIntKMGT :: Int -> Property
testParsingIntKMGT v =
  forAll (elements (siKMGT ++ binaryUnits)) $ \unit ->
  let str = show v ++ unitSymbol unit in
  expectParse v unit (parseValue ParseKMGT str::Either String Integer)

-- | Parsed values should be correct.
testParsingInt :: Int -> Property
testParsingInt v =
  forAll (elements (siSupraunitary ++ binaryUnits)) $ \unit ->
  let str = show v ++ unitSymbol unit in
  expectParse v unit (parseValue ParseExact str::Either String Integer)

-- | Parsed values should be correct.
--
-- Note that this tests floating point number equality, so it could be
-- flaky.
testParsingDouble :: Unit -> Double -> Property
testParsingDouble unit d =
  let str = show d ++ unitSymbol unit in
  case parseValue ParseExact str::Either String Double of
    Left err -> failParseUnit unit err
    Right d' -> printTestCase ("Parsing of " ++ str ++ " failed: ") $
                d' ==? fromRational (toRational d * unitMultiplier unit)

-- | Parsed values should be correct.
testParsingRational :: Unit -> Integer -> Property
testParsingRational unit v =
  let str = show v ++ "%1" ++ unitSymbol unit in
  expectParse v unit (parseValue ParseExact str::Either String Rational)

testFailParsing :: ParseMode -> Int -> Property
testFailParsing pmode v =
  expectParseFailure ("no-such" `isInfixOf`)
    (parseValue pmode ("no-such" ++ show v)::Either String Int)

-- | Test fail parse on required unit.
testParsingRequired :: ParseMode -> Int -> Property
testParsingRequired pmode v =
  expectParseFailure ("is required" `isInfixOf`)
    (parseGeneric UnitRequired [] pmode (show v)::Either String Int)

testParsingDefault :: Unit -> ParseMode -> Rational -> Property
testParsingDefault unit pmode v =
  expectParse v unit
    (parseGeneric (UnitDefault unit) [] pmode (show v)::Either String Rational)

testParsingInvalidList :: Unit -> [Unit] -> Int -> Property
testParsingInvalidList unit valid v =
  unit `notElem` valid && not (null valid) ==>
  expectParseFailure ("not part of the accepted unit list" `isInfixOf`)
    (parseGeneric UnitOptional valid ParseExact
       (show v ++ unitSymbol unit)::Either String Int)

testParsingValidList :: Unit -> [Unit] -> Rational -> Property
testParsingValidList unit valid v =
  unit `elem` valid && not (null valid) ==>
  expectParse v unit
   (parseGeneric UnitOptional valid ParseExact
      (show v ++ unitSymbol unit)::Either String Rational)

-- ** Formatting

testTrivialFormattingRec :: FormatMode -> Property
testTrivialFormattingRec fmt =
  recommendedUnit fmt (0::Int) ==? Nothing .&&.
  recommendedUnit fmt (0::Double) ==? Nothing

testTrivialFormattingFmt :: FormatMode -> Property
testTrivialFormattingFmt fmt =
  formatValue (Left fmt) (0::Int) ==? (0, Nothing) .&&.
  formatValue (Left fmt) (0::Double) ==? (0, Nothing)

testTrivialFormattingShow :: FormatMode -> Property
testTrivialFormattingShow fmt =
  showValue (Left fmt) (0::Int) ==? "0" .&&.
  showValue (Left fmt) (0::Double) ==? show (0::Double)

testRecommend :: Property
testRecommend =
  forAll (elements [FormatSiAll, FormatBinary]) $ \fmt ->
  forAll (elements (unitRange fmt)) $ \unit ->
  let value = unitMultiplier unit in
  case recommendedUnit fmt value of
    Nothing -> failTest $ "Expected recommendation of unit " ++
               unitName unit ++ " for " ++ show value ++ " but got nothing"
    Just unit' -> printTestCase ("Mismatch in recommended unit for value " ++
                                 show value ++ ": ") $ unit ==? unit'

testForceUnit :: Unit -> Rational -> Property
testForceUnit unit v =
  case formatValue (Right unit) v of
    (v', Just u') -> printTestCase "Invalid value/unit computed" $
                     unit ==? u' .&&.
                     v ==? v' * unitMultiplier unit
    x -> failTest $ "Invalid result from formatValue: " ++ show x

testFormatIntegral :: Property
testFormatIntegral =
  forAll (elements [FormatSiSupraunitary, FormatBinary]) $ \fmt ->
  forAll (elements (unitRange fmt)) $ \unit ->
  let fmted = formatValue (Left fmt) . truncate . unitMultiplier $ unit
  in fmted ==? (1::Integer, Just unit)

testFormatFractional :: Property
testFormatFractional =
  forAll (elements [FormatSiSupraunitary, FormatBinary]) $ \fmt ->
  forAll (elements (unitRange fmt)) $ \unit ->
  let fmted = formatValue (Left fmt) . unitMultiplier $ unit
  in fmted ==? (1::Rational, Just unit)

testShowIntegralBinary :: Property
testShowIntegralBinary =
  forAll (elements binaryUnits) $ \ unit ->
  let value = truncate (unitMultiplier unit)::Integer in
  printTestCase ("Formatting/showing unit " ++ show unit) $
   showValue (Left FormatBinary) value ==? '1' : unitSymbol unit

testShowRational :: Unit -> Property
testShowRational unit =
  let fmtmode = if unit `elem` binaryUnits then FormatBinary else FormatSiAll
      value = unitMultiplier unit
  in printTestCase ("Formatting/showing unit " ++ show unit) $
     showValue (Left fmtmode) value ==? "1 % 1" ++ unitSymbol unit

-- * Test harness

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "definitions"
    [ testCase "unique names" testUniqueNames
    , testCase "unique symbols" testUniqueSymbols
    , testCase "unique fancy symbols" testUniqueFancySymbols
    , testCase "ordering" testOrdering
    , testCase "type mixup" testSIBinary
    ]
  , testGroup "parsing"
    [ testProperty "null unit integral" testNullUnitInt
    , testProperty "null unit fractional" testNullUnitFrac
    , testProperty "symbol parsing/exact" testSymbolParsingExact
    , testProperty "symbol parsing/binary" testSymbolParsingBinary
    , testProperty "symbol parsing/binary-all" testSymbolParsingBinaryAll
    , testProperty "symbol parsing/binary-abbrev" testSymbolParsingBinaryAbbrev
    , testProperty "symbol parsing/failure" testSymbolParsingFail
    , testProperty "parsing/integral-kmgt" testParsingIntKMGT
    , testProperty "parsing/integral" testParsingInt
    , testProperty "parsing/fractional" testParsingDouble
    , testProperty "parsing/rational" testParsingRational
    , testProperty "parsing/failure" testFailParsing
    , testProperty "parsing/required unit" testParsingRequired
    , testProperty "parsing/default" testParsingDefault
    , testProperty "parsing/invalid-list" testParsingInvalidList
    , testProperty "parsing/valid-list" testParsingValidList
    ]
  , testGroup "formatting"
    [ testProperty "trivial formatting/recommend" testTrivialFormattingRec
    , testProperty "trivial formatting/fmt" testTrivialFormattingFmt
    , testProperty "trivial formatting/show" testTrivialFormattingShow
    , testProperty "recommend" testRecommend
    , testProperty "force unit" testForceUnit
    , testProperty "format/int" testFormatIntegral
    , testProperty "format/frac" testFormatFractional
    , testProperty "show/integral binary" testShowIntegralBinary
    , testProperty "show/rational" testShowRational
    ]
  ]
