{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.MajorityJudgement
  ( summarizeGrades,
  )
where

import CSDC.Types.Election
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Char (ord)
import Data.Csv
import Data.Foldable (foldl', toList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as Text
import System.Process (readProcess)

--------------------------------------------------------------------------------
-- Execute command

summarizeGrades :: [HashMap ElectionChoice Grade] -> IO (HashMap ElectionChoice Int)
summarizeGrades votes = do
  let grades = mergeGrades votes
  output <- readProcess "majority-judgement" [] $ toInput grades
  pure $ fromOutput output

--------------------------------------------------------------------------------
-- Merging grades

data Grades = Grades
  { excellent :: Int,
    veryGood :: Int,
    good :: Int,
    acceptable :: Int,
    bad :: Int,
    veryBad :: Int
  }

zero :: Grades
zero = Grades 0 0 0 0 0 0

add :: Grades -> Grades -> Grades
add (Grades e1 vg1 g1 a1 b1 vb1) (Grades e2 vg2 g2 a2 b2 vb2) =
  Grades (e1 + e2) (vg1 + vg2) (g1 + g2) (a1 + a2) (b1 + b2) (vb1 + vb2)

fromGrade :: Grade -> Grades
fromGrade = \case
  GradeExcellent -> zero {excellent = 1}
  GradeVeryGood -> zero {veryGood = 1}
  GradeGood -> zero {good = 1}
  GradeAcceptable -> zero {acceptable = 1}
  GradeBad -> zero {bad = 1}
  GradeVeryBad -> zero {veryBad = 1}

mergeGrades :: [HashMap ElectionChoice Grade] -> HashMap ElectionChoice Grades
mergeGrades = foldl' (HashMap.unionWith add) HashMap.empty . fmap (fmap fromGrade)

--------------------------------------------------------------------------------
-- Input and output

toLine :: (ElectionChoice, Grades) -> NamedRecord
toLine (ElectionChoice choice, Grades {..}) =
  [ ("Candidates", toField choice),
    ("Reject", toField veryBad),
    ("Poor", toField bad),
    ("Acceptable", toField acceptable),
    ("Good", toField good),
    ("VeryGood", toField veryGood),
    ("Excellent", toField excellent)
  ]

toInput :: HashMap ElectionChoice Grades -> String
toInput =
  let csvOptions =
        defaultEncodeOptions
          { encUseCrLf = False
          }
      csvHeader =
        [ "Candidates",
          "Reject",
          "Poor",
          "Acceptable",
          "Good",
          "VeryGood",
          "Excellent"
        ]
   in ByteString.unpack
        . encodeByNameWith csvOptions csvHeader
        . fmap toLine
        . HashMap.toList

fromOutput :: String -> HashMap ElectionChoice Int
fromOutput output =
  let decodeOptions =
        defaultDecodeOptions
          { decDelimiter = fromIntegral (ord '\t')
          }

      toPair hmap =
        ( ElectionChoice $ hmap HashMap.! "Candidates",
          read $ Text.unpack $ hmap HashMap.! "Grades"
        )
   in case decodeByNameWith @(HashMap Field Text) decodeOptions (ByteString.pack output) of
        Left e ->
          error e
        Right (_, vals) ->
          HashMap.fromList $ fmap toPair $ toList vals
