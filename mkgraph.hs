import Prelude

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph

import Data.Graph.Inductive.Dot

import Data.Graph.Inductive.Arbitrary
import Test.QuickCheck

import Text.LaTeX
import Data.String

graph2dot :: (Show a, Show b) => String -> Gr a b -> IO ()
graph2dot name graph = writeFile (name ++ ".dot") $ (showDot.fglToDot) graph

getCycloComp  :: (Show a, Show b) => Gr a b -> Int
getCycloComp graph = edges - nodes + p
  where
    nodes = (length.labNodes) graph
    edges = 1 + (length.labEdges) graph
    p = 1

makeReport :: (Show a, Show b, Monad m) => Gr a b -> LaTeXT_ m
makeReport graph = do
  documentclass [] article
  document $ do fromString "The Cyclomatic Complexity for this software is: "
                (fromString.show) $ getCycloComp graph

writeReport :: (Show a, Show b) => Gr a b -> String -> IO ()
writeReport graph name = execLaTeXT report >>= renderFile fileName
  where
    report = makeReport graph
    fileName = name ++ ".tex"

genLNodes :: [LNode String]
genLNodes = zip [1..5] ["A","B","C","D","E"]

genLEdges :: [LEdge String]
genLEdges = [(1,2, "A->B"),(2,3, "B->C"),(3,4, "C->D"),(4,5, "D->E")]

mygraph :: Gr String String
mygraph = mkGraph genLNodes genLEdges

exampleGraph :: IO ()
exampleGraph = graph2dot "exampleGraph" mygraph

exampleCycloComp :: Int
exampleCycloComp = getCycloComp mygraph

exampleCycloReport :: IO ()
exampleCycloReport = writeReport mygraph "simple"





testGraph ::  IO (Gr String Char)
testGraph = connArbGraph <$> generate arbitrary

testDot :: IO ()
testDot = testGraph >>= graph2dot "test"

testCycloReport :: Int -> IO ()
testCycloReport 0 = return ()
testCycloReport testNum = do graph <- testGraph
                             let fileName = show testNum
                             writeReport graph ("test_" ++ fileName ++ "_results")
                             graph2dot fileName graph
                             testCycloReport $testNum - 1
