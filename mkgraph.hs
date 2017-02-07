import Prelude

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph

import Data.Graph.Inductive.Dot

import Data.Graph.Inductive.Arbitrary
import Test.QuickCheck

import Text.LaTeX
import Text.LaTeX.Packages.Beamer
import Data.String

graph2dot :: (Show a, Show b) => String -> Gr a b -> IO ()
graph2dot name graph = writeFile (name ++ ".dot") $ (showDot.fglToDot) graph

getCycloComp  :: (Show a, Show b) => Gr a b -> Int
getCycloComp graph = edges - nodes + p
  where
    nodes = (length.labNodes) graph
    edges = 1 + (length.labEdges) graph
    p = 1

addGraphDoc :: (Monad m) => String -> LaTeXT_ m
addGraphDoc name = (frame.raw.fromString) $ "\\includedot[scale=0.20]{" ++ dotName ++ "}"
  where
    dotName = name ++ "Dot"

makeReport ::(Show a, Show b, Monad m) => Gr a b -> LaTeXT_ m
makeReport graph = frame $ do fromString "The Cyclomatic Complexity for this software is: "
                              (fromString.show) $ getCycloComp graph

reportOnly :: (Show a, Show b, Monad m) => Gr a b -> LaTeXT_ m
reportOnly graph = documentclass [] beamer <> doc
  where
    report = makeReport graph
    doc = document report

docAndGraph :: (Show a, Show b, Monad m) => Gr a b -> String -> LaTeXT_ m
docAndGraph graph reportName = do documentclass [] beamer
                                  raw.fromString $ "\\usepackage[pdftex]{graphicx}"
                                  raw.fromString $ "\\usepackage{graphviz}"
                                  doc
  where
    report = makeReport graph
    reportGraph = addGraphDoc reportName
    doc = document $ do (reportGraph <> report)


writeReport :: String ->  LaTeXT IO () -> IO ()
writeReport name report = createLatex >>= renderFile fileName
  where
    createLatex = execLaTeXT report
    fileName = name ++ ".tex"


------------
--Hand Examples
------------
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
exampleCycloReport = graph2dot "exampleGraph" mygraph
                  >> (writeReport "exampleReport" $ reportOnly mygraph)

exampleCycloReportGraph :: IO ()
exampleCycloReportGraph = graph2dot "exampleGraphDot" mygraph
                       >> (writeReport "exampleReport" $ docAndGraph mygraph "exampleGraph")


------------
--QuickCheck
------------
testGraph ::  IO (Gr String Char)
testGraph = connArbGraph <$> generate arbitrary

testDot :: IO ()
testDot = testGraph >>= graph2dot "test"

testCycloReport :: Int -> IO ()
testCycloReport 0 = return ()
testCycloReport testNum = do graph <- testGraph
                             let reportName = show testNum
                             graph2dot reportName graph
                             writeReport reportName $ reportOnly graph
                             testCycloReport $ testNum - 1

testCycloReportGraph :: Int -> IO ()
testCycloReportGraph 0 = return ()
testCycloReportGraph testNum = do graph <- testGraph
                                  let reportName = show testNum
                                  graph2dot (reportName ++ "Dot") graph
                                  writeReport reportName $ docAndGraph graph reportName
                                  testCycloReportGraph $ testNum - 1
