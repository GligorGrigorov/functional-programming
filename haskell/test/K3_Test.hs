module K3_Test where

import Test.HUnit
import K3  

sampleTree1 = Node 'a' (Node 'b' (Node 'd' EmptyTree
                                          (Node 'g' EmptyTree EmptyTree))
                                (Node 'e' EmptyTree EmptyTree))
                      (Node 'c' EmptyTree
                                (Node 'f' EmptyTree EmptyTree))
sampleTree2 = Node 'a' EmptyTree EmptyTree
sampleTree3 = EmptyTree
sampleTree4 = Node 'a' (Node 'b' (Node 'c' EmptyTree EmptyTree) EmptyTree) EmptyTree
sampleTree5 = Node 'e' EmptyTree (Node 'f' EmptyTree (Node 'g' EmptyTree EmptyTree))                                     

sampleTree1Expected = ["abdg", "abe", "acf"]
sampleTree2Expected = ["a"]
sampleTree3Expected = []
sampleTree4Expected = ["abc"]
sampleTree5Expected = ["efg"]

test1 = TestCase $ assertEqual "sample tree 1 check" sampleTree1Expected (treeWords sampleTree1)
test2 = TestCase $ assertEqual "sample tree 2 check" sampleTree2Expected (treeWords sampleTree2)
test3 = TestCase $ assertEqual "sample tree 3 check" sampleTree3Expected (treeWords sampleTree3)
test4 = TestCase $ assertEqual "sample tree 4 check" sampleTree4Expected (treeWords sampleTree4)
test5 = TestCase $ assertEqual "sample tree 5 check" sampleTree5Expected (treeWords sampleTree5)

tl1 = TestList [test1,test2, test3,test4, test5]

-- Tests for task 2

samplef1 :: Integral t => (t -> t)
samplef1 x = x + 1

samplef2 :: Integral t => (t -> t)
samplef2 x = x * x

samplef3 :: Integral t => (t -> t)
samplef3 x = if x == 10 then x - 5 else x

t1ExpectedInterval = (2,8)
t2ExpectedInterval = (1,49)
t3ExpectedInterval = (5,11)
t1 = TestCase $ assertEqual "mapTo first function check" t1ExpectedInterval (mapsTo samplef1 1 7)
t2 = TestCase $ assertEqual "mapTo second function check" t2ExpectedInterval (mapsTo samplef2 1 7)
t3 = TestCase $ assertEqual "mapTo third function check" t3ExpectedInterval (mapsTo samplef3 8 11)

tl2 = TestList [t1, t2, t3]

main = do
  runTestTT tl1
  runTestTT tl2