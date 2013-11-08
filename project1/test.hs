import Test.HUnit
import Oska
---------------IMPORTANT-------------------------------
--- To run this you need to get the Test.HUnit module--
--- http://hunit.sourceforge.net/                    --
---													 --
---You also need to compile the Oska file into Oska.o--
---run $ ghc --make Oska.hs                          --
-------------------------------------------------------


---run test suites with  *Main> runTestTT tests

--- end of game tests
test1 = TestCase (assertEqual "" (True, -9) (endOfGame_o6o7 ["bbb","-w","-w-"] 'w'))
test2 = TestCase (assertEqual "" (False, 0) (endOfGame_o6o7 ["bbb","-w","-w-"] 'b'))
test3 = TestCase (assertEqual "" (True, 9) (endOfGame_o6o7 ["-w-","-w","bbb"] 'b'))
test4 = TestCase (assertEqual "" (True, 9) (endOfGame_o6o7 ["bbb","ww","bbb"] 'b'))  ---TODO: is test is failing, b should win.
test5 = TestCase (assertEqual "" (True, 9) (endOfGame_o6o7 ["---","bb","---"] 'b'))
test6 = TestCase (assertEqual "" (True, -9) (endOfGame_o6o7 ["---","bb","---"] 'w'))
--tests = TestList [test1]
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5, TestLabel "test6" test6]