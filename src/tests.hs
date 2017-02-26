import Test.HUnit
import qualified BStack as BS
import qualified BProgramCounter as BPC
import qualified BMemory as BM
import Data.Array.IO
import Types
import qualified BInstructions as BI

--------TEST CASES FOR BSTACK-------

--build a stack from a list.
--The end of the list is the top of the stack
--stackFromList [1, 2, 3] = a stack with 3 at the top and 1 at the bottom.
createStack :: [Int] -> BS.BStack
createStack s = build s BS.empty
   where
      build [] acc     = acc
      build (s:ss) acc = build ss (BS.push acc s)

-------push---------
stackTest1 = TestCase (assertEqual "push empty 1" (createStack [1]) (BS.push BS.empty 1))
stackTest2 = TestCase (assertEqual "push empty 0" (BS.empty) (BS.push BS.empty 0))
stackTest3 = TestCase (assertEqual "push [1,2,3] 4" (createStack [1, 2, 3, 4]) (BS.push (createStack [1, 2, 3]) 4))

-------pop-----------
stackTest4 = TestCase (assertEqual "pop empty" (BS.empty, 0) (BS.pop BS.empty))
stackTest5 = TestCase (assertEqual "pop [1]" (BS.empty, 1) (BS.pop (createStack [1])))
stackTest6 = TestCase (assertEqual "pop [1,2,3]" (createStack [1, 2], 3) (BS.pop (createStack [1, 2, 3])))

-------top-----------
stackTest7 = TestCase (assertEqual "top empty" 0 (BS.top BS.empty))
stackTest8 = TestCase (assertEqual "top [1,2,3]" 3 (BS.top (createStack [1, 2, 3])))

stackTests = TestList [
                     TestLabel "stackTest1" stackTest1,
                     TestLabel "stackTest2" stackTest2,
                     TestLabel "stackTest3" stackTest3,
                     TestLabel "stackTest4" stackTest4,
                     TestLabel "stackTest5" stackTest5,
                     TestLabel "stackTest6" stackTest6,
                     TestLabel "stackTest7" stackTest7,
                     TestLabel "stackTest8" stackTest8
                     ]

------------------------------------

---TEST CASES FOR BPROGRAMCOUNTER---
--assuming width = 80 and height = 25 (defined in Types.hs)

createPC :: Position -> Direction -> StringMode -> BPC.BProgramCounter
createPC p d s = BPC.setStringMode (BPC.setDirection (BPC.setPosition BPC.starting p) d) s

--reverse
counterTest1 = TestCase (assertEqual "reverse starting" (createPC (0, 0) West False) (BPC.reverse BPC.starting))

--step
counterTest2 = TestCase (assertEqual "step starting" (createPC (1, 0) East False) (BPC.step BPC.starting))
counterTest3 = TestCase (assertEqual "step case 1" (createPC (0, 2) South False) (BPC.step (createPC (0, 1) South False)))
counterTest4 = TestCase (assertEqual "step case 2 wrapa round" (createPC (79, 0) West False) (BPC.step (createPC (0, 0) West False)))
counterTest5 = TestCase (assertEqual "step case 3 wrapa round" (createPC (50, 0) South False) (BPC.step (createPC (50, 24) South False)))

--some set and get
counterTest6 = TestCase (assertEqual "isStringMode" True (BPC.isStringMode (createPC (0, 0) North True)))
counterTest7 = TestCase (assertEqual "setDirection" (createPC (0, 0) South False) (BPC.setDirection BPC.starting South))

counterTests = TestList [
                        TestLabel "counterTest1" counterTest1,
                        TestLabel "counterTest2" counterTest2,
                        TestLabel "counterTest3" counterTest3,
                        TestLabel "counterTest4" counterTest4,
                        TestLabel "counterTest5" counterTest5,
                        TestLabel "counterTest6" counterTest6,
                        TestLabel "counterTest7" counterTest7
                        ]

------------------------------------

--------------BMEMORY---------------

--the tiny 3x3 grid we will perform tests on
-- a point (.) represents a space and not the actual befunge command
{-
.><
^0$
.#.
-}
memoryTest1 = TestCase $ do
   memory <- newArray ((0, 0), (2, 2)) ' ' :: IO BM.BMemory --initialize new array
   BM.buildMemory memory [" ><", "^0$", " # "] --loading the tiny grid into memory

   val1 <- BM.getValue memory (1, 0)
   assertEqual "get value at (1, 0)" '>' val1

   BM.putValue memory (1, 0) '?' --change the value on that position

   val2 <- BM.getValue memory (1, 0)
   assertEqual "get value at (1, 0) after change" '?' val2

--fullsize grid set to the size of width and height in Types.hs
{-
.><.................. >
^0$.................. >
.#................... >
..................... >
..................... >

v v v v v v v v v v v
-}
memoryTest2 = TestCase $ do
   memory <- newArray ((0, 0), (width-1, height-1)) ' ' :: IO BM.BMemory --initialize new array
   BM.buildMemory memory [" ><", "^0$", " # "]

   val1 <- BM.getValue memory (1, 0)
   assertEqual "get value at (1, 0)" '>' val1

   val2 <- BM.getValue memory (81, 25) --the grid wraps around
   assertEqual "get value at (81, 25) wrap around" '>' val2

memoryTests = TestList [
                        TestLabel "memoryTest1" memoryTest1,
                        TestLabel "memoryTest2" memoryTest2
                        ]

------------------------------------

----------BInstructions---------------

--add
instructionTest1 = TestCase (assertEqual "add [1, 2]" (createStack [3]) (BI.add (createStack [1, 2])))
instructionTest2 = TestCase (assertEqual "add [1]" (createStack [1]) (BI.add (createStack [1])))

--subtract
instructionTest3 = TestCase (assertEqual "subtract [1, 2]" (createStack [-1]) (BI.subtract (createStack [1, 2])))

--multiply
instructionTest4 = TestCase (assertEqual "multiply [1, 2]" (createStack [2]) (BI.multiply (createStack [1, 2])))

--divide
instructionTest5 = TestCase (assertEqual "divide [1, 2]" (createStack []) (BI.divide (createStack [1, 2])))

--modulo
instructionTest6 = TestCase (assertEqual "modulo [5, 2]" (createStack [1]) (BI.modulo (createStack [5, 2])))

--logicalNot
instructionTest7 = TestCase (assertEqual "logicalNot []" (createStack [1]) (BI.logicalNot (createStack [])))

--greaterThan
instructionTest8 = TestCase (assertEqual "greaterThan [1, 2]" (createStack []) (BI.greaterThan (createStack [1, 2])))

--ifs
instructionTest9 = TestCase (assertEqual "ifHorizontal [1, 2] PC ((0, 0) South False)"
                              (createStack [1], createPC (0, 0) West False)
                              (BI.ifHorizontal (createStack [1, 2]) (createPC (0, 0) South False)))

instructionTest10 = TestCase (assertEqual "ifVertical [] PC ((0, 0) West True)"
                              (createStack [], createPC (0, 0) South True)
                              (BI.ifVertical (createStack []) (createPC (0, 0) West True)))

--duplicate
instructionTest11 = TestCase (assertEqual "duplicate []" (createStack []) (BI.duplicate (createStack [])))
instructionTest12 = TestCase (assertEqual "duplicate [42, 5, 6]" (createStack [42, 5, 6, 6]) (BI.duplicate (createStack [42, 5, 6])))

--swap
instructionTest13 = TestCase (assertEqual "swap [1]" (createStack [1, 0]) (BI.swap (createStack [1])))
instructionTest14 = TestCase (assertEqual "swap [1, 2]" (createStack [2, 1]) (BI.swap (createStack [1, 2])))

--discard
instructionTest15 = TestCase (assertEqual "discard [1, 2]" (createStack [1]) (BI.discard (createStack [1, 2])))

--get and put ascii
--same grid as in memoryTest2
instructionTest16 = TestCase $ do
   memory <- newArray ((0, 0), (width-1, height-1)) ' ' :: IO BM.BMemory --initialize new array
   BM.buildMemory memory [" ><", "^0$", " # "]
   let stack = createStack [1, 2, 65, 1, 2]

   stack' <- BI.putASCII memory stack --should have put 'A' at position (1, 2)

   stack'' <- BI.getASCII memory stack' --gets the character at (1, 2) and puts it on the stack

   assertEqual "get changed value from memory onto stack" (createStack [65]) stack''


instructionTests = TestList [
                              TestLabel "instructionTest1" instructionTest1,
                              TestLabel "instructionTest2" instructionTest2,
                              TestLabel "instructionTest3" instructionTest3,
                              TestLabel "instructionTest4" instructionTest4,
                              TestLabel "instructionTest5" instructionTest5,
                              TestLabel "instructionTest6" instructionTest6,
                              TestLabel "instructionTest7" instructionTest7,
                              TestLabel "instructionTest8" instructionTest8,
                              TestLabel "instructionTest9" instructionTest9,
                              TestLabel "instructionTest10" instructionTest10,
                              TestLabel "instructionTest11" instructionTest11,
                              TestLabel "instructionTest12" instructionTest12,
                              TestLabel "instructionTest13" instructionTest13,
                              TestLabel "instructionTest14" instructionTest14,
                              TestLabel "instructionTest15" instructionTest15,
                              TestLabel "instructionTest16" instructionTest16
                           ]

------------------------------------

allTests = TestList [stackTests, counterTests, memoryTests, instructionTests]
