{-
    Name:       Fengkai Wu
    Department: University of Melbourne
    ID:         936424
    File summary:
                This file is written for solving "The Game of ChordProbe"
                Which is the first project of COMP90048 2017 SM2
                Declarative Programming.
    Version:    1.0
    Use in GHCI:
                :load Proj1.hs
    Details of implementation:
                This program implements the best guessing technique.
                It iterates all possible candidates and compares weight number
                of their possibility of left chords before each guess. By
                doing this, the program is able to make best choose in every
                guess, and it is able to get correct answer within an average
                of 4.3 guesses.
                This program also includes a function for easily testing the
                average guesses called "o0TestAverageResult".
-}
module Proj1 (initialGuess, nextGuess, GameState) where
import Data.Char

{-
    The game state contains a list of pre-solved feedbacks and their
    corresponding candidates sets pairs from last guess where:
    (Int, Int, Int) is possible feedback
    [[String]] is a list of candidate chords
    Then ((Int,Int,Int),[[String]]) is the feedback with its corresponding
    left candidate chords
-}
type GameState = [((Int,Int,Int),[[String]])]

{-  This function makes first guess.-}
initialGuess :: ([String],GameState)
initialGuess = (firstGuess,gamestate)
    where
        firstGuess = ["A1","B2","C3"]
        gamestate = o0PossibleCandidateSets firstGuess o0AllPossibleTarget
        
{-  This function makes guesses except first guess.-}
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (_,possibleCandidateSets) key = (bestGuess,bestGuessesCandidateSets)
    where 
        candidateSet = o0FindCandidateSet key possibleCandidateSets
        (_, bestGuess, bestGuessesCandidateSets)
            = o0BestChoise candidateSet candidateSet

{-  
    This function can run program many times and calculate the average guesses
    times.
    How to use:
        o0TestAverageResult {number of times to run}
-}
o0TestAverageResult :: Int -> Double
o0TestAverageResult number 
    = (fromIntegral (sum resultList)) / (fromIntegral (length resultList))
    where 
        mod = fst (divMod 1330 number)
        resultList = o0PartTester mod o0AllPossibleTarget

{-  
    This function can run program once and return times of guess the 
    program made with the last game state.
    How to use:
        (num, guess, gamestate) = o0TestOnce {chord to guess} ([],[])
            Do not need to change ([],[])
-}
o0TestOnce :: [String] -> ([String],GameState) -> (Int, [String],GameState)
o0TestOnce target ([],_) = (num+1, guess, gamestate)
    where
        (num, guess, gamestate) = o0TestOnce target initialGuess
o0TestOnce target (lastGuess, lastGamestate) 
    = if a == 3 then (0,[],[]) else (num+1, guess, gamestate)
    where
        (a,b,c) = o0FeedBack lastGuess target
        (num, guess, gamestate) 
            = o0TestOnce target (nextGuess (lastGuess, lastGamestate) (a,b,c))
   
{-  
    This function can run program a number times with the input chord list and
    return a list of times of guesses.
    How to use:
        o0Tester [["A1","A2","A3"],...,["B1","C2","D3"]]
-}
o0Tester :: [[String]] -> [Int]
o0Tester [] = []
o0Tester list = num:o0Tester (drop 1 list)
    where (num,_,_) = o0TestOnce (head list) ([],[])

{-  
    This function can run program a number times with the input chord list,
    and return a list of times of guesses. However, this function only tests
    chords' with index that can be divisible by input Integer.
    How to use:
        o0PartTester 3 [["A1","A2","A3"],...,["B1","C2","D3"]]
            above will test every three chord
        o0PartTester 1 [["A1","A2","A3"],...,["B1","C2","D3"]]
            above will test every chord
-}
o0PartTester :: Int -> [[String]] -> [Int]
o0PartTester _ [] = []
o0PartTester index list = if (snd (divMod (length list) index)) == 0 
    then num:o0PartTester index (drop 1 list)
    else o0PartTester index (drop 1 list)
    where (num,_,_) = o0TestOnce (head list) ([],[])

{-  
    This function can remove one element at specified index from a list.
    How to use:
        remove 2 "word"
            above result: wod
-}
remove :: Int -> [a] -> [a]
remove index list = (take index list)++(drop (index+1) list)

{-  
    This function can count the number of same string between two string list.
    How to use:
        o0CorrectNum ["A1","A2","A3"] 0 ["B2","A2","A1"] 0
            above result: 2
            the first 0 is the running index for ["A1","A2","A3"]
            the second 0 is the running index for ["B2","A2","A1"]
            keep both values 0, do not need to change
-}
o0CorrectNum :: [String] -> Int -> [String] -> Int
    -> (Int, [String], [String])
o0CorrectNum [] _ target _ = (0,[],target)
o0CorrectNum guess _ [] _ = (0,guess,[])
o0CorrectNum guess guessIndex target targetIndex 
    = if guessIndex >= length guess || targetIndex >= length target
        then (0,guess,target) 
        else (if stringEqual then correctPitch+1 
            else correctPitch, leftGuessPitch, leftTargetPitch)
    where 
        stringEqual = guess!!guessIndex == target!!targetIndex
        isMaxTargetIndex = targetIndex == (length target)-1
        
        newLeftGuessPitch = remove guessIndex guess
        newLeftTargetPitch = remove targetIndex target
        
        (correctPitch, leftGuessPitch, leftTargetPitch) = if stringEqual then
            o0CorrectNum newLeftGuessPitch guessIndex newLeftTargetPitch 0
            else o0CorrectNum guess (if isMaxTargetIndex then guessIndex+1 
            else guessIndex) target (if isMaxTargetIndex then 0 
            else targetIndex+1)

{-  
    This function can convert a list of pitches to a list of notes
    How to use:
        o0PitchListToNoteList ["A1","B2","C3"]
            above result: ["A","B","C"]
-}
o0PitchListToNoteList :: [String] -> [String]
o0PitchListToNoteList [] = []
o0PitchListToNoteList pitch
    = (take 1 (head pitch)):(o0PitchListToNoteList (drop 1 pitch))

{-  
    This function can convert a list of pitches to a list of octaves
    How to use:
        o0PitchListToOctaveList ["A1","B2","C3"]
            above result: ["1","2","3"]
-}
o0PitchListToOctaveList :: [String] -> [String]
o0PitchListToOctaveList [] = []
o0PitchListToOctaveList pitch 
    = (drop 1 (head pitch)):(o0PitchListToOctaveList (drop 1 pitch))

{-  
    This function can calculate feedback from two chords
    How to use:
        o0FeedBack ["A1","B2","C3"] ["A1","B3","C4"]
            above result: ["1","2","1"]
                The first 1 is the number of same pitches.
                2 is the number of same notes except same pitches.
                The second 1 is the number of same octave except same pitches.
-}         
o0FeedBack :: [String] -> [String] -> (Int,Int,Int)
o0FeedBack guess target = (correctPitch, correctNote, correctOctave)
    where 
        (correctPitch, leftGuessPitch, leftTargetPitch) 
            = o0CorrectNum guess 0 target 0
        (correctNote, _, _) = o0CorrectNum
            (o0PitchListToNoteList leftGuessPitch) 0 
            (o0PitchListToNoteList leftTargetPitch) 0
        (correctOctave, _, _) = o0CorrectNum
            (o0PitchListToOctaveList leftGuessPitch) 0
            (o0PitchListToOctaveList leftTargetPitch) 0
            
{-  
    This function can convert integer type of pitch to string type of pitch
    How to use:
        o0IndexToPitch 0
            above result: "A1"
        o0IndexToPitch 1
            above result: "A2"
        o0IndexToPitch 6
            above result: "C1"
        o0IndexToPitch 8
            above result: "C3"
-}
o0IndexToPitch :: Int -> String
o0IndexToPitch index = [noteSet!!fst indexes, octaveSet!!snd indexes]
    where
        noteSet = ['A','B'..'G']
        octaveSet = ['1','2','3']
        indexes = divMod index (length octaveSet)
        
{-  
    This function can convert a list of integer type pitch to a list of string
    type pitch
    How to use:
        o0IndexToChord [0,1,2]
            above result: ["A1","A2","A3"]
-}
o0IndexToChord :: [Int] -> [String]
o0IndexToChord [] = []
o0IndexToChord list
    = (o0IndexToPitch (head list)):o0IndexToChord (drop 1 list)

{-  
    This function can convert a list of integer type chord to a list of string
    type chord
    How to use:
        o0IndexListToTargetList [[0,1,2],...,[2,3,4]]
            above result: [["A1","A2","A3"],...,["A2","A3","B1"]]
-}
o0IndexListToTargetList :: [[Int]] -> [[String]]
o0IndexListToTargetList [] = []
o0IndexListToTargetList list 
    = (o0IndexToChord (head list)):o0IndexListToTargetList (drop 1 list)
    
{-  
    This function can convert a string type pitch to a integer type pitch
    How to use:
        o0PitchToIndex "A3"
            above result: 2
-}
o0PitchToIndex :: String -> Int
o0PitchToIndex [note, octave] = ((ord note) - 65) * 3 + (ord octave) - 49
      
{-  
    This defines the maximum number of pitch index. As the pitch is from
    A1 to G3, totally 21, so its index is from 0 to 20. Then the maximum is 20
-}
maxPitchIndex = 20

{-  
    This function can return an all possible targets' index set, totally 1330
    How to use:
        o0AllPossibleTargetIndex []
            Do not change []
-}
o0AllPossibleTargetIndex :: [[Int]] -> [[Int]]
o0AllPossibleTargetIndex [] = o0AllPossibleTargetIndex [[0, 1, 2]]
o0AllPossibleTargetIndex list =
    if lastTarget!!2 == maxPitchIndex
    then (if lastTarget!!1 == maxPitchIndex - 1
    then (if lastTarget!!0 == maxPitchIndex - 2
    then list
    else (o0AllPossibleTargetIndex
        ([lastTarget!!0 +1, lastTarget!!0 +2, lastTarget!!0 +3]:list)))
    else (o0AllPossibleTargetIndex
        ([lastTarget!!0, lastTarget!!1 +1, lastTarget!!1 +2]:list)))
    else (o0AllPossibleTargetIndex 
        ([lastTarget!!0, lastTarget!!1, lastTarget!!2 +1]:list))
    where lastTarget = head list
    
{-  
    This function can return an all possible targets set, totally 1330
    How to use:
        o0AllPossibleTarget
-}
o0AllPossibleTarget :: [[String]]
o0AllPossibleTarget = o0IndexListToTargetList (o0AllPossibleTargetIndex [])

{-  
    This function can insert a candidate to the corresponding feedback
    candidate sets, and return a inserted candidate sets dictionary.
    How to use:
        o0InsertToCandidateSets {feedback} {candidate}
            {candidate sets dictionary}
-}
o0InsertToCandidateSets :: (Int,Int,Int) -> [String] 
    -> [((Int,Int,Int),[[String]])] -> [((Int,Int,Int),[[String]])]
o0InsertToCandidateSets feedback guess [] = [(feedback,[guess])]
o0InsertToCandidateSets feedback guess candidateSets 
    = if feedback == (fst currentSet) 
        then (feedback,guess:(snd currentSet)):(drop 1 candidateSets)
        else currentSet:(o0InsertToCandidateSets feedback guess 
            (drop 1 candidateSets))
    where currentSet = head candidateSets
    
{-  
    This function can return a candidate set which corresponding to the input
    feedback from a candidate sets dictionary.
    How to use:
        o0FindCandidateSet {feedback} {candidate sets dictionary}
-}
o0FindCandidateSet :: (Int,Int,Int) -> [((Int,Int,Int),[[String]])] 
    -> [[String]]
o0FindCandidateSet _ [] = []
o0FindCandidateSet key candidateSets 
    = if key == (fst currentSet) then snd currentSet
        else o0FindCandidateSet key (drop 1 candidateSets)
    where currentSet = head candidateSets
   
{-  
    This function can return a list of all possible feedback and their 
    corresponding candidate sets of one guess.
    How to use:
        o0PossibleCandidateSets {a guess} {a list of candidates}
-}   
o0PossibleCandidateSets :: [String] -> [[String]]
    -> [((Int,Int,Int),[[String]])]
o0PossibleCandidateSets _ [] = []
o0PossibleCandidateSets guess remaining 
    = o0InsertToCandidateSets feedBack (head remaining) 
        (o0PossibleCandidateSets guess (drop 1 remaining))
    where feedBack = o0FeedBack guess (head remaining)
    
{-  
    This function can return the minimum expected number of remaining
    candidates times total number of candidates. Note that without dividing
    total number does not affecting the result of comparison.
    How to use:
        o0ExpectedNumTimesTotalNum {candidate sets dictionary of one guess}
-}   
o0ExpectedNumTimesTotalNum :: [((Int,Int,Int),[[String]])] -> Int
o0ExpectedNumTimesTotalNum [] = 0
o0ExpectedNumTimesTotalNum possibleCandidateSets 
    = targetNumber * targetNumber 
        + (o0ExpectedNumTimesTotalNum (drop 1 possibleCandidateSets))
    where
        (_,targetSet) = head possibleCandidateSets
        targetNumber = length targetSet
    
{-  
    This function can compute candidate sets for all guesses in list, and
    return a list of candidate sets.
    How to use:
        o0AllChoisesResults {a list of guesses}
-}  
o0AllChoisesResults :: [[String]] -> [([String],[((Int,Int,Int),[[String]])])]
o0AllChoisesResults [] = []
o0AllChoisesResults candidateSet 
    = (current,o0PossibleCandidateSets current candidateSet)
        :o0AllChoisesResults (drop 1 candidateSet)
    where current = head candidateSet

{-  
    This function can return the best guess with its corresponding candidate
    sets.
    How to use:
        (_,bestGuess,candidateSets)
            = o0BestChoise {all guesses} {all candidates}
-}    
o0BestChoise :: [[String]] -> [[String]] 
    -> (Int,[String],[((Int,Int,Int),[[String]])])
o0BestChoise [] _ = (999999,[],[])
o0BestChoise candidateSet allCandidates 
    = if currentScore >= nextScore then (nextScore, next, nextCandidateSets)
        else (currentScore, current, currentCandidateSets)
    where 
        current = head candidateSet
        currentCandidateSets = o0PossibleCandidateSets current allCandidates
        currentScore = o0ExpectedNumTimesTotalNum currentCandidateSets
        (nextScore, next, nextCandidateSets)
            = o0BestChoise (drop 1 candidateSet) allCandidates



    
