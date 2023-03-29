module Brainfuck where

import Prelude

import Control.Monad.Free (Free, liftF)
import Control.Monad.Reader (Reader)
import Control.Monad.ST (run)
import Control.Monad.State (State, gets, modify, modify_)
import Data.Array (reverse)
import Data.Array.ST (STArray)
import Data.Char (toCharCode)
import Data.List (List(..))
import Data.List as List
import Data.String.CodeUnits (toCharArray)

data Command f
  = Increment f
  | Decrement f
  | Forward f
  | Backward f
  | Get f
  | Log f
  | LoopStart f
  | LoopEnd f

derive instance Functor Command

type Brainfuck a = Free Command a

inc :: Brainfuck Unit
inc = liftF $ Increment unit

dec :: Brainfuck Unit
dec = liftF $ Decrement unit

fwd :: Brainfuck Unit
fwd = liftF $ Forward unit

bwd :: Brainfuck Unit
bwd = liftF $ Backward unit

log :: Brainfuck Unit
log = liftF $ Log unit

get :: Brainfuck Unit
get = liftF $ Get unit

lps :: Brainfuck Unit
lps = liftF $ LoopStart unit

lpe :: Brainfuck Unit
lpe = liftF $ LoopEnd unit

data Memory = Memory (List Int) Int (List Int)

initMemory :: Memory
initMemory = Memory Nil 0 Nil

moveLeft :: Memory -> Memory
moveLeft = case _ of
  Memory (x List.: xs) y zs -> Memory xs x (y List.: zs)
  Memory Nil y zs -> Memory Nil 0 (y List.: zs)

moveRight :: Memory -> Memory
moveRight = case _ of
  Memory xs y (z List.: zs) -> Memory (y List.: xs) z zs
  Memory xs y Nil -> Memory (y List.: xs) 0 Nil

increment :: Memory -> Memory
increment = case _ of
  Memory xs y zs -> Memory xs ((y + 1) `mod` 256) zs

decrement :: Memory -> Memory
decrement = case _ of
  Memory xs y zs -> Memory xs ((y - 1) `mod` 256) zs

type BrainfuckM a = State { memory :: Memory, input :: List Int, output :: List Int, loops :: List (Brainfuck Unit), program :: Brainfuck Unit, skip :: Boolean } a

popInput :: BrainfuckM Int
popInput = do
  input <- gets _.input
  case input of
    Nil -> pure 0
    x List.: xs -> do
      modify_ _ { input = xs }
      pure x

pushOutput :: Int -> BrainfuckM Unit
pushOutput x = do
  output <- gets _.output
  modify_ _ { output = x List.: output }

pushLoop :: Brainfuck Unit -> BrainfuckM Unit
pushLoop x = do
  loops <- gets _.loops
  modify_ _ { loops = x List.: loops }

lookupLoop :: BrainfuckM (Brainfuck Unit)
lookupLoop = do
  loops <- gets _.loops
  case loops of
    Nil -> do
      program <- gets _.program
      pure program -- 対応する先頭括弧がない場合は最初から実行
    x List.: xs -> do
      pure x

popLoop :: BrainfuckM (Brainfuck Unit)
popLoop = do
  loops <- gets _.loops
  case loops of
    Nil -> do
      program <- gets _.program
      pure program -- 対応する先頭括弧がない場合は最初から実行
    x List.: xs -> do
      modify_ _ { loops = xs }
      pure x

skipLoop :: BrainfuckM Unit
skipLoop = do
  loops <- gets _.loops
  case loops of
    Nil -> do
      program <- gets _.program
      modify_ _ { program = program, skip = true } -- 対応する先頭括弧がない場合は最初から実行
    x List.: xs -> do
      modify_ _ { loops = xs, skip = true }

-- interpreter :: Command (Brainfuck Unit) -> BrainfuckM (Brainfuck Unit)
-- interpreter = case _ of
--   Increment next -> do
--     modify_ _ { memory = increment _.memory }
--     pure next
--   Decrement next -> do
--     modify_ _ { memory = decrement _.memory }
--     pure next
--   Forward next -> do
--     modify_ _ { memory = moveRight _.memory }
--     pure next
--   Backward next -> do
--     modify_ _ { memory = moveLeft _.memory }
--     pure next
--   Get next -> do
--     x <- popInput
--     modify_ _ { memory = _ { _.memory = x } }
--     pure next
--   Log next -> do
--     y <- gets _.memory.y
--     pushOutput y
--     pure next
--   LoopStart next -> do

-- -- runBrainfuck :: Brainfuck Unit -> String -> String
-- runBrainfuck =
--   let
--     inputAsList = map toCharCode $ List.fromFoldable $ toCharArray input
