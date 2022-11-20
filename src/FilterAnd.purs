module FilterAnd where

import Prelude

import Control.Apply (lift2)
import Data.Array (filter, (..))
import Data.Maybe (Maybe(..))

f1 :: Int -> Boolean
f1 x = x > 30

f2 :: Int -> Boolean
f2 x = x < 60

f3 :: Int -> Boolean
f3 x = x `mod` 3 == 0

filtered :: Array Int
filtered = filter (f1 && f2 && f3) $ 1 .. 90

data SectionInt = Year | Month | Day | Hour | Minute | Second

data SectionString = MonthName | FromString String

type SectionIntSettings =
  { adjust :: Maybe Int
  }

defaultSectionIntSettings :: SectionIntSettings
defaultSectionIntSettings =
  { adjust: Nothing
  }

data Format = FormatInt SectionInt SectionIntSettings | FormatString SectionString

defaultInt :: SectionInt -> Format
defaultInt section = FormatInt section defaultSectionIntSettings

int :: SectionInt -> (SectionIntSettings -> SectionIntSettings) -> Format
int section f = FormatInt section (f defaultSectionIntSettings)

defaultYear :: Format
defaultYear = defaultInt Year

year :: (SectionIntSettings -> SectionIntSettings) -> Format
year = int Year

defaultMonth :: Format
defaultMonth = defaultInt Month

month :: (SectionIntSettings -> SectionIntSettings) -> Format
month = int Month

defaultDay :: Format
defaultDay = defaultInt Day

day :: (SectionIntSettings -> SectionIntSettings) -> Format
day = int Day

defaultHour :: Format
defaultHour = defaultInt Hour

hour :: (SectionIntSettings -> SectionIntSettings) -> Format
hour = int Hour

defaultMinute :: Format
defaultMinute = defaultInt Minute

minute :: (SectionIntSettings -> SectionIntSettings) -> Format
minute = int Minute

defaultSecond :: Format
defaultSecond = defaultInt Second

second :: (SectionIntSettings -> SectionIntSettings) -> Format
second = int Second

monthName :: Format
monthName = FormatString MonthName

string :: String -> Format
string = FormatString <<< FromString

slush :: Format
slush = string "/"

colon :: Format
colon = string ":"

space :: Format
space = string " "

-- -- 使う側
format :: Array Format
format =
  [ year _ { adjust = Just 4 }
  , slush
  , month _ { adjust = Just 2 }
  , slush
  , day _ { adjust = Just 2 }
  , space
  , hour _ { adjust = Just 2 }
  , colon
  , minute _ { adjust = Just 2 }
  , colon
  , second _ { adjust = Just 2 }
  ]
