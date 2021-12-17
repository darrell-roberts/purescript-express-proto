module Framework.Formatter (formatDate) where

import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.DateTime (DateTime)
import Data.List (fromFoldable)

formatDate :: DateTime -> String
formatDate d = format f d
  where
  f =
    fromFoldable
      [ YearFull
      , Placeholder "-"
      , MonthTwoDigits
      , Placeholder "-"
      , DayOfMonthTwoDigits
      , Placeholder "T"
      , Hours24
      , Placeholder ":"
      , MinutesTwoDigits
      , Placeholder ":"
      , SecondsTwoDigits
      , Placeholder "Z"
      ]
