{-# LANGUAGE DeriveAnyClass #-}

module DB.Booking where

import Data.Aeson
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Servant.API
import GHC.Generics
-- import Control.Monad

import DB.MovieSession (MovieSessionId)
import DB.Seat (SeatId)
import DB.Internal

{-
  Тип для идентификатора бронирования
-}
newtype BookingId = BookingId
  { unBookingId :: Integer }
  deriving (Eq, Show)
  deriving ToRow via (Only Integer)
  -- ^ этот инстанс позволяет использовать `BookingId` с функциями для базы данных
  -- `via` говорит о том, что `BookingId` нужно использовать как `Integer`.
  -- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html
  deriving (FromHttpApiData, FromField, ToField, FromJSON, ToJSON)
    via Integer
  -- ^ тоже самое для других классов

{-
  Record, описывающий таблицу `bookings`
-}
data Booking = Booking
  { bookingId :: BookingId
  , seatId :: SeatId
  , movieSessionId :: MovieSessionId
  , isPreliminary :: Bool
  , createdAt :: UTCTime
  } deriving (Eq, Show, Generic)
-- Класс Generic отвечает за универсальное кодирование типа, т.е. за  такое представление,
-- в котором используются конструкторы типов из ограниченного набора
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Generics.html
-- Это представление используется при выводе instance'ов других классов

deriving instance FromRow Booking
deriving instance ToRow Booking
-- ^ получаем возможность записывать и читать данные из базы данных с помощью `Booking`

instance ToJSON Booking
instance FromJSON Booking
-- ^ возможность для работы с JSON

{-
  Booking запрос должен проверить наличие предварительного бронирования.
  Если оно существует и прошло меньше 10 минут от создания, то бронирование
  проходит успешно, иначе необходимо вернуть сообщение об ошибке в JSON формате.
-}

tenMinutes :: NominalDiffTime
tenMinutes = fromInteger 600 :: NominalDiffTime

getBookingCheckout
  :: DBMonad m
  => BookingId
  -> m String
getBookingCheckout bId = runSQL $ \conn -> do
  bookings <- query conn ("SELECT id, seat_id, movie_session_id, is_preliminary, created_at from bookings where id = ?") (bId) :: IO [Booking]
  if (length bookings == 0)
    then return "Invalid booking index"
    else do
      let booking = bookings !! 0
      curTime <- getCurrentTime
      let rotten = (diffUTCTime curTime (createdAt booking)) >= tenMinutes
      if not (isPreliminary booking)
        then return "Booking already checkout"
        else do
          if rotten 
            then do
              execute conn "DELETE FROM bookings WHERE id = ?" (bId)
              return "Booking was delete because it's rotten"
            else do
              execute conn "DELETE FROM bookings WHERE id = ?" (bId)
              execute conn "UPDATE seats SET available = false WHERE id = ?" (seatId booking)
              return "Success booking checkout"

getBookingRefund
  :: DBMonad m
  => BookingId
  -> m String
getBookingRefund bId = runSQL $ \conn -> do
  bookings <- query conn ("SELECT id, seat_id, movie_session_id, is_preliminary, created_at from bookings where id = ?") (bId) :: IO [Booking]
  if (length bookings == 0)
    then return "Invalid booking index"
    else do
      execute conn "DELETE FROM bookings WHERE id = ?" (bId)
      return "Booking was successfully delete"

getBookingInfo
  :: DBMonad m
  => m [Booking]
getBookingInfo = runSQL $ \conn -> do
  query_ conn "SELECT * from bookings"
