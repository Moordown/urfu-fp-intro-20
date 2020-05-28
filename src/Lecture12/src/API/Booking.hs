module API.Booking where

import Servant.API
import DB.Booking

type BookingSessionAPI
  = "api" :> "checkout" :> Capture "id" BookingId :> Get '[JSON] String
  :<|>
    ("api" :> "refund" :> Capture "id" BookingId :> Get '[JSON] String)
  :<|>
    ("api" :> "bookings" :> Get '[JSON] [Booking])