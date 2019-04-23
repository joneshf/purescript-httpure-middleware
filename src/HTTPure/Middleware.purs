module HTTPure.Middleware
  ( combinedLogFormat
  , commonLogFormat
  ) where

import Prelude

import Data.Array as Data.Array
import Data.DateTime as Data.DateTime
import Data.Formatter.DateTime as Data.Formatter.DateTime
import Data.Maybe as Data.Maybe
import Data.String as Data.String
import Effect.Class as Effect.Class
import Effect.Class.Console as Effect.Class.Console
import Effect.Now as Effect.Now
import HTTPure as HTTPure

type Middleware
  = (HTTPure.Request -> HTTPure.ResponseM) -> HTTPure.Request -> HTTPure.ResponseM

-- | A middleware that logs requests in the
-- | [Combined Log Format](https://httpd.apache.org/docs/trunk/logs.html#combined).
combinedLogFormat :: Middleware
combinedLogFormat router request = do
  now <- Effect.Class.liftEffect Effect.Now.nowDateTime
  response <- router request
  let ipAddress = unknown
      rfc1413 = unknown
      userId = unknown
  Effect.Class.Console.log
    ( Data.String.joinWith
      " "
      [ ipAddress
      , rfc1413
      , userId
      , renderDateTime now
      , renderRequest request
      , show response.status
      , renderSize response.headers
      , renderReferer request
      , renderUserAgent request
      ]
    )
  pure response

-- | A middleware that logs requests in the
-- | [Common Log Format](https://httpd.apache.org/docs/trunk/logs.html#common).
commonLogFormat :: Middleware
commonLogFormat router request = do
  now <- Effect.Class.liftEffect Effect.Now.nowDateTime
  response <- router request
  let ipAddress = unknown
      rfc1413 = unknown
      userId = unknown
  Effect.Class.Console.log
    ( Data.String.joinWith
      " "
      [ ipAddress
      , rfc1413
      , userId
      , renderDateTime now
      , renderRequest request
      , show response.status
      , renderSize response.headers
      ]
    )
  pure response

renderDateTime :: Data.DateTime.DateTime -> String
renderDateTime =
  Data.Formatter.DateTime.format
    ( Data.Array.toUnfoldable
      [ Data.Formatter.DateTime.Placeholder "["
      , Data.Formatter.DateTime.DayOfMonthTwoDigits
      , Data.Formatter.DateTime.Placeholder "/"
      , Data.Formatter.DateTime.MonthShort
      , Data.Formatter.DateTime.Placeholder "/"
      , Data.Formatter.DateTime.YearFull
      , Data.Formatter.DateTime.Placeholder ":"
      , Data.Formatter.DateTime.Hours24
      , Data.Formatter.DateTime.Placeholder ":"
      , Data.Formatter.DateTime.MinutesTwoDigits
      , Data.Formatter.DateTime.Placeholder ":"
      , Data.Formatter.DateTime.SecondsTwoDigits
      , Data.Formatter.DateTime.Placeholder " -0000"
      , Data.Formatter.DateTime.Placeholder "]"
      ]
    )

renderMethod :: HTTPure.Method -> String
renderMethod = case _ of
  HTTPure.Connect -> "CONNECT"
  HTTPure.Delete -> "DELETE"
  HTTPure.Get -> "GET"
  HTTPure.Head -> "HEAD"
  HTTPure.Options -> "OPTIONS"
  HTTPure.Patch -> "PATCH"
  HTTPure.Post -> "POST"
  HTTPure.Put -> "PUT"
  HTTPure.Trace -> "TRACE"

renderReferer :: HTTPure.Request -> String
renderReferer { headers } = show (HTTPure.at headers "Referer")

renderRequest :: HTTPure.Request -> String
renderRequest { method, path } =
  show
    ( Data.String.joinWith
      " "
      [ renderMethod method
      , "/" <> Data.String.joinWith "/" path
      , httpVersion
      ]
    )
  where
  httpVersion :: String
  httpVersion = unknown

renderSize :: HTTPure.Headers -> String
renderSize headers =
  Data.Maybe.fromMaybe unknown (HTTPure.lookup headers "Content-Length")

renderUserAgent :: HTTPure.Request -> String
renderUserAgent { headers } = show (HTTPure.at headers "User-Agent")

unknown :: String
unknown = "-"
