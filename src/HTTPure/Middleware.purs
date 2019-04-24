module HTTPure.Middleware
  ( combinedLogFormat
  , commonLogFormat
  , developmentLogFormat
  ) where

import Prelude

import Ansi.Codes as Ansi.Codes
import Ansi.Output as Ansi.Output
import Data.Array as Data.Array
import Data.DateTime as Data.DateTime
import Data.FoldableWithIndex as Data.FoldableWithIndex
import Data.Formatter.DateTime as Data.Formatter.DateTime
import Data.Int as Data.Int
import Data.Maybe as Data.Maybe
import Data.String as Data.String
import Data.Time.Duration as Data.Time.Duration
import Effect.Class as Effect.Class
import Effect.Class.Console as Effect.Class.Console
import Effect.Now as Effect.Now
import Foreign.Object as Foreign.Object
import HTTPure as HTTPure
import HTTPure.Headers as HTTPure.Headers

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

colorMethod :: HTTPure.Method -> String
colorMethod method = Ansi.Output.withGraphics graphics (renderMethod method)
  where
  graphics = case method of
    HTTPure.Connect -> Ansi.Output.foreground Ansi.Codes.Blue
    HTTPure.Delete -> Ansi.Output.foreground Ansi.Codes.Red
    HTTPure.Get -> Ansi.Output.foreground Ansi.Codes.Cyan
    HTTPure.Head -> Ansi.Output.foreground Ansi.Codes.Cyan
    HTTPure.Options -> Ansi.Output.foreground Ansi.Codes.Blue
    HTTPure.Patch -> Ansi.Output.foreground Ansi.Codes.Magenta
    HTTPure.Post -> Ansi.Output.foreground Ansi.Codes.Yellow
    HTTPure.Put -> Ansi.Output.foreground Ansi.Codes.Green
    HTTPure.Trace -> Ansi.Output.foreground Ansi.Codes.Blue

colorStatus :: HTTPure.Status -> String
colorStatus status = Ansi.Output.withGraphics graphics (renderStatus status)
  where
  graphics
    | status < 200 = Ansi.Output.foreground Ansi.Codes.Blue
    | 200 <= status && status < 300 = Ansi.Output.foreground Ansi.Codes.Green
    | 300 <= status && status < 400 = Ansi.Output.foreground Ansi.Codes.Cyan
    | 400 <= status && status < 500 = Ansi.Output.foreground Ansi.Codes.BrightYellow
    | otherwise = Ansi.Output.foreground Ansi.Codes.Red

-- | A middleware that logs request in an unstandardized development format.
-- | The logs are more verbose, colorful, and a bit easier to read.
developmentLogFormat :: Middleware
developmentLogFormat router request = do
  start <- Effect.Class.liftEffect Effect.Now.nowDateTime
  response <- router request
  stop <- Effect.Class.liftEffect Effect.Now.nowDateTime
  Effect.Class.Console.log
    ( Data.String.joinWith "\n"
      ( method
        <> object "Query" request.query
        <> body
        <> object "Headers" headers
        <> duration stop start
        <> status response.status
      )
    )
  pure response
  where
  body
    | hasBody request.method = ["  " <> white "Body" <> ": " <> request.body]
    | otherwise = []
  duration stop start =
    [ "  "
      <> white "Duration"
      <> ": "
      <> renderDuration (Data.DateTime.diff stop start)
    ]
  method =
    [colorMethod request.method <> " /" <> Data.String.joinWith "/" request.path]
  object name obj
    | not Foreign.Object.isEmpty obj =
      [ "  " <> white name <> ":"
      ] <> flip Data.FoldableWithIndex.foldMapWithIndex obj \key val ->
        ["    " <> white key <> ": " <> val]
    | otherwise = []
  status x = ["  " <> white "Status" <> ": " <> colorStatus x]
  white = Ansi.Output.withGraphics (Ansi.Output.foreground Ansi.Codes.White)
  (HTTPure.Headers.Headers headers) = request.headers

hasBody :: HTTPure.Method -> Boolean
hasBody = case _ of
  HTTPure.Patch -> true
  HTTPure.Post -> true
  HTTPure.Put -> true
  _ -> false

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

renderDuration :: Data.Time.Duration.Milliseconds -> String
renderDuration = case _ of
  Data.Time.Duration.Milliseconds x -> show (Data.Int.round x) <> "ms"

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

renderStatus :: HTTPure.Status -> String
renderStatus status = show status <> " " <> human
  where
  human = case status of
    100 -> "Continue"
    101 -> "Switching Protocols"
    102 -> "Processing"
    200 -> "Ok"
    201 -> "Created"
    202 -> "Accepted"
    203 -> "Non Authoritative Information"
    204 -> "No Content"
    205 -> "Reset Content"
    206 -> "Partial Content"
    207 -> "Multi Status"
    208 -> "Already Reported"
    226 -> "IM Used"
    300 -> "Multiple Choices"
    301 -> "Moved Permanently"
    302 -> "Found"
    303 -> "See Other"
    304 -> "Not Modified"
    305 -> "Use Proxy"
    307 -> "Temporary Redirect"
    308 -> "Permanent Redirect"
    400 -> "Bad Request"
    401 -> "Unauthorized"
    402 -> "Payment Required"
    403 -> "Forbidden"
    404 -> "Not Found"
    405 -> "Method Not Allowed"
    406 -> "Not Acceptable"
    407 -> "Proxy Authentication Required"
    408 -> "Request Timeout"
    409 -> "Conflict"
    410 -> "Gone"
    411 -> "Length Required"
    412 -> "Precondition Failed"
    413 -> "Payload Too Large"
    414 -> "URI Too Long"
    415 -> "Unsupported Media Type"
    416 -> "Range Not Satisfiable"
    417 -> "Expectation Failed"
    418 -> "Im A Teapot"
    421 -> "Misdirected Request"
    422 -> "Unprocessable Entity"
    423 -> "Locked"
    424 -> "Failed Dependency"
    426 -> "Upgrade Required"
    428 -> "Precondition Required"
    429 -> "Too Many Requests"
    431 -> "Request Header Fields Too Large"
    451 -> "Unavailable For Legal Reasons"
    500 -> "Internal Server Error"
    501 -> "Not Implemented"
    502 -> "Bad Gateway"
    503 -> "Service Unavailable"
    504 -> "Gateway Timeout"
    505 -> "HTTP Version Not Supported"
    506 -> "Variant Also Negotiates"
    507 -> "Insufficient Storage"
    508 -> "Loop Detected"
    510 -> "Not Extended"
    511 -> "Network Authentication Required"
    _ -> ""


renderUserAgent :: HTTPure.Request -> String
renderUserAgent { headers } = show (HTTPure.at headers "User-Agent")

unknown :: String
unknown = "-"
