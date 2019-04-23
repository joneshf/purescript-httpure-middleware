module Test.Main where

import Prelude

import Effect as Effect
import HTTPure as HTTPure
import HTTPure.Middleware as HTTPure.Middleware
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert
import Test.Unit.Main as Test.Unit.Main

main :: Effect.Effect Unit
main = Test.Unit.Main.runTest do
  Test.Unit.suite "HTTPure.Middleware" do
    Test.Unit.suite "combinedLogFormat" do
      Test.Unit.test "Doesn't alter the request" do
        originalResponse <- router request
        newResponse <- HTTPure.Middleware.combinedLogFormat router request
        Test.Unit.Assert.equal originalResponse.headers newResponse.headers
        Test.Unit.Assert.equal originalResponse.status newResponse.status

    Test.Unit.suite "commonLogFormat" do
      Test.Unit.test "Doesn't alter the request" do
        originalResponse <- router request
        newResponse <- HTTPure.Middleware.commonLogFormat router request
        Test.Unit.Assert.equal originalResponse.headers newResponse.headers
        Test.Unit.Assert.equal originalResponse.status newResponse.status

request :: HTTPure.Request
request =
  { body: "Testing"
  , headers: HTTPure.empty
  , method: HTTPure.Get
  , path: ["foo", "bar", "baz.html"]
  , query: mempty
  }

router :: HTTPure.Request -> HTTPure.ResponseM
router _ = HTTPure.noContent' (HTTPure.header "content-length" "0")
