# Changelog

# Unreleased

# 4.0.0 - 2021-03-20
## Changed

* Bump all dependencies for `purescript` version `0.14.0` - this change is non-breaking, but all the dependcies caused a major bump

# 3.0.2 - 2019-12-08

## Changed

* Bump `purescript-httpure` to `0.10.0` - non-breaking changes should allow this to be a patch version bump

# 3.0.1 - 2019-12-08

## Changed

* Bump `purescript-httpure` to `0.9.0` - non-breaking changes should allow this to be a patch version bump

# 3.0.0 - 2019-12-08

## Changed

* Bump `purescript-httpure` to `0.8.3` - transitive dependencies require a major version bump

# 2.1.0 - 2019-12-08

## Changed

* Render HTTP Version in logs - the version is now available in the request

# 2.0.0 - 2019-12-08

## Changed

* Update `purescript-httpure` to `0.8.2` - breaking changes require a major version bump

# 1.2.1 - 2019-12-08

## Fixed

* Fixed bower dependencies - transitive dependencies were causing install failures

# 1.2.0 - 2019-12-08

## Added

* `HTTPure.Middleware.LogLifecycle` - codifies lifecycle functions for logging
* `HTTPure.Middleware.LogTime` - codifies time data around a request
* `HTTPure.Middleware.Middleware` - a type synonym for middlewares
* `HTTPure.Middleware.log` - generalizes the previous log middlewares to allow any output
* `HTTPure.Middleware.logWithTime` - provides a simplification for logging with time
* `HTTPure.Middleware.timeout` - aborts requests if they take too long

## Fixed

* Lock down `purescript-httpure` version - `purescript-httpure` allows breaking changes in patch versions which conflicts with SemVer caret

# 1.1.0 - 2019-04-25

## Added

* `HTTPure.Middleware.developmentLogFormat` - uses a non-standard log format that is more verbose and easier to read

# 1.0.0 - 2019-04-23

Initial release.

## Added

* `HTTPure.Middleware.commonLogFormat` - following the Apache common log format
* `HTTPure.Middleware.combinedLogFormat` - Following the Apache combined log format
