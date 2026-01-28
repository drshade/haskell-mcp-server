# Revision history for mcp-server

## Unreleased

* Switch default-language to GHC2021 to support broader range of GHC versions (9.6 - 9.12)

## 0.1.0.17 -- 2026-01-28

* Implement protocol version negotiation according to spec
* Remove unused dependencies, fix GHC warnings
* Add tested-with and haskell-ci generated GitHub Actions config

## 0.1.0.16 -- 2026-01-19

* Bump template-haskell dependency upper bound

## 0.1.0.15 -- 2025-08-13

* Update to MCP spec 2025-06-18

## 0.1.0.14 -- 2025-06-26

* Bump version bounds before adding to Stackage
* Remove support for JSON-RPC batching

## 0.1.0.13 -- 2025-06-17

* Better handling of UTF-8 in logs

## 0.1.0.12 -- 2025-06-17

* Fix unicode handling
* Refactor transports to remove unneeded functions
* Add unicode handling tests

## 0.1.0.11 -- 2025-06-17

* Refactor transports and add HTTP streaming support
* Add `MCP.Server.Handlers` module
* Add `MCP.Server.Transport.Http` and `MCP.Server.Transport.Stdio` modules

## 0.1.0.10 -- 2025-06-13

* Fix resources handling

## 0.1.0.9 -- 2025-06-13

* Bump versions of dependencies
* Port tests to hspec

## 0.1.0.8 -- 2025-06-12

* Support for nestable data types

## 0.1.0.7 -- 2025-06-09

* Documentation updates

## 0.1.0.6 -- 2025-06-09

* Remove pagination support

## 0.1.0.5 -- 2025-06-09

* Add descriptions to constructors and fields

## 0.1.0.4 -- 2025-06-09

* Clean up build configuration

## 0.1.0.3 -- 2025-06-09

* Refactor example modules
* Fix JSON to Haskell type conversion

## 0.1.0.0 -- 2025-06-05

* First version. Released on an unsuspecting world.
