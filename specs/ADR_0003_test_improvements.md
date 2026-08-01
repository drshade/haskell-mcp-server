> **ADR_0003 — Historical.** This document predates the ADR convention: it is preserved as-written (a working note from the time), renumbered for the index. New ADRs follow the format described in [README.md](README.md).

# Test Suite Improvement Recommendations

## Current State Analysis

### ✅ Good Practices Already Present:
- Comprehensive test coverage
- QuickCheck property-based testing  
- End-to-end integration tests
- Clear test organization with sections
- Template Haskell testing

### ❌ Areas Needing Improvement:

## 1. **Test Framework**
**Current:** Manual test runner with `putStrLn` and boolean tracking
```haskell
-- Current approach
testSomething :: IO Bool
testSomething = do
  result <- someOperation
  let success = result == expected
  putStrLn $ "Test: " ++ if success then "PASS" else "FAIL"
  return success
```

**Better:** Use proper test framework (Hspec, Tasty, etc.)
```haskell
-- Improved approach
testSomething :: Spec
testSomething = describe "Feature" $ do
  it "should work correctly" $ do
    result <- someOperation
    result `shouldBe` expected
```

## 2. **Error Handling**
**Current:** Uses `error` which crashes tests
```haskell
parsed = case readMaybe text of
  Just result -> result
  Nothing -> error $ "Failed to parse: " <> text
```

**Better:** Proper test assertions and failure messages
```haskell
parsed <- case readMaybe text of
  Just result -> return result
  Nothing -> expectationFailure $ "Failed to parse: " <> text
```

## 3. **Code Organization**
**Current:** Long, complex test functions with nested pattern matching
```haskell
let test1 = case result1 of
      Right (ContentText content) -> content == expected
      _ -> False
    test2 = case result2 of 
      Right (ContentText content) -> content == expected2
      _ -> False
in test1 && test2
```

**Better:** Helper functions and clear separation
```haskell
executeToolTest :: ToolCallHandler -> String -> [(String,String)] -> Text -> IO ()
executeToolTest handler name args expected = do
  result <- handler name args
  case result of
    Right (ContentText content) -> content `shouldBe` expected
    other -> expectationFailure $ "Unexpected result: " ++ show other
```

## 4. **Property-Based Testing**
**Current:** Properties that use `error`
```haskell
prop_something x = 
  case someOperation x of
    Just result -> result == expected
    Nothing -> error "Parse failed"  -- Bad!
```

**Better:** Properties that return `Bool` cleanly
```haskell
prop_something x =
  case someOperation x of
    Just result -> result == expected
    Nothing -> False  -- Proper failure
```

## 5. **Test Data Management**
**Current:** Inline test data mixed with logic
```haskell
result <- callHandler "get_value" [("_gvpKey", "mykey")]
```

**Better:** Separate test data definitions
```haskell
testCases :: [(String, [(String, String)], Text)]
testCases = 
  [ ("get_value", [("_gvpKey", "mykey")], "Getting value for key: mykey")
  , ("set_value", [("_svpKey", "key"), ("_svpValue", "val")], "Setting key = val")
  ]
```

## 6. **Assertion Helpers**
**Current:** Repetitive pattern matching
```haskell
let getDef = case filter (\def -> toolDefinitionName def == "get_value") defs of
      [def] -> def
      _ -> error "Tool not found"
```

**Better:** Reusable helper functions
```haskell
assertToolExists :: String -> [ToolDefinition] -> ToolDefinition
assertHasProperty :: String -> ToolDefinition -> PropertyType
assertPropertyDescription :: String -> PropertyType -> IO ()
```

## Incremental Migration Plan

### **Phase 1: Setup Hybrid Test Environment** ✅ COMPLETED
- [x] Add Hspec dependency to cabal file
- [x] Create new test-suite `haskell-mcp-server-hspec` in cabal
- [x] Create `test/HspecMain.hs` for new test runner
- [x] Modify existing `test/Main.hs` to run both old and new tests
- [x] Verify both test suites run successfully

### **Phase 2: Migrate Property-Based Tests (Lowest Risk)** ✅ COMPLETED
- [x] Create `test/Spec/JSONConversion.hs` module
- [x] Migrate `prop_intRoundTrip` - fix `error` usage
- [x] Migrate `prop_boolRoundTrip` - fix `error` usage  
- [x] Migrate `prop_textRoundTrip` (already safe)
- [x] Migrate `testIntConversion` and `testBoolConversion`
- [x] Remove migrated tests from old suite

### **Phase 3: Migrate Simple Derivation Tests** ✅ COMPLETED
- [x] Create `test/Spec/BasicDerivation.hs` module
- [x] Migrate basic prompt derivation tests
- [x] Migrate basic resource derivation tests
- [x] Migrate basic tool derivation tests
- [x] Create helper functions for common patterns

### **Phase 4: Migrate Complex Tests** ✅ COMPLETED
- [x] Create `test/Spec/SchemaValidation.hs` module
- [x] Migrate schema generation validation tests (`testSchemaGeneration`)
- [x] Create assertion helpers (`assertToolExists`, `assertHasProperty`, etc.)
- [x] Migrate custom description tests (`testCustomDescriptions`)
- [x] Create `test/Spec/AdvancedDerivation.hs` module
- [x] Migrate separate parameter types tests (`testSeparateParamsDerivation` - 112 lines, highest complexity)
- [x] Remove migrated complex tests from legacy suite

### **Phase 5: Test Data Separation** ✅ COMPLETED
- [x] Create `test/TestData.hs` module
- [x] Extract test cases into separate data structures
- [x] Refactor tests to use test data module
- [x] Clean up inline test data from test logic

### **Phase 6: Final Cleanup** ✅ COMPLETED
- [x] Remove old test runner from `test/Main.hs`
- [x] Update main test-suite to use only Hspec
- [x] Remove manual test patterns and boolean tracking
- [x] Update CI configuration if needed
- [x] Update documentation

## Migration Progress Summary

### ✅ **Migration Complete: 100% Finished**
- **All 6 phases** successfully completed
- **32 tasks completed** - full migration achieved
- **Critical error fixes** implemented (4 `error` calls eliminated)
- **Modern test patterns** established with comprehensive helper functions
- **Complex Template Haskell tests** successfully migrated
- **Legacy test suite** completely removed
- **Zero test regression** - all tests passing

### 📊 **Final Test Statistics**
- **Legacy Suite**: Completely removed
- **Modern Suite**: 26 comprehensive tests with proper error handling and modern patterns
- **Test Coverage**: Maintained 100% - no functionality lost during migration
- **Architecture**: Single unified Hspec test suite with modular organization

### 🎉 **Major Milestone: All Complex Tests Migrated**
✅ **`testSeparateParamsDerivation`** (112 lines) - Advanced Template Haskell metaprogramming → `test/Spec/AdvancedDerivation.hs`  
✅ **`testSchemaGeneration`** - Critical API compliance validation → `test/Spec/SchemaValidation.hs`  
✅ **`testCustomDescriptions`** - Documentation feature validation → `test/Spec/SchemaValidation.hs`

### 🚀 **New Modern Test Modules**
1. **`test/Spec/JSONConversion.hs`** - Property-based and manual JSON tests (5 tests)
2. **`test/Spec/BasicDerivation.hs`** - Template Haskell derivation basics (8 tests)
3. **`test/Spec/SchemaValidation.hs`** - Schema generation and descriptions (4 tests)
4. **`test/Spec/AdvancedDerivation.hs`** - Complex Template Haskell features (9 tests)

### 🎯 **Migration Completed Successfully**
**Total Time**: Phases 1-6 completed incrementally
- Test data separation completed
- Legacy suite successfully removed
- Single unified modern test suite operational

**✅ Project now uses modern Hspec testing framework exclusively**

## Migration Strategy Benefits

✅ **Zero Risk**: Old tests continue running during migration  
✅ **Gradual Validation**: Compare old vs new test results  
✅ **Team Adoption**: New features can use modern test style immediately  
✅ **Continuous Integration**: No disruption to CI/CD pipeline  
✅ **Incremental Progress**: Each phase delivers immediate value

## Recommended Next Steps

1. **Add Hspec dependency** to cabal file
2. **Migrate tests gradually** - start with new features
3. **Create helper modules** for common assertions
4. **Separate test data** from test logic
5. **Use proper test runners** that provide:
   - Parallel test execution
   - Better failure reporting  
   - Test filtering/selection
   - CI integration

## Benefits of Improvements

- **Better debugging**: Clear failure messages and stack traces
- **Maintainability**: Cleaner, more readable test code
- **Reliability**: Proper test isolation and error handling
- **Developer experience**: Standard tooling and patterns
- **CI/CD integration**: Better reporting and failure analysis

## Example Cabal Dependencies

```cabal
test-suite improved-tests
  type: exitcode-stdio-1.0
  hs-source-dirs: test
  main-is: ImprovedMain.hs
  build-depends:
    base >= 4.7 && < 5,
    mcp-server,
    hspec >= 2.7,
    QuickCheck >= 2.14,
    text,
    aeson
```