> **ADR_0002 — Historical.** This document predates the ADR convention: it is preserved as-written (a working note from the time), renumbered for the index. New ADRs follow the format described in [README.md](README.md).

# Support for better parameter types

Currently, the mcp-server library supports defining Tools, Prompts and Resources like the following:

```haskell
data SimpleTool
    = GetValue { key :: Text }
    | SetValue { key :: Text, value :: Text }
    deriving (Show, Eq)
```

And then deriving the tool handlers like so:

```haskell
main :: IO ()
main = do
    let handleTool :: SimpleTool -> IO Content
        handleTool (GetValue k) = pure $ ContentText "..."
        handleTool (SetValue k v) = pure $ ContentText "..."

    -- Derive the tool handlers using Template Haskell with descriptions
    let tools = $(deriveToolHandler ''SimpleTool 'handleTool)
     in runMcpServerStdio
        McpServerInfo
            { serverName = "Simple Key-Value MCP Server"
            , serverVersion = "1.0.0"
            , serverInstructions = "A simple key-value store with GetValue and SetValue tools"
            }
        McpServerHandlers
            { prompts = Nothing     -- No prompts in this example
            , resources = Nothing   -- No resources in this example
            , tools = Just tools    -- Only tools in this example
            }
```

The downside of this approach is the reliance on field accessors in the constructor records, e.g. "key" and "value" are technically partial functions and prone to issues.

Although these are OK for small examples (and are nice and easy to understand), the better and more acceptable approach in haskell is to rather introduce additional data types for each Constructor's parameters (which must always be named fields), e.g.:

```haskell
-- Using fields, but prefixing with the Type name, because maybe the user wants to use Lenses
data GetValueParams = GetValueParams { _gvpKey :: Text }
data SetValueParams = SetValueParams { _svpKey :: Text, _svpValue :: Text }

data SimpleTool
    = GetValue GetValueParams
    | SetValue SetValueParams
    deriving (Show, Eq)

makeLenses ''GetValueParams
makeLenses ''SetValueParams
```

This would then be reflected in the API like:

```haskell
main :: IO ()
main = do
    let handleTool :: SimpleTool -> IO Content
        handleTool (GetValue (GetValueParams key)) = pure $ ContentText "..."
        handleTool (SetValue (SetValueParams key value)) = pure $ ContentText "..."

    -- Derive the tool handlers using Template Haskell with descriptions
    let tools = $(deriveToolHandler ''SimpleTool 'handleTool)
     in runMcpServerStdio
        McpServerInfo
            { serverName = "Simple Key-Value MCP Server"
            , serverVersion = "1.0.0"
            , serverInstructions = "A simple key-value store with GetValue and SetValue tools"
            }
        McpServerHandlers
            { prompts = Nothing     -- No prompts in this example
            , resources = Nothing   -- No resources in this example
            , tools = Just tools    -- Only tools in this example
            }
```

e.g. this is NOT supported:

```haskell
-- Positional arguments
data SimpleTool
    = GetValue Int
    | SetValue Int Text
    deriving (Show, Eq)
```

and nor is this supported:

```haskell
-- Positional arguments
data GetValueParams = GetValueParams Text
data SetValueParams = SetValueParams Text Text

data SimpleTool
    = GetValue GetValueParams
    | SetValue SetValueParams
    deriving (Show, Eq)
```

Why? The challenge here is that although we can support the positional parameters, the Template Haskell will not be able to figure out their names and generates names such as "param0", "param1", etc. Whereas with record fields we can extract proper names such as "key", "value" etc (although they remain partial). Because we don't know what to name the parameters in the MCP definition and we want to avoid naming schemes like "param0", "param1", etc. This must be explicitly not supported.

# Suggested approach to support parameter types

update the `derivePromptHandler`, `deriveResourceHandler` and `deriveToolHandler` functions to allow for support for multiple constructor parameters:
- record fields (current approach)
- positional parameters, which must always be well-defined data types, which eventually end in record types (so we can generate a name).



