Update the MCP Protocol version from: 2025-03-26 to 2025-06-18.

**BREAKING CHANGE**: Remove backward compatibility. Server will ONLY support 2025-06-18.

## Required Implementation Changes

### 1. Protocol Version Updates
**Files to modify:**
- `src/MCP/Server/Protocol.hs:46` - Update `protocolVersion = "2025-06-18"`
- `src/MCP/Server/Transport/Http.hs:27` - Update comment reference
- `src/MCP/Server/Transport/Http.hs:80` - Update discovery response version
- `src/MCP/Server/Handlers.hs:64-67` - Remove legacy version support, only accept "2025-06-18"
- `README.md:7` - Update documentation

### 2. Mandatory Protocol Version Header
**New requirement:** All HTTP requests MUST include `MCP-Protocol-Version: 2025-06-18` header
- Add header validation in `src/MCP/Server/Transport/Http.hs:71`
- Reject requests without proper header with 400 Bad Request

### 3. Enhanced Tool Response Structure
**Add structured tool output support:**
- Extend `ToolsCallResponse` in `src/MCP/Server/Protocol.hs:227-236`
- Add optional `_meta` field
- Add support for resource links in responses

### 4. New Fields for Human-Friendly Display
**Add `title` field to:**
- `PromptDefinition` (src/MCP/Server/Types.hs)
- `ResourceDefinition` (src/MCP/Server/Types.hs) 
- `ToolDefinition` (src/MCP/Server/Types.hs)

### 5. Remove Legacy Code
**Clean up backward compatibility:**
- Remove 2024-11-05 and 2025-03-26 version handling
- Simplify protocol validation logic
- Update error messages to reference 2025-06-18 only

## Implementation Priority
1. **Phase 1**: Protocol version updates and header validation (breaking changes)
2. **Phase 2**: Enhanced response structures and new fields
3. **Phase 3**: Legacy code cleanup

## Validation Checklist
- [ ] Server rejects clients with wrong protocol version
- [ ] HTTP requests without version header are rejected
- [ ] New structured responses work correctly
- [ ] All examples and documentation updated

Key changes reference:
https://modelcontextprotocol.io/specification/2025-06-18/changelog

