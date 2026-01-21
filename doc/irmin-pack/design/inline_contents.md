# Inline Small Contents

Date: 2025-01

## Introduction

This document describes the inline contents feature for `irmin-pack`, which allows small content values to be stored directly within node (inode) entries rather than as separate pack file entries.

**TL;DR**

When storing small values (e.g., short strings or small binary blobs), the overhead of creating a separate pack file entry can be significant relative to the data size. The inline contents feature embeds small content values directly within the parent node's entry, eliminating the need for a separate contents entry and the associated offset/hash reference. This reduces storage overhead and improves read performance for small values.

## Background

In `irmin-pack`, a tree structure is stored as:
- **Contents entries**: Leaf values (user data)
- **Node/Inode entries**: Directory-like structures containing references to children (other nodes or contents)

Traditionally, every content value, regardless of size, is stored as a separate entry in the pack file. The parent node stores a reference to this entry (either as an offset or a hash). This approach has overhead:

1. **Storage overhead**: Each contents entry requires a magic byte, length prefix, and the serialized value
2. **Reference overhead**: The parent node must store an offset (8 bytes) or hash (20+ bytes) to reference the contents
3. **Read overhead**: Reading a small value requires two pack file reads (node + contents)

For very small values (e.g., a single byte), this overhead can be larger than the value itself.

## Design

### Inlining Threshold

Content values are inlined when their **serialized size** is less than 16 bytes. The serialized size includes:
- 1-byte variant tag (for the internal `Contents.t` encoding)
- 1-byte varint length prefix (for strings up to 127 bytes)
- The raw content bytes

Therefore, raw content values of **13 bytes or less** are eligible for inlining (13 + 2 = 15 < 16).

### Node Entry Format

The Inode_v3 format introduces new variant cases for node entries to support inlined contents:

```
Traditional node entry reference:
| Contents (name, offset/hash, metadata) |

Inlined contents entry:
| Contents_inlined_value (name, bytes, metadata) |
```

The serialized format uses additional variant tags:
- `inlined-i`: Inlined contents with indirect (dictionary) name, default metadata
- `inlined-x-i`: Inlined contents with indirect name and explicit metadata
- `inlined-d`: Inlined contents with direct name, default metadata
- `inlined-x-d`: Inlined contents with direct name and explicit metadata

### Pack File Entry Kinds

New magic bytes distinguish Inode_v3 entries from earlier versions:

| Kind | Magic | Description |
|------|-------|-------------|
| Inode_v2_root | 'R' (0x52) | V2 root inode |
| Inode_v2_nonroot | 'O' (0x4F) | V2 non-root inode |
| Inode_v3_root | 'S' (0x53) | V3 root inode (supports inline contents) |
| Inode_v3_nonroot | 'T' (0x54) | V3 non-root inode (supports inline contents) |

### Configuration

Inlining is controlled by the `inline_contents` configuration option:

```ocaml
Irmin_pack.config ~inline_contents:true root
```

When `inline_contents` is `false` (the default), all contents are stored as separate entries, maintaining backward compatibility.

### Hash Computation

The hash of a node depends on its entries. With inline contents:
- The **content hash** remains unchanged (hash of the raw value)
- The **node hash** changes because the node entry format includes inlined values differently

This means that the same logical tree will have different node hashes depending on whether inlining is enabled. The content values themselves remain semantically equivalent.

## Implementation Details

### Tree Module Integration

The `Irmin.Tree` module checks content size during tree construction:

```ocaml
let of_contents ?(metadata = Metadata.default) c =
  if inline_contents_enabled then
    let len = Repr.Size.of_value Contents.t c in
    if len > 0 && len < 16 then
      `Contents_inlined_3 (c, metadata)
    else
      `Contents (c, metadata)
  else
    `Contents (c, metadata)
```

### Node Value Type Extension

The `Node.value` type is extended with an inlined variant:

```ocaml
type value =
  [ `Node of node_key * contents_key list
  | `Contents of contents_key * metadata
  | `Contents_inlined of string * metadata ]  (* New *)
```

### Backward Compatibility

- **Reading**: Stores with Inode_v3 entries can be read by code supporting the format
- **Writing**: New writes use Inode_v3 when inlining is enabled
- **Migration**: No automatic migration; existing V2 inodes remain valid

## Benefits

1. **Reduced storage**: Small values don't require separate pack entries
2. **Fewer I/O operations**: Reading a small value requires only one pack read (the node)
3. **Better cache locality**: Inlined values are adjacent to their parent node data

## Limitations

1. **Hash incompatibility**: Enabling inlining changes node hashes, making stores incompatible for hash-based comparisons
2. **Threshold fixed**: The 16-byte threshold is hardcoded; very small values still have some overhead
3. **Not retroactive**: Existing contents entries are not automatically inlined on read

## Testing

The inline contents feature is tested in `test/irmin-pack/test_inline_contents.ml`:

1. **Correctness without inlining**: Data stored and retrieved correctly with inlining disabled
2. **Correctness with inlining**: Data stored and retrieved correctly with inlining enabled
3. **Content equivalence**: Same data produces same content values regardless of inlining
4. **Structure verification**: Verifies that small contents are actually stored inline in the node structure

## Future Work

- Configurable inlining threshold
- Automatic migration of existing small contents
- Statistics/metrics for inline vs non-inline contents ratio
