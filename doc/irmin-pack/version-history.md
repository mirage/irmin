# Version History

Summary of the data versions of `irmin-pack` and key changes for the entries in the pack file.

## Data versions

See `irmin-pack/version.ml`.

| Version  | Description                      |
| -------- | --------                         |
| V1       | Original version of `irmin-pack` |
| V2       | Introduced V2 version of objects/keys. [Irmin 3.0.0](https://github.com/mirage/irmin/releases/tag/3.0.0) via [PR#1655](https://github.com/mirage/irmin/pull/1655) |
| V3       | Introduced control file. [Irmin 3.3.0](https://github.com/mirage/irmin/releases/tag/3.3.0) via [PR#1865](https://github.com/mirage/irmin/pull/1865) |
| V4       | Introduced chunked suffix. [Irmin 3.5.0](https://github.com/mirage/irmin/releases/tag/3.5.0) via [PR#2110](https://github.com/mirage/irmin/pull/2110) |
| V5       | Introduced lower layer + volumes. [Irmin 3.7.0](https://github.com/mirage/irmin/releases/tag/3.7.0) via [PR#2184](https://github.com/mirage/irmin/pull/2184) and [PR#2180](https://github.com/mirage/irmin/pull/2180) |

Prior to `V3`, `irmin-pack` stored version information in headers of files but now stores it only in a header of the control file.

## Pack store entries

See `irmin-pack/pack_value.ml` and `irmin-pack/inode.ml` for how `irmin-pack` represents `irmin`'s core types of contents, nodes, and commits.

### Contents

| Version  | Description                                     |
| -------- | --------                                        |
| V1       | The first and only format for contents entries. |

`Contents` objects only have one version, but a user-defined contents length header [configuration](https://github.com/mirage/irmin/blob/1f046ddaedf3532bb236f7d19510182c5948b9d6/src/irmin-pack/conf.mli#L34-L45) was added in [Irmin 3.0.0](https://github.com/mirage/irmin/releases/tag/3.0.0) via [PR#1644](https://github.com/mirage/irmin/pull/1644). 

💡 Since Tezos' contents type is [bytes](https://github.com/mirage/irmin/blob/1f046ddaedf3532bb236f7d19510182c5948b9d6/src/irmin-tezos/schema.ml#L146) and `repr` stores [bytes](https://github.com/mirage/repr/blob/2b260367d8073a421e38be4e564d2c1931d15ee0/src/repr/type_intf.ml#L10-L11) with a [length prefix](https://github.com/mirage/repr/blob/2b260367d8073a421e38be4e564d2c1931d15ee0/src/repr/binary.ml#L307-L312) that is [LEB128-encoded](https://github.com/mirage/repr/blob/2b260367d8073a421e38be4e564d2c1931d15ee0/src/repr/binary.ml#L104-L112), its contents type automatically supports `Varint` for `contents_length_header`.

### Inodes

| Version  | Description                                         |
| -------- | --------                                            |
| V1       | Original data. Corresponds with V1 keys.            |
| V2       | Introduces length prefix. Corresponds with V2 keys. |


V1 added an [explicit length prefix](https://github.com/mirage/irmin/blob/1f046ddaedf3532bb236f7d19510182c5948b9d6/src/irmin-pack/inode.ml#L400) to support minimal indexing.

### Commits

| Version  | Description                                         |
| -------- | --------                                            |
| V1       | Original data. Corresponds with V1 keys.            |
| V2       | Introduces length prefix. Corresponds with V2 keys. |

V2 added an [explicit length prefix](https://github.com/mirage/irmin/blob/1f046ddaedf3532bb236f7d19510182c5948b9d6/src/irmin-pack/pack_value.ml#L193) to support minimal indexing.

## Keys

See `irmin-pack/unix/pack_key.ml`.

| Version  | Name           | Description                                                           |
| -------- | --------       | --------                                                              |
| V1       | Hash           | Original "keys". Relied on index for all object offsets in pack file. |
| V2       | Structured     | [Irmin 3.0.0](https://github.com/mirage/irmin/releases/tag/3.0.0) via [PR#1659](https://github.com/mirage/irmin/pull/1659). Supports existing `V0` through `Pack_key.Indexed`. All new keys are written as `Pack_key.Direct`. |

Keys do not have versions in the code, but versions are indicated here to correspond to data version.

## Indexing Strategies

| Type        | Data Indexed                   | Description        |
| --------    | --------                       | --------           |
| Non-minimal | All: Contents, Inodes, Commits | Original strategy  |
| Minimal     | Commits                        | [Irmin 3.0.0](https://github.com/mirage/irmin/releases/tag/3.0.0) via [PR#1664](https://github.com/mirage/irmin/pull/1664) |

## Release Dates

Dates of release are a lower bounds on when we can expect different versions of data in stores, but the actual date is later due to delays of when clients ship their updates.

- [Irmin 3.0.0](https://github.com/mirage/irmin/releases/tag/3.0.0) on Feb 14, 2022.
- [Irmin 3.3.0](https://github.com/mirage/irmin/releases/tag/3.3.0) on Jun 20, 2022.
- [Irmin 3.5.0](https://github.com/mirage/irmin/releases/tag/3.5.0) on Dec 14, 2022.
- [Irmin 3.7.0](https://github.com/mirage/irmin/releases/tag/3.5.0) on Apr 26, 2023.
