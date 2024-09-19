# `irmin-pack` Design Docs

- [Layered store](./layered_store.md), the design document for the first version of garbage collection (GC) that shipped with [Irmin 3.4](https://github.com/mirage/irmin/releases/tag/3.4.0).
- [Chunked suffix](./chunked_suffix.md), the design document for the GC's second version, which introduced a chunked suffix for disk space saving during a GC. Introduced in [Irmin 3.5](https://github.com/mirage/irmin/releases/tag/3.5.0).
- [Lower layer](./lower_layer.md), the design document for the GC's third phase, extending it to work for unlimited history stores by archiving instead of deleting data. Will be introduced in Irmin 3.7.
