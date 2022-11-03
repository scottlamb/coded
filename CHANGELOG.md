## in progress

*   BREAKING: `coded::Error` should be `Send + Sync`. In practice, coded 0.1 is
    of limited use because of this oversight.
*   BREAKING: require Rust 1.65+.
*   BREAKING: use `std::backtrace::Backtrace` without a wrapper.

## `v0.1.0` (2022-01-10)

Initial release.
