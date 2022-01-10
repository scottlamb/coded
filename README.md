# coded

This is a concrete error type with an `ErrorKind` enum matching
Google's "canonical error codes". You may know these from [Google Cloud
errors](https://cloud.google.com/apis/design/errors),
[`absl::Status`](https://abseil.io/docs/cpp/guides/status), or
[gRPC status codes](https://grpc.github.io/grpc/core/md_doc_statuscodes.html).

## Status

The error code enum is exceptionally stable. The overall crate API is a work in
progress. Ideas welcome!

I'll convert [Moonfire NVR](https://github.com/scottlamb/moonfire-nvr) to this
shortly.

## Example

```rust
use coded::{bail, err, Error, ErrorBuilder, ErrorKind, ResultExt};

/// Reads application config from its well-known paths.
fn read_config() -> Result<MyConfig, Error> {
    for path in &PATHS {
        match read_json(p) {
            Ok(c) => return Ok(c),
            Err(e) if e.kind() == ErrorKind::NotFound => { /* keep trying */ },

            // The `bail!` macro is a convenient, flexible shorthand.
            Err(e) => bail!(e, msg("can't read {}", p.display()))
        }
    }

    // `bail!` lets us write `NotFound` without `use ErrorKind::*`.
    bail!(NotFound, msg("no config file at any of {:?}", PATHS))
}

/// Reads a JSON object from the given path.
/// 
/// This returns an `ErrorBuilder` rather than an `Error`, avoiding a redundant
/// entry in the error chain when it's wrapped by the caller.
fn read_json<T: Deserialize>(p: &Path) -> Result<(), ErrorBuilder> {
    // There's automatic conversion from std::io::Error to coded::ErrorBuilder which
    // selects an appropriate ErrorKind.
    let raw = std::fs::read(p)?;

    // ResultExt::err_kind wraps any std::error::Error impl, using the supplied
    // kind. It doesn't add a message.
    serde_json::from_str(&raw).err_kind(ErrorKind::InvalidArgument)
}

fn main() {
    if let Err(e) = inner_main() {
        // `Error::chain` prints not only `e` itself but also the full chain of sources.
        eprintln!("Fatal error:\n{}", e.chain());
        std::process::exit(1);
    }
}

fn inner_main() -> Result<(), Error> {
    let config = read_config()?;

    // ...
}
```

## When should I use it?

*   When you want the advantages of this single well-designed,
    general-purpose error code enum:
    *   *familiarity:* when you use the same error codes widely, the expectations
        for handling them are clear.
    *   *monitoring:* you can meaningfully aggregate errors returned by
        different APIs with these codes.
    *   *stability:* existing error codes and their numbers will never change.
        The enum is marked `#[non_exhaustive]` because new codes could be
        added, but this hasn't happened since 2015. This is great for RPC
        or crate boundaries.
    *   *gRPC interoperability:* many services (not only Google's) use these
        error codes already.
*   When you want your errors to emphasize how the caller should handle them
    rather than details of your implementation or dependencies. See the blog
    post [Rust Error Handling](https://www.unwoundstack.com/blog/rust-error-handling.html).
*   When returning `Ok` has to be cheap. A `Result<(), coded::Error>` is
    one word, so returning `Ok` is [faster](https://github.com/rust-lang-deprecated/failure/issues/9)
    than with larger error types.
*   When you want rich human-readable error messages with the code, details,
    complete error chain, and more. Currently "more" can be stack traces
    (controlled by the application's `Cargo.toml` and environment variables).
    In the future, perhaps
    [`tracing_error::SpanTrace`](https://crates.io/crates/tracing-error) and/or
    arbitrary payloads.
*   When you want to return errors easily with the `err!` and `bail!` macros.

## When shouldn't I use it?

*   When you don't care about error codes at all. You might be more interested
    in [`anyhow`](https://crates.io/crates/anyhow),
    [`eyre`](https://crates.io/crates/eyre), or
    [`snafu::Whatever`](https://docs.rs/snafu/0.7.0/snafu/struct.Whatever.html).
*   When you want an absolutely stable error type *right now*. As written above,
    the actual enum values aren't changing, but it's a little early for the rest
    of `coded`'s API to reach 1.0. (Note: if there's demand, I could split
    the absolutely-stable `coded::{ErrorKind, ToErrorKind}` types into
    their own crate. Then you could have a stable error type by wrapping
    `coded::Error` in your own crate's public `Error` type.)
*   When you need exhaustive enums with custom fields to guide the caller in
    handling domain-specific errors, sometimes at the cost of API stability.
*   When returning `Err` has to be cheap. `coded::Error` isn't cheap: it
    requires heap allocation, and currently stack traces can't be disabled for
    particular libraries or call sites.
*   When you want to just pass along other crates' errors with `?` without
    having to make your own wrapper around those error types and/or
    `coded`. Due to [Rust's orphan
    rule](https://rust-lang.github.io/chalk/book/clauses/coherence.html),
    `coded::ToErrorKind` can only be implemented where the error is defined
    or in `coded`. This limits ergonomics. (Once specialization is stable,
    `?` *could* pass along other types using `ErrorKind::Unknown`, but this
    might be more of a footgun than a help. Likewise, we could fight the
    orphan rule with something like
    [`inventory`](https://github.com/dtolnay/inventory), but we probably
    shouldn't.)

[Error Handling in a Correctness-Critical Rust Project](http://sled.rs/errors.html)
describes how many of these apply to the [`sled`](https://crates.io/crates/sled)
database.

If you need your own error type but hate writing boilerplate, try the derive
macros from [`thiserror`](https://crates.io/crates/thiserror) or
[`snafu`](https://crates.io/crates/snafu)).

## How should I use it?

Return `coded::Error`. Use comments to document the error kinds your API
returns in certain situations. Feel free to add additional error kinds without a
semver break, as callers must match non-exhaustively.

## What's missing?

*   The ability to extend the status with typed payloads as `absl::Status`
    supports. I'd like to use support baked into the `std::error::Error` trait
    for this (see [RFC 2895](https://github.com/rust-lang/rfcs/pull/2895)) but
    it doesn't exist yet. `coded` might grow its own API for this in the
    meantime.
*   Support for serializing and deserializing as protobufs. There are at least
    three Rust protobuf libraries ([`prost`](https://crates.io/crates/prost),
    [`protobuf`](https://crates.io/crates/protobuf), and
    [`quick-protobuf`](https://crates.io/crates/quick-protobuf)). We could
    support each via Cargo feature flags.
    
## License

Apache 2.0 or MIT, at your option.

## Author

Scott Lamb &lt;slamb@slamb.org>
