// Copyright (C) 2022 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0
//
// The `ErrorKind` variant names, numbers, and descriptions are copied from
// Google source code. I believe this is fair use of an API as established in
// Google LLC v. Oracle America, Inc. In any case, the original Google code is
// under the Apache-2.0 license also.

//! Concrete error type with an `ErrorKind` enum matching Google's "canonical
//! error codes" and associated types/helpers.

use std::backtrace::{Backtrace, BacktraceStatus};
use std::convert::From;
use std::error::Error as StdError;
use std::fmt::Display;

/// A general-purpose error with a "kind" from Google's canonical error space.
///
/// This currently tracks:
///
/// *   The error "kind", taken from Google's canonical error space.
/// *   A human-readable message.
/// *   An optional source/cause, exposed through `std::error::Error::cause`.
/// *   An optional backtrace. This is present if the program was run with `RUST_BACKTRACE` or
///     `RUST_LIB_BACKTRACE` set as described at [`std::backtrace`].
///
/// The `Display` impl will display a short summary of this error itself.
/// It *won't* display the chain of sources or the backtrace.
///
/// To display this error along with its causes and stack trace, use the `chain`
/// method.
#[derive(Debug)]
pub struct Error(Box<ErrorInt>);

impl Error {
    /// Returns a new error with the given kind and message.
    pub fn new(kind: ErrorKind, msg: String) -> Self {
        Error(Box::new(ErrorInt {
            kind,
            msg: Some(msg),
            source: None,
            backtrace: Backtrace::capture(),
        }))
    }

    /// Returns a new error wrapping a source with no additional message.
    pub fn wrap<E: StdError + Send + Sync + 'static>(
        kind: ErrorKind,
        msg: Option<String>,
        source: E,
    ) -> Self {
        Error(Box::new(ErrorInt {
            kind,
            msg,
            source: Some(source.into()),
            backtrace: Backtrace::capture(),
        }))
    }

    /// Returns the error kind.
    #[inline]
    pub fn kind(&self) -> ErrorKind {
        self.0.kind
    }

    /// Returns a borrowed value which can display not only this error but also
    /// the full chain of causes and (where applicable) the stack trace.
    ///
    /// The exact format may change. Currently, it displays the stack trace for
    /// the current error but not any of the sources.
    pub fn chain(&self) -> impl Display + '_ {
        ErrorChain(self)
    }

    /// Returns a `Backtrace` object.
    ///
    /// The returned backtrace may or may not have been captured; see [`Backtrace::status`].
    pub fn backtrace(&self) -> &Backtrace {
        &self.0.backtrace
    }
}

/// Formats this error alone (*not* its full chain).
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.msg {
            None => std::fmt::Display::fmt(self.0.kind.grpc_name(), f),
            Some(ref msg) => write!(f, "{}: {}", self.0.kind.grpc_name(), msg),
        }
    }
}

/// Value returned by [`Error::chain`].
struct ErrorChain<'a>(&'a Error);

impl Display for ErrorChain<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)?;
        let mut source = self.0.source();
        while let Some(n) = source {
            write!(f, "\ncaused by: {}", n)?;
            source = n.source()
        }
        let int = &self.0 .0;
        match int.backtrace.status() {
            BacktraceStatus::Unsupported => Ok(()),
            BacktraceStatus::Disabled => {
                write!(
                    f,
                    "\n\nnote: run with `RUST_BACKTRACE=1` environment variable to display a \
                     backtrace"
                )
            }
            BacktraceStatus::Captured => {
                write!(f, "\n\nBacktrace:\n{}", &int.backtrace)
            }
            _ => write!(f, "\n\nUnknown backtrace status"),
        }
    }
}

impl StdError for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        // https://users.rust-lang.org/t/question-about-error-source-s-static-return-type/34515/8
        self.0.source.as_ref().map(|e| e.as_ref() as &_)
    }
}

impl From<ErrorBuilder> for Error {
    #[inline]
    fn from(builder: ErrorBuilder) -> Self {
        builder.build()
    }
}

/// Builder for [`Error`].
pub struct ErrorBuilder(Box<ErrorInt>);

impl Default for ErrorBuilder {
    #[inline]
    fn default() -> Self {
        Self(Box::new(ErrorInt {
            kind: ErrorKind::Unknown,
            msg: None,
            source: None,
            backtrace: Backtrace::capture(),
        }))
    }
}

impl ErrorBuilder {
    #[inline]
    pub fn kind(mut self, kind: ErrorKind) -> Self {
        self.0.kind = kind;
        self
    }

    #[inline]
    pub fn msg(mut self, msg: String) -> Self {
        self.0.msg = Some(msg);
        self
    }

    #[inline]
    pub fn source<S: Into<Box<dyn StdError + Send + Sync + 'static>>>(mut self, source: S) -> Self {
        self.0.source = Some(source.into());
        self
    }

    #[inline]
    pub fn build(self) -> Error {
        Error(self.0)
    }
}

/// Creates a new builder for an error which uses `e` for its source and kind.
impl<E> From<E> for ErrorBuilder
where
    E: StdError + 'static + Sync + Send + ToErrKind,
{
    #[inline(always)]
    fn from(e: E) -> Self {
        let kind = e.err_kind();
        Self::default().kind(kind).source(e)
    }
}

/// Crates a new builder for an error of the given kind.
impl From<ErrorKind> for ErrorBuilder {
    fn from(k: ErrorKind) -> Self {
        Self::default().kind(k)
    }
}

#[derive(Debug)]
struct ErrorInt {
    kind: ErrorKind,
    msg: Option<String>,
    source: Option<Box<dyn StdError + Send + Sync + 'static>>,
    backtrace: std::backtrace::Backtrace,
}

/// Error kind matching Google's "canonical error codes".
///
/// You may know these from [Google Cloud
/// errors](https://cloud.google.com/apis/design/errors),
/// [`absl::Status`](https://abseil.io/docs/cpp/guides/status), or
/// [gRPC status codes](https://grpc.github.io/grpc/core/md_doc_statuscodes.html).
///
/// These codes [haven't
/// changed](https://github.com/googleapis/googleapis/commits/master/google/rpc/code.proto)
/// since they were made public in 2015. It's possible a new code will be added
/// some day; existing codes will never change name or number.
#[derive(Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
#[non_exhaustive]
pub enum ErrorKind {
    /// The operation was cancelled (typically by the caller).
    Cancelled = 1,

    /// Unknown error.
    ///
    /// An example of where this error may be returned is if a Status value
    /// received from another address space belongs to an error-space that is
    /// not known in this address space. Also errors raised by APIs that do not
    /// return enough error information may be converted to this error.
    Unknown = 2,

    /// Client specified an invalid argument.
    ///
    /// Note that this differs from `FailedPrecondition`. `InvalidArgument`
    /// indicates arguments that are problematic regardless of the state of the
    /// system (e.g., a malformed file name).
    InvalidArgument = 3,

    /// Deadline expired before operation could complete.
    ///
    /// For operations that change the state of the system, this error may be
    /// returned even if the operation has completed successfully. For example,
    /// a successful response from a server could have been delayed long enough
    /// for the deadline to expire.
    DeadlineExceeded = 4,

    /// Some requested entity (e.g., file or directory) was not found.
    NotFound = 5,

    /// Some entity that we attempted to create (e.g., file or directory) already
    /// exists.
    AlreadyExists = 6,

    /// The caller does not have permission to execute the specified operation.
    ///
    /// `PermissionDenied` must not be used for rejections caused by exhausting
    /// some resource (use `ResourceExhausted` instead for those errors).
    /// `PermissionDenied` must not be used if the caller can not be identified
    /// (use `Unauthenticated` instead for those errors).
    PermissionDenied = 7,

    /// Some resource has been exhausted, perhaps a per-user quota, or perhaps the
    /// entire file system is out of space.
    ResourceExhausted = 8,

    /// Operation was rejected because the system is not in a state required for
    /// the operation's execution.
    ///
    /// For example, directory to be deleted may be non-empty, an rmdir
    /// operation is applied to a non-directory, etc.
    ///
    /// A litmus test that may help a service implementor in deciding
    /// between `FailedPrecondition`, `Aborted`, and `Unavailable`:
    ///
    /// 1.  Use `Unavailable` if the client can retry just the failing call.
    /// 2.  Use `Aborted` if the client should retry at a higher-level
    ///     (e.g., restarting a read-modify-write sequence).
    /// 3.  Use `FailedPrecondition` if the client should not retry until
    ///     the system state has been explicitly fixed. E.g., if an "rmdir"
    ///     fails because the directory is non-empty, `FailedPrecondition`
    ///     should be returned since the client should not retry unless
    ///     they have first fixed up the directory by deleting files from it.
    /// 4.  Use `FailedPrecondition` if the client performs conditional
    ///     REST Get/Update/Delete on a resource and the resource on the
    ///     server does not match the condition. E.g., conflicting
    ///     read-modify-write on the same resource.
    FailedPrecondition = 9,

    /// The operation was aborted, typically due to a concurrency issue like
    /// sequencer check failures, transaction aborts, etc.
    ///
    /// See litmus test above for deciding between `FailedPrecondition`,
    /// `Aborted`, and `Unavailable`.
    Aborted = 10,

    /// Operation was attempted past the valid range. E.g., seeking or reading
    /// past end of file.
    ///
    /// Unlike `InvalidArgument`, this error indicates a problem that may be fixed
    /// if the system state changes. For example, a 32-bit file system will
    /// generate `InvalidArgument` if asked to read at an offset that is not in the
    /// range [0,2^32-1], but it will generate `OutOfRange` if asked to read from
    /// an offset past the current file size.
    ///
    /// There is a fair bit of overlap between `FailedPrecondition` and
    /// `OutOfRange`. We recommend using `OutOfRange` (the more specific error)
    /// when it applies so that callers who are iterating through a space can
    /// easily look for an `OutOfRange` error to detect when they are done.
    OutOfRange = 11,

    /// Operation is not implemented or not supported/enabled in this service.
    Unimplemented = 12,

    /// Internal errors.
    ///
    /// Means some invariants expected by underlying System has been broken. If
    /// you see one of these errors, Something is very broken.
    Internal = 13,

    /// The service is currently unavailable. This is a most likely a transient
    /// condition and may be corrected by retrying with a backoff.
    ///
    /// **Warning:** Although data MIGHT not have been transmitted when this
    /// status occurs, there is NOT A GUARANTEE that the server has not seen
    /// anything. So in general it is unsafe to retry on this status code
    /// if the call is non-idempotent.
    ///
    /// See litmus test above for deciding between `FailedPrecondition`,
    /// `Aborted`, and `Unavailable`.
    Unavailable = 14,

    /// Unrecoverable data loss or corruption.
    DataLoss = 15,

    /// The request does not have valid authentication credentials for the
    /// operation.
    Unauthenticated = 16,
}

impl ErrorKind {
    /// Returns this code's name in the gRPC `SHOUTY_SNAKE_CASE` style.
    pub fn grpc_name(self) -> &'static str {
        match self {
            ErrorKind::Cancelled => "CANCELLED",
            ErrorKind::Unknown => "UNKNOWN",
            ErrorKind::InvalidArgument => "INVALID_ARGUMENT",
            ErrorKind::DeadlineExceeded => "DEADLINE_EXCEEDED",
            ErrorKind::NotFound => "NOT_FOUND",
            ErrorKind::AlreadyExists => "ALREADY_EXISTS",
            ErrorKind::PermissionDenied => "PERMISSION_DENIED",
            ErrorKind::ResourceExhausted => "RESOURCE_EXHAUSTED",
            ErrorKind::FailedPrecondition => "FAILED_PRECONDITION",
            ErrorKind::Aborted => "ABORTED",
            ErrorKind::OutOfRange => "OUT_OF_RANGE",
            ErrorKind::Unimplemented => "UNIMPLEMENTED",
            ErrorKind::Internal => "INTERNAL",
            ErrorKind::Unavailable => "UNAVAILABLE",
            ErrorKind::DataLoss => "DATA_LOSS",
            ErrorKind::Unauthenticated => "UNAUTHENTICATED",
        }
    }

    /// Returns an `ErrorKind` for a client to use when a server returns the
    /// given HTTP status and no grpc-status, according to [gRPC's
    /// mappings](https://github.com/grpc/grpc/blob/master/doc/http-grpc-status-mapping.md).
    pub fn from_grpc_http_status(status: u16) -> Self {
        use ErrorKind::*;
        match status {
            400 /* Bad Request */ => Internal,
            401 /* Unauthorized	*/ => Unauthenticated,
            403 /* Forbidden */ => PermissionDenied,
            404 /* Not Found */ => Unimplemented,
            429 /* Too Many Requests */ => Unavailable,
            502 /* Bad Gateway */ => Unavailable,
            503 /* Service Unavailable */ => Unavailable,
            504 /* Gateway Timeout */ => Unavailable,
            _ => Unknown,
        }
    }

    /// Returns a lossy mapping to HTTP status codes, as defined in
    /// [`code.proto`](https://github.com/googleapis/googleapis/commits/master/google/rpc/code.proto)
    pub fn to_http_status(self) -> u16 {
        use ErrorKind::*;
        match self {
            Cancelled => 499,          /* Client Closed Request */
            Unknown => 500,            /* Internal Server Error */
            InvalidArgument => 400,    /* Bad Request */
            DeadlineExceeded => 504,   /* Gateway Timeout */
            NotFound => 404,           /* Not Found */
            AlreadyExists => 409,      /* Conflict */
            PermissionDenied => 403,   /* Forbidden */
            Unauthenticated => 401,    /* Unauthenticated */
            ResourceExhausted => 429,  /* Too Many Requests */
            FailedPrecondition => 400, /* Bad Request */
            Aborted => 409,            /* Conflict */
            OutOfRange => 400,         /* Bad Request */
            Unimplemented => 501,      /* Not Implemented */
            Internal => 500,           /* Internal Server Error */
            Unavailable => 503,        /* Service Unavailable */
            DataLoss => 500,           /* Internal Server Error */
        }
    }
}

impl std::fmt::Debug for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.grpc_name(), f)
    }
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.grpc_name(), f)
    }
}

/// Lossy conversion from `std::io::ErrorKind`.
///
/// Note that kinds which are currently mapped to `Unknown` may map to something
/// else in the future without a semver break.
impl From<std::io::ErrorKind> for ErrorKind {
    fn from(k: std::io::ErrorKind) -> Self {
        use std::io;
        use ErrorKind::*;
        match k {
            io::ErrorKind::NotFound => NotFound,

            // Matching PermissionDenied rather than converting to
            // Unauthenticated. Unauthenticated would mean there are no
            // credentials. io::ErrorKind::PermissionDenied seems to refer to
            // the ambient authentication context (uid/gid, capabilities, etc.)
            // which by definition always exists.
            io::ErrorKind::PermissionDenied => PermissionDenied,

            io::ErrorKind::ConnectionRefused => Unavailable,
            io::ErrorKind::ConnectionReset => Unavailable,
            io::ErrorKind::ConnectionAborted => Unavailable,
            io::ErrorKind::NotConnected => FailedPrecondition,
            io::ErrorKind::AddrInUse => FailedPrecondition,
            io::ErrorKind::AddrNotAvailable => FailedPrecondition,

            // FailedPrecondition rather than alternatives:
            // * Unavailable falsely implies retrying the write alone might succeed.
            // * Aborted refers to a "concurrency issues" which doesn't apply,
            //   and suggests retrying a higher-level operation might succeed.
            //   This function doesn't know that. If a caller does, it can
            //   convert to Aborted or Unavailable as appropriate.
            io::ErrorKind::BrokenPipe => FailedPrecondition,

            io::ErrorKind::AlreadyExists => FailedPrecondition,
            io::ErrorKind::WouldBlock => FailedPrecondition,
            io::ErrorKind::InvalidInput => InvalidArgument,
            io::ErrorKind::InvalidData => InvalidArgument,
            io::ErrorKind::TimedOut => DeadlineExceeded,
            io::ErrorKind::WriteZero => ResourceExhausted,
            io::ErrorKind::Interrupted => Aborted,
            io::ErrorKind::Unsupported => Unimplemented,
            io::ErrorKind::UnexpectedEof => OutOfRange,
            io::ErrorKind::OutOfMemory => ResourceExhausted,

            // There are currently several `io::ErrorKind`s gated by `io_error_more`:
            // <https://github.com/rust-lang/rust/issues/86442>. Mapping these
            // all to `Unknown` for now. We could map them to a more specific `ErrorKind`
            // with an unstable feature.
            _ => Unknown,
        }
    }
}

/// A type which can produce an [`ErrorKind`].
///
/// `impl ToErrKind for T` is conceptually the same as `impl Into<ErrKind> for &T` (note
/// the added `&`). This trait seems more clear.
pub trait ToErrKind {
    fn err_kind(&self) -> ErrorKind;
}

impl ToErrKind for Error {
    #[inline]
    fn err_kind(&self) -> ErrorKind {
        self.kind()
    }
}

impl ToErrKind for std::io::Error {
    #[inline]
    fn err_kind(&self) -> ErrorKind {
        self.kind().into()
    }
}

/// Extension methods for `Result`.
pub trait ResultExt<T, E> {
    /// Builds an [`Error`] with the given kind, and the original error as source.
    ///
    /// Example:
    /// ```
    /// use coded::{Error, ErrorKind, ResultExt};
    /// use std::io::Read;
    /// fn foo() -> Result<(), Error> {
    ///     let mut buf = [0u8; 1];
    ///     std::io::Cursor::new("").read_exact(&mut buf[..]).err_kind(ErrorKind::Internal)?;
    ///     Ok(())
    /// }
    /// assert_eq!(foo().unwrap_err().kind(), coded::ErrorKind::Internal);
    /// ```
    fn err_kind<K: Into<ErrorKind>>(self, k: K) -> Result<T, ErrorBuilder>
    where
        E: StdError + 'static;

    /// Shorthand for `err_kind(ErrorKind::Unknown)`.
    fn err_unknown(self) -> Result<T, ErrorBuilder>
    where
        Self: Sized,
        E: StdError + 'static,
    {
        self.err_kind(ErrorKind::Unknown)
    }
}

impl<T, E: StdError + Send + Sync + 'static> ResultExt<T, E> for Result<T, E> {
    /// Wraps errors using the given kind.
    #[inline]
    fn err_kind<K: Into<ErrorKind>>(self, k: K) -> Result<T, ErrorBuilder> {
        self.map_err(|e| ErrorBuilder::default().kind(k.into()).source(Box::new(e)))
    }
}

/// Returns an [`Error`] tersely via `return Err(err!(...))`.
///
/// See [`err`].
///
/// ## Example
///
/// ```
/// use coded::bail;
/// let e = || -> Result<(), coded::Error> {
///     bail!(Unauthenticated, msg("unknown user: {}", "slamb"));
/// }().unwrap_err();
/// assert_eq!(e.kind(), coded::ErrorKind::Unauthenticated);
/// assert!(e.to_string().starts_with("UNAUTHENTICATED: unknown user: slamb"));
/// ```
#[macro_export]
macro_rules! bail {
    ($($body:tt)*) => { return $crate::Err($crate::err!($($body)*)); };
}

/// Re-export of [`std::result::Result::Err`] for `bail!`.
#[doc(hidden)]
pub use Err;

/// Constructs an [`Error`], tersely.
///
/// This is a shorthand way to use [`ErrorBuilder`].
///
/// The first argument is an `Into<ErrorBuilder>`, such as the following:
///
/// *   an [`ErrorKind`] enum variant name like `Unauthenticated`.
///     There's an implicit `use ::coded::ErrorKind::*` to allow the bare
///     variant names just within this restrictive scope where you're unlikely
///     to have conflicts with other identifiers.
/// *   an [`std::io::Error`] as a source, which sets the new `Error`'s
///     `ErrorKind` based on the `std::io::Error`.
/// *   an `Error` as a source, which similarly copies the `ErrorKind`.
/// *   an existing `ErrorBuilder`, which does not create a new source link.
///
/// Following arguments may be of these forms:
///
/// *   `msg(...)`, which expands to `.msg(format!(...))`. See [`ErrorBuilder::msg`].
/// *   `source(...)`, which simply expands to `.source($src)`. See [`ErrorBuilder::source`].
///
/// ## Examples
///
/// Simplest:
///
/// ```rust
/// # use coded::err;
/// let e = err!(InvalidArgument);
/// let e = err!(InvalidArgument,); // trailing commas are allowed
/// assert_eq!(e.kind(), coded::ErrorKind::InvalidArgument);
/// ```
///
/// Constructing with a fixed error variant name:
///
/// ```rust
/// # use {coded::err, std::error::Error, std::num::ParseIntError};
/// let input = "a12";
/// let src = i32::from_str_radix(input, 10).unwrap_err();
///
/// let e = err!(InvalidArgument, source(src.clone()), msg("bad argument {:?}", input));
/// // The line above is equivalent to:
/// let e2 = ::coded::ErrorBuilder::from(::coded::ErrorKind::InvalidArgument)
///     .source(src.clone())
///     .msg(format!("bad argument {:?}", input))
///     .build();
///
/// assert_eq!(e.kind(), coded::ErrorKind::InvalidArgument);
/// assert_eq!(e.source().unwrap().downcast_ref::<ParseIntError>().unwrap(), &src);
/// ```
///
/// Constructing from an `std::io::Error`:
///
/// ```rust
/// # use coded::err;
/// let e = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
/// let e = err!(e, msg("path {} not found", "foo"));
/// assert_eq!(e.kind(), coded::ErrorKind::NotFound);
/// ```
#[macro_export]
macro_rules! err {
    // This uses the "incremental TT munchers", "internal rules", and "push-down accumulation"
    // patterns explained in the excellent "The Little Book of Rust Macros":
    // <https://veykril.github.io/tlborm/decl-macros/patterns/push-down-acc.html>.

    (@accum $body:tt $(,)?) => {
        $body.build()
    };

    (@accum ($($body:tt)*), source($src:expr) $($tail:tt)*) => {
        $crate::err!(@accum ($($body)*.source($src)) $($tail)*);
    };

    // msg(...) uses the `format!` form even when there's only the format string.
    // This can catch errors (e.g. https://github.com/dtolnay/anyhow/issues/55)
    // and will allow supporting implicit named parameters:
    // https://rust-lang.github.io/rfcs/2795-format-args-implicit-identifiers.html
    (@accum ($($body:tt)*), msg($format:expr) $($tail:tt)*) => {
        $crate::err!(@accum ($($body)*.msg(format!($format))) $($tail)*);
    };
    (@accum ($($body:tt)*), msg($format:expr, $($args:tt)*) $($tail:tt)*) => {
        $crate::err!(@accum ($($body)*.msg(format!($format, $($args)*))) $($tail)*);
    };

    ($builder:expr $(, $($tail:tt)*)? ) => {
        $crate::err!(@accum ({
                use $crate::ErrorKind::*;
                $crate::ErrorBuilder::from($builder)
            })
            , $($($tail)*)*
        )
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    fn _assert_send_sync<T: Send + Sync>() {}

    fn _assert_error_send_sync() {
        _assert_send_sync::<Error>()
    }

    #[test]
    fn chain() {
        let inner = Error::new(ErrorKind::InvalidArgument, "inner error".to_owned());
        let middle = Error::wrap(ErrorKind::Internal, Some("middle error".to_owned()), inner);
        let outer = Error::wrap(ErrorKind::Unknown, Some("outer error".to_owned()), middle);

        // Error's Display impl doesn't follow the links in a chain, to avoid surprises
        // if something wraps it and also follows the chain.
        let msg = outer.to_string();
        assert_eq!(msg.matches("inner error").count(), 0);
        assert_eq!(msg.matches("middle error").count(), 0);
        assert_eq!(msg.matches("outer error").count(), 1);

        // Error::chain()'s purpose is to display the full chain at the caller's explicit request.
        let msg = outer.chain().to_string();
        assert_eq!(msg.matches("inner error").count(), 1);
        assert_eq!(msg.matches("middle error").count(), 1);
        assert_eq!(msg.matches("outer error").count(), 1);
    }

    #[test]
    fn backtrace() {
        std::env::set_var("RUST_LIB_BACKTRACE", "1");

        #[inline(never)]
        fn my_funny_named_fn() -> Error {
            err!(Unknown)
        }

        let e = my_funny_named_fn();
        let s = e.chain().to_string();
        assert!(
            s.contains("my_funny_named_fn"),
            "Error chain unexpectedly had no backtrace:\n{}",
            s
        );
    }
}
