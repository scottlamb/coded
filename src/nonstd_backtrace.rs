// Copyright (C) 2022 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

//! Implementation of capturing/displaying backtraces with the [`backtrace`] crate.
//!
//! This is provided because `std::backtrace` is
//! [unstable](https://github.com/rust-lang/rust/issues/53487).

use std::{
    cell::UnsafeCell,
    sync::{atomic::AtomicBool, Once},
};

/// Returns true iff backtraces are enabled, with the same
/// [rules](https://doc.rust-lang.org/std/backtrace/index.html#environment-variables)
/// as `std::backtrace`.
fn enabled() -> bool {
    static SET_ENABLED: Once = Once::new();

    // ENABLED isn't Atomic because it's depended on for ordering; SET_ENABLED takes care of that.
    // It's just to avoid fighting the compiler about UnsafeCell not being Sync. There might be
    // some overhead of using an atomic on some platforms, but it's probably minimal with Relaxed.
    static ENABLED: AtomicBool = AtomicBool::new(false);
    SET_ENABLED.call_once(|| {
        let enabled = if std::env::var_os("RUST_LIB_BACKTRACE").map(|s| s != "0") == Some(true) {
            true
        } else {
            std::env::var_os("RUST_BACKTRACE")
                .map(|s| s != "0")
                .unwrap_or(false)
        };
        ENABLED.store(enabled, std::sync::atomic::Ordering::Relaxed);
    });
    ENABLED.load(std::sync::atomic::Ordering::Relaxed)
}

#[inline(always)]
pub fn capture() -> Option<Backtrace> {
    if enabled() {
        Some(Backtrace {
            resolved: Once::new(),
            backtrace: UnsafeCell::new(backtrace::Backtrace::new_unresolved()),
        })
    } else {
        None
    }
}

/// A backtrace that resolves symbols during [`std::fmt::Display`].
pub struct Backtrace {
    resolved: Once,
    backtrace: UnsafeCell<backtrace::Backtrace>,
}

// Backtrace isn't Send+Sync by default because of the backtrace field, but its usage is protected
// by once.
unsafe impl Send for Backtrace {}
unsafe impl Sync for Backtrace {}

impl Backtrace {
    fn resolve(&self) -> &backtrace::Backtrace {
        self.resolved
            .call_once(|| unsafe { &mut *self.backtrace.get() }.resolve());
        unsafe { &*self.backtrace.get() }
    }
}

impl std::fmt::Debug for Backtrace {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.resolve(), f)
    }
}

impl std::fmt::Display for Backtrace {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.resolve(), f)
    }
}
