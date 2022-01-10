// Copyright (C) 2022 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

//! Backtrace implementation using `std::backtrace`.

pub use std::backtrace::Backtrace;

pub fn capture() -> Option<Backtrace> {
    let bt = Backtrace::capture();
    if bt.status() == std::backtrace::BacktraceStatus::Captured {
        Some(bt)
    } else {
        None
    }
}
