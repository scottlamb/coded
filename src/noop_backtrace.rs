// Copyright (C) 2022 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

//! No-op implementation of just enough of [`std::backtrace::Backtrace`].

pub enum Backtrace {}

pub fn capture() -> Option<Backtrace> {
    None
}

impl std::fmt::Debug for Backtrace {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl std::fmt::Display for Backtrace {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}
