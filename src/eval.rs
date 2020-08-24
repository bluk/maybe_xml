// Copyright 2020 Bryant Luk
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Evaluators transform scanned byte sequences into complete tokens.
//!
//! Evaluators are usually the first type which most library users
//! should use. The choice of an evaluator is mainly based on how
//! the XML will be read. For instance, if there's an existing
//! byte buffer or some type which can be transformed into a
//! `BufRead` type (such as `String`), then the `BufReadEvaluator`
//! is the best choice.

pub mod bufread;
pub mod recv;
