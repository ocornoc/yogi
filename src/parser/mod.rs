use std::convert::*;
use std::str::FromStr;
use std::array::IntoIter;
use thiserror::Error;
use arrayvec::ArrayVec;
use crate::arith::{Number, YString};

pub mod cst;
pub mod pre_ast;
pub mod ast;

const LINE_LENGTH: usize = 70;
pub const NUM_LINES: usize = 20;

pub mod raw {
    use super::*;

    #[derive(Debug, Default)]
    pub struct Line(pub ArrayVec<char, LINE_LENGTH>);

    #[derive(Debug, Default)]
    pub struct Script(pub [Line; NUM_LINES]);
}
