use std::convert::*;
use std::str::FromStr;
use std::array::IntoIter;
use thiserror::Error;
use crate::arith::{Number, YString};

pub mod cst;
pub mod pre_ast;
pub mod ast;

const LINE_LENGTH: usize = 100;
pub const NUM_LINES: usize = 20;

pub mod raw {
    use super::*;

    #[derive(Debug, Default)]
    pub struct Line(pub Vec<char>);

    #[derive(Debug, Clone, Copy, Error)]
    pub enum ParseErr {
        #[error("Too many lines in the script")]
        TooManyLines,
        #[error("A line was too long")]
        LineTooLong,
        #[error("The script wasn't ASCII")]
        NonAscii,
    }

    #[derive(Debug, Default)]
    pub struct Script(pub [Line; NUM_LINES]);

    impl FromStr for Script {
        type Err = ParseErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            if !s.is_ascii() {
                return Err(ParseErr::NonAscii);
            }
            let mut script = Self::default();
            for (i, line) in s.lines().enumerate() {
                if i >= NUM_LINES {
                    return Err(ParseErr::TooManyLines);
                }
                let line = line.chars().collect::<Vec<_>>();
                if line.len() >= LINE_LENGTH {
                    return Err(ParseErr::LineTooLong);
                }
                script.0[i] = Line(line);
            }
            Ok(script)
        }
    }
}
