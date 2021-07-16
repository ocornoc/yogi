use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Operator(char),
    StrLit(String),
    NumLit(String),
    LocalIdent(String),
    GlobalIdent(String),
    If,
    Goto,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Default, Clone)]
pub struct Line(pub ArrayVec<Token, LINE_LENGTH>);

#[derive(Debug, Default, Clone)]
pub struct Script(pub [Line; NUM_LINES]);

#[derive(Debug, Error)]
pub enum ParseErr {
    #[error("no name for global identifier, only ':'")]
    NoGlobalName,
    #[error("unknown symbol '{0}'")]
    UnknownSymbol(char),
    #[error("no digits after decimal")]
    NoDigitsAfterDot,
    #[error("no end quote for string")]
    NoEndQuote,
    #[error("keyword in variable name")]
    KeywordInVariable,
}

fn parse_ident(remainder: &mut &[char], col: &mut usize, id: &mut String) {
    let mut end_idx = remainder.len();
    for (i, c) in remainder.iter().enumerate() {
        if !c.is_alphanumeric() && *c != '_' {
            end_idx = i;
            break;
        }
    }
    let (l, r) = remainder.split_at(end_idx);
    *col += end_idx;
    *remainder = r;
    id.extend(l.iter());
}

fn parse_int(remainder: &mut &[char], col: &mut usize, num: &mut String) {
    let mut end_idx = remainder.len();
    for (i, c) in remainder.iter().enumerate() {
        if !c.is_ascii_digit() {
            end_idx = i;
            break;
        }
    }
    let (l, r) = remainder.split_at(end_idx);
    *col += end_idx;
    *remainder = r;
    num.extend(l.iter());
}

impl TryFrom<raw::Line> for Line {
    type Error = ParseErr;

    fn try_from(value: raw::Line) -> Result<Self, Self::Error> {
        let mut tokens = ArrayVec::new_const();
        let mut remainder = value.0.as_ref();
        let mut col = 0;

        while let &[c, ref r@..] = remainder {
            remainder = r;
            if c.is_alphabetic() || c == '_' {
                let mut id: String = c.into();
                let start = col;
                parse_ident(&mut remainder, &mut col, &mut id);
                id.make_ascii_lowercase();
                if id == "if" {
                    tokens.push(Token {
                        kind: TokenKind::If,
                        start,
                        end: col,
                    });
                } else if id == "goto" {
                    tokens.push(Token {
                        kind: TokenKind::Goto,
                        start,
                        end: col,
                    });
                } else if id.contains("if") || id.contains("goto") {
                    return Err(ParseErr::KeywordInVariable);
                } else {
                    tokens.push(Token {
                        kind: TokenKind::LocalIdent(id),
                        start,
                        end: col,
                    });
                }
            } else if c == ':' {
                let mut id: String = String::with_capacity(2);
                let start = col;
                parse_ident(&mut remainder, &mut col, &mut id);
                id.make_ascii_lowercase();
                if id.is_empty() {
                    return Err(ParseErr::NoGlobalName);
                } else {
                    tokens.push(Token {
                        kind: TokenKind::GlobalIdent(id),
                        start,
                        end: col,
                    });
                }
            } else if c.is_ascii_digit() {
                let mut num = String::with_capacity(5);
                let start = col;
                num.push(c);
                parse_int(&mut remainder, &mut col, &mut num);
                if let Some(('.', r)) = remainder.split_first() {
                    num.push('.');
                    let old_len = num.len();
                    remainder = r;
                    col += 1;
                    parse_int(&mut remainder, &mut col, &mut num);
                    if old_len == num.len() {
                        return Err(ParseErr::NoDigitsAfterDot);
                    }
                }

                tokens.push(Token {
                    kind: TokenKind::NumLit(num),
                    start,
                    end: col,
                });
            } else if c == '/' && matches!(r, ['/', ..]) {
                break;
            } else if c == '"' {
                let mut escaped = false;
                if let Some((i, _)) = remainder
                    .iter()
                    .enumerate()
                    .find(|&(_, &c)| if c == '"' {
                        !escaped
                    } else if c == '\\' {
                        escaped = !escaped;
                        false
                    } else {
                        false
                    })
                {
                    let start = col;
                    let (l, r) = remainder.split_at(i);
                    remainder = &r[1..];
                    col += i;
                    escaped = false;
                    tokens.push(Token {
                        kind: TokenKind::StrLit(l
                            .into_iter()
                            .collect::<String>()
                            .replace(|c| if c == '"' {
                                escaped
                            } else if c == '\\' {
                                escaped = !escaped;
                                false
                            } else {
                                false
                            }, "\"")
                            .replace("\\\\", "\\")
                        ),
                        start,
                        end: col,
                    });
                } else {
                    return Err(ParseErr::NoEndQuote);
                }
            } else if c.is_ascii_whitespace() {
                //nothing
            } else if c.is_ascii_punctuation() {
                //col += 1;
                tokens.push(Token {
                    kind: TokenKind::Operator(c),
                    start: col,
                    end: col,
                });
            } else {
                return Err(ParseErr::UnknownSymbol(c));
            }
            col += 1;
        }

        Ok(Line(tokens))
    }
}

impl TryFrom<raw::Script> for Script {
    type Error = ParseErr;

    fn try_from(lines: raw::Script) -> Result<Self, Self::Error> {
        Ok(Script(IntoIter::new(lines.0)
            .into_iter()
            .map(|l| <raw::Line as TryInto<Line>>::try_into(l))
            .collect::<Result<Vec<_>, _>>()?
            .try_into()
            .unwrap(),
        ))
    }
}
