use super::metadata::*;

/// Represent a metadata token
#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub struct Token(pub u32);

/// ``DecodedToken(table, entry)``. ``entry`` is 1-based index.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct DecodedToken(pub u32, pub u32);

impl Into<Token> for u16 {
    fn into(self) -> Token {
        Token(self as u32)
    }
}

impl Into<Token> for u32 {
    fn into(self) -> Token {
        Token(self)
    }
}

impl Into<Token> for DecodedToken {
    fn into(self) -> Token {
        let DecodedToken(table, entry) = self;
        encode_token(table, entry)
    }
}

pub fn decode_resolution_scope_token<T: Into<Token>>(token: T) -> DecodedToken {
    let Token(raw_token) = token.into();
    let tag = raw_token & 0b11;
    let table: u32 = match tag {
        0 => TableKind::Module,
        1 => TableKind::ModuleRef,
        2 => TableKind::AssemblyRef,
        3 => TableKind::TypeRef,
        _ => unreachable!(),
    }
    .into();
    let entry = raw_token >> 2;
    DecodedToken(table, entry)
}

pub fn decode_typedef_or_ref_token<T: Into<Token>>(token: T) -> DecodedToken {
    let Token(raw_token) = token.into();
    let tag = raw_token & 0b11;
    let idx = raw_token >> 2;
    DecodedToken(
        match tag {
            0 => TableKind::TypeDef.into(),
            1 => TableKind::TypeRef.into(),
            2 => TableKind::TypeSpec.into(),
            _ => unreachable!(),
        },
        idx,
    )
}

pub fn decode_member_ref_parent_token<T: Into<Token>>(token: T) -> DecodedToken {
    let Token(raw_token) = token.into();
    let tag = raw_token & 0b111;
    let table: u32 = match tag {
        0 => TableKind::TypeDef,
        1 => TableKind::TypeRef,
        2 => TableKind::ModuleRef,
        3 => TableKind::MethodDef,
        4 => TableKind::TypeSpec,
        _ => unreachable!(),
    }
    .into();
    let entry = raw_token >> 3;
    DecodedToken(table, entry)
}

pub fn encode_token(table: u32, entry: u32) -> Token {
    Token((table << (32 - 8)) | entry)
}

pub fn decode_token<T: Into<Token>>(token: T) -> DecodedToken {
    let Token(raw_token) = token.into();
    let table = raw_token >> (32 - 8);
    let entry = raw_token & 0x00ff_ffff;
    DecodedToken(table, entry)
}
