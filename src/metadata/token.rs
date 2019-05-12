use super::metadata::*;

pub type Token = u32;

/// ``DecodedToken(table, entry)``. ``entry`` is 1-based index.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct DecodedToken(pub u32, pub u32);

impl Into<Token> for DecodedToken {
    fn into(self) -> Token {
        let DecodedToken(table, entry) = self;
        encode_token(table, entry)
    }
}

pub fn encode_typedef_or_ref_token(table: TableKind, entry: u32) -> u32 {
    let tag = match table {
        TableKind::TypeDef => 0,
        TableKind::TypeRef => 1,
        TableKind::TypeSpec => 2,
        _ => unreachable!(),
    };
    let idx = entry << 2;
    tag | idx
}

pub fn decode_typedef_or_ref_token(token: u32) -> (u32, u32) {
    let tag = token & 0b11;
    let idx = token >> 2;
    match tag {
        0 => (TableKind::TypeDef.into_num() as u32, idx),
        1 => (TableKind::TypeRef.into_num() as u32, idx),
        2 => (TableKind::TypeSpec.into_num() as u32, idx),
        _ => unreachable!(),
    }
}

pub fn decode_member_ref_parent_token<T: Into<u32>>(token: T) -> DecodedToken {
    let token: u32 = token.into();
    let tag = token & 0b111;
    let table: u32 = match tag {
        0 => TableKind::TypeDef,
        1 => TableKind::TypeRef,
        2 => TableKind::ModuleRef,
        3 => TableKind::MethodDef,
        4 => TableKind::TypeSpec,
        _ => unreachable!(),
    }
    .into();
    let entry = token >> 3;
    DecodedToken(table, entry)
}

pub fn encode_token<T: Into<Token>>(table: T, entry: u32) -> u32 {
    let table = (table.into()) << (32 - 8);
    table | entry
}

pub fn decode_token(token: u32) -> DecodedToken {
    let table = token >> (32 - 8);
    let entry = token & 0x00ff_ffff;
    DecodedToken(table, entry)
}
