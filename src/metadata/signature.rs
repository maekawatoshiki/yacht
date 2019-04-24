use crate::metadata::{class::*, metadata::*};
use std::{iter::repeat_with, slice::Iter};

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub base: ElementType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElementType {
    Void,
    Boolean,
    I4,
    String,
    Class(Box<ClassInfo>),
    FnPtr(Box<MethodSignature>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodSignature {
    /// Represent ``HASTHIS``, ``EXPLICITTHIS``, ``DEFAULT``, ``GENERIC`` and ``VARARG``
    pub info: u8,

    /// Return type
    pub ret: Type,

    /// Parameters' types
    pub params: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct SignatureParser<'a> {
    sig: Iter<'a, u8>,
}

impl Type {
    pub fn new(base: ElementType) -> Self {
        Self { base }
    }

    pub fn into_type<'a>(image: &Image, sig: &mut Iter<'a, u8>) -> Option<Self> {
        match sig.next()? {
            0x1 => Some(Type::new(ElementType::Void)),
            0x2 => Some(Type::new(ElementType::Boolean)),
            0x8 => Some(Type::new(ElementType::I4)),
            0xe => Some(Type::new(ElementType::String)),
            0x12 => Type::class_into_type(image, sig),
            // TODO
            // 0x1b => Some(ElementType::FnPtr
            _ => None,
        }
    }

    pub fn as_fnptr(&self) -> Option<&MethodSignature> {
        match self.base {
            ElementType::FnPtr(ref fnptr) => Some(fnptr),
            _ => None,
        }
    }

    pub fn equal_method(&self, ret: ElementType, params: &[ElementType]) -> bool {
        match self.base {
            ElementType::FnPtr(ref ms) => {
                ms.ret.base == ret && ms.params.iter().zip(params).all(|(p, q)| &p.base == q)
            }
            _ => false,
        }
    }

    pub fn is_void(&self) -> bool {
        self.base == ElementType::Void
    }

    fn class_into_type<'a>(image: &Image, sig: &mut Iter<'a, u8>) -> Option<Self> {
        let token = decompress_uint(sig).unwrap();
        let (table, idx) = decode_typedef_or_ref_token(token);
        let tables = &image.metadata.metadata_stream.tables[table];
        let table = &tables[idx - 1];
        let (name, namespace, fields) = match table {
            Table::TypeDef(tdt) => {
                let next_table = tables.get(idx);
                let field_list_bgn = tdt.field_list as usize - 1;
                let field_list_end = match next_table {
                    Some(table) => match table {
                        Table::TypeDef(tdt) => tdt.field_list as usize - 1,
                        _ => unreachable!(),
                    },
                    None => tables.len(),
                };
                let fields = image.metadata.metadata_stream.tables[TableKind::Field.into_num()]
                    [field_list_bgn..field_list_end]
                    .iter()
                    .map(|t| match t {
                        Table::Field(ft) => {
                            let name = image.get_string(ft.name).clone();
                            let mut sig = image.get_blob(ft.signature).iter();
                            assert_eq!(sig.next().unwrap(), &0x06);
                            let ty = Type::into_type(image, &mut sig).unwrap();
                            ClassField { name, ty }
                        }
                        _ => unreachable!(),
                    })
                    .collect();
                (
                    image.get_string(tdt.type_name).clone(),
                    image.get_string(tdt.type_namespace).clone(),
                    fields,
                )
            }
            _ => unimplemented!(),
        };
        Some(Type::new(ElementType::Class(Box::new(ClassInfo {
            name,
            namespace,
            fields,
        }))))
    }
}

impl<'a> SignatureParser<'a> {
    pub fn new(sig: &'a [u8]) -> Self {
        Self { sig: sig.iter() }
    }

    pub fn parse_method_ref_sig(&mut self, image: &Image) -> Option<Type> {
        let first = *self.sig.next()?;
        let _has_this = first & 0x20;
        let _explicit_this = first & 0x40;
        let _var_arg = first & 0x5;

        let param_count = decompress_uint(&mut self.sig)?;
        let ret = Type::into_type(image, &mut self.sig)?;

        let params = repeat_with(|| Type::into_type(image, &mut self.sig).unwrap())
            .take(param_count as usize)
            .collect();

        let _var_arg_params = if let Some(0x41) = self.sig.next() {
            repeat_with(|| Type::into_type(image, &mut self.sig).unwrap())
                .take(param_count as usize)
                .collect()
        } else {
            vec![]
        };

        Some(Type::new(ElementType::FnPtr(Box::new(MethodSignature {
            info: first,
            ret,
            params,
        }))))
    }

    pub fn parse_method_def_sig(&mut self, image: &Image) -> Option<Type> {
        let first = *self.sig.next()?;
        let _default = first == 0;
        let _has_this = first & 0x20;
        let _explicit_this = first & 0x40;
        let _var_arg = first & 0x5;
        let _generic = first & 0x10;

        let param_count = decompress_uint(&mut self.sig)?;
        let ret = Type::into_type(image, &mut self.sig)?;

        let params = repeat_with(|| Type::into_type(image, &mut self.sig).unwrap())
            .take(param_count as usize)
            .collect();

        Some(Type::new(ElementType::FnPtr(Box::new(MethodSignature {
            info: first,
            ret,
            params,
        }))))
    }
}

pub fn decompress_uint<'a>(sig: &mut Iter<'a, u8>) -> Option<u32> {
    let x = *sig.next()? as u32;
    if x & 0b1000_0000 == 0 {
        // 1 byte
        Some(x)
    } else if x & 0b1000_0000 > 0 {
        // 2 bytes
        let y = *sig.next()? as u32;
        Some(((x & 0b0011_1111) << 8) + y)
    } else {
        // 4 bytes
        let y = *sig.next()? as u32;
        let z = *sig.next()? as u32;
        let u = *sig.next()? as u32;
        Some(((x & 0b0001_1111) << 24) + (y << 16) + (z << 8) + u)
    }
}

pub fn decode_typedef_or_ref_token(token: u32) -> (usize, usize) {
    let tag = token & 0b11;
    let idx = token as usize >> 2;
    match tag {
        0 => (TableKind::TypeDef.into_num(), idx),
        1 => (TableKind::TypeRef.into_num(), idx),
        2 => (TableKind::TypeSpec.into_num(), idx),
        _ => unreachable!(),
    }
}
