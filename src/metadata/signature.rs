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
    Char,
    I4,
    String,
    Class(ClassInfoRef),
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

    pub fn void_ty() -> Self {
        Self::new(ElementType::Void)
    }

    pub fn string_ty() -> Self {
        Self::new(ElementType::String)
    }

    pub fn boolean_ty() -> Self {
        Self::new(ElementType::Boolean)
    }

    pub fn char_ty() -> Self {
        Self::new(ElementType::Char)
    }

    pub fn i4_ty() -> Self {
        Self::new(ElementType::I4)
    }

    pub fn simple_method_ty(ret: Type, params: &[Type]) -> Self {
        Self::new(ElementType::FnPtr(Box::new(MethodSignature {
            info: 0,
            ret,
            params: params.to_vec(),
        })))
    }

    pub fn full_method_ty(flags: u8, ret: Type, params: &[Type]) -> Self {
        Self::new(ElementType::FnPtr(Box::new(MethodSignature {
            info: flags,
            ret,
            params: params.to_vec(),
        })))
    }

    pub fn into_type<'a>(image: &Image, sig: &mut Iter<'a, u8>) -> Option<Self> {
        match sig.next()? {
            0x01 => Some(Type::new(ElementType::Void)),
            0x02 => Some(Type::new(ElementType::Boolean)),
            0x03 => Some(Type::new(ElementType::Char)),
            0x08 => Some(Type::new(ElementType::I4)),
            0x0e => Some(Type::new(ElementType::String)),
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

    pub fn as_class(&self) -> Option<&ClassInfoRef> {
        match self.base {
            ElementType::Class(ref class) => Some(class),
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
        let class_ref = image.class_cache.get(&token)?;
        Some(Type::new(ElementType::Class(class_ref.clone())))
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

impl MethodSignature {
    pub fn has_this(&self) -> bool {
        self.info & 0x20 > 0
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
