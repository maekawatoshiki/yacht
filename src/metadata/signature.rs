use crate::metadata::{class::*, image::*, token::*};
use std::{fmt, iter::repeat_with, slice::Iter};

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub base: ElementType,
}

#[derive(Clone, PartialEq)]
pub enum ElementType {
    Void,
    Boolean,
    Char,
    I4,
    R8,
    String,
    Class(ClassInfoRef),
    SzArray(Box<SzArrayInfo>),
    FnPtr(Box<MethodSignature>),
    Object,
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

#[derive(Debug, Clone, PartialEq)]
pub struct SzArrayInfo {
    /// Array's element type
    pub elem_ty: Type,
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

    pub fn r8_ty() -> Self {
        Self::new(ElementType::R8)
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

    pub fn i4_szarr_ty() -> Self {
        Self::new(ElementType::SzArray(Box::new(SzArrayInfo {
            elem_ty: Type::i4_ty(),
        })))
    }

    pub fn boolean_szarr_ty() -> Self {
        Self::new(ElementType::SzArray(Box::new(SzArrayInfo {
            elem_ty: Type::boolean_ty(),
        })))
    }

    pub fn object_ty() -> Self {
        Self::new(ElementType::Object)
    }

    pub fn into_type<'a>(image: &Image, sig: &mut Iter<'a, u8>) -> Option<Self> {
        match sig.next()? {
            0x01 => Some(Type::new(ElementType::Void)),
            0x02 => Some(Type::new(ElementType::Boolean)),
            0x03 => Some(Type::new(ElementType::Char)),
            0x08 => Some(Type::new(ElementType::I4)),
            0x0d => Some(Type::new(ElementType::R8)),
            0x0e => Some(Type::new(ElementType::String)),
            0x12 => Type::class_into_type(image, sig),
            0x1c => Some(Type::new(ElementType::Object)),
            0x1d => Some(Type::new(ElementType::SzArray(Box::new(SzArrayInfo {
                elem_ty: Type::into_type(image, sig)?,
            })))),
            // TODO
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
        let class_ref = image
            .class_cache
            .get(&decode_typedef_or_ref_token(token).into())?;
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

impl fmt::Debug for ElementType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "ElementType::{}",
            match self {
                ElementType::Class(c) => format!("Class({})", c.borrow().name),
                ElementType::Void => "Void".to_string(),
                ElementType::Boolean => "Boolean".to_string(),
                ElementType::Char => "Char".to_string(),
                ElementType::I4 => "I4".to_string(),
                ElementType::R8 => "R8".to_string(),
                ElementType::String => "String".to_string(),
                ElementType::SzArray(s) => format!("SzArray({:?})", s),
                ElementType::FnPtr(f) => format!("FnPtr({:?})", f),
                ElementType::Object => format!("Object"),
            }
        )
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
