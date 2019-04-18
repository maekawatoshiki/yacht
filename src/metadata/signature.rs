use std::{iter::repeat_with, slice::Iter};

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    base: ElementType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElementType {
    Void,
    String,
    FnPtr(Box<MethodSignature>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodSignature {
    /// Represent ``HASTHIS``, ``EXPLICITTHIS`` and ``VARARG``
    info: u8,

    /// Return type
    ret: Type,

    /// Parameters' types
    params: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct SignatureParser<'a> {
    sig: Iter<'a, u8>,
}

impl Type {
    pub fn new(base: ElementType) -> Self {
        Self { base }
    }

    pub fn into_type<'a>(sig: &mut Iter<'a, u8>) -> Option<Self> {
        match sig.next()? {
            0x1 => Some(Type::new(ElementType::Void)),
            0xe => Some(Type::new(ElementType::String)),
            // TODO
            // 0x1b => Some(ElementType::FnPtr
            _ => None,
        }
    }
}

impl<'a> SignatureParser<'a> {
    pub fn new(sig: &'a [u8]) -> Self {
        Self { sig: sig.iter() }
    }

    pub fn parse_method_ref_sig(&mut self) -> Option<Type> {
        let first = *self.sig.next()?;
        let _has_this = first & 0x20;
        let _explicit_this = first & 0x40;
        let _var_arg = first & 0x5;

        let param_count = self.decompress_uint()?;
        let ret = Type::into_type(&mut self.sig)?;

        let params = repeat_with(|| Type::into_type(&mut self.sig).unwrap())
            .take(param_count as usize)
            .collect();

        let _var_arg_params = if let Some(0x41) = self.sig.next() {
            repeat_with(|| Type::into_type(&mut self.sig).unwrap())
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

    fn decompress_uint(&mut self) -> Option<u32> {
        let x = *self.sig.next()? as u32;
        if x & 0b1000_0000 == 0 {
            // 1 byte
            Some(x)
        } else if x & 0b1000_0000 > 0 {
            // 2 bytes
            let y = *self.sig.next()? as u32;
            Some(((x & 0b0011_1111) << 8) + y)
        } else {
            // 4 bytes
            let y = *self.sig.next()? as u32;
            let z = *self.sig.next()? as u32;
            let u = *self.sig.next()? as u32;
            Some(((x & 0b0001_1111) << 24) + (y << 16) + (z << 8) + u)
        }
    }
}
