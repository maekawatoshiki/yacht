use crate::metadata::class::ClassInfo;

/// MethodFullPath(assembly name, type namespace, type name, method name)
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct MethodFullPath<'a>(pub &'a str, pub &'a str, pub &'a str, pub &'a str);

/// MethodFullPath(type namespace, type name, method name)
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct MethodPathWithoutAsmName<'a>(pub &'a str, pub &'a str, pub &'a str);

/// TypeFullName(assembly name, type namespace, type name)
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct TypeFullPath<'a>(pub &'a str, pub &'a str, pub &'a str);

/// TypeNamespaceAndName(type namespace, type name)
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct TypeNamespaceAndName<'a>(pub &'a str, pub &'a str);

impl<'a> Into<TypeFullPath<'a>> for TypeNamespaceAndName<'a> {
    fn into(self) -> TypeFullPath<'a> {
        let TypeNamespaceAndName(type_namespace, type_name) = self;
        TypeFullPath("", type_namespace, type_name)
    }
}

impl<'a> Into<MethodFullPath<'a>> for MethodPathWithoutAsmName<'a> {
    fn into(self) -> MethodFullPath<'a> {
        let MethodPathWithoutAsmName(type_namespace, type_name, method_name) = self;
        MethodFullPath("", type_namespace, type_name, method_name)
    }
}

impl<'a> Into<TypeFullPath<'a>> for &'a ClassInfo {
    fn into(self) -> TypeFullPath<'a> {
        TypeFullPath(
            self.resolution_scope.get_name(),
            self.namespace.as_str(),
            self.name.as_str(),
        )
    }
}

impl<'a> TypeFullPath<'a> {
    pub fn with_method_name(self, name: &'a str) -> MethodFullPath<'a> {
        let TypeFullPath(asm_name, ty_namespace, ty_name) = self;
        MethodFullPath(asm_name, ty_namespace, ty_name, name)
    }
}
