use crate::metadata::class::ClassInfo;

/// MethodFullPath. Use Vec<_> internally because classes can be nested.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct MethodFullPath<'a>(pub Vec<&'a str>);

/// MethodFullPath(type namespace, type name, method name)
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct MethodPathWithoutAsmName<'a>(pub &'a str, pub &'a str, pub &'a str);

/// TypeFullPath. Use Vec<_> internally because classes can be nested.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct TypeFullPath<'a>(pub Vec<&'a str>);

/// TypeNamespaceAndName(type namespace, type name)
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct TypeNamespaceAndName<'a>(pub &'a str, pub &'a str);

impl<'a> Into<Vec<&'a str>> for TypeFullPath<'a> {
    fn into(self) -> Vec<&'a str> {
        self.0
    }
}

impl<'a> Into<Vec<&'a str>> for TypeNamespaceAndName<'a> {
    fn into(self) -> Vec<&'a str> {
        let full: TypeFullPath = self.into();
        full.into()
    }
}

impl<'a> Into<TypeFullPath<'a>> for TypeNamespaceAndName<'a> {
    fn into(self) -> TypeFullPath<'a> {
        let TypeNamespaceAndName(type_namespace, type_name) = self;
        TypeFullPath(vec!["", type_namespace, type_name])
    }
}

impl<'a> Into<Vec<&'a str>> for MethodFullPath<'a> {
    fn into(self) -> Vec<&'a str> {
        self.0
    }
}

impl<'a> Into<Vec<&'a str>> for MethodPathWithoutAsmName<'a> {
    fn into(self) -> Vec<&'a str> {
        let full: MethodFullPath = self.into();
        full.into()
    }
}

impl<'a> Into<MethodFullPath<'a>> for MethodPathWithoutAsmName<'a> {
    fn into(self) -> MethodFullPath<'a> {
        let MethodPathWithoutAsmName(type_namespace, type_name, method_name) = self;
        MethodFullPath(vec!["", type_namespace, type_name, method_name])
    }
}

impl<'a> Into<TypeFullPath<'a>> for &'a ClassInfo {
    fn into(self) -> TypeFullPath<'a> {
        TypeFullPath(vec![
            self.resolution_scope.get_name(),
            self.namespace.as_str(),
            self.name.as_str(),
        ])
    }
}

impl<'a> TypeFullPath<'a> {
    pub fn with_method_name(self, name: &'a str) -> MethodFullPath<'a> {
        let TypeFullPath(mut path) = self;
        path.push(name);
        MethodFullPath(path)
    }
}
