use crate::metadata::class::ClassInfo;

/// MethodFullPath. Use Vec<_> internally since classes can be nested.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct MethodFullPath<'a>(pub Vec<&'a str>);

/// TypeFullPath. Use Vec<_> internally since classes can be nested.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct TypeFullPath<'a>(pub Vec<&'a str>);

impl<'a> Into<Vec<&'a str>> for TypeFullPath<'a> {
    fn into(self) -> Vec<&'a str> {
        self.0
    }
}

impl<'a> Into<Vec<&'a str>> for MethodFullPath<'a> {
    fn into(self) -> Vec<&'a str> {
        self.0
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
