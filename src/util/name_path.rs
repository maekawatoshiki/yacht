use crate::metadata::class::ClassInfo;

/// MethodPath. Use Vec<_> internally since classes can be nested.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct MethodPath<'a>(pub Vec<&'a str>);

/// TypePath. Use Vec<_> internally since classes can be nested.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct TypePath<'a>(pub Vec<&'a str>);

impl<'a> Into<Vec<&'a str>> for TypePath<'a> {
    fn into(self) -> Vec<&'a str> {
        self.0
    }
}

impl<'a> Into<Vec<&'a str>> for MethodPath<'a> {
    fn into(self) -> Vec<&'a str> {
        self.0
    }
}

impl<'a> Into<TypePath<'a>> for &'a ClassInfo {
    fn into(self) -> TypePath<'a> {
        TypePath(vec![
            self.resolution_scope.get_name(),
            self.namespace.as_str(),
            self.name.as_str(),
        ])
    }
}

impl<'a> TypePath<'a> {
    pub fn with_method_name(self, name: &'a str) -> MethodPath<'a> {
        let TypePath(mut path) = self;
        path.push(name);
        MethodPath(path)
    }
}
