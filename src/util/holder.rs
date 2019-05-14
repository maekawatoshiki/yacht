use super::name_path::*;
use rustc_hash::FxHashMap;

#[derive(Clone, Debug)]
pub struct TypeHolder<T> {
    map: FxHashMap<String, FxHashMap<String, FxHashMap<String, T>>>,
}

impl<T> TypeHolder<T> {
    pub fn new() -> Self {
        TypeHolder {
            map: FxHashMap::default(),
        }
    }

    pub fn get<'a, P: Into<TypeFullPath<'a>>>(&self, path: P) -> Option<&T> {
        let TypeFullPath(asm_name, ty_namespace, ty_name) = path.into();
        self.map.get(asm_name)?.get(ty_namespace)?.get(ty_name)
    }

    pub fn add<'a, P: Into<TypeFullPath<'a>>>(&mut self, path: P, val: T) {
        let TypeFullPath(asm_name, ty_namespace, ty_name) = path.into();
        self.map
            .entry(asm_name.to_string())
            .or_insert(FxHashMap::default())
            .entry(ty_namespace.to_string())
            .or_insert(FxHashMap::default())
            .insert(ty_name.to_string(), val);
    }
}

#[derive(Clone, Debug)]
pub struct MethodHolder<T: Clone> {
    map: FxHashMap<String, FxHashMap<String, FxHashMap<String, FxHashMap<String, Vec<T>>>>>,
}

impl<T: Clone> MethodHolder<T> {
    pub fn new() -> Self {
        MethodHolder {
            map: FxHashMap::default(),
        }
    }

    pub fn get_list<'a, P: Into<MethodFullPath<'a>>>(&self, path: P) -> Option<&Vec<T>> {
        let MethodFullPath(asm_name, ty_namespace, ty_name, method_name) = path.into();
        self.map
            .get(asm_name)?
            .get(ty_namespace)?
            .get(ty_name)?
            .get(method_name)
    }

    pub fn add<'a, P: Into<MethodFullPath<'a>>>(&mut self, path: P, val: T) {
        let MethodFullPath(asm_name, ty_namespace, ty_name, method_name) = path.into();
        self.map
            .entry(asm_name.to_string())
            .or_insert(FxHashMap::default())
            .entry(ty_namespace.to_string())
            .or_insert(FxHashMap::default())
            .entry(ty_name.to_string())
            .or_insert(FxHashMap::default())
            .entry(method_name.to_string())
            .or_insert(vec![])
            .push(val)
    }

    pub fn add_list<'a, P: Into<MethodFullPath<'a>>>(&mut self, path: P, list: Vec<T>) {
        let MethodFullPath(asm_name, ty_namespace, ty_name, method_name) = path.into();
        *self
            .map
            .entry(asm_name.to_string())
            .or_insert(FxHashMap::default())
            .entry(ty_namespace.to_string())
            .or_insert(FxHashMap::default())
            .entry(ty_name.to_string())
            .or_insert(FxHashMap::default())
            .entry(method_name.to_string())
            .or_insert(vec![]) = list;
    }

    pub fn collect(&self) -> Vec<T> {
        let mut methods = vec![];
        // TODO: Use ``flatten``
        for (_, type_namespace_map) in &self.map {
            for (_, type_name_map) in type_namespace_map {
                for (_, method_map) in type_name_map {
                    for (_, method_list) in method_map {
                        for method in method_list {
                            methods.push(method.clone());
                        }
                    }
                }
            }
        }
        methods
    }
}
