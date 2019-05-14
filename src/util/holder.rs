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
