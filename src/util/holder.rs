use super::name_path::*;
use rustc_hash::FxHashMap;
use std::fmt::Debug;

#[derive(Debug, Clone)]
pub enum TreeMap<T: Debug> {
    Map(FxHashMap<String, TreeMap<T>>),
    Value(T),
}

impl<T: Debug> TreeMap<T> {
    pub fn as_map(&self) -> Option<&FxHashMap<String, TreeMap<T>>> {
        match self {
            TreeMap::Map(map) => Some(map),
            TreeMap::Value(_) => None,
        }
    }

    pub fn as_value(&self) -> Option<&T> {
        match self {
            TreeMap::Map(_) => None,
            TreeMap::Value(val) => Some(val),
        }
    }

    pub fn get<'a, P: Into<Vec<&'a str>>>(&self, path: P) -> Option<&T> {
        let mut cur = self;
        for name in path.into() {
            match cur {
                TreeMap::Map(map) => cur = map.get(name)?,
                TreeMap::Value(_) => return None,
            }
        }
        cur.as_value()
    }

    pub fn add<'a, P: Into<Vec<&'a str>>>(&mut self, path: P, val: T) -> Option<()> {
        let path = path.into();
        let len = path.len();
        let mut cur = self;
        for (i, name) in path.iter().enumerate() {
            let last = i == len - 1;
            match cur {
                TreeMap::Map(ref mut map) if last => {
                    map.insert(name.to_string(), TreeMap::Value(val));
                    break;
                }
                TreeMap::Map(ref mut map) => {
                    cur = map
                        .entry(name.to_string())
                        .or_insert_with(|| TreeMap::Map(FxHashMap::default()))
                }
                TreeMap::Value(_) => return None,
            }
        }
        Some(())
    }
}

#[derive(Debug, Clone)]
pub struct Holder<T: Debug> {
    map: TreeMap<T>,
}

impl<T: Debug> Holder<T> {
    pub fn new() -> Self {
        Holder {
            map: TreeMap::Map(FxHashMap::default()),
        }
    }

    pub fn get<'a, P: Into<Vec<&'a str>>>(&self, path: P) -> Option<&T> {
        self.map.get(path.into())
    }

    pub fn add<'a, P: Into<Vec<&'a str>>>(&mut self, path: P, val: T) {
        self.map.add(path.into(), val).unwrap()
    }
}

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
