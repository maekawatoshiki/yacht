use rustc_hash::FxHashMap;
use std::fmt::Debug;

#[derive(Debug, Clone)]
pub enum TreeMap<T: Debug + Clone> {
    Map(FxHashMap<String, TreeMap<T>>),
    Value(T),
}

impl<T: Debug + Clone> TreeMap<T> {
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

    pub fn as_value_mut(&mut self) -> Option<&mut T> {
        match self {
            TreeMap::Map(_) => None,
            TreeMap::Value(val) => Some(val),
        }
    }

    pub fn contains_key<'a, P: Into<Vec<&'a str>>>(&self, path: P) -> bool {
        let mut cur = self;
        for name in path.into() {
            match cur {
                TreeMap::Map(map) => {
                    if let Some(next) = map.get(name) {
                        cur = next
                    } else {
                        return false;
                    }
                }
                TreeMap::Value(_) => return false,
            }
        }
        matches!(cur, TreeMap::Value(_))
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

    pub fn get_mut<'a, P: Into<Vec<&'a str>>>(&mut self, path: P) -> Option<&mut T> {
        let mut cur = self;
        for name in path.into() {
            match cur {
                TreeMap::Map(map) => cur = map.get_mut(name)?,
                TreeMap::Value(_) => return None,
            }
        }
        cur.as_value_mut()
    }
    //
    // pub fn get_mut_or<'a, P: Into<Vec<&'a str>>>(&mut self, path: P, default: T) -> Option<&mut T> {
    //     if !self.contains_key(path) {
    //         self.add(path, default);
    //     }
    //     self.get_mut(path)
    // }

    pub fn add<'a, P: Into<Vec<&'a str>>>(&mut self, path_: P, val: T) -> Option<()> {
        let path = path_.into();
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

    pub fn collect_values(&self) -> Vec<T> {
        let mut values = vec![];
        match self {
            TreeMap::Map(map) => {
                for (_, elem) in map {
                    values.append(&mut elem.collect_values())
                }
            }
            TreeMap::Value(val) => values.push(val.clone()),
        }
        values
    }
}

#[derive(Debug, Clone)]
pub struct NameResolver<T: Debug + Clone> {
    map: TreeMap<T>,
}

impl<T: Debug + Clone> NameResolver<T> {
    pub fn new() -> Self {
        NameResolver {
            map: TreeMap::Map(FxHashMap::default()),
        }
    }

    pub fn get<'a, P: Into<Vec<&'a str>>>(&self, path: P) -> Option<&T> {
        self.map.get(path.into())
    }

    pub fn get_mut<'a, P: Into<Vec<&'a str>>>(&mut self, path: P) -> Option<&mut T> {
        self.map.get_mut(path.into())
    }

    pub fn add<'a, P: Into<Vec<&'a str>>>(&mut self, path: P, val: T) {
        self.map.add(path.into(), val).unwrap()
    }

    pub fn collect_values(&self) -> Vec<T> {
        self.map.collect_values()
    }
}
