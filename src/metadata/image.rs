use crate::{
    metadata::{
        assembly::*, class::*, metadata::*, method::*, pe_parser::*, signature::*, token::*,
    },
    util::{holder::*, name_path::*},
};
use rustc_hash::FxHashMap;
use std::ops::Range;
use std::path;
use std::{cell::RefCell, rc::Rc};

pub type RVA = u32;

#[derive(Debug, Clone)]
pub struct Image {
    /// CLI Info
    pub cli_info: CLIInfo,

    /// Metadata streams
    pub metadata: MetaDataStreams,

    /// PE file reader
    pub pe_parser: Option<Rc<RefCell<PEParser>>>,

    /// Cache ``MethodInfoRef`` by RVA
    pub method_cache: FxHashMap<RVA, MethodInfoRef>,

    /// Cache ``ClassInfoRef`` by token
    pub class_cache: FxHashMap<Token, ClassInfoRef>,

    /// Assembly References
    pub asm_refs: FxHashMap<String, AssemblyRef>,

    /// File name from which this image is loaded
    pub filename: path::PathBuf,

    pub setup_info: SetupInfo, // pub memberref_cache: FxHashMap<usize, MethodBodyRef>
}

#[derive(Debug, Clone)]
pub struct SetupInfo {
    pub methods: Vec<(ClassInfoRef, Range<usize>)>,
    pub fields: Vec<(ClassInfoRef, Range<usize>)>,
    pub extends: Vec<(ClassInfoRef, u16)>,
}

impl Image {
    pub fn new(
        cli_info: CLIInfo,
        metadata: MetaDataStreams,
        filename: path::PathBuf,
        pe_parser: Option<Rc<RefCell<PEParser>>>,
    ) -> Self {
        Self {
            cli_info,
            metadata,
            pe_parser,
            method_cache: FxHashMap::default(),
            class_cache: FxHashMap::default(),
            asm_refs: FxHashMap::default(),
            filename,
            setup_info: SetupInfo::new(),
        }
    }

    pub fn collect_all_reachable_assemblies(&self, asms: &mut FxHashMap<String, AssemblyRef>) {
        for (name, asm) in self.asm_refs.clone() {
            if asms.contains_key(&name) {
                continue;
            }
            asms.insert(name, asm.clone());
            asm.borrow().image.collect_all_reachable_assemblies(asms);
        }
    }

    pub fn setup_all_asmref(&mut self, loaded: &mut FxHashMap<String, AssemblyRef>) {
        let asmrefs = &self.metadata.metadata_stream.tables[TableKind::AssemblyRef.into_num()];
        for asmref_ in asmrefs {
            let asmref = retrieve!(asmref_, Table::AssemblyRef);
            let name = self.get_string(asmref.name);

            // TODO: Treat as special
            if name == "mscorlib" {
                continue;
            }

            // Already loaded
            if let Some(asm) = loaded.get(name) {
                self.asm_refs.insert(name.clone(), asm.clone());
                continue;
            }

            let asm = Assembly::load_exclusive(
                {
                    let mut path = self.filename.parent().unwrap().to_path_buf();
                    path.push(name.clone());
                    path.set_extension("dll");
                    path
                },
                loaded,
            )
            .unwrap();

            // Register loaded one
            loaded.insert(name.clone(), asm.clone());

            self.asm_refs.insert(name.clone(), asm);
        }
    }

    pub fn setup_all_typeref(&mut self) {
        for (i, typeref) in self.metadata.metadata_stream.tables[TableKind::TypeRef.into_num()]
            .iter()
            .enumerate()
        {
            let token = encode_token(TableKind::TypeRef.into(), i as u32 + 1);
            let tref = retrieve!(typeref, Table::TypeRef);
            let namespace = self.get_string(tref.type_namespace).as_str();
            let name = self.get_string(tref.type_name).as_str();
            let asm = retrieve!(
                self.get_table_entry(tref.resolution_scope_table_and_entry()),
                Table::AssemblyRef
            );
            let asm_name = self.get_string(asm.name);

            // TODO: Treat as special
            if asm_name == "mscorlib" {
                if let Some(class) =
                    get_mscorlib().get(TypeFullPath(vec!["mscorlib", namespace, name]))
                {
                    self.class_cache.insert(token, class.clone());
                }
                continue;
            }

            let class = self
                .asm_refs
                .get(asm_name)
                .unwrap()
                .borrow()
                .image
                .find_class(TypeFullPath(vec![asm_name.as_str(), namespace, name]))
                .unwrap();
            self.class_cache.insert(token, class.clone());
        }
    }

    pub fn setup_all_class(&mut self) {
        // Set class fields
        for (class, range) in &self.setup_info.fields {
            let fields: Vec<ClassField> = self.metadata.metadata_stream.tables
                [TableKind::Field.into_num()][range.clone()]
            .iter()
            .map(|t| {
                let ft = retrieve!(t, Table::Field);
                let name = self.get_string(ft.name).clone();
                let mut sig = self.get_blob(ft.signature).iter();
                assert_eq!(sig.next().unwrap(), &0x06);
                let ty = Type::into_type(self, &mut sig).unwrap();
                ClassField { name, ty }
            })
            .collect();
            class.borrow_mut().fields = fields;
        }

        // Set parent class
        for (class, extends) in &self.setup_info.extends {
            let token = decode_typedef_or_ref_token(*extends as u32);
            let typedef_or_ref = self.get_table_entry(token);
            match typedef_or_ref {
                Table::TypeDef(_) => {
                    class.borrow_mut().parent =
                        Some(self.class_cache.get(&token.into()).unwrap().clone());
                }
                Table::TypeRef(_) => {} // TODO
                _ => unreachable!(),
            }
        }

        let pe_parser_ref = self.pe_parser.as_ref().unwrap().clone();
        let mut pe_parser = pe_parser_ref.borrow_mut();

        // Set class methods
        for (class, range) in &self.setup_info.methods {
            let mut methods = vec![];

            for mdef in &self.metadata.metadata_stream.tables[TableKind::MethodDef.into_num()]
                [range.clone()]
            {
                let mdef = retrieve!(mdef, Table::MethodDef);
                let method = pe_parser
                    .read_method(self, class.clone(), mdef.rva)
                    .unwrap();
                self.method_cache.insert(mdef.rva, method.clone());
                methods.push(method)
            }

            class.borrow_mut().methods = methods;
        }

        self.setup_all_class_method_table();
    }

    pub fn define_all_class(&mut self) {
        let typedefs = &self.metadata.metadata_stream.tables[TableKind::TypeDef.into_num()];
        let fields = &self.metadata.metadata_stream.tables[TableKind::Field.into_num()];
        let methoddefs = &self.metadata.metadata_stream.tables[TableKind::MethodDef.into_num()];

        for (i, typedef) in typedefs.iter().enumerate() {
            let typedef = retrieve!(typedef, Table::TypeDef);
            let next_typedef = typedefs.get(i + 1);
            let field_list_bgn = typedef.field_list as usize - 1;
            let method_list_bgn = typedef.method_list as usize - 1;
            let (field_list_end, method_list_end) =
                next_typedef.map_or((fields.len(), methoddefs.len()), |table| {
                    let td = retrieve!(table, Table::TypeDef);
                    (td.field_list as usize - 1, td.method_list as usize - 1)
                });
            let class_info = ClassInfo::new_ref(
                ResolutionScope::AssemblyRef {
                    name: self.get_assembly_name().unwrap().clone(),
                },
                self.get_string(typedef.type_namespace),
                self.get_string(typedef.type_name),
                vec![],
                vec![],
                None,
            );

            self.class_cache.insert(
                encode_token(TableKind::TypeDef.into(), i as u32 + 1),
                class_info.clone(),
            );

            self.setup_info
                .methods
                .push((class_info.clone(), method_list_bgn..method_list_end));
            self.setup_info
                .fields
                .push((class_info.clone(), field_list_bgn..field_list_end));

            if typedef.extends != 0 {
                self.setup_info.extends.push((class_info, typedef.extends))
            }
        }
    }

    fn get_assembly_name(&self) -> Option<&String> {
        let asm = retrieve!(
            self.metadata.metadata_stream.tables[TableKind::Assembly.into_num()].get(0)?,
            Table::Assembly
        );
        Some(self.get_string(asm.name))
    }

    fn setup_all_class_method_table(&mut self) {
        for (_token, class_ref) in &self.class_cache {
            self.construct_class_method_table(class_ref);
        }
    }

    fn construct_class_method_table(&self, class_ref: &ClassInfoRef) {
        let mut class = class_ref.borrow_mut();
        let mut method_table = match &class.parent {
            Some(parent) => {
                self.construct_class_method_table(parent);
                parent.borrow().method_table.clone()
            }
            // If already borrowed, it means that ``class`` is System::Object.
            None => mscorlib_system_object()
                .try_borrow()
                .map(|sys_obj| sys_obj.methods.clone())
                .unwrap_or_else(|_| class.methods.clone()),
        };

        for minforef in &class.methods {
            let minfo = minforef.borrow();
            if let Some(m) = method_table
                .iter_mut()
                .find(|m| m.borrow().get_name() == minfo.get_name())
            {
                // Override
                *m = minforef.clone()
            } else {
                // New slot
                method_table.push(minforef.clone());
            }
        }

        class.method_table = method_table;
    }

    pub fn get_table_entry<T: Into<Token>>(&self, token: T) -> Table {
        let DecodedToken(table, entry) = decode_token(token.into());
        self.metadata.metadata_stream.tables[table as usize][entry as usize - 1]
    }

    pub fn get_class<T: Into<Token>>(&self, token: T) -> Option<&ClassInfoRef> {
        self.class_cache.get(&token.into())
    }

    pub fn get_string<T: Into<u32>>(&self, n: T) -> &String {
        self.metadata.strings.get(&n.into()).unwrap()
    }

    pub fn get_user_string<T: Into<u32>>(&self, n: T) -> &Vec<u16> {
        self.metadata.user_strings.get(&n.into()).unwrap()
    }

    pub fn get_entry_method(&mut self) -> MethodInfoRef {
        let method_or_file = self.get_table_entry(self.cli_info.cli_header.entry_point_token);
        let mdef = match method_or_file {
            Table::MethodDef(t) => t,
            // TOOD: File
            _ => unimplemented!(),
        };
        self.get_method_by_rva(mdef.rva)
    }

    pub fn get_method_by_rva(&self, rva: u32) -> MethodInfoRef {
        self.method_cache.get(&rva).unwrap().clone()
    }

    pub fn get_method_def_table_by_rva(&self, rva: u32) -> Option<&MethodDefTable> {
        for method_def in &self.metadata.metadata_stream.tables[TableKind::MethodDef.into_num()] {
            match method_def {
                Table::MethodDef(mdt) if mdt.rva == rva => return Some(mdt),
                Table::MethodDef(_) => continue,
                _ => return None,
            }
        }
        None
    }

    pub fn get_blob<T: Into<u32>>(&self, n: T) -> &Vec<u8> {
        self.metadata.blob.get(&n.into()).unwrap()
    }

    pub fn get_path_from_type_ref_table<'a>(
        &'a self,
        type_ref_table: &TypeRefTable,
    ) -> TypeFullPath<'a> {
        let token = type_ref_table.resolution_scope_table_and_entry();
        let assembly_ref_table = retrieve!(self.get_table_entry(token), Table::AssemblyRef);
        let asm_ref_name = self.get_string(assembly_ref_table.name).as_str();
        let ty_namespace = self.get_string(type_ref_table.type_namespace).as_str();
        let ty_name = self.get_string(type_ref_table.type_name).as_str();
        TypeFullPath(vec![asm_ref_name, ty_namespace, ty_name])
    }

    pub fn get_method_ref_type_from_signature(&self, signature: u16) -> Type {
        let sig = self.get_blob(signature);
        SignatureParser::new(sig)
            .parse_method_ref_sig(self)
            .unwrap()
    }

    pub fn find_class<'a, P: Into<TypeFullPath<'a>>>(&self, path_: P) -> Option<ClassInfoRef> {
        let path = path_.into();
        for (_, info) in &self.class_cache {
            if (&*info.borrow()).into(): TypeFullPath == path {
                return Some(info.clone());
            }
        }
        None
    }
}

impl SetupInfo {
    pub fn new() -> Self {
        Self {
            methods: vec![],
            fields: vec![],
            extends: vec![],
        }
    }
}

thread_local! {
    pub static MSCORLIB: Rc<Holder<ClassInfoRef>> = {
        #[rustfmt::skip]
        macro_rules! parse_ty {
            (void) => { Type::void_ty() };
            (i4  ) => { Type::i4_ty() };
            (r8  ) => { Type::r8_ty() };
            (char) => { Type::char_ty() };
            (obj ) => { Type::object_ty() };
            (str ) => { Type::string_ty() };
        }

        macro_rules! method {
            ($ret_ty:ident, [ $($param_ty:ident),* ], $f:expr, $name:expr) => {{
                method!([0], $ret_ty, [$($param_ty),*], $f, $name)
            }};
            ([$flags:expr], $ret_ty:ident, [ $($param_ty:ident),* ], $name:expr, $class:expr) => {{
                Rc::new(RefCell::new(MethodInfo::MRef(MemberRefInfo {
                    name: $name.to_string(), class: $class.clone(),
                    ty: Type::full_method_ty($flags, parse_ty!($ret_ty), &[$(parse_ty!($param_ty)),*]),
                })))
            }}
        }

        macro_rules! class { ($name:ident, $parent:expr) => {{
            ClassInfo::new_ref(
                ResolutionScope::asm_ref("mscorlib"),
                "System", stringify!($name), vec![], vec![], $parent,
            )}}}

        let class_system_obj_ref = class!(Object, None);
        let class_system_int32_ref = class!(Int32, Some(class_system_obj_ref.clone()));
        let class_system_string_ref = class!(String, Some(class_system_obj_ref.clone()));

        {
            let mut class_system_obj = class_system_obj_ref.borrow_mut();
            let mut class_system_int32 = class_system_int32_ref.borrow_mut();
            let mut class_system_string = class_system_string_ref.borrow_mut();

            class_system_obj.methods =
                vec![method!([0x20], str, [], "ToString", class_system_obj_ref)];
            class_system_int32.methods =
                vec![method!([0x20], str, [], "ToString", class_system_int32_ref)];
            class_system_string.methods = vec![
                method!([0x20], str, [], "ToString", class_system_string_ref),
                method!([0x20], char, [i4], "get_Chars", class_system_string_ref),
                method!([0x20], i4, [], "get_Length", class_system_string_ref),
            ];

            class_system_obj.method_table = class_system_obj.methods.clone();
            class_system_int32.method_table = class_system_int32.methods.clone();
            class_system_string.method_table = class_system_string.methods.clone();
        }

        let mut holder = Holder::new();

        holder.add(
            TypeFullPath(vec!["mscorlib", "System", "Object"]),
            class_system_obj_ref,
        );
        holder.add(
            TypeFullPath(vec!["mscorlib", "System", "Int32"]),
            class_system_int32_ref,
        );
        holder.add(
            TypeFullPath(vec!["mscorlib", "System", "String"]),
            class_system_string_ref,
        );

        Rc::new(holder)
    };
}

pub fn mscorlib_system_string() -> ClassInfoRef {
    get_mscorlib()
        .get(TypeFullPath(vec!["mscorlib", "System", "String"]))
        .unwrap()
        .clone()
}

pub fn mscorlib_system_object() -> ClassInfoRef {
    get_mscorlib()
        .get(TypeFullPath(vec!["mscorlib", "System", "String"]))
        .unwrap()
        .clone()
}

pub fn get_mscorlib() -> Rc<Holder<ClassInfoRef>> {
    MSCORLIB.with(|mscorlib| mscorlib.clone())
}
