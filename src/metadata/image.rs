use crate::metadata::{class::*, file_reader::*, metadata::*, method::*, signature::*};
use rustc_hash::FxHashMap;
use std::{cell::RefCell, rc::Rc};

pub type Token = u32;
pub type RVA = u32;

#[derive(Debug, Clone)]
pub struct Image {
    /// CLI Info
    pub cli_info: CLIInfo,

    /// Metadata streams
    pub metadata: MetaDataStreams,

    /// PE file reader
    pub reader: Option<Rc<RefCell<PEFileReader>>>,

    /// Cache ``MethodInfoRef`` by RVA
    pub method_cache: FxHashMap<RVA, MethodInfoRef>,

    /// Cache ``ClassInfoRef`` by token
    pub class_cache: FxHashMap<Token, ClassInfoRef>,
    // pub memberref_cache: FxHashMap<usize, MethodBodyRef>,
}

impl Image {
    pub fn setup_all_class(&mut self) {
        let typedefs = &self.metadata.metadata_stream.tables[TableKind::TypeDef.into_num()];
        let fields = &self.metadata.metadata_stream.tables[TableKind::Field.into_num()];
        let methoddefs = &self.metadata.metadata_stream.tables[TableKind::MethodDef.into_num()];
        let mut methods_to_setup = vec![];
        let mut fields_to_setup = vec![];
        let mut extends = vec![];

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
            let class_info = Rc::new(RefCell::new(ClassInfo {
                parent: None,
                fields: vec![],
                methods: vec![],
                name: self.get_string(typedef.type_name).clone(),
                namespace: self.get_string(typedef.type_namespace).clone(),
                virtual_methods: vec![],
                vtable_ptr: ::std::ptr::null_mut(),
            }));

            self.class_cache.insert(
                encode_typedef_or_ref_token(TableKind::TypeDef, i as u32 + 1),
                class_info.clone(),
            );
            methods_to_setup.push((class_info.clone(), method_list_bgn..method_list_end));
            fields_to_setup.push((class_info.clone(), field_list_bgn..field_list_end));

            if typedef.extends != 0 {
                extends.push((class_info, typedef.extends))
            }
        }

        for (class, range) in fields_to_setup {
            let fields: Vec<ClassField> = fields[range]
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

        let file_reader_ref = self.reader.as_ref().unwrap().clone();
        let mut file_reader = file_reader_ref.borrow_mut();

        for (class, range) in methods_to_setup {
            let mut methods = vec![];

            for mdef in &methoddefs[range] {
                let mdef = retrieve!(mdef, Table::MethodDef);
                let method = file_reader
                    .read_method(self, class.clone(), mdef.rva)
                    .unwrap();
                self.method_cache.insert(mdef.rva, method.clone());
                methods.push(method)
            }

            class.borrow_mut().methods = methods;
        }

        for (class, extends) in extends {
            let (table, entry) = decode_typedef_or_ref_token(extends as u32);
            let typedef_or_ref = self.get_table(table, entry - 1);

            match typedef_or_ref {
                Table::TypeDef(t) => {
                    class.borrow_mut().parent =
                        Some(self.class_cache.get(&(extends as u32)).unwrap().clone());
                    dprintln!(
                        "DEF: {} {}",
                        self.get_string(t.type_name),
                        self.get_string(t.type_namespace)
                    );
                }
                Table::TypeRef(t) => {
                    dprintln!(
                        "REF: {} {}",
                        self.get_string(t.type_name),
                        self.get_string(t.type_namespace)
                    );
                }
                _ => unreachable!(),
            }
        }

        self.setup_all_class_vtable();
    }

    fn setup_all_class_vtable(&mut self) {
        for (_token, class_ref) in &self.class_cache {
            self.construct_class_vtable(class_ref);
        }
    }

    fn construct_class_vtable(&self, class_ref: &ClassInfoRef) {
        let mut class = class_ref.borrow_mut();
        let mut virtual_methods = match &class.parent {
            Some(parent) => {
                self.construct_class_vtable(parent);
                parent.borrow().virtual_methods.clone()
            }
            None => vec![],
        };

        for mref in &class.methods {
            let method = mref.borrow();

            if !method.is_virtual() {
                continue;
            }

            if method.is_new_slot() {
                virtual_methods.push(mref.clone());
                continue;
            }

            for vmethod in &mut virtual_methods {
                if vmethod.borrow().name == method.name {
                    *vmethod = mref.clone();
                }
            }
        }

        dprintln!("VTABLE: {:?}", virtual_methods);

        class.vtable_ptr =
            Box::into_raw(vec![0u64; virtual_methods.len()].into_boxed_slice()) as VTablePtr;
        class.virtual_methods = virtual_methods;
    }

    pub fn get_table(&self, table: u32, entry: u32) -> &Table {
        &self.metadata.metadata_stream.tables[table as usize][entry as usize]
    }

    pub fn get_string<T: Into<u32>>(&self, n: T) -> &String {
        self.metadata.strings.get(&n.into()).unwrap()
    }

    pub fn get_user_string<T: Into<u32>>(&self, n: T) -> &Vec<u16> {
        self.metadata.user_strings.get(&n.into()).unwrap()
    }

    pub fn get_entry_method(&mut self) -> MethodInfoRef {
        let (table, entry) = decode_token(self.cli_info.cli_header.entry_point_token);
        let method_or_file = self.get_table(table, entry - 1);
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

    pub fn get_info_from_type_ref_table(
        &self,
        type_ref_table: &TypeRefTable,
    ) -> (&String, &String, &String) {
        let (table, entry) = type_ref_table.resolution_scope_table_and_entry();
        let assembly_ref_table = retrieve!(self.get_table(table, entry - 1), Table::AssemblyRef);
        let asm_ref_name = self.get_string(assembly_ref_table.name);
        let ty_namespace = self.get_string(type_ref_table.type_namespace);
        let ty_name = self.get_string(type_ref_table.type_name);
        (asm_ref_name, ty_namespace, ty_name)
    }

    pub fn get_method_ref_type_from_signature(&self, signature: u16) -> Type {
        let sig = self.get_blob(signature);
        SignatureParser::new(sig)
            .parse_method_ref_sig(self)
            .unwrap()
    }
}
