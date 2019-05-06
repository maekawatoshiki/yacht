use crate::metadata::{class::*, file_reader::*, metadata::*, method::*, signature::*};
use rustc_hash::FxHashMap;
use std::{cell::RefCell, rc::Rc};

#[derive(Debug, Clone)]
pub struct Image {
    pub cli_info: CLIInfo,
    pub metadata: MetaDataStreams,
    pub reader: Option<Rc<RefCell<PEFileReader>>>,
    // Cache
    pub method_cache: FxHashMap<u32, MethodInfoRef>,
    pub class_cache: FxHashMap<u32, ClassInfoRef>,
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
                vtable: vec![],
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
            for methoddef in &methoddefs[range] {
                let methoddef = retrieve!(methoddef, Table::MethodDef);
                let method = file_reader
                    .read_method(self, class.clone(), methoddef.rva)
                    .unwrap();
                self.method_cache.insert(methoddef.rva, method.clone());
                methods.push(method)
            }
            class.borrow_mut().methods = methods;
        }

        for (class, extends) in extends {
            let (table, entry) = decode_typedef_or_ref_token(extends as u32);
            let typedef_or_ref = &self.metadata.metadata_stream.tables[table][entry - 1];
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
        let mut vtable = if let Some(parent) = &class.parent {
            self.construct_class_vtable(parent);
            parent.borrow().vtable.clone()
        } else {
            vec![]
        };
        for method_ref in &class.methods {
            let method = method_ref.borrow();
            if method.is_virtual() {
                if method.is_new_slot() {
                    vtable.push(method_ref.clone());
                } else {
                    for virtual_method_ref in &mut vtable {
                        if virtual_method_ref.borrow().name == method.name {
                            *virtual_method_ref = method_ref.clone();
                        }
                    }
                }
            }
        }

        dprintln!("VTABLE: {:?}", vtable);

        class.vtable_ptr = Box::into_raw(vec![0u64; vtable.len()].into_boxed_slice()) as VTablePtr;
        class.vtable = vtable;
    }

    pub fn get_string<T: Into<u32>>(&self, n: T) -> &String {
        self.metadata.strings.get(&n.into()).unwrap()
    }

    pub fn get_user_string<T: Into<u32>>(&self, n: T) -> &Vec<u16> {
        self.metadata.user_strings.get(&n.into()).unwrap()
    }

    pub fn get_entry_method(&mut self) -> MethodInfoRef {
        let kind = self.cli_info.cli_header.entry_point_token as usize >> (32 - 8);
        let row = self.cli_info.cli_header.entry_point_token as usize & 0x00ffffff;
        let method_or_file = self.metadata.metadata_stream.tables[kind][row - 1].clone();
        let method_table = match method_or_file {
            Table::MethodDef(t) => t,
            // TOOD: File
            _ => unimplemented!(),
        };
        self.get_method(method_table.rva)
    }

    pub fn get_method(&self, rva: u32) -> MethodInfoRef {
        self.method_cache.get(&rva).unwrap().clone()
    }

    pub fn get_method_def_table_by_rva(&self, rva: u32) -> Option<&MethodDefTable> {
        for method_def in &self.metadata.metadata_stream.tables[TableKind::MethodDef.into_num()] {
            match method_def {
                Table::MethodDef(mdt) => {
                    if mdt.rva == rva {
                        return Some(mdt);
                    }
                }
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
        let assembly_ref_table = retrieve!(
            self.metadata.metadata_stream.tables[table][entry - 1],
            Table::AssemblyRef
        );
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
