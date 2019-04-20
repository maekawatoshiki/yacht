use crate::metadata::{
    file_reader::PEFileReader,
    header::{CLIHeader, SectionHeader},
    method::*,
};
use rustc_hash::FxHashMap;
use std::{cell::RefCell, rc::Rc};

#[derive(Debug, Clone)]
pub struct Image {
    pub cli_info: CLIInfo,
    pub metadata: MetaDataStreams,
    pub reader: Option<Rc<RefCell<PEFileReader>>>,
    // Cache
    pub method_cache: FxHashMap<u32, MethodBodyRef>,
    // pub memberref_cache: FxHashMap<usize, MethodBodyRef>,
    // pub memberref_cache: FxHashMap<usize, MethodBodyRef>,
}

#[derive(Debug, Clone)]
pub struct CLIInfo {
    pub cli_header: CLIHeader,
    pub sections: Vec<SectionHeader>,
}

/// #~ Stream
#[derive(Debug, Clone)]
pub struct MetaDataStream {
    /// Major version of table schemata; shall be 2
    pub major_version: u8,

    /// Minor version of table schemata; shall be 0
    pub minor_version: u8,

    /// Bit vector for heap sizes.
    pub heap_sizes: u8,

    /// Bit vector of present tables, let n be the number of bits that
    /// are 1.
    pub valid: u64,

    /// Bit vector of sorted tables.
    pub sorted: u64,

    /// Array of n 4-byte unsigned integers indicating the number of
    /// rows for each present table.
    pub rows: Vec<u32>,

    /// The sequence of physical tables.
    pub tables: Vec<Vec<Table>>,
}

#[derive(Debug, Clone)]
pub struct MetaDataStreams {
    pub metadata_stream: MetaDataStream,
    pub strings: FxHashMap<u32, String>,
    pub user_strings: FxHashMap<u32, Vec<u16>>,
    pub blob: FxHashMap<u32, Vec<u8>>,
    pub guid: String,
}

pub const NUM_TABLES: usize = 45;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum TableKind {
    Assembly,
    AssemblyOS,
    AssemblyProcessor,
    AssemblyRef,
    AssemblyRefOS,
    AssemblyRefProcessor,
    ClassLayout,
    Constant,
    CustomAttribute,
    DeclSecurity,
    EventMap,
    Event,
    ExportedType,
    Field,
    FieldLayout,
    FieldMarshal,
    FieldRVA,
    File,
    GenericParam,
    GenericParamConstraint,
    ImplMap,
    InterfaceImpl,
    ManifestResource,
    MemberRef,
    MethodDef,
    MethodImpl,
    MethodSemantics,
    MethodSpec,
    Module,
    ModuleRef,
    NestedClass,
    Param,
    Property,
    PropertyMap,
    StandAloneSig,
    TypeDef,
    TypeRef,
    TypeSpec,
}

#[derive(Debug, Clone)]
pub enum Table {
    Assembly(AssemblyTable),
    // AssemblyOS,
    // AssemblyProcessor,
    AssemblyRef(AssemblyRefTable),
    // AssemblyRefOS,
    // AssemblyRefProcessor,
    // ClassLayout,
    // Constant,
    CustomAttribute(CustomAttributeTable),
    // DeclSecurity,
    // EventMap,
    // Event,
    // ExportedType,
    // Field,
    // FieldLayout,
    // FieldMarshal,
    // FieldRVA,
    // File,
    // GenericParam,
    // GenericParamConstraint,
    // ImplMap,
    // InterfaceImpl,
    // ManifestResource,
    MemberRef(MemberRefTable),
    MethodDef(MethodDefTable),
    // MethodImpl,
    // MethodSemantics,
    // MethodSpec,
    Module(ModuleTable),
    // ModuleRef,
    // NestedClass,
    Param(ParamTable),
    // Property,
    // PropertyMap,
    // StandAloneSig,
    TypeDef(TypeDefTable),
    TypeRef(TypeRefTable),
    // TypeSpec,
}

/// II.22.2 Assembly
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct AssemblyTable {
    pub hash_alg_id: u32,
    pub major_version: u16,
    pub minor_version: u16,
    pub build_number: u16,
    pub revision_number: u16,
    pub flags: u32,
    pub public_key: u16,
    pub name: u16,
    pub culture: u16,
}

/// II.22.15 AssemblyRef
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct AssemblyRefTable {
    pub major_version: u16,
    pub minor_version: u16,
    pub build_number: u16,
    pub revision_number: u16,
    pub flags: u32,
    pub public_key_or_token: u16,
    pub name: u16,
    pub culture: u16,
    pub hash_value: u16,
}

/// II.22.10 CustomAttribute
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct CustomAttributeTable {
    pub parent: u16,
    pub type_: u16,
    pub value: u16,
}

/// II.22.25 MemberRef
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct MemberRefTable {
    pub class: u16,
    pub name: u16,
    pub signature: u16,
}

/// II.22.26 MethodDef
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct MethodDefTable {
    pub rva: u32,
    pub impl_flags: u16,
    pub flags: u16,
    pub name: u16,
    pub signature: u16,
    pub param_list: u16,
}

/// II.22.30 Module
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct ModuleTable {
    pub generation: u16,
    pub name: u16,
    pub mvid: u16,
    pub env_id: u16,
    pub env_base_id: u16,
}

/// II.22.33 Param
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct ParamTable {
    flags: u16,
    sequence: u16,
    name: u16,
}

/// II.22.37 TypeDef
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct TypeDefTable {
    pub flags: u32,
    pub type_name: u16,
    pub type_namespace: u16,
    pub extends: u16,
    pub field_list: u16,
    pub module_list: u16,
}

/// II.22.38 TypeRef
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct TypeRefTable {
    pub resolution_scope: u16,
    pub type_name: u16,
    pub type_namespace: u16,
}

impl MemberRefTable {
    pub fn class_table_and_entry(&self) -> (usize, usize) {
        let tag = self.class & 0b0000_0000_0000_0111; // MemberRefParent
        let table = match tag {
            0 => TableKind::TypeDef.into_num(),
            1 => TableKind::TypeRef.into_num(),
            2 => TableKind::ModuleRef.into_num(),
            3 => TableKind::MethodDef.into_num(),
            4 => TableKind::TypeSpec.into_num(),
            _ => unreachable!(),
        };
        let entry = self.class as usize >> 3;
        (table, entry)
    }
}

impl TypeRefTable {
    pub fn resolution_scope_table_and_entry(&self) -> (usize, usize) {
        let tag = self.resolution_scope & 0b0000_0000_0000_0011; // ResolutionScope
        let table = match tag {
            0 => TableKind::Module.into_num(),
            1 => TableKind::ModuleRef.into_num(),
            2 => TableKind::AssemblyRef.into_num(),
            3 => TableKind::TypeRef.into_num(),
            _ => unreachable!(),
        };
        let entry = self.resolution_scope as usize >> 2;
        (table, entry)
    }
}

impl TableKind {
    pub fn table_kinds(valid: u64) -> Vec<TableKind> {
        let mut tables = vec![];
        for i in 0..64 {
            if valid & (1 << i) > 0 {
                tables.push(TableKind::into_table_kind(i).unwrap())
            }
        }
        tables
    }

    pub fn into_table_kind<I: Into<u8>>(n: I) -> Option<TableKind> {
        match n.into() {
            0x20 => Some(TableKind::Assembly),
            0x22 => Some(TableKind::AssemblyOS),
            0x21 => Some(TableKind::AssemblyProcessor),
            0x23 => Some(TableKind::AssemblyRef),
            0x25 => Some(TableKind::AssemblyRefOS),
            0x24 => Some(TableKind::AssemblyRefProcessor),
            0x0F => Some(TableKind::ClassLayout),
            0x0B => Some(TableKind::Constant),
            0x0C => Some(TableKind::CustomAttribute),
            0x0E => Some(TableKind::DeclSecurity),
            0x12 => Some(TableKind::EventMap),
            0x14 => Some(TableKind::Event),
            0x27 => Some(TableKind::ExportedType),
            0x04 => Some(TableKind::Field),
            0x10 => Some(TableKind::FieldLayout),
            0x0D => Some(TableKind::FieldMarshal),
            0x1D => Some(TableKind::FieldRVA),
            0x26 => Some(TableKind::File),
            0x2A => Some(TableKind::GenericParam),
            0x2C => Some(TableKind::GenericParamConstraint),
            0x1C => Some(TableKind::ImplMap),
            0x09 => Some(TableKind::InterfaceImpl),
            0x28 => Some(TableKind::ManifestResource),
            0x0A => Some(TableKind::MemberRef),
            0x06 => Some(TableKind::MethodDef),
            0x19 => Some(TableKind::MethodImpl),
            0x18 => Some(TableKind::MethodSemantics),
            0x2B => Some(TableKind::MethodSpec),
            0x00 => Some(TableKind::Module),
            0x1A => Some(TableKind::ModuleRef),
            0x29 => Some(TableKind::NestedClass),
            0x08 => Some(TableKind::Param),
            0x17 => Some(TableKind::Property),
            0x15 => Some(TableKind::PropertyMap),
            0x11 => Some(TableKind::StandAloneSig),
            0x02 => Some(TableKind::TypeDef),
            0x01 => Some(TableKind::TypeRef),
            0x1B => Some(TableKind::TypeSpec),
            _ => None,
        }
    }

    pub fn into_num(self) -> usize {
        match self {
            TableKind::Assembly => 0x20,
            TableKind::AssemblyOS => 0x22,
            TableKind::AssemblyProcessor => 0x21,
            TableKind::AssemblyRef => 0x23,
            TableKind::AssemblyRefOS => 0x25,
            TableKind::AssemblyRefProcessor => 0x24,
            TableKind::ClassLayout => 0x0F,
            TableKind::Constant => 0x0B,
            TableKind::CustomAttribute => 0x0C,
            TableKind::DeclSecurity => 0x0E,
            TableKind::EventMap => 0x12,
            TableKind::Event => 0x14,
            TableKind::ExportedType => 0x27,
            TableKind::Field => 0x04,
            TableKind::FieldLayout => 0x10,
            TableKind::FieldMarshal => 0x0D,
            TableKind::FieldRVA => 0x1D,
            TableKind::File => 0x26,
            TableKind::GenericParam => 0x2A,
            TableKind::GenericParamConstraint => 0x2C,
            TableKind::ImplMap => 0x1C,
            TableKind::InterfaceImpl => 0x09,
            TableKind::ManifestResource => 0x28,
            TableKind::MemberRef => 0x0A,
            TableKind::MethodDef => 0x06,
            TableKind::MethodImpl => 0x19,
            TableKind::MethodSemantics => 0x18,
            TableKind::MethodSpec => 0x2B,
            TableKind::Module => 0x00,
            TableKind::ModuleRef => 0x1A,
            TableKind::NestedClass => 0x29,
            TableKind::Param => 0x08,
            TableKind::Property => 0x17,
            TableKind::PropertyMap => 0x15,
            TableKind::StandAloneSig => 0x11,
            TableKind::TypeDef => 0x02,
            TableKind::TypeRef => 0x01,
            TableKind::TypeSpec => 0x1B,
        }
    }
}

impl Image {
    pub fn get_string<T: Into<u32>>(&self, n: T) -> &String {
        self.metadata.strings.get(&n.into()).unwrap()
    }

    pub fn get_method(&mut self, rva: u32) -> MethodBodyRef {
        if let Some(method_ref) = self.method_cache.get(&rva) {
            return method_ref.clone();
        }
        let reader = self.reader.as_mut().unwrap().clone();
        let method_ref = reader.borrow_mut().read_method(self, rva).unwrap();
        self.method_cache.insert(rva, method_ref.clone());
        method_ref
    }
}

#[test]
fn test_table_kind() {
    TableKind::into_table_kind(0x20).unwrap();
    TableKind::into_table_kind(0x22).unwrap();
    TableKind::into_table_kind(0x21).unwrap();
    TableKind::into_table_kind(0x23).unwrap();
    TableKind::into_table_kind(0x25).unwrap();
    TableKind::into_table_kind(0x24).unwrap();
    TableKind::into_table_kind(0x0F).unwrap();
    TableKind::into_table_kind(0x0B).unwrap();
    TableKind::into_table_kind(0x0C).unwrap();
    TableKind::into_table_kind(0x0E).unwrap();
    TableKind::into_table_kind(0x12).unwrap();
    TableKind::into_table_kind(0x14).unwrap();
    TableKind::into_table_kind(0x27).unwrap();
    TableKind::into_table_kind(0x04).unwrap();
    TableKind::into_table_kind(0x10).unwrap();
    TableKind::into_table_kind(0x0D).unwrap();
    TableKind::into_table_kind(0x1D).unwrap();
    TableKind::into_table_kind(0x26).unwrap();
    TableKind::into_table_kind(0x2A).unwrap();
    TableKind::into_table_kind(0x2C).unwrap();
    TableKind::into_table_kind(0x1C).unwrap();
    TableKind::into_table_kind(0x09).unwrap();
    TableKind::into_table_kind(0x28).unwrap();
    TableKind::into_table_kind(0x0A).unwrap();
    TableKind::into_table_kind(0x06).unwrap();
    TableKind::into_table_kind(0x19).unwrap();
    TableKind::into_table_kind(0x18).unwrap();
    TableKind::into_table_kind(0x2B).unwrap();
    TableKind::into_table_kind(0x00).unwrap();
    TableKind::into_table_kind(0x1A).unwrap();
    TableKind::into_table_kind(0x29).unwrap();
    TableKind::into_table_kind(0x08).unwrap();
    TableKind::into_table_kind(0x17).unwrap();
    TableKind::into_table_kind(0x15).unwrap();
    TableKind::into_table_kind(0x11).unwrap();
    TableKind::into_table_kind(0x02).unwrap();
    TableKind::into_table_kind(0x01).unwrap();
    TableKind::into_table_kind(0x1B).unwrap();
    assert_eq!(TableKind::into_table_kind(0xFF), None);

    assert_eq!(TableKind::Assembly.into_num(), 0x20);
    assert_eq!(TableKind::AssemblyOS.into_num(), 0x22);
    assert_eq!(TableKind::AssemblyProcessor.into_num(), 0x21);
    assert_eq!(TableKind::AssemblyRef.into_num(), 0x23);
    assert_eq!(TableKind::AssemblyRefOS.into_num(), 0x25);
    assert_eq!(TableKind::AssemblyRefProcessor.into_num(), 0x24);
    assert_eq!(TableKind::ClassLayout.into_num(), 0x0F);
    assert_eq!(TableKind::Constant.into_num(), 0x0B);
    assert_eq!(TableKind::CustomAttribute.into_num(), 0x0C);
    assert_eq!(TableKind::DeclSecurity.into_num(), 0x0E);
    assert_eq!(TableKind::EventMap.into_num(), 0x12);
    assert_eq!(TableKind::Event.into_num(), 0x14);
    assert_eq!(TableKind::ExportedType.into_num(), 0x27);
    assert_eq!(TableKind::Field.into_num(), 0x04);
    assert_eq!(TableKind::FieldLayout.into_num(), 0x10);
    assert_eq!(TableKind::FieldMarshal.into_num(), 0x0D);
    assert_eq!(TableKind::FieldRVA.into_num(), 0x1D);
    assert_eq!(TableKind::File.into_num(), 0x26);
    assert_eq!(TableKind::GenericParam.into_num(), 0x2A);
    assert_eq!(TableKind::GenericParamConstraint.into_num(), 0x2C);
    assert_eq!(TableKind::ImplMap.into_num(), 0x1C);
    assert_eq!(TableKind::InterfaceImpl.into_num(), 0x09);
    assert_eq!(TableKind::ManifestResource.into_num(), 0x28);
    assert_eq!(TableKind::MemberRef.into_num(), 0x0A);
    assert_eq!(TableKind::MethodDef.into_num(), 0x06);
    assert_eq!(TableKind::MethodImpl.into_num(), 0x19);
    assert_eq!(TableKind::MethodSemantics.into_num(), 0x18);
    assert_eq!(TableKind::MethodSpec.into_num(), 0x2B);
    assert_eq!(TableKind::Module.into_num(), 0x00);
    assert_eq!(TableKind::ModuleRef.into_num(), 0x1A);
    assert_eq!(TableKind::NestedClass.into_num(), 0x29);
    assert_eq!(TableKind::Param.into_num(), 0x08);
    assert_eq!(TableKind::Property.into_num(), 0x17);
    assert_eq!(TableKind::PropertyMap.into_num(), 0x15);
    assert_eq!(TableKind::StandAloneSig.into_num(), 0x11);
    assert_eq!(TableKind::TypeDef.into_num(), 0x02);
    assert_eq!(TableKind::TypeRef.into_num(), 0x01);
    assert_eq!(TableKind::TypeSpec.into_num(), 0x1B);
}
