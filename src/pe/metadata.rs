// use crate::pe::header::SectionHeader;

/// #~ Stream
#[derive(Debug, Clone)]
pub struct HashTildaStream {
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
    pub tables: Vec<Table>,
}

#[derive(Debug, Clone)]
pub struct MetaDataStreams {
    pub metadata_stream: HashTildaStream,
    pub strings: Vec<char>,
    pub user_strings: Vec<u16>,
    pub blob: Vec<u8>,
    pub guid: String,
}

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
    ModuleRef,
    // NestedClass,
    // Param,
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
    hash_alg_id: u32,
    major_version: u16,
    minor_version: u16,
    build_number: u16,
    revision_number: u16,
    flags: u32,
    public_key: u16,
    name: u16,
    culture: u16,
}

/// II.22.15 AssemblyRef
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct AssemblyRefTable {
    major_version: u16,
    minor_version: u16,
    build_number: u16,
    revision_number: u16,
    flags: u32,
    public_key_or_token: u16,
    name: u16,
    culture: u16,
    hash_value: u16,
}

/// II.22.10 CustomAttribute
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct CustomAttributeTable {
    parent: u16,
    type_: u16,
    value: u16,
}

/// II.22.25 MemberRef
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct MemberRefTable {
    class: u16,
    name: u16,
    signature: u16,
}

/// II.22.26 MethodDef
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct MethodDefTable {
    rva: u32,
    impl_flags: u16,
    flags: u16,
    name: u16,
    signature: u16,
    param_list: u16,
}

/// II.22.30 Module
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct ModuleTable {
    generation: u16,
    name: u16,
    mvid: u16,
    env_id: u16,
    env_base_id: u16,
}

/// II.22.37 TypeDef
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct TypeDefTable {
    flags: u32,
    type_name: u16,
    type_namespace: u16,
    extends: u16,
    field_list: u16,
    module_list: u16,
}

/// II.22.38 TypeRef
#[derive(Debug, Clone, PartialEq, Copy)]
#[repr(C, packed)]
pub struct TypeRefTable {
    resolution_scope: u16,
    type_name: u16,
    type_namespace: u16,
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
}

// impl Table {
//     fn dump(&self, section: &SectionHeader) {
//         match self {
//             Table::MethodDef(mdt) => mdt.dump(section),
//         }
//     }
// }
//
// impl MethodDefTable {
//     fn dump(&self, section: &SectionHeader) {
//         let virtual_address = section.virtual_address;
//         self.rva
//     }
// }
