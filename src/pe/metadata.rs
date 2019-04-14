// use crate::pe::header::SectionHeader;
use rustc_hash::FxHashMap;

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
    pub user_strings: Vec<u16>,
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
