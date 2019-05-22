#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct PEFileHeader {
    /// Always 0x14c
    pub machine: u16,

    /// Number of sections; indicates size of the Section Table,
    /// which immediately follows the headers.
    pub number_of_sections: u16,

    /// Time and date the file was created in seconds since
    /// January 1st 1970 00:00:00 or 0.
    pub time_date_stamp: u32,

    /// Always 0
    pub pointer_to_symbol_table: u32,

    /// Always 0
    pub number_of_symbols: u32,

    /// Size of the optional header, the format is described below.
    pub optional_header_size: u16,

    /// Flags indicating attributes of the file.
    pub characteristics: u16,
}

#[derive(Debug, Clone)]
pub struct PEOptionalHeader {
    /// Size of the code (text) section, or the sum of all code sections
    /// if there are multiple sections.
    pub code_size: u32,

    /// Size of the initialized data section, or the sum of all such
    /// sections if there are multiple data sections.
    pub initialized_data_size: u32,

    /// Size of the uninitialized data section, or the sum of all such
    /// sections if there are multiple unitinitalized data sections.
    pub uninitialized_data_size: u32,

    /// RVA of entry point , needs to point to bytes 0xFF 0x25
    /// followed by the RVA in a section marked execute/read for
    /// EXEs or 0 for DLLs
    pub entry_point_rva: u32,

    /// RVA of the code section.
    pub base_of_code: u32,

    /// RVA of the data section.
    pub base_of_data: u32,

    /// Shall be a multiple of 0x10000.
    pub image_base: u32,

    /// Shall be greater than File Alignment.
    pub section_alignment: u32,

    /// Size, in bytes, of image, including all headers and padding;
    /// shall be a multiple of Section Alignment.
    pub image_size: u32,

    /// Combined size of MS-DOS Header, PE Header, PE Optional
    /// Header and padding; shall be a multiple of the file alignment.
    pub header_size: u32,

    /// Subsystem required to run this image.
    pub sub_system: u16,

    /// Bits 0x100f shall be zero.
    pub dll_flags: u16,

    /// Should be 0x100000 (1Mb)
    pub stack_reserve_size: u32,

    /// Should be 0x1000 (4Kb)
    pub stack_commit_size: u32,

    /// Should be 0x100000 (1Mb)
    pub heap_reserve_size: u32,

    /// Should be 0x1000 (4Kb)
    pub heap_commit_size: u32,

    /// Shall be 0
    pub loader_flags: u32,

    /// Shall be 0x10
    pub number_of_data_directories: u32,

    // Data directories
    /// Import Table: RVA
    pub import_table_rva: u32,

    /// Import Table: Size of Import Table
    pub import_table_size: u32,

    /// Base Relocation Table: RVA
    pub base_relocation_table_rva: u32,

    /// Base Relocation Table: Block size
    pub base_relocation_table_size: u32,

    /// IAT: RVA
    pub iat_rva: u32,

    /// IAT: Size
    pub iat_size: u32,

    /// CLI Header: RVA
    pub cli_header_rva: u32,

    /// CLI Header: Size
    pub cli_header_size: u32,
}

#[derive(Debug, Clone)]
pub struct SectionHeader {
    /// An 8-byte, null-padded ASCII string. There is no terminating null
    /// if the string is exactly eight characters long.
    pub name: String,

    /// Total size of the section in bytes. If this value is greater than
    /// SizeOfRawData, the section is zero-padded.
    pub virtual_size: u32,

    /// For executable images this is the address of the first byte of the
    /// section, when loaded into memory, relative to the image base.
    pub virtual_address: u32,

    /// Size of the initialized data on disk in bytes, shall be a multiple of
    /// FileAlignment from the PE header. If this is less than VirtualSize
    /// the remainder of the section is zero filled. Because this field is
    /// rounded while the VirtualSize field is not it is possible for this to
    /// be greater than VirtualSize as well. When a section contains only
    /// uninitialized data, this field should be 0.
    pub size_of_raw_data: u32,

    /// Offset of section’s first page within the PE file. This shall be a
    /// multiple of FileAlignment from the optional header. When a
    /// section contains only uninitialized data, this field should be 0.
    pub pointer_to_raw_data: u32,

    /// Should be 0
    pub pointer_to_relocations: u32,

    /// Should be 0
    pub pointer_to_linenumbers: u32,

    /// Should be 0
    pub number_of_relocations: u16,

    /// Should be 0
    pub number_of_linenumbers: u16,

    /// Flags describing section’s characteristics
    pub characteristics: u32,
}

#[derive(Debug, Clone)]
pub struct CLIHeader {
    /// Size of the header in bytes
    pub cb: u32,

    /// The minimum version of the runtime required to run
    /// this program, currently 2.
    pub major_runtime_version: u16,

    /// The minor portion of the version, currently 0.
    pub minor_runtime_version: u16,

    /// Metadata: RVA
    pub metadata_rva: u32,

    /// Metadata: Size
    pub metadata_size: u32,

    /// Flags describing this runtime image.
    pub flags: u32,

    /// Token for the MethodDef or File of the entry point
    /// for the image
    pub entry_point_token: u32,

    /// Resources: RVA
    pub resources_rva: u32,

    /// Resources: Size
    pub resources_size: u32,

    /// RVA of the hash data for this PE file used by the
    /// CLI loader for binding
    pub strong_name_signature_rva: u32,

    /// Versioning of the hash data for this PE file used by the
    /// CLI loader for binding
    pub strong_name_signature_version: u32,

    pub vtable_fixups_virtual_address: u32,

    pub vtable_fixups_size: u16,

    pub vtable_fixups_type: u16,
}

#[derive(Debug, Clone)]
pub struct MetaDataHeader {
    /// Version string
    pub version: String,

    /// Number of streams
    pub streams: u16,
}

#[derive(Debug, Clone)]
pub struct StreamHeader {
    /// Memory offset to start of this stream from start of the
    /// metadata root
    pub offset: u32,

    /// Size of this stream in bytes, shall be a multiple of 4.
    pub size: u32,

    /// Name of the stream
    pub name: String,
}

#[allow(dead_code)]
mod pe_file_header_characteristics {
    pub const IMAGE_FILE_RELOCS_STRIPPED: u16 = 0x0001;
    pub const IMAGE_FILE_EXECUTABLE_IMAGE: u16 = 0x0002;
    pub const IMAGE_FILE_32BIT_MACHINE: u16 = 0x0100;
    pub const IMAGE_FILE_DLL: u16 = 0x2000;
}

#[allow(dead_code)]
mod sub_system {
    pub const IMAGE_SUBSYSTEM_WINDOWS_CUI: u16 = 0x3;
    pub const IMAGE_SUBSYSTEM_WINDOWS_GUI: u16 = 0x2;
}

#[allow(dead_code)]
mod section_characteristics {
    /// Section contains code.
    pub const IMAGE_SCN_CNT_CODE: u32 = 0x00000020;

    /// Section contains initialized data.
    pub const IMAGE_SCN_CNT_INITIALIZED_DATA: u32 = 0x00000040;

    /// Section contains uninitialized data.
    pub const IMAGE_SCN_CNT_UNINITIALIZED_DATA: u32 = 0x00000080;

    /// Section can be executed as code.
    pub const IMAGE_SCN_MEM_EXECUTE: u32 = 0x20000000;

    /// Section can be read.
    pub const IMAGE_SCN_MEM_READ: u32 = 0x40000000;

    /// Section can be written to.
    pub const IMAGE_SCN_MEM_WRITE: u32 = 0x80000000;
}
