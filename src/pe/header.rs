#[derive(Debug, Clone)]
pub struct PEFileHeader {
    // machine: Always 0x14c
    /// Number of sections; indicates size of the Section Table,
    /// which immediately follows the headers.
    pub number_of_sections: u16,

    /// Time and date the file was created in seconds since
    /// January 1st 1970 00:00:00 or 0.
    pub time_date_stamp: u32,

    // pointer_to_symbol_table: Always 0
    // number_of_symbols: Always 0
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

    /// Import Table: RVA
    pub import_table_rva: u32,

    /// Import Table: Size of Import Table
    pub import_table_size: u32,

    /// Base Relocation Table: RVA
    pub base_relocation_table_rva: u32,

    /// Base Relocation Table: Block size
    pub base_relocation_table_size: u32,
}

mod characteristics {
    pub const IMAGE_FILE_RELOCS_STRIPPED: u16 = 0x0001;
    pub const IMAGE_FILE_EXECUTABLE_IMAGE: u16 = 0x0002;
    pub const IMAGE_FILE_32BIT_MACHINE: u16 = 0x0100;
    pub const IMAGE_FILE_DLL: u16 = 0x2000;
}

mod sub_system {
    pub const IMAGE_SUBSYSTEM_WINDOWS_CUI: u16 = 0x3;
    pub const IMAGE_SUBSYSTEM_WINDOWS_GUI: u16 = 0x2;
}
