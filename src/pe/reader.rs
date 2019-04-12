// use super::attribute::{
//     Annotation, Attribute, AttributeInfo, CodeAttribute, ElementValue, ElementValuePair, Exception,
//     InnerClassesBody, LineNumber, StackMapFrame, StackMapFrameBody, VerificationTypeInfo,
// };
// use super::classfile::ClassFile;
// use super::constant;
// use super::constant::{Constant, ConstantType};
// use super::field::FieldInfo;
// use super::method::MethodInfo;
use super::header;
use std::fs::File;
use std::io::{BufReader, Read};
use std::io::{Seek, SeekFrom};
// use std::mem::transmute;

#[derive(Debug)]
pub struct PEFileReader {
    reader: BufReader<File>,
}

macro_rules! try_eq {
    ($expr:expr) => {{
        if !$expr {
            return None;
        }
    }};
}

impl PEFileReader {
    pub fn new(filename: &str) -> Option<Self> {
        let file = match File::open(filename) {
            Ok(file) => file,
            Err(_) => return None,
        };

        Some(Self {
            reader: BufReader::new(file),
        })
    }

    pub fn read(&mut self) -> Option<()> {
        self.read_msdos_header()?;

        let pe_file_header = self.read_pe_file_header()?;
        dprintln!("PE File Header: {:?}", pe_file_header);

        let mut pe_optional_headers = vec![];
        let pe_optional_header_size = 224;
        for i in 0..pe_file_header.optional_header_size / pe_optional_header_size {
            let pe_optional_header = self.read_pe_optional_header()?;
            dprintln!("PE Optional Header({}): {:?}", i, pe_optional_header);
            pe_optional_headers.push(pe_optional_header);
        }

        let mut sections = vec![];
        for i in 0..pe_file_header.number_of_sections {
            let section = self.read_section_header()?;
            dprintln!("Section({}): {:?}", i, section);
            sections.push(section);
        }

        let text_section = || -> Option<header::SectionHeader> {
            for section in &sections {
                if section.name != ".text" {
                    continue;
                }
                return Some(section.clone());
            }
            None
        }()?;

        dprintln!(".text starts at 0x{:x}", text_section.pointer_to_raw_data);

        let cli_header_offset = text_section.pointer_to_raw_data as u64 + 8; /* CLI loader stub */
        self.reader.seek(SeekFrom::Start(cli_header_offset)).ok()?;

        let cli_header = self.read_cli_header()?;
        dprintln!("CLI Header: {:?}", cli_header);

        let metadata_offset = cli_header.metadata_rva - text_section.virtual_address
            + text_section.pointer_to_raw_data;
        dprintln!("MetaData starts at {:x}", metadata_offset);
        self.reader
            .seek(SeekFrom::Start(metadata_offset as u64))
            .ok()?;

        let metadata_header = self.read_metadata_header()?;
        dprintln!("MetaData header: {:?}", metadata_header);

        Some(())
    }

    fn read_msdos_header(&mut self) -> Option<()> {
        let mut first = [0u8; 60];
        self.read_bytes(&mut first)?;

        try_eq!(
            &first[..]
                == &[
                    0x4d, 0x5a, 0x90, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0xFF,
                    0xFF, 0x00, 0x00, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00,
                    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                ][..]
        );

        let lfanew = self.read_u32()?;
        dprintln!("lfanew: {:x}", lfanew);

        let mut latter = [0u8; 64];
        self.read_bytes(&mut latter)?;

        try_eq!(
            &latter[..]
                == &[
                    0x0e, 0x1f, 0xba, 0x0e, 0x00, 0xb4, 0x09, 0xcd, 0x21, 0xb8, 0x01, 0x4c, 0xcd,
                    0x21, 0x54, 0x68, 0x69, 0x73, 0x20, 0x70, 0x72, 0x6f, 0x67, 0x72, 0x61, 0x6d,
                    0x20, 0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x62, 0x65, 0x20, 0x72, 0x75,
                    0x6e, 0x20, 0x69, 0x6e, 0x20, 0x44, 0x4f, 0x53, 0x20, 0x6d, 0x6f, 0x64, 0x65,
                    0x2e, 0x0d, 0x0d, 0x0a, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                ][..]
        );

        dprintln!("MSDOS Header: success");

        Some(())
    }

    fn read_pe_file_header(&mut self) -> Option<header::PEFileHeader> {
        let mut pe_signature = [0u8; 4];
        self.read_bytes(&mut pe_signature)?;
        try_eq!(&pe_signature[..] == &['P' as u8, 'E' as u8, 0, 0][..]);

        let machine = self.read_u16()?;
        try_eq!(machine == 0x14c);

        let number_of_sections = self.read_u16()?;

        let time_date_stamp = self.read_u32()?;

        let pointer_to_symbol_table = self.read_u32()?;
        try_eq!(pointer_to_symbol_table == 0);

        let number_of_symbols = self.read_u32()?;
        try_eq!(number_of_symbols == 0);

        let optional_header_size = self.read_u16()?;

        let characteristics = self.read_u16()?;

        dprintln!("PE File Header: success");

        Some(header::PEFileHeader {
            number_of_sections,
            time_date_stamp,
            optional_header_size,
            characteristics,
        })
    }

    fn read_pe_optional_header(&mut self) -> Option<header::PEOptionalHeader> {
        let magic = self.read_u16()?;
        try_eq!(magic == 0x10b);

        let _lmajor = self.read_u8()?;
        // println!("{}", lmajor);
        // try_eq!(lmajor == 6);

        let lminor = self.read_u8()?;
        // println!("{}", lminor);
        try_eq!(lminor == 0);

        let code_size = self.read_u32()?;

        let initialized_data_size = self.read_u32()?;

        let uninitialized_data_size = self.read_u32()?;

        let entry_point_rva = self.read_u32()?;

        let base_of_code = self.read_u32()?;

        let base_of_data = self.read_u32()?;

        let image_base = self.read_u32()?;

        let section_alignment = self.read_u32()?;

        let file_alignment = self.read_u32()?;
        try_eq!(file_alignment == 0x200);

        let os_major = self.read_u16()?;
        try_eq!(os_major == 5 || os_major == 4);

        let os_minor = self.read_u16()?;
        try_eq!(os_minor == 0);

        let user_major = self.read_u16()?;
        try_eq!(user_major == 0);

        let user_minor = self.read_u16()?;
        try_eq!(user_minor == 0);

        let subsys_major = self.read_u16()?;
        try_eq!(subsys_major == 5 || subsys_major == 4);

        let subsys_minor = self.read_u16()?;
        try_eq!(subsys_minor == 0);

        let reserved = self.read_u32()?;
        try_eq!(reserved == 0);

        let image_size = self.read_u32()?;

        let header_size = self.read_u32()?;

        let file_checksum = self.read_u32()?;
        try_eq!(file_checksum == 0);

        let sub_system = self.read_u16()?;

        let dll_flags = self.read_u16()?;

        let stack_reserve_size = self.read_u32()?;

        let stack_commit_size = self.read_u32()?;

        let heap_reserve_size = self.read_u32()?;

        let heap_commit_size = self.read_u32()?;

        let loader_flags = self.read_u32()?;

        let number_of_data_directories = self.read_u32()?;

        let export_table = self.read_u64()?;
        try_eq!(export_table == 0);

        let import_table_rva = self.read_u32()?;

        let import_table_size = self.read_u32()?;

        let _resource_table = self.read_u64()?;
        // try_eq!(resource_table == 0);

        let exception_table = self.read_u64()?;
        try_eq!(exception_table == 0);

        let certification_table = self.read_u64()?;
        try_eq!(certification_table == 0);

        let base_relocation_table_rva = self.read_u32()?;

        let base_relocation_table_size = self.read_u32()?;

        let debug = self.read_u64()?;
        try_eq!(debug == 0);

        let copyright = self.read_u64()?;
        try_eq!(copyright == 0);

        let global_ptr = self.read_u64()?;
        try_eq!(global_ptr == 0);

        let tls_table = self.read_u64()?;
        try_eq!(tls_table == 0);

        let load_config_table = self.read_u64()?;
        try_eq!(load_config_table == 0);

        let bound_import = self.read_u64()?;
        try_eq!(bound_import == 0);

        let iat_rva = self.read_u32()?;

        let iat_size = self.read_u32()?;

        let delay_import_descriptor = self.read_u64()?;
        try_eq!(delay_import_descriptor == 0);

        let cli_header_rva = self.read_u32()?;

        let cli_header_size = self.read_u32()?;

        let reserved = self.read_u64()?;
        try_eq!(reserved == 0);

        Some(header::PEOptionalHeader {
            code_size,
            initialized_data_size,
            uninitialized_data_size,
            entry_point_rva,
            base_of_code,
            base_of_data,
            image_base,
            section_alignment,
            image_size,
            header_size,
            sub_system,
            dll_flags,
            stack_reserve_size,
            stack_commit_size,
            heap_reserve_size,
            heap_commit_size,
            loader_flags,
            number_of_data_directories,
            import_table_rva,
            import_table_size,
            base_relocation_table_rva,
            base_relocation_table_size,
            iat_rva,
            iat_size,
            cli_header_rva,
            cli_header_size,
        })
    }

    fn read_section_header(&mut self) -> Option<header::SectionHeader> {
        let mut name_bytes = [0u8; 8];
        self.read_bytes(&mut name_bytes)?;
        let name = name_bytes
            .iter()
            .take_while(|b| **b != 0)
            .map(|&s| s as char)
            .collect::<String>();

        let virtual_size = self.read_u32()?;

        let virtual_address = self.read_u32()?;

        let size_of_raw_data = self.read_u32()?;

        let pointer_to_raw_data = self.read_u32()?;

        let pointer_to_relocations = self.read_u32()?;

        let pointer_to_linenumbers = self.read_u32()?;

        let number_of_relocations = self.read_u16()?;

        let number_of_linenumbers = self.read_u16()?;

        let characteristics = self.read_u32()?;

        Some(header::SectionHeader {
            name,
            virtual_size,
            virtual_address,
            size_of_raw_data,
            pointer_to_raw_data,
            pointer_to_relocations,
            pointer_to_linenumbers,
            number_of_relocations,
            number_of_linenumbers,
            characteristics,
        })
    }

    fn read_cli_header(&mut self) -> Option<header::CLIHeader> {
        let cb = self.read_u32()?;

        let major_runtime_version = self.read_u16()?;

        let minor_runtime_version = self.read_u16()?;

        let metadata_rva = self.read_u32()?;

        let metadata_size = self.read_u32()?;

        let flags = self.read_u32()?;

        let entry_point_token = self.read_u32()?;

        let resources_rva = self.read_u32()?;

        let resources_size = self.read_u32()?;

        let strong_name_signature_rva = self.read_u32()?;

        let strong_name_signature_version = self.read_u32()?;

        let code_manager_table = self.read_u64()?;
        try_eq!(code_manager_table == 0);

        let vtable_fixups_virtual_address = self.read_u32()?;

        let vtable_fixups_size = self.read_u16()?;

        let vtable_fixups_type = self.read_u16()?;

        Some(header::CLIHeader {
            cb,
            major_runtime_version,
            minor_runtime_version,
            metadata_rva,
            metadata_size,
            flags,
            entry_point_token,
            resources_rva,
            resources_size,
            strong_name_signature_rva,
            strong_name_signature_version,
            vtable_fixups_virtual_address,
            vtable_fixups_size,
            vtable_fixups_type,
        })
    }

    fn read_metadata_header(&mut self) -> Option<header::MetaDataHeader> {
        let signature = self.read_u32()?;
        try_eq!(signature == 0x424A5342);

        let _major_version = self.read_u16()?;
        let _minor_version = self.read_u16()?;

        let reserved = self.read_u32()?;
        try_eq!(reserved == 0);

        let length = self.read_u32()?;

        let mut version_raw = vec![0u8; length as usize];
        self.read_bytes(version_raw.as_mut_slice())?;
        let version = version_raw
            .iter()
            .take_while(|b| **b != 0)
            .map(|&c| c as char)
            .collect::<String>();

        let flags = self.read_u16()?;
        try_eq!(flags == 0);

        let streams = self.read_u16()?;

        Some(header::MetaDataHeader { version, streams })
    }
}

impl PEFileReader {
    fn read_bytes(&mut self, buf: &mut [u8]) -> Option<()> {
        match self.reader.read_exact(buf) {
            Ok(()) => Some(()),
            Err(_) => None,
        }
    }

    fn read_u64(&mut self) -> Option<u64> {
        let mut buf = [0u8; 8];
        match self.reader.read_exact(&mut buf) {
            Ok(()) => Some(
                ((buf[7] as u64) << 56)
                    + ((buf[6] as u64) << 48)
                    + ((buf[5] as u64) << 40)
                    + ((buf[4] as u64) << 32)
                    + ((buf[3] as u64) << 24)
                    + ((buf[2] as u64) << 16)
                    + ((buf[1] as u64) << 8)
                    + buf[0] as u64,
            ),
            Err(_) => None,
        }
    }

    fn read_u32(&mut self) -> Option<u32> {
        let mut buf = [0u8; 4];
        match self.reader.read_exact(&mut buf) {
            Ok(()) => Some(
                ((buf[3] as u32) << 24)
                    + ((buf[2] as u32) << 16)
                    + ((buf[1] as u32) << 8)
                    + buf[0] as u32,
            ),
            Err(_) => None,
        }
    }

    fn read_u16(&mut self) -> Option<u16> {
        let mut buf = [0u8; 2];
        match self.reader.read_exact(&mut buf) {
            Ok(()) => Some(((buf[1] as u16) << 8) + buf[0] as u16),
            Err(_) => None,
        }
    }

    fn read_u8(&mut self) -> Option<u8> {
        let mut buf = [0u8; 1];
        match self.reader.read_exact(&mut buf) {
            Ok(()) => Some(buf[0]),
            Err(_) => None,
        }
    }
}
