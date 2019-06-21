// TODO: What a dirty code

use crate::exec::decode::BytesToInstructions;
use crate::metadata::{
    assembly::*, class::*, header::*, image::*, metadata::*, method::*, signature::*,
};
use rustc_hash::FxHashMap;
use std::{cell::RefCell, rc::Rc};
use std::{
    fs::File,
    io::{BufReader, Read, Seek, SeekFrom},
    iter,
    path::PathBuf,
    str::from_utf8,
};

#[derive(Debug)]
pub struct PEParser {
    reader: BufReader<File>,
    filename: PathBuf,
}

macro_rules! try_eq {
    ($expr:expr) => {{
        if !$expr {
            return None;
        }
    }};
}

impl PEParser {
    pub fn new(filename: PathBuf) -> Option<Self> {
        Some(Self {
            reader: BufReader::new(match File::open(&filename) {
                Ok(file) => file,
                Err(_) => return None,
            }),
            filename: filename.to_path_buf(),
        })
    }

    pub fn create_assembly(&mut self) -> Option<Assembly> {
        self.read_msdos_header()?;

        let pe_file_header = self.read_pe_file_header()?;

        let mut pe_optional_headers = vec![];
        let pe_optional_header_size = 224;
        for _ in 0..pe_file_header.optional_header_size / pe_optional_header_size {
            let pe_optional_header = self.read_pe_optional_header()?;
            pe_optional_headers.push(pe_optional_header);
        }

        let mut sections = vec![];
        for _ in 0..pe_file_header.number_of_sections {
            let section = self.read_section_header()?;
            sections.push(section);
        }

        let text_section = || -> Option<&SectionHeader> {
            for section in &sections {
                if section.name != ".text" {
                    continue;
                }
                return Some(section);
            }
            None
        }()?;

        let cli_header_offset = text_section.pointer_to_raw_data as u64 + 8; /* CLI loader stub */
        self.reader.seek(SeekFrom::Start(cli_header_offset)).ok()?;

        let cli_header = self.read_cli_header()?;

        let metadata_offset = (cli_header.metadata_rva - text_section.virtual_address
            + text_section.pointer_to_raw_data) as u64;
        self.reader.seek(SeekFrom::Start(metadata_offset)).ok()?;

        let metadata_header = self.read_metadata_header()?;
        dprintln!("MetaData header: {:?}", metadata_header);

        let mut stream_headers = vec![];
        for _ in 0..metadata_header.streams {
            let stream = self.read_stream_header()?;
            stream_headers.push(stream);
        }

        dprintln!("Stream headers: {:?}", stream_headers);

        let metadata_streams =
            self.read_metadata_streams(metadata_offset, &metadata_header, &stream_headers)?;

        let image = Image::new(
            CLIInfo {
                cli_header,
                sections,
            },
            metadata_streams,
            self.filename.clone(),
            None,
        );

        let asm = retrieve!(
            image.metadata.metadata_stream.tables[TableKind::Assembly.into_num()]
                .get(0)
                .unwrap(),
            Table::Assembly
        );
        let name = image.get_string(asm.name).to_string();

        Some(Assembly { name, image })
    }

    pub fn read_method(
        &mut self,
        image: &Image,
        class: &ClassInfoRef,
        rva: u32,
    ) -> Option<MethodInfoRef> {
        let text_section = image
            .cli_info
            .sections
            .iter()
            .find(|section| section.name == ".text")
            .unwrap();
        let start = (rva - text_section.virtual_address + text_section.pointer_to_raw_data) as u64;
        self.read_method_body(image, class, rva, start)
    }

    fn read_method_body(
        &mut self,
        image: &Image,
        class: &ClassInfoRef,
        rva: u32,
        start: u64,
    ) -> Option<MethodInfoRef> {
        self.reader.seek(SeekFrom::Start(start)).ok()?;

        let (impl_flags, flags, name, ty) = unsafe {
            let MethodDefTable {
                name,
                signature,
                impl_flags,
                flags,
                ..
            } = image.get_method_def_table_by_rva(rva).unwrap();
            let sig = image.metadata.blob.get(&(*signature as u32)).unwrap();
            (
                *impl_flags,
                *flags,
                image.get_string(*name).to_string(),
                SignatureParser::new(sig)
                    .parse_method_def_sig(image)
                    .unwrap(),
            )
        };

        let header_ty = self.read_method_header_type()?;

        match header_ty {
            MethodHeaderType::TinyFormat { bytes } => {
                let mut raw_body = vec![0u8; bytes];
                self.read_bytes(raw_body.as_mut_slice())?;
                let body = BytesToInstructions::new(&raw_body).convert()?;
                Some(Rc::new(RefCell::new(MethodInfo::MDef(MethodDefInfo {
                    rva,
                    impl_flags,
                    flags,
                    name,
                    header_ty,
                    body,
                    locals_ty: vec![],
                    ty,
                    class: class.clone(),
                }))))
            }
            MethodHeaderType::FatFormat {
                code_size,
                local_var_sig_tok,
                ..
            } => {
                let locals_ty = match image.metadata.get_table_entry(local_var_sig_tok) {
                    Some(sig) => match sig {
                        Table::StandAloneSig(sast) => {
                            let mut blob = image
                                .metadata
                                .blob
                                .get(&(sast.signature as u32))
                                .unwrap()
                                .iter();
                            assert_eq!(blob.next()?, &0x07);
                            let len = *blob.next()? as usize;
                            use std::iter::repeat_with;
                            repeat_with(|| Type::into_type(image, &mut blob).unwrap())
                                .take(len)
                                .collect()
                        }
                        _ => unimplemented!(),
                    },
                    None => vec![],
                };

                let mut raw_body = vec![0u8; code_size as usize];
                self.read_bytes(raw_body.as_mut_slice())?;
                let body = BytesToInstructions::new(&raw_body).convert()?;

                Some(Rc::new(RefCell::new(MethodInfo::MDef(MethodDefInfo {
                    rva,
                    impl_flags,
                    flags,
                    name,
                    header_ty,
                    body,
                    locals_ty,
                    ty,
                    class: class.clone(),
                }))))
            }
        }
    }

    fn read_method_header_type(&mut self) -> Option<MethodHeaderType> {
        let first = self.read_u8()?;
        match first & 0b11 {
            TINY_FORMAT => Some(MethodHeaderType::TinyFormat {
                bytes: first as usize >> 2,
            }),
            FAT_FORMAT => {
                // TODO: More flags
                let flags_size = self.read_u8()?;
                let size = flags_size & 0b1111;
                let max_stack = self.read_u16()?;
                let code_size = self.read_u32()?;
                let local_var_sig_tok = self.read_u32()?;
                Some(MethodHeaderType::FatFormat {
                    flags: ((first as u16) << 8) + ((flags_size as u16) >> 4),
                    size,
                    max_stack,
                    code_size,
                    local_var_sig_tok,
                })
            }
            _ => None,
        }
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

        let _lfanew = self.read_u32()?;

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

        Some(())
    }

    fn read_pe_file_header(&mut self) -> Option<PEFileHeader> {
        let mut pe_signature = [0u8; 4];
        self.read_bytes(&mut pe_signature)?;
        try_eq!(&pe_signature[..] == &['P' as u8, 'E' as u8, 0, 0][..]);

        let pe_file_header = self.read_struct::<PEFileHeader>()?;
        try_eq!(pe_file_header.machine == 0x14c);
        try_eq!(pe_file_header.pointer_to_symbol_table == 0);
        try_eq!(pe_file_header.number_of_symbols == 0);

        Some(pe_file_header)
    }

    fn read_pe_optional_header(&mut self) -> Option<PEOptionalHeader> {
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

        Some(PEOptionalHeader {
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

    fn read_section_header(&mut self) -> Option<SectionHeader> {
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

        Some(SectionHeader {
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

    fn read_cli_header(&mut self) -> Option<CLIHeader> {
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

        Some(CLIHeader {
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

    fn read_metadata_header(&mut self) -> Option<MetaDataHeader> {
        let signature = self.read_u32()?;
        try_eq!(signature == 0x424A_5342);

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

        Some(MetaDataHeader { version, streams })
    }

    fn read_stream_header(&mut self) -> Option<StreamHeader> {
        let offset = self.read_u32()?;

        let size = self.read_u32()?;

        let mut name = "".to_string();
        let mut count = 1;
        loop {
            let c = self.read_u8()?;
            if c == 0 {
                while count % 4 != 0 {
                    self.read_u8()?;
                    count += 1;
                }
                break;
            }
            name.push(c as char);
            count += 1;
        }

        Some(StreamHeader { offset, size, name })
    }

    fn read_metadata_stream(&mut self) -> Option<MetaDataStream> {
        let reserved = self.read_u32()?;
        try_eq!(reserved == 0);

        let major_version = self.read_u8()?;

        let minor_version = self.read_u8()?;

        let heap_sizes = self.read_u8()?;

        let _reserved = self.read_u8()?;
        // try_eq!(reserved == 1);

        let valid = self.read_u64()?;

        let sorted = self.read_u64()?;

        let n = valid.count_ones();

        let mut rows = vec![];
        for _ in 0..n {
            let row = self.read_u32()?;
            rows.push(row);
        }

        let table_kinds = TableKind::table_kinds(valid);
        let tables = self.read_metadata_tables(&table_kinds, &rows)?;

        Some(MetaDataStream {
            major_version,
            minor_version,
            heap_sizes,
            valid,
            sorted,
            rows,
            tables,
        })
    }

    fn read_metadata_tables(
        &mut self,
        table_kinds: &[TableKind],
        rows: &[u32],
    ) -> Option<Vec<Vec<Table>>> {
        let mut tables: Vec<Vec<Table>> = iter::repeat_with(|| vec![]).take(NUM_TABLES).collect();

        for (i, kind) in table_kinds.iter().enumerate() {
            let num = rows[i];
            for _ in 0..num {
                tables[kind.into_num()].push(match kind {
                    TableKind::Module => Table::Module(self.read_struct::<ModuleTable>()?),
                    TableKind::TypeRef => Table::TypeRef(self.read_struct::<TypeRefTable>()?),
                    TableKind::TypeDef => Table::TypeDef(self.read_struct::<TypeDefTable>()?),
                    TableKind::MethodDef => Table::MethodDef(self.read_struct::<MethodDefTable>()?),
                    TableKind::MemberRef => Table::MemberRef(self.read_struct::<MemberRefTable>()?),
                    TableKind::CustomAttribute => {
                        Table::CustomAttribute(self.read_struct::<CustomAttributeTable>()?)
                    }
                    TableKind::Assembly => Table::Assembly(self.read_struct::<AssemblyTable>()?),
                    TableKind::AssemblyRef => {
                        Table::AssemblyRef(self.read_struct::<AssemblyRefTable>()?)
                    }
                    TableKind::Constant => Table::Constant(self.read_struct::<ConstantTable>()?),
                    TableKind::Param => Table::Param(self.read_struct::<ParamTable>()?),
                    TableKind::StandAloneSig => {
                        Table::StandAloneSig(self.read_struct::<StandAlongSigTable>()?)
                    }
                    TableKind::Field => Table::Field(self.read_struct::<FieldTable>()?),
                    e => unimplemented!("{:?}", e),
                })
            }
        }

        Some(tables)
    }

    // Return (length, read bytes)
    fn read_blob_length(&mut self) -> Option<(u32, u32)> {
        let first = self.read_u8()? as u32;
        if first & 0b1000_0000 == 0 {
            Some((first & 0b0111_1111, 1))
        } else if first & 0b1000_0000 > 0 {
            Some((((first & 0b0111_1111) << 8) + self.read_u8()? as u32, 2))
        } else if first & 0b1100_0000 > 0 {
            let x = self.read_u8()? as u32;
            let y = self.read_u8()? as u32;
            let z = self.read_u8()? as u32;
            Some((((first & 0b0011_1111) << 24) + (x << 16) + (y << 8) + z, 4))
        } else {
            None
        }
    }

    fn read_metadata_streams(
        &mut self,
        metadata_offset: u64,
        metadata_header: &MetaDataHeader,
        stream_headers: &[StreamHeader],
    ) -> Option<MetaDataStreams> {
        // Some of followings are not streams but heaps.
        let mut metadata_stream = None;
        let mut strings = None;
        let mut user_strings = None;
        let mut blob = None;
        let mut guid = None;

        for i in 0..metadata_header.streams as usize {
            self.reader
                .seek(SeekFrom::Start(
                    metadata_offset + stream_headers[i].offset as u64,
                ))
                .ok()?;
            match stream_headers[i].name.as_str() {
                "#~" => metadata_stream = self.read_metadata_streams_metadata_stream(),
                "#Strings" => strings = self.read_metadata_streams_strings(&stream_headers[i]),
                "#US" => user_strings = self.read_metadata_streams_user_strings(&stream_headers[i]),
                "#Blob" => blob = self.read_metadata_streams_blob(&stream_headers[i]),
                "#GUID" => guid = self.read_metadata_streams_guid(),
                _ => unreachable!(),
            }
        }

        Some(MetaDataStreams {
            metadata_stream: metadata_stream?,
            strings: strings?,
            user_strings: user_strings?,
            blob: blob?,
            guid: guid?,
        })
    }

    fn read_metadata_streams_metadata_stream(&mut self) -> Option<MetaDataStream> {
        let stream = self.read_metadata_stream()?;
        dprintln!("#~ stream: {:?}", stream);
        Some(stream)
    }

    fn read_metadata_streams_strings(
        &mut self,
        sh: &StreamHeader,
    ) -> Option<FxHashMap<u32, String>> {
        let mut strings = FxHashMap::default();
        let mut bytes = vec![];
        let mut bgn = 0;

        for i in 0..sh.size {
            let c = self.read_u8()?;
            if c == 0 {
                strings.insert(bgn, from_utf8(&bytes).unwrap().to_string());
                bytes.clear();
                bgn = i + 1;
            } else {
                bytes.push(c);
            }
        }

        when_debug!(for (i, s) in &strings {
            dprintln!("#Strings({:02X}): '{}'", i, s)
        });

        Some(strings)
    }

    fn read_metadata_streams_user_strings(
        &mut self,
        sh: &StreamHeader,
    ) -> Option<FxHashMap<u32, Vec<u16>>> {
        let mut user_strings = FxHashMap::default();
        let mut bytes = vec![];
        let mut count = 0;

        while count < sh.size {
            let bgn = count;
            let (len, read_bytes) = self.read_blob_length()?;
            count += read_bytes;

            /* read 2 bytes each so divide len by 2 */
            for _ in 0..len / 2 {
                bytes.push(self.read_u16()?);
            }

            if len % 2 == 1 {
                // Consume additional one byte
                self.read_u8()?; // TODO
            }

            user_strings.insert(bgn as u32, bytes.clone());
            bytes.clear();
            count += len as u32;
        }

        when_debug!(for (i, us) in &user_strings {
            dprintln!("#US({:02X}): '{}'", i, String::from_utf16(&us).ok()?)
        });

        Some(user_strings)
    }

    fn read_metadata_streams_blob(&mut self, sh: &StreamHeader) -> Option<FxHashMap<u32, Vec<u8>>> {
        let mut blob = FxHashMap::default();
        let mut bytes = vec![];
        let mut count = 0;

        while count < sh.size {
            let bgn = count;
            let (len, read_bytes) = self.read_blob_length()?;
            count += read_bytes;

            for _ in 0..len {
                bytes.push(self.read_u8()?);
            }

            blob.insert(bgn as u32, bytes.clone());
            bytes.clear();
            count += len as u32;
        }

        when_debug!(for (i, b) in &blob {
            dprintln!("#Blob({:02X}): {:?}", i, b)
        });

        Some(blob)
    }

    fn read_metadata_streams_guid(&mut self) -> Option<String> {
        let data1 = self.read_u32()?;
        let data2 = self.read_u16()?;
        let data3 = self.read_u16()?;
        let mut data4 = [0u8; 8];
        self.read_bytes(&mut data4)?;

        let guid = format!(
            "{:08X}-{:04X}-{:04X}-{:02X}{:02X}-{:02X}{:02X}{:02X}{:02X}{:02X}{:02X}",
            data1,
            data2,
            data3,
            data4[0],
            data4[1],
            data4[2],
            data4[3],
            data4[4],
            data4[5],
            data4[6],
            data4[7],
        );

        dprintln!("#GUID: {}", guid);

        Some(guid)
    }

    fn read_struct<T>(&mut self) -> Option<T> {
        unsafe {
            let size = ::std::mem::size_of::<T>();
            let mut strct: T = ::std::mem::zeroed();
            let strct_slice =
                ::std::slice::from_raw_parts_mut(&mut strct as *mut _ as *mut u8, size);
            self.reader.read_exact(strct_slice).ok()?;
            Some(strct)
        }
    }
}

impl PEParser {
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
