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
}
