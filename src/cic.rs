// From cen64.

const CIC_SEED_NUS_5101: u32 = 0x0000AC00;
const CIC_SEED_NUS_6101: u32 = 0x00063F3F;
const CIC_SEED_NUS_6102: u32 = 0x00023F3F;
const CIC_SEED_NUS_6103: u32 = 0x0002783F;
const CIC_SEED_NUS_6105: u32 = 0x0002913F;
const CIC_SEED_NUS_6106: u32 = 0x0002853F;
const CIC_SEED_NUS_8303: u32 = 0x0000DD00;

const CRC_NUS_5101: u32 = 0x587BD543;
const CRC_NUS_6101: u32 = 0x6170A4A1;
const CRC_NUS_6102: u32 = 0x90BB6CB5;
const CRC_NUS_6103: u32 = 0x0B050EE0;
const CRC_NUS_6105: u32 = 0x98BC2C86;
const CRC_NUS_6106: u32 = 0xACC8580A;
const CRC_NUS_8303: u32 = 0x0E018159;

pub fn get_cic_seed(rom_data: &[u8]) -> Option<u32> {
    let crc = si_crc32(&rom_data[0x40..0x1000]);
    let aleck64crc = si_crc32(&rom_data[0x40..0xc00]);

    if aleck64crc == CRC_NUS_5101 {
        return Some(CIC_SEED_NUS_5101);
    }
    match crc {
        CRC_NUS_6101 => Some(CIC_SEED_NUS_6101),
        CRC_NUS_6102 => Some(CIC_SEED_NUS_6102),
        CRC_NUS_6103 => Some(CIC_SEED_NUS_6103),
        CRC_NUS_6105 => Some(CIC_SEED_NUS_6105),
        CRC_NUS_6106 => Some(CIC_SEED_NUS_6106),
        CRC_NUS_8303 => Some(CIC_SEED_NUS_6101),
        _            => None
    }
}

fn si_crc32(data: &[u8]) -> u32 {
    // no use precalculating the table, we need it only once
    let mut table = [0u32; 256];
    for n in 0..256 {
        let mut c = n as u32;
        for _ in 0..8 {
            if c & 1 != 0 {
                c = (c >> 1) ^ 0xedb88320;
            } else {
                c >>= 1;
            }
        }
        table[n] = c;
    }
    let mut c = 0 ^ 0xffff_ffff;
    for b in data {
        c = table[(c ^ *b as u32) as usize & 0xff] ^ (c >> 8);
    }
    c ^ 0xffff_ffff
}
