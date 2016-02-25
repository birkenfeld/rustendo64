use byteorder::{BigEndian, ByteOrder};
use simd::{i16x8, u8x16};

// Helpers for the vector <-> memory load/stores.

pub fn u8_from_qword((dw1, dw2): (u64, u64)) -> u8x16 {
    let mut buffer = [0_u8; 16];
    BigEndian::write_u64(&mut buffer[0..], dw1);
    BigEndian::write_u64(&mut buffer[8..], dw2);
    u8x16::load(&buffer, 0)
}

pub fn qword_from_u8(vec: u8x16) -> (u64, u64) {
    let mut buffer = [0_u8; 16];
    vec.store(&mut buffer, 0);
    (BigEndian::read_u64(&buffer[0..]), BigEndian::read_u64(&buffer[8..]))
}

pub fn pack_signed(dword: u64) -> i16x8 {
    i16x8::new(
        ((dword >> 48) & 0xff00) as i16,
        ((dword >> 40) & 0xff00) as i16,
        ((dword >> 32) & 0xff00) as i16,
        ((dword >> 24) & 0xff00) as i16,
        ((dword >> 16) & 0xff00) as i16,
        ((dword >> 8)  & 0xff00) as i16,
        ( dword        & 0xff00) as i16,
        ((dword << 8)  & 0xff00) as i16,
    )
}

pub fn unpack_signed(vec: i16x8) -> u64 {
    ((vec.extract(0) as u64) & 0xff00) << 48 |
    ((vec.extract(1) as u64) & 0xff00) << 40 |
    ((vec.extract(2) as u64) & 0xff00) << 32 |
    ((vec.extract(3) as u64) & 0xff00) << 24 |
    ((vec.extract(4) as u64) & 0xff00) << 16 |
    ((vec.extract(5) as u64) & 0xff00) << 8  |
    ((vec.extract(6) as u64) & 0xff00)       |
    ((vec.extract(7) as u64) & 0xff00) >> 8
}

pub fn pack_unsigned(dword: u64) -> i16x8 {
    i16x8::new(
        ((dword >> 49) & 0x7f80) as i16,
        ((dword >> 41) & 0x7f80) as i16,
        ((dword >> 33) & 0x7f80) as i16,
        ((dword >> 25) & 0x7f80) as i16,
        ((dword >> 17) & 0x7f80) as i16,
        ((dword >> 9)  & 0x7f80) as i16,
        ((dword >> 1)  & 0x7f80) as i16,
        ((dword << 7)  & 0x7f80) as i16,
    )
}

pub fn unpack_unsigned(vec: i16x8) -> u64 {
    ((vec.extract(0) as u64) & 0x7f80) << 49 |
    ((vec.extract(1) as u64) & 0x7f80) << 41 |
    ((vec.extract(2) as u64) & 0x7f80) << 33 |
    ((vec.extract(3) as u64) & 0x7f80) << 25 |
    ((vec.extract(4) as u64) & 0x7f80) << 17 |
    ((vec.extract(5) as u64) & 0x7f80) << 9  |
    ((vec.extract(6) as u64) & 0x7f80) << 1  |
    ((vec.extract(7) as u64) & 0x7f80) >> 7
}

pub fn pack_unsigned_alternate((dw1, dw2): (u64, u64), offset: u64) -> i16x8 {
    if offset == 0 {
        i16x8::new(
            ((dw1 >> 49) & 0x7f80) as i16,
            ((dw1 >> 33) & 0x7f80) as i16,
            ((dw1 >> 17) & 0x7f80) as i16,
            ((dw1 >> 1)  & 0x7f80) as i16,
            ((dw2 >> 49) & 0x7f80) as i16,
            ((dw2 >> 33) & 0x7f80) as i16,
            ((dw2 >> 17) & 0x7f80) as i16,
            ((dw2 >> 1)  & 0x7f80) as i16
        )
    } else {
        i16x8::new(
            ((dw1 >> 41) & 0x7f80) as i16,
            ((dw1 >> 25) & 0x7f80) as i16,
            ((dw1 >> 9)  & 0x7f80) as i16,
            ((dw1 << 7)  & 0x7f80) as i16,
            ((dw2 >> 41) & 0x7f80) as i16,
            ((dw2 >> 25) & 0x7f80) as i16,
            ((dw2 >> 9)  & 0x7f80) as i16,
            ((dw2 << 7)  & 0x7f80) as i16
        )
    }
}

pub fn unpack_unsigned_alternate(vec: i16x8, offset: u64) -> (u64, u64) {
    if offset == 0 {
        (
            ((vec.extract(0) as u64) & 0x7f80) << 49 |
            ((vec.extract(1) as u64) & 0x7f80) << 33 |
            ((vec.extract(2) as u64) & 0x7f80) << 17 |
            ((vec.extract(3) as u64) & 0x7f80) << 1,
            ((vec.extract(4) as u64) & 0x7f80) << 49 |
            ((vec.extract(5) as u64) & 0x7f80) << 33 |
            ((vec.extract(6) as u64) & 0x7f80) << 17 |
            ((vec.extract(7) as u64) & 0x7f80) << 1
        )
    } else {
        (
            ((vec.extract(0) as u64) & 0x7f80) << 41 |
            ((vec.extract(1) as u64) & 0x7f80) << 25 |
            ((vec.extract(2) as u64) & 0x7f80) << 9  |
            ((vec.extract(3) as u64) & 0x7f80) >> 7,
            ((vec.extract(4) as u64) & 0x7f80) << 41 |
            ((vec.extract(5) as u64) & 0x7f80) << 25 |
            ((vec.extract(6) as u64) & 0x7f80) << 9  |
            ((vec.extract(7) as u64) & 0x7f80) >> 7
        )
    }
}

pub fn pack_unsigned_fourths((dw1, dw2): (u64, u64), offset: u64, lower: bool) -> i16x8 {
    let v1;
    let v2;
    let v3;
    let v4;

    match offset {
        0 => {
            v1 = ((dw1 >> 49) & 0x7f80) as i16;
            v2 = ((dw1 >> 17) & 0x7f80) as i16;
            v3 = ((dw2 >> 49) & 0x7f80) as i16;
            v4 = ((dw2 >> 17) & 0x7f80) as i16;
        },
        1 => {
            v1 = ((dw1 >> 41) & 0x7f80) as i16;
            v2 = ((dw1 >> 9)  & 0x7f80) as i16;
            v3 = ((dw2 >> 41) & 0x7f80) as i16;
            v4 = ((dw2 >> 9)  & 0x7f80) as i16;
        },
        2 => {
            v1 = ((dw1 >> 33) & 0x7f80) as i16;
            v2 = ((dw1 >> 1)  & 0x7f80) as i16;
            v3 = ((dw2 >> 33) & 0x7f80) as i16;
            v4 = ((dw2 >> 1)  & 0x7f80) as i16;
        },
        _ => {
            v1 = ((dw1 >> 25) & 0x7f80) as i16;
            v2 = ((dw1 << 7)  & 0x7f80) as i16;
            v3 = ((dw2 >> 25) & 0x7f80) as i16;
            v4 = ((dw2 << 7)  & 0x7f80) as i16;
        }
    }
    if lower {
        i16x8::new(0, 0, 0, 0, v1, v2, v3, v4)
    } else {
        i16x8::new(v1, v2, v3, v4, 0, 0, 0, 0)
    }
}

pub fn unpack_unsigned_fourths(vec: i16x8, (dw1, dw2): (u64, u64), offset: u64,
                               lower: bool) -> (u64, u64) {
    let v1;
    let v2;
    let v3;
    let v4;
    if lower {
        v1 = (vec.extract(4) as u64) & 0x7f80;
        v2 = (vec.extract(5) as u64) & 0x7f80;
        v3 = (vec.extract(6) as u64) & 0x7f80;
        v4 = (vec.extract(7) as u64) & 0x7f80;
    } else {
        v1 = (vec.extract(0) as u64) & 0x7f80;
        v2 = (vec.extract(1) as u64) & 0x7f80;
        v3 = (vec.extract(2) as u64) & 0x7f80;
        v4 = (vec.extract(3) as u64) & 0x7f80;
    }

    match offset {
        0 => ((dw1 & 0x00ffffff_00ffffff) | (v1 << 49) | (v2 << 17),
              (dw2 & 0x00ffffff_00ffffff) | (v3 << 49) | (v4 << 17)),
        1 => ((dw1 & 0xff00ffff_ff00ffff) | (v1 << 41) | (v2 << 9),
              (dw2 & 0xff00ffff_ff00ffff) | (v3 << 41) | (v4 << 9)),
        2 => ((dw1 & 0xffff00ff_ffff00ff) | (v1 << 33) | (v2 << 1),
              (dw2 & 0xffff00ff_ffff00ff) | (v3 << 33) | (v4 << 1)),
        _ => ((dw1 & 0xffffff00_ffffff00) | (v1 << 25) | (v2 >> 7),
              (dw2 & 0xffffff00_ffffff00) | (v3 << 25) | (v4 >> 7)),
    }
}
