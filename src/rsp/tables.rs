use simd::u8x16;

#[derive(Debug)]
pub struct SimdTables {
    pub shift_l:  [u8x16; 16],  // shuffle: shift n bytes to the left
    pub shift_r:  [u8x16; 16],  // shuffle: shift n bytes to the right
    pub keep_l:   [u8x16; 16],  // and: keep n bytes on the right
    pub keep_r:   [u8x16; 16],  // and: keep n bytes on the left
    pub el_shuf:  [u8x16; 16],  // shuffle: for "elements" spec in instrs
    pub bswap:    u8x16,         // shuffle: byte-swap 16-bit units
}

impl SimdTables {
    pub fn new() -> SimdTables {
        let mut tables = SimdTables {
            shift_l:  [u8x16::splat(0x80); 16],
            shift_r:  [u8x16::splat(0x80); 16],
            keep_l:   [u8x16::splat(0); 16],
            keep_r:   [u8x16::splat(0); 16],
            el_shuf:  SimdTables::el_shuf(),
            bswap:    u8x16::new(1, 0, 3, 2, 5, 4, 7, 6,
                                 9, 8, 11, 10, 13, 12, 15, 14),
        };
        for i in 0..16 {
            for j in 0..16 {
                macro_rules! set {($tbl:ident, if $cond:expr, $repl:expr) => {
                    if $cond { tables.$tbl[i] = tables.$tbl[i].replace(j as u32, $repl as u8); }
                }};

                set!(shift_l,  if j >= i,       j - i);
                set!(shift_r,  if j <  16 - i,  j + i);
                set!(keep_l,   if j <  i,       0xff);
                set!(keep_r,   if j >= 16 - i,  0xff);
            }
        }
        tables
    }

    fn el_shuf() -> [u8x16; 16] {
        // from cen64
        [
  /* -- */ u8x16::new(0x0,0x1,0x2,0x3,0x4,0x5,0x6,0x7,0x8,0x9,0xA,0xB,0xC,0xD,0xE,0xF),
  /* -- */ u8x16::new(0x0,0x1,0x2,0x3,0x4,0x5,0x6,0x7,0x8,0x9,0xA,0xB,0xC,0xD,0xE,0xF),

  /* 0q */ u8x16::new(0x0,0x1,0x0,0x1,0x4,0x5,0x4,0x5,0x8,0x9,0x8,0x9,0xC,0xD,0xC,0xD),
  /* 1q */ u8x16::new(0x3,0x2,0x3,0x2,0x6,0x7,0x6,0x7,0xA,0xB,0xA,0xB,0xE,0xF,0xE,0xF),

  /* 0h */ u8x16::new(0x0,0x1,0x0,0x1,0x0,0x1,0x0,0x1,0x8,0x9,0x8,0x9,0x8,0x9,0x8,0x9),
  /* 1h */ u8x16::new(0x2,0x3,0x2,0x3,0x2,0x3,0x2,0x3,0xA,0xB,0xA,0xB,0xA,0xB,0xA,0xB),
  /* 2h */ u8x16::new(0x4,0x5,0x4,0x5,0x4,0x5,0x4,0x5,0xC,0xD,0xC,0xD,0xC,0xD,0xC,0xD),
  /* 3h */ u8x16::new(0x6,0x7,0x6,0x7,0x6,0x7,0x6,0x7,0xE,0xF,0xE,0xF,0xE,0xF,0xE,0xF),

  /* 0w */ u8x16::new(0x0,0x1,0x0,0x1,0x0,0x1,0x0,0x1,0x0,0x1,0x0,0x1,0x0,0x1,0x0,0x1),
  /* 1w */ u8x16::new(0x2,0x3,0x2,0x3,0x2,0x3,0x2,0x3,0x2,0x3,0x2,0x3,0x2,0x3,0x2,0x3),
  /* 2w */ u8x16::new(0x4,0x5,0x4,0x5,0x4,0x5,0x4,0x5,0x4,0x5,0x4,0x5,0x4,0x5,0x4,0x5),
  /* 3w */ u8x16::new(0x6,0x7,0x6,0x7,0x6,0x7,0x6,0x7,0x6,0x7,0x6,0x7,0x6,0x7,0x6,0x7),
  /* 4w */ u8x16::new(0x8,0x9,0x8,0x9,0x8,0x9,0x8,0x9,0x8,0x9,0x8,0x9,0x8,0x9,0x8,0x9),
  /* 5w */ u8x16::new(0xA,0xB,0xA,0xB,0xA,0xB,0xA,0xB,0xA,0xB,0xA,0xB,0xA,0xB,0xA,0xB),
  /* 6w */ u8x16::new(0xC,0xD,0xC,0xD,0xC,0xD,0xC,0xD,0xC,0xD,0xC,0xD,0xC,0xD,0xC,0xD),
  /* 7w */ u8x16::new(0xE,0xF,0xE,0xF,0xE,0xF,0xE,0xF,0xE,0xF,0xE,0xF,0xE,0xF,0xE,0xF),
        ]
    }
}
