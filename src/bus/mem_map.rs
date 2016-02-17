// Constants for all the memory regions and registers.

macro_rules! define_consts {
    ($name:ident = $value:expr, $($nm:tt = $vl:tt),+) => {
        pub const $name: u32 = $value;
        define_consts!($($nm = $vl),+);
    };
    ($name:ident = $value:expr) => { pub const $name: u32 = $value; };
}

macro_rules! define_registers {
    (first: $basename:ident = $base:expr, last: $lastname:ident, $($names:tt),+) => {
        pub const $basename: u32 = $base;
        define_registers!(INNER $lastname, $base, $($names),+);
    };
    (INNER $lastname:ident, $addr:expr, (skip to $newaddr:expr), $($more:tt),+) => {
        define_registers!(INNER $lastname, $newaddr, $($more),+);
    };
    (INNER $lastname:ident, $addr:expr, $name:ident, $($more:tt),+) => {
        pub const $name: u32 = $addr;
        define_registers!(INNER $lastname, $addr + 4, $($more),+);
    };
    (INNER $lastname:ident, $addr:expr, $name:ident) => {
        pub const $name: u32 = $addr;
        pub const $lastname: u32 = $addr + 3;
    };
}

// Basic processor constants ---------------------------------------------------

pub const RESET_VECTOR:   u64 = 0xffff_ffff_bfc0_0000;
pub const BS_EXC_VECTOR:  u64 = 0xffff_ffff_bfc0_0200;
pub const DEF_EXC_VECTOR: u64 = 0xffff_ffff_8000_0000;
pub const KSEG0_START:    u64 = 0xffff_ffff_8000_0000;
pub const KSEG1_START:    u64 = 0xffff_ffff_a000_0000;

// RDRAM -----------------------------------------------------------------------

pub const RDRAM_SIZE:     usize = 0x80_0000;  // 8 MB

define_consts!(
    RDRAM_START     = 0x0000_0000,
    RDRAM_END       = 0x007f_ffff
);

define_registers!(
    first: RDRAM_REG_START = 0x03f0_0000,
    last:  RDRAM_REG_END,
    RDRAM_REG_CONFIG,
    RDRAM_REG_DEVICE_ID,
    RDRAM_REG_DELAY,
    RDRAM_REG_MODE,
    RDRAM_REG_REF_INTERVAL,
    RDRAM_REG_REF_ROW,
    RDRAM_REG_RAS_INTERVAL,
    RDRAM_REG_MIN_INTERVAL,
    RDRAM_REG_ADDR_SELECT,
    RDRAM_REG_DEVICE_MANUF
);

define_registers!(
    first: RI_REG_START = 0x0470_0000,
    last:  RI_REG_END,
    RI_REG_MODE,
    RI_REG_CONFIG,
    RI_REG_CURRENT_LOAD,  // write only
    RI_REG_SELECT,
    RI_REG_REFRESH,
    RI_REG_LATENCY,
    RI_REG_RERROR,        // read only
    RI_REG_WERROR         // write only
);

// RSP interface ---------------------------------------------------------------

pub const SP_RAM_SIZE: usize = 0x2000;

define_consts!(
    SP_DMEM_START = 0x0400_0000,
    // SP_DMEM_END   = 0x0400_0fff,
    // SP_IMEM_START = 0x0400_1000,
    SP_IMEM_END   = 0x0400_1fff
);

define_registers!(
    first: SP_REG_START = 0x0404_0000,
    last:  SP_REG_END,
    SP_REG_MEM_ADDR,
    SP_REG_DRAM_ADDR,
    SP_REG_RD_LEN,
    SP_REG_WR_LEN,
    SP_REG_STATUS,
    SP_REG_DMA_FULL,  // read only
    SP_REG_DMA_BUSY,  // read only
    SP_REG_SEMAPHORE,
    (skip to 0x0408_0000),
    SP_REG_PC,
    SP_REG_IBIST
);

// RDP interface ---------------------------------------------------------------

define_registers!(
    first: DP_REG_START = 0x0410_0000,
    last:  DP_REG_END,
    DPC_REG_DMA_START,
    DPC_REG_DMA_END,
    DPC_REG_CURRENT,  // read only
    DPC_REG_STATUS,
    DPC_REG_CLOCK,    // read only
    DPC_REG_BUFBUSY,  // read only
    DPC_REG_PIPEBUSY, // read only
    DPC_REG_TMEM,     // read only
    (skip to 0x0420_0000),
    DPS_REG_TBIST,
    DPS_REG_TEST_MODE,
    DPS_REG_BUFTEST_ADDR,
    DPS_REG_BUFTEST_DATA
);

// MIPS interface --------------------------------------------------------------

define_registers!(
    first: MI_REG_START = 0x0430_0000,
    last:  MI_REG_END,
    MI_REG_MODE,
    MI_REG_VERSION,   // read only
    MI_REG_INTR,      // read only
    MI_REG_INTR_MASK
);

// Video interface -------------------------------------------------------------

define_registers!(
    first: VI_REG_START = 0x0440_0000,
    last:  VI_REG_END,
    VI_REG_STATUS,
    VI_REG_ORIGIN,
    VI_REG_H_WIDTH,
    VI_REG_V_INTR,
    VI_REG_CURRENT,
    VI_REG_BURST,
    VI_REG_V_SYNC,
    VI_REG_H_SYNC,
    VI_REG_LEAP,
    VI_REG_H_START,
    VI_REG_V_START,
    VI_REG_V_BURST,
    VI_REG_X_SCALE,
    VI_REG_Y_SCALE
);

// Audio interface -------------------------------------------------------------

define_registers!(
    first: AI_REG_START = 0x0450_0000,
    last:  AI_REG_END,
    AI_REG_DRAM_ADDR,
    AI_REG_LEN,
    AI_REG_CONTROL,   // write only
    AI_REG_STATUS,
    AI_REG_DACRATE,   // write only
    AI_REG_BITRATE    // write only
);

// Peripheral interface --------------------------------------------------------

define_consts!(
    PIF_ROM_START = 0x1fc0_0000,
    PIF_ROM_END   = 0x1fc0_07bf,
    PIF_RAM_START = 0x1fc0_07c0,
    PIF_RAM_END   = 0x1fc0_07ff
);

define_registers!(
    first: PI_REG_START = 0x0460_0000,
    last:  PI_REG_END,
    PI_REG_DRAM_ADDR,
    PI_REG_CART_ADDR,
    PI_REG_RD_LEN,
    PI_REG_WR_LEN,
    PI_REG_STATUS,
    PI_REG_BSD_DOM1_LAT,
    PI_REG_BSD_DOM1_PWD,
    PI_REG_BSD_DOM1_PGS,
    PI_REG_BSD_DOM1_RLS,
    PI_REG_BSD_DOM2_LAT,
    PI_REG_BSD_DOM2_PWD,
    PI_REG_BSD_DOM2_PGS,
    PI_REG_BSD_DOM2_RLS
);

// Serial interface ------------------------------------------------------------

define_registers!(
    first: SI_REG_START = 0x0480_0000,
    last:  SI_REG_END,
    SI_REG_DRAM_ADDR,
    SI_REG_PIF_ADDR_RD64B,  // write only
    (skip to 0x0480_0010),
    SI_REG_PIF_ADDR_WR64B,  // write only
    (skip to 0x0480_0018),
    SI_REG_STATUS
);

// Cartridge and disk drive ----------------------------------------------------

define_consts!(
    CART_ROM_START = 0x1000_0000,
    CART_ROM_END   = 0x1fbf_ffff,
    DD_REG_START   = 0x0500_0500,
    DD_REG_END     = 0x0500_054b,
    DD_ROM_START   = 0x0600_0000,
    DD_ROM_END     = 0x063f_ffff
);
