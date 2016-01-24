// ********************** Config register **********************

// TODO: Better name?
#[derive(Debug)]
enum ConfigEp {
    D, // TODO: Better name?
    DxxDxx, // TODO: Better name?
    RFU
}

impl Default for ConfigEp {
    fn default() -> ConfigEp {
        ConfigEp::D
    }
}

// TODO: Better name?
#[derive(Debug)]
enum ConfigBe {
    LittleEndian,
    BigEndian
}

impl Default for ConfigBe {
    fn default() -> ConfigBe {
        ConfigBe::BigEndian
    }
}

#[derive(Debug, Default)]
struct Config {
    ep: ConfigEp,
    be: ConfigBe
}

impl Config {
    fn power_on_reset(&mut self) {
        self.ep = ConfigEp::D;
        self.be = ConfigBe::BigEndian;
    }
}

// ********************** Status register **********************

#[derive(Debug)]
enum StatusKsu {
    Kernel,
    Supervisor,
    User
}

impl Default for StatusKsu {
    fn default() -> StatusKsu {
        StatusKsu::Kernel
    }
}

#[derive(Debug, Default)]
struct Status {
    ie: bool,  // Interrupt enable
    exl: bool, // Exception level
    erl: bool, // Error level
    ksu: StatusKsu, // Mode (kernel, supervisor, user)
    ux: bool,  // 64-bit addressing enable in user mode
    sx: bool,  // 64-bit addressing enable in supervisor mode
    kx: bool,  // 64-bit addressing enable in kernel mode
    im: [bool; 8],  // Interrupt mask
    // ds: not implemented
    re: bool,  // Reverse endian
    fr: bool,  // Additional FPR enable
    rp: bool,  // Low-power mode
    cu: [bool; 4],  // Coprocessor enable
}

impl Status {
    fn power_on_reset(&mut self) {

    }
}

// ********************** Cp0 itself **********************

#[derive(Debug, Default)]
pub struct Cp0 {
    reg_status: Status,
    reg_config: Config
}

impl Cp0 {
    pub fn power_on_reset(&mut self) {
        self.reg_config.power_on_reset();
        self.reg_status.power_on_reset();
    }

    pub fn write_reg(&mut self, reg: u32, value: u64) {
        match reg {
            _ => {}
        }
    }

    pub fn read_reg(&self, reg: u32) -> u64 {
        match reg {
            _ => 0
        }
    }
}
