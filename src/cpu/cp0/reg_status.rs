use util::bit_set;

#[derive(Debug, Default)]
pub struct RegStatus {
    // CU
    coprocessor_usability: [bool; 4],

    // RP
    low_power: bool,

    // FR
    additional_fp_regs: bool,

    // RE
    reverse_endian: bool,

    // DS
    diagnostic_status: DiagnosticStatus,

    // IM(7:0)
    pub interrupt_mask: InterruptMask,

    // KX
    kernel_mode_64bit_addressing: bool,

    // SX
    supervisor_mode_64bit_addressing: bool,

    // UX
    user_mode_64bit_addressing: bool,

    // KSU
    mode: Mode,

    // ERL
    pub error_level: bool,

    // EXL
    pub exception_level: bool,

    // IE
    pub interrupts_enabled: bool
}

impl From<u32> for RegStatus {
    fn from(value: u32) -> Self {
        RegStatus {
            coprocessor_usability: [
                bit_set(value, 28),
                bit_set(value, 29),
                bit_set(value, 30),
                bit_set(value, 31)],

            low_power:                        bit_set(value, 27),
            additional_fp_regs:               bit_set(value, 26),
            reverse_endian:                   bit_set(value, 25),

            diagnostic_status: value.into(),
            interrupt_mask: value.into(),

            kernel_mode_64bit_addressing:     bit_set(value,  7),
            supervisor_mode_64bit_addressing: bit_set(value,  6),
            user_mode_64bit_addressing:       bit_set(value,  5),

            mode: value.into(),

            error_level:                      bit_set(value,  2),
            exception_level:                  bit_set(value,  1),
            interrupts_enabled:               bit_set(value,  0)
        }
    }
}

impl RegStatus {
    pub fn power_on_reset(&mut self) {
        self.error_level = true;
        self.diagnostic_status.soft_reset_or_nmi_occurred = false;
        self.low_power = false;
    }

    pub fn to_u32(&self) -> u32 {
        (self.coprocessor_usability[0] as u32) << 28 |
        (self.coprocessor_usability[1] as u32) << 29 |
        (self.coprocessor_usability[2] as u32) << 30 |
        (self.coprocessor_usability[3] as u32) << 31 |
        (self.low_power as u32) << 27 |
        (self.additional_fp_regs as u32) << 26 |
        (self.reverse_endian as u32) << 25 |
        (self.diagnostic_status.instruction_trace_support as u32) << 24 |
        (self.diagnostic_status.tlb_shutdown as u32) << 21 |
        (self.diagnostic_status.soft_reset_or_nmi_occurred as u32) << 20 |
        (self.diagnostic_status.condition_bit as u32) << 18 |
        self.interrupt_mask.to_u32() |
        (self.kernel_mode_64bit_addressing as u32) << 7 |
        (self.supervisor_mode_64bit_addressing as u32) << 6 |
        (self.user_mode_64bit_addressing as u32) << 5 |
        self.mode.to_u32() |
        (self.error_level as u32) << 2 |
        (self.exception_level as u32) << 1 |
        (self.interrupts_enabled as u32)
    }

    pub fn is_bootstrap(&self) -> bool {
        self.diagnostic_status.exception_vector_location == ExceptionVectorLocation::Bootstrap
    }
}

#[derive(Debug, Default)]
struct DiagnosticStatus {
    // ITS
    instruction_trace_support: bool,

    // BEV
    // TODO: Better name?
    exception_vector_location: ExceptionVectorLocation,

    // TS
    tlb_shutdown: bool,

    // SR
    soft_reset_or_nmi_occurred: bool,

    // CH
    condition_bit: bool,
}

impl From<u32> for DiagnosticStatus {
    fn from(value: u32) -> Self {
        DiagnosticStatus {
            instruction_trace_support:  bit_set(value, 24),

            exception_vector_location:  value.into(),

            tlb_shutdown:               bit_set(value, 21),
            soft_reset_or_nmi_occurred: bit_set(value, 20),
            condition_bit:              bit_set(value, 18)
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ExceptionVectorLocation {
    Normal,
    Bootstrap
}

impl Default for ExceptionVectorLocation {
    fn default() -> Self {
        ExceptionVectorLocation::Normal
    }
}

impl From<u32> for ExceptionVectorLocation {
    fn from(value: u32) -> Self {
        match (value >> 22) & 0b1 {
            0 => ExceptionVectorLocation::Normal,
            1 => ExceptionVectorLocation::Bootstrap,
            _ => unreachable!()
        }
    }
}

#[derive(Debug, Default)]
pub struct InterruptMask {
    // IM(7)
    pub timer_interrupt: bool,

    // IM(6:2)
    pub external_interrupt: [bool; 5],

    // IM(1:0)
    pub software_interrupt: [bool; 2]
}

impl From<u32> for InterruptMask {
    fn from(value: u32) -> Self {
        InterruptMask {
            timer_interrupt: bit_set(value, 15),

            external_interrupt: [
                bit_set(value, 10),
                bit_set(value, 11),
                bit_set(value, 12),
                bit_set(value, 13),
                bit_set(value, 14)],

            software_interrupt: [
                bit_set(value,  8),
                bit_set(value,  9)]
        }
    }
}

impl InterruptMask {
    pub fn to_u32(&self) -> u32 {
        (self.timer_interrupt as u32) << 15 |
        (self.external_interrupt[0] as u32) << 10 |
        (self.external_interrupt[1] as u32) << 11 |
        (self.external_interrupt[2] as u32) << 12 |
        (self.external_interrupt[3] as u32) << 13 |
        (self.external_interrupt[4] as u32) << 14 |
        (self.software_interrupt[0] as u32) << 8 |
        (self.software_interrupt[1] as u32) << 9
    }
}

#[derive(Debug)]
enum Mode {
    Kernel,
    Supervisor,
    User
}

impl Default for Mode {
    fn default() -> Self {
        Mode::Kernel
    }
}

impl From<u32> for Mode {
    fn from(value: u32) -> Self {
        match (value >> 3) & 0b11 {
            0b00 => Mode::Kernel,
            0b01 => Mode::Supervisor,
            0b10 => Mode::User,
            _ => panic!("Invalid cp0 KSU bits: {:#b}", value)
        }
    }
}

impl Mode {
    fn to_u32(&self) -> u32 {
        match *self {
            Mode::Kernel     => 0,
            Mode::Supervisor => 1 << 3,
            Mode::User       => 2 << 3,
        }
    }
}
