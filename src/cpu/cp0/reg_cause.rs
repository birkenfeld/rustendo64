use super::reg_status::InterruptMask;
use util::bit_set;

#[derive(Debug, Default)]
pub struct RegCause {
    // BD
    pub exc_in_delay_slot: bool,

    // CE
    pub coprocessor: u8,

    // IP
    pub interrupts_pending: InterruptMask,

    // ExcCode
    pub exception_code: u8,
}

impl From<u32> for RegCause {
    fn from(value: u32) -> Self {
        RegCause {
            exc_in_delay_slot:   bit_set(value, 31),
            coprocessor:         (value >> 28) as u8 & 0b11,
            interrupts_pending:  value.into(),
            exception_code:      (value >> 2) as u8 & 0b11111,
        }
    }
}

impl RegCause {
    pub fn to_u32(&self) -> u32 {
        (self.exc_in_delay_slot as u32) << 31 |
        (self.coprocessor as u32) << 28 |
        self.interrupts_pending.to_u32() |
        (self.exception_code as u32) << 2
    }
}
