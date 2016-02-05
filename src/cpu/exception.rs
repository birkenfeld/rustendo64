use super::cp0::InterruptMask;
use mem_map::*;

#[derive(Debug)]
pub struct Exception {
    pub exc_type: ExcType,
}

#[derive(Debug)]
pub enum ExcType {
    // ColdReset, SoftReset and NMI never happens
    InstrAddressError,
    InstrTLBMiss,
    InstrXTLBMiss,
    InstrTLBInvalid,
    InstrBusError,
    SystemCall,
    Breakpoint,
    CoprocessorUnusable(u8),
    ReservedInstruction,
    Trap,
    IntegerOverflow,
    FloatingPointException,
    DataAddressError(bool),  // on store?
    DataTLBMiss(bool),
    DataXTLBMiss(bool),
    DataTLBInvalid(bool),
    DataTLBModification,
    DataBusError,
    Watch,
    Interrupt(Intr),
}

#[derive(Debug, PartialEq)]
pub enum Intr {
    Timer,
    Ext(u8),
    Swi0,
    Swi1,
}

impl Exception {
    pub fn vector_location(&self, bootstrap: bool) -> u64 {
        let offset = match self.exc_type {
            ExcType::InstrTLBMiss | ExcType::DataTLBMiss(_)   => 0x0,
            ExcType::InstrXTLBMiss | ExcType::DataXTLBMiss(_) => 0x080,
            _                                                 => 0x180,
        };
        offset + if bootstrap { BS_EXC_VECTOR } else { DEF_EXC_VECTOR }
    }

    pub fn coprocessor(&self) -> u8 {
        match self.exc_type {
            ExcType::CoprocessorUnusable(n) => n,
            _ => 0
        }
    }

    pub fn code(&self) -> u8 {
        match self.exc_type {
            ExcType::InstrAddressError         => 4,
            ExcType::InstrTLBMiss              => 2,
            ExcType::InstrXTLBMiss             => 2,
            ExcType::InstrTLBInvalid           => 2,
            ExcType::InstrBusError             => 6,
            ExcType::SystemCall                => 8,
            ExcType::Breakpoint                => 9,
            ExcType::CoprocessorUnusable(_)    => 11,
            ExcType::ReservedInstruction       => 10,
            ExcType::Trap                      => 13,
            ExcType::IntegerOverflow           => 12,
            ExcType::FloatingPointException    => 15,
            ExcType::DataAddressError(false)   => 4,
            ExcType::DataAddressError(true)    => 5,
            ExcType::DataTLBMiss(false)        => 2,
            ExcType::DataTLBMiss(true)         => 3,
            ExcType::DataXTLBMiss(false)       => 2,
            ExcType::DataXTLBMiss(true)        => 3,
            ExcType::DataTLBInvalid(false)     => 2,
            ExcType::DataTLBInvalid(true)      => 3,
            ExcType::DataTLBModification       => 1,
            ExcType::DataBusError              => 7,
            ExcType::Watch                     => 23,
            ExcType::Interrupt(_)              => 0,
        }
    }

    // TODO: fixup this mess
    pub fn interrupt_mask(&self) -> InterruptMask {
        match self.exc_type {
            ExcType::Interrupt(ref i) => InterruptMask {
                timer_interrupt:    *i == Intr::Timer,
                external_interrupt: [*i == Intr::Ext(0), *i == Intr::Ext(1),
                                     *i == Intr::Ext(2), *i == Intr::Ext(3),
                                     *i == Intr::Ext(4)],
                software_interrupt: [*i == Intr::Swi0, *i == Intr::Swi1]
            },
            _ => InterruptMask {
                timer_interrupt: false,
                external_interrupt: [false; 5],
                software_interrupt: [false; 2],
            }
        }
    }

    // pub fn is_masked(&self, mask: &InterruptMask) -> bool {
    //     match self.exc_type {
    //         ExcType::Interrupt(ref i) 
    //     }
    // }
}
