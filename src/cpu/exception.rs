use std::fmt;

use cp0::InterruptMask;
use cp0::Cp0;
use bus::mem_map::*;

#[allow(dead_code)]
pub enum Exception {
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
        let offset = match *self {
            Exception::InstrTLBMiss | Exception::DataTLBMiss(_)   => 0x0,
            Exception::InstrXTLBMiss | Exception::DataXTLBMiss(_) => 0x080,
            _                                                     => 0x180,
        };
        offset + if bootstrap { BS_EXC_VECTOR } else { DEF_EXC_VECTOR }
    }

    pub fn coprocessor(&self) -> u8 {
        match *self {
            Exception::CoprocessorUnusable(n) => n,
            _ => 0
        }
    }

    pub fn code(&self) -> u8 {
        match *self {
            Exception::InstrAddressError         => 4,
            Exception::InstrTLBMiss              => 2,
            Exception::InstrXTLBMiss             => 2,
            Exception::InstrTLBInvalid           => 2,
            Exception::InstrBusError             => 6,
            Exception::SystemCall                => 8,
            Exception::Breakpoint                => 9,
            Exception::CoprocessorUnusable(_)    => 11,
            Exception::ReservedInstruction       => 10,
            Exception::Trap                      => 13,
            Exception::IntegerOverflow           => 12,
            Exception::FloatingPointException    => 15,
            Exception::DataAddressError(false)   => 4,
            Exception::DataAddressError(true)    => 5,
            Exception::DataTLBMiss(false)        => 2,
            Exception::DataTLBMiss(true)         => 3,
            Exception::DataXTLBMiss(false)       => 2,
            Exception::DataXTLBMiss(true)        => 3,
            Exception::DataTLBInvalid(false)     => 2,
            Exception::DataTLBInvalid(true)      => 3,
            Exception::DataTLBModification       => 1,
            Exception::DataBusError              => 7,
            Exception::Watch                     => 23,
            Exception::Interrupt(_)              => 0,
        }
    }

    // TODO: fixup this mess
    pub fn interrupt_mask(&self) -> InterruptMask {
        match *self {
            Exception::Interrupt(ref i) => InterruptMask {
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

    pub fn is_enabled(&self, cp0: &Cp0) -> bool {
        let mask = &cp0.reg_status.interrupt_mask;
        match *self {
            Exception::Interrupt(ref i) =>
                cp0.reg_status.interrupts_enabled && match *i {
                    Intr::Timer  => mask.timer_interrupt,
                    Intr::Swi0   => mask.software_interrupt[0],
                    Intr::Swi1   => mask.software_interrupt[1],
                    Intr::Ext(0) => mask.external_interrupt[0],
                    Intr::Ext(1) => mask.external_interrupt[1],
                    Intr::Ext(2) => mask.external_interrupt[2],
                    Intr::Ext(3) => mask.external_interrupt[3],
                    Intr::Ext(4) => mask.external_interrupt[4],
                    Intr::Ext(_) => false,
                },
            _ => true
        }
    }
}

impl fmt::Debug for Exception {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Exception::*;
        let descr = match *self {
            InstrAddressError         => "instr address error",
            InstrTLBMiss              => "instr TLB miss",
            InstrXTLBMiss             => "instr XTLB miss",
            InstrTLBInvalid           => "instr TLB invalid",
            InstrBusError             => "instr bus error",
            SystemCall                => "syscall",
            Breakpoint                => "breakpoint",
            CoprocessorUnusable(_)    => "coprocessor unusable",
            ReservedInstruction       => "reserved instruction",
            Trap                      => "trap",
            IntegerOverflow           => "int overflow",
            FloatingPointException    => "float exception",
            DataAddressError(false)   => "read data address error",
            DataAddressError(true)    => "write data address error",
            DataTLBMiss(false)        => "read data TLB miss",
            DataTLBMiss(true)         => "write data TLB miss",
            DataXTLBMiss(false)       => "read data XTLB miss",
            DataXTLBMiss(true)        => "write data XTLB miss",
            DataTLBInvalid(false)     => "read data TLB invalid",
            DataTLBInvalid(true)      => "write data TLB invalid",
            DataTLBModification       => "data write-protected",
            DataBusError              => "data bus error",
            Watch                     => "watch",
            Interrupt(Intr::Timer)    => "timer interrupt",
            Interrupt(Intr::Swi0)     => "sw interrupt 0",
            Interrupt(Intr::Swi1)     => "sw interrupt 1",
            Interrupt(Intr::Ext(0))   => "ext interrupt 0",
            Interrupt(Intr::Ext(1))   => "ext interrupt 1",
            Interrupt(Intr::Ext(2))   => "ext interrupt 2",
            Interrupt(Intr::Ext(3))   => "ext interrupt 3",
            Interrupt(Intr::Ext(4))   => "ext interrupt 4",
            Interrupt(Intr::Ext(_))   => "ext interrupt ?",
        };
        write!(f, "{}", descr)
    }
}
