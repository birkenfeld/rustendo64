use std::sync::atomic::{AtomicUsize, Ordering};
use byteorder::{BigEndian, ByteOrder};

use mi;
use bus::IoResult;
use mem::RamAccess;
use mem_map::*;
use ui::UiChannel;

#[derive(Default, Debug)]
pub struct Si {
    pif_rom:       Box<[u8]>,
    pif_ram:       Box<[u8]>,
    pif_status:    AtomicUsize,
    reg_dram_addr: u32,
    reg_status:    u32,
}

impl Si {
    pub fn new(pif_rom: Box<[u8]>) -> Si {
        Si { pif_rom: pif_rom,
             pif_ram: vec![0; 64].into_boxed_slice(),
             ..Si::default() }
    }

    pub fn read_reg(&self, addr: u32) -> IoResult<u32> {
        Ok(match addr {
            SI_REG_DRAM_ADDR  => self.reg_dram_addr,
            SI_REG_STATUS     => self.reg_status,
            _ => return Err("Unsupported SI register")
        })
    }

    pub fn write_reg<R>(&mut self, addr: u32, word: u32, mi: &mi::Mi,
                        ram: &mut R, ui: &UiChannel) -> IoResult<()>
        where R: RamAccess
    {
        Ok(match addr {
            SI_REG_DRAM_ADDR       => {
                self.reg_dram_addr = word & 0xff_ffff;
            },
            SI_REG_PIF_ADDR_RD64B  => {
                self.dma_read(ram, ui.get_input_state());
                self.reg_status |= 0x1000;
                mi.set_interrupt(mi::Intr::SI);
            },
            SI_REG_PIF_ADDR_WR64B  => {
                self.dma_write(ram);
                self.reg_status |= 0x1000;
                mi.set_interrupt(mi::Intr::SI);
            },
            SI_REG_STATUS          => {
                self.reg_status &= !0x1000;
                mi.clear_interrupt(mi::Intr::SI);
            },
            _ => return Err("Unsupported SI register")
        })
    }


    pub fn read_pif_rom(&self, addr: u32) -> IoResult<u32> {
        let rel_addr = (addr - PIF_ROM_START) as usize;
        Ok(BigEndian::read_u32(&self.pif_rom[rel_addr..]))
    }

    pub fn read_pif_ram(&self, addr: u32) -> IoResult<u32> {
        let rel_addr = (addr - PIF_RAM_START) as usize;
        if rel_addr == 0x3c {
            Ok(self.pif_status.load(Ordering::SeqCst) as u32)
        } else {
            if rel_addr == 0x24 {
                // hack to avoid looping at the end of the PIF rom
                self.pif_status.store(0x80, Ordering::SeqCst);
            }
            Ok(BigEndian::read_u32(&self.pif_ram[rel_addr..]))
        }
    }

    pub fn write_pif_ram(&mut self, addr: u32, word: u32, mi: &mi::Mi)
                         -> IoResult<()> {
        let rel_addr = (addr - PIF_RAM_START) as usize;
        BigEndian::write_u32(&mut self.pif_ram[rel_addr..], word);
        self.reg_status |= 0x1000;
        mi.set_interrupt(mi::Intr::SI);
        Ok(())
    }

    pub fn dma_read<R: RamAccess>(&mut self, ram: &mut R, cstate: u32) {
        // execute commands in PIF ram
        self.execute(cstate);
        // transfer 64 bytes PIF ram -> main memory
        let data: Vec<_> = (0..16).map(
            |i| BigEndian::read_u32(&self.pif_ram[4*i..])).collect();
        ram.write_range(self.reg_dram_addr as usize / 4, &data);
        // println!("\nPIF read:");
        // for i in 0..8 { println!("  {:#10x} {:#10x}", data[2*i], data[2*i+1]); }
    }

    pub fn dma_write<R: RamAccess>(&mut self, ram: &mut R) {
        // TODO: we should execute commands here too. But since we only
        // implement commands that read state, executing on dma_read() is fine.
        // transfer 64 bytes main memory -> PIF ram
        let data = ram.read_range(self.reg_dram_addr as usize / 4, 16);
        // println!("\nPIF write:");
        // for i in 0..8 { println!("  {:#10x} {:#10x}", data[2*i], data[2*i+1]); }
        for (i, word) in data.into_iter().enumerate() {
            BigEndian::write_u32(&mut self.pif_ram[4*i..], word);
        }
    }

    fn execute(&mut self, cstate: u32) {
        // nothing to do? XXX: probably only checked on dma_write().
        // if self.pif_ram[63] & 1 == 0 { return; }
        let mut channel = 0;
        let mut start = 0;
        while start <= 60 {  // last possible byte for a command
            let ntrans = self.pif_ram[start] as usize;
            if ntrans == 0xfe { break; }  // signals end of commands
            if ntrans >= 0x80 { start += 1; continue; }  // skip byte
            channel += 1;
            if channel > 6 { break; }  // reached max channels
            if ntrans == 0 { start += 1; continue; }  // skip channel
            let nrecv = self.pif_ram[start + 1] as usize;
            if nrecv & 0xc0 != 0 { break; }  // remaining error
            let end = start + 2 + ntrans + nrecv;
            if end > 63 {
                // not enough bytes left to read and write
                break;
            }
            let cmdresult = {
                let (input, output) = self.pif_ram[start + 2..end].split_at_mut(ntrans);
                Si::do_command(channel, ntrans, nrecv, input, output, cstate)
            };
            if let Some(err) = cmdresult {
                self.pif_ram[start + 1] |= err;
            }
            start = end;
        }
        self.pif_ram[63] = 0;  // commands executed
    }

    fn do_command(channel: u8, ntrans: usize, nrecv: usize, input: &[u8],
                  output: &mut [u8], cstate: u32) -> Option<u8> {
        if channel != 1 {
            return Some(0x80);  // nothing connected
        }
        match input[0] {  // command
            0 => {  // read status
                if !(ntrans == 1 && nrecv == 3) {
                    return Some(0x40);
                }
                // type: controller, nothing plugged in
                output[0] = 0x05;
                output[1] = 0x0;
                output[2] = 0x02;
            }
            1 => {  // read controller
                if !(ntrans == 1 && nrecv == 4) {
                    return Some(0x40);
                }
                BigEndian::write_u32(output, cstate);
            },
            _ => {
                return Some(0x80);
            }
        }
        None
    }

    pub fn set_cic_seed(&mut self, seed: u32) {
        BigEndian::write_u32(&mut self.pif_ram[0x24..], seed);
    }
}
