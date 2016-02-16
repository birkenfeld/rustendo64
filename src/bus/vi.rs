use std::cmp::max;
use std::sync::atomic::{AtomicUsize, Ordering};

use bus::IoResult;
use bus::mi;
use bus::mem_map::*;
use ui::{UiChannel, UiOutput};

#[derive(Default, Debug)]
pub struct Vi {
    reg_status:      u32,
    reg_origin:      u32,
    reg_width:       u32,
    reg_intr:        u32,
    reg_current:     AtomicUsize,
    reg_burst:       u32,
    reg_v_sync:      u32,
    reg_h_sync:      u32,
    reg_leap:        u32,
    reg_h_start:     u32,
    reg_v_start:     u32,
    reg_v_burst:     u32,
    reg_x_scale:     u32,
    reg_y_scale:     u32,

    frame_width:     usize,
    frame_height:    usize,
    frame_hskip:     usize,
    vram_pixelsize:  usize,
    pub vram_start:  usize,
    pub vram_end:    usize,
}

impl Vi {
    pub fn read_reg(&self, addr: u32) -> IoResult<u32> {
        Ok(match addr {
            VI_REG_STATUS   => 0, // self.vi.reg_status,
            VI_REG_ORIGIN   => self.reg_origin,
            VI_REG_H_WIDTH  => self.reg_width,
            VI_REG_V_INTR   => self.reg_intr,
            /* TODO */
            VI_REG_CURRENT  =>
                self.reg_current.fetch_add(1, Ordering::Relaxed) as u32 % 525,
            VI_REG_BURST    => self.reg_burst,
            VI_REG_V_SYNC   => self.reg_v_sync,
            VI_REG_H_SYNC   => self.reg_h_sync,
            VI_REG_LEAP     => self.reg_leap,
            VI_REG_H_START  => self.reg_h_start,
            VI_REG_V_START  => self.reg_v_start,
            VI_REG_V_BURST  => self.reg_v_burst,
            VI_REG_X_SCALE  => self.reg_x_scale,
            VI_REG_Y_SCALE  => self.reg_y_scale,
            _ => return Err("Unsupported VI register")
        })
    }

    pub fn write_reg(&mut self, addr: u32, word: u32, mi: &mut mi::Mi,
                     ui: &UiChannel) -> IoResult<()> {
        Ok(match addr {
            VI_REG_STATUS   => {
                self.reg_status = word & 0xffff;
                self.update(ui);
            },
            VI_REG_ORIGIN   => {
                self.reg_origin = word & 0xff_ffff;  // only 24 bits
                // println!("VRAM at {:#x}", word);
                self.update_vram();
            },
            VI_REG_H_WIDTH  => {
                self.reg_width = word & 0xfff;
                self.update(ui);
            },
            VI_REG_V_INTR   => self.reg_intr = word & 0x3ff,
            VI_REG_CURRENT  => {
                mi.clear_interrupt(mi::Intr::VI);
            },
            VI_REG_BURST    => self.reg_burst = word & 0x3fff_ffff,
            VI_REG_V_SYNC   => self.reg_v_sync = word & 0x3ff,
            VI_REG_H_SYNC   => self.reg_h_sync = word & 0x1f_ffff,
            VI_REG_LEAP     => self.reg_leap = word & 0xfff_ffff,
            VI_REG_H_START  => {
                self.reg_h_start = word & 0x3ff_ffff;
                self.update(ui);
            },
            VI_REG_V_START  => {
                self.reg_v_start = word & 0x3ff_ffff;
                self.update(ui);
            },
            VI_REG_V_BURST  => self.reg_v_burst = word & 0x3ff_ffff,
            VI_REG_X_SCALE  => {
                self.reg_x_scale = word & 0xfff_ffff;
                self.update(ui);
            },
            VI_REG_Y_SCALE  => {
                self.reg_y_scale = word & 0xfff_ffff;
                self.update(ui);
            },
            _ => return Err("Unsupported VI register")
        })
    }

    pub fn update(&mut self, ui: &UiChannel) {
        let hstart = (self.reg_h_start >> 16) & 0x3ff;
        let vstart = (self.reg_v_start >> 16) & 0x3ff;
        let hend   = self.reg_h_start & 0x3ff;
        let vend   = self.reg_v_start & 0x3ff;
        let hcoeff = (self.reg_x_scale & 0xfff) as f64 / (1 << 10) as f64;
        let vcoeff = (self.reg_y_scale & 0xfff) as f64 / (1 << 10) as f64;
        let width  = (hend - hstart) as f64 * hcoeff;
        let height = ((vend - vstart) >> 1) as f64 * vcoeff;
        // println!("{} {} {} {} {} {} {} {}",
        //          hstart, vstart, hend, vend, hcoeff, vcoeff, width, height);
        self.frame_width  = width as usize;
        self.frame_height = height as usize;
        self.frame_hskip  = max(self.reg_width as usize, self.frame_width)
            - self.frame_width;
        self.vram_pixelsize = match self.reg_status & 0b11 {
            0b00 => 0,
            0b01 => panic!("using reserved video mode"),
            0b10 => 2,
            0b11 => 4,
            _    => unreachable!()
        };
        self.update_vram();
        // println!("Video: {}+{}x{}, {} bit color",
        //          self.frame_hskip, self.frame_width, self.frame_height,
        //          self.vram_pixelsize * 8);
        // TODO: dont show skip
        ui.send(UiOutput::SetMode(
            self.frame_width + self.frame_hskip, self.frame_height,
            self.vram_pixelsize));
    }

    pub fn update_vram(&mut self) {
        self.vram_start = self.reg_origin as usize / 4;
        self.vram_end   = self.vram_start +
            (self.reg_width as usize) * self.frame_height *
            self.vram_pixelsize / 4;
    }
}
