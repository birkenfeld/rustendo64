use ui::InterfaceChannel;
use ui::IfOutput;

#[derive(Default, Debug)]
pub struct Vi {
    pub reg_status:  u32,
    pub reg_origin:  u32,
    pub reg_width:   u32,
    pub reg_intr:    u32,
    pub reg_current: u32,
    pub reg_burst:   u32,
    pub reg_v_sync:  u32,
    pub reg_h_sync:  u32,
    pub reg_leap:    u32,
    pub reg_h_start: u32,
    pub reg_v_start: u32,
    pub reg_v_burst: u32,
    pub reg_x_scale: u32,
    pub reg_y_scale: u32,

    frame_width:     usize,
    frame_height:    usize,
    frame_hskip:     usize,
    vram_pixelsize:  usize,
    pub vram_start:  usize,
    pub vram_end:    usize,
}


impl Vi {
    pub fn update(&mut self, interface: &mut InterfaceChannel) {
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
        self.frame_hskip  = self.reg_width as usize - self.frame_width;
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
        interface.send(IfOutput::SetMode(
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
