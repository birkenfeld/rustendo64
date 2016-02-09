#[derive(Default, Debug)]
pub struct Ai {
    pub reg_dram_addr: u32,
    pub reg_len: u32,
    pub reg_control: u32,
    pub reg_status: u32,
    pub reg_dacrate: u32,
    pub reg_bitrate: u32,
}
