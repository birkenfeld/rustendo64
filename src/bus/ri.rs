#[derive(Default, Debug)]
pub struct RdRegs {
    pub reg_config: u32,
    pub reg_device_id: u32,
    pub reg_delay: u32,
    pub reg_mode: u32,
    pub reg_ref_interval: u32,
    pub reg_ref_row: u32,
    pub reg_ras_interval: u32,
    pub reg_min_interval: u32,
    pub reg_addr_select: u32,
    pub reg_device_manuf: u32,
}

#[derive(Default, Debug)]
pub struct Ri {
    pub reg_mode: u32,
    pub reg_config: u32,
    pub reg_select: u32,
    pub reg_refresh: u32,
    pub reg_latency: u32,
    pub reg_rerror: u32,
}
