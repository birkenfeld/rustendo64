#[link(name = "n64video")]
extern "C" {
    fn angrylion_rdp_init();
    fn rdp_process_list(
        dp_start: *mut u32,
        dp_current: *mut u32,
        dp_end: *mut u32,
        dp_status: *mut u32,
        rsp_dmem: *const u32,
        rdram: *mut u32,
    ) -> u32;
}

pub fn power_on_reset() {
    unsafe {
        angrylion_rdp_init();
    }
}

pub fn process_list(dp_start: &mut u32,
                    dp_current: &mut u32,
                    dp_end: &mut u32,
                    dp_status: &mut u32,
                    rsp_dmem: &[u32],
                    rdram: &mut [u32]) -> bool {
    unsafe {
        rdp_process_list(
            dp_start as *mut u32,
            dp_current as *mut u32,
            dp_end as *mut u32,
            dp_status as *mut u32,
            rsp_dmem.as_ptr(),
            rdram.as_mut_ptr(),
        ) != 0
    }
}
