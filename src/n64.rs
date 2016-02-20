use std::sync::{Arc, Condvar, RwLock};
use std::sync::atomic::AtomicBool;
use std::time::Duration;
use std::thread;
use crossbeam;

use ui;
use cpu;
use rdp;
use rsp;
use bus::{self, Bus};
use bus::mem_map::*;
use r4k::debug::DebugSpecList;

pub struct N64 {
    ui: ui::UiChannel,
    cpu: cpu::Cpu,
    rsp: rsp::Rsp,
    ifs: bus::BusInterfaces,
    ram: RwLock<Box<[u32]>>,
    spram: RwLock<Box<[u32]>>,
}

impl N64 {
    pub fn new(ui_opts: ui::Options, pif_rom: Box<[u8]>, cart_rom: Box<[u8]>,
               debug_cpu: DebugSpecList, debug_rsp: DebugSpecList) -> N64 {
        let rsp_sync_bit = Arc::new(AtomicBool::new(false));
        let rsp_sync_cond = Arc::new(Condvar::new());
        N64 {
            ui: ui::init_ui::<ui::minifb::MinifbInterface>(ui_opts),
            cpu: cpu::Cpu::new(debug_cpu),
            rsp: rsp::Rsp::new(debug_rsp, rsp_sync_bit.clone(), rsp_sync_cond.clone()),
            ifs: bus::BusInterfaces::new(pif_rom, cart_rom, rsp_sync_bit, rsp_sync_cond),
            ram: RwLock::new(vec![0; RDRAM_SIZE/4].into_boxed_slice()),
            spram: RwLock::new(vec![0; SP_RAM_SIZE/4].into_boxed_slice()),
        }
    }

    pub fn power_on_reset(&mut self) {
        rdp::power_on_reset();
        self.cpu.power_on_reset();
        self.ifs.power_on_reset();
        // write memory size into ram
        self.ram.write().unwrap()[0x3f0] = RDRAM_SIZE as u32;
    }

    pub fn run(&mut self) {
        crossbeam::scope(|scope| {
            let vi_ui = self.ui.clone();
            let mut vi_bus = bus::Bus::new(vi_ui, &self.ifs, &self.ram, &self.spram);
            let mut ai_bus = vi_bus.clone();
            // VI thread
            scope.spawn(move || {
                loop {
                    vi_bus.vi_cycle();
                    thread::sleep(Duration::new(0, 16_666_666));
                }
            });
            // AI thread
            scope.spawn(move || {
                loop {
                    ai_bus.ai_cycle();
                    thread::sleep(Duration::new(0, 1_000_000));
                }
            });
            // RSP thread
            let mut rsp_ui = self.ui.clone();
            let rsp_ram = &self.ram;      // unfortunately we have to create all
            let rsp_ifs = &self.ifs;      // these references here to be able to
            let rsp_spram = &self.spram;  // move them into the closure
            let rsp = &mut self.rsp;
            scope.spawn(move || {
                loop {
                    rsp.wait_for_start();
                    {
                        let mut spram = rsp_spram.write().unwrap();
                        let mut rsp_bus = bus::Bus::new(rsp_ui, rsp_ifs, rsp_ram, &mut **spram);
                        rsp.run_sequence(&mut rsp_bus);
                        rsp_ui = rsp_bus.into_ui();
                    }
                    thread::yield_now();
                }
            });
            // CPU - runs in main thread
            let mut cpu_ui = self.ui.clone();
            loop {
                {
                    let mut ram = self.ram.write().unwrap();
                    let mut cpu_bus = Bus::new(cpu_ui, &self.ifs, &mut **ram, &self.spram);
                    self.cpu.run_sequence(&mut cpu_bus, 100);
                    cpu_ui = cpu_bus.into_ui();
                }
                thread::yield_now();
            }
        })
    }
}
