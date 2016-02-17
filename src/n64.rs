use std::sync::RwLock;
use std::time::Duration;
use std::thread;
use crossbeam;

use ui;
use bus::{self, Bus};
use cpu;
use debug::DebugSpecList;
use bus::mem_map::*;

pub struct N64 {
    ui: ui::UiChannel,
    cpu: cpu::Cpu,
    ifs: bus::BusInterfaces,
    ram: RwLock<Box<[u32]>>,
    spram: RwLock<Box<[u32]>>,
}

impl N64 {
    pub fn new(pif_rom: Box<[u8]>, cart_rom: Box<[u8]>,
               debug: DebugSpecList) -> N64 {
        N64 {
            ui: ui::init_ui::<ui::minifb::MinifbInterface>(),
            cpu: cpu::Cpu::new(debug),
            ifs: bus::BusInterfaces::new(pif_rom, cart_rom),
            ram: RwLock::new(vec![0; RDRAM_SIZE/4].into_boxed_slice()),
            spram: RwLock::new(vec![0; SP_RAM_SIZE/4].into_boxed_slice()),
        }
    }

    pub fn power_on_reset(&mut self) {
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
            // CPU - runs in main thread
            let mut cpu_ui = self.ui.clone();
            loop {
                /* TODO: tweak this */
                {
                    let mut ram = self.ram.write().unwrap();
                    let mut cpu_bus = Bus::new(cpu_ui, &self.ifs, &mut **ram, &self.spram);
                    for _ in 0..10000 {
                        self.cpu.run_instruction(&mut cpu_bus);
                    }
                    cpu_ui = cpu_bus.destroy();
                }
                thread::yield_now();
            }
        })
    }
}
