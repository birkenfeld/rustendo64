use cpu;
use ui;
use bus;
use debug::DebugSpecList;

#[derive(Debug)]
pub struct N64 {
    cpu: cpu::Cpu,
}

impl N64 {
    pub fn new(pif_rom: Vec<u8>, cart_rom: Vec<u8>,
               debug: DebugSpecList) -> N64 {
        let interface = ui::init_ui::<ui::minifb::MinifbInterface>();
        let interconnect = bus::Bus::new(pif_rom, cart_rom, interface, debug);
        let cpu = cpu::Cpu::new(interconnect);

        N64 {
            cpu: cpu,
        }
    }

    pub fn power_on_reset(&mut self) {
        self.cpu.power_on_reset();
    }

    pub fn run(&mut self) {
        loop {
            for _ in 0..100000 { /* TODO: tweak this */
                self.cpu.run_instruction();
            }
            self.cpu.bus.vi_cycle();
        }
    }

    pub fn run_instruction(&mut self) {
        self.cpu.run_instruction();
    }
}
