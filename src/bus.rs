use crate::cpu::Mem;
use crate::joypad::{Joypad, self};
use crate::ppu::NesPPU;
use crate::rom::Rom;

pub struct Bus<'call> {
    cpu_vram: [u8; 2048],
    prg_rom: Vec<u8>,
    ppu: NesPPU,
    joypad: Joypad,

    cycles: usize,
    gameloop_callback: Box<dyn FnMut(&NesPPU, &mut Joypad) + 'call>,
}

const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;

impl<'a> Mem for Bus<'a> {
    fn mem_read(&mut self, addr: u16) -> u8 {
        match addr {
            RAM ..= RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b00000111_11111111;
                self.cpu_vram[mirror_down_addr as usize]
            }
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                panic!("Attempt to read from write-only PPU {:x}", addr);
            }

            0x2002 => self.ppu.read_from_status(),

            0x2004 => self.ppu.read_from_oam(),

            0x2007 => self.ppu.read_data(),

            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b00100000_00000111;
                self.mem_read(mirror_down_addr)
            }

            0x4000..=0x4015 => {
                //ignore APU 
                0
            }

            0x4016 => {
                self.joypad.read()
            }

            0x4017 => {
                // ignore joypad 2
                0
            }

            0x8000..=0xFFFF => self.read_prg_rom(addr),
            _ => {
                println!("Mem read does not work at {:#x}", addr);
                0
            }
        }
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            RAM ..= RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b00000111_11111111;
                self.cpu_vram[mirror_down_addr as usize] = data;
            }
            0x2000 => self.ppu.write_to_ctrl(data),

            0x2001 => self.ppu.write_to_mask(data),

            0x2003 => self.ppu.write_to_oam_addr(data),

            0x2004 => self.ppu.write_to_oam(data),

            0x2005 => self.ppu.write_to_scrll(data),

            0x2006 => self.ppu.write_to_ppu_addr(data),

            0x2007 => self.ppu.write_data(data),

            0x4000..=0x4013 | 0x4015 => {
                //ignore APU
            }

            0x4016 => {
                self.joypad.write(data);
            }

            0x4017 => {
                //ignore joypad 2
            }

            0x4014 => {
                for i in 0..=255 {
                    let addr= (data as u16) << 8 | i;
                    let data = self.mem_read(addr);
                    self.ppu.write_to_oam(data);
                }
            }

            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b00100000_00000111;
                self.mem_write(mirror_down_addr, data);
            }
            0x8000..=0xFFFF => {
                panic!("Attempt to write to Cartridge ROM space: {:x}", addr);
            }
            _ => {
                println!("Mem write does not work at {:#x}", addr);
            }
        }
    }

    fn mem_read_u16(&mut self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos+1) as u16;
        (hi << 8) | (lo as u16)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xFF) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos + 1, hi);
    }
    
}

impl<'a> Bus<'a> {
    pub fn new<'call, F>(rom: Rom, gameloop_callback: F) -> Bus<'call> 
    where
        F: FnMut(&NesPPU, &mut Joypad) + 'call,
    {
        let ppu = NesPPU::new(rom.chr_rom, rom.screen_mirroring);
        let joypad = Joypad::new();

        Bus {
            cpu_vram: [0; 2048],
            prg_rom: rom.prg_rom,
            ppu,
            joypad,
            cycles: 0,
            gameloop_callback: Box::from(gameloop_callback),
        }
    }

    pub fn tick(&mut self, cycles: u8) {
        self.cycles += cycles as usize;

        let nmi_before = self.ppu.nmi_is_some();
        self.ppu.tick(cycles * 3);
        let nmi_after = self.ppu.nmi_is_some();

        if !nmi_before && nmi_after {
            (self.gameloop_callback)(&self.ppu, &mut self.joypad);
        }
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if self.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            addr = addr % 0x4000;
        }
        self.prg_rom[addr as usize]
    }

    pub fn poll_nmi_status(&mut self) -> Option<usize> {
        self.ppu.get_nmi_interrupt()
    }
}