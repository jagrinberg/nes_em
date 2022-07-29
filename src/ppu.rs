use core::panic;
use crate::rom::Mirroring;



#[derive(Debug)]
pub struct NesPPU {
    pub chr_rom: Vec<u8>,
    pub palette_table: [u8; 32],
    pub vram: [u8; 2048],
    pub oam_data: [u8; 256],

    pub mirroring: Mirroring,

    internal_data_buf: u8,

    pub ctrl: CtrlRegister,
    mask: MaskRegister,
    status: StatRegister,
    oam_addr: OAMAddr,
    pub scroll: ScrollRegister,
    addr: AddrRegister,

    cycles: usize,
    scanline: usize,

    nmi_interrupt: Option<usize>
}

impl NesPPU {
    pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        NesPPU { 
            chr_rom,
            palette_table: [0; 32],
            vram: [0; 2048],
            oam_data: [0; 256],
            mirroring,
            internal_data_buf: 0,
            ctrl: CtrlRegister::new(),
            mask: MaskRegister::new(),
            status: StatRegister::new(),
            oam_addr: OAMAddr::new(),
            scroll: ScrollRegister::new(),
            addr: AddrRegister::new(),
            cycles: 0,
            scanline: 0,
            nmi_interrupt: None,
        }
    }

    pub fn get_scroll_x(&self) -> usize {
        self.scroll.get_x() as usize
    }

    pub fn get_scroll_y(&self) -> usize {
        self.scroll.get_y() as usize
    }

    pub fn get_nmi_interrupt(&mut self) -> Option<usize> {
        let ret = self.nmi_interrupt;
        self.nmi_interrupt = None;
        ret
    }

    pub fn nmi_is_some(&self) -> bool {
        if let Some(_) = self.nmi_interrupt {
            true
        } else {
            false
        }
    }

    pub fn write_to_ppu_addr(&mut self, value: u8) {
        self.addr.update(value);
    }
    pub fn write_to_ctrl(&mut self, value: u8) {
        let before_nmi_status = self.ctrl.generate_vblank_nmi();
        self.ctrl.update(value);
        if !before_nmi_status && self.ctrl.generate_vblank_nmi() && self.status.is_in_vblank() {
            self.nmi_interrupt = Some(1);
        }
    }
    pub fn write_to_mask(&mut self, value: u8) {
        self.mask.update(value);
    }
    pub fn write_to_oam_addr(&mut self, value: u8) {
        self.oam_addr.update(value);
    }
    pub fn write_to_scrll(&mut self, value: u8) {
        self.scroll.update(value);
    }

    pub fn increment_vram_addr(&mut self) {
        self.addr.increment(self.ctrl.vram_addr_increment());
    }

    pub fn write_to_oam(&mut self, data: u8) {
        let result = self.oam_addr.get_addr();
        self.oam_data[result as usize] = data;
        self.oam_addr.inc();
    }

    pub fn read_from_status(&mut self) -> u8 {
        self.status.get_status()
    }
    pub fn read_from_oam(&self) -> u8 {
        self.oam_data[self.oam_addr.get_addr() as usize]
    }
    
    // Horizontal:
    //   [ A ] [ a ]
    //   [ B ] [ b ]

    // Vertical:
    //   [ A ] [ B ]
    //   [ a ] [ b ]
    pub fn mirror_vram_addr(&self, addr: u16) -> u16{
        let mirrored_vram = addr & 0b10_1111_1111_1111;
        let vram_index = mirrored_vram - 0x2000;
        let name_table = vram_index / 0x400;

        match (&self.mirroring, name_table) {
            (Mirroring::VERTICAL, 2) | (Mirroring::VERTICAL, 3) => vram_index - 0x800,
            (Mirroring::HORIZONTAL, 1) | (Mirroring::HORIZONTAL, 2) => vram_index - 0x400,
            (Mirroring::HORIZONTAL, 3) => vram_index - 0x800,
            (Mirroring::FOUR_SCREEN, _) => vram_index,
            _ => vram_index,
        }
    }

    pub fn read_data(&mut self) -> u8 {
        let addr = self.addr.get();
        self.increment_vram_addr();

        match addr {
            0..=0x1fff => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.chr_rom[addr as usize];
                result
            }
            0x2000..=0x2fff => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.vram[self.mirror_vram_addr(addr) as usize];
                result
            }
            0x3000..=0x3eff => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.vram[self.mirror_vram_addr(addr-0x1000) as usize];
                result
            }
            0x3f00..=0x3fff => {
                let mut addr = addr;
                while addr > 0x3f1f {
                    addr -= 32;
                }
                self.palette_table[(addr - 0x3f00) as usize]
            }
            _ => panic!("unexpected access to mirror read {}", addr),
        }
    }

    pub fn write_data(&mut self, data: u8) {
        let addr = self.addr.get();
        self.increment_vram_addr();

        match addr {
            0..=0x1fff => {
                self.chr_rom[addr as usize] = data;
            }
            0x2000..=0x2fff => {
                self.vram[self.mirror_vram_addr(addr) as usize] = data;
            }
            0x3000..=0x3eff => {
                self.vram[self.mirror_vram_addr(addr-0x1000) as usize] = data;
            },
            0x3f00..=0x3fff => {
                let mut addr = addr;
                while addr > 0x3f1f {
                    addr -= 32;
                }
                self.palette_table[(addr - 0x3f00) as usize] = data;
            }
            _ => panic!("unexpected access to mirror write {}", addr),
        }
    }

    pub fn tick(&mut self, cycles: u8) -> bool{
        self.cycles += cycles as usize;
        if self.cycles >= 341 {
            if self.is_sprite_0_hit(self.cycles) {
                self.status.set_sprite_zero_hit(true);
            }

            self.cycles = self.cycles - 341;
            self.scanline += 1;

            if self.scanline == 241 {
                self.status.set_vblank_status(true);
                self.status.set_sprite_zero_hit(false);
                if self.ctrl.generate_vblank_nmi() {
                    self.nmi_interrupt = Some(1);
                }
            }

            if self.scanline >= 262 {
                self.scanline = 0;
                self.nmi_interrupt = None;
                self.status.set_sprite_zero_hit(false);
                self.status.reset_vblank_status();
                return true;
            }
        }
        return false;
    }

    fn is_sprite_0_hit(&self, cycle: usize) -> bool {
        let y = self.oam_data[0] as usize;
        let x = self.oam_data[3] as usize;
        (y == self.scanline as usize) && x <= cycle && self.mask.show_sprites()
    }
}

#[derive(Debug)]
pub struct AddrRegister {
    value: (u8, u8),
    hi_ptr: bool,
}

impl AddrRegister {
    pub fn new() -> Self {
        AddrRegister {
            value: (0, 0),
            hi_ptr: true,
        }
    }
    fn set(&mut self, data: u16) {
        self.value.0 = (data >> 8) as u8;
        self.value.1 = (data & 0xff) as u8;
    }

    pub fn update(&mut self, data: u8) {
        if self.hi_ptr {
            self.value.0 = data;
        } else {
            self.value.1 = data;
        }

        if self.get() > 0x3fff {
            self.set(self.get() & 0b11111111111111);
        }
        self.hi_ptr = !self.hi_ptr;
    }

    pub fn increment(&mut self, inc: u8) {
        let lo = self.value.1;
        self.value.1 = self.value.1.wrapping_add(inc);
        if lo > self.value.1 {
            self.value.0 = self.value.0.wrapping_add(1);
        }
        if self.get() > 0x3fff {
            self.set(self.get() & 0b11111111111111);
        }
    }

    pub fn reset_latch(&mut self) {
        self.hi_ptr = true;
    }

    pub fn get(&self) -> u16 {
        ((self.value.0 as u16) << 8) | (self.value.1 as u16)
    }
}

const _NAMETABLE1: u8             = 0b00000001;
const _NAMETABLE2: u8             = 0b00000010;
const VRAM_ADD_INCREMENT: u8     = 0b00000100;
const SPRITE_PATTERN_ADDR: u8    = 0b00001000;
const BACKROUND_PATTERN_ADDR: u8 = 0b00010000;
const SPRITE_SIZE: u8            = 0b00100000;
const MASTER_SLAVE_SELECT: u8    = 0b01000000;
const GENERATE_NMI: u8           = 0b10000000;

#[derive(Debug)]
pub struct CtrlRegister {
    control: u8,
}

impl CtrlRegister {
    pub fn new() -> Self {
        CtrlRegister { control: 0 }
    }

    pub fn vram_addr_increment(&self) -> u8 {
        if VRAM_ADD_INCREMENT & self.control == 0 {
            1
        } else {
            32
        }
    }

    pub fn update(&mut self, data: u8) {
        self.control = data;
    }

    pub fn generate_vblank_nmi(&mut self) -> bool {
        GENERATE_NMI & self.control == GENERATE_NMI
    }

    pub fn bknd_pattern_addr(&self) -> u16 {
        ((self.control & BACKROUND_PATTERN_ADDR) as u16) << 8
    }

    pub fn sprt_pattern_addr(&self) -> u16 {
        ((self.control & SPRITE_PATTERN_ADDR) as u16) << 9
    }

    pub fn get_nametable(&self) -> u16 {
        let mul = (self.control & 0b11) as u16;
        (0x0400) * mul + 0x2000
    }
}

const GREYSCALE: u8         = 0b00000001;
const BG_LEFT_SHOW: u8      = 0b00000010;
const SPRITES_LEFT_SHOW: u8 = 0b00000100;
const BG_SHOW: u8           = 0b00001000;
const SPRITES_SHOW: u8      = 0b00010000;
const EMPH_RED: u8          = 0b00100000;
const EMPH_GREEN: u8        = 0b01000000;
const EMPH_BLUE: u8         = 0b10000000;

#[derive(Debug)]
pub struct MaskRegister {
    mask: u8,
}

impl MaskRegister {
    pub fn new() -> Self {
        MaskRegister {
            mask: 0,
        }
    }

    pub fn update(&mut self, data: u8) {
        self.mask = data;
    }

    pub fn show_sprites(&self) -> bool {
        (self.mask & SPRITES_SHOW) == SPRITES_SHOW
    }
}

const SPRITE_OVERFLOW: u8 = 0b00100000;
const SPRITE_0_HIT: u8    = 0b01000000;
const VBLANK_START: u8    = 0b10000000;

#[derive(Debug)]
pub struct StatRegister {
    status: u8,
}

impl StatRegister {
    pub fn new() -> Self {
        StatRegister { status: 0 }
    }

    pub fn get_status(&mut self) -> u8 {
        let result = self.status;
        self.status = self.status & !VBLANK_START;
        result
    }

    pub fn set_vblank_status(&mut self, value: bool) {
        if value {
            self.status = self.status | VBLANK_START;
        }
    }

    pub fn reset_vblank_status(&mut self) {
        self.status = self.status & !VBLANK_START;
    }

    pub fn is_in_vblank(&self) -> bool {
        self.status & VBLANK_START == VBLANK_START
    }

    pub fn set_sprite_zero_hit(&mut self, value: bool) {
        self.status = if value { self.status | SPRITE_0_HIT } else { self.status & !SPRITE_0_HIT }
    }
}

#[derive(Debug)]
pub struct OAMAddr {
    addr: u8,
}

impl OAMAddr {
    pub fn new() -> Self {
        OAMAddr { addr: 0 }
    }

    pub fn update (&mut self, addr: u8) {
        self.addr = addr;
    }

    pub fn get_addr (&self) -> u8 {
        self.addr
    }

    pub fn inc(&mut self) {
        self.addr = self.addr.wrapping_add(1);
    }
}

#[derive(Debug)]
pub struct ScrollRegister {
    scroll_x: u8,
    scroll_y: u8,
    toggle: bool,
}

impl ScrollRegister {
    pub fn new() -> Self {
        ScrollRegister { 
            scroll_x: 0,
            scroll_y: 0,
            toggle: true,
        }
    }

    pub fn update(&mut self, data: u8) {
        if self.toggle {
            self.scroll_x = data;
        } else {
            self.scroll_y = data;
        }
        self.toggle = !self.toggle;
    }
    
    pub fn get_x(&self) -> u8 {
        self.scroll_x
    }

    pub fn get_y(&self) -> u8 {
        self.scroll_y
    }
}


