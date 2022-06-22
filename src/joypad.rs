pub const RIGHT:    u8 = 0b1000_0000;
pub const LEFT:     u8 = 0b0100_0000;
pub const DOWN:     u8 = 0b0010_0000;
pub const UP:       u8 = 0b0001_0000;
pub const START:    u8 = 0b0000_1000;
pub const SELECT:   u8 = 0b0000_0100;
pub const BUTTON_B: u8 = 0b0000_0010;
pub const BUTTON_A: u8 = 0b0000_0001;



pub struct Joypad {
    strobe: bool,
    button_index: u8,
    button_status: u8,
}

impl Joypad {
    pub fn new() -> Self {
        Joypad {
            strobe: false,
            button_index: 0,
            button_status: 0,
        }
    }

    pub fn write(&mut self, data: u8) {
        self.strobe = data & 1 == 1;
        if self.strobe {
            self.button_index = 0;
        }
    }

    pub fn read(&mut self) -> u8 {
        if self.button_index > 7 {
            return 1;
        }
        let response = (self.button_status & (1 << self.button_index)) >> self.button_index;
        if !self.strobe && self.button_index <= 7 {
            self.button_index += 1;
        }
        response
    }

    pub fn set_button_pressed_status(&mut self, but: u8, state: bool) {
        if state {
            self.button_status = self.button_status | but;
        } else {
            self.button_status = self.button_status & (!but);
        }
    }
}
