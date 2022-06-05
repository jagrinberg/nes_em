use crate::opcode::OpCode;
use crate::opcode::CPU_OPS_LOOKUP;

use crate::bus::Bus;

use crate::rom::Rom;

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    Accumulator,
    Relative,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect,
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
}

const STACK: u16 = 0x0100;
const STACK_RESET: u8 = 0xfd;

#[derive(Debug)]
pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub stack_pointer: u8,
    pub status: u8,
    pub program_counter: u16,
    pub bus: Bus,
}

pub trait Mem {
    fn mem_read(&self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, data: u8);
    fn mem_read_u16(&self, pos: u16) -> u16;
    fn mem_write_u16(&mut self, pos: u16, data: u16);
}

impl Mem for CPU {
    fn mem_read(&self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.mem_write(addr, data);
    }

    fn mem_read_u16(&self, pos: u16) -> u16 {
        self.bus.mem_read_u16(pos)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        self.bus.mem_write_u16(pos, data);
    }

}

impl CPU {
    pub fn new(bus: Bus) -> Self {
        CPU { 
            register_a: 0, 
            register_x: 0,
            register_y: 0,
            stack_pointer: STACK_RESET,
            status: 0b100100, 
            program_counter: 0,
            bus,
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = STACK_RESET;
        self.status = 0b100100;

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    // pub fn load(&mut self, program: Vec<u8>) {
    //     self.memory[0x0600 .. (0x0600 + program.len())].copy_from_slice(&program[..]);
    //     self.mem_write_u16(0xFFFC, 0x0600);
    // }


    // pub fn load_and_run(&mut self, program: Vec<u8>) {
    //     self.load(program);
    //     self.reset();
    //     self.run();
    // }

    fn push_stack(&mut self, value: u8) {
        self.mem_write(STACK + (self.stack_pointer as u16), value);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn push_stack_u16(&mut self, value: u16) {
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
        self.mem_write_u16(STACK + (self.stack_pointer as u16), value);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn pop_stack(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.mem_read(STACK + (self.stack_pointer as u16))
    }

    fn pop_stack_u16(&mut self) -> u16 {
        self.stack_pointer = self.stack_pointer.wrapping_add(2);
        self.mem_read_u16(STACK + (self.stack_pointer.wrapping_sub(1) as u16))
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut CPU),
    {
        loop {
            callback(self);
            let opscode = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let opscode: &OpCode = *CPU_OPS_LOOKUP.get(&opscode).unwrap();

            let op = opscode.name;

            // println!("{}, {:#x}\n", op, opscode.opcode);

            match op {
                "BRK" => {
                    return;
                }

                "ADC" => self.adc(&opscode.add_mode),

                "AND" => self.and(&opscode.add_mode),

                "ASL" => self.asl(&opscode.add_mode),

                "BCC" => self.bcc(),

                "BCS" => self.bcs(),

                "BEQ" => self.beq(),

                "BIT" => self.bit(&opscode.add_mode),

                "BMI" => self.bmi(),

                "BNE" => self.bne(),

                "BPL" => self.bpl(),

                "BVC" => self.bvc(),

                "BVS" => self.bvs(),

                "CLC" => self.clc(),

                "CLD" => self.cld(),

                "CLI" => self.cli(),

                "CLV" => self.clv(),

                "CMP" => self.cmp(&opscode.add_mode),

                "CPX" => self.cpx(&opscode.add_mode),

                "CPY" => self.cpy(&opscode.add_mode),

                "DEC" => self.dec(&opscode.add_mode),

                "DEX" => self.dex(),

                "DEY" => self.dey(),

                "EOR" => self.eor(&opscode.add_mode),

                "INC" => self.inc(&opscode.add_mode),

                "INX" => self.inx(),

                "INY" => self.iny(),

                "JMP" => self.jmp(&opscode.add_mode),

                "JSR" => self.jsr(),

                "LDA" => self.lda(&opscode.add_mode), // LDA - loads accumulator with immediate parameter and sets status

                "LDX" => self.ldx(&opscode.add_mode),

                "LDY" => self.ldy(&opscode.add_mode),
                
                "LSR" => self.lsr(&opscode.add_mode),

                "NOP" => (),

                "ORA" => self.ora(&opscode.add_mode),

                "PHA" => self.pha(),

                "PHP" => self.php(),

                "PLA" => self.pla(),

                "PLP" => self.plp(),
                    
                "ROL" => self.rol(&opscode.add_mode),

                "ROR" => self.ror(&opscode.add_mode),
                
                "RTI" => self.rti(),

                "RTS" => self.rts(),
                
                "SBC" => self.sbc(&opscode.add_mode),

                "SEC" => self.sec(),

                "SED" => self.sed(),

                "SEI" => self.sei(),

                "STA" => self.sta(&opscode.add_mode),

                "STX" => self.stx(&opscode.add_mode),

                "STY" => self.sty(&opscode.add_mode),

                "TAX" => self.tax(),

                "TAY" => self.tay(),

                "TSX" => self.tsx(),

                "TXA" => self.txa(),

                "TXS" => self.txs(),

                "TYA" => self.tya(),

                _ => todo!(),
            }

            self.program_counter += opscode.bytes-1;
        }
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,

            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,

            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),

            AddressingMode::Indirect => {
                let addr = self.mem_read_u16(self.program_counter);
                if addr & 0xFF == 0xFF {
                    let lo = self.mem_read(addr);
                    let hi = self.mem_read(addr - 0xFF);
                    (hi as u16) << 8 | (lo as u16)
                } else {
                    self.mem_read_u16(addr)
                }
            }

            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }

            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }
            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_y as u16);
                addr
            }

            AddressingMode::Indirect_X => {
                let base = self.mem_read(self.program_counter);

                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);

                let lo = self.mem_read(base as u16);
                let hi = self.mem_read((base as u8).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
            }
            AddressingMode::NoneAddressing | AddressingMode::Accumulator | AddressingMode::Relative => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let total = (self.register_a as u16) + (value as u16) + (1 & self.status as u16);
        
        self.update_carry_flag(total);
        let total = total as u8;
        self.update_overflow_flag(total, self.register_a, value);
        self.update_zero_and_negative_flags(total);

        self.register_a = total;
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = self.register_a & value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn asl(&mut self, mode: &AddressingMode) {
        let value;
        if let AddressingMode::Accumulator = mode {
            value = (self.register_a as u16) << 1;
            self.register_a = value as u8;
        } else {
            let addr = self.get_operand_address(mode);
            value = (self.mem_read(addr) as u16) << 1;
            self.mem_write(addr, value as u8);
        }
        self.update_carry_flag(value);
        self.update_zero_and_negative_flags(value as u8);
    }

    fn bcc(&mut self) {
        if self.status & 1 == 0 {
            self.branch();
        }
    }

    fn bcs(&mut self) {
        if self.status & 1 == 1 {
            self.branch();
        }
    }
    
    fn beq(&mut self) {
        if self.status & 0b0000_0010 == 0b0000_0010 {
            self.branch();
        }
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = self.register_a & value;
        self.update_zero_and_negative_flags(result);
        self.status = (self.status & 0b0011_1111) | (value & 0b1100_0000);
    }

    fn bmi(&mut self) {
        if self.status & 0b1000_0000 == 0b1000_0000 {
            self.branch();
        }
    }

    fn bne(&mut self) {
        if self.status & 0b10 == 0 {
            
            self.branch();
        }
    }

    fn bpl(&mut self) {
        if self.status & 0b1000_0000 == 0 {
            self.branch();
        }
    }

    fn bvc(&mut self) {
        if self.status & 0b0100_0000 == 0 {
            self.branch();
        }
    }

    fn bvs(&mut self) {
        if self.status & 0b0100_0000 == 0b0100_0000 {
            self.branch();
        }
    }

    fn clc(&mut self) {
        self.status = self.status & 0b1111_1110;
    }

    fn cld(&mut self) {
        self.status = self.status & 0b1111_0111;
    }

    fn cli(&mut self) {
        self.status = self.status & 0b1111_1011;
    }

    fn clv(&mut self) {
        self.status = self.status & 0b1011_1111;
    }

    fn cmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
       

        if value <= self.register_a {
            self.status = self.status | 1;
        } else {
            self.status = self.status & !(1 as u8);
        }

        let value = (!value).wrapping_add(1);

        let result = (self.register_a).wrapping_add(value);
        
        self.update_zero_and_negative_flags(result);
    }

    fn cpx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
       

        if value <= self.register_x {
            self.status = self.status | 1;
        } else {
            self.status = self.status & !(1 as u8);
        }

        let value = (!value).wrapping_add(1);

        let result = (self.register_x).wrapping_add(value);
        
        self.update_zero_and_negative_flags(result);
    }

    fn cpy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
       

        if value <= self.register_y {
            self.status = self.status | 1;
        } else {
            self.status = self.status & !(1 as u8);
        }

        let value = (!value).wrapping_add(1);

        let result = (self.register_y).wrapping_add(value);
        
        self.update_zero_and_negative_flags(result);
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        
        let result = value.wrapping_sub(1);
        self.mem_write(addr, result);

        self.update_zero_and_negative_flags(result);
    }

    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = self.register_a ^ value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = value.wrapping_add(1);
        self.mem_write(addr, result);
        self.update_zero_and_negative_flags(result);
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn jmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.jump(addr.wrapping_sub(2));
    }

    fn jsr(&mut self) {
        let addr = self.get_operand_address(&AddressingMode::Absolute);
        self.push_stack_u16(self.program_counter+1);
        self.jump(addr.wrapping_sub(2));
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn lsr(&mut self, mode: &AddressingMode) {
        let value;
        let carry;
        if let AddressingMode::Accumulator = mode {
            carry = self.register_a & 1;
            value = (self.register_a) >> 1;
            self.register_a = value;
        } else {
            let addr = self.get_operand_address(mode);
            let num = self.mem_read(addr);
            carry = num & 1;
            value = (self.mem_read(addr)) >> 1;
            self.mem_write(addr, value);
        }
        self.update_carry_flag(((carry as u16) << 8 )| (value as u16));
        self.update_zero_and_negative_flags(value);
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = self.register_a | value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn pha(&mut self) {
        self.push_stack(self.register_a);
    }

    fn php(&mut self) {
        self.push_stack(self.status | 0b0011_0000);
    }

    fn pla(&mut self) {
        self.register_a = self.pop_stack();
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn plp(&mut self) {
        self.status = (self.pop_stack() & 0b1110_1111) | 0b0010_0000;
    }

    fn rol(&mut self, mode: &AddressingMode) {
        let value;
        let carry = self.status & 1;
        if let AddressingMode::Accumulator = mode {
            self.update_carry_flag((self.register_a as u16) << 1);
            self.register_a = (self.register_a << 1) | (carry);
            value = self.register_a;
        } else {
            let addr = self.get_operand_address(mode);
            let v = self.mem_read(addr);
            self.update_carry_flag((v as u16) << 1);
            value = (v << 1) | (carry);
            self.mem_write(addr, value);
        }
        self.update_zero_and_negative_flags(value);
    }

    fn ror(&mut self, mode: &AddressingMode) {
        let value;
        let u;
        if let AddressingMode::Accumulator = mode {
            u = ((self.register_a & 1) as u16) << 8;
            self.register_a = (self.register_a >> 1) | ((self.status & 1) << 7);
            
            value = self.register_a;
        } else {
            let addr = self.get_operand_address(mode);
            let v = self.mem_read(addr);
            u = ((v & 1) as u16) << 8;
            value = (v >> 1) | ((self.status & 1) << 7);
            self.mem_write(addr, value);
        }
        self.update_carry_flag(u);
        self.update_zero_and_negative_flags(value);
    }

    fn rti(&mut self) {
        self.plp();
        self.program_counter = self.pop_stack_u16();
    }

    fn rts(&mut self) {
        self.program_counter = self.pop_stack_u16() + 1;
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = !self.mem_read(addr);

        let total = (self.register_a as u16) + (value as u16) + (1 & self.status as u16);
        
        self.update_carry_flag(total);
        let total = total as u8;
        self.update_overflow_flag(total, self.register_a, value);
        self.update_zero_and_negative_flags(total);

        self.register_a = total;
    }

    fn sec(&mut self) {
        self.status = self.status | 1;
    }

    fn sed(&mut self) {
        self.status = self.status | 0b1000;
    }

    fn sei(&mut self) {
        self.status = self.status | 0b100;
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }

    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn tsx(&mut self) {
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn txs(&mut self) {
        self.stack_pointer = self.register_x;
    }

    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn jump(&mut self, addr: u16) {
        self.program_counter = addr;
    }

    fn branch(&mut self) {
        let value = self.mem_read(self.program_counter);
        if value & 0b1000_0000 == 0 {
            self.program_counter = self.program_counter.wrapping_add(value as u16);
        } else {
            self.program_counter = self.program_counter.wrapping_add((value as u16) | 0xff00);
        }     
    }

    fn update_carry_flag(&mut self, result: u16) {
        if result & 0x100 == 0 {
            self.status = self.status & 0b1111_1110;
        } else {
            self.status = self.status | 0b0000_0001;
        }
    }

    fn update_overflow_flag(&mut self, result: u8, a: u8, b: u8) {
        if (!result & a & b & 0b1000_0000 == 0b1000_0000) || (result & !a & !b & 0b1000_0000 == 0b1000_0000) {
            self.status = self.status | 0b0100_0000;
        } else {
            self.status = self.status & 0b1011_1111;
        }
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            self.status = self.status | 0b0000_0010;
        } else {
            self.status = self.status & 0b1111_1101;
        }

        if result & 0b1000_0000 != 0 {
            self.status = self.status | 0b1000_0000;
        } else {
            self.status = self.status & 0b0111_1111;
        }
    }
}

