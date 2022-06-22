use crate::opcode::OpCode;
use crate::opcode::CPU_OPS_LOOKUP;

use crate::bus::Bus;


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

pub struct CPU<'a> {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub stack_pointer: u8,
    pub status: u8,
    pub program_counter: u16,
    pub bus: Bus<'a>,
}

pub trait Mem {
    fn mem_read(&mut self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, data: u8);
    fn mem_read_u16(&mut self, pos: u16) -> u16;
    fn mem_write_u16(&mut self, pos: u16, data: u16);
}

impl<'a> Mem for CPU<'a> {
    fn mem_read(&mut self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.mem_write(addr, data);
    }

    fn mem_read_u16(&mut self, pos: u16) -> u16 {
        self.bus.mem_read_u16(pos)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        self.bus.mem_write_u16(pos, data);
    }

}

impl<'a> CPU<'a> {
    pub fn new(bus: Bus<'a>) -> Self {
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

    fn interrupt_nmi(&mut self) {
        self.push_stack_u16(self.program_counter);
        self.php();

        self.status = self.status | 0b0000_0100;
        self.bus.tick(2);
        self.program_counter = self.mem_read_u16(0xfffa);
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut CPU),
    {
        loop {
            
            if let Some(_nmi) = self.bus.poll_nmi_status() {
                self.interrupt_nmi();
            }
            callback(self);
            let opscode = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let opscode: &OpCode = *CPU_OPS_LOOKUP.get(&opscode).unwrap();

            let op = opscode.name;

            // println!("{}, {:#x}\n", op, opscode.opcode);
            let cyc = match op {
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

                "NOP" => (0),

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

                "ALR" => self.alr(),

                "ANC" => self.anc(),

                "ANE" => self.ane(),

                "ARR" => self.arr(),

                "DCP" => self.dcp(&opscode.add_mode),

                "ISB" => self.isc(&opscode.add_mode),

                "LAS" => self.las(),

                "LAX" => self.lax(&opscode.add_mode),

                "LXA" => self.lxa(),

                "RLA" => self.rla(&opscode.add_mode),

                "RRA" => self.rra(&opscode.add_mode),

                "SAX" => self.sax(&opscode.add_mode),

                "SBX" => self.sbx(),

                "SHA" => self.sha(&opscode.add_mode),

                "SHX" => self.shx(),

                "SHY" => self.shy(),

                "SLO" => self.slo(&opscode.add_mode),

                "SRE" => self.sre(&opscode.add_mode),

                "TAS" => self.tas(),

                _ => todo!(),
            };

            self.bus.tick(opscode.cycles + (cyc as u8));

            self.program_counter += opscode.bytes-1;
        }
    }

    fn get_operand_address(&mut self, mode: &AddressingMode) -> (u16, usize) {
        match mode {
            AddressingMode::Immediate => (self.program_counter, 0),

            AddressingMode::ZeroPage => (self.mem_read(self.program_counter) as u16, 0),

            AddressingMode::Absolute => (self.mem_read_u16(self.program_counter), 0),

            AddressingMode::Indirect => {
                let addr = self.mem_read_u16(self.program_counter);
                if addr & 0xFF == 0xFF {
                    let lo = self.mem_read(addr);
                    let hi = self.mem_read(addr - 0xFF);
                    ((hi as u16) << 8 | (lo as u16), 0)
                } else {
                    (self.mem_read_u16(addr), 0)
                }
            }

            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                (addr, 0)
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                (addr, 0)
            }

            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_x as u16);
                if (base & 0xFF00) == (addr & 0xFF00) {
                    (addr, 0)
                } else {
                    (addr, 1)
                }
                
            }
            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_y as u16);
                if (base & 0xFF00) == (addr & 0xFF00) {
                    (addr, 0)
                } else {
                    (addr, 1)
                }
            }

            AddressingMode::Indirect_X => {
                let base = self.mem_read(self.program_counter);

                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                ((hi as u16) << 8 | (lo as u16), 0)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);

                let lo = self.mem_read(base as u16);
                let hi = self.mem_read((base as u8).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                if (deref_base & 0xFF00) == (deref & 0xFF00) {
                    (deref, 0)
                } else {
                    (deref, 1)
                }
            }
            AddressingMode::NoneAddressing | AddressingMode::Accumulator | AddressingMode::Relative => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }

    fn adc(&mut self, mode: &AddressingMode) -> usize {
        let (addr, cyc) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let total = (self.register_a as u16) + (value as u16) + (1 & self.status as u16);
        
        self.update_carry_flag(total);
        let total = total as u8;
        self.update_overflow_flag(total, self.register_a, value);
        self.update_zero_and_negative_flags(total);

        self.register_a = total;

        cyc
    }

    fn and(&mut self, mode: &AddressingMode) -> usize {
        let (addr, cyc) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = self.register_a & value;
        self.update_zero_and_negative_flags(self.register_a);

        cyc
    }

    fn asl(&mut self, mode: &AddressingMode) -> usize {
        let value;
        if let AddressingMode::Accumulator = mode {
            value = (self.register_a as u16) << 1;
            self.register_a = value as u8;
        } else {
            let (addr, _) = self.get_operand_address(mode);
            value = (self.mem_read(addr) as u16) << 1;
            self.mem_write(addr, value as u8);
        }
        self.update_carry_flag(value);
        self.update_zero_and_negative_flags(value as u8);

        0
    }

    fn bcc(&mut self) -> usize {
        if self.status & 1 == 0 {
            self.branch()
        } else {
            0
        }
        
    }

    fn bcs(&mut self) -> usize {
        if self.status & 1 == 1 {
            self.branch()
        } else {
            0
        }
        
    }
    
    fn beq(&mut self) -> usize {
        if self.status & 0b0000_0010 == 0b0000_0010 {
            self.branch()
        } else {
            0
        }
    }

    fn bit(&mut self, mode: &AddressingMode) -> usize {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = self.register_a & value;
        self.update_zero_and_negative_flags(result);
        self.status = (self.status & 0b0011_1111) | (value & 0b1100_0000);

        0
    }

    fn bmi(&mut self) -> usize {
        if self.status & 0b1000_0000 == 0b1000_0000 {
            self.branch()
        } else {
            0
        }
    }

    fn bne(&mut self) -> usize {
        if self.status & 0b10 == 0 {
            self.branch()
        } else {
            0
        }
    }

    fn bpl(&mut self) -> usize {
        if self.status & 0b1000_0000 == 0 {
            self.branch()
        } else {
            0
        }
    }

    fn bvc(&mut self) -> usize {
        if self.status & 0b0100_0000 == 0 {
            self.branch()
        } else {
            0
        }
    }

    fn bvs(&mut self) -> usize {
        if self.status & 0b0100_0000 == 0b0100_0000 {
            self.branch()
        } else {
            0
        }
    }

    fn clc(&mut self) -> usize {
        self.status = self.status & 0b1111_1110;

        0
    }

    fn cld(&mut self) -> usize {
        self.status = self.status & 0b1111_0111;

        0
    }

    fn cli(&mut self) -> usize {
        self.status = self.status & 0b1111_1011;

        0
    }

    fn clv(&mut self) -> usize {
        self.status = self.status & 0b1011_1111;

        0
    }

    fn cmp(&mut self, mode: &AddressingMode) -> usize {
        let (addr, cyc) = self.get_operand_address(mode);
        let value = self.mem_read(addr);
       

        if value <= self.register_a {
            self.status = self.status | 1;
        } else {
            self.status = self.status & !(1 as u8);
        }

        let value = (!value).wrapping_add(1);

        let result = (self.register_a).wrapping_add(value);
        
        self.update_zero_and_negative_flags(result);

        cyc
    }

    fn cpx(&mut self, mode: &AddressingMode) -> usize {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);
       

        if value <= self.register_x {
            self.status = self.status | 1;
        } else {
            self.status = self.status & !(1 as u8);
        }

        let value = (!value).wrapping_add(1);

        let result = (self.register_x).wrapping_add(value);
        
        self.update_zero_and_negative_flags(result);

        0
    }

    fn cpy(&mut self, mode: &AddressingMode) -> usize {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);
       

        if value <= self.register_y {
            self.status = self.status | 1;
        } else {
            self.status = self.status & !(1 as u8);
        }

        let value = (!value).wrapping_add(1);

        let result = (self.register_y).wrapping_add(value);
        
        self.update_zero_and_negative_flags(result);

        0
    }

    fn dec(&mut self, mode: &AddressingMode) -> usize {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        
        let result = value.wrapping_sub(1);
        self.mem_write(addr, result);

        self.update_zero_and_negative_flags(result);

        0
    }

    fn dex(&mut self) -> usize {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);

        0
    }

    fn dey(&mut self) -> usize {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);

        0
    }

    fn eor(&mut self, mode: &AddressingMode) -> usize {
        let (addr, cyc) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = self.register_a ^ value;
        self.update_zero_and_negative_flags(self.register_a);

        cyc
    }

    fn inc(&mut self, mode: &AddressingMode) -> usize {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = value.wrapping_add(1);
        self.mem_write(addr, result);
        self.update_zero_and_negative_flags(result);

        0
    }

    fn inx(&mut self) -> usize {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);

        0
    }

    fn iny(&mut self) -> usize {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);

        0
    }

    fn jmp(&mut self, mode: &AddressingMode) -> usize {
        let (addr, _) = self.get_operand_address(mode);
        self.jump(addr.wrapping_sub(2));

        0
    }

    fn jsr(&mut self) -> usize {
        let (addr, _) = self.get_operand_address(&AddressingMode::Absolute);
        self.push_stack_u16(self.program_counter+1);
        self.jump(addr.wrapping_sub(2));

        0
    }

    fn lda(&mut self, mode: &AddressingMode) -> usize {
        let (addr, cyc) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);

        cyc
    }

    fn ldx(&mut self, mode: &AddressingMode) -> usize {
        let (addr, cyc) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);

        cyc
    }

    fn ldy(&mut self, mode: &AddressingMode) -> usize {
        let (addr, cyc) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);

        cyc
    }

    fn lsr(&mut self, mode: &AddressingMode) -> usize {
        let value;
        let carry;
        if let AddressingMode::Accumulator = mode {
            carry = self.register_a & 1;
            value = (self.register_a) >> 1;
            self.register_a = value;
        } else {
            let (addr, _) = self.get_operand_address(mode);
            let num = self.mem_read(addr);
            carry = num & 1;
            value = (self.mem_read(addr)) >> 1;
            self.mem_write(addr, value);
        }
        self.update_carry_flag(((carry as u16) << 8 )| (value as u16));
        self.update_zero_and_negative_flags(value);

        0
    }

    fn ora(&mut self, mode: &AddressingMode) -> usize {
        let (addr, cyc) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = self.register_a | value;
        self.update_zero_and_negative_flags(self.register_a);

        cyc
    }

    fn pha(&mut self) -> usize {
        self.push_stack(self.register_a);

        0
    }

    fn php(&mut self) -> usize {
        self.push_stack(self.status | 0b0011_0000);

        0
    }

    fn pla(&mut self) -> usize {
        self.register_a = self.pop_stack();
        self.update_zero_and_negative_flags(self.register_a);

        0
    }

    fn plp(&mut self) -> usize {
        self.status = (self.pop_stack() & 0b1110_1111) | 0b0010_0000;

        0
    }

    fn rol(&mut self, mode: &AddressingMode) -> usize {
        let value;
        let carry = self.status & 1;
        if let AddressingMode::Accumulator = mode {
            self.update_carry_flag((self.register_a as u16) << 1);
            self.register_a = (self.register_a << 1) | (carry);
            value = self.register_a;
        } else {
            let (addr, _) = self.get_operand_address(mode);
            let v = self.mem_read(addr);
            self.update_carry_flag((v as u16) << 1);
            value = (v << 1) | (carry);
            self.mem_write(addr, value);
        }
        self.update_zero_and_negative_flags(value);

        0
    }

    fn ror(&mut self, mode: &AddressingMode) -> usize {
        let value;
        let u;
        if let AddressingMode::Accumulator = mode {
            u = ((self.register_a & 1) as u16) << 8;
            self.register_a = (self.register_a >> 1) | ((self.status & 1) << 7);
            
            value = self.register_a;
        } else {
            let (addr, _) = self.get_operand_address(mode);
            let v = self.mem_read(addr);
            u = ((v & 1) as u16) << 8;
            value = (v >> 1) | ((self.status & 1) << 7);
            self.mem_write(addr, value);
        }
        self.update_carry_flag(u);
        self.update_zero_and_negative_flags(value);

        0
    }

    fn rti(&mut self) -> usize {
        self.plp();
        self.program_counter = self.pop_stack_u16();

        0
    }

    fn rts(&mut self) -> usize {
        self.program_counter = self.pop_stack_u16() + 1;

        0
    }

    fn sbc(&mut self, mode: &AddressingMode) -> usize {
        let (addr, cyc) = self.get_operand_address(mode);
        let value = !self.mem_read(addr);

        let total = (self.register_a as u16) + (value as u16) + (1 & self.status as u16);
        
        self.update_carry_flag(total);
        let total = total as u8;
        self.update_overflow_flag(total, self.register_a, value);
        self.update_zero_and_negative_flags(total);

        self.register_a = total;

        cyc
    }

    fn sec(&mut self) -> usize {
        self.status = self.status | 1;

        0
    }

    fn sed(&mut self) -> usize {
        self.status = self.status | 0b1000;

        0
    }

    fn sei(&mut self) -> usize {
        self.status = self.status | 0b100;

        0
    }

    fn sta(&mut self, mode: &AddressingMode) -> usize {
        let (addr, _) = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);

        0
    }

    fn stx(&mut self, mode: &AddressingMode) -> usize {
        let (addr, _) = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);

        0
    }

    fn sty(&mut self, mode: &AddressingMode) -> usize {
        let (addr, _) = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);

        0
    }

    fn tax(&mut self) -> usize {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);

        0
    }

    fn tay(&mut self) -> usize {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);

        0
    }

    fn tsx(&mut self) -> usize {
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_x);

        0
    }

    fn txa(&mut self) -> usize {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);

        0
    }

    fn txs(&mut self) -> usize {
        self.stack_pointer = self.register_x;

        0
    }

    fn tya(&mut self) -> usize {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);

        0
    }

    //illegal opcodes
    fn alr(&mut self) -> usize {
        self.and(&AddressingMode::Immediate);
        self.lsr(&AddressingMode::Accumulator);

        0
    }

    fn anc(&mut self) -> usize {
        self.and(&AddressingMode::Immediate);
        self.set_carry_flag(self.register_a >> 7);

        0
    }

    fn ane(&mut self) -> usize {
        self.register_a = self.register_a & self.register_x;
        self.and(&AddressingMode::Immediate);

        0
    }

    fn arr(&mut self) -> usize {
        let op = self.mem_read(self.program_counter);
        self.and(&AddressingMode::Immediate);
        let result = op.wrapping_add(self.register_a);
        self.update_overflow_flag(result, self.register_a, op);
        self.ror(&AddressingMode::Accumulator);

        0
    }

    fn dcp(&mut self, mode: &AddressingMode) -> usize {
        self.dec(mode);
        self.cmp(mode);

        0
    }

    fn isc(&mut self, mode: &AddressingMode) -> usize {
        self.inc(mode);
        self.sbc(mode);

        0
    }

    fn las(&mut self) -> usize {
        let (addr, cyc) = self.get_operand_address(&AddressingMode::Absolute_Y);
        let value = self.mem_read(addr);
        self.register_a = value & self.stack_pointer;
        self.register_x = self.register_a;
        self.stack_pointer = self.register_a;

        cyc
    }

    fn lax(&mut self, mode: &AddressingMode) -> usize {
        let cyc = self.lda(mode);
        self.ldx(mode);

        cyc
    }

    fn lxa(&mut self) -> usize {
        self.and(&AddressingMode::Immediate);
        self.register_x = self.register_a;

        0
    }

    fn rla(&mut self, mode: &AddressingMode) -> usize {
        self.rol(mode);
        self.and(mode);

        0
    }

    fn rra(&mut self, mode: &AddressingMode) -> usize {
        self.ror(mode);
        self.adc(mode);

        0
    }

    fn sax(&mut self, mode: &AddressingMode) -> usize {
        let (addr, _) = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a & self.register_x);

        0
    }

    fn sbx(&mut self) -> usize {
        let value = self.mem_read(self.program_counter);
        self.register_x = self.register_a & self.register_x;
        self.cpx(&AddressingMode::Immediate);
        self.register_x = self.register_x.wrapping_sub(value);

        0
    }

    fn sha(&mut self, mode: &AddressingMode) -> usize {
        let (addr, _) = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a & self.register_x & ((addr >> 8) as u8).wrapping_add(1));
    
        0
    }

    fn shx(&mut self) -> usize {
        let (addr, _) = self.get_operand_address(&AddressingMode::Absolute_Y);
        self.mem_write(addr, self.register_x & ((addr >> 8) as u8).wrapping_add(1));

        0
    }

    fn shy(&mut self) -> usize {
        let (addr, _) = self.get_operand_address(&AddressingMode::Absolute_X);
        self.mem_write(addr, self.register_y & ((addr >> 8) as u8).wrapping_add(1));

        0
    }

    fn slo(&mut self, mode: &AddressingMode) -> usize {
        self.asl(mode);
        self.ora(mode);

        0
    }

    fn sre(&mut self, mode: &AddressingMode) -> usize {
        self.lsr(mode);
        self.eor(mode);

        0
    }

    fn tas(&mut self) -> usize {
        self.stack_pointer = self.register_a & self.register_x;
        self.shx();
        
        0
    }

    fn jump(&mut self, addr: u16) {
        self.program_counter = addr;
    }

    fn branch(&mut self) -> usize {
        let value = self.mem_read(self.program_counter);
        let cur = self.program_counter;
        if value & 0b1000_0000 == 0 {
            self.program_counter = self.program_counter.wrapping_add(value as u16);
        } else {
            self.program_counter = self.program_counter.wrapping_add((value as u16) | 0xff00);
        }
        
        if (cur & 0xFF00) == (self.program_counter & 0xFF00) {
            1
        } else {
            2
        }
    }

    fn update_carry_flag(&mut self, result: u16) {
        if result & 0x100 == 0 {
            self.status = self.status & 0b1111_1110;
        } else {
            self.status = self.status | 0b0000_0001;
        }
    }

    fn set_carry_flag(&mut self, carry: u8) {
        if carry == 0 {
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

