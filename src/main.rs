#[macro_use]
extern crate lazy_static;

extern crate sdl2;

pub mod cpu;
pub mod opcode;
pub mod bus;
pub mod rom;

use std::fs;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::Write;
use std::ops::Add;

use bus::Bus;
use cpu::AddressingMode;
use cpu::CPU;
use cpu::Mem;

use opcode::OpCode;
use opcode::CPU_OPS_LOOKUP;
use rand::Rng;
use sdl2::event::Event;
use sdl2::EventPump;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::pixels::PixelFormatEnum;

use crate::rom::Rom;

fn main() {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window("SNAKE", (32.0 * 10.0) as u32, (32.0 * 10.0) as u32)
        .position_centered()
        .build().unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    let mut event_pump = sdl_context.event_pump().unwrap();
    canvas.set_scale(10.0, 10.0).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 32, 32).unwrap();

    // let game_code = vec![0x20, 0x06, 0x06, 0x20, 0x38, 0x06, 0x20, 0x0d, 0x06, 0x20, 0x2a, 0x06, 0x60, 0xa9, 0x02, 0x85,
    // 0x02, 0xa9, 0x04, 0x85, 0x03, 0xa9, 0x11, 0x85, 0x10, 0xa9, 0x10, 0x85, 0x12, 0xa9, 0x0f, 0x85,
    // 0x14, 0xa9, 0x04, 0x85, 0x11, 0x85, 0x13, 0x85, 0x15, 0x60, 0xa5, 0xfe, 0x85, 0x00, 0xa5, 0xfe,
    // 0x29, 0x03, 0x18, 0x69, 0x02, 0x85, 0x01, 0x60, 0x20, 0x4d, 0x06, 0x20, 0x8d, 0x06, 0x20, 0xc3,
    // 0x06, 0x20, 0x19, 0x07, 0x20, 0x20, 0x07, 0x20, 0x2d, 0x07, 0x4c, 0x38, 0x06, 0xa5, 0xff, 0xc9,
    // 0x77, 0xf0, 0x0d, 0xc9, 0x64, 0xf0, 0x14, 0xc9, 0x73, 0xf0, 0x1b, 0xc9, 0x61, 0xf0, 0x22, 0x60,
    // 0xa9, 0x04, 0x24, 0x02, 0xd0, 0x26, 0xa9, 0x01, 0x85, 0x02, 0x60, 0xa9, 0x08, 0x24, 0x02, 0xd0,
    // 0x1b, 0xa9, 0x02, 0x85, 0x02, 0x60, 0xa9, 0x01, 0x24, 0x02, 0xd0, 0x10, 0xa9, 0x04, 0x85, 0x02,
    // 0x60, 0xa9, 0x02, 0x24, 0x02, 0xd0, 0x05, 0xa9, 0x08, 0x85, 0x02, 0x60, 0x60, 0x20, 0x94, 0x06,
    // 0x20, 0xa8, 0x06, 0x60, 0xa5, 0x00, 0xc5, 0x10, 0xd0, 0x0d, 0xa5, 0x01, 0xc5, 0x11, 0xd0, 0x07,
    // 0xe6, 0x03, 0xe6, 0x03, 0x20, 0x2a, 0x06, 0x60, 0xa2, 0x02, 0xb5, 0x10, 0xc5, 0x10, 0xd0, 0x06,
    // 0xb5, 0x11, 0xc5, 0x11, 0xf0, 0x09, 0xe8, 0xe8, 0xe4, 0x03, 0xf0, 0x06, 0x4c, 0xaa, 0x06, 0x4c,
    // 0x35, 0x07, 0x60, 0xa6, 0x03, 0xca, 0x8a, 0xb5, 0x10, 0x95, 0x12, 0xca, 0x10, 0xf9, 0xa5, 0x02,
    // 0x4a, 0xb0, 0x09, 0x4a, 0xb0, 0x19, 0x4a, 0xb0, 0x1f, 0x4a, 0xb0, 0x2f, 0xa5, 0x10, 0x38, 0xe9,
    // 0x20, 0x85, 0x10, 0x90, 0x01, 0x60, 0xc6, 0x11, 0xa9, 0x01, 0xc5, 0x11, 0xf0, 0x28, 0x60, 0xe6,
    // 0x10, 0xa9, 0x1f, 0x24, 0x10, 0xf0, 0x1f, 0x60, 0xa5, 0x10, 0x18, 0x69, 0x20, 0x85, 0x10, 0xb0,
    // 0x01, 0x60, 0xe6, 0x11, 0xa9, 0x06, 0xc5, 0x11, 0xf0, 0x0c, 0x60, 0xc6, 0x10, 0xa5, 0x10, 0x29,
    // 0x1f, 0xc9, 0x1f, 0xf0, 0x01, 0x60, 0x4c, 0x35, 0x07, 0xa0, 0x00, 0xa5, 0xfe, 0x91, 0x00, 0x60,
    // 0xa6, 0x03, 0xa9, 0x00, 0x81, 0x10, 0xa2, 0x00, 0xa9, 0x01, 0x81, 0x10, 0x60, 0xa2, 0x00, 0xea,
    // 0xea, 0xca, 0xd0, 0xfb, 0x60];
    
    let game_code = fs::read("nestest.nes").unwrap();

    println!("{:?}", game_code);
    println!("{}", game_code.len());

    let mut cpu = CPU::new(Bus::new(Rom::new(&game_code).unwrap()));
    cpu.reset();
    cpu.program_counter = 0xC000;

    // let mut screen_state = [0 as u8; 32 * 3 * 32];
    // let mut rng = rand::thread_rng();

    // cpu.run_with_callback(move |cpu| {
    //     handle_user_input(cpu, &mut event_pump);
    //     cpu.mem_write(0xfe, rng.gen_range(1, 16));

    //     if read_screen_state(cpu, &mut screen_state) {
            
    //         texture.update(None, &screen_state, 32 * 3).unwrap();
    //         canvas.copy(&texture, None, None).unwrap();
    //         canvas.present();
    //     }
    // });
    let mut f = OpenOptions::new()
        .append(true)
        .create(true) // Optionally create the file if it doesn't already exist
        .open("codetest.txt")
        .expect("Unable to open file");
    cpu.run_with_callback(|cpu| {
        f.write_all(format!("{}\n", trace(cpu)).as_bytes()).expect("could not write");
    });

}

fn read_screen_state(cpu: &CPU, frame: &mut [u8; 32 * 3 * 32]) -> bool {
    let mut frame_idx = 0;
    let mut update = false;
    for i in 0x200..0x600 {
        let color_idx = cpu.mem_read(i as u16);
        let (b1, b2, b3) = color(color_idx).rgb();
        if frame[frame_idx] != b1 || frame[frame_idx + 1] != b2 || frame[frame_idx + 2] != b3 {
            frame[frame_idx] = b1;
            frame[frame_idx+1] = b2;
            frame[frame_idx+2] = b3;
            update = true;
        }
        frame_idx += 3;
    }
    update
}

fn color(byte: u8) -> Color {
    match byte {
        0 => sdl2::pixels::Color::BLACK,
        1 => sdl2::pixels::Color::WHITE,
        2 | 9 => sdl2::pixels::Color::GREY,
        3 | 10 => sdl2::pixels::Color::RED,
        4 | 11 => sdl2::pixels::Color::GREEN,
        5 | 12 => sdl2::pixels::Color::BLUE,
        6 | 13 => sdl2::pixels::Color::MAGENTA,
        7 | 14 => sdl2::pixels::Color::YELLOW,
        _ => sdl2::pixels::Color::CYAN,
    }
}

fn handle_user_input(cpu: &mut CPU, event_pump: &mut EventPump) {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. } | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                std::process::exit(0)
            },
            Event::KeyDown { keycode: Some(Keycode::W), .. } => {
                cpu.mem_write(0xff, 0x77);
            },
            Event::KeyDown { keycode: Some(Keycode::S), .. } => {
                cpu.mem_write(0xff, 0x73);
            },
            Event::KeyDown { keycode: Some(Keycode::A), .. } => {
                cpu.mem_write(0xff, 0x61);
            },
            Event::KeyDown { keycode: Some(Keycode::D), .. } => {
                cpu.mem_write(0xff, 0x64);
            },
            _ => {}
        }
    }
}

fn trace(cpu: &mut CPU) -> String {
    let mut ret = String::new();

    //get program counter
    ret.push_str(&format!("{:04X}  ", cpu.program_counter));

    //get opcodes
    let opscode = cpu.mem_read(cpu.program_counter);
    let opscode: &OpCode = *CPU_OPS_LOOKUP.get(&opscode).unwrap();
    ret.push_str(&format!("{:02X} ", opscode.opcode));
    for i in 1..3 {
        if opscode.bytes > i  {
            ret.push_str(&format!("{:02X} ", cpu.mem_read(cpu.program_counter+i)));
        } else {
            ret.push_str("   ");
        }
    }
    ret.push_str(" ");

    //get assembly language
    ret.push_str(&format!("{} ", opscode.name));
    match opscode.add_mode {
        AddressingMode::Immediate => ret.push_str(&format!("#${:02X}", cpu.mem_read(cpu.program_counter+1))),

        AddressingMode::Accumulator => ret.push_str("A"),

        AddressingMode::Relative => {
            let value = cpu.mem_read(cpu.program_counter+1);
            if value & 0b1000_0000 == 0 {
                ret.push_str(&format!("${:04X}", (cpu.program_counter+opscode.bytes).wrapping_add(value as u16)));
            } else {
                ret.push_str(&format!("${:04X}", (cpu.program_counter+opscode.bytes).wrapping_add((value as u16) | 0xff00)));
            }
        }

        AddressingMode::ZeroPage => {
            let addr = cpu.mem_read(cpu.program_counter+1);
            ret.push_str(&format!("${:02X} = {:02X}", addr, cpu.mem_read(addr as u16)));
        }

        AddressingMode::Absolute => {
            let addr = cpu.mem_read_u16(cpu.program_counter+1);
            if opscode.name == "JSR" || opscode.name == "JMP" {
                ret.push_str(&format!("${:04X}", addr))
            } else {
                ret.push_str(&format!("${:04X} = {:02X}", addr, cpu.mem_read(addr)))
            }
        }
        ,

        AddressingMode::Indirect => {
            let addr = cpu.mem_read_u16(cpu.program_counter+1);
            
            let r = if addr & 0xFF == 0xFF {
                let lo = cpu.mem_read(addr);
                let hi = cpu.mem_read(addr - 0xFF);
                (hi as u16) << 8 | (lo as u16)
            } else {
                cpu.mem_read_u16(addr)
            };

            ret.push_str(&format!("(${:04X}) = {:04X}", addr, r));
        }

        AddressingMode::ZeroPage_X => {
            let base = cpu.mem_read(cpu.program_counter+1);

            let ptr: u8 = (base as u8).wrapping_add(cpu.register_x);

            ret.push_str(&format!("${:02X},X @ {:02X} = {:02X}", base, ptr, cpu.mem_read(ptr as u16)));
        }

        AddressingMode::ZeroPage_Y => {
            let base = cpu.mem_read(cpu.program_counter+1);

            let ptr: u8 = (base as u8).wrapping_add(cpu.register_y);

            ret.push_str(&format!("${:02X},Y @ {:02X} = {:02X}", base, ptr, cpu.mem_read(ptr as u16)));
        }

        AddressingMode::Absolute_X => {
            let base = cpu.mem_read_u16(cpu.program_counter+1);

            let ptr = (base).wrapping_add(cpu.register_x as u16);

            ret.push_str(&format!("${:04X},X @ {:04X} = {:02X}", base, ptr, cpu.mem_read(ptr)));
        }

        AddressingMode::Absolute_Y => {
            let base = cpu.mem_read_u16(cpu.program_counter+1);

            let ptr = (base).wrapping_add(cpu.register_y as u16);

            ret.push_str(&format!("${:04X},Y @ {:04X} = {:02X}", base, ptr, cpu.mem_read(ptr)));
        }

        AddressingMode::Indirect_X => {
            let base = cpu.mem_read(cpu.program_counter+1);

            let ptr: u8 = (base as u8).wrapping_add(cpu.register_x);

            let lo = cpu.mem_read(ptr as u16);
            let hi = cpu.mem_read(ptr.wrapping_add(1) as u16);
            let addr = (hi as u16) << 8 | (lo as u16);

            ret.push_str(&format!("(${:02X},X) @ {:02X} = {:04X} = {:02X}", base, ptr, addr, cpu.mem_read(addr)));
        }
        AddressingMode::Indirect_Y => {
            let base = cpu.mem_read(cpu.program_counter+1);

            let lo = cpu.mem_read(base as u16);
            let hi = cpu.mem_read((base as u8).wrapping_add(1) as u16);
            let deref_base = (hi as u16) << 8 | (lo as u16);

            let deref = deref_base.wrapping_add(cpu.register_y as u16);

            ret.push_str(&format!("(${:02X}),Y = {:04X} @ {:04X} = {:02X}", base, deref_base, deref, cpu.mem_read(deref)));
        }
        AddressingMode::NoneAddressing => (),
    }

    while ret.len() < 48 {
        ret.push_str(" ");
    }

    ret.push_str(&format!("A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}", cpu.register_a, cpu.register_x, cpu.register_y, cpu.status, cpu.stack_pointer));
    ret
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::bus::Bus;
    use crate::rom::Rom;

    #[test]
    fn test_out_trace() {
        let mut bus = Bus::new(Rom::new(&fs::read("snake.nes").unwrap()).unwrap());
        bus.mem_write(100, 0xa2);
        bus.mem_write(101, 0x01);
        bus.mem_write(102, 0x00);
        bus.mem_write(103, 0x88);
        bus.mem_write(104, 0x00);

        let mut cpu = CPU::new(bus);
        cpu.program_counter = 0x64;
        cpu.register_a = 1;
        cpu.register_x = 2;
        cpu.register_y = 3;
        let mut result: Vec<String> = vec![];
        cpu.run_with_callback(|cpu| {
            result.push(trace(cpu));
        });
        println!("{:?}", result);
    }

    #[test]
    fn test_format_trace() {
        let mut bus = Bus::new(Rom::new(&fs::read("snake.nes").unwrap()).unwrap());
        bus.mem_write(100, 0xa2);
        bus.mem_write(101, 0x01);
        bus.mem_write(102, 0xca);
        bus.mem_write(103, 0x88);
        bus.mem_write(104, 0x00);

        let mut cpu = CPU::new(bus);
        cpu.program_counter = 0x64;
        cpu.register_a = 1;
        cpu.register_x = 2;
        cpu.register_y = 3;
        let mut result: Vec<String> = vec![];
        cpu.run_with_callback(|cpu| {
            result.push(trace(cpu));
        });
        assert_eq!(
            "0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD",
            result[0]
        );
        assert_eq!(
            "0066  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD",
            result[1]
        );
        assert_eq!(
            "0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD",
            result[2]
        );
    }

    #[test]
    fn test_format_mem_access() {
        let mut bus = Bus::new(Rom::new(&fs::read("snake.nes").unwrap()).unwrap());
        // ORA ($33), Y
        bus.mem_write(100, 0x11);
        bus.mem_write(101, 0x33);


        //data
        bus.mem_write(0x33, 00);
        bus.mem_write(0x34, 04);

        //target cell
        bus.mem_write(0x400, 0xAA);

        let mut cpu = CPU::new(bus);
        cpu.program_counter = 0x64;
        cpu.register_y = 0;
        let mut result: Vec<String> = vec![];
        cpu.run_with_callback(|cpu| {
            result.push(trace(cpu));
        });
        assert_eq!(
            "0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD",
            result[0]
        );
    }
}


// #[cfg(test)]
// mod test {
//     use super::*;

//     #[test]
//     fn test_0xa9_lda_immediate_load_data() {
//         let mut cpu = CPU::new();
//         cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
//         assert_eq!(cpu.register_a, 0x05);
//         assert!(cpu.status & 0b0000_0010 == 0);
//         assert!(cpu.status & 0b1000_0000 == 0);
//     }

//     #[test]
//     fn test_0xa9_lda_zero_flag() {
//         let mut cpu = CPU::new();
//         cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
//         assert!(cpu.status & 0b0000_0010 == 0b10);
//     }

//     #[test]
//     fn test_lda_from_memory() {
//         let mut cpu = CPU::new();
//         cpu.mem_write(0x10, 0x55);

//         cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

//         assert_eq!(cpu.register_a, 0x55);
//     }

//     #[test]
//     fn test_0xaa_tax_move_a_to_x() {
//         let mut cpu = CPU::new();

//         cpu.load_and_run(vec![0xa9, 0x0a, 0xaa, 0x00]);

//         assert_eq!(cpu.register_x, 10)
//     }
//     #[test]
//     fn test_5_ops_working_together() {
//         let mut cpu = CPU::new();
//         cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

//         assert_eq!(cpu.register_x, 0xc1)
//     }

//     #[test]
//     fn test_inx_overflow() {
//         let mut cpu = CPU::new();

//         cpu.load_and_run(vec![0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00]);

//         assert_eq!(cpu.register_x, 1)
//     }

//     #[test]
//     fn test_adc_normal() {
//         let mut cpu = CPU::new();

//         cpu.load_and_run(vec![0x69, 0x54, 0x00]);

//         assert_eq!(cpu.register_a, 84);
//         assert!(cpu.status & 0b0100_0000 == 0);
//     }

//     #[test]
//     fn test_adc_overflow() {
//         let mut cpu = CPU::new();

//         cpu.load_and_run(vec![0x69, 0x94, 0x69, 0x94, 0x00]);

//         assert!(cpu.status & 0b0100_0000 == 0b0100_0000);
//         assert!(cpu.status & 1 == 1);
//     }

//     #[test]
//     fn test_and() {
//         let mut cpu = CPU::new();

//         cpu.load_and_run(vec![0xa9, 0b1110_1110, 0x29, 0b1100_0011, 0x00]);
//         assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
//         assert_eq!(cpu.register_a, 0b1100_0010);
//     }

//     #[test]
//     fn test_asl_register_a() {
//         let mut cpu = CPU::new();

//         cpu.load_and_run(vec![0xa9, 0b1110_1110, 0x0a, 0x00]);

//         assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
//         assert!(cpu.status & 0b0000_0001 == 1);
//         assert_eq!(cpu.register_a, 0b1101_1100);
//     }

//     #[test]
//     fn test_asl_mem() {
//         let mut cpu = CPU::new();
//         cpu.mem_write(0x10, 0b0001_1010);

//         cpu.load_and_run(vec![0x06, 0x10, 0x00]);

//         assert!(cpu.status & 0b1000_0000 == 0);
//         assert!(cpu.status & 0b0000_0001 == 0);
//         assert_eq!(cpu.mem_read(0x10), 0b0011_0100);
//     }

//     #[test]
//     fn test_bit() {
//         let mut cpu = CPU::new();
//         cpu.mem_write(0x10, 0b0101_1010);

//         cpu.load_and_run(vec![0xa9, 0b1010_0000, 0x24, 0x10, 0x00]);
//         assert_eq!(cpu.status, 0b0100_0010);
//     }

//     #[test]
//     fn test_cmp() {
//         let mut cpu = CPU::new();
//         cpu.mem_write(0x10, 0b1000_0000);
//         cpu.load_and_run(vec![0xa9, 0b0010_0000, 0xc5, 0x10, 0x00]);

//         assert_eq!(cpu.status, 0b1000_0000);
//     }

//     #[test]
//     fn test_dec() {
//         let mut cpu = CPU::new();
//         cpu.mem_write(0x10, 0b0000_0000);
//         cpu.load_and_run(vec![0xc6, 0x10, 0x00]);

//         assert_eq!(cpu.mem_read(0x10), 0b1111_1111);
//         assert!(cpu.status == 0b1000_0000);
//     }

//     #[test]
//     fn test_jmp() {
//         let mut cpu = CPU::new();
//         cpu.mem_write(0x10, 0b0000_1111);
//         cpu.load_and_run(vec![0xa9, 0x56, 0x4c, 0x07, 0x80, 0x25, 0x10, 0x65, 0x10, 0x00]);

//         assert_eq!(cpu.register_a, 0b0110_0101);   
//     }

//     #[test]
//     fn test_indir_jmp() {
//         let mut cpu = CPU::new();
//         cpu.mem_write(0x10, 0b0000_1111);
//         cpu.mem_write_u16(0x12, 0x8007);
//         cpu.load_and_run(vec![0xa9, 0x56, 0x6c, 0x12, 0x00, 0x25, 0x10, 0x65, 0x10, 0x00]);

//         assert_eq!(cpu.register_a, 0b0110_0101);   
//     }

//     #[test]
//     fn test_lsr() {
//         let mut cpu = CPU::new();
//         cpu.load_and_run(vec![0xa9, 0x01, 0x4a, 0x00]);

//         assert_eq!(cpu.register_a, 0);
//         assert_eq!(cpu.status, 0b0000_0011);
//     }

//     #[test]
//     fn test_rol() {
//         let mut cpu = CPU::new();
//         cpu.load_and_run(vec![0xa9, 0b1011_0001, 0x2a, 0x2a, 0x00]);

//         assert_eq!(cpu.register_a, 0b1100_0110);
//         assert_eq!(cpu.status, 0b1000_0000);
//     }

//     #[test]
//     fn test_ror() {
//         let mut cpu = CPU::new();
//         cpu.load_and_run(vec![0xa9, 0b1011_0001, 0x6a, 0x6a, 0x00]);

//         assert_eq!(cpu.register_a, 0b0110_1100);
//         assert_eq!(cpu.status, 0b0000_0000);
//     }

//     #[test]
//     fn test_pha() {
//         let mut cpu = CPU::new();
//         cpu.load_and_run(vec![0xa9, 0x30, 0x48, 0xa9, 0x80, 0x68, 0x00]);

//         assert_eq!(cpu.register_a, 0x30);
//     }
// }