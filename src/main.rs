// #![feature(slice_rotate)]
// #![feature(iterator_step_by)]

extern crate clap;
use clap::{Arg,App};
use std::fs::File;
// use std::io::prelude::*;
// use std::char::{decode_utf16, REPLACEMENT_CHARACTER};
use std::io::{self, Read};

struct Process {
	pc : u16,
	program : Vec<u16>,
	registers : [u16; 8],
	stack : Vec<u16>,
	_verbosity : u32,
}

impl Process {

	fn read_short(&mut self) -> u16 {
		let result = self.program[self.pc as usize] as u16;
		self.pc += 1;
		result
	}

	fn to_register(val : u16) -> u16 {
		val-32768
	}

	fn get_value(&self, val : u16) -> u16 {
		match val {
			0...32767 => val,
			32768...32775 => self.registers[Process::to_register(val) as usize],
			_ => panic!("Unexpected value: {}", val),
		}
	}

	fn read_value(&mut self) -> u16 {
		let val = self.read_short();
		let tmp = self.get_value(val);
		if tmp >= 32768 { panic!("Invalid number (too high): {}", tmp); }
		return tmp;
	}

	fn read_register(&mut self) -> u16 {
		Process::to_register(self.read_short())
	}

	fn set_register(&mut self, reg : u16, val : u16) {
		if val >= 32768 { panic!("Invalid number (too high): {}", val); }
		self.registers[reg as usize] = val % 32768;
	}

	fn run_next_instruction(&mut self, stdin : &mut io::StdinLock) -> bool {
		if self.pc as usize >= self.program.len() {
			return false;
		}
		let opcode = self.read_short();
		if self._verbosity > 1 { println!("PC={:x}, op={} ({:x}, {:x}, {:x}) Stack={{{}}} {}", self.pc-1, opcode, self.program[self.pc as usize], self.program[(self.pc+1) as usize], self.program[(self.pc+2) as usize],
			self.stack.iter().map(|i| format!("{:x}", i)).collect::<Vec<String>>().join(", "),
			(0..8).map(|i| format!("{}={:x}", i, self.registers[i])).collect::<Vec<String>>().join(", "));}
		match opcode {
			0 => { // halt
				println!("Halt instruction at PC={}", self.pc);
				return false;
			}
			1 => { // set
				let dest_reg = self.read_register();
				let val = self.read_value();
				if val >= 32768 { panic!("Invalid number (too high): {}", val)}
				self.set_register(dest_reg, val);
			}
			2 => { // push
				let val = self.read_value();
				self.stack.push(val);
			}
			3 => { // pop
				let dest_reg = self.read_register();
				let stack_val = self.stack.pop().unwrap();
				self.set_register(dest_reg, stack_val);
			}
			4 => { // eq
				let dest_reg = self.read_register();
				let val_b = self.read_value();
				let val_c = self.read_value();
				self.set_register(dest_reg, if val_b == val_c { 1 } else { 0 });
			}
			5 => { // gt
				let dest_reg = self.read_register();
				let val_b = self.read_value();
				let val_c = self.read_value();
				self.set_register(dest_reg, if val_b > val_c { 1 } else { 0 });
			}
			6 => { // jump
				let dest = self.read_value();
				self.pc = dest;
			}
			7 => { // jt (jnz)
				let val = self.read_value();
				let addr = self.read_value();
				if val != 0 {
					self.pc = addr;
				}
			}
			8 => { // jf (jz)
				let val = self.read_value();
				let addr = self.read_value();
				if val == 0 {
					self.pc = addr;
				}
			}
			9 | 10 | 11 => { // add / mul / mod
				let dest_reg = self.read_register();
				let op1 = self.read_value();
				let op2 = self.read_value();
				// if self._verbosity >= 2 { println!("ADD/MUL dest_reg = {} {}", op1, op2);}
				self.set_register(dest_reg, match opcode {
					9 => (op1 + op2) % 32768, //add
					10 => ((op1 as u32 * op2 as u32) % 32768) as u16, //mul
					11 => op1 % op2, //mod
					_ => panic!(""),});
			}
			12 | 13 => { // and/or
				let dest_reg = self.read_register();
				let op1 = self.read_value();
				let op2 = self.read_value();
				self.set_register(dest_reg, match opcode {
					12 => op1 & op2, // and
					13 => op1 | op2, // or
					_ => panic!("")});
			}
			14 => { // not
				let dest_reg = self.read_register();
				let val = self.read_value();
				self.set_register(dest_reg, val ^ 0x7FFF);
			}
			15 => { // rmem
				let dest_reg = self.read_register();
				let addr = self.read_value();
				let addr_val = self.program[addr as usize];
				self.set_register(dest_reg, addr_val);
			}
			16 => { // wmem
				let addr = self.read_value();
				let val = self.read_value();
				self.program[addr as usize] = val;
			}
			17 => { // call
				let addr = self.read_value();
				self.stack.push(self.pc);
				self.pc = addr;
			}
			18 => { // ret
				if let Some(addr) = self.stack.pop() {
					self.pc = addr;
				} else {
					println!("Ret with no stack. Halting.");
					return false; //halt
				}
			}
			19 => { // out
				let ch = self.read_value() as u8;
				if ch == 0 || ch > 128 {
					println!("Writing invalid character ({:x})...halting.", ch as u32);
					return false;
				}
				// let u16s = [ch];
				// let s = decode_utf16(u16s.iter().cloned())
				// 			.map(|r| r.unwrap_or(REPLACEMENT_CHARACTER))
				// 			.collect::<String>();
				print!("{}", ch as char);
				// print!("{}", s);
			}
			20 => { // in
				let dest_reg = self.read_register();
				let mut buffer = [0; 1];
				stdin.read(&mut buffer[..]).unwrap();
				self.set_register(dest_reg, buffer[0] as u16);
			}
			21 => (), //no-op
			_ => panic!("Unhandled opcode '{}'", opcode),
		}
		true
	}
}

// Convert Vec<u8> to Vec<u16> (via unsafe)
fn convert_vec_u8_to_u16(mut input : Vec<u8>) -> Vec<u16> {
	input.shrink_to_fit();
	let len = input.len() / 2;

	let result;
	unsafe {
		result = Vec::from_raw_parts(input.as_mut_ptr() as *mut u16, len, len);
		std::mem::forget(input);
	}
	result
}

fn main() {
	let matches = App::new("Synacore Challenge")
		.author("Galen Elias, gelias@gmail.com")
		.version("0.1.0")
		.about("Synacore Challenge - Solutions in Rust")
		.arg(
			Arg::with_name("verbosity")
				.short("v")
				.required(false)
				.index(1)
				.help("specifies verbosity"))
		.after_help("Longer explaination to appear after the options when \
					displaying the help information from --help or -h")
		.get_matches();

	let mut file=File::open("challenge.bin").unwrap();
	let mut buffer_u8 = Vec::new();

	// read the whole file
	file.read_to_end(&mut buffer_u8).unwrap();
	let mut buffer_u16 = convert_vec_u8_to_u16(buffer_u8);

	println!("Length (post): {}", buffer_u16.len());
	buffer_u16.resize(0x8000, 0);

	let mut app = Process{pc : 0, program : buffer_u16, registers : [0u16; 8], stack : Vec::new(),_verbosity : matches.value_of("verbosity").unwrap_or("0").parse::<u32>().unwrap() };

	let stdin = io::stdin();
	let mut handle = stdin.lock();

	while app.run_next_instruction(&mut handle) {

	}
}
