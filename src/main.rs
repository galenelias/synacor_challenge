extern crate clap;
extern crate data_encoding;

use clap::{Arg,App};
use std::fs::File;
use std::io::{self, Read};
use std::io::prelude::*;
use std::io::BufReader;
use data_encoding::BASE64;
use std::collections::VecDeque;

struct Process {
	pc : u16,
	memory : Vec<u16>,
	registers : [u16; 8],
	stack : Vec<u16>,
	_verbosity : u32,
}

impl Process {

	fn read_short(&mut self) -> u16 {
		let result = self.memory[self.pc as usize] as u16;
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

	fn get_friendly_arg(&self, pc : usize, i : usize, count : usize) -> String {
		if i >= count { return "".to_string(); }
		let mem = self.memory[pc as usize + 1 + i];
		if mem >= 32768 { (('a' as u8 + (mem - 32768) as u8) as char).to_string() } else { format!("{:04x}", mem) }
	}

	fn opcode_param_count(opcode : u16) -> usize {
		match opcode {
				0 | 18 | 21 => 0,
				2 | 3 | 6 | 17 | 19 | 20 => 1,
				1 | 7 | 8 | 14 | 15 | 16 => 2,
				4 | 5 | 9 | 10 | 11 | 12 | 13=> 3,
				_ => panic!("Unexpected opcode"),
			}	
	}

	fn disassemble(&self, pc : usize) {
		let opcode = self.memory[pc as usize] as u16;
		let opcode_friendly = match opcode {
			0 => "halt", 1 => "set", 2 => "push", 3 => "pop", 4 => "eq",
			5 => "gt", 6 => "jmp", 7 => "jt", 8 => "jf", 9 => "add",
			10 => "add", 11 => "mult", 12 => "mod", 13 => "and", 14 => "not",
			15 => "rmem", 16 => "wmem", 17 => "call", 18 => "ret", 19 => "out",
			20 => "in", 21 => "nop", _ => panic!("Unexpected opcode"),
		};

		let param_count = Process::opcode_param_count(opcode);
		println!("{:04x}: {:04x} {:04x} {:04x} {:04x}    {} {} {} {}", pc, self.memory[pc as usize + 0], self.memory[pc as usize + 1], self.memory[pc as usize + 2], self.memory[pc as usize + 3], 
			opcode_friendly, self.get_friendly_arg(pc, 0, param_count), self.get_friendly_arg(pc, 1, param_count), self.get_friendly_arg(pc, 2, param_count));
	}

	fn dump_disassembly(&self) {
		let mut pc = 0;
		while pc < self.memory.len() {
			if self.memory[pc] < 22 {
				self.disassemble(pc);
				pc += 1 + Process::opcode_param_count(self.memory[pc]);
			} else {
				pc += 1;
			}
		}
	}

	fn run_next_instruction(&mut self, stdin : &mut io::StdinLock) -> bool {
		if self.pc as usize >= self.memory.len() {
			return false;
		}

		// Puzzle 7
		if self.pc == 0x1571 {
			self.pc += 2;
			// Next instruction is now a 'eq b a <val>' with our expected result.  Extract this result
			let target_result = self.memory[self.pc as usize + 3];
			println!("Skipping over expensive function at 0x{:04x}, manually computing result of super_recursive({},{})={}", self.pc, self.registers[0], self.registers[1], target_result);
			self.registers[7] = solve_for_register_7(target_result);
			self.registers[0] = target_result;
		}

		if self._verbosity == 2 {
			self.disassemble(self.pc as usize);
		}

		let opcode = self.read_short();
		if self._verbosity == 1 {
			println!("PC={:x}, op={} ({:x}, {:x}, {:x}) Stack={{{}}} {}", self.pc-1, opcode, self.memory[self.pc as usize], self.memory[(self.pc+1) as usize], self.memory[(self.pc+2) as usize],
			self.stack.iter().map(|i| format!("{:x}", i)).collect::<Vec<String>>().join(", "),
			(0..8).map(|i| format!("{}={:x}", i, self.registers[i])).collect::<Vec<String>>().join(", "));
		}

		match opcode {
			0 => { // halt
				println!("Halt instruction at PC={}", self.pc);
				return false;
			}
			1 => { // set
				let dest_reg = self.read_register();
				let val = self.read_value();
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
				self.set_register(dest_reg, match opcode {
					9 => (op1 + op2) % 32768, //add
					10 => ((op1 as u32 * op2 as u32) % 32768) as u16, //mul
					11 => op1 % op2, //mod
					_ => unreachable!(),});
			}
			12 | 13 => { // and/or
				let dest_reg = self.read_register();
				let op1 = self.read_value();
				let op2 = self.read_value();
				self.set_register(dest_reg, match opcode {
					12 => op1 & op2, // and
					13 => op1 | op2, // or
					_ => unreachable!()});
			}
			14 => { // not
				let dest_reg = self.read_register();
				let val = self.read_value();
				self.set_register(dest_reg, val ^ 0x7FFF);
			}
			15 => { // rmem
				let dest_reg = self.read_register();
				let addr = self.read_value();
				let addr_val = self.memory[addr as usize];
				self.set_register(dest_reg, addr_val);
			}
			16 => { // wmem
				let addr = self.read_value();
				let val = self.read_value();
				self.memory[addr as usize] = val;
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
					return false;
				}
			}
			19 => { // out
				let ch = self.read_value() as u8;
				print!("{}", ch as char);
			}
			20 => { // in
				let dest_reg = self.read_register();
				let mut buffer = [0; 1];
				stdin.read(&mut buffer[..]).unwrap();

				// Allow for special '?' prefixed debug commands
				while buffer[0] == '?' as u8 {
					let mut input = String::new();
					stdin.read_line(&mut input).unwrap();
					self.run_debug_command(input.as_ref());
					stdin.read(&mut buffer[..]).unwrap();
				}

				self.set_register(dest_reg, buffer[0] as u16);
			}
			21 => (), //no-op
			_ => panic!("Unhandled opcode '{}'", opcode),
		}
		true
	}

	fn run_debug_command(&mut self, command : &str) {
		println!("Running debug command: {}", command);

		let parts = command.split_whitespace().collect::<Vec<_>>();
		match parts[0] {
			"dump" => {
				println!("PC={:x}, ({:x}, {:x}, {:x}) Stack={} {{{}}}   Registers = {}", self.pc-1, self.memory[self.pc as usize], self.memory[(self.pc+1) as usize], self.memory[(self.pc+2) as usize], self.stack.len(),
							self.stack.iter().map(|i| format!("{:x}", i)).collect::<Vec<String>>().join(", "),
							(0..8).map(|i| format!("{}={:x}", i, self.registers[i])).collect::<Vec<String>>().join(", "));
			},
			"save" | "load" => {
				if let Some(filename) = parts.get(1) {
					match parts[0] {
						"save" => self.save_state(filename),
						"load" => self.load_state(filename),
						_ => (),
					}
				} else {
					println!("Usage: save <filename>");
				}
			},
			"setreg" => {
				if parts.len() >= 3 {
					self.registers[parts[1].parse::<usize>().unwrap()] = parts[2].parse::<u16>().unwrap();
				} else {
					println!("Usage: setreg <reg> <value>");
				}
			}
			"setverbosity" => {
				if let Some(Ok(verbosity)) = parts.get(1).map(|s| s.parse::<u32>()) {
					self._verbosity = verbosity;
				} else {
					println!("Usage: setverbosity <level>");
				}
			}
			_ => println!("Unhandled debug command '{}'. Accepted commands: dump, save, setreg, setverbosity", parts[0]),
		}
	}

	fn save_state(&self, filename : &str) {
		let full_path = format!("saves/{}", filename);
		let mut file = File::create(&full_path).unwrap();

		writeln!(&mut file, "{:x}", self.pc).unwrap();
		writeln!(&mut file, "{}", (0..8).map(|i| format!("{:x}", self.registers[i])).collect::<Vec<String>>().join(" ")).unwrap();
		writeln!(&mut file, "{}", self.stack.iter().map(|i| format!("{:x}", i)).collect::<Vec<String>>().join(" ")).unwrap();
		unsafe {
			let slice = std::slice::from_raw_parts(self.memory.as_ptr() as *const u8, self.memory.len() * 2);
			let mem = BASE64.encode(slice);
			file.write_all(mem.as_bytes()).expect("Write memory");
		}
		println!("Saved state to: '{}'", full_path);
	}

	fn load_state(&mut self, filename : &str) {
		let full_path = format!("saves/{}", filename);
		if let Ok(file) = File::open(&full_path) {
			let file = BufReader::new(&file);
			let mut lines = file.lines();

			self.pc = u16::from_str_radix(&lines.next().unwrap().unwrap(), 16).unwrap();

			let regs = lines.next().unwrap().unwrap().split_whitespace().map(|i| u16::from_str_radix(i, 16).unwrap()).collect::<Vec<_>>();
			self.registers.clone_from_slice(&regs[0..8]);

			self.stack = lines.next().unwrap().unwrap().split_whitespace().map(|i| u16::from_str_radix(i, 16).unwrap()).collect::<Vec<_>>();

			let memory_str = lines.next().unwrap().unwrap();
			self.memory = convert_vec_u8_to_u16(BASE64.decode(memory_str.as_bytes()).unwrap());
			println!("Loaded state from: '{}'", full_path);
		} else {
			println!("Failed to load '{}'", full_path);
		}
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

// This is the rust implementation of the very recursive function found at instruction 0x178b
// Memo'ization is added to make the performance acceptable
fn super_recursive(a : u16, b : u16, h : u16, memo : &mut [[u16; 32768]; 5]) -> u16 {
	if a == 0 {
		return (b + 1) % 32768;
	}
	
	if  memo[a as usize][b as usize] != 0 {
		return memo[a as usize][b as usize];
	}

	let result = if b == 0 {
		super_recursive(a-1, h, h, memo)
	} else {
		super_recursive(a-1, super_recursive(a, b-1, h, memo), h, memo)
	};
	memo[a as usize][b as usize] = result;
	result
}

fn solve_for_register_7(expected : u16) -> u16 {
	for h in 1.. {
		let mut memo = [[0; 32768]; 5];
		let result = super_recursive(4,1,h, &mut memo);
		if result == expected {
			return result;
		}
	}
	panic!("Failed to find valid initial condition to match the result")
}

#[derive(PartialEq, Clone, Debug)]
enum Op {
	Plus,
	Minus,
	Multiply,
}

#[derive(PartialEq, Clone, Debug)]
enum EqPart {
	Number(i32),
	Op(Op),
}

impl EqPart {
	fn number(&self) -> i32 {
		match *self {
			EqPart::Number(n) => n,
			EqPart::Op(_) => unreachable!(),
		}
	}

	fn op(&self) -> Op {
		match self {
			&EqPart::Number(_) => unreachable!(),
			&EqPart::Op(ref op) => op.clone(),
		}
	}
}

fn eval_equation(equation : &[EqPart]) -> i32 {
	let mut result = equation[0].number();
	let mut i = 1;
	while i < equation.len() {
		result = match equation[i].op() {
			Op::Plus => result + equation[i+1].number(),
			Op::Minus => result - equation[i+1].number(),
			Op::Multiply => result * equation[i+1].number(),
		};
		i += 2;
	}
	result
}

struct State {
	equation : Vec<EqPart>,
	pos : (usize, usize),
}

fn solve_maze() {
	let maze = [
		[EqPart::Number(22), EqPart::Op(Op::Minus), EqPart::Number(9), EqPart::Op(Op::Multiply)],
		[EqPart::Op(Op::Plus), EqPart::Number(4), EqPart::Op(Op::Minus), EqPart::Number(18)],
		[EqPart::Number(4), EqPart::Op(Op::Multiply), EqPart::Number(11), EqPart::Op(Op::Multiply)],
		[EqPart::Op(Op::Multiply), EqPart::Number(8), EqPart::Op(Op::Minus), EqPart::Number(1)],
	];

	let target = 30;
	let mut queue = VecDeque::new();
	queue.push_back(State { equation: Vec::new(), pos: (0,0) });

	while !queue.is_empty() {
		let q = queue.pop_front().unwrap();
		let mut new_eq = q.equation;
		new_eq.push(maze[q.pos.0][q.pos.1].clone());

		if new_eq.len() % 2 == 1 {
			let weight = eval_equation(&new_eq);

			// The walk is over if our 'weight' goes negative, or if we return to 0,0
			if weight < 0 || new_eq.len() > 1 && q.pos == (0, 0) {
				continue;
			}

			if q.pos == (3, 3) {
				if weight == target {
					println!("Maze solution: {:?}", new_eq);
					break;
				} else {
					continue; // Not allowed to visit the vault if we haven't solved the equation
				}
			}
		}

		if q.pos.0 > 0 { queue.push_back( State { pos : (q.pos.0 - 1, q.pos.1), equation: new_eq.clone() }); }
		if q.pos.0 < 3 { queue.push_back( State { pos : (q.pos.0 + 1, q.pos.1), equation: new_eq.clone() }); }
		if q.pos.1 > 0 { queue.push_back( State { pos : (q.pos.0, q.pos.1 - 1), equation: new_eq.clone() }); }
		if q.pos.1 < 3 { queue.push_back( State { pos : (q.pos.0, q.pos.1 + 1), equation: new_eq.clone() }); }
	}
}

fn main() {
	let matches = App::new("Synacor Challenge")
		.author("Galen Elias, gelias@gmail.com")
		.version("0.1.0")
		.about("Synacore Challenge - Solutions in Rust")
		.arg(
			Arg::with_name("verbosity")
				.short("v")
				.required(false)
				.index(1)
				.help("specifies verbosity"))
		.arg(
			Arg::with_name("disassemble")
				.short("d")
				.required(false)
				.help("specifies that we should disassemble the binary instead of run it"))
		.arg(
			Arg::with_name("recursive")
				.short("r")
				.required(false)
				.help("specifies that we should run the super expensive secret algorithm to find the correct inital value of register 7 to use with the teleporter"))
		.arg(
			Arg::with_name("maze")
				.short("m")
				.required(false)
				.help("specifies we should solve the maze portion of the puzzle"))
		.after_help("Longer explaination to appear after the options when \
					displaying the help information from --help or -h")
		.get_matches();

	let mut file=File::open("challenge.bin").unwrap();
	let mut buffer_u8 = Vec::new();

	// read the whole file
	file.read_to_end(&mut buffer_u8).unwrap();
	let mut buffer_u16 = convert_vec_u8_to_u16(buffer_u8);
	buffer_u16.resize(0x8000, 0);

	let mut app = Process{pc : 0, memory : buffer_u16, registers : [0u16; 8], stack : Vec::new(),_verbosity : matches.value_of("verbosity").unwrap_or("0").parse::<u32>().unwrap() };

	if matches.is_present("disassemble") {
		app.dump_disassembly();
	} else if matches.is_present("recursive") {
		let result = solve_for_register_7(6);
		println!("Register 7 = {}", result);
	} else if matches.is_present("maze") {
		println!("Solving maze");
		solve_maze();
	} else {
		let stdin = io::stdin();
		let mut handle = stdin.lock();

		while app.run_next_instruction(&mut handle) { }
	}
}
