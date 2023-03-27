use gccjit;
use gccjit::ToRValue as _;
use std::collections::HashMap;

pub struct Chip8State<'ctx> {
    pub i: gccjit::LValue<'ctx>,
    pub vs: Vec<gccjit::LValue<'ctx>>,
    pub mem: gccjit::LValue<'ctx>,
    pub blocks: HashMap<u16, gccjit::Block<'ctx>>,
    pub functions: HashMap<u16, gccjit::Function<'ctx>>,
    pub stack: Vec<u16>,
    pub cls: gccjit::Function<'ctx>,
    pub rnd: gccjit::Function<'ctx>,
    pub drw: gccjit::Function<'ctx>,
    pub kp: gccjit::Function<'ctx>,
    pub bkp: gccjit::Function<'ctx>,
    pub lddt: gccjit::Function<'ctx>,
    pub stdt: gccjit::Function<'ctx>,
}

static FONT: [u8; 80] = [
    0xF0, 0x90, 0x90, 0x90, 0xF0,
    0x20, 0x60, 0x20, 0x20, 0x70,
    0xF0, 0x10, 0xF0, 0x80, 0xF0,
    0xF0, 0x10, 0xF0, 0x10, 0xF0,
    0x90, 0x90, 0xF0, 0x10, 0x10,
    0xF0, 0x80, 0xF0, 0x10, 0xF0,
    0xF0, 0x80, 0xF0, 0x90, 0xF0,
    0xF0, 0x10, 0x20, 0x40, 0x40,
    0xF0, 0x90, 0xF0, 0x90, 0xF0,
    0xF0, 0x90, 0xF0, 0x10, 0xF0,
    0xF0, 0x90, 0xF0, 0x90, 0x90,
    0xE0, 0x90, 0xE0, 0x90, 0xE0,
    0xF0, 0x80, 0x80, 0x80, 0xF0,
    0xE0, 0x90, 0x90, 0x90, 0xE0,
    0xF0, 0x80, 0xF0, 0x80, 0xF0,
    0xF0, 0x80, 0xF0, 0x80, 0x80
];

impl<'ctx> Chip8State<'ctx> {
    pub fn new(context: &'ctx gccjit::Context<'ctx>) -> Chip8State<'ctx> {
        let i = context.new_global(
            None,
            gccjit::GlobalType::Exported,
            context.new_type::<u16>(),
            "i",
        );

        let mut vs = Vec::new();
        for n in 0..16 {
            vs.push(context.new_global(
                None,
                gccjit::GlobalType::Exported,
                context.new_type::<u8>(),
                ["v", &n.to_string()].concat(),
            ));
        }

        let mem = context.new_global(
            None,
            gccjit::GlobalType::Exported,
            context.new_array_type(None, context.new_type::<u8>(), 4096),
            "mem"
        );

        let blocks = HashMap::new();
        let functions = HashMap::new();
        let stack = Vec::new();

        let kp = context.new_function(
            None,
            gccjit::FunctionType::Extern,
            context.new_type::<u8>(),
            &[context.new_parameter(None, context.new_type::<u8>(), "key")],
            "kp",
            false
        );

        let bkp = context.new_function(
            None,
            gccjit::FunctionType::Extern,
            context.new_type::<u8>(),
            &[],
            "bkp",
            false
        );

        let cls = context.new_function(
            None,
            gccjit::FunctionType::Extern,
            context.new_type::<()>(),
            &[],
            "cls",
            false
        );

        let rnd = context.new_function(
            None,
            gccjit::FunctionType::Extern,
            context.new_type::<u8>(),
            &[],
            "rnd",
            false
        );

        let drw = context.new_function(
            None,
            gccjit::FunctionType::Extern,
            context.new_type::<u8>(),
            &[
                // *const u8 and context.new_cast(None, rvalue, context.new_type::<*const u8>()) appear to not work
                context.new_parameter(None, context.new_type::<*mut u8>(), "mem"),
                context.new_parameter(None, context.new_type::<u8>(), "x"),
                context.new_parameter(None, context.new_type::<u8>(), "y"),
                context.new_parameter(None, context.new_type::<u16>(), "i"),
                context.new_parameter(None, context.new_type::<u8>(), "n"),
            ],
            "drw",
            false
        );

        let lddt = context.new_function(
            None,
            gccjit::FunctionType::Extern,
            context.new_type::<u8>(),
            &[],
            "lddt",
            false
        );

        let stdt = context.new_function(
            None,
            gccjit::FunctionType::Extern,
            context.new_type::<()>(),
            &[context.new_parameter(None, context.new_type::<u8>(), "val"),],
            "stdt",
            false
        );

        Chip8State {
            i,
            vs,
            mem,
            blocks,
            functions,
            stack,
            cls,
            rnd,
            drw,
            kp,
            bkp,
            lddt,
            stdt,
        }
    }
}

#[derive(Debug)]
pub enum JitResult {
    SelfModifyingCode { pc: u16, len: usize },
    DynamicJump { pc: u16 },
    EndOfProgram,
}

pub fn recompile_rom<'ctx>(
    context: &'ctx gccjit::Context<'ctx>,
    chip8: &'ctx mut Chip8State<'ctx>,
    path: &str
) -> Result<usize, anyhow::Error> {
    let mut generation = 0;
    let mut rom = std::fs::read(path)?;
    let mut address = 512;
    let mut residual_mode = false;
    loop {
        eprintln!("jit: jitting at addr {:03X}", address);
        let (results, _) = compile_call(context, chip8, rom.as_slice(), None, address, residual_mode, generation)?;
        eprintln!("jit: compiling");
        // chip8.functions.get(&0x200).unwrap().dump_to_dot("chip8_0x200.dot");
        let compile_result = context.compile();
        let main_result = compile_result.get_function(format!("f_{}_0x{:03X}", generation, address));

        eprintln!("jit: executing");
        let main: extern "C" fn() =
            if !main_result.is_null() {
                unsafe { std::mem::transmute(main_result) }
            }
        else {
            panic!("failed to codegen")
        };


        main();

        residual_mode = false;
        for result in results {
            match result {
                JitResult::SelfModifyingCode { pc, len } => {
                    let mem_result = compile_result.get_global("mem");
                    let i_result = compile_result.get_global("i");
                    let mem: &mut [u8] = if !mem_result.is_null() {
                        unsafe { std::slice::from_raw_parts_mut(std::mem::transmute(mem_result), 4096) }
                    } else {
                        panic!("failed to get mem pointer")
                    };
                    let i: u16 = if !mem_result.is_null() {
                        unsafe { *std::mem::transmute::<*mut (), *mut u16>(i_result) }
                    } else {
                        panic!("failed to get i pointer")
                    };

                    // eprint!("jit: memdump(   ): ");
                    // for j in 0..64 {
                    //     eprint!("{:02X} ", j);
                    // }
                    //     eprintln!("");
                    // for j in 0..64 {
                    //     eprint!("jit: memdump({:03X}): ", j * 64);
                    //     for k in 0..64 {
                    //         eprint!("{:02X} ", mem[j * 64 + k]);
                    //     }
                    //     eprintln!("");
                    // }

                    let rom_addr = i as usize - 0x200;

                    if rom_addr < rom.len() {
                        chip8.blocks = HashMap::new();
                        chip8.functions = HashMap::new();
                        for j in 0..len {
                            if rom_addr + j < rom.len() {
                                eprintln!("jit: self-modifying code: rom[0x{:03X}/{:03X}] = mem[0x{:03X}] (0x{:02X})", rom_addr + j, rom.len(), i as usize + j, mem[i as usize + j]);
                                rom[rom_addr + j] = mem[i as usize + j];
                            }
                        }
                        generation += 1;
                    }
                    address = pc;
                    residual_mode = true;
                },
                JitResult::DynamicJump { pc } => {
                    address = pc;
                    residual_mode = true;
                },
                JitResult::EndOfProgram => {}
            };
        }

    }
}

#[no_mangle]
#[export_name = "rnd"]
pub extern "C" fn rnd() -> u8 {
    rand::random::<u8>()
}

pub fn compile_call<'ctx>(
    context: &'ctx gccjit::Context<'ctx>,
    chip8: &mut Chip8State<'ctx>,
    rom: &[u8],
    stack_address: Option<u16>,
    address: u16,
    residual_mode: bool,
    generation: usize,
) -> Result<(Vec<JitResult>, gccjit::Function<'ctx>), anyhow::Error> {
    if let Some(func) = chip8.functions.get(&address) {
        Ok((vec!(JitResult::EndOfProgram), *func))
    } else {
        if let Some(stack_addr) = stack_address {
            chip8.stack.push(stack_addr);
        }
        eprintln!("jit(0x{:03x}) new function f_{}_0x{:X}", address, generation, address);

        let func = context.new_function(
            None,
            gccjit::FunctionType::Exported,
            context.new_type::<()>(),
            &[],
            format!("f_{}_0x{:03X}", generation, address),
            false,
        );
        chip8.functions.insert(address, func);

        let block = func.new_block(format!("f_{}_0x{:03X}_entry", generation, address));
        chip8.blocks.insert(address, block);

        if address == 0x200 {
            bootstrap(context, chip8, rom, &block);
        }

        Ok((codegen(context, chip8, rom, address, residual_mode, generation)?, func))
    }
}

pub fn bootstrap<'ctx>(
    context: &'ctx gccjit::Context<'ctx>,
    chip8: &mut Chip8State<'ctx>,
    rom: &[u8],
    block: &gccjit::Block<'ctx>,
) {
    // Initial loading
    for (i, byte) in FONT.iter().enumerate() {
        let const_byte = context.new_rvalue_from_int(context.new_type::<u8>(), *byte as i32);
        let const_mem_offset = context.new_rvalue_from_int(context.new_type::<usize>(), (0x50 + i) as i32);
        let array = context.new_array_access(None, chip8.mem, const_mem_offset);
        block.add_assignment(None, array, const_byte);
    }

    for (i, byte) in rom.iter().enumerate() {
        let const_byte = context.new_rvalue_from_int(context.new_type::<u8>(), *byte as i32);
        let const_mem_offset = context.new_rvalue_from_int(context.new_type::<usize>(), (512 + i) as i32);
        let array = context.new_array_access(None, chip8.mem, const_mem_offset);
        block.add_assignment(None, array, const_byte);
    }
}

pub fn codegen<'ctx>(
    context: &'ctx gccjit::Context<'ctx>,
    chip8: &mut Chip8State<'ctx>,
    rom: &[u8],
    block_address: u16,
    residual_mode: bool,
    generation: usize,
) -> Result<Vec<JitResult>, anyhow::Error> {
    for (i, bytes) in (&rom[block_address as usize - 512..]).chunks(2).enumerate() {
        let [hi, lo]: [u8; 2] = bytes.try_into()?;
        let a = (hi & 0xF0) >> 4;
        let b = hi & 0x0F;
        let c = (lo & 0xF0) >> 4;
        let d = lo & 0x0F;
        let insn_address = block_address + (i as u16 * 2);

        let block = chip8.blocks.get(&block_address).ok_or(anyhow::anyhow!("chip8.blocks addr {:X} missing", block_address))?;
        let function = block.get_function();

        match (a, b, c, d) {
            (0x0, 0x0, 0x0, 0x0) => {
                eprintln!("jit(0x{:03x}) detected uninitialized memory, ignoring (probably self-modifying code)", insn_address);
            }
            (0x0, 0x0, 0xE, 0x0) => {
                eprintln!("jit(0x{:03x}) emitting cls", insn_address);
                let call = context.new_call(None, chip8.cls, &[]);
                block.add_eval(None, call);
            }
            (0x0, 0x0, 0xE, 0xE) => {
                eprintln!("jit(0x{:03x}) emitting ret for 0x{:03X}", insn_address, block_address);

                if residual_mode {
                    let next_addr = chip8.stack.pop();
                    eprintln!("jit: next addr for residual mode: 0x{:03X}", next_addr.unwrap_or(0));
                    if let Some(addr) = next_addr {
                        let (next_result, next) = if let Some(blk) = chip8.blocks.get(&addr) {
                            (vec!(JitResult::EndOfProgram), *blk)
                        } else {
                            let blk = function.new_block(format!("{}_{:03X}", generation, addr));
                            chip8.blocks.insert(addr, blk);
                            (codegen(context, chip8, rom, addr, residual_mode, generation)?, blk)
                        };
                        let block = chip8.blocks.get(&block_address).ok_or(anyhow::anyhow!("chip8.blocks addr {:X} missing", block_address))?;
                        block.end_with_jump(None, next);
                        return Ok(next_result);
                    }
                }

                let block = chip8.blocks.get(&block_address).ok_or(anyhow::anyhow!("chip8.blocks addr {:X} missing", block_address))?;
                block.end_with_void_return(None);

                return Ok(vec!(JitResult::EndOfProgram));
            }
            (0x1, x, y, z) => {
                let addr = ((x as u16) << 8) | ((y as u16) << 4) | (z as u16);
                eprintln!("jit(0x{:03x}) emitting jp #{:X}", insn_address, addr);

                let (next_result, next) = if let Some(blk) = chip8.blocks.get(&addr) {
                    (vec!(JitResult::EndOfProgram), *blk)
                } else {
                    let blk = function.new_block(format!("{}_{:03X}", generation, addr));
                    chip8.blocks.insert(addr, blk);
                    (codegen(context, chip8, rom, addr, residual_mode, generation)?, blk)
                };

                let block = chip8.blocks.get(&block_address).ok_or(anyhow::anyhow!("chip8.blocks addr {:X} missing", block_address))?;
                block.end_with_jump(None, next);
                return Ok(next_result);
            }
            (0x2, x, y, z) => {
                let addr = ((x as u16) << 8) | ((y as u16) << 4) | (z as u16);
                eprintln!("jit(0x{:03x}) emitting call #{:X}", insn_address, addr);

                let (results, function) = compile_call(context, chip8, rom, Some(insn_address + 2), addr, false, generation)?;

                let call = context.new_call(
                    None,
                    function,
                    &[],
                );

                let block = chip8.blocks.get(&block_address).ok_or(anyhow::anyhow!("chip8.blocks addr {:X} missing", block_address))?;
                block.add_eval(None, call);

                for result in results.iter() {
                    match result {
                        JitResult::EndOfProgram => {}
                        _ => {
                            block.end_with_void_return(None);
                            return Ok(results);
                        }
                    }
                }
            }
            (0x3, vx, _, _) => {
                eprintln!("jit(0x{:03x}) emitting se V{:X}, #{:X}", insn_address, vx, lo);
                let const_val = context.new_rvalue_from_int(context.new_type::<u8>(), lo as i32);
                let conditional = context.new_comparison(
                    None,
                    gccjit::ComparisonOp::Equals,
                    chip8.vs[vx as usize],
                    const_val,
                );

                let (mut t_results, t) = if let Some(blk) = chip8.blocks.get(&(insn_address + 4)) {
                    (vec!(JitResult::EndOfProgram), *blk)
                } else {
                    let blk = function.new_block(format!("{}_{:03X}", generation, insn_address + 4));
                    chip8.blocks.insert(insn_address + 4, blk);
                    (codegen(context, chip8, rom, insn_address + 4, residual_mode, generation)?, blk)
                };

                let (mut f_results, f) = if let Some(blk) = chip8.blocks.get(&(insn_address + 2)) {
                    (vec!(JitResult::EndOfProgram), *blk)
                } else {
                    let blk = function.new_block(format!("{}_{:03X}", generation, insn_address + 2));
                    chip8.blocks.insert(insn_address + 2, blk);
                    (codegen(context, chip8, rom, insn_address + 2, residual_mode, generation)?, blk)
                };

                let block = chip8.blocks.get(&block_address).ok_or(anyhow::anyhow!("chip8.blocks addr {:X} missing", block_address))?;
                block.end_with_conditional(None, conditional, t, f);

                t_results.append(&mut f_results);
                return Ok(t_results);
            }
            (0x4, vx, _, _) => {
                eprintln!("jit(0x{:03x}) emitting sne V{:X}, #{:X}", insn_address, vx, lo);
                let const_val = context.new_rvalue_from_int(context.new_type::<u8>(), lo as i32);
                let conditional = context.new_comparison(
                    None,
                    gccjit::ComparisonOp::NotEquals,
                    chip8.vs[vx as usize],
                    const_val,
                );

                let (mut t_results, t) = if let Some(blk) = chip8.blocks.get(&(insn_address + 4)) {
                    (vec!(JitResult::EndOfProgram), *blk)
                } else {
                    let blk = function.new_block(format!("{}_{:03X}", generation, insn_address + 4));
                    chip8.blocks.insert(insn_address + 4, blk);
                    (codegen(context, chip8, rom, insn_address + 4, residual_mode, generation)?, blk)
                };

                let (mut f_results, f) = if let Some(blk) = chip8.blocks.get(&(insn_address + 2)) {
                    (vec!(JitResult::EndOfProgram), *blk)
                } else {
                    let blk = function.new_block(format!("{}_{:03X}", generation, insn_address + 2));
                    chip8.blocks.insert(insn_address + 2, blk);
                    (codegen(context, chip8, rom, insn_address + 2, residual_mode, generation)?, blk)
                };

                let block = chip8.blocks.get(&block_address).ok_or(anyhow::anyhow!("chip8.blocks addr {:X} missing", block_address))?;
                block.end_with_conditional(None, conditional, t, f);

                t_results.append(&mut f_results);
                return Ok(t_results);
            }
            (0x5, vx, vy, 0) => {
                eprintln!("jit(0x{:03x}) emitting se V{:X}, V{:X}", insn_address, vx, vy);
                let conditional = context.new_comparison(
                    None,
                    gccjit::ComparisonOp::Equals,
                    chip8.vs[vx as usize],
                    chip8.vs[vy as usize],
                );

                let (mut t_results, t) = if let Some(blk) = chip8.blocks.get(&(insn_address + 4)) {
                    (vec!(JitResult::EndOfProgram), *blk)
                } else {
                    let blk = function.new_block(format!("{}_{:03X}", generation, insn_address + 4));
                    chip8.blocks.insert(insn_address + 4, blk);
                    (codegen(context, chip8, rom, insn_address + 4, residual_mode, generation)?, blk)
                };

                let (mut f_results, f) = if let Some(blk) = chip8.blocks.get(&(insn_address + 2)) {
                    (vec!(JitResult::EndOfProgram), *blk)
                } else {
                    let blk = function.new_block(format!("{}_{:03X}", generation, insn_address + 2));
                    chip8.blocks.insert(insn_address + 2, blk);
                    (codegen(context, chip8, rom, insn_address + 2, residual_mode, generation)?, blk)
                };

                let block = chip8.blocks.get(&block_address).ok_or(anyhow::anyhow!("chip8.blocks addr {:X} missing", block_address))?;
                block.end_with_conditional(None, conditional, t, f);

                t_results.append(&mut f_results);
                return Ok(t_results);
            }
            (0x6, vx, _, _) => {
                eprintln!("jit(0x{:03x}) emitting ld V{:X}, #{:X}", insn_address, vx, lo);
                let const_val = context.new_rvalue_from_int(context.new_type::<u8>(), lo as i32);
                block.add_assignment(None, chip8.vs[vx as usize], const_val);
            }
            (0x7, vx, _, _) => {
                eprintln!("jit(0x{:03x}) emitting add V{:X}, #{:X}", insn_address, vx, lo);
                let const_val = context.new_rvalue_from_int(context.new_type::<u8>(), lo as i32);
                block.add_assignment_op(None, chip8.vs[vx as usize], gccjit::BinaryOp::Plus, const_val);
            }
            (0x8, vx, vy, 0x0) => {
                eprintln!("jit(0x{:03x}) emitting ld V{:X}, V{:X}", insn_address, vx, vy);
                block.add_assignment(None, chip8.vs[vx as usize], chip8.vs[vy as usize]);
            },
            (0x8, vx, vy, 0x1) => {
                eprintln!("jit(0x{:03x}) emitting or V{:X}, V{:X}", insn_address, vx, vy);
                let result = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::BitwiseOr,
                    context.new_type::<u8>(),
                    chip8.vs[vx as usize],
                    chip8.vs[vy as usize],
                );
                block.add_assignment(None, chip8.vs[vx as usize], result);
            }
            (0x8, vx, vy, 0x2) => {
                eprintln!("jit(0x{:03x}) emitting and V{:X}, V{:X}", insn_address, vx, vy);
                let result = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::BitwiseAnd,
                    context.new_type::<u8>(),
                    chip8.vs[vx as usize],
                    chip8.vs[vy as usize],
                );
                block.add_assignment(None, chip8.vs[vx as usize], result);
            }
            (0x8, vx, vy, 0x3) => {
                eprintln!("jit(0x{:03x}) emitting xor V{:X}, V#{:X}", insn_address, vx, vy);
                let result = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::BitwiseXor,
                    context.new_type::<u8>(),
                    chip8.vs[vx as usize],
                    chip8.vs[vy as usize],
                );
                block.add_assignment(None, chip8.vs[vx as usize], result);
            }
            (0x8, vx, vy, 0x4) => {
                eprintln!("jit(0x{:03x}) emitting add V{:X}, V{:X}", insn_address, vx, vy);
                let result = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::Plus,
                    context.new_type::<u8>(),
                    chip8.vs[vx as usize],
                    chip8.vs[vy as usize],
                );
                block.add_assignment(None, chip8.vs[vx as usize], result);
            }
            (0x8, vx, vy, 0x5) => {
                eprintln!("jit(0x{:03x}) emitting sub V{:X}, V{:X}", insn_address, vx, vy);
                let result = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::Minus,
                    context.new_type::<u8>(),
                    chip8.vs[vx as usize],
                    chip8.vs[vy as usize],
                );
                block.add_assignment(None, chip8.vs[vx as usize], result);
            }
            (0x8, vx, vy, 0x6) => {
                eprintln!("jit(0x{:03x}) emitting shr V{:X}, (V{:X})", insn_address, vx, vy);
                let const_mask = context.new_rvalue_from_int(context.new_type::<u8>(), 0x1 as i32);
                let const_shift = context.new_rvalue_from_int(context.new_type::<u8>(), 0x1 as i32);
                let carry = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::BitwiseAnd,
                    context.new_type::<u8>(),
                    chip8.vs[vx as usize],
                    const_mask,
                );
                let result = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::RShift,
                    context.new_type::<u8>(),
                    chip8.vs[vx as usize],
                    if true { const_shift } else { chip8.vs[vy as usize].to_rvalue() },
                );
                block.add_assignment(None, chip8.vs[vx as usize], result);
                block.add_assignment(None, chip8.vs[0xF], carry);
            }
            (0x8, vx, vy, 0x7) => {
                eprintln!("jit(0x{:03x}) emitting subn V{:X}, V{:X}", insn_address, vx, vy);
                let carry = context.new_comparison(
                    None,
                    gccjit::ComparisonOp::GreaterThan,
                    chip8.vs[vy as usize],
                    chip8.vs[vx as usize],
                );
                let result = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::Minus,
                    context.new_type::<u8>(),
                    chip8.vs[vy as usize],
                    chip8.vs[vx as usize],
                );
                block.add_assignment(None, chip8.vs[vx as usize], result);
                block.add_assignment(None, chip8.vs[0xF], context.new_cast(None, carry, context.new_type::<u8>()));
            }
            (0x8, vx, vy, 0xE) => {
                eprintln!("jit(0x{:03x}) emitting shl V{:X}, (V{:X})", insn_address, vx, vy);
                let const_mask = context.new_rvalue_from_int(context.new_type::<u8>(), 0x80 as i32);
                let const_mask_shift = context.new_rvalue_from_int(context.new_type::<u8>(), 7 as i32);
                let const_shift = context.new_rvalue_from_int(context.new_type::<u8>(), 0x1 as i32);
                let carry = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::RShift,
                    context.new_type::<u8>(),
                    context.new_binary_op(
                        None,
                        gccjit::BinaryOp::BitwiseAnd,
                        context.new_type::<u8>(),
                        chip8.vs[vx as usize],
                        const_mask,
                    ),
                    const_mask_shift,
                );

                let result = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::LShift,
                    context.new_type::<u8>(),
                    chip8.vs[vx as usize],
                    if true { const_shift } else { chip8.vs[vy as usize].to_rvalue() },
                );

                block.add_assignment(None, chip8.vs[vx as usize], result);
                block.add_assignment(None, chip8.vs[0xF], carry);
            }
            (0x9, vx, vy, 0) => {
                eprintln!("jit(0x{:03x}) emitting sne V{:X}, V{:X}", insn_address, vx, vy);
                let conditional = context.new_comparison(
                    None,
                    gccjit::ComparisonOp::NotEquals,
                    chip8.vs[vx as usize],
                    chip8.vs[vy as usize],
                );


                let (mut t_results, t) = if let Some(blk) = chip8.blocks.get(&(insn_address + 4)) {
                    (vec!(JitResult::EndOfProgram), *blk)
                } else {
                    let blk = function.new_block(format!("{}_{:03X}", generation, insn_address + 4));
                    chip8.blocks.insert(insn_address + 4, blk);
                    (codegen(context, chip8, rom, insn_address + 4, residual_mode, generation)?, blk)
                };

                let (mut f_results, f) = if let Some(blk) = chip8.blocks.get(&(insn_address + 2)) {
                    (vec!(JitResult::EndOfProgram), *blk)
                } else {
                    let blk = function.new_block(format!("{}_{:03X}", generation, insn_address + 2));
                    chip8.blocks.insert(insn_address + 2, blk);
                    (codegen(context, chip8, rom, insn_address + 2, residual_mode, generation)?, blk)
                };

                let block = chip8.blocks.get(&block_address).ok_or(anyhow::anyhow!("chip8.blocks addr {:X} missing", block_address))?;
                block.end_with_conditional(None, conditional, t, f);

                t_results.append(&mut f_results);
                return Ok(t_results);
            }
            (0xA, x, y, z) => {
                let addr = ((x as u16) << 8) | ((y as u16) << 4) | (z as u16);
                eprintln!("jit(0x{:03x}) emitting ld I, #{:X}", insn_address, addr);
                let const_val = context.new_rvalue_from_int(context.new_type::<u16>(), addr as i32);
                block.add_assignment(None, chip8.i, const_val);
            }
            (0xB, x, y, z) => {
                let addr = ((x as u16) << 8) | ((y as u16) << 4) | (z as u16);
                eprintln!("jit(0x{:03x}) emitting jp V0 #{:X} (return to host)", insn_address, addr);
                block.end_with_void_return(None);
                return Ok(vec!(JitResult::DynamicJump { pc: addr }.into()));
            }
            (0xC, vx, _, _) => {
                eprintln!("jit(0x{:03x}) emitting rnd V{:X}, #{:X}", insn_address, vx, lo);
                let const_mask = context.new_rvalue_from_int(context.new_type::<u8>(), lo as i32);
                let rand_result = context.new_call(None, chip8.rnd, &[]);
                let and_result = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::BitwiseAnd,
                    context.new_type::<u8>(),
                    rand_result,
                    const_mask,
                );
                block.add_assignment(None, chip8.vs[vx as usize], and_result);
            }
            (0xD, vx, vy, n) => {
                eprintln!("jit(0x{:03x}) emitting drw V{:X}, V{:X}, #{:X}", insn_address, vx, vy, n);
                let const_n = context.new_rvalue_from_int(context.new_type::<u8>(), n as i32);

                let intersection = context.new_call(None, chip8.drw, &[
                    chip8.mem.get_address(None),
                    chip8.vs[vx as usize].to_rvalue(),
                    chip8.vs[vy as usize].to_rvalue(),
                    chip8.i.to_rvalue(),
                    const_n,
                ]);

                block.add_assignment(None, chip8.vs[0xF], intersection);
            }
            (0xE, vx, 0x9, 0xE) => {
                eprintln!("jit(0x{:03x}) emitting skp V{:X}", insn_address, vx);
                let is_pressed = context.new_call(None, chip8.kp, &[chip8.vs[vx as usize].to_rvalue()]);

                let (mut t_results, t) = if let Some(blk) = chip8.blocks.get(&(insn_address + 4)) {
                    (vec!(JitResult::EndOfProgram), *blk)
                } else {
                    let blk = function.new_block(format!("{}_{:03X}", generation, insn_address + 4));
                    chip8.blocks.insert(insn_address + 4, blk);
                    (codegen(context, chip8, rom, insn_address + 4, residual_mode, generation)?, blk)
                };

                let (mut f_results, f) = if let Some(blk) = chip8.blocks.get(&(insn_address + 2)) {
                    (vec!(JitResult::EndOfProgram), *blk)
                } else {
                    let blk = function.new_block(format!("{}_{:03X}", generation, insn_address + 2));
                    chip8.blocks.insert(insn_address + 2, blk);
                    (codegen(context, chip8, rom, insn_address + 2, residual_mode, generation)?, blk)
                };

                let block = chip8.blocks.get(&block_address).ok_or(anyhow::anyhow!("chip8.blocks addr {:X} missing", block_address))?;
                block.end_with_conditional(None, context.new_cast(None, is_pressed, context.new_type::<bool>()), t, f);

                t_results.append(&mut f_results);
                return Ok(t_results);
            }
            (0xE, vx, 0xA, 0x1) => {
                eprintln!("jit(0x{:03x}) emitting sknp V{:X}", insn_address, vx);
                let is_pressed = context.new_call(None, chip8.kp, &[chip8.vs[vx as usize].to_rvalue()]);
                let is_not_pressed = context.new_unary_op(
                    None,
                    gccjit::UnaryOp::LogicalNegate,
                    context.new_type::<bool>(),
                    context.new_cast(None, is_pressed, context.new_type::<bool>())
                );


                let (mut t_results, t) = if let Some(blk) = chip8.blocks.get(&(insn_address + 4)) {
                    (vec!(JitResult::EndOfProgram), *blk)
                } else {
                    let blk = function.new_block(format!("{}_{:03X}", generation, insn_address + 4));
                    chip8.blocks.insert(insn_address + 4, blk);
                    (codegen(context, chip8, rom, insn_address + 4, residual_mode, generation)?, blk)
                };

                let (mut f_results, f) = if let Some(blk) = chip8.blocks.get(&(insn_address + 2)) {
                    (vec!(JitResult::EndOfProgram), *blk)
                } else {
                    let blk = function.new_block(format!("{}_{:03X}", generation, insn_address + 2));
                    chip8.blocks.insert(insn_address + 2, blk);
                    (codegen(context, chip8, rom, insn_address + 2, residual_mode, generation)?, blk)
                };

                let block = chip8.blocks.get(&block_address).ok_or(anyhow::anyhow!("chip8.blocks addr {:X} missing", block_address))?;
                block.end_with_conditional(None, is_not_pressed, t, f);

                t_results.append(&mut f_results);
                return Ok(t_results);
            }
            (0xF, vx, 0x0, 0x7) => {
                eprintln!("jit(0x{:03x}) emitting ld V{:X}, DT", insn_address, vx);
                let dt_result = context.new_call(None, chip8.lddt, &[]);
                block.add_assignment(None, chip8.vs[vx as usize], dt_result);
            }
            (0xF, vx, 0x0, 0xA) => {
                eprintln!("jit(0x{:03x}) emitting bkp V{:X}", insn_address, vx);
                let key_result = context.new_call(None, chip8.bkp, &[]);
                block.add_assignment(None, chip8.vs[vx as usize], key_result);
            }
            (0xF, vx, 0x1, 0x5) => {
                eprintln!("jit(0x{:03x}) emitting ld DT, V{:X}", insn_address, vx);
                let dt_result = context.new_call(None, chip8.stdt, &[chip8.vs[vx as usize].to_rvalue()]);
                block.add_eval(None, dt_result);
            }
            (0xF, vx, 0x1, 0x8) => {
                eprintln!("jit(0x{:03x}) emitting ld ST, V{:X} (TODO)", insn_address, vx);
            }
            (0xF, vx, 0x1, 0xE) => {
                eprintln!("jit(0x{:03x}) emitting add I, V{:X}", insn_address, vx);
                let cast = context.new_cast(None, chip8.vs[vx as usize], context.new_type::<u16>());
                block.add_assignment_op(None, chip8.i, gccjit::BinaryOp::Plus, cast);
            }
            (0xF, vx, 0x2, 0x9) => {
                eprintln!("jit(0x{:03x}) emitting fld I, V{:X}", insn_address, vx);
                let const_font_offset = context.new_rvalue_from_int(context.new_type::<u8>(), 0x50 as i32);
                let font_addr = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::Plus,
                    context.new_type::<u16>(),
                    const_font_offset,
                    chip8.vs[vx as usize],
                );
                block.add_assignment(None, chip8.i, font_addr);
            }
            (0xF, vx, 0x3, 0x3) => {
                eprintln!("jit(0x{:03x}) emitting bcd [I], V{:X} (flushing jit cache)", insn_address, vx);
                let const_hundred = context.new_rvalue_from_int(context.new_type::<u8>(), 100 as i32);
                let const_ten = context.new_rvalue_from_int(context.new_type::<u8>(), 10 as i32);
                let const_zero = context.new_rvalue_from_int(context.new_type::<u16>(), 0 as i32);
                let const_one = context.new_rvalue_from_int(context.new_type::<u16>(), 1 as i32);
                let const_two = context.new_rvalue_from_int(context.new_type::<u16>(), 2 as i32);
                let hundreds_digit = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::Divide,
                    context.new_type::<u8>(),
                    chip8.vs[vx as usize],
                    const_hundred,
                );
                let tens_digit = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::Modulo,
                    context.new_type::<u8>(),
                    context.new_binary_op(
                        None,
                        gccjit::BinaryOp::Divide,
                        context.new_type::<u8>(),
                        chip8.vs[vx as usize],
                        const_ten,
                    ),
                    const_ten,
                );
                let ones_digit = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::Modulo,
                    context.new_type::<u8>(),
                    chip8.vs[vx as usize],
                    const_ten,
                );
                let hundreds_loc = context.new_array_access(
                    None,
                    chip8.mem,
                    context.new_binary_op(
                        None,
                        gccjit::BinaryOp::Plus,
                        context.new_type::<u16>(),
                        chip8.i,
                        const_zero
                    )
                );
                let tens_loc = context.new_array_access(
                    None,
                    chip8.mem,
                    context.new_binary_op(
                        None,
                        gccjit::BinaryOp::Plus,
                        context.new_type::<u16>(),
                        chip8.i,
                        const_one
                    )
                );
                let ones_loc = context.new_array_access(
                    None,
                    chip8.mem,
                    context.new_binary_op(
                        None,
                        gccjit::BinaryOp::Plus,
                        context.new_type::<u16>(),
                        chip8.i,
                        const_two
                    )
                );
                block.add_assignment(None, hundreds_loc, hundreds_digit);
                block.add_assignment(None, tens_loc, tens_digit);
                block.add_assignment(None, ones_loc, ones_digit);
                block.end_with_void_return(None);
                return Ok(vec!(JitResult::SelfModifyingCode { pc: insn_address + 2, len: 3 }.into()));
            }
            (0xF, vx, 0x5, 0x5) => {
                eprintln!("jit(0x{:03x}) emitting ld [I], V{:X} (flushing jit cache)", insn_address, vx);
                for v in 0..=vx {
                    let const_offset = context.new_rvalue_from_int(context.new_type::<u16>(), v as i32);
                    let loc = context.new_array_access(None, chip8.mem, const_offset);
                    block.add_assignment(None, loc, chip8.vs[v as usize]);
                }
                block.end_with_void_return(None);
                return Ok(vec!(JitResult::SelfModifyingCode { pc: insn_address + 2, len: vx as usize + 1 }.into()));
            }
            (0xF, vx, 0x6, 0x5) => {
                eprintln!("jit(0x{:03x}) emitting ld V{:X}, [I]", insn_address, vx);
                for v in 0..=vx {
                    let const_offset = context.new_rvalue_from_int(context.new_type::<u16>(), v as i32);
                    let loc = context.new_array_access(None, chip8.mem, const_offset);
                    block.add_assignment(None, chip8.vs[v as usize], loc);
                }
            }
            _ => {
                eprintln!("jit(0x{:03x}) unknown insn {:X} {:X}, aborting", insn_address, hi, lo);
                return Ok(vec!(JitResult::EndOfProgram));
            }
        }
    }
    {
        eprintln!("jit: returning for end of rom");
        let block = chip8.blocks.get(&block_address).ok_or(anyhow::anyhow!("chip8.blocks addr {:X} missing", block_address))?;
        block.end_with_void_return(None);
        Ok(vec!(JitResult::EndOfProgram))
    }
}
