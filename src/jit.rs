use gccjit;

pub struct Chip8State<'ctx> {
    pub pc: gccjit::LValue<'ctx>,
    pub sp: gccjit::LValue<'ctx>,
    pub st: gccjit::LValue<'ctx>,
    pub dt: gccjit::LValue<'ctx>,
    pub i: gccjit::LValue<'ctx>,
    pub vs: Vec<gccjit::LValue<'ctx>>,
    pub mem: gccjit::LValue<'ctx>,
    pub fb: gccjit::LValue<'ctx>,
    pub blocks: std::collections::HashMap<u16, gccjit::Block<'ctx>>,
    pub main: gccjit::Function<'ctx>,
    pub cls: gccjit::Function<'ctx>,
    pub rnd: gccjit::Function<'ctx>,
    pub drw: gccjit::Function<'ctx>,
}

static font: [u8; 80] = [
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
        let pc = context.new_global(
            None,
            gccjit::GlobalType::Exported,
            context.new_type::<u16>(),
            "pc",
        );

        let sp = context.new_global(
            None,
            gccjit::GlobalType::Exported,
            context.new_type::<u8>(),
            "sp",
        );

        let st = context.new_global(
            None,
            gccjit::GlobalType::Exported,
            context.new_type::<u8>(),
            "st",
        );

        let dt = context.new_global(
            None,
            gccjit::GlobalType::Exported,
            context.new_type::<u8>(),
            "dt",
        );

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

        let fb = context.new_global(
            None,
            gccjit::GlobalType::Exported,
            context.new_array_type(None, context.new_type::<u8>(), 32 * 64),
            "fb"
        );

        let main = context.new_function(
            None,
            gccjit::FunctionType::Exported,
            context.new_type::<()>(),
            &[],
            "chip8_main",
            false,
        );

        let blocks = std::collections::HashMap::new();

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
            gccjit::FunctionType::AlwaysInline,
            context.new_type::<()>(),
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

        Chip8State {
            pc,
            sp,
            st,
            dt,
            i,
            vs,
            mem,
            fb,
            blocks,
            main,
            cls,
            rnd,
            drw,
        }
    }
}

pub fn recompile_rom<'ctx>(
    context: &'ctx gccjit::Context<'ctx>,
    chip8: &'ctx Chip8State<'ctx>,
    path: &str
) -> Result<usize, anyhow::Error> {
    let rom = std::fs::read(path)?;
    let mut blockcache = std::collections::HashMap::new();
    blockcache.insert(512, chip8.main.new_block("entry"));
    context.set_dump_code_on_compile(true);
    codegen(context, chip8, rom.as_slice(), 512, &mut blockcache)?;
    let result = context.compile();
    let main_result = result.get_function("chip8_main");

    let main: extern "C" fn() =
        if !main_result.is_null() {
            unsafe { std::mem::transmute(main_result) }
        }
        else {
           panic!("failed to codegen")
        };
    main();
    eprintln!("foo");
    Ok(0)
}

#[no_mangle]
#[export_name = "rnd"]
pub extern "C" fn rnd() -> u8 {
    rand::random::<u8>()
}

pub fn codegen<'ctx>(
    context: &'ctx gccjit::Context<'ctx>,
    chip8: &'ctx Chip8State<'ctx>,
    rom: &[u8],
    address: u16,
    blockcache: &mut std::collections::HashMap<u16, gccjit::Block<'ctx>>,
) -> Result<(), anyhow::Error> {
    let block = blockcache.get(&address).ok_or(anyhow::anyhow!("blockcache addr {:x} missing", address))?;
    for (i, byte) in font.iter().enumerate() {
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

    for (i, bytes) in (&rom[address as usize - 512..]).chunks(2).enumerate() {
        let [hi, lo]: [u8; 2] = bytes.try_into()?;
        let a = (hi & 0xF0) >> 4;
        let b = hi & 0x0F;
        let c = (lo & 0xF0) >> 4;
        let d = lo & 0x0F;
        let i_addr = address + (i as u16 * 2);

        match (a, b, c, d) {
            (0x0, 0x0, 0xE, 0x0) => {
                eprintln!("jit(0x{:03x}) emitting cls", i_addr);
                let call = context.new_call(None, chip8.cls, &[]);
                block.add_eval(None, call);
            }
            (0x1, x, y, z) => {
                let addr = ((x as u16) << 8) | ((y as u16) << 4) | (z as u16);
                eprintln!("jit(0x{:03x}) emitting jp #{:x}", i_addr, addr);

                if !blockcache.contains_key(&addr) {
                    blockcache.insert(addr, chip8.main.new_block(format!("{}", addr)));
                    codegen(context, chip8, rom, addr, blockcache)?;
                }

                let block2 = blockcache.get(&address).ok_or(anyhow::anyhow!("blockcache addr {:x} missing", address))?;
                let next = blockcache.get(&addr).ok_or(anyhow::anyhow!("blockcache addr {:x} missing", addr))?;
                block2.end_with_jump(None, *next);
                return Ok(());
            }
            (0x3, vx, _, _) => {
                eprintln!("jit(0x{:03x}) emitting se V{:x}, #{:x}", i_addr, vx, lo);
                let const_val = context.new_rvalue_from_int(context.new_type::<u8>(), lo as i32);
                let conditional = context.new_comparison(
                    None,
                    gccjit::ComparisonOp::Equals,
                    chip8.vs[vx as usize],
                    const_val,
                );

                if !blockcache.contains_key(&(i_addr + 4)) {
                    blockcache.insert(i_addr + 4, chip8.main.new_block(format!("{}", i_addr)));
                    codegen(context, chip8, rom, i_addr + 4, blockcache)?;
                }

                if !blockcache.contains_key(&(i_addr + 2)) {
                    blockcache.insert(i_addr + 2, chip8.main.new_block(format!("{}", i_addr)));
                    codegen(context, chip8, rom, i_addr + 2, blockcache)?;
                }

                let block2 = blockcache.get(&address).ok_or(anyhow::anyhow!("blockcache addr {:x} missing", address))?;
                let t = blockcache.get(&(i_addr + 4)).ok_or(anyhow::anyhow!("blockcache addr {:x} missing", i_addr))?;
                let f = blockcache.get(&(i_addr + 2)).ok_or(anyhow::anyhow!("blockcache addr {:x} missing", i_addr))?;
                block2.end_with_conditional(None, conditional, *t, *f);
                return Ok(());
            }
            (0x4, vx, _, _) => {
                eprintln!("jit(0x{:03x}) emitting sne V{:x}, #{:x}", i_addr, vx, lo);
                let const_val = context.new_rvalue_from_int(context.new_type::<u8>(), lo as i32);
                let conditional = context.new_comparison(
                    None,
                    gccjit::ComparisonOp::NotEquals,
                    chip8.vs[vx as usize],
                    const_val,
                );

                if !blockcache.contains_key(&(i_addr + 4)) {
                    blockcache.insert(i_addr + 4, chip8.main.new_block(format!("{}", i_addr)));
                    codegen(context, chip8, rom, i_addr + 4, blockcache)?;
                }

                if !blockcache.contains_key(&(i_addr + 2)) {
                    blockcache.insert(i_addr + 2, chip8.main.new_block(format!("{}", i_addr)));
                    codegen(context, chip8, rom, i_addr + 2, blockcache)?;
                }

                let block2 = blockcache.get(&address).ok_or(anyhow::anyhow!("blockcache addr {:x} missing", address))?;
                let t = blockcache.get(&(i_addr + 4)).ok_or(anyhow::anyhow!("blockcache addr {:x} missing", i_addr))?;
                let f = blockcache.get(&(i_addr + 2)).ok_or(anyhow::anyhow!("blockcache addr {:x} missing", i_addr))?;
                block2.end_with_conditional(None, conditional, *t, *f);
                return Ok(());
            }
            (0x6, vx, _, _) => {
                // eprintln!("jit(0x{:03x}) emitting ld V{:x}, #{:x}", i_addr, vx, lo);
                let const_val = context.new_rvalue_from_int(context.new_type::<u8>(), lo as i32);
                block.add_assignment(None, chip8.vs[vx as usize], const_val);
            }
            (0x7, vx, _, _) => {
                // eprintln!("jit(0x{:03x}) emitting add V{:x}, #{:x}", i_addr, vx, lo);
                let const_val = context.new_rvalue_from_int(context.new_type::<u8>(), lo as i32);
                block.add_assignment_op(None, chip8.vs[vx as usize], gccjit::BinaryOp::Plus, const_val);
            }
            (0x8, vx, vy, 0x0) => {
                // eprintln!("jit(0x{:03x}) emitting ld V{:x}, V{:x}", i_addr, vx, vy);
                block.add_assignment(None, chip8.vs[vx as usize], chip8.vs[vy as usize]);
            },
            (0x8, vx, vy, 0x1) => {
                eprintln!("jit(0x{:03x}) emitting or V{:x}, V{:x}", i_addr, vx, vy);
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
                eprintln!("jit(0x{:03x}) emitting and V{:x}, V{:x}", i_addr, vx, vy);
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
                eprintln!("jit(0x{:03x}) emitting xor V{:x}, V#{:x}", i_addr, vx, vy);
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
                eprintln!("jit(0x{:03x}) emitting add V{:x}, V{:x}", i_addr, vx, vy);
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
                eprintln!("jit(0x{:03x}) emitting sub V{:x}, V{:x}", i_addr, vx, vy);
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
                use gccjit::ToRValue;
                eprintln!("jit(0x{:03x}) emitting shr V{:x}, (V{:x})", i_addr, vx, vy);
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
                eprintln!("jit(0x{:03x}) emitting subn V{:x}, V{:x}", i_addr, vx, vy);
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
                block.add_assignment(None, chip8.vs[0xF], carry);
            }
            (0x8, vx, vy, 0xE) => {
                use gccjit::ToRValue;
                eprintln!("jit(0x{:03x}) emitting shl V{:x}, (V{:x})", i_addr, vx, vy);
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
            (0xA, x, y, z) => {
                let addr = ((x as u16) << 8) | ((y as u16) << 4) | (z as u16);
                // eprintln!("jit(0x{:03x}) emitting ld I, #{:x}", i_addr, addr);
                let const_val = context.new_rvalue_from_int(context.new_type::<u16>(), addr as i32);
                block.add_assignment(None, chip8.i, const_val);
            }
            (0xC, vx, _, _) => {
                eprintln!("jit(0x{:03x}) emitting rnd V{:x}, #{:x}", i_addr, vx, lo);
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
                use gccjit::ToRValue;
                eprintln!("jit(0x{:03x}) emitting drw V{:x}, V{:x}, #{:x}", i_addr, vx, vy, n);
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
            _ => {
                eprintln!("jit(0x{:03x}) unknown insn {:x} {:x}, aborting", i, hi, lo);
                break;
            }
        }
    }
    block.end_with_void_return(None);
    Ok(())
}
