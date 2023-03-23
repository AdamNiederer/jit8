use gccjit;

pub struct Chip8State<'ctx> {
    pub st: gccjit::LValue<'ctx>,
    pub dt: gccjit::LValue<'ctx>,
    pub i: gccjit::LValue<'ctx>,
    pub vs: Vec<gccjit::LValue<'ctx>>,
    pub mem: gccjit::LValue<'ctx>,
    pub blocks: std::collections::HashMap<u16, gccjit::Block<'ctx>>,
    pub main: gccjit::Function<'ctx>,
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

        let main = context.new_function(
            None,
            gccjit::FunctionType::Exported,
            context.new_type::<()>(),
            &[],
            "chip8_main",
            false,
        );

        let blocks = std::collections::HashMap::new();

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
            st,
            dt,
            i,
            vs,
            mem,
            blocks,
            main,
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

pub fn recompile_rom<'ctx>(
    context: &'ctx gccjit::Context<'ctx>,
    chip8: &'ctx Chip8State<'ctx>,
    path: &str
) -> Result<usize, anyhow::Error> {
    let rom = std::fs::read(path)?;
    let mut blockcache = std::collections::HashMap::new();
    blockcache.insert(512, chip8.main.new_block("entry"));
    // context.set_dump_code_on_compile(true);
    codegen(context, chip8, rom.as_slice(), 512, &mut blockcache)?;
    eprintln!("jit: compiling");
    let result = context.compile();
    let main_result = result.get_function("chip8_main");
    eprintln!("jit: executing");
    let main: extern "C" fn() =
        if !main_result.is_null() {
            unsafe { std::mem::transmute(main_result) }
        }
        else {
           panic!("failed to codegen")
        };
    main();
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
    let block = blockcache.get(&address).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", address))?;
    if address == 0x200 {
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
            (0x0, 0x0, 0xE, 0xE) => {
                eprintln!("jit(0x{:03x}) emitting ret", i_addr);
                // TODO: This actually needs to jump back to PC + 2
                block.end_with_void_return(None);
                return Ok(());
            }
            (0x1, x, y, z) => {
                let addr = ((x as u16) << 8) | ((y as u16) << 4) | (z as u16);
                eprintln!("jit(0x{:03x}) emitting jp #{:X}", i_addr, addr);

                if !blockcache.contains_key(&addr) {
                    blockcache.insert(addr, chip8.main.new_block(format!("{}", addr)));
                    codegen(context, chip8, rom, addr, blockcache)?;
                }

                let block2 = blockcache.get(&address).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", address))?;
                let next = blockcache.get(&addr).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", addr))?;
                block2.end_with_jump(None, *next);
                return Ok(());
            }
            (0x2, x, y, z) => {
                let addr = ((x as u16) << 8) | ((y as u16) << 4) | (z as u16);
                eprintln!("jit(0x{:03x}) emitting call #{:X}", i_addr, addr);

                if !blockcache.contains_key(&addr) {
                    blockcache.insert(addr, chip8.main.new_block(format!("{}", addr)));
                    codegen(context, chip8, rom, addr, blockcache)?;
                }

                let block2 = blockcache.get(&address).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", address))?;
                let next = blockcache.get(&addr).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", addr))?;
                block2.end_with_jump(None, *next);
                return Ok(());
            }
            (0x3, vx, _, _) => {
                eprintln!("jit(0x{:03x}) emitting se V{:X}, #{:X}", i_addr, vx, lo);
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

                let block2 = blockcache.get(&address).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", address))?;
                let t = blockcache.get(&(i_addr + 4)).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", i_addr))?;
                let f = blockcache.get(&(i_addr + 2)).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", i_addr))?;
                block2.end_with_conditional(None, conditional, *t, *f);
                return Ok(());
            }
            (0x4, vx, _, _) => {
                eprintln!("jit(0x{:03x}) emitting sne V{:X}, #{:X}", i_addr, vx, lo);
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

                let block2 = blockcache.get(&address).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", address))?;
                let t = blockcache.get(&(i_addr + 4)).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", i_addr))?;
                let f = blockcache.get(&(i_addr + 2)).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", i_addr))?;
                block2.end_with_conditional(None, conditional, *t, *f);
                return Ok(());
            }
            (0x5, vx, vy, 0) => {
                eprintln!("jit(0x{:03x}) emitting se V{:X}, V{:X}", i_addr, vx, vy);
                let conditional = context.new_comparison(
                    None,
                    gccjit::ComparisonOp::Equals,
                    chip8.vs[vx as usize],
                    chip8.vs[vy as usize],
                );

                if !blockcache.contains_key(&(i_addr + 4)) {
                    blockcache.insert(i_addr + 4, chip8.main.new_block(format!("{}", i_addr)));
                    codegen(context, chip8, rom, i_addr + 4, blockcache)?;
                }

                if !blockcache.contains_key(&(i_addr + 2)) {
                    blockcache.insert(i_addr + 2, chip8.main.new_block(format!("{}", i_addr)));
                    codegen(context, chip8, rom, i_addr + 2, blockcache)?;
                }

                let block2 = blockcache.get(&address).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", address))?;
                let t = blockcache.get(&(i_addr + 4)).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", i_addr))?;
                let f = blockcache.get(&(i_addr + 2)).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", i_addr))?;
                block2.end_with_conditional(None, conditional, *t, *f);
                return Ok(());
            }
            (0x6, vx, _, _) => {
                eprintln!("jit(0x{:03x}) emitting ld V{:X}, #{:X}", i_addr, vx, lo);
                let const_val = context.new_rvalue_from_int(context.new_type::<u8>(), lo as i32);
                block.add_assignment(None, chip8.vs[vx as usize], const_val);
            }
            (0x7, vx, _, _) => {
                eprintln!("jit(0x{:03x}) emitting add V{:X}, #{:X}", i_addr, vx, lo);
                let const_val = context.new_rvalue_from_int(context.new_type::<u8>(), lo as i32);
                block.add_assignment_op(None, chip8.vs[vx as usize], gccjit::BinaryOp::Plus, const_val);
            }
            (0x8, vx, vy, 0x0) => {
                eprintln!("jit(0x{:03x}) emitting ld V{:X}, V{:X}", i_addr, vx, vy);
                block.add_assignment(None, chip8.vs[vx as usize], chip8.vs[vy as usize]);
            },
            (0x8, vx, vy, 0x1) => {
                eprintln!("jit(0x{:03x}) emitting or V{:X}, V{:X}", i_addr, vx, vy);
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
                eprintln!("jit(0x{:03x}) emitting and V{:X}, V{:X}", i_addr, vx, vy);
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
                eprintln!("jit(0x{:03x}) emitting xor V{:X}, V#{:X}", i_addr, vx, vy);
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
                eprintln!("jit(0x{:03x}) emitting add V{:X}, V{:X}", i_addr, vx, vy);
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
                eprintln!("jit(0x{:03x}) emitting sub V{:X}, V{:X}", i_addr, vx, vy);
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
                eprintln!("jit(0x{:03x}) emitting shr V{:X}, (V{:X})", i_addr, vx, vy);
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
                eprintln!("jit(0x{:03x}) emitting subn V{:X}, V{:X}", i_addr, vx, vy);
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
                eprintln!("jit(0x{:03x}) emitting shl V{:X}, (V{:X})", i_addr, vx, vy);
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
                eprintln!("jit(0x{:03x}) emitting sne V{:X}, V{:X}", i_addr, vx, vy);
                let conditional = context.new_comparison(
                    None,
                    gccjit::ComparisonOp::NotEquals,
                    chip8.vs[vx as usize],
                    chip8.vs[vy as usize],
                );

                if !blockcache.contains_key(&(i_addr + 4)) {
                    blockcache.insert(i_addr + 4, chip8.main.new_block(format!("{}", i_addr)));
                    codegen(context, chip8, rom, i_addr + 4, blockcache)?;
                }

                if !blockcache.contains_key(&(i_addr + 2)) {
                    blockcache.insert(i_addr + 2, chip8.main.new_block(format!("{}", i_addr)));
                    codegen(context, chip8, rom, i_addr + 2, blockcache)?;
                }

                let block2 = blockcache.get(&address).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", address))?;
                let t = blockcache.get(&(i_addr + 4)).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", i_addr))?;
                let f = blockcache.get(&(i_addr + 2)).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", i_addr))?;
                block2.end_with_conditional(None, conditional, *t, *f);
                return Ok(());
            }
            (0xA, x, y, z) => {
                let addr = ((x as u16) << 8) | ((y as u16) << 4) | (z as u16);
                eprintln!("jit(0x{:03x}) emitting ld I, #{:X}", i_addr, addr);
                let const_val = context.new_rvalue_from_int(context.new_type::<u16>(), addr as i32);
                block.add_assignment(None, chip8.i, const_val);
            }
            (0xB, x, y, z) => {
                let addr = ((x as u16) << 8) | ((y as u16) << 4) | (z as u16);
                eprintln!("jit(0x{:03x}) emitting jp V0 #{:X} (return to host)", i_addr, addr);
                block.end_with_void_return(None);
                return Ok(());
            }
            (0xC, vx, _, _) => {
                eprintln!("jit(0x{:03x}) emitting rnd V{:X}, #{:X}", i_addr, vx, lo);
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
                eprintln!("jit(0x{:03x}) emitting drw V{:X}, V{:X}, #{:X}", i_addr, vx, vy, n);
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
                use gccjit::ToRValue;
                eprintln!("jit(0x{:03x}) emitting skp V{:X}", i_addr, vx);
                let is_pressed = context.new_call(None, chip8.kp, &[chip8.vs[vx as usize].to_rvalue()]);

                if !blockcache.contains_key(&(i_addr + 4)) {
                    blockcache.insert(i_addr + 4, chip8.main.new_block(format!("{}", i_addr)));
                    codegen(context, chip8, rom, i_addr + 4, blockcache)?;
                }

                if !blockcache.contains_key(&(i_addr + 2)) {
                    blockcache.insert(i_addr + 2, chip8.main.new_block(format!("{}", i_addr)));
                    codegen(context, chip8, rom, i_addr + 2, blockcache)?;
                }

                let block2 = blockcache.get(&address).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", address))?;
                let t = blockcache.get(&(i_addr + 4)).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", i_addr))?;
                let f = blockcache.get(&(i_addr + 2)).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", i_addr))?;
                block2.end_with_conditional(None, context.new_cast(None, is_pressed, context.new_type::<bool>()), *t, *f);
                return Ok(());
            }
            (0xE, vx, 0xA, 0x1) => {
                use gccjit::ToRValue;
                eprintln!("jit(0x{:03x}) emitting sknp V{:X}", i_addr, vx);
                let is_pressed = context.new_call(None, chip8.kp, &[chip8.vs[vx as usize].to_rvalue()]);
                let is_not_pressed = context.new_unary_op(
                    None,
                    gccjit::UnaryOp::LogicalNegate,
                    context.new_type::<bool>(),
                    context.new_cast(None, is_pressed, context.new_type::<bool>())
                );

                if !blockcache.contains_key(&(i_addr + 4)) {
                    blockcache.insert(i_addr + 4, chip8.main.new_block(format!("{}", i_addr)));
                    codegen(context, chip8, rom, i_addr + 4, blockcache)?;
                }

                if !blockcache.contains_key(&(i_addr + 2)) {
                    blockcache.insert(i_addr + 2, chip8.main.new_block(format!("{}", i_addr)));
                    codegen(context, chip8, rom, i_addr + 2, blockcache)?;
                }

                let block2 = blockcache.get(&address).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", address))?;
                let t = blockcache.get(&(i_addr + 4)).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", i_addr))?;
                let f = blockcache.get(&(i_addr + 2)).ok_or(anyhow::anyhow!("blockcache addr {:X} missing", i_addr))?;
                block2.end_with_conditional(None, is_not_pressed, *t, *f);
                return Ok(());
            }
            (0xF, vx, 0x0, 0x7) => {
                eprintln!("jit(0x{:03x}) emitting ld V{:X}, DT", i_addr, vx);
                let dt_result = context.new_call(None, chip8.lddt, &[]);
                block.add_assignment(None, chip8.vs[vx as usize], dt_result);
            }
            (0xF, vx, 0x0, 0xA) => {
                eprintln!("jit(0x{:03x}) emitting bkp V{:X}", i_addr, vx);
                let key_result = context.new_call(None, chip8.bkp, &[]);
                block.add_assignment(None, chip8.vs[vx as usize], key_result);
            }
            (0xF, vx, 0x1, 0x5) => {
                use gccjit::ToRValue;
                eprintln!("jit(0x{:03x}) emitting ld DT, V{:X}", i_addr, vx);
                let dt_result = context.new_call(None, chip8.stdt, &[chip8.vs[vx as usize].to_rvalue()]);
                block.add_eval(None, dt_result);
            }
            (0xF, vx, 0x1, 0xE) => {
                eprintln!("jit(0x{:03x}) emitting add I, V{:X}", i_addr, vx);
                let cast = context.new_cast(None, chip8.vs[vx as usize], context.new_type::<u16>());
                block.add_assignment_op(None, chip8.i, gccjit::BinaryOp::Plus, cast);
            }
            (0xF, vx, 0x2, 0x9) => {
                eprintln!("jit(0x{:03x}) emitting fld I, V{:X}", i_addr, vx);
                let const_font_offset = context.new_rvalue_from_int(context.new_type::<u8>(), 0x50 as i32);
                let font_address = context.new_binary_op(
                    None,
                    gccjit::BinaryOp::Plus,
                    context.new_type::<u16>(),
                    const_font_offset,
                    chip8.vs[vx as usize],
                );
                block.add_assignment(None, chip8.i, font_address);
            }
            (0xF, vx, 0x3, 0x3) => {
                eprintln!("jit(0x{:03x}) emitting bcd [I], V{:X} (flushing jit cache)", i_addr, vx);
                let const_hundred = context.new_rvalue_from_int(context.new_type::<u8>(), 100 as i32);
                let const_ten = context.new_rvalue_from_int(context.new_type::<u8>(), 10 as i32);
                let const_zero = context.new_rvalue_from_int(context.new_type::<u8>(), 10 as i32);
                let const_one = context.new_rvalue_from_int(context.new_type::<u8>(), 10 as i32);
                let const_two = context.new_rvalue_from_int(context.new_type::<u8>(), 10 as i32);
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
                let hundreds_loc = context.new_array_access(None, chip8.mem, const_zero);
                let tens_loc = context.new_array_access(None, chip8.mem, const_one);
                let ones_loc = context.new_array_access(None, chip8.mem, const_two);
                block.add_assignment(None, hundreds_loc, hundreds_digit);
                block.add_assignment(None, tens_loc, tens_digit);
                block.add_assignment(None, ones_loc, ones_digit);
            }
            (0xF, vx, 0x5, 0x5) => {
                eprintln!("jit(0x{:03x}) emitting ld [I], V{:X} (flushing jit cache)", i_addr, vx);
                for v in 0..=vx {
                    let const_offset = context.new_rvalue_from_int(context.new_type::<u16>(), v as i32);
                    let loc = context.new_array_access(None, chip8.mem, const_offset);
                    block.add_assignment(None, loc, chip8.vs[v as usize]);

                }
            }
            (0xF, vx, 0x6, 0x5) => {
                eprintln!("jit(0x{:03x}) emitting ld V{:X}, [I]", i_addr, vx);
                for v in 0..=vx {
                    let const_offset = context.new_rvalue_from_int(context.new_type::<u16>(), v as i32);
                    let loc = context.new_array_access(None, chip8.mem, const_offset);
                    block.add_assignment(None, chip8.vs[v as usize], loc);
                }
            }
            _ => {
                eprintln!("jit(0x{:03x}) unknown insn {:X} {:X}, aborting", i, hi, lo);
                break;
            }
        }
    }
    block.end_with_void_return(None);
    Ok(())
}
