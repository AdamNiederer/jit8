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
    pub main: gccjit::Function<'ctx>,
}

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

        Chip8State {
            pc: pc,
            sp: sp,
            st: st,
            dt: dt,
            i: i,
            vs: vs,
            mem: mem,
            fb: fb,
            main: main,

        }
    }
}

pub fn recompile_rom<'ctx>(
    context: &'ctx gccjit::Context<'ctx>,
    chip8: &'ctx Chip8State<'ctx>,
    path: &str
) -> Result<usize, Box<dyn std::error::Error>> {
    let rom = std::fs::read(path)?;
    let entry = chip8.main.new_block("entry");

    codegen(context, chip8, rom.as_slice(), &entry)?;
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

pub fn codegen<'ctx>(
    context: &'ctx gccjit::Context<'ctx>,
    chip8: &'ctx Chip8State<'ctx>,
    rom: &[u8],
    block: &'ctx gccjit::Block<'ctx>,
) -> Result<usize, Box<dyn std::error::Error>> {
    let cls = context.new_function(
        None,
        gccjit::FunctionType::Extern,
        context.new_type::<()>(),
        &[],
        "cls",
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

    for (i, byte) in rom.iter().enumerate() {
        let const_byte = context.new_rvalue_from_int(context.new_type::<u8>(), *byte as i32);
        let const_mem_offset = context.new_rvalue_from_int(context.new_type::<usize>(), (512 + i) as i32);
        let array = context.new_array_access(None, chip8.mem, const_mem_offset);
        block.add_assignment(None, array, const_byte);
    }

    for (i, bytes) in (&rom).chunks(2).enumerate() {
        let [hi, lo]: [u8; 2] = bytes.try_into()?;
        let a = (hi & 0xF0) >> 4;
        let b = hi & 0x0F;
        let c = (lo & 0xF0) >> 4;
        let d = lo & 0x0F;

        match (a, b, c, d) {
            (0x0, 0x0, 0xE, 0x0) => {
                eprintln!("jit({}): emitting cls", i);
                let call = context.new_call(None, cls, &[]);
                block.add_eval(None, call);
            }
            (0x6, reg, _, _) => {
                eprintln!("jit({}): emitting ld V{:x}, #{:x}", i, reg, lo);
                let const_val = context.new_rvalue_from_int(context.new_type::<u8>(), lo as i32);
                block.add_assignment(None, chip8.vs[reg as usize], const_val);
            }
            (0x7, reg, _, _) => {
                eprintln!("jit({}): emitting add V{:x}, #{:x}", i, reg, lo);
                let const_val = context.new_rvalue_from_int(context.new_type::<u8>(), lo as i32);
                block.add_assignment_op(None, chip8.vs[reg as usize], gccjit::BinaryOp::Plus, const_val);
            }
            (0xA, x, y, z) => {
                let addr: u16 = ((x as u16) << 8) | ((y as u16) << 4) | (z as u16);
                eprintln!("jit({}): emitting ld I, #{:x}", i, addr);
                let const_val = context.new_rvalue_from_int(context.new_type::<u16>(), addr as i32);
                block.add_assignment(None, chip8.i, const_val);
            }
            (0xD, vx, vy, n) => {
                use gccjit::ToRValue;
                eprintln!("jit({}): emitting drw V{:x}, V{:x}, #{:x}", i, vx, vy, n);
                let const_n = context.new_rvalue_from_int(context.new_type::<u8>(), n as i32);

                let intersection = context.new_call(None, drw, &[
                    chip8.mem.get_address(None),
                    chip8.vs[vx as usize].to_rvalue(),
                    chip8.vs[vy as usize].to_rvalue(),
                    chip8.i.to_rvalue(),
                    const_n,
                ]);

                block.add_assignment(None, chip8.vs[0x0F], intersection);
            }
            _ => {
                eprintln!("jit({}): unknown insn {:x} {:x}, aborting", i, hi, lo);
                break;
            }
        }
    }
    block.end_with_void_return(None);
    Ok(0)
}
