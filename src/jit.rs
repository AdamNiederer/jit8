use gccjit;

struct Chip8State<'ctx> {
    pc: gccjit::LValue<'ctx>,
    sp: gccjit::LValue<'ctx>,
    st: gccjit::LValue<'ctx>,
    dt: gccjit::LValue<'ctx>,
    vs: Vec<gccjit::LValue<'ctx>>,
    mem: gccjit::LValue<'ctx>,
    fb: gccjit::LValue<'ctx>,
}

impl<'ctx> Chip8State<'ctx> {
    fn new(context: &'ctx gccjit::Context<'ctx>) -> Chip8State<'ctx> {
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

        let vs = (0..16).into_iter().map(|n| {
            context.new_global(
                None,
                gccjit::GlobalType::Exported,
                context.new_type::<u8>(),
                ["v", &n.to_string()].concat(),
            )
        }).collect::<Vec<gccjit::LValue>>();

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

        Chip8State {
            pc: pc,
            sp: sp,
            st: st,
            dt: dt,
            vs: vs,
            mem: mem,
            fb: fb,
        }
    }
}

fn jit_init() {
    let context = gccjit::Context::default();
    context.set_optimization_level(gccjit::OptimizationLevel::Standard);
    let chip8 = Chip8State::new(&context);

    let chip8_main = context.new_function(
        None,
        gccjit::FunctionType::Exported,
        context.new_type::<*mut ()>(),
        &[],
        "chip8_main",
        false,
    );
}

fn recompile_rom(path: &str) -> Result<usize, Box<dyn std::error::Error>> {
    let rom = std::fs::read(path)?;



    Ok(0)
}
