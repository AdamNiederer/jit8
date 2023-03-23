mod video;
mod jit;
mod input;
mod timers;

fn main() {
    std::thread::spawn(|| {
        let context = gccjit::Context::default();
        context.set_optimization_level(gccjit::OptimizationLevel::Limited);
        let mut chip8 = jit::Chip8State::new(&context);
        jit::recompile_rom(&context, &mut chip8, "chip8-test-suite.ch8").unwrap()
    });

    let runtime = tokio::runtime::Runtime::new().unwrap();
    runtime.block_on(async {
        video::run_display().await;
    });
}

pub use video::cls;
