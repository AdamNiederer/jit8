mod video;
mod jit;

#[tokio::main]
async fn main() {
    std::thread::spawn(|| {
        let context = gccjit::Context::default();
        context.set_optimization_level(gccjit::OptimizationLevel::Standard);
        let mut chip8 = jit::Chip8State::new(&context);
        jit::recompile_rom(&context, &mut chip8, "CUBE8.ch8").unwrap()
    });

    video::run_display().await;
}

pub use video::cls;
