mod video;
mod jit;

#[tokio::main]
async fn main() {
    let context = gccjit::Context::default();
    context.set_optimization_level(gccjit::OptimizationLevel::Standard);
    let chip8 = jit::Chip8State::new(&context);
    jit::recompile_rom(&context, &chip8, "IBM Logo.ch8").unwrap();

    video::run_display().await;
}

pub use video::cls;
