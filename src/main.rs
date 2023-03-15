mod video;
mod jit;

#[tokio::main]
async fn main() {
    video::run_display().await;
}
