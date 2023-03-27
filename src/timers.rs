use std::sync::RwLock;
use std::time::Instant;
use once_cell::sync::Lazy;

static DELAY_TIMER: RwLock<Lazy<Instant>> = RwLock::new(Lazy::new(|| Instant::now()));
static DELAY_VALUE: RwLock<u8> = RwLock::new(0);

#[no_mangle]
#[export_name = "stdt"]
pub extern "C" fn stdt(val: u8) {
    // eprintln!("timers: stdt");
    let mut timer = DELAY_TIMER.write().unwrap();
    let mut value = DELAY_VALUE.write().unwrap();
    **timer = Instant::now();
    *value = val;
}

#[no_mangle]
#[export_name = "lddt"]
pub extern "C" fn lddt() -> u8 {
    // eprintln!("timers: lddt");
    let timer = DELAY_TIMER.read().unwrap();
    let value = DELAY_VALUE.read().unwrap();
    *value - ((Instant::now().duration_since(**timer).as_millis() as f32 / 16.666) as u8)
}
