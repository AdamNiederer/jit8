use winit::event::VirtualKeyCode;
use std::sync::RwLock;
use std::time::Duration;

pub static KEYPAD: RwLock<[u8; 12]> = RwLock::new([0; 12]);

pub fn keycode_to_index(key: VirtualKeyCode) -> Option<usize> {
    match key {
        VirtualKeyCode::Key1 => Some(0),
        VirtualKeyCode::Key2 => Some(1),
        VirtualKeyCode::Key3 => Some(2),
        VirtualKeyCode::Q => Some(3),
        VirtualKeyCode::W => Some(4),
        VirtualKeyCode::E => Some(5),
        VirtualKeyCode::A => Some(6),
        VirtualKeyCode::S => Some(7),
        VirtualKeyCode::D => Some(8),
        VirtualKeyCode::Z => Some(9),
        VirtualKeyCode::X => Some(10),
        VirtualKeyCode::C => Some(11),
        _ => None
    }
}

#[no_mangle]
#[export_name = "kp"]
pub extern "C" fn kp(key: u8) -> u8 {
    let reader = KEYPAD.read().unwrap();
    eprintln!("kp: {}={}", key, reader[(key % 12) as usize]);
    if key >= 12 || key == 0 {
        eprintln!("input: kp invalid key: {}", key);
    }
    reader[(key % 12) as usize]
}

fn is_pressed(old: &[u8], new: &[u8]) -> Option<usize> {
    for i in 0..(old.len()) {
        if old[i] == 0 && new[i] == 1 {
            return Some(i);
        }
    }
    return None;
}

#[no_mangle]
#[export_name = "bkp"]
pub extern "C" fn bkp() -> u8 {
    eprintln!("bkp: waiting");
    let keypad_copy = KEYPAD.read().unwrap().clone();
    loop {
        if let Some(i) = is_pressed(&keypad_copy, &KEYPAD.read().unwrap().as_slice()) {
            eprintln!("bkp: {}", i);
            return i as u8;
        }
        std::thread::sleep(Duration::from_millis(1));
    }
}
