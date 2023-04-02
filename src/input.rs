use winit::event::VirtualKeyCode;
use std::sync::RwLock;
use std::time::Duration;

pub static KEYPAD: RwLock<[u8; 16]> = RwLock::new([0; 16]);

pub fn keycode_to_index(key: VirtualKeyCode) -> Option<usize> {
    match key {
        VirtualKeyCode::Key1 => Some(0x0),
        VirtualKeyCode::Key2 => Some(0x1),
        VirtualKeyCode::Key3 => Some(0x2),
        VirtualKeyCode::Key4 => Some(0xC),
        VirtualKeyCode::Q => Some(0x3),
        VirtualKeyCode::W => Some(0x4),
        VirtualKeyCode::E => Some(0x5),
        VirtualKeyCode::R => Some(0xD),
        VirtualKeyCode::A => Some(0x6),
        VirtualKeyCode::S => Some(0x7),
        VirtualKeyCode::D => Some(0x8),
        VirtualKeyCode::F => Some(0xE),
        VirtualKeyCode::Z => Some(0x9),
        VirtualKeyCode::X => Some(0xA),
        VirtualKeyCode::C => Some(0xB),
        VirtualKeyCode::V => Some(0xF),
        _ => None
    }
}

#[no_mangle]
#[export_name = "kp"]
#[inline(never)]
pub extern "C" fn kp(key: u8) -> u8 {
    let reader = KEYPAD.read().unwrap();
    if key >= 16 {
        eprintln!("input: kp invalid key: {}", key);
    }
    eprintln!("kp: {}={}", key, reader[(key % 16) as usize]);
    let ret = reader[(key % 16) as usize];
    drop(reader);
    std::thread::sleep(std::time::Duration::from_millis(10));
    ret
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
