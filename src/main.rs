#![feature(generators, generator_trait)]

use std::ops::{Generator, GeneratorState};
use std::pin::Pin;

use yokai::chars::CodeMap;
use yokai::chars::LENGTH_MAX;
use yokai::hasher::{calc_checksum, ShiftHasher};
use yokai::searcher::search;

fn main() {
    let codemap = CodeMap::new();
    let shift_hasher = ShiftHasher::new();
    //let target: [u8; 8] = calc_checksum(&shift_hasher, &codemap.codes_of("TEST").unwrap());
    let target: [u8; 8] = calc_checksum(&shift_hasher, &codemap.codes_of("MONITOR").unwrap());
    //let target: [u8; 8] = calc_checksum(&shift_hasher, &codemap.codes_of("SPEED-UP").unwrap());
    //let target: [u8; 8] = calc_checksum(&shift_hasher, &codemap.codes_of("UDADAGAWA").unwrap());
    //let target: [u8; 8] = calc_checksum(&shift_hasher, &codemap.codes_of("KOBAYASHI").unwrap());
    //let target: [u8; 8] = calc_checksum(&shift_hasher, &codemap.codes_of("OHAYOUKAWADA").unwrap());
    //let target: [u8; 8] = [0x64, 0x98, 0x0B, 0x15, 0x91, 0x18, 0xB1, 0x15]; /* 11文字 */
    //let target: [u8; 8] = [0x65, 0x94, 0x0E, 0xAC, 0xE9, 0x07, 0x33, 0x25]; /* 14文字 */
    println!(
        "target: {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}",
        target[0], target[1], target[2], target[3], target[4], target[5], target[6], target[7]
    );
    match search(&codemap, target) {
        Ok(d) => println!("FOUND !!!  passcode is {}", codemap.string_of(d).unwrap()),
        Err(_) => panic!("sorry. not found"),
    }
}
