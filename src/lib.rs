#![feature(generators, generator_trait)]

pub mod chars;
pub mod hasher;
pub mod searcher;

pub fn count_bits(d: u8) -> u8 {
    let mut cnt = 0;
    let mut a = d;
    while a != 0 {
        if a & 0x01 != 0 {
            cnt += 1;
        }
        a >>= 1;
    }
    cnt
}
