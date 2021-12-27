use itertools::Itertools;
use std::collections::HashMap;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;

use crate::count_bits;

pub const LENGTH_MAX: usize = 16;

#[derive(Debug)]
pub struct CodeMap {
    code2char: HashMap<u8, char>,
    char2code: HashMap<char, u8>,
    bits2code: [Vec<u8>; 5],
}

impl CodeMap {
    pub fn new() -> CodeMap {
        let mut code2char = HashMap::new();
        let mut char2code = HashMap::new();
        let table = vec![
            (0x00u8, "AHOV16"),
            (0x08u8, "BIPW27"),
            (0x10u8, "CJQX38"),
            (0x18u8, "DKRY49"),
            (0x20u8, "ELSZ50"),
            (0x28u8, "FMT-な!"),
            (0x30u8, "GNU.むこ"),
        ];
        for (start, chars) in table {
            for (i, c) in chars.chars().enumerate() {
                let index = start + (i as u8);
                code2char.insert(index, c);
                char2code.insert(c, index);
            }
        }
        assert_eq!(code2char.len(), 6 * 7);
        assert_eq!(char2code.len(), 6 * 7);

        let mut bits2code = [vec![], vec![], vec![], vec![], vec![]];
        for &k in code2char.keys() {
            let bits = count_bits(k);
            bits2code[bits as usize].push(k);
        }
        CodeMap {
            code2char,
            char2code,
            bits2code,
        }
    }

    pub fn code_of(&self, c: char) -> Option<&u8> {
        self.char2code.get(&c)
    }

    pub fn char_of(&self, d: u8) -> Option<&char> {
        self.code2char.get(&d)
    }

    pub fn codes_of(&self, string: &str) -> Result<Vec<u8>, String> {
        string
            .chars()
            .map(|c| {
                if let Some(&d) = self.code_of(c) {
                    Ok(d)
                } else {
                    Err(format!("unknown letter: {}", c))
                }
            })
            .collect()
    }

    pub fn string_of(&self, codes: Vec<u8>) -> Result<String, ()> {
        codes
            .iter()
            .map(|&d| {
                if let Some(&c) = self.char_of(d) {
                    Ok(c)
                } else {
                    Err(())
                }
            })
            .collect()
    }

    pub fn generate_charset(&self, bits: u8, length: u8) -> Vec<Vec<u8>> {
        let mut result = Vec::new();
        let codes = &self.bits2code[bits as usize];
        let mut index = [0usize; 20];
        assert_eq!(length <= 20, true);
        let index = &mut index[0..(length as usize)];

        loop {
            result.push(index.iter().map(|&i| codes[i]).collect());

            let first_incrementable = index
                .iter()
                .enumerate()
                .find(|(_i, &ii)| ii + 1 < codes.len());
            match first_incrementable {
                Some((i, _)) => {
                    index[i] += 1;
                    let n = index[i];
                    for ii in &mut index[0..i] {
                        *ii = n;
                    }
                }
                None => break,
            }
        }

        result
    }

    pub fn generate_codes_from_location<'a>(
        &'a self,
        location: &'a Vec<u8>,
    ) -> (impl Generator<Yield = Vec<u8>> + 'a, usize) {
        let count = location
            .iter()
            .map(|&b| self.bits2code[b as usize].len())
            .product();
        let generator = move || {
            for p in location
                .iter()
                .map(|&b| &self.bits2code[b as usize])
                .multi_cartesian_product()
            {
                yield p.iter().map(|&&d| d).collect_vec();
            }
        };
        (generator, count)
    }
}

#[cfg(test)]
#[test]
fn test_code_map() {
    let codemap = CodeMap::new();
    assert_eq!(codemap.code_of('こ'), Some(&0x35));
    assert_eq!(codemap.char_of(0x35), Some(&'こ'));
    assert_eq!(codemap.codes_of("NEC"), Ok(vec![0x31, 0x20, 0x10]));
    assert_eq!(codemap.codes_of("nec"), Err("unknown letter: n".into()));
    assert_eq!(codemap.string_of(vec![0x31, 0x20, 0x10]), Ok("NEC".into()));
}

#[cfg(test)]
#[test]
fn test_generate_charset() {
    let codemap = CodeMap::new();
    assert_eq!(codemap.generate_charset(0, 2), vec![vec![0, 0]]);
    let mut r = codemap.generate_charset(1, 2);
    for s in &mut r {
        s.sort();
    }
    r.sort();
    assert_eq!(
        r,
        vec![
            [1, 1],
            [1, 2],
            [1, 4],
            [1, 8],
            [1, 16],
            [1, 32],
            [2, 2],
            [2, 4],
            [2, 8],
            [2, 16],
            [2, 32],
            [4, 4],
            [4, 8],
            [4, 16],
            [4, 32],
            [8, 8],
            [8, 16],
            [8, 32],
            [16, 16],
            [16, 32],
            [32, 32],
        ]
    );
}

#[cfg(test)]
#[test]
fn test_generate_code_from_location() {
    let codemap = CodeMap::new();
    let location = vec![0, 1, 0];
    let (mut gen, count) = codemap.generate_codes_from_location(&location);
    assert_eq!(count, 6);
    let mut result = Vec::new();
    while let GeneratorState::Yielded(code) = Pin::new(&mut gen).resume(()) {
        println!("{:?}", code);
        result.push(code);
    }
    result.sort();
    assert_eq!(
        result,
        vec![
            vec![0, 1, 0],
            vec![0, 2, 0],
            vec![0, 4, 0],
            vec![0, 8, 0],
            vec![0, 16, 0],
            vec![0, 32, 0]
        ]
    )
}
