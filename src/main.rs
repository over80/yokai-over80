use itertools::Itertools;
use std::cmp::min;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
struct CodeMap {
    code2char: HashMap<u8, char>,
    char2code: HashMap<char, u8>,
    bits2code: HashMap<u8, Vec<u8>>,
}

fn count_bits(d: u8) -> u8 {
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

impl CodeMap {
    fn new() -> CodeMap {
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

        let mut bits2code = HashMap::new();
        for &k in code2char.keys() {
            let bits = count_bits(k);
            bits2code
                .entry(bits)
                .and_modify(|e: &mut Vec<_>| e.push(k))
                .or_insert(vec![k]);
        }

        CodeMap {
            code2char,
            char2code,
            bits2code,
        }
    }

    fn code_of(&self, c: char) -> Option<&u8> {
        self.char2code.get(&c)
    }

    fn char_of(&self, d: u8) -> Option<&char> {
        self.code2char.get(&d)
    }

    fn codes_of(&self, string: &str) -> Result<Vec<u8>, String> {
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

    fn string_of(&self, codes: Vec<u8>) -> Result<String, ()> {
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

    fn generate_charset(&self, bits: u8, length: u8) -> Vec<Vec<u8>> {
        let mut result = Vec::new();
        let codes = self.bits2code.get(&bits).unwrap();
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

fn ror(num: &mut u8, carry: &mut bool) {
    let next_carry = (*num & 0x01) != 0;
    *num >>= 1;
    if *carry {
        *num |= 0x80;
    }
    *carry = next_carry;
}

fn ror16(num: &mut u16, carry: &mut bool) {
    let next_carry = (*num & 0x0001) != 0;
    *num >>= 1;
    if *carry {
        *num |= 0x8000;
    }
    *carry = next_carry;
}

#[allow(dead_code)]
fn rol(num: &mut u8, carry: &mut bool) {
    let next_carry = (*num & 0x80) != 0;
    *num <<= 1;
    if *carry {
        *num |= 0x01;
    }
    *carry = next_carry;
}

fn asl(num: &mut u8, carry: &mut bool) {
    *carry = (*num & 0x80) != 0;
    *num <<= 1;
}

#[cfg(test)]
#[test]
fn test_asm_rotate() {
    let mut a: u8;
    let mut c: bool;
    a = 0x84;
    c = true;
    ror(&mut a, &mut c);
    assert_eq!((a, c), (0xc2, false));
    ror(&mut a, &mut c);
    assert_eq!((a, c), (0x61, false));
    ror(&mut a, &mut c);
    assert_eq!((a, c), (0x30, true));
    rol(&mut a, &mut c);
    assert_eq!((a, c), (0x61, false));
    rol(&mut a, &mut c);
    assert_eq!((a, c), (0xc2, false));
    rol(&mut a, &mut c);
    assert_eq!((a, c), (0x84, true));
    asl(&mut a, &mut c);
    assert_eq!((a, c), (0x08, true));
    asl(&mut a, &mut c);
    assert_eq!((a, c), (0x10, false));
}

fn adc(num1: u8, num2: u8, carry: &mut bool) -> u8 {
    let num = num1 as u16 + num2 as u16 + *carry as u16;
    if num > 0xff {
        *carry = true;
        (num & 0xff) as u8
    } else {
        *carry = false;
        num as u8
    }
}
#[cfg(test)]
#[test]
fn test_asm_adc() {
    let mut c: bool = true;
    assert_eq!(adc(0xa5, 0x5a, &mut c), 0x00);
    assert_eq!(c, true);
    c = false;
    assert_eq!(adc(0xa5, 0x5a, &mut c), 0xff);
    assert_eq!(c, false);
}

fn calc_checksum(codes: &Vec<u8>) -> [u8; 8] {
    let mut m_31f4_31f5 = 0u16;
    let m_31f6 = codes.len() as u8; // 文字数で固定
    let mut m_31f7 = 0u8;
    let mut m_31f8 = 0u8;
    let mut m_31f9 = 0u8;
    let mut m_31fa = 1u8;
    let mut m_31fb = 0u8;

    for &code in codes {
        // check 1
        let mut a = code;
        let mut carry = false;
        for _ in 0..8 {
            asl(&mut a, &mut carry);
            ror16(&mut m_31f4_31f5, &mut carry);
            if carry {
                m_31f4_31f5 ^= 0x8408;
            }
        }
        let m_31f4 = (m_31f4_31f5 >> 8) as u8;
        let m_31f5 = (m_31f4_31f5 & 0xff) as u8;

        // check 2
        carry = m_31f4 >= 0xe5;
        m_31f7 = adc(code, m_31f7, &mut carry);
        m_31f8 = adc(m_31f8, m_31f5, &mut carry);

        // check 3
        m_31f9 ^= code;

        // check 4
        ror(&mut m_31fa, &mut carry);
        m_31fa = adc(m_31fa, code, &mut carry);

        // check 5
        m_31fb += carry as u8;
        m_31fb += count_bits(code);
    }

    let m_31f4 = (m_31f4_31f5 >> 8) as u8;
    let m_31f5 = (m_31f4_31f5 & 0xff) as u8;
    [
        m_31f4, m_31f5, m_31f6, m_31f7, m_31f8, m_31f9, m_31fa, m_31fb,
    ]
}

#[cfg(test)]
#[test]
fn test_calc_checksum() {
    let codemap = CodeMap::new();
    let codes = codemap.codes_of("SPEED-UP").unwrap();
    assert_eq!(
        calc_checksum(&codes),
        [0xED, 0x26, 0x08, 0xEE, 0x3D, 0x23, 0x1D, 0x12]
    );
}

fn listup_bit_distribution(num_bits: u8, num_chars: u8) -> Vec<[u8; 5]> {
    let mut result = Vec::new();
    let rem_bits = num_bits as i32;
    let rem_chars = num_chars as i32;
    for s4 in 0..(min(rem_bits / 4, rem_chars) + 1) {
        let (rem_bits, rem_chars) = (rem_bits - s4 * 4, rem_chars - s4);
        for s3 in 0..(min(rem_bits / 3, rem_chars) + 1) {
            let (rem_bits, rem_chars) = (rem_bits - s3 * 3, rem_chars - s3);
            for s2 in 0..(min(rem_bits / 2, rem_chars) + 1) {
                let (rem_bits, rem_chars) = (rem_bits - s2 * 2, rem_chars - s2);
                for s1 in 0..(min(rem_bits / 1, rem_chars) + 1) {
                    let (rem_bits, rem_chars) = (rem_bits - s1 * 1, rem_chars - s1);
                    if rem_bits >= 0 && rem_chars >= 0 {
                        let s0 = rem_chars;
                        result.push([s0 as u8, s1 as u8, s2 as u8, s3 as u8, s4 as u8]);
                    }
                }
            }
        }
    }
    result
}

#[cfg(test)]
#[test]
fn test_bit_dist() {
    assert_eq!(
        listup_bit_distribution(4, 5),
        vec![
            [5, 0, 0, 0, 0],
            [4, 1, 0, 0, 0],
            [3, 2, 0, 0, 0],
            [2, 3, 0, 0, 0],
            [1, 4, 0, 0, 0],
            [4, 0, 1, 0, 0],
            [3, 1, 1, 0, 0],
            [2, 2, 1, 0, 0],
            [3, 0, 2, 0, 0],
            [4, 0, 0, 1, 0],
            [3, 1, 0, 1, 0],
            [4, 0, 0, 0, 1],
        ]
    );
}

fn search(codemap: &CodeMap, target: [u8; 8]) -> Result<Vec<u8>, ()> {
    let pass_length = target[2]; // パスコードの文字列長
    let pass_xor = target[5]; // パスコードの XOR 総和
    let pass_bitsum = target[7]; // パスコードの立っているビットの総和 + α(最大 pass_length)

    let dist_pattern = listup_bit_distribution(pass_bitsum, pass_length);
    for s in &dist_pattern {
        let remaining_bitsum = pass_bitsum - s[1] - s[2] * 2 - s[3] * 3 - s[4] * 4;
        if remaining_bitsum > pass_length {
            continue;
        }

        print!("{:?} ", s);
        let cc = s
            .iter()
            .enumerate()
            .map(|(i, &si)| codemap.generate_charset(i as u8, si))
            .collect::<Vec<Vec<_>>>();
        print!("{:?} ", cc.iter().map(|c| c.len()).collect::<Vec<_>>());
        println!("{:?}", cc.iter().map(|c| c.len()).product::<usize>());

        for ss in cc.iter().map(|c| c.iter()).multi_cartesian_product() {
            //println!("{:?}", ss);
            let chars = ss.iter().flat_map(|&s| s).collect::<Vec<&u8>>();
            if chars.iter().fold(0, |a, &b| a ^ b) != pass_xor {
                continue;
            }

            let mut checked = HashSet::<Vec<u8>>::new();

            for passcode in chars.iter().permutations(chars.len()) {
                let passcode = passcode.iter().map(|&&&d| d).collect::<Vec<u8>>();
                if checked.contains(&passcode) {
                    continue;
                }
                checked.insert(passcode.to_vec());
                //println!("TRYING {:?}", passcode);
                let checksum = calc_checksum(&passcode);
                if checksum == target {
                    return Ok(passcode);
                }
            }
        }
    }
    Err(())
}

fn main() {
    let codemap = CodeMap::new();
    let target: [u8; 8] = calc_checksum(&codemap.codes_of("SPEED-UP").unwrap());
    match search(&codemap, target) {
        Ok(d) => println!("FOUND !!!  passcode is {}", codemap.string_of(d).unwrap()),
        Err(_) => panic!(),
    }
}
