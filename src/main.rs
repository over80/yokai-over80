#![feature(generators, generator_trait)]

use itertools::izip;
use itertools::Itertools;
use std::cmp::min;
use std::collections::{HashMap, HashSet};
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;

const LENGTH_MAX: usize = 16;

#[derive(Debug)]
struct CodeMap {
    code2char: HashMap<u8, char>,
    char2code: HashMap<char, u8>,
    bits2code: [Vec<u8>; 5],
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

    fn generate_codes_from_location<'a>(
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

#[derive(Copy, Clone)]
struct ReversedShiftHashValue(u8, u8);

impl ReversedShiftHashValue {
    fn new() -> ReversedShiftHashValue {
        ReversedShiftHashValue(0, 0)
    }

    fn reverse(d: u8) -> u8 {
        let mut d = d;
        d = (d & 0xf0) >> 4 | (d & 0x0f) << 4;
        d = (d & 0xcc) >> 2 | (d & 0x33) << 2;
        d = (d & 0xaa) >> 1 | (d & 0x55) << 1;
        d
    }

    fn from(d: u8, e: u8) -> ReversedShiftHashValue {
        ReversedShiftHashValue(Self::reverse(d), Self::reverse(e))
    }

    fn as_normal(&self) -> (u8, u8) {
        (Self::reverse(self.0), Self::reverse(self.1))
    }
}

#[cfg(test)]
#[test]
fn test_reverse() {
    assert_eq!(ReversedShiftHashValue::reverse(0x01), 0x80);
    assert_eq!(ReversedShiftHashValue::reverse(0x02), 0x40);
    assert_eq!(ReversedShiftHashValue::reverse(0x04), 0x20);
    assert_eq!(ReversedShiftHashValue::reverse(0x08), 0x10);
    assert_eq!(ReversedShiftHashValue::reverse(0x10), 0x08);
    assert_eq!(ReversedShiftHashValue::reverse(0x20), 0x04);
    assert_eq!(ReversedShiftHashValue::reverse(0x40), 0x02);
    assert_eq!(ReversedShiftHashValue::reverse(0x80), 0x01);
}

#[cfg(test)]
#[test]
fn test_rev_value_type() {
    let r = ReversedShiftHashValue::from(0x12, 0x34);
    assert_eq!((r.0, r.1), (0x48, 0x2c));
    assert_eq!(r.as_normal(), (0x12, 0x34));
}

struct ShiftHasher {
    map: [(u8, u8); 256],
    rmap: [(u8, u8); 256], // map.0 -> (index, map.1)
}

impl ShiftHasher {
    fn new() -> ShiftHasher {
        let mut map = [(0u8, 0u8); 256];
        for (i, p) in map.iter_mut().enumerate() {
            let mut d = (i as u32) << 8;
            for _ in 0..8 {
                d <<= 1;
                if d & 0x1_00_00 != 0 {
                    d ^= 0x10_21/* reverse of 0x84_08 */;
                }
            }
            let d = (d & 0xffff) as u16;
            *p = ((d & 0xff) as u8, (d >> 8) as u8)
        }
        let mut rmap = [(0u8, 0u8); 256];
        for (ri, rp) in rmap.iter_mut().enumerate() {
            for (index, v) in map.iter().enumerate() {
                if ri as u8 == v.0 {
                    *rp = (index as u8, v.1)
                }
            }
        }
        ShiftHasher { map, rmap }
    }

    fn progress(&self, value: &mut ReversedShiftHashValue, code: u8) {
        let ub = value.1;
        let lb = value.0;
        let mv = self.map[ub as usize];
        value.0 = mv.0 ^ code;
        value.1 = mv.1 ^ lb;
    }

    fn backward(&self, value: &mut ReversedShiftHashValue, code: u8) {
        let mv0 = value.0 ^ code;
        let (ub, mv1) = self.rmap[mv0 as usize];
        let lb = value.1 ^ mv1;
        value.1 = ub;
        value.0 = lb;
    }
}

#[cfg(test)]
#[test]
fn test_shift_hasher() {
    let sh = ShiftHasher::new();
    for (i, d) in sh.map.iter().enumerate() {
        println!("{:02x} {:08b} {:08b}", i, d.0, d.1);
    }
    let codemap = CodeMap::new();
    let codes = codemap.codes_of("SPEED-UP").unwrap();
    let mut hv = ReversedShiftHashValue::new();
    for d in &codes {
        sh.progress(&mut hv, *d);
        println!("in({:02x}) {:02x} {:02x}", d, hv.0, hv.1);
    }
    assert_eq!(hv.as_normal(), (0xED, 0x26));

    for d in codes.iter().rev() {
        sh.backward(&mut hv, *d);
        println!("out({:02x}) {:02x} {:02x}", d, hv.0, hv.1);
    }
    assert_eq!(hv.as_normal(), (0x00, 0x00));
}

fn calc_checksum(shift_hasher: &ShiftHasher, codes: &Vec<u8>) -> [u8; 8] {
    let mut m_31f4_31f5 = ReversedShiftHashValue::new();
    let m_31f6 = codes.len() as u8; // 文字数で固定
    let mut m_31f7 = 0u8;
    let mut m_31f8 = 0u8;
    let mut m_31f9 = 0u8;
    let mut m_31fa = 1u8;
    let mut m_31fb = 0u8;

    for &code in codes {
        // check 1
        shift_hasher.progress(&mut m_31f4_31f5, code);
        let (m_31f4, m_31f5) = m_31f4_31f5.as_normal();

        // check 2
        let mut carry = m_31f4 >= 0xe5;
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

    let (m_31f4, m_31f5) = m_31f4_31f5.as_normal();
    [
        m_31f4, m_31f5, m_31f6, m_31f7, m_31f8, m_31f9, m_31fa, m_31fb,
    ]
}

#[cfg(test)]
#[test]
fn test_calc_checksum() {
    let codemap = CodeMap::new();
    let shift_hasher = ShiftHasher::new();
    let codes = codemap.codes_of("SPEED-UP").unwrap();
    assert_eq!(
        calc_checksum(&shift_hasher, &codes),
        [0xED, 0x26, 0x08, 0xEE, 0x3D, 0x23, 0x1D, 0x12]
    );
}

fn generate_bit_distribution(num_bits: u8, num_chars: u8) -> impl Generator<Yield = [u8; 5]> {
    move || {
        let rem_bits = num_bits as i32;
        let rem_chars = num_chars as i32;
        for s4 in (0..(min(rem_bits / 4, rem_chars) + 1)).rev() {
            let (rem_bits, rem_chars) = (rem_bits - s4 * 4, rem_chars - s4);
            for s3 in (0..(min(rem_bits / 3, rem_chars) + 1)).rev() {
                let (rem_bits, rem_chars) = (rem_bits - s3 * 3, rem_chars - s3);
                for s2 in (0..(min(rem_bits / 2, rem_chars) + 1)).rev() {
                    let (rem_bits, rem_chars) = (rem_bits - s2 * 2, rem_chars - s2);
                    for s1 in (0..(min(rem_bits / 1, rem_chars) + 1)).rev() {
                        let (rem_bits, rem_chars) = (rem_bits - s1 * 1, rem_chars - s1);
                        if rem_bits >= 0 && rem_chars >= 0 {
                            let s0 = rem_chars;
                            yield [s0 as u8, s1 as u8, s2 as u8, s3 as u8, s4 as u8];
                        }
                    }
                }
            }
        }
    }
}

fn listup_bit_distribution(num_bits: u8, num_chars: u8) -> Vec<[u8; 5]> {
    let mut result = Vec::new();
    let mut gen = generate_bit_distribution(num_bits, num_chars);
    while let GeneratorState::Yielded(dist) = Pin::new(&mut gen).resume(()) {
        result.push(dist);
    }
    result
}

#[cfg(test)]
#[test]
fn test_bit_dist() {
    let target = vec![
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
    .sort();

    assert_eq!(listup_bit_distribution(4, 5).sort(), target);
}

fn generate_bit_locations(
    distribution: [u8; 5],
    total: u8,
) -> (impl Generator<Yield = Vec<u8>>, usize) {
    let choice_max_rev = distribution
        .iter()
        .rev()
        .scan(total, |rest, &d| {
            let current = *rest;
            *rest -= d;
            Some(current)
        })
        .collect::<Vec<_>>();

    let char_iters_rev = izip!(&choice_max_rev, distribution.iter().rev())
        .map(|(&cmax, &cnum)| (0..cmax).combinations(cnum as usize).collect())
        .collect::<Vec<Vec<_>>>();

    let count = char_iters_rev.iter().map(|chars| chars.len()).product();
    let generator = move || {
        for choosed_indexes_for_each_num_rev in char_iters_rev.into_iter().multi_cartesian_product()
        {
            let mut result = (0..total).map(|_| 0).collect_vec();
            for (i, choosed_indexes_for_i) in
                izip!((1..total).rev(), choosed_indexes_for_each_num_rev)
            // i = 0 は省略する
            {
                let mut iter = result.iter_mut();
                let mut skipped_count = 0;
                for choosed_index in choosed_indexes_for_i {
                    while choosed_index > skipped_count {
                        let mut d = iter.next().unwrap();
                        while *d != 0 {
                            d = iter.next().unwrap();
                        }
                        skipped_count += 1;
                    }
                    let mut d = iter.next().unwrap();
                    while *d != 0 {
                        d = iter.next().unwrap();
                    }
                    *d = i;
                    skipped_count += 1;
                }
            }
            yield result.clone();
        }
    };
    (generator, count)
}

#[cfg(test)]
#[test]
fn test_bit_loc() {
    let pat: [u8; 5] = [2, 0, 2, 0, 1];
    let (mut gen, count) = generate_bit_locations(pat, pat.iter().sum());
    let len_expected = 5 * (4 * 3 / 2); /* 5C1 x 4C2 */
    assert_eq!(count, len_expected);
    let mut count = 0;
    while let GeneratorState::Yielded(data) = Pin::new(&mut gen).resume(()) {
        println!("{}: {:?}", count, data);
        for i in 0..5 {
            assert_eq!(
                data.iter().filter(|&&d| d == i).count(),
                pat[i as usize] as usize
            );
        }
        count += 1;
    }
    assert_eq!(count, len_expected);
}

// ２つに分ける分け方のパターンを返す。（連続している同じ文字に対する対応入り）
fn generate_split_in_two(chars: Vec<u8>) -> impl Generator<Yield = (Vec<u8>, Vec<u8>)> {
    let l = chars.len();
    move || {
        for pat in (0..l).combinations(l / 2) {
            // TODO: 流石に無駄が多い。要修正
            let mut pat_indexed = [false; LENGTH_MAX];
            let mut iter = pat.iter().peekable();
            for (i, p) in (&mut pat_indexed[0..l]).into_iter().enumerate() {
                if let Some(&&ii) = iter.peek() {
                    if i == ii {
                        iter.next();
                        *p = true;
                    }
                }
            }
            let mut t = izip!(&chars, &pat_indexed);
            let mut tprev = t.clone();
            if izip!(tprev, t.skip(1)).all(|((&c_prev, &b_prev), (&c, &b))| {
                !(c_prev == c && b_prev == false && b == true) /* 同じ文字は前から順に抜く場合のみOK */
            }) {
                let (l, r) : (Vec<(&u8, bool)>,Vec<(&u8, bool)>) = izip!(&chars, pat_indexed).partition(|(_, b)| *b);
                let l = l.into_iter().map(|(&c,_)|c).collect_vec();
                let r = r.into_iter().map(|(&c,_)|c).collect_vec();
                
                yield (r, l);
            }
        }
    }
}

#[cfg(test)]
#[test]
fn test_split_in_two() {
    let chars: Vec<u8> = vec![10, 10, 20];
    let mut gen = generate_split_in_two(chars);
    let mut result = vec![];
    while let GeneratorState::Yielded(v) = Pin::new(&mut gen).resume(()) {
        result.push(v)
    }
    result.sort();
    assert_eq!(
        result,
        vec![(vec![10, 10], vec![20]), (vec![10, 20], vec![10])]
    );
}

fn generate_permutations_wo_dup(chars: Vec<u8>) -> impl Generator<Yield = Vec<u8>> {
    let l = chars.len();
    let mut charnums = Vec::with_capacity(l);
    let mut total = 0usize;
    let mut count = 0usize;
    let mut prev = 0;
    for &c in &chars {
        if c == prev {
            count += 1;
            total += 1;
        } else {
            if count > 0 {
                charnums.push((prev, count, total));
            }
            prev = c;
            count = 1;
            total += 1;
        }
    }
    if count > 0 {
        charnums.push((prev, count, total));
    }

    move || {
        let iters = charnums
            .iter()
            .map(|&(_, count, total)| (0..total).combinations(count));
        let mut char_uniq = [0u8; LENGTH_MAX];
        charnums
            .iter()
            .map(|&(c, _, _)| c)
            .zip(&mut char_uniq)
            .for_each(|(c, p)| *p = c);

        for pat in iters.multi_cartesian_product() {
            let mut code = Vec::with_capacity(l);
            for (c, ps) in izip!(char_uniq, pat) {
                for p in ps {
                    code.insert(p, c);
                }
            }
            yield code;
        }
    }
}

#[cfg(test)]
#[test]
fn test_permutations_wo_dup() {
    let mut gen = generate_permutations_wo_dup(vec![10, 10, 20, 30]);
    let mut result = Vec::new();
    while let GeneratorState::Yielded(code) = Pin::new(&mut gen).resume(()) {
        result.push(code);
    }
    result.sort();
    assert_eq!(
        result,
        vec![
            vec![10, 10, 20, 30],
            vec![10, 10, 30, 20],
            vec![10, 20, 10, 30],
            vec![10, 20, 30, 10],
            vec![10, 30, 10, 20],
            vec![10, 30, 20, 10],
            vec![20, 10, 10, 30],
            vec![20, 10, 30, 10],
            vec![20, 30, 10, 10],
            vec![30, 10, 10, 20],
            vec![30, 10, 20, 10],
            vec![30, 20, 10, 10],
        ]
    );
}

fn search(codemap: &CodeMap, target: [u8; 8]) -> Result<Vec<u8>, ()> {
    let shift_hasher = ShiftHasher::new();
    let pass_length = target[2]; // パスコードの文字列長
    let pass_sum = target[3]; // パスコードの総和 + α(最大 pass_length)
    let pass_xor = target[5]; // パスコードの XOR 総和
    let pass_bitsum = target[7]; // パスコードの立っているビットの総和 + α(最大 pass_length)
    let pass_xor_bits_odd = (count_bits(pass_xor) % 2) == 1;

    let mut dist_gen = generate_bit_distribution(pass_bitsum, pass_length);
    while let GeneratorState::Yielded(s) = Pin::new(&mut dist_gen).resume(()) {
        let used_bits = s[1] + s[2] * 2 + s[3] * 3 + s[4] * 4;
        if (used_bits % 2 == 1) != pass_xor_bits_odd {
            // 使用した1ビットの数と xor の1ビットの数の偶奇は一致しないとダメ
            continue;
        }
        let remaining_bitsum = pass_bitsum - used_bits;
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
            let chars = ss.iter().flat_map(|&s| s).map(|&s| s).collect::<Vec<u8>>();
            if chars.iter().fold(0, |a, &b| a ^ b) != pass_xor {
                continue;
            }
            let csum = (chars.iter().map(|&d| d as u16).sum::<u16>() & 0xff) as u8;
            if pass_sum.wrapping_sub(csum) >= pass_length {
                continue;
            }

            let mut splitgen = generate_split_in_two(chars);
            while let GeneratorState::Yielded((left, right)) = Pin::new(&mut splitgen).resume(()) {
                let mut codegen = generate_permutations_wo_dup(left);
                while let GeneratorState::Yielded(passcode_l) = Pin::new(&mut codegen).resume(()) {
                    let mut hv = ReversedShiftHashValue::new();
                    for d in &passcode_l {
                        shift_hasher.progress(&mut hv, *d);
                    }

                    let mut codegen = generate_permutations_wo_dup(right.clone());
                    while let GeneratorState::Yielded(passcode_r) = Pin::new(&mut codegen).resume(()) {
                        let mut hv = hv.clone();
                        for d in &passcode_r {
                            shift_hasher.progress(&mut hv, *d);
                        }
                        if hv.as_normal() != (target[0], target[1]) {
                            continue;
                        }

                        // 本確認
                        let mut passcode = vec![];
                        passcode.append(&mut passcode_l.clone());
                        passcode.append(&mut passcode_r.clone());
                        //println!("TRYING {:?}", passcode);
                        let checksum = calc_checksum(&shift_hasher, &passcode);
                        if checksum == target {
                            return Ok(passcode);
                        }
                    }
                }
            }
        }
    }
    Err(())
}

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
