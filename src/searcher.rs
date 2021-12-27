use itertools::izip;
use itertools::Itertools;
use std::cmp::min;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;

use crate::algo::count_bits;
use crate::chars::CodeMap;
use crate::chars::LENGTH_MAX;
use crate::hasher::{calc_checksum, ReversedShiftHashValue, ShiftHasher};

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

#[allow(dead_code)]
fn listup_bit_distribution(num_bits: u8, num_chars: u8) -> Vec<[u8; 5]> {
    let mut result = Vec::new();
    let mut gen = generate_bit_distribution(num_bits, num_chars);
    while let GeneratorState::Yielded(dist) = Pin::new(&mut gen).resume(()) {
        result.push(dist);
    }
    result
}

#[allow(dead_code)]
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

// ２つに分ける分け方のパターンを返す。（連続している同じ文字に対する対応入り）
fn generate_split_in_two(chars: Vec<u8>) -> impl Generator<Yield = (Vec<u8>, Vec<u8>)> {
    let l = chars.len();
    move || {
        for pat in (0..l).combinations(min(l / 2, 1 /* FIXME: なぜか多いほど遅い */)) {
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
            let t = izip!(&chars, &pat_indexed);
            let tprev = t.clone();
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

pub fn search(codemap: &CodeMap, target: [u8; 8]) -> Result<Vec<u8>, ()> {
    let shift_hasher = ShiftHasher::new();
    let pass_length = target[2]; // パスコードの文字列長
    let pass_sum = target[3]; // パスコードの総和 + α(最大 pass_length)
    let pass_xor = target[5]; // パスコードの XOR 総和
    let pass_bitsum = target[7]; // パスコードの立っているビットの総和 + α(最大 pass_length)
    let pass_xor_bits_odd = (count_bits(pass_xor) % 2) == 1;

    // 逆探索用バッファ
    const BUF_LEN: usize = 100;
    let mut rmap: [[Vec<[u8; LENGTH_MAX / 2]>; 256]; 256] =
        array_init::array_init(|_| array_init::array_init(|_| Vec::with_capacity(BUF_LEN)));

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
                // 逆方向探索
                for p in rmap.iter_mut().flat_map(|p| p.iter_mut()) {
                    p.resize(0, Default::default());
                }
                let mut codegen_r = generate_permutations_wo_dup(right.clone());
                while let GeneratorState::Yielded(passcode_r) = Pin::new(&mut codegen_r).resume(())
                {
                    let mut hv = ReversedShiftHashValue::from(target[0], target[1]);
                    for d in &passcode_r {
                        shift_hasher.backward(&mut hv, *d);
                    }
                    let mut passcode_r_array = [0u8; LENGTH_MAX / 2];
                    passcode_r
                        .iter()
                        .zip(&mut passcode_r_array)
                        .for_each(|(&d, p)| *p = d);
                    if rmap[hv.0 as usize][hv.1 as usize].len() >= BUF_LEN {
                        for line in &rmap {
                            for col in line {
                                print!("{:02} ", col.len());
                            }
                            println!("");
                        }
                        panic!();
                    };
                    rmap[hv.0 as usize][hv.1 as usize].push(passcode_r_array);
                }

                // 順方向探索
                let mut codegen_l = generate_permutations_wo_dup(left);
                while let GeneratorState::Yielded(passcode_l) = Pin::new(&mut codegen_l).resume(())
                {
                    let mut hv = ReversedShiftHashValue::new();
                    for d in &passcode_l {
                        shift_hasher.progress(&mut hv, *d);
                    }

                    for passcode_r in &rmap[hv.0 as usize][hv.1 as usize] {
                        // 本確認
                        let mut passcode = vec![];
                        for &c in &passcode_l {
                            passcode.push(c);
                        }
                        for &c in passcode_r
                            .iter()
                            .take(pass_length as usize - passcode_l.len())
                            .rev()
                        {
                            passcode.push(c);
                        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bit_dist() {
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

    #[test]
    fn bit_loc() {
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

    #[test]
    fn split_in_two() {
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

    #[test]
    fn permutations_wo_dup() {
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
}
