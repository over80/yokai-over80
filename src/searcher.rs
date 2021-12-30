use itertools::izip;
use itertools::Itertools;
use std::cmp::min;
use std::ops::{Generator, GeneratorState};
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use std::thread;

use crate::algo::*;
use crate::chars::CodeMap;
use crate::chars::LENGTH_MAX;
use crate::hasher::{calc_checksum, ReversedShiftHashValue, ShiftHasher};

fn generate_bit_distribution(num_bits: u8, num_chars: u8) -> impl Generator<Yield = [u8; 5]> {
    move || {
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
        for pat in (0..l).combinations(min(l / 2, 8 /* FIXME: なぜか多いほど遅い */)) {
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

const BUF_LEN: usize = 64;

fn search_one_dist(
    codemap: &CodeMap,
    shift_hasher: &ShiftHasher,
    rmap: &mut [[[[u8; LENGTH_MAX / 2]; BUF_LEN]; 256]; 256],
    rmap_used: &mut [[usize; 256]; 256],
    dist: &[u8; 5],
    target: &[u8; 8],
    prefix_codes: &Vec<u8>,
    pass_length: u8,
    pass_sum: u8,
    prefix_pass_sum: u8,
    prefix_hv: ReversedShiftHashValue,
    nonprefix_pass_length: u8,
    nonprefix_pass_xor: u8,
    nonprefix_pass_bitsum: u8,
    nonprefix_pass_xor_bits_odd: bool,
) -> Vec<Vec<u8>> {
    let mut result = vec![];
    let used_bits = dist[1] + dist[2] * 2 + dist[3] * 3 + dist[4] * 4;
    if (used_bits % 2 == 1) != nonprefix_pass_xor_bits_odd {
        // 使用した1ビットの数と xor の1ビットの数の偶奇は一致しないとダメ
        return vec![];
    }
    let remaining_bitsum = nonprefix_pass_bitsum - used_bits;
    if remaining_bitsum > (pass_length/* carry 許容数 */) {
        return vec![];
    }

    let cc = dist
        .iter()
        .enumerate()
        .map(|(i, &si)| codemap.generate_charset(i as u8, si))
        .collect::<Vec<Vec<_>>>();

    println!(
        "{:?}   {:?} -> {}",
        dist,
        cc.iter().map(|c| c.len()).collect::<Vec<_>>(),
        cc.iter().map(|c| c.len()).product::<usize>()
    );

    for ss in cc.iter().map(|c| c.iter()).multi_cartesian_product() {
        let chars = ss.iter().flat_map(|&s| s).map(|&s| s).collect::<Vec<u8>>();

        if chars.iter().fold(0, |a, &b| a ^ b) != nonprefix_pass_xor {
            continue;
        }
        let csum =
            ((prefix_pass_sum as u16 + chars.iter().map(|&d| d as u16).sum::<u16>()) & 0xff) as u8;
        if pass_sum.wrapping_sub(csum) >= (pass_length/* carry 許容数 */) {
            continue;
        }

        let mut splitgen = generate_split_in_two(chars);
        while let GeneratorState::Yielded((left, right)) = Pin::new(&mut splitgen).resume(()) {
            // 逆方向探索
            for p in rmap_used.iter_mut().flat_map(|line| line.iter_mut()) {
                *p = 0;
            }

            let mut passcode_r = right.clone();
            let r_len = passcode_r.len();
            init_permutations_wo_dup(&mut passcode_r);
            loop {
                let mut hv = ReversedShiftHashValue::from(target[0], target[1]);
                for d in &passcode_r {
                    shift_hasher.backward(&mut hv, *d);
                }
                let mut passcode_r_array = [0u8; LENGTH_MAX / 2];
                passcode_r
                    .iter()
                    .zip(&mut passcode_r_array)
                    .for_each(|(&d, p)| *p = d);
                if rmap_used[hv.0 as usize][hv.1 as usize] >= BUF_LEN {
                    // TODO ハッシュ衝突対応
                    for line in rmap_used {
                        for col in line {
                            print!("{:02} ", col);
                        }
                        println!("");
                    }
                    panic!();
                };
                rmap[hv.0 as usize][hv.1 as usize][rmap_used[hv.0 as usize][hv.1 as usize]]
                    [0..r_len]
                    .copy_from_slice(&passcode_r_array[0..r_len]);
                rmap_used[hv.0 as usize][hv.1 as usize] += 1;

                if !update_permutations_wo_dup(&mut passcode_r) {
                    break;
                }
            }

            // 順方向探索
            let mut passcode_l = left.clone();
            init_permutations_wo_dup(&mut passcode_l);
            loop {
                let mut hv = prefix_hv.clone();
                for d in &passcode_l {
                    shift_hasher.progress(&mut hv, *d);
                }

                for passcode_r in
                    &rmap[hv.0 as usize][hv.1 as usize][0..rmap_used[hv.0 as usize][hv.1 as usize]]
                {
                    // 本確認
                    let mut passcode = vec![];
                    for &c in prefix_codes {
                        passcode.push(c);
                    }
                    for &c in &passcode_l {
                        passcode.push(c);
                    }
                    for &c in passcode_r
                        .iter()
                        .take(nonprefix_pass_length as usize - passcode_l.len())
                        .rev()
                    {
                        passcode.push(c);
                    }
                    //println!("TRYING {:?}", passcode);
                    let checksum = calc_checksum(&shift_hasher, &passcode);
                    if checksum == *target {
                        println!(
                            "FOUND!! {:02x?} {}",
                            passcode,
                            codemap.string_of(passcode.clone()).unwrap()
                        );
                        result.push(passcode);
                    }
                }

                if !update_permutations_wo_dup(&mut passcode_l) {
                    break;
                }
            }
        }
    }

    result
}

pub fn search(target: [u8; 8], prefix_codes: Vec<u8>, num_threads: usize) -> Vec<Vec<u8>> {
    let shift_hasher = ShiftHasher::new();
    let pass_length = target[2]; // パスコードの文字列長
    let pass_sum = target[3]; // パスコードの総和 + α(最大 pass_length)
    let pass_xor = target[5]; // パスコードの XOR 総和
    let pass_bitsum = target[7]; // パスコードの立っているビットの総和 + α(最大 pass_length)
    let pass_xor_bits_odd = (count_bits(pass_xor) % 2) == 1;

    let prefix_pass_length = prefix_codes.len() as u8;
    let prefix_pass_sum = (prefix_codes.iter().map(|&d| d as u16).sum::<u16>() & 0xff) as u8;
    let prefix_pass_xor = prefix_codes.iter().fold(0, |a, &b| a ^ b);
    let prefix_pass_bitsum = prefix_codes.iter().fold(0, |a, &b| a + count_bits(b));
    let prefix_pass_xor_bits_odd = (prefix_pass_bitsum % 2) == 1;

    let nonprefix_pass_length = pass_length - prefix_pass_length;
    let nonprefix_pass_xor = pass_xor ^ prefix_pass_xor;
    let nonprefix_pass_bitsum = pass_bitsum - prefix_pass_bitsum;
    let nonprefix_pass_xor_bits_odd = pass_xor_bits_odd ^ prefix_pass_xor_bits_odd;

    let mut prefix_hv = ReversedShiftHashValue::new();
    for c in &prefix_codes {
        shift_hasher.progress(&mut prefix_hv, *c);
    }
    let prefix_hv = prefix_hv;

    let mut bit_dist = vec![];
    let mut dist_gen = generate_bit_distribution(nonprefix_pass_bitsum, nonprefix_pass_length);
    while let GeneratorState::Yielded(s) = Pin::new(&mut dist_gen).resume(()) {
        bit_dist.push(s);
    }

    let result = Arc::new(Mutex::new(vec![]));
    let bit_dist = Arc::new(Mutex::new(bit_dist));
    let mut handles = vec![];
    for _ in 0..num_threads {
        let result = Arc::clone(&result);
        let bit_dist = Arc::clone(&bit_dist);

        let shift_hasher = ShiftHasher::new();
        let prefix_codes = prefix_codes.clone();

        // 逆探索用バッファ
        let mut rmap: Box<[[[[u8; LENGTH_MAX / 2]; BUF_LEN]; 256]; 256]> =
            Box::new(array_init::array_init(|_| {
                array_init::array_init(|_| {
                    array_init::array_init(|_| array_init::array_init(|_| 0))
                })
            }));
        let mut rmap_used: [[usize; 256]; 256] =
            array_init::array_init(|_| array_init::array_init(|_| 0usize));

        let handle = thread::spawn(move || {
            let codemap = CodeMap::new();
            loop {
                let s = bit_dist.lock().unwrap().pop();
                if let None = s {
                    return;
                }
                let s = s.unwrap();

                let r = search_one_dist(
                    &codemap,
                    &shift_hasher,
                    &mut rmap,
                    &mut rmap_used,
                    &s,
                    &target,
                    &prefix_codes,
                    pass_length,
                    pass_sum,
                    prefix_pass_sum,
                    prefix_hv,
                    nonprefix_pass_length,
                    nonprefix_pass_xor,
                    nonprefix_pass_bitsum,
                    nonprefix_pass_xor_bits_odd,
                );
                result.lock().unwrap().extend(r);
            }
        });
        handles.push(handle)
    }

    for handle in handles {
        handle.join().unwrap();
    }
    return result.lock().unwrap().clone();
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
}
