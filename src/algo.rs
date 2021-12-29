use std::ops::Generator;

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

pub fn init_permutations_wo_dup(chars: &mut Vec<u8>) {
    chars.sort();
    chars.reverse();
}

pub fn update_permutations_wo_dup(chars: &mut Vec<u8>) -> bool {
    let t1 = chars
        .iter()
        .zip(&chars[1..])
        .enumerate()
        .find_map(|(i, (c, n))| if *c > *n { Some((i + 1, *n)) } else { None });
    if let Some((i, ci)) = t1 {
        let j = chars
            .iter()
            .enumerate()
            .find_map(|(j, cj)| if ci < *cj { Some(j) } else { None })
            .unwrap(); // c[i] < c[i-1] なので、少なくとも i-1 では見つかる
        chars.swap(i, j);
        chars[0..i].reverse();
        return true;
    } else {
        return false;
    }
}

pub fn generate_permutations_wo_dup(chars: Vec<u8>) -> impl Generator<Yield = Vec<u8>> {
    let mut curr = chars.clone();
    curr.sort();
    curr.reverse();
    move || loop {
        yield curr.clone(); // FIXME
        let t1 = curr
            .iter()
            .zip(&curr[1..])
            .enumerate()
            .find_map(|(i, (c, n))| if *c > *n { Some((i + 1, *n)) } else { None });
        if let Some((i, ci)) = t1 {
            let j = curr
                .iter()
                .enumerate()
                .find_map(|(j, cj)| if ci < *cj { Some(j) } else { None })
                .unwrap(); // c[i] < c[i-1] なので、少なくとも i-1 では見つかる
            curr.swap(i, j);
            curr[0..i].reverse();
        } else {
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::ops::GeneratorState;
    use std::pin::Pin;

    #[test]
    fn test_count_bits() {
        assert_eq!(count_bits(0x00), 0);
        assert_eq!(count_bits(0x5a), 4);
        assert_eq!(count_bits(0xff), 8);
    }

    #[test]
    fn test_permutations_wo_dup_1() {
        let input = vec![10, 10, 20];
        let mut result = vec![];
        let mut gen = generate_permutations_wo_dup(input);
        while let GeneratorState::Yielded(output) = Pin::new(&mut gen).resume(()) {
            result.push(output);
        }
        result.sort();
        assert_eq!(
            result,
            vec![vec![10, 10, 20], vec![10, 20, 10], vec![20, 10, 10]]
        );
    }

    #[test]
    fn test_permutations_wo_dup_2() {
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

    #[test]
    fn test_permutations_wo_dup_2_2() {
        let mut data = vec![10, 10, 20, 30];
        let mut result = vec![];

        init_permutations_wo_dup(&mut data);
        loop {
            result.push(data.clone());
            if !update_permutations_wo_dup(&mut data) {
                break;
            }
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
