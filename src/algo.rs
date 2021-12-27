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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_count_bits() {
        assert_eq!(count_bits(0x00), 0);
        assert_eq!(count_bits(0x5a), 4);
        assert_eq!(count_bits(0xff), 8);
    }
}
