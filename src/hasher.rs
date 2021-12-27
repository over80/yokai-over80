use crate::chars::CodeMap;
use crate::count_bits;

fn ror(num: &mut u8, carry: &mut bool) {
    let next_carry = (*num & 0x01) != 0;
    *num >>= 1;
    if *carry {
        *num |= 0x80;
    }
    *carry = next_carry;
}

#[allow(dead_code)]
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

#[allow(dead_code)]
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
pub struct ReversedShiftHashValue(pub u8, pub u8);

impl ReversedShiftHashValue {
    pub fn new() -> ReversedShiftHashValue {
        ReversedShiftHashValue(0, 0)
    }

    fn reverse(d: u8) -> u8 {
        let mut d = d;
        d = (d & 0xf0) >> 4 | (d & 0x0f) << 4;
        d = (d & 0xcc) >> 2 | (d & 0x33) << 2;
        d = (d & 0xaa) >> 1 | (d & 0x55) << 1;
        d
    }

    pub fn from(d: u8, e: u8) -> ReversedShiftHashValue {
        ReversedShiftHashValue(Self::reverse(d), Self::reverse(e))
    }

    pub fn as_normal(&self) -> (u8, u8) {
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

pub struct ShiftHasher {
    map: [(u8, u8); 256],
    rmap: [(u8, u8); 256], // map.0 -> (index, map.1)
}

impl ShiftHasher {
    pub fn new() -> ShiftHasher {
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

    pub fn progress(&self, value: &mut ReversedShiftHashValue, code: u8) {
        let ub = value.1;
        let lb = value.0;
        let mv = self.map[ub as usize];
        value.0 = mv.0 ^ code;
        value.1 = mv.1 ^ lb;
    }

    pub fn backward(&self, value: &mut ReversedShiftHashValue, code: u8) {
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

pub fn calc_checksum(shift_hasher: &ShiftHasher, codes: &Vec<u8>) -> [u8; 8] {
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
