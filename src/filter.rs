use anyhow::{anyhow, Result};
use std::collections::HashMap;

const NODE_CHAR_MAX: usize = 29;
const CHAR_MAX: usize = 39;

#[derive(Debug)]
struct CharMap {
    map: HashMap<char, usize>,
    rmap: HashMap<usize, char>,
    delim: [bool; CHAR_MAX],
    num: [bool; CHAR_MAX],
}

impl CharMap {
    fn new() -> CharMap {
        let mut map = HashMap::new();
        let mut rmap = HashMap::new();
        let mut delim = [false; CHAR_MAX];
        let mut num = [false; CHAR_MAX];

        let mut index = 0;
        for c in 'A'..='Z' {
            map.insert(c, index);
            rmap.insert(index, c);
            index += 1;
        }
        for c in ['.', '-', '!'] {
            map.insert(c, index);
            rmap.insert(index, c);
            delim[index] = true;
            index += 1;
        }
        assert!(NODE_CHAR_MAX >= index);
        for c in '0'..='9' {
            map.insert(c, index);
            rmap.insert(index, c);
            num[index] = true;
            index += 1;
        }
        assert!(CHAR_MAX >= index);
        CharMap {
            map,
            rmap,
            delim,
            num,
        }
    }

    fn char_to_code(&self, c: char) -> Option<usize> {
        self.map.get(&c).map(|e| *e)
    }

    fn code_to_char(&self, d: usize) -> Option<char> {
        self.rmap.get(&d).map(|e| *e)
    }

    fn str_to_codes(&self, s: &str) -> Result<Vec<usize>> {
        let mut codes = Vec::with_capacity(s.len());
        for c in s.chars() {
            codes.push(self.char_to_code(c).ok_or(anyhow!("unknown char"))?)
        }
        Ok(codes)
    }

    fn codes_to_str(&self, v: &[usize]) -> Result<String> {
        v.iter()
            .map(|&d| self.code_to_char(d).ok_or(anyhow!("unknown code")))
            .collect()
    }

    fn is_delim(&self, d: usize) -> bool {
        self.delim[d]
    }

    fn is_num(&self, d: usize) -> bool {
        self.num[d]
    }
}

#[derive(Debug)]
struct Node {
    next: [Option<Box<Node>>; NODE_CHAR_MAX],
    word: Option<String>,
}

impl Node {
    fn new() -> Node {
        let next = Default::default();
        let word = None;
        Node { next, word }
    }

    fn insert(&mut self, code: &[usize], word: &str) {
        if code.len() == 0 {
            self.word = Some(word.to_owned());
        } else {
            let c = code[0];
            let rest = &code[1..];
            let next_node = &mut self.next[c];
            if let None = *next_node {
                *next_node = Some(Box::new(Node::new()))
            }
            let next_node = self.next[c].as_mut().unwrap();
            next_node.insert(rest, word);
        }
    }

    fn find<'a>(&'a self, code: &[usize], num_used: usize, result: &mut Vec<(usize, &'a str)>) {
        if let Some(word) = &self.word {
            result.push((num_used, &word));
        }
        if code.len() > 0 {
            let c = code[0];
            if c >= self.next.len() {
                // it's a number letter
                return;
            }
            let rest = &code[1..];
            if let Some(next_node) = &self.next[c] {
                next_node.find(rest, num_used + 1, result);
            }
        }
    }

    fn has_node(&self, code: &[usize]) -> bool {
        if code.len() == 0 {
            return true;
        } else {
            let c = code[0];
            if c >= self.next.len() {
                return true;
            }
            if let Some(next_node) = &self.next[c] {
                let rest = &code[1..];
                return next_node.has_node(rest);
            } else {
                return false;
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Word(String),
    Delimiter(String),
    Number(String),
}

impl std::string::ToString for Token {
    fn to_string(&self) -> String {
        match self {
            Self::Word(w) => w.to_owned(),
            Self::Delimiter(w) => w.to_owned(),
            Self::Number(w) => w.to_owned(),
        }
    }
}

#[derive(Debug)]
pub struct Dictionary {
    charmap: CharMap,
    root: Box<Node>,
}

impl Dictionary {
    pub fn new() -> Dictionary {
        let charmap = CharMap::new();
        let root = Box::new(Node::new());
        Dictionary { charmap, root }
    }

    pub fn insert(&mut self, word: &str, label: &str) -> Result<()> {
        let codes = self.charmap.str_to_codes(word)?;

        self.root.insert(&codes, label);

        Ok(())
    }

    fn find_from(&self, codes: &[usize]) -> Vec<(usize, &str)> {
        let mut result = Vec::with_capacity(codes.len());
        self.root.find(codes, 0, &mut result);

        result
    }

    pub fn tokenlize(&self, word: &str) -> Option<Vec<Token>> {
        let codes = word
            .chars()
            .map(|c| self.charmap.char_to_code(c))
            .collect::<Option<Vec<_>>>()?;
        let mut result = Vec::with_capacity(codes.len());
        let mut used_num = 0;
        let mut start_pos = vec![0];
        let mut candidate = vec![];

        let mut next_word_candidate = self
            .find_from(&codes)
            .iter()
            .map(|(len, word)| (*len, Token::Word(word.to_string())))
            .collect::<Vec<(usize, Token)>>();
        if self.charmap.is_delim(codes[used_num]) {
            next_word_candidate.push((
                1,
                Token::Delimiter(
                    self.charmap
                        .code_to_char(codes[used_num])
                        .unwrap()
                        .to_string(),
                ),
            ))
        }
        if self.charmap.is_num(codes[used_num]) {
            let mut i = used_num + 1;
            for &c in &codes[i..] {
                if self.charmap.is_num(c) {
                    i += 1;
                } else {
                    break;
                }
            }
            next_word_candidate.push((
                i - used_num,
                Token::Number(self.charmap.codes_to_str(&codes[used_num..i]).unwrap()),
            ))
        }

        if next_word_candidate.len() == 0 {
            return None;
        }

        candidate.push(next_word_candidate);

        while let Some(last) = candidate.last_mut() {
            if let Some((word_len, token)) = last.pop() {
                start_pos.push(used_num);
                used_num += word_len;
                result.push(token);

                if used_num == codes.len() {
                    return Some(result);
                }

                let mut next_word_candidate = self
                    .find_from(&codes[used_num..])
                    .iter()
                    .map(|(len, word)| (*len, Token::Word(word.to_string())))
                    .collect::<Vec<(usize, Token)>>();
                if self.charmap.is_delim(codes[used_num]) {
                    next_word_candidate.push((
                        1,
                        Token::Delimiter(
                            self.charmap
                                .code_to_char(codes[used_num])
                                .unwrap()
                                .to_string(),
                        ),
                    ))
                }
                if self.charmap.is_num(codes[used_num]) {
                    let mut i = used_num + 1;
                    for &c in &codes[i..] {
                        if self.charmap.is_num(c) {
                            i += 1;
                        } else {
                            break;
                        }
                    }
                    next_word_candidate.push((
                        i - used_num,
                        Token::Number(self.charmap.codes_to_str(&codes[used_num..i]).unwrap()),
                    ))
                }
                candidate.push(next_word_candidate);
            } else {
                result.pop();
                candidate.pop();
                used_num = start_pos.pop().unwrap();
            }
        }

        None
    }

    pub fn has_node(&self, word: &str) -> bool {
        let codes = word
            .chars()
            .map(|c| self.charmap.char_to_code(c))
            .collect::<Option<Vec<_>>>();
        if let None = codes {
            return false;
        }
        let codes = codes.unwrap();

        return self.root.has_node(&codes);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_charmap() -> Result<()> {
        let charmap = CharMap::new();
        println!("{:?}", charmap.map);
        assert_eq!(charmap.char_to_code('*'), None);
        assert_eq!(charmap.char_to_code('A'), Some(0));
        assert_eq!(charmap.char_to_code('Z'), Some(25));
        assert_eq!(charmap.char_to_code('0'), Some(29));
        assert_eq!(charmap.char_to_code('1'), Some(30));
        assert_eq!(charmap.char_to_code('9'), Some(38));

        assert_eq!(charmap.code_to_char(0), Some('A'));
        assert_eq!(charmap.code_to_char(25), Some('Z'));
        assert_eq!(charmap.code_to_char(29), Some('0'));
        assert_eq!(charmap.code_to_char(30), Some('1'));
        assert_eq!(charmap.code_to_char(38), Some('9'));
        assert_eq!(charmap.code_to_char(39), None);

        assert_eq!(charmap.str_to_codes("A0")?, vec![0, 29]);
        assert_eq!(charmap.codes_to_str(&[0, 29])?, "A0");

        assert_eq!(charmap.is_delim(5), false);
        assert_eq!(charmap.is_delim(27), true);
        assert_eq!(charmap.is_delim(34), false);
        assert_eq!(charmap.is_num(5), false);
        assert_eq!(charmap.is_num(27), false);
        assert_eq!(charmap.is_num(34), true);

        Ok(())
    }

    #[test]
    fn test_insert_and_find() -> Result<()> {
        let mut dic = Dictionary::new();
        dic.insert("ABC", "abc")?;

        let charmap = CharMap::new();
        let codes = charmap.str_to_codes("ABC")?;
        assert_eq!(
            dic.root.next[codes[0]].as_ref().unwrap().next[codes[1]]
                .as_ref()
                .unwrap()
                .next[codes[2]]
                .as_ref()
                .unwrap()
                .word,
            Some("abc".to_owned())
        );

        dic.insert("ABD", "abd")?;
        dic.insert("ABDE", "abde")?;

        assert_eq!(
            dic.find_from(&charmap.str_to_codes("ABCXX")?),
            vec![(3, "abc")]
        );
        assert_eq!(
            dic.find_from(&charmap.str_to_codes("ABDEX")?),
            vec![(3, "abd"), (4, "abde")]
        );

        Ok(())
    }

    #[test]
    fn test_tokenlize() -> Result<()> {
        let mut dic = Dictionary::new();
        dic.insert("AB", "ab")?;
        dic.insert("ABC", "abc")?;
        dic.insert("CD", "cd")?;
        dic.insert("DE", "de")?;

        assert_eq!(dic.tokenlize("A"), None);
        assert_eq!(dic.tokenlize("AC"), None);
        assert_eq!(
            dic.tokenlize("ABCD"),
            Some(vec![
                Token::Word("ab".to_owned()),
                Token::Word("cd".to_owned())
            ])
        );
        assert_eq!(
            dic.tokenlize("ABCDE"),
            Some(vec![
                Token::Word("abc".to_owned()),
                Token::Word("de".to_owned())
            ])
        );
        assert_eq!(
            dic.tokenlize("AB-CD"),
            Some(vec![
                Token::Word("ab".to_owned()),
                Token::Delimiter("-".to_owned()),
                Token::Word("cd".to_owned())
            ])
        );
        assert_eq!(
            dic.tokenlize("AB123CD"),
            Some(vec![
                Token::Word("ab".to_owned()),
                Token::Number("123".to_owned()),
                Token::Word("cd".to_owned())
            ])
        );
        Ok(())
    }

    #[test]
    fn test_hasnode() -> Result<()> {
        let mut dic = Dictionary::new();

        dic.insert("ABC", "")?;
        assert_eq!(dic.has_node("A"), true);
        assert_eq!(dic.has_node("AB"), true);
        assert_eq!(dic.has_node("ABC"), true);
        assert_eq!(dic.has_node("AC"), false);
        assert_eq!(dic.has_node("B"), false);
        assert_eq!(dic.has_node("DE"), false);

        Ok(())
    }
}
