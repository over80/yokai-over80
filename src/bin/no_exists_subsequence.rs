use anyhow::{Context, Result};
use clap::Parser;
use itertools::Itertools;
use std::cmp::min;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

use yokai::filter::Dictionary;

/// tokenize and filter out
#[derive(Debug, Parser)]
#[clap(name = "no_exists_subsequence")]
struct Args {
    /// dictionary file
    #[clap(short = 'd', long = "dictionary", required = true)]
    dictionaries: Vec<PathBuf>,

    /// max length of subsequence
    #[clap(long, short, default_value = "3")]
    length: usize,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let mut dic = Dictionary::new();
    for path in &args.dictionaries {
        let p = path.to_owned();
        let file =
            File::open(path).with_context(|| format!("cannot open file `{}'", p.display()))?;
        for line in BufReader::new(file).lines() {
            let line = line?;
            let line = line.trim();
            let mut words = line.split_whitespace().collect::<Vec<_>>();
            let label = match words.pop() {
                Some(word) => word,
                None => continue, // empty lineqq
            };
            if words.len() == 0 {
                words.push(label.clone());
            }

            for word in &words {
                let upper_word = word.to_uppercase();

                if upper_word.chars().all(|c| match c {
                    'A'..='Z' => true,
                    _ => false,
                }) {
                    for i in 0..(upper_word.len() - 1) {
                        dic.insert(&upper_word[i..min(i + args.length, upper_word.len())], "")?
                    }
                }
            }
        }
    }

    for l in 1..=args.length {
        for letters in (0..l).map(|_| 'A'..='Z').multi_cartesian_product() {
            let letters = String::from_iter(letters);
            if dic.has_node(&letters) == false
                && dic.has_node(&letters[..(l - 1)]) == true
                && dic.has_node(&letters[1..]) == true
            {
                println!("{}", letters);
            }
        }
    }

    Ok(())
}
