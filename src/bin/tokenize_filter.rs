use anyhow::{bail, Context, Result};
use clap::Parser;
use itertools::Itertools;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::PathBuf;

use yokai::filter::Dictionary;
use yokai::filter::Token::*;

/// tokenize and filter out
#[derive(Debug, Parser)]
#[clap(name = "tokenize_filter")]
struct Args {
    /// dictionary file
    #[clap(short = 'd', long = "dictionary", required = true)]
    dictionaries: Vec<PathBuf>,

    /// filter out if the word has multiple numeric part
    #[clap(long)]
    filter_multi_num: bool,

    /// input file (default: standard input)
    #[clap(name = "INPUT")]
    input: Option<PathBuf>,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let mut input: Box<dyn io::Read> = Box::new(io::stdin());
    if let Some(path) = args.input {
        let p = path.to_owned();
        input = Box::new(
            File::open(path).with_context(|| format!("cannot open file `{}'", p.display()))?,
        )
    }

    let mut dic = Dictionary::new();
    for path in &args.dictionaries {
        let p = path.to_owned();
        let file =
            File::open(path).with_context(|| format!("cannot open file `{}'", p.display()))?;
        for line in BufReader::new(file).lines() {
            let line = line?;
            let org_word = line.trim();
            let upper_word = line.to_uppercase();

            if upper_word.len() == 1 {
                continue;
            }

            if upper_word.chars().all(|c| match c {
                'A'..='Z' => true,
                _ => false,
            }) {
                dic.insert(&upper_word, org_word)?
            }
        }
    }

    for line in BufReader::new(input).lines() {
        let line = line?;
        let word = line.trim();
        if let Some(result) = dic.tokenlize(word) {
            // 数字が別れて登場するものは無効
            if args.filter_multi_num {
                #[derive(PartialEq)]
                enum NumberState {
                    Initial,
                    Found,
                    Finished,
                    MoreThanTwice,
                };
                if result
                    .iter()
                    .fold(NumberState::Initial, |s, t| match (&s, t) {
                        (NumberState::Initial, Number(_)) => NumberState::Found,
                        (NumberState::Found, Word(_)) => NumberState::Finished,
                        (NumberState::Finished, Number(_)) => NumberState::MoreThanTwice,
                        _ => s,
                    })
                    == NumberState::MoreThanTwice
                {
                    continue;
                }
            }

            // 特定のトークン間では空白を開けない
            let spacer: Vec<String> = result
                .iter()
                .zip(&result[1..])
                .map(|(a, b)| match (&a, &b) {
                    // "-" や "." の前後
                    (_, Delimiter(ref a)) if a == "-" || a == "." => "",
                    (Delimiter(ref a), _) if a == "-" || a == "." => "",

                    // "!" の前
                    (_, Delimiter(ref a)) if a == "!" => "",

                    // 記号同士の間
                    (Delimiter(_), Delimiter(_)) => "",

                    // デフォルトはスペースを空ける
                    _ => " ",
                })
                .map(|s| s.to_string())
                .collect();

            println!(
                "{}\t{}",
                word,
                result
                    .iter()
                    .map(|t| t.to_string())
                    .interleave(spacer)
                    .collect::<String>()
            );
        } else {
            //print!("{}\r", word);
        }
    }

    Ok(())
}
