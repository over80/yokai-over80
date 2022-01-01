#![feature(generators, generator_trait)]

use clap::{App, Arg};
use itertools::Itertools;

use yokai::chars::CodeMap;
use yokai::hasher::{calc_checksum, ShiftHasher};
use yokai::searcher::search;

fn main() {
    let matches = App::new("Yokai password searcher")
        .version("0.1")
        .subcommand(
            App::new("checksum").about("calc checksum of password").arg(
                Arg::new("PASSWORD")
                    .help("the string to calc checksum")
                    .required(true)
                    .index(1),
            ),
        )
        .subcommand(
            App::new("search")
                .about("search password from checksum")
                .arg(Arg::new("C1").required(true).index(1))
                .arg(Arg::new("C2").required(true).index(2))
                .arg(Arg::new("C3").required(true).index(3))
                .arg(Arg::new("C4").required(true).index(4))
                .arg(Arg::new("C5").required(true).index(5))
                .arg(Arg::new("C6").required(true).index(6))
                .arg(Arg::new("C7").required(true).index(7))
                .arg(Arg::new("C8").required(true).index(8))
                .arg(
                    Arg::new("prefix")
                        .long("prefix")
                        .value_name("STR")
                        .takes_value(true)
                        .number_of_values(1)
                        .default_value("")
                        .help("search password which only start with this"),
                )
                .arg(
                    Arg::new("thread")
                        .long("thread")
                        .value_name("NUM")
                        .takes_value(true)
                        .number_of_values(1)
                        .help("number of CPU threads"),
                ),
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("checksum") {
        let password = matches.value_of("PASSWORD").unwrap();

        let codemap = CodeMap::new();

        let code = codemap.codes_of(password);
        if let Err(msg) = code {
            println!("{}", msg);
            return;
        }
        let code = code.unwrap();

        let shift_hasher = ShiftHasher::new();
        let checksum = calc_checksum(&shift_hasher, &code);

        println!(
            "checksum: {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X}",
            checksum[0],
            checksum[1],
            checksum[2],
            checksum[3],
            checksum[4],
            checksum[5],
            checksum[6],
            checksum[7]
        );
    } else if let Some(matches) = matches.subcommand_matches("search") {
        let mut checksum = [0u8; 8];
        for (label, p) in ["C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"]
            .iter()
            .zip(&mut checksum)
        {
            let s = matches.value_of(label).unwrap();
            match u8::from_str_radix(s, 16) {
                Ok(r) => *p = r,
                Err(e) => {
                    println!("ERROR: '{}' : {}", s, e);
                    return;
                }
            }
        }

        let codemap = CodeMap::new();

        let search_prefix = matches.value_of("prefix").unwrap();
        let search_prefix_codes = codemap.codes_of(search_prefix);
        if let Err(e) = search_prefix_codes {
            println!("ERROR: '{}' : {}", search_prefix, e);
            return;
        }
        let search_prefix_codes = search_prefix_codes.unwrap();
        let num_threads = matches
            .value_of("thread")
            .map(|s| s.parse::<usize>())
            .unwrap_or(Ok(num_cpus::get()));
        if let Err(e) = num_threads {
            println!("ERROR: invalid thread num : {}", e);
            return;
        }
        let num_threads = num_threads.unwrap();

        println!(
            "target checksum: {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X}",
            checksum[0],
            checksum[1],
            checksum[2],
            checksum[3],
            checksum[4],
            checksum[5],
            checksum[6],
            checksum[7]
        );

        println!("fixed prefix: {}", search_prefix);
        println!("num of threads: {}", num_threads);

        let result = search(checksum, search_prefix_codes, num_threads);
        println!("---------------------------------------------------");
        if result.len() == 0 {
            println!("sorry. not found");
        } else {
            println!("FOUND!!  passcode is...");
            let result = result
                .into_iter()
                .map(|code| codemap.string_of(code).unwrap())
                .collect_vec();
            for password in result {
                println!("    {}", password);
            }
        }
    } else {
        println!("ERROR: Please specify subcommand.");
    }
    /*
        let codemap = CodeMap::new();
        let shift_hasher = ShiftHasher::new();
        //let target: [u8; 8] = calc_checksum(&shift_hasher, &codemap.codes_of("TEST").unwrap());
        //let target: [u8; 8] = calc_checksum(&shift_hasher, &codemap.codes_of("MONITOR").unwrap());
        //let target: [u8; 8] = calc_checksum(&shift_hasher, &codemap.codes_of("SPEED-UP").unwrap());
        //let target: [u8; 8] = calc_checksum(&shift_hasher, &codemap.codes_of("UDADAGAWA").unwrap());
        //let target: [u8; 8] = calc_checksum(&shift_hasher, &codemap.codes_of("KOBAYASHI").unwrap());
        //let target: [u8; 8] = calc_checksum(&shift_hasher, &codemap.codes_of("OHAYOUKAWADA").unwrap());
        let target: [u8; 8] = [0x64, 0x98, 0x0B, 0x15, 0x91, 0x18, 0xB1, 0x15]; /* 11文字 */
        //let target: [u8; 8] = [0x65, 0x94, 0x0E, 0xAC, 0xE9, 0x07, 0x33, 0x25]; /* 14文字 */
        println!(
            "target: {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}",
            target[0], target[1], target[2], target[3], target[4], target[5], target[6], target[7]
        );
        let result = search(&codemap, target, true);
        if result.len() == 0 {
            println!("sorry. not found");
        } else {
            println!("FOUND !!!  passcode is...");
            for code in result {
                println!("    {}", codemap.string_of(code).unwrap());
            }
        }
    */
}
