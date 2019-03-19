use std::io::{self, BufRead, Write};
use std::process::Command;
use std::str;

fn main() {
    loop {
        print!("> ");
        io::stdout().flush().ok().expect("Could not flush stdout");

        let mut line = String::new();
        let input = io::stdin();
        input
            .lock()
            .read_line(&mut line)
            .expect("Could not read stdin");

        let args = line.split_whitespace().collect::<Vec<&str>>();

        if args.len() < 1 {
            continue;
        }

        let output = Command::new(args[0])
        .arg(args[1]) // TODO: use all given args
        .output()
        .expect("Could not execute process");

        let s = str::from_utf8(&output.stdout).expect("Could not convert output to string");

        println!("output: {}", s.trim_right());
    }
}
