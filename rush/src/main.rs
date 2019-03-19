use std::io::{self, BufRead, Write};
use std::process::Command;
use std::str;

fn main() {
    let mut line = String::new();
    let input = io::stdin();

    print!("> ");
    io::stdout().flush().ok().expect("Could not flush stdout");

    input
        .lock()
        .read_line(&mut line)
        .expect("Could not read stdin");

    // TODO: don't use sh
    let output = Command::new("sh")
        .arg("-c")
        .arg(line)
        .output()
        .expect("Could not execute process");

    let s = str::from_utf8(&output.stdout).expect("Could not convert output to string");

    println!("output: {}", s.trim_right());
}
