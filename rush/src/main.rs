use std::io::{self, BufRead, Write};
use std::process::Command;
use std::str;

fn main() {
    let input = io::stdin();

    loop {
        print!("> ");
        io::stdout().flush().ok().expect("Could not flush stdout");

        let mut line = String::new();
        input
            .lock()
            .read_line(&mut line)
            .expect("Could not read stdin");

        let mut args = line.split_whitespace().collect::<Vec<&str>>();
        if args.len() < 1 {
            continue;
        }

        match args[0] {
            "cd" => {
                println!("TODO");
                continue;
            }
            "exit" => return,
            _ => {}
        };

        let mut cmd = Command::new(args[0]);
        args.remove(0);
        cmd.args(args);

        let output = match cmd.output() {
            Ok(v) => v,
            Err(err) => {
                eprintln!("{}", err);
                continue;
            }
        };

        let s = str::from_utf8(&output.stdout).expect("Could not convert output to string");
        println!("output: {}", s.trim_right());
    }
}
