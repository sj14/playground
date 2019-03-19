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

        let mut cmd = Command::new(args[0]);
        // TODO: remove first element
        for a in args {
            cmd.arg(a);
        }

        // TODO: Does not wait until the command has finished
        let output = match cmd.output() {
            Ok(v) => v,
            Err(err) => continue, // TODO: how can I log the err and continue the loop?
        };

        let s = str::from_utf8(&output.stdout).expect("Could not convert output to string");
        println!("output: {}", s.trim_right());
    }
}
