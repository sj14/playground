use std::env;
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
                if args.len() != 2 {
                    eprintln!("cd needs exactly 1 argument");
                    continue;
                }
                match env::set_current_dir(args[1]) {
                    Ok(v) => v,
                    Err(e) => eprintln!("{}", e),
                };
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
        println!("{}", s.trim_right());
    }
}
