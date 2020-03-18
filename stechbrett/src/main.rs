extern crate rand;

use rand::Rng;

fn main() {
    let ticket = rand::thread_rng().gen_range(1, 1201);
    // let lottery_ticket = 666;
    println!("ticket: {}", ticket);
    println!("{}", lottery(ticket));
}

fn lottery<'a>(ticket: i32) -> &'a str {
    match ticket {
        1100 => {
            return "magic number! You win 100 €";
        }
        270 => {
            return "magic number! You win 70 €";
        }
        750 => {
            return "magic number! You win 50 €";
        }
        340 | 540 | 840 => {
            return "magic number! You win 40 €";
        }
        230 | 630 | 1030 => {
            return "magic number! You win 30 €";
        }
        120 | 420 | 720 => {
            return "magic number! You win 20 €";
        }
        _ => (),
    }

    if ticket % 100 == 0 && ticket < 1000 {
        return "modulo 100! You win 10 €!";
    }

    let s = ticket.to_string();

    if ticket >= 100
        && ticket <= 1000
        && s.chars().nth(0) == s.chars().nth(1)
        && s.chars().nth(1) == s.chars().nth(2)
    {
        return "repdigit! You win 5 €!";
    }

    if s.chars().nth(s.len() - 1).unwrap() == '7' {
        return "last digit 7! You get a free ticket!";
    }
    return "nothing :(";
}
