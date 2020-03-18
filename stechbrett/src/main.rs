extern crate rand;

use rand::Rng;

fn main() {
    let lottery_ticket = rand::thread_rng().gen_range(1, 1201);
    // let lottery_ticket = 666;
    println!("ticket: {}", lottery_ticket);

    match lottery_ticket {
        1100 => {
            println!("magic number! You win 100 €");
            return;
        }
        270 => {
            println!("magic number! You win 70 €");
            return;
        }
        750 => {
            println!("magic number! You win 50 €");
            return;
        }
        340 | 540 | 840 => {
            println!("magic number! You win 40 €");
            return;
        }
        230 | 630 | 1030 => {
            println!("magic number! You win 30 €");
            return;
        }
        120 | 420 | 720 => {
            println!("magic number! You win 20 €");
            return;
        }

        _ => (),
    }

    if lottery_ticket % 100 == 0 && lottery_ticket < 1000 {
        println!("modulo 100! You win 10 €!");
        return;
    }

    let s = lottery_ticket.to_string();

    if lottery_ticket >= 100
        && lottery_ticket <= 1000
        && s.chars().nth(0) == s.chars().nth(1)
        && s.chars().nth(1) == s.chars().nth(2)
    {
        println!("repdigit! You win 5 €!");
        return;
    }

    if s.chars().nth(s.len() - 1).unwrap() == '7' {
        println!("last digit 7! You get a free ticket!");
        return;
    }
}
