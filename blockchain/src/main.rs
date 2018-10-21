use std::time::{SystemTime};
extern crate sha2;
use sha2::*; // TODO: bad practice

fn main() {
    let mut blockchain = Blockchain::default();

    blockchain.add(String::from("my data"));

    for i in blockchain.blocks.iter() {
        // TODO: how to print the time
        println!("data: {} | hash: {} | previous: {} | time: {}", i.data, i.hash, i.prev_hash, "idk");
    }

     match blockchain.last() {
        None => println!("none"),
        Some(last) => {
            println!("{}", last.hash)
        },
     }
}

struct Block {
    data: String,
    hash: String,
    prev_hash: String,
    time: SystemTime,
}

struct Blockchain {
    blocks: Vec<Block>,
}

impl Default for Blockchain {
    fn default() -> Blockchain {
        Blockchain {
            blocks: Vec::new(),
        }
    }
}


impl Blockchain {
    fn add(&mut self, data: String) {
        // TODO: calculate hash
        if self.blocks.len() == 0 {
            // TODO: no previous hash when genesis block
        }

        let mut hash = Sha256::default();
        hash.input(&data); 
        let block = Block{data: data, hash: hash, prev_hash: String::from("prev hash"), time: SystemTime::now()};
        // ^ FIXME: hash
        self.blocks.append(&mut vec![block])
    }

    fn last(&self) -> Option<&Block> {
        return self.blocks.last();
    }
}

