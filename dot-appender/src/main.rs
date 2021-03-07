use std::io::Seek;
use std::io::Read;
use std::env;
use std::fs;
use std::fs::OpenOptions;
use std::io::{Write, BufReader, BufRead, Error};
use serde_derive::Deserialize;


fn main() {
    read_appender_file();
    read_target_file();
}

fn read_appender_file() {

}

fn read_target_file() {
    let filename = "testfile";

    let header = "\n\nDOT-APPENDER START\n";
    let footer = "\nDOT-APPENDER END\n";


    let mut file = OpenOptions::new().write(true).read(true).open(filename).expect("failed open");

    let mut content = String::new();
    file.read_to_string(&mut content).expect("failed reading");

    // remove old appender config if exists
    let start = content.find(header).unwrap_or(content.len());
    let mut end = content.find(footer).unwrap_or(0);
    if end != 0 {
        end += footer.len();
        content.replace_range(start..end, "");
    } 



    println!("content1:\n{}", content);

    // overwrite file with new config
    // TODO: add backup of file
    file.seek(std::io::SeekFrom::Start(0)).expect("failed seeking");
    file.write_all(content.as_bytes()).expect("failed writing content");
    file.write_all(header.as_bytes()).expect("failed writing");
    file.write_all(b"my_config=123").expect("failed writing");
    file.write_all(footer.as_bytes()).expect("failed writing");
}
