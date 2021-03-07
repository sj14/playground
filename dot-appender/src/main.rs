use serde_derive::Deserialize;
use std::fs::OpenOptions;
use std::io::Read;
use std::io::Seek;
use std::io::{Write};

fn main() {
    read_appender_file();
    read_target_file();
}

#[derive(Deserialize)]
struct Configs {
    configs: Vec<Config>,
}

#[derive(Deserialize)]
struct Config {
    target: String,
    comment: String,
    append: String,
}

fn read_appender_file() {}

fn read_target_file() {
    let mut file = OpenOptions::new()
        .write(true)
        .read(true)
        .open("test.toml")
        .expect("failed open");

    let mut content = String::new();
    file.read_to_string(&mut content).expect("failed reading");

    let appender_configs: Configs =
        toml::from_str(content.as_ref()).expect("faild reading toml file");

    for cfg in appender_configs.configs {
        let header = format!("\n{} {}", cfg.comment, "DOT-APPENDER START\n");
        let footer = format!("\n{} {}", cfg.comment, "DOT-APPENDER END\n");
        let mut file = OpenOptions::new()
            .write(true)
            .read(true)
            .create(true)
            .open(cfg.target)
            .expect("failed open");

        let mut content = String::new();
        file.read_to_string(&mut content).expect("failed reading");

        // remove old appender config if exists
        let start = content.find(&header).unwrap_or(content.len());
        let mut end = content.find(&footer).unwrap_or(0);
        if end != 0 {
            end += footer.len();
            content.replace_range(start..end, "");
        }

        // overwrite file with new config
        // TODO: add backup of file
        file.seek(std::io::SeekFrom::Start(0))
            .expect("failed seeking");
        file.write_all(content.as_bytes())
            .expect("failed writing content");
        file.write_all(header.as_bytes()).expect("failed writing");
        file.write_all(cfg.append.as_bytes())
            .expect("failed writing");
        file.write_all(footer.as_bytes()).expect("failed writing");
    }
}
