use serde_derive::Deserialize;
use std::fs::OpenOptions;
use std::io::Read;
use std::io::Seek;
use std::io::Write;

fn main() {
    let config_file = std::env::args().nth(2).expect("no config file given");

    read_target_file(config_file);
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

fn read_target_file(config_file: String) {
    let mut file = OpenOptions::new()
        .write(true)
        .read(true)
        .open(config_file)
        .expect("failed open");

    let mut content = String::new();
    file.read_to_string(&mut content).expect("failed reading");

    let appender_configs: Configs =
        toml::from_str(content.as_ref()).expect("faild reading toml file");

    for cfg in appender_configs.configs {
        println!("writing {}", cfg.target);

        let boundary_start = "CONFIBLE START";
        let boundary_stop = "CONFIBLE END";

        let header = format!("\n{} {}\n", cfg.comment, boundary_start);
        let footer = format!("\n{} {}\n", cfg.comment, boundary_stop);
        let mut file = OpenOptions::new()
            .write(true)
            .read(true)
            .create(true)
            .open(cfg.target)
            .expect("failed open");

        let mut content = String::new();
        file.read_to_string(&mut content).expect("failed reading");

        // remove old appender config if exists
        let mut new_content = String::new();
        let mut should_add = true;
        for l in content.lines() {
            if l.contains(boundary_start) {
                new_content.pop(); // remove empty new line
                should_add = false;
            }
            if l.contains(boundary_stop) {
                should_add = true;
                continue;
            }

            if should_add {
                new_content.push_str(&format!("{}\n", l));
            }
        }

        // overwrite file with new config
        // TODO: add backup of file
        file.seek(std::io::SeekFrom::Start(0))
            .expect("failed seeking");
        file.write_all(new_content.as_bytes())
            .expect("failed writing content");
        file.write_all(header.as_bytes()).expect("failed writing");
        file.write_all(cfg.append.as_bytes())
            .expect("failed writing");
        file.write_all(footer.as_bytes()).expect("failed writing");
    }
}
