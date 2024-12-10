use std::fs::{File, OpenOptions};

use config::ClimateConfig;
use package::Package;

mod automation;
mod config;
mod entity_id;
mod helpers;
mod package;
mod template;
mod translator;
mod types;

fn main() -> Result<(), anyhow::Error> {
    let mut args = std::env::args();
    let program = args.next().expect("Missing arguments");
    let (Some(config_file), Some(output_file)) = (args.next(), args.next()) else {
        eprintln!("Usage: {program} <config_file>.yaml <result_file>.yaml");
        std::process::exit(1);
    };
    let config: ClimateConfig = {
        let yaml_file = File::open(config_file)?;
        serde_yaml::from_reader(&yaml_file)?
    };
    let package = Package::try_from(&config)?;

    let output = OpenOptions::new()
        .create_new(true)
        .write(true)
        .open(output_file)?;
    serde_yaml::to_writer(&output, &package)?;
    Ok(())
}
