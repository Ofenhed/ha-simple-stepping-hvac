use std::{fs::File, io::Read};

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
    println!("Hello, world!");
    let mut data = vec![];
    let config: ClimateConfig = {
        let mut args = std::env::args();
        let program = args.next().expect("Missing arguments");
        let Some(config_file) = args.next() else {
            eprintln!("Usage: {program} <config_file>.yaml");
            std::process::exit(1);
        };
        let mut yaml_file = File::open(config_file)?;
        yaml_file.read_to_end(&mut data)?;
        serde_yaml::from_slice(&data[..])?
    };
    let package = Package::try_from(&config)?;

    //let example = Package {
    //    automation: vec![Automation {
    //        alias: "My first automation".into(),
    //        trigger: vec![Trigger::State {
    //            entity_id: vec!["climate.basemenet_radiator".to_string()].into(),
    //        }]
    //        .into(),
    //        action: vec![Action::SetNumberValue {
    //            metadata: Default::default(),
    //            data: 95.into(),
    //            target: "climate.basement_radiator_valve_closing_degree"
    //                .to_string()
    //                .into(),
    //        }],
    //    }]
    //    .into(),
    //    helpers: Helpers {
    //        sensor: vec![Sensor::Derivative {
    //            name: "Derivate for somewhere".to_string(),
    //            source: "sensor.basement_temperature".to_string(),
    //            time_window: Duration {
    //                hours: 0,
    //                minutes: 30,
    //                seconds: 0,
    //            },
    //            unit_time: Some(UnitTime::Minutes),
    //            round: None,
    //        }],
    //        template: Some(TemplateSensor {
    //            sensor: vec![TemplateSensorInner {
    //                name: "Temperature in basement".to_string(),
    //                device_class: Some(SensorDeviceClass::Temperature),
    //                state: "{{ states.climate.basement_radiator.attributes.current_temperature }}"
    //                    .to_string(),
    //                state_class: Some(StateClass::Measurement),
    //                unit_of_measurement: Some(UnitOfMeasurement::Celcius),
    //                unique_id: Some("extracted_basement_temperature".to_string()),
    //            }],
    //        }),
    //    },
    //    ..Default::default()
    //};
    println!("{}", serde_yaml::to_string(&package).unwrap());
    Ok(())
}
