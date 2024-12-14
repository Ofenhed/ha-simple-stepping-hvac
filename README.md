# Simple Stepping HVAC in Home Assistant
This project is a one time setup for my radiators. This works by setting an
offset temperature over the radiators set temperature. I've been running 2
degrees, which has been working well. The reason for this is to avoid that the
radiator goes into a heat cycle and fully heats up the radiator, while still
allowing for heat cycles in the event that home assistant dies, or
something bugs out. **It may become very warm if the automations stop
controlling the opening valves.**

There are some bugs, such as initial values not working, but it mostly works
besides that. I haven't figured out yet which values are best for the
different knobs, so expect some experimentation if you use this. I'm not
guaranteeing anything, use this on your own responsibility.

The generated files are huge. I have not noticed any performance inpact on my
device, but I would recommend that you keep an eye on it.

## Install
This code isn't executed in Home Assistant, but on a separate computer. It
will generate a [package
definition](https://www.home-assistant.io/docs/configuration/packages/), which
you can then upload to home assistant.

I would recommend running this package in a VM, to avoid manual review of the
source code.

> [!NOTE]
> I apologize for such a long letter - I didn't have time to write a short
> one.
>
> - Mark Twain

You may also use `vim` to paste it. If you do, I recommend that you use `:set
syntax=` first, or it will be really slow.

The script is simple, with an input and and output file as arguments:
```bash
cargo run -- config.yaml result.yaml
```

## Usage
When installed, this will create 4 groups:
- **Radiator Temperature Global Configuration**

  This is where you set the offset and temperature difference tolerances.
- **Radiators Temperature Global Configuration**

  This is where you set maximum and minimum valve positions for each radiator,
  as well as minimum inteval between updates of the valve position.
- **Radiator Temperature Status**

  This shows temperature information per room.
- **Radiator Temperature Internals**

  Simply ignore this one.

## Example configuration:
```yaml
entity_id_prefix: auto_radiators
derivative_spanning: # The time window for which the temperature derivate is calculated
  minutes: 30
trend_spanning: # The time window for which the derivate of the temperature derivate is calculated
  minutes: 15
backoff_after_heat: # The temperature will be affected by invalid data after the radiator has been in heat mode. This disables cooling after such a period.
  minutes: 15
full_close_friction: true # Makes it sligtly harder to reach 100% closed
rooms:
  Kitchen: # Name is only for grouping and logs
    temperature_sensor: sensor.kitchen_temperature
    radiators:
      - entity_id: climate.kitchen_radiator_1
    valve_closing_automation: true # Currently required, just go with it
  Living Room:
    radiators:
      - entity_id: climate.living_room_radiator_1
      - entity_id: climate.living_room_radiator_2
    valve_closing_automation: true
  Bathroom:
    radiators:
      - entity_id: climate.living_room_radiator
    valve_closing_automation: true
    full_close_friction: false # Exclude bathroom from full close friction
```
