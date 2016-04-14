extern crate rustendo64_ui as ui;
extern crate glium as glium_crate;
extern crate cpal;

use glium_crate::{Display, DisplayBuild, Texture2d, Surface};
use glium_crate::glutin::{WindowBuilder, Event, ElementState, VirtualKeyCode};
use glium_crate::texture::{RawImage2d, ClientFormat};
use glium_crate::uniforms::MagnifySamplerFilter;

use std::borrow::Cow;
use std::ops::DerefMut;
use std::thread;
use std::time;

pub struct Interface {
    receiver: ui::UiReceiver,
    title: String,
    video: Option<Display>,
    video_mode: ui::VideoMode,
    video_reopen: bool,
    key_state: u32,
    audio_dev: Option<cpal::Endpoint>,
    audio_src: Option<cpal::Voice>,
    audio_mute: bool,
    audio_rate: u32,
    audio_underflowed: bool,
}

impl ui::Interface for Interface {
    fn new(opts: ui::Options, receiver: ui::UiReceiver, sender: ui::UiSender) -> Self {
        thread::spawn(move || Interface::ping_thread(sender));
        Interface {
            receiver: receiver,
            title: format!("Rustendo64_gb: {}", opts.win_title),
            video: None,
            video_mode: ui::VideoMode::default(),
            video_reopen: false,
            key_state: 0,
            audio_dev: cpal::get_default_endpoint(),
            audio_src: None,
            audio_mute: opts.mute_audio,
            audio_rate: 0,
            audio_underflowed: false,
        }
    }

    fn run(&mut self) {
        while let Ok(msg) = self.receiver.recv() {
            match msg {
                ui::UiMessage::Update => if self.update() { break; },
                ui::UiMessage::VideoMode(vm) => self.set_mode(vm),
                ui::UiMessage::Video(v) => self.display(v),
                ui::UiMessage::Audio(f, v) => self.play(f, v),
            }
        }
        println!("Interface closed.");
    }
}

impl Interface {
    fn ping_thread(sender: ui::UiSender) {
        // Just ping the UI every now and then to update the input state.
        loop {
            thread::sleep(time::Duration::from_millis(10));
            // This will exit when the channel is broken.
            sender.send(ui::UiMessage::Update);
        }
    }

    fn update(&mut self) -> bool {
        // Update pending audio samples.
        if let Some(ref voice) = self.audio_src {
            let samples = voice.get_pending_samples();
            self.receiver.set_pending_audio(samples);
            if voice.underflowed() && !self.audio_underflowed {
                println!("Audio: underflow detected!");
                self.audio_underflowed = true;
            }
        }

        let mut quit = false;

        // Update controller button state.
        if let Some(ref mut win) = self.video {
            for evt in win.poll_events() {
                if let Event::KeyboardInput(pressed, _, Some(code)) = evt {
                    let bit = match code {
                        VirtualKeyCode::LShift     => 1 << 31,  // A
                        VirtualKeyCode::LControl   => 1 << 30,  // B
                        VirtualKeyCode::Z          => 1 << 29,  // Z
                        VirtualKeyCode::Return     => 1 << 28,  // Start
                        VirtualKeyCode::W          => 1 << 27,
                        VirtualKeyCode::S          => 1 << 26,  // Joypad
                        VirtualKeyCode::A          => 1 << 25,
                        VirtualKeyCode::D          => 1 << 24,
                        VirtualKeyCode::X          => 1 << 21,  // Left trigger
                        VirtualKeyCode::C          => 1 << 20,  // Right trigger
                        VirtualKeyCode::I          => 1 << 19,  // C-up
                        VirtualKeyCode::K          => 1 << 18,  // C-down
                        VirtualKeyCode::J          => 1 << 17,  // C-left
                        VirtualKeyCode::L          => 1 << 16,  // C-right
                        VirtualKeyCode::Left       => 1 << 0,
                        VirtualKeyCode::Right      => 1 << 1,  // Analog stick
                        VirtualKeyCode::Down       => 1 << 2,
                        VirtualKeyCode::Up         => 1 << 3,
                        VirtualKeyCode::RShift     => 1 << 4,
                        VirtualKeyCode::RControl   => 1 << 5,
                        VirtualKeyCode::Escape     => 1 << 10,
                        _ => 0
                    };
                    if pressed == ElementState::Pressed {
                        self.key_state |= bit;
                        if code == VirtualKeyCode::M {
                            self.audio_mute = !self.audio_mute;
                            println!("Audio: now {}.",
                                     if self.audio_mute { "muted" } else { "unmuted" });
                        }
                    } else {
                        self.key_state &= !bit;
                    }
                }
            }
            if self.key_state & (1 << 10) != 0 {
                quit = true;
            }
            let mut a_x: i8 = match self.key_state & 3 {
                1 => -0x80,
                2 => 0x7f,
                _ => 0
            };
            let mut a_y: i8 = match (self.key_state >> 2) & 3 {
                1 => -0x80,
                2 => 0x7f,
                _ => 0
            };
            match (self.key_state >> 4) & 3 {
                1 => { a_x = (a_x / 4) * 3; a_y = (a_y / 4) * 3; }
                2 => { a_x /= 2; a_y /= 2; }
                3 => { a_x /= 4; a_y /= 4; }
                _ => { }
            }
            let cstate = (self.key_state & 0xffff0000) |
                ((a_x as u8 as u32) << 8) | (a_y as u8 as u32);
            self.receiver.set_input_state(cstate);
        }
        quit
    }

    fn set_mode(&mut self, mode: ui::VideoMode) {
        if mode == self.video_mode {
            return;
        }
        drop(self.video.take());
        if mode.width == 0 || mode.height == 0 || mode.depth == ui::Depth::Blank {
            self.video_reopen = false;
        } else {
            self.video_reopen = true;
        }
        self.video_mode = mode;
    }

    fn display(&mut self, buffer: Vec<u32>) {
        if self.video_reopen {
            self.video_reopen = false;
            let mode = &self.video_mode;
            match WindowBuilder::new()
                .with_dimensions(mode.width as u32, mode.height as u32)
                .with_title(self.title.clone())
                .with_vsync()
                .build_glium() {
                Ok(win) => {
                    println!("Video: new window with resolution {}x{}, {:?}",
                             mode.width, mode.height, mode.depth);
                    self.video = Some(win);
                }
                Err(err) => {
                    println!("Unable to create window: {}", err);
                    return;
                }
            }
        }

        if let Some(ref mut win) = self.video {
            if self.video_mode.depth == ui::Depth::Rgb32 {
                Self::fill_tex(win, &self.video_mode, &buffer, 1, ClientFormat::U8U8U8);
            } else if self.video_mode.depth == ui::Depth::Rgb16 {
                Self::fill_tex(win, &self.video_mode, &buffer, 2, ClientFormat::U5U5U5U1);
            }
            // else it's blank
        }
    }

    fn fill_tex(win: &Display, vmode: &ui::VideoMode, data: &[u32], skip: usize, fmt: ClientFormat) {
        let tex = if vmode.width % skip != 0 {
            // have to convert the u32 array into u16
            let mut data = data.iter()
                           .flat_map(|&v| vec![(v >> 16) as u16, v as u16])
                           .collect::<Vec<_>>();
            data.pop().unwrap();
            let data = data.chunks(vmode.width)
                           .rev()
                           .flat_map(|row| row.iter())
                           .cloned()
                           .collect::<Vec<_>>();
            let img = RawImage2d {
                data: Cow::Owned(data),
                width: vmode.width as u32,
                height: vmode.height as u32,
                format: fmt,
            };
            Texture2d::new(win, img)
        } else {
            let data = data.chunks(vmode.width / skip)
                           .rev()
                           .flat_map(|row| row.iter())
                           .cloned()
                           .collect();
            let img = RawImage2d {
                data: Cow::Owned(data),
                width: vmode.width as u32,
                height: vmode.height as u32,
                format: fmt,
            };
            Texture2d::new(win, img)
        };
        match tex {
            Ok(tex) => {
                let frame = win.draw();
                tex.as_surface().fill(&frame, MagnifySamplerFilter::Linear);
                frame.finish().unwrap();
            }
            Err(err) => println!("Could not create texture: {}", err),
        }
    }

    fn play(&mut self, rate: u32, data: Vec<u32>) {
        if let Some(ref endpoint) = self.audio_dev {
            if self.audio_rate != rate {
                /* TODO: clear this mess up */
                let clean_rate = match rate {
                    0...7000       => 5512,
                    7001...9000    => 8000,
                    9001...13000   => 11025,
                    13001...19000  => 16000,
                    19001...26000  => 22050,
                    26001...38000  => 32000,
                    38001...46000  => 44100,
                    _              => 48000
                };
                self.audio_src = cpal::Voice::new(&endpoint, &cpal::Format {
                    channels: vec![cpal::ChannelPosition::FrontLeft,
                                   cpal::ChannelPosition::FrontRight],
                    samples_rate: cpal::SamplesRate(clean_rate),
                    data_type: cpal::SampleFormat::I16
                }).ok();
                self.audio_rate = rate;
                self.audio_underflowed = false;
                println!("Audio: opened new source with {} ({}) Hz", clean_rate, rate);
            }
            if let Some(ref mut voice) = self.audio_src {
                let nsamples = 2 * data.len();
                match voice.append_data(nsamples) {
                    cpal::UnknownTypeBuffer::F32(_) |
                    cpal::UnknownTypeBuffer::U16(_) => { },
                    cpal::UnknownTypeBuffer::I16(mut buf) => {
                        if !self.audio_mute {
                            let max = buf.deref_mut().len() / 2;
                            for (p, i) in data.iter().zip(0..max) {
                                buf[2*i] = (p >> 16) as i16;
                                buf[2*i+1] = *p as i16;
                            }
                        }
                    }
                }
                // println!("Audio: buffer filled, now pending: {}",
                //          voice.get_pending_samples());
                voice.play();
            }
        }
    }
}
