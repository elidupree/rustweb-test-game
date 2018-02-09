#![feature (macro_vis_matcher)]
#![recursion_limit="128"]

#[macro_use]
extern crate stdweb;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate derivative;
extern crate nalgebra;
extern crate array_ext;
extern crate rand;
extern crate boolinator;

extern crate time_steward;

#[macro_use]
mod misc;
#[macro_use]
mod physics_infrastructure;
mod physics;
mod ui;

pub use time_steward::stewards::{simple_full as steward_module};

pub use misc::*;
pub use physics_infrastructure::*;
pub use physics::*;
pub use ui::*;

fn main() {ui::run()}
