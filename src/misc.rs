// ##########################################
// ######  annoying abstraction stuff #######
// ##########################################
// 
// either general-use support features or annoying boilerplate.
// A lot of it should be defined, in one form or another, in TimeSteward support libraries,
// rather than this game specifically.

use super::*;

use std::ops::{Add, AddAssign, Mul};

use array_ext::*;
use rand::Rng;
use integer_sqrt::IntegerSquareRoot;


use time_steward::{QueryResult};
pub use time_steward::stewards::{simple_full as steward_module};
use steward_module::{DataHandle, DataTimelineCell, Accessor, EventAccessor, simple_timeline, bbox_collision_detection_2d as collisions};
use self::simple_timeline::{SimpleTimeline, query, set};

use time_steward::support::rounding_error_tolerant_math::Range;

macro_rules! printlnerr(
    ($($arg:tt)*) => { {use std::io::Write;
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);

macro_rules! define_event {
  (
    $visibility: vis struct $Struct: ident {$($contents:tt)*},
    PersistentTypeId($id: expr),
    fn execute $($execute:tt)*
  ) => {
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
$visibility struct $Struct {$($contents)*}
impl PersistentlyIdentifiedType for $Struct {
  const ID: PersistentTypeId = PersistentTypeId($id);
}
impl Event for $Struct {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> $($execute)*
  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, _accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}
  }
}


pub fn get_detector <A: Accessor <Steward = Steward>>(accessor: &A)->DataHandle <Detector> {
  query (accessor, & accessor.globals().detector)
}

pub fn modify<A: EventAccessor <Steward = Steward>, T: QueryResult, F: FnOnce(&mut T)>(accessor: &A, cell: &DataTimelineCell <SimpleTimeline <T, Steward>>, f: F) {
  let mut data = query (accessor, cell);
  (f)(&mut data);
  set (accessor, cell, data);
}


pub fn magnitude (vector: Vector)->Coordinate {
  let vector = Vector::new (vector[0].abs(), vector[1].abs());
  let mut shift = 0;
  loop {
    let reduced = Vector::new (vector[0] >> shift, vector[1] >> shift);
    if reduced[0] < (1i64 << 31) && reduced[1] < (1i64 << 31) {
      return (reduced[0]*reduced[0] + reduced[1]*reduced[1]).integer_sqrt() << shift
    }
    shift += 16;
  }
  //distance_squared (first, second).sqrt().unwrap()
}
pub fn magnitude_less_than (vector: Vector, threshold: Coordinate)->bool {
  let vector = Vector::new (vector[0].abs(), vector[1].abs());
  if vector [0] >= threshold || vector [1] >= threshold {return false}
  if vector [0] + vector [1] < threshold {return true}
  let mut shift = 0;
  loop {
    let reduced = Vector::new (vector[0] >> shift, vector[1] >> shift);
    let reduced_threshold = threshold >> shift;
    if reduced[0] < (1i64 << 31) && reduced[1] < (1i64 << 31) && reduced_threshold < (1i64 << 31) {
      return (reduced[0]*reduced[0] + reduced[1]*reduced[1]) < reduced_threshold*reduced_threshold
    }
    shift += 16;
  }
}
pub fn octagonal_magnitude (vector: Vector)->Coordinate {
  let vector = Vector::new (vector[0].abs(), vector[1].abs());
  let (bigger, smaller) = if vector [0] > vector [1] {(vector [0], vector [1])} else {(vector [1], vector [0])};
  (bigger*1007 + smaller*441 + 512)/1024
}
pub fn distance_squared (first: Vector, second: Vector)->Range {
  Range::exactly (second [0] - first [0])*Range::exactly (second [0] - first [0])
  + Range::exactly (second [1] - first [1])*Range::exactly (second [1] - first [1])
}
pub fn distance (first: Vector, second: Vector)->Coordinate {
  magnitude (second - first)
}
pub fn distance_less_than (first: Vector, second: Vector, threshold: Coordinate)->bool {
  magnitude_less_than (second - first, threshold)
}
pub fn octagonal_distance (first: Vector, second: Vector)->Coordinate {
  octagonal_magnitude (second - first)
}
pub fn normalized_to (mut vector: Vector, length: Coordinate)->Vector {
  while vector [0].abs() > (1<<10) || vector [1].abs() > (1<<10) { vector /= 2; }
  vector*length*100/magnitude (vector*100)
}
pub fn random_vector_exact_length <G: Rng> (generator: &mut G, length: Coordinate)->Vector {
  loop {
    let vector = Vector::new (
      generator.gen_range (- length, length+1),
      generator.gen_range (- length, length+1),);
    let test_length = magnitude (vector);
    if test_length <= length && test_length*2 >= length {
      return normalized_to (vector, length);
    }
  }
}
pub fn random_vector_within_length <G: Rng> (generator: &mut G, length: Coordinate)->Vector {
  loop {
    let vector = Vector::new (
      generator.gen_range (- length, length+1),
      generator.gen_range (- length, length+1),);
    let test_length = magnitude (vector);
    if test_length <= length {
      return vector;
    }
  }
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct LinearTrajectory <V> {
  pub origin: Time,
  pub position: V,
  pub velocity: V,
}

impl <V> LinearTrajectory <V>
where V: Copy + Add <V, Output = V> + AddAssign <V> + Mul <Time, Output = V> {
  pub fn update (&mut self, time: Time) {
    self.position = self.evaluate (time) ;
    self.origin = time;
  }
  pub fn add (&mut self, time: Time, added: V) {
    self.update (time) ;
    self.position += added;
  }
  pub fn add_velocity (&mut self, time: Time, added: V) {
    self.update (time) ;
    self.velocity += added;
  }
  pub fn set (&mut self, time: Time, added: V) {
    self.update (time) ;
    self.position = added;
  }
  pub fn set_velocity (&mut self, time: Time, added: V) {
    self.update (time) ;
    self.velocity = added;
  }
  pub fn evaluate (&self, time: Time)->V {
    self.position + self.velocity*(time - self.origin)
  }
  pub fn new (time: Time, position: V, velocity: V)->Self {
    LinearTrajectory {origin: time, position: position, velocity: velocity}
  }
  
}

pub type LinearTrajectory1 = LinearTrajectory <Coordinate>;
pub type LinearTrajectory2 = LinearTrajectory <Vector>;

impl LinearTrajectory1 {
  pub fn when_reaches (&self, now: Time, amount: Coordinate)->Option <Time> {
    if self.evaluate (now) >= amount {Some (now)}
    else if self.velocity <= 0 {None}
    else {Some (self.origin + ((amount - self.position) + (self.velocity-1))/self.velocity)}
  }
  pub fn constant (time: Time, position: Coordinate)->Self {
    LinearTrajectory {origin: time, position: position, velocity: 0}
  }
}
impl ::std::ops::Neg for LinearTrajectory1 {
  type Output = LinearTrajectory1;
  fn neg (self)-> LinearTrajectory1 {Self::new (self.origin, - self.position, - self.velocity)}
}
impl LinearTrajectory2 {
  pub fn when_escapes (&self, now: Time, bounds: [[Coordinate; 2]; 2])->Option <Time> {
    bounds.iter().enumerate().flat_map (| (dimension, bounds) |
      bounds.iter().enumerate().filter_map (move | (direction, bound) | {
        if direction == 0 {
          LinearTrajectory1::new (self.origin, -self.position [dimension], -self.velocity [dimension]).when_reaches (now, (-bound) + 1)
        }
        else {
          LinearTrajectory1::new (self.origin, self.position [dimension], self.velocity [dimension]).when_reaches (now, bound + 1)
        }
      })
    ).min()
  }
  pub fn when_collides (&self, now: Time, other: & Self, distance: Coordinate)->Option <Time> {
    let mut displacement = self.clone();
    displacement.update (now);
    let mut other = other.clone();
    other.update (now);
    displacement.position -= other.position;
    displacement.velocity -= other.velocity;
    //let polynomial = time_steward::support::rounding_error_tolerant_math::multiply_polynomials (
    //  &[Range::exactly (displacement.position[0]), Range::exactly (displacement.velocity [0])
    let polynomial = [
      Range::exactly (displacement.position [0])*displacement.position [0] +
      Range::exactly (displacement.position [1])*displacement.position [1] - Range::exactly (distance)*distance,
      Range::exactly (displacement.position [0])*displacement.velocity[0]*2 +
      Range::exactly (displacement.position [1])*displacement.velocity[1]*2,
      Range::exactly (displacement.velocity [0])*displacement.velocity[0] +
      Range::exactly (displacement.velocity [1])*displacement.velocity[1],
      ];
    let result = time_steward::support::rounding_error_tolerant_math::roots (& polynomial,
      0,
      Time::max_value(),
    ).first().and_then (| front | front.max().checked_add(now));
    //println!("{:?}", (displacement, polynomial, distance, result));
    result
  }
  pub fn constant (time: Time, position: Vector)->Self {
    LinearTrajectory {origin: time, position: position, velocity: Vector::new (0, 0)}
  }
}




pub fn to_collision_space (coordinate: Coordinate)->collisions::Coordinate {
  (coordinate as collisions::Coordinate).wrapping_sub(1u64 << 63)
}
pub fn from_collision_space (coordinate: collisions::Coordinate)->Coordinate {
  (coordinate.wrapping_add(1u64 << 63)) as Coordinate
}

pub fn to_collision_vector (vector: Vector)->[collisions::Coordinate; 2] {
  Array::from_fn (| dimension | to_collision_space (vector [dimension]))
}
