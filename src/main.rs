#![feature (macro_vis_matcher)]
#![recursion_limit="128"]

#[macro_use]
extern crate stdweb;
#[macro_use]
extern crate serde_derive;
extern crate nalgebra;
extern crate array_ext;
extern crate rand;

extern crate time_steward;

use std::iter;
use std::ops::{Add, AddAssign, Mul};

use stdweb::web;
use array_ext::*;
use nalgebra::{Vector2};
use rand::Rng;


use time_steward::{DeterministicRandomId};
use time_steward::{PersistentTypeId, ListedType, PersistentlyIdentifiedType, DataTimelineCellTrait, QueryResult, EventHandleTrait, Basics as BasicsTrait};
pub use time_steward::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, Event, DataHandle, DataTimelineCell, Accessor, EventAccessor, FutureCleanupAccessor, simple_timeline, bbox_collision_detection_2d as collisions};
use simple_timeline::{SimpleTimeline, query, set};
use self::collisions::{NumDimensions, Detector as DetectorTrait};
use self::collisions::simple_grid::{SimpleGridDetector};

use time_steward::support::rounding_error_tolerant_math::Range;

fn modify<A: EventAccessor <Steward = Steward>, T: QueryResult, F: FnOnce(&mut T)>(accessor: &A, cell: &DataTimelineCell <SimpleTimeline <T, Steward>>, f: F) {
  let mut data = query (accessor, cell);
  (f)(&mut data);
  set (accessor, cell, data);
}

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

type Timeline <T> = DataTimelineCell <SimpleTimeline <T, Steward>>;
fn new_timeline <T: QueryResult> ()->Timeline <T> {DataTimelineCell::new (SimpleTimeline::new())}
type Detector = SimpleGridDetector <Space>;
type BoundingBox = collisions::BoundingBox<Space>;
type DetectorData = collisions::simple_grid::DetectorDataPerObject<Space>;

type Time = i64;
type Coordinate = i64;
type Vector = Vector2 <Coordinate>;

const SECOND: Time = 1 << 20;
const STRIDE: Coordinate = SECOND << 8;
const PALACE_RADIUS: Coordinate = STRIDE*20;
const GUILD_RADIUS: Coordinate = STRIDE*15;
const PALACE_DISTANCE: Coordinate = PALACE_RADIUS*5;
const RANGER_RANGE: Coordinate = STRIDE*20;

fn distance_squared (first: Vector, second: Vector)->Range {
  Range::exactly (second [0] - first [0])*Range::exactly (second [0] - first [0])
  + Range::exactly (second [1] - first [1])*Range::exactly (second [1] - first [1])
}
fn distance (first: Vector, second: Vector)->Range {
  distance_squared (first, second).sqrt().unwrap()
}
fn normalized_to (mut vector: Vector, length: Coordinate)->Vector {
  while vector [0].abs() > (1<<10) || vector [1].abs() > (1<<10) { vector /= 2; }
  vector*length*100/(distance (vector*100, Vector::new (0, 0)).max())
}
fn random_vector<G: Rng> (generator: &mut G, length: Coordinate)->Vector {
  loop {
    let vector = Vector::new (
      generator.gen_range (- length, length+1),
      generator.gen_range (- length, length+1),);
    let test_length = distance(vector, Vector::new (0, 0)).max();
    if test_length <= length && test_length*2 >= length {
      return normalized_to (vector, length);
    }
  }
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct LinearTrajectory <V> {
  origin: Time,
  position: V,
  velocity: V,
}

impl <V> LinearTrajectory <V>
where V: Copy + Add <V, Output = V> + AddAssign <V> + Mul <Time, Output = V> {
  fn update (&mut self, time: Time) {
    self.position = self.evaluate (time) ;
    self.origin = time;
  }
  fn add (&mut self, time: Time, added: V) {
    self.update (time) ;
    self.position += added;
  }
  fn add_velocity (&mut self, time: Time, added: V) {
    self.update (time) ;
    self.velocity += added;
  }
  fn set_velocity (&mut self, time: Time, added: V) {
    self.update (time) ;
    self.velocity = added;
  }
  fn evaluate (&self, time: Time)->V {
    self.position + self.velocity*(time - self.origin)
  }
  fn new (time: Time, position: V, velocity: V)->Self {
    LinearTrajectory {origin: time, position: position, velocity: velocity}
  }
  
}

type LinearTrajectory1 = LinearTrajectory <Coordinate>;
type LinearTrajectory2 = LinearTrajectory <Vector>;

impl LinearTrajectory1 {
  fn when_reaches (&self, now: Time, amount: Coordinate)->Option <Time> {
    if self.evaluate (now) >= amount {Some (now)}
    else if self.velocity <= 0 {None}
    else {Some (self.origin + ((amount - self.position) + (self.velocity-1))/self.velocity)}
  }
  fn constant (time: Time, position: Coordinate)->Self {
    LinearTrajectory {origin: time, position: position, velocity: 0}
  }
}
impl LinearTrajectory2 {
  fn when_escapes (&self, now: Time, bounds: [[Coordinate; 2]; 2])->Option <Time> {
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
  fn constant (time: Time, position: Vector)->Self {
    LinearTrajectory {origin: time, position: position, velocity: Vector::new (0, 0)}
  }
}




fn to_collision_space (coordinate: Coordinate)->collisions::Coordinate {
  (coordinate as collisions::Coordinate).wrapping_sub(1u64 << 63)
}
fn from_collision_space (coordinate: collisions::Coordinate)->Coordinate {
  (coordinate.wrapping_add(1u64 << 63)) as Coordinate
}

fn to_collision_vector (vector: Vector)->[collisions::Coordinate; 2] {
  Array::from_fn (| dimension | to_collision_space (vector [dimension]))
}


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Space;
impl PersistentlyIdentifiedType for Space {
  const ID: PersistentTypeId = PersistentTypeId(0x879511343e48addd);
}
impl collisions::Space for Space {
  type Steward = Steward;
  type Object = Object;
  type DetectorDataPerObject = DetectorData;
  type UniqueId = DeterministicRandomId;
  
  const DIMENSIONS: NumDimensions = 2;

  // An Object generally has to store some opaque data for the collision detector.
  // It would normally include a DataHandle to a tree node.
  // These are getter and setter methods for that data.
  fn get_detector_data<A: Accessor <Steward = Self::Steward>>(&self, accessor: &A, object: &DataHandle<Self::Object>)->Option<Self::DetectorDataPerObject> {
    query (accessor, &object.varying).detector_data
  }
  fn set_detector_data<A: EventAccessor <Steward = Self::Steward>>(&self, accessor: &A, object: &DataHandle<Self::Object>, data: Option<Self::DetectorDataPerObject>) {
    modify (accessor, &object.varying, | varying | varying.detector_data = data);
  }
  fn unique_id<A: EventAccessor <Steward = Self::Steward>>(&self, _accessor: &A, object: &DataHandle<Self::Object>)->Self::UniqueId {
    object.id
  }

  fn current_bounding_box<A: EventAccessor <Steward = Self::Steward>>(&self, accessor: &A, object: &DataHandle<Self::Object>)->BoundingBox {
    let varying = query (accessor, & object.varying);
    let center = varying.trajectory.evaluate (*accessor.now());
    BoundingBox::centered (to_collision_vector (center), radius (&varying) as u64)
  }
  fn when_escapes<A: EventAccessor <Steward = Self::Steward>>(&self, accessor: &A, object: &DataHandle<Self::Object>, bounds: BoundingBox)->Option<<<Self::Steward as TimeSteward>::Basics as BasicsTrait>::Time> {
    let varying = query (accessor, & object.varying);
    varying.trajectory.when_escapes (
      accessor.now().clone(),
      [
        [from_collision_space (bounds.bounds [0] [0]) + radius (&varying), from_collision_space (bounds.bounds [0] [1]) - radius (&varying)],
        [from_collision_space (bounds.bounds [1] [0]) + radius (&varying), from_collision_space (bounds.bounds [1] [1]) - radius (&varying)],
      ]
    )
  }
}


#[derive (Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Default)]
pub struct Basics {}
impl BasicsTrait for Basics {
  type Time = Time;
  type Globals = Globals;
  type Types = (ListedType <BuildGuild>);
  const MAX_ITERATION: u32 = 12;
}

type Steward = steward_module::Steward <Basics>;
type EventHandle = <Steward as TimeSteward>::EventHandle;

#[derive (Serialize, Deserialize, Debug)]
pub struct Globals {
  detector: Timeline <DataHandle <Detector>>,
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
enum ObjectType {
  Palace (Palace),
  Guild (Guild),
  Ranger (Ranger),
}


#[derive (Clone, Serialize, Deserialize, Debug)]
pub struct Object {
  id: DeterministicRandomId,
  varying: Timeline <ObjectVarying>,
}
impl PersistentlyIdentifiedType for Object {
  const ID: PersistentTypeId = PersistentTypeId(0x2079a1bb8d4e9d9f);
}
type ObjectHandle = DataHandle <Object>;

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct ObjectVarying {
  object_type: ObjectType,
  trajectory: LinearTrajectory2,
  detector_data: Option <DetectorData>,
  team: usize,
  prediction: Option <EventHandle>,
  destroyed: bool,
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Palace {
  gold: LinearTrajectory1,
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Guild {
  gold: LinearTrajectory1,
}


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Ranger {
  thoughts: LinearTrajectory1,
  target: Option <ObjectHandle>,
}

macro_rules! unwrap_object_type {
  (
    $varying: expr, $Variant: ident
  ) => {
match $varying.object_type {ObjectType::$Variant (ref mut value) => value,_=> unreachable!()}
  }
}


fn create_object_impl <A: EventAccessor <Steward = Steward>>(accessor: &A, source_object: Option <& ObjectHandle>, id: DeterministicRandomId, team: usize, trajectory: LinearTrajectory2, object_type: ObjectType) {
  let created = accessor.new_handle (Object {id: id, varying: new_timeline()});
  set (accessor, & created.varying, ObjectVarying {
    object_type: object_type, trajectory: trajectory, team: team, detector_data: None, prediction: None, destroyed: false,
  });
  Detector::insert (accessor, & get_detector (accessor), & created, source_object);
  object_changed (accessor, & created);
}

fn create_object <A: EventAccessor <Steward = Steward>>(accessor: &A, source_object: & ObjectHandle, unique: u64, trajectory: LinearTrajectory2, object_type: ObjectType) {
  create_object_impl (accessor, Some (source_object),
    DeterministicRandomId::new (& (accessor.extended_now().id, source_object.id, unique)), 
    query (accessor, & source_object.varying).team, 
    trajectory,
    object_type) ;
}


fn destroy_object <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
  modify (accessor, & object.varying, | varying | {
    varying.prediction = None;
    varying.destroyed = true;
  });
  Detector::remove (accessor, & get_detector (accessor), object);
}

fn is_destroyed <A: Accessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle)->bool {
  query (accessor, & object.varying).destroyed
}

fn is_enemy <A: Accessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle, other: & ObjectHandle)->bool {
  query (accessor, & object.varying).team != query (accessor, & other.varying).team
}

fn get_detector <A: Accessor <Steward = Steward>>(accessor: &A)->DataHandle <Detector> {
  query (accessor, & accessor.globals().detector)
}


fn modify_object <A: EventAccessor <Steward = Steward>, F: FnOnce(&mut ObjectVarying)>(accessor: &A, object: & ObjectHandle, f: F) {
  modify (accessor, & object.varying, |varying| {
    (f)(varying)
  });
  object_changed (accessor, object);
}

fn object_changed <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
  let id = DeterministicRandomId::new (& (0x93562b6a9bcdca8cu64, accessor.extended_now().id, object.id));
  modify (accessor, & object.varying, | varying | {
    let iterator = iter::empty().chain (
      match varying.object_type {
        ObjectType::Palace (ref palace) => {
          iter::once (accessor.create_prediction (palace.gold.when_reaches (*accessor.now(), 100*SECOND).unwrap(), id, BuildGuild {palace: object.clone()}))
        },
        ObjectType::Guild (ref guild) => {
          iter::once (accessor.create_prediction (guild.gold.when_reaches (*accessor.now(), 100*SECOND).unwrap(), id, RecruitRanger {guild: object.clone()}))
        },
        ObjectType::Ranger (ref ranger) => {
          iter::once (accessor.create_prediction (ranger.thoughts.when_reaches (*accessor.now(), 10*SECOND).unwrap(), id, Think {ranger: object.clone()}))
        },
      }
    );
    
    varying.prediction = iterator.min_by_key (| prediction | prediction.extended_time().clone());
  });
  Detector::changed_position (accessor, & get_detector (accessor), object);
}

fn radius (varying: & ObjectVarying)->Coordinate {
  match varying.object_type {
    ObjectType::Palace (_) => PALACE_RADIUS,
    ObjectType::Guild (_) => GUILD_RADIUS,
    ObjectType::Ranger (_) => STRIDE/2,
  }
}


define_event! {
  pub struct Initialize {},
  PersistentTypeId(0x9a633852de46827f),
  fn execute (&self, accessor: &mut Accessor) {
    set (accessor, &accessor.globals().detector, SimpleGridDetector::new (accessor, Space, (STRIDE*50) as collisions::Coordinate));
    for team in 0..2 {
      create_object_impl (accessor, None, DeterministicRandomId::new (& (team, 0xb2e085cd02f2f8dbu64)), team,
        LinearTrajectory2::constant (*accessor.now(), Vector2::new (0, PALACE_DISTANCE*team as Coordinate*2 - PALACE_DISTANCE)),
        ObjectType::Palace (Palace {gold: LinearTrajectory1::new (*accessor.now(), 0, 10)}),
      );
    }
  }
}

define_event! {
  pub struct BuildGuild {palace: ObjectHandle},
  PersistentTypeId(0x3995cd28e2829c09),
  fn execute (&self, accessor: &mut Accessor) {
    modify_object (accessor, & self.palace, | varying | {
      unwrap_object_type!(varying, Palace).gold.add (*accessor.now(), - 100*SECOND) ;
    });
    let mut varying = query (accessor, &self.palace.varying) ;
    create_object (accessor, & self.palace, 0x379661e69cdd5fe7,
      LinearTrajectory2::constant (*accessor.now(), varying.trajectory.evaluate (*accessor.now()) + random_vector (&mut accessor.extended_now().id.to_rng(), PALACE_RADIUS + GUILD_RADIUS + 10*STRIDE)),
      ObjectType::Guild (Guild {gold: LinearTrajectory1::new (*accessor.now(), 0, 10)}),
    );
    println!("{:?}", (unwrap_object_type!(varying, Palace)));
    println!("{:?}", (&varying.object_type, &varying.trajectory));
  }
}

define_event! {
  pub struct RecruitRanger {guild: ObjectHandle},
  PersistentTypeId(0x90198a81b2628f04),
  fn execute (&self, accessor: &mut Accessor) {
    modify_object (accessor, & self.guild, | varying | {
      unwrap_object_type!(varying, Guild).gold.add (*accessor.now(), - 100*SECOND) ;
    });
    let varying = query (accessor, &self.guild.varying) ;
    create_object (accessor, & self.guild, 0x91db5029ba8b0a4e,
      LinearTrajectory2::constant (*accessor.now(), varying.trajectory.evaluate (*accessor.now()) + random_vector (&mut accessor.extended_now().id.to_rng(), GUILD_RADIUS + 2*STRIDE)),
      ObjectType::Ranger (Ranger {thoughts: LinearTrajectory1::new (*accessor.now(), 0, 10), target: None}),
    );
  }
}



define_event! {
  pub struct Think {ranger: ObjectHandle},
  PersistentTypeId(0xe35485dcd0277599),
  fn execute (&self, accessor: &mut Accessor) {
    let mut attack = None;
    modify_object (accessor, & self.ranger, | varying | {
      //let ranger =
      unwrap_object_type!(varying, Ranger).thoughts.add (*accessor.now(), - 10*SECOND) ;
      let position = varying.trajectory.evaluate (*accessor.now());
      for object in Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (position), RANGER_RANGE as u64), Some (& self.ranger)) {
        if is_enemy (accessor, & self.ranger, & object) {
          if distance_squared (position, query (accessor, & object.varying).trajectory.evaluate (*accessor.now())) <= Range::exactly (RANGER_RANGE)*RANGER_RANGE {
            attack = Some (object.clone());
          }
        }
      }
      
      if unwrap_object_type!(varying, Ranger).target.as_ref().map_or (true, | target | is_destroyed (accessor, target)) {
        for object in Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (position), PALACE_DISTANCE as u64*3), Some (& self.ranger)) {
          let other_varying = query (accessor, & object.varying);
          if is_enemy (accessor, & self.ranger, & object) && match other_varying.object_type {ObjectType::Ranger (_) => false,_=> true} {
            let other_position = other_varying.trajectory.evaluate (*accessor.now());
            unwrap_object_type!(varying, Ranger).target = Some (object);
            let new_velocity = normalized_to (other_position - position, 10*STRIDE/SECOND);
            //println!("vel {:?}", (&new_velocity));
            varying.trajectory.set_velocity (*accessor.now(), new_velocity);
          }
        }
      }
    });
    if let Some(victim) = attack {
      destroy_object (accessor, & victim);
    }
  }
}



struct Game {
  steward: Steward,
  now: Time,
  last_ui_time: f64,
}

fn draw_game <A: Accessor <Steward = Steward>>(accessor: &A) {
  js! {
    context.clearRect (0, 0, canvas.width, canvas.height);
  }
  for object in Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (Vector::new (0, 0)), PALACE_DISTANCE as u64*2), None) {
    let scale = STRIDE as f64/2.0;
    let varying = query (accessor, & object.varying);
    let center = varying.trajectory.evaluate (*accessor.now());
    let center = Vector2::new (center [0] as f64 + PALACE_DISTANCE as f64, center [1] as f64 + PALACE_DISTANCE as f64*1.5)/scale;
    let object_radius = radius (& varying) as f64/scale ;
    //println!("{:?}", (varying.trajectory, center, object_radius));
    js! {
      context.beginPath();
      context.arc (@{center [0]},@{center [1]},@{object_radius}, 0, Math.PI*2);
      context.strokeStyle = "rgba("+@{varying.team as i32*255}+",0,"+@{(1-varying.team as i32)*255}+",1.0)";
      context.stroke();
      if (@{varying.team == 1}) {
        context.fillStyle = "rgba(255,0,0,0.2)";
        context.fill();
      }
    }
  }
}

fn main_loop (time: f64, mut game: Game) {
  let observed_duration = time - game.last_ui_time;
  let duration_to_simulate = if observed_duration < 100.0 {observed_duration} else {100.0};
  let duration_to_simulate = (duration_to_simulate*(SECOND as f64)/1000.0) as Time;
  assert!(duration_to_simulate >= 0) ;
  game.last_ui_time = time;
  game.now += duration_to_simulate;
  let snapshot = game.steward.snapshot_before (& game.now). unwrap ();
  draw_game (& snapshot);
  game.steward.forget_before (& game.now);
  
  web::window().request_animation_frame (move | time | main_loop (time, game));
}

fn main() {
  stdweb::initialize();
  
    js! {
    var canvas = window.canvas = document.createElement ("canvas");
    canvas.setAttribute ("width", 600);
    canvas.setAttribute ("height", 600);
    document.body.appendChild (canvas);
    window.context = canvas.getContext ("2d");
  }
  
  let mut steward: Steward = Steward::from_globals (Globals {detector: new_timeline()});
  steward.insert_fiat_event (0, DeterministicRandomId::new (& 0xae06fcf3129d0685u64), Initialize {}).unwrap();
  let game = Game {steward: steward, now: 1, last_ui_time: 0.0};
  
  web::window().request_animation_frame (move | time | main_loop (time, game));

  stdweb::event_loop();
}
