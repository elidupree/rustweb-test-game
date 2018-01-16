#![feature (macro_vis_matcher)]

#[macro_use]
extern crate stdweb;
#[macro_use]
extern crate serde_derive;
extern crate nalgebra;
extern crate array_ext;

extern crate time_steward;

use std::iter;
use std::ops::{Add, Mul};

use stdweb::web;
use array_ext::*;
use nalgebra::Vector2;


use time_steward::{DeterministicRandomId};
use time_steward::{PersistentTypeId, ListedType, PersistentlyIdentifiedType, DataHandleTrait, DataTimelineCellTrait, QueryResult, Basics as BasicsTrait};
pub use time_steward::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, Event, DataHandle, DataTimelineCell, Accessor, EventAccessor, FutureCleanupAccessor, simple_timeline, bbox_collision_detection_2d as collisions};
use simple_timeline::{SimpleTimeline, query, set, destroy};
use self::collisions::{NumDimensions, Detector as DetectorTrait};
use self::collisions::simple_grid::{SimpleGridDetector};

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
const RANGER_RANGE: Coordinate = STRIDE*10;

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct LinearTrajectory <V> {
  origin: Time,
  position: V,
  velocity: V,
}

impl <V> LinearTrajectory <V>
where Vector: Add <V, Output = V> + Mul <Time, Output = V> {
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
}

type LinearTrajectory1 = LinearTrajectory <Coordinate>;
type LinearTrajectory2 = LinearTrajectory <Vector>;

impl LinearTrajectory1 {
  fn when_reaches (&self, now: Time, amount: Coordinate)->Option <Time> {
    if self.evaluate (now) >= amount {Some (now)}
    else if self.velocity <= 0 {None}
    else {Some (self.origin + ((amount - self.position) + (self.velocity-1))/self.velocity)}
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
    BoundingBox::centered (center, radius (&varying))
  }
  fn when_escapes<A: EventAccessor <Steward = Self::Steward>>(&self, accessor: &A, object: &DataHandle<Self::Object>, bounds: BoundingBox)->Option<<<Self::Steward as TimeSteward>::Basics as BasicsTrait>::Time> {
    let varying = query (accessor, & object.varying);
    varying.trajectory.approximately_when_escapes (
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
}

type Steward = steward_module::Steward <Basics>;
type EventHandle = <Steward as TimeSteward>::EventHandle;

#[derive (Serialize, Deserialize, Debug)]
struct Globals {
  detector: Timeline <DataHandle <Detector>>,
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
enum ObjectType {
  Palace (Palace),
  Guild (Guild),
  Ranger (Ranger),
}


#[derive (Clone, Serialize, Deserialize, Debug)]
struct Object {
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


fn create_object <A: EventAccessor <Steward = Steward>>(accessor: &A, source_object: & ObjectHandle, unique: u64, trajectory: LinearTrajectory2, object_type: ObjectType) {
  let created = accessor.new_handle (Object {id: DeterministicRandomId::new (& (accessor.extended_now().id, source_object.id, unique)), varying: Timeline::new()});
  set (accessor, created.varying, ObjectVarying {
    object_type: object_type, trajectory: trajectory, detector_data: None, prediction: None, destroyed: false,
  });
  Detector::insert (accessor, get_detector (accessor), created, Some (source_object));
  object_changed (accessor, & created);
}


fn destroy_object <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
  modify (accessor, & object.varying, | varying | {
    varying.prediction = None;
    varying.destroyed = true;
  });
  Detector::remove (accessor, get_detector (accessor), object);
}

fn is_destroyed <A: Accessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
  query (accessor, & object.varying).destroyed;
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
        ObjectType::Palace (palace) => {
          iter::once (accessor.create_prediction (palace.gold.when_reaches (100*SECOND), id, BuildGuild {palace: object}))
        },
        ObjectType::Guild (guild) => {
          iter::once (accessor.create_prediction (guild.gold.when_reaches (100*SECOND), id, RecruitRanger {guild: object}))
        },
        ObjectType::Ranger (ranger) => {
          iter::once (accessor.create_prediction (ranger.thoughts.when_reaches (100*SECOND), id, Think {ranger: object}))
        },
      }
    );
    
    varying.prediction = iterator.min_by_key (| prediction | prediction.extended_time());
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
  struct BuildGuild {palace: ObjectHandle},
  PersistentTypeId(0x3995cd28e2829c09),
  fn execute (&self, accessor: &mut Accessor) {
    modify_object (accessor, & self.palace, | varying | {
      varying.gold.add (accessor.now(), - 100) ;
    });
    let varying = query (accessor, &self.palace.varying) ;
    create_object (accessor, & self.palace, 0x379661e69cdd5fe7,
      LinearTrajectory2::constant (accessor.now(), varying.trajectory.evaluate (accessor.now()) + Vector2::new (0, 10*STRIDE)),
      ObjectType::Guild (Guild {gold: LinearTrajectory::new (0, 10)}),
    );
  }
}



define_event! {
  struct Think {ranger: ObjectHandle},
  PersistentTypeId(0xe35485dcd0277599),
  fn execute (&self, accessor: &mut Accessor) {
    let attack = None;
    modify_object (accessor, & self.ranger, | varying | {
      match varying.object_type {Ranger (ranger) => ranger,_=> unreachable!()}.thoughts.add (accessor.now(), - 100) ;
      for object in Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (varying.trajectory.evaluate (*accessor.now())), RANGER_RANGE as u64), Some (& self.ranger)) {
        if is_enemy (accessor, & self.ranger, & object) {
          if distance < RANGER_RANGE {
            attack = Some (object.clone());
          }
        }
      }
    });
    if let Some(victim) = attack {
      destroy_object (accessor, & victim);
    }
  }
}






#[derive (Serialize, Deserialize, Debug)]
struct Tile {
  variable: u32,
}

struct Game {
  steward: Steward,
  now: Time,
  last_ui_time: f64,
}

//js_serializable!(Game);

fn display_location (location: Vector2 <f64>)->Vector2 <f64> {
  Vector2::new (
    105.0 + location [0]*10.0 - location [1]*10.0,
    5.0 + location [0]*5.0 + location [1]*5.0,
  )
}

fn draw_tile (location: Vector2 <usize>, tile: & Tile) {
  let center = Vector2::new (location [0] as f64, location [1] as f64);
  let corners = [
    display_location (center + Vector2::new (0.5, 0.5)),
    display_location (center + Vector2::new (0.5, -0.5)),
    display_location (center + Vector2::new (-0.5, -0.5)),
    display_location (center + Vector2::new (-0.5, 0.5)),
  ];
  js!{
    context.beginPath();
    context.moveTo(@{corners [3] [0]},@{corners [3] [1]});
  }
  for corner in corners.iter() {
    js!{
      context.lineTo(@{corner [0]},@{corner [1]});
    }
  }
  js!{
    context.strokeStyle = "rgba(0,0,0,255)";
    context.stroke();
    context.fillStyle = "rgba(255,0,0,"+@{tile.variable as f64/23.0}+")";
    context.fill ();
  }
}

fn draw_game <A: Accessor <Steward = Steward>>(accessor: &A) {
  js! {
    context.clearRect (0, 0, canvas.width, canvas.height);
  }
  for handle in Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered ([0, 0], PALACE_DISTANCE as u64*2), None) {
    js! {
      context.clearRect (0, 0, canvas.width, canvas.height);
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
  
  /*
  //game.variable += 1;
  js! {
    context.clearRect (0, 0, canvas.width, canvas.height);
  }
  for (first, whatever) in game.tiles.iter_mut().enumerate() {
    for (second, tile) in whatever.iter_mut().enumerate() {
      draw_tile (Vector2::new (first, second), tile);
      tile.variable = (tile.variable + first as u32) % 23;
    }
  }*/
  web::window().request_animation_frame (move | time | main_loop (time, game));
}

fn main() {
  stdweb::initialize();

  //let message = "Hello!";
  js! {
    var canvas = window.canvas = document.createElement ("canvas");
    document.body.appendChild (canvas);
    window.context = canvas.getContext ("2d");
      
    //alert( @{message} );
  }
  println!("uuu");

  
  let game = Game {tiles: Array::from_fn (|_| Array::from_fn (|_| Tile {variable: 0}))};
  
  web::window().request_animation_frame (move | time | main_loop (time, game));

  stdweb::event_loop();
}
