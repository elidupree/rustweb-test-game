#![feature (macro_vis_matcher)]
#![recursion_limit="128"]

#[macro_use]
extern crate stdweb;
#[macro_use]
extern crate serde_derive;
extern crate nalgebra;
extern crate array_ext;
extern crate rand;
extern crate boolinator;

extern crate time_steward;

use std::ops::{Add, AddAssign, Mul};
use std::rc::Rc;
use std::cell::RefCell;

use stdweb::web;
use stdweb::unstable::TryInto;

use array_ext::*;
use nalgebra::{Vector2};
use rand::Rng;
use boolinator::Boolinator;


use time_steward::{DeterministicRandomId};
use time_steward::{PersistentTypeId, ListedType, PersistentlyIdentifiedType, DataTimelineCellTrait, QueryResult, EventHandleTrait, Basics as BasicsTrait};
pub use time_steward::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, Event, DataHandle, DataTimelineCell, Accessor, EventAccessor, FutureCleanupAccessor, simple_timeline, bbox_collision_detection_2d as collisions};
use simple_timeline::{SimpleTimeline, query, query_ref, set};
use self::collisions::{NumDimensions, Detector as DetectorTrait};
use self::collisions::simple_grid::{SimpleGridDetector};

use time_steward::support::rounding_error_tolerant_math::Range;


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
const AWARENESS_RANGE: Coordinate = STRIDE*100;


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
    query_ref (accessor, &object.varying).detector_data.clone()
  }
  fn set_detector_data<A: EventAccessor <Steward = Self::Steward>>(&self, accessor: &A, object: &DataHandle<Self::Object>, data: Option<Self::DetectorDataPerObject>) {
    modify (accessor, &object.varying, | varying | varying.detector_data = data);
  }
  fn unique_id<A: EventAccessor <Steward = Self::Steward>>(&self, _accessor: &A, object: &DataHandle<Self::Object>)->Self::UniqueId {
    object.id
  }

  fn current_bounding_box<A: EventAccessor <Steward = Self::Steward>>(&self, accessor: &A, object: &DataHandle<Self::Object>)->BoundingBox {
    let varying = query_ref (accessor, & object.varying);
    let center = varying.trajectory.evaluate (*accessor.now());
    BoundingBox::centered (to_collision_vector (center), radius (&varying) as u64)
  }
  fn when_escapes<A: EventAccessor <Steward = Self::Steward>>(&self, accessor: &A, object: &DataHandle<Self::Object>, bounds: BoundingBox)->Option<<<Self::Steward as TimeSteward>::Basics as BasicsTrait>::Time> {
    let varying = query_ref (accessor, & object.varying);
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
  type Types = (ListedType <CompleteAction>);
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
  Palace,
  Guild,
  Ranger,
  Arrow,
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
enum ActionType {
  BuildGuild,
  RecruitRanger,
  Think,
  Shoot,
  Disappear,
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Action {
  action_type: ActionType,
  target: Option <ObjectHandle>,
  progress: LinearTrajectory1,
  cost: Coordinate,
}

impl Default for Action {fn default()->Self {Action {
  action_type: ActionType::Think,
  target: None,
  progress: LinearTrajectory1::new (0, 0, 0),
  cost: 999*SECOND,
}}}


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
  hitpoints: i64,
  action: Option <Action>,
  target: Option <ObjectHandle>,
  target_location: Option <Vector>,
  home: Option <ObjectHandle>,
  dependents: Vec <ObjectHandle>,
  prediction: Option <EventHandle>,
  destroyed: bool,
}

impl Default for ObjectVarying {fn default()->Self {ObjectVarying {
  object_type: ObjectType::Palace,
  trajectory: LinearTrajectory2::constant (0, Vector::new (0, 0)),
  detector_data: None,
  team: 0,
  hitpoints: 1,
  action: None,
  target: None,
  target_location: None,
  home: None,
  dependents: Vec::new(),
  prediction: None,
  destroyed: false,
}}}



fn create_object_impl <A: EventAccessor <Steward = Steward>>(accessor: &A, source_object: Option <& ObjectHandle>, id: DeterministicRandomId, varying: ObjectVarying)->ObjectHandle {
  let created = accessor.new_handle (Object {id: id, varying: new_timeline()});
  set (accessor, & created.varying, varying);
  Detector::insert (accessor, & get_detector (accessor), & created, source_object);
  choose_action (accessor, & created) ;
  //object_changed (accessor, & created);
  created
}

fn create_object <A: EventAccessor <Steward = Steward>>(accessor: &A, source_object: & ObjectHandle, unique: u64, varying: ObjectVarying)->ObjectHandle {
  create_object_impl (accessor, Some (source_object),
    DeterministicRandomId::new (& (accessor.extended_now().id, source_object.id, unique)), 
    varying)
}


fn destroy_object <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
  let mut home = None;
  modify (accessor, & object.varying, | varying | {
    varying.prediction = None;
    varying.action = None;
    varying.destroyed = true;
    home = varying.home.take();
  });
  let nearby = Detector::objects_near_object (accessor, & get_detector (accessor), object);
  Detector::remove (accessor, & get_detector (accessor), object);
  
  if let Some(home) = home { if !is_destroyed(accessor, &home) {
    let mut reconsider = false;
    modify_object (accessor, & home, | varying | {
      varying.dependents.retain (|a| a != object);
      reconsider = varying.action.is_none();
    });
    if reconsider {choose_action (accessor, & home) ;}
  }}
  
  // fix any predictions of colliding with this
  for other in nearby {
    assert! (!is_destroyed (accessor, & other), "destroyed objects shouldn't be in the collision detection") ;
    let mut update = false;
    if let Some(collide) = query_ref (accessor, & other.varying).prediction.as_ref() {
      if let Some(collide) = collide.downcast_ref::<Collide>() {
        if &collide.objects [0] == object || &collide.objects [1] == object {
          update = true;
        }
      }
    }
    if update { update_prediction (accessor, & other); }
  }
}

fn is_destroyed <A: Accessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle)->bool {
  query_ref (accessor, & object.varying).destroyed
}

fn is_enemy <A: Accessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle, other: & ObjectHandle)->bool {
  query_ref (accessor, & object.varying).team != query_ref (accessor, & other.varying).team
}


fn modify_object <A: EventAccessor <Steward = Steward>, F: FnOnce(&mut ObjectVarying)>(accessor: &A, object: & ObjectHandle, f: F) {
  modify (accessor, & object.varying, |varying| {
    (f)(varying)
  });
  object_changed (accessor, object);
}

fn update_prediction <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
  let id = DeterministicRandomId::new (& (0x93562b6a9bcdca8cu64, accessor.extended_now().id, object.id));
  modify (accessor, & object.varying, | varying | {
    let mut earliest_prediction = None;
    {
      let mut consider = | prediction: EventHandle | {
        if earliest_prediction.as_ref().map_or (true, | earliest: & EventHandle | prediction.extended_time() < earliest.extended_time()) {earliest_prediction = Some (prediction);}
      };
      if let Some (action) = varying.action.as_ref() {
        consider (accessor.create_prediction (action.progress.when_reaches (*accessor.now(), action.cost).unwrap(), id, CompleteAction {object: object.clone()}));
      }
      for other in Detector::objects_near_object (accessor, & get_detector (accessor), object) {
        let other_varying = query_ref (accessor, & other.varying);
        assert! (!is_destroyed (accessor, & other), "destroyed objects shouldn't be in the collision detection") ;
        if is_enemy (accessor, & object, & other) && (varying.object_type == ObjectType::Arrow) != (other_varying.object_type == ObjectType::Arrow) {
          if let Some(time) = varying.trajectory.when_collides (*accessor.now(), &other_varying.trajectory, radius (& varying) + radius (& other_varying)) {
            consider (accessor.create_prediction (time, id, Collide {objects: [object.clone(), other.clone()]}));
          }
        }
      }
    };
    
    varying.prediction = earliest_prediction;
  });
}
fn object_changed <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
  assert! (!is_destroyed (accessor, & object), "destroyed objects shouldn't be changed") ;
  update_prediction (accessor, object);
  Detector::changed_course (accessor, & get_detector (accessor), object);
}
fn choose_action <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
  let mut generator = DeterministicRandomId::new (& (accessor.extended_now().id, 0x7b017f025975dd1du64)).to_rng();
  modify_object (accessor, & object, | varying | {
    varying.action = match varying.object_type.clone() {
      ObjectType::Palace => (varying.dependents.len() < 6).as_some (Action {
        action_type: ActionType::BuildGuild,
        progress: LinearTrajectory1::new (*accessor.now(), 0, 10),
        cost: 100*SECOND + generator.gen_range (- SECOND*5, SECOND*5),
        ..Default::default()
      }),
      ObjectType::Guild => (varying.dependents.len() < 4).as_some (Action {
        action_type: ActionType::RecruitRanger,
        progress: LinearTrajectory1::new (*accessor.now(), 0, 10),
        cost: 100*SECOND + generator.gen_range (- SECOND*5, SECOND*5),
        ..Default::default()
      }),
      ObjectType::Ranger => {
        let position = varying.trajectory.evaluate (*accessor.now());
        let mut result = None;
        
        let nearby = Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (position), RANGER_RANGE as u64), Some (& object));
        let closest = nearby.into_iter().filter_map (| other | {
          assert! (!is_destroyed (accessor, & other), "destroyed objects shouldn't be in the collision detection") ;
          let other_varying = query_ref (accessor, & other.varying);
          if is_enemy (accessor, object, & other) && other_varying.object_type != ObjectType::Arrow {
            let other_position = other_varying.trajectory.evaluate (*accessor.now());
            let other_distance = distance (position, other_position).max();
            if other_distance <= RANGER_RANGE + radius (& other_varying) {
              return Some ((other.clone(), other_distance, other_position));
            }
          }
          None
        }).min_by_key (| t | t.1);
        
        if let Some(closest) = closest {
          result = Some (Action {
            action_type: ActionType::Shoot,
            target: Some(closest.0),
            progress: LinearTrajectory1::new (*accessor.now(), 0, 10),
            cost: 10*SECOND + generator.gen_range (- SECOND/2, SECOND/2),
            ..Default::default()
          });
          varying.trajectory.set_velocity (*accessor.now(), Vector::new (0, 0));
          //varying.target = None;
        }
                  
        if result.is_none() {result = Some(Action {
          action_type: ActionType::Think,
          progress: LinearTrajectory1::new (*accessor.now(), 0, 10),
          cost: 6*SECOND + generator.gen_range (- SECOND, SECOND),
          ..Default::default()}
        )}
        result
      },
      ObjectType::Arrow => Some(Action {
        action_type: ActionType::Disappear,
        progress: LinearTrajectory1::new (*accessor.now(), 0, 10),
        cost: 5*SECOND + generator.gen_range (- SECOND/2, SECOND/2),
        ..Default::default()}
      ),
    };
  });
}

fn radius (varying: & ObjectVarying)->Coordinate {
  match varying.object_type {
    ObjectType::Palace => PALACE_RADIUS,
    ObjectType::Guild => GUILD_RADIUS,
    ObjectType::Ranger => STRIDE/2,
    ObjectType::Arrow => STRIDE/5,
  }
}
fn is_building(varying: & ObjectVarying)->bool {
  match varying.object_type {
    ObjectType::Palace => true,
    ObjectType::Guild => true,
    _=>false,
  }
}


define_event! {
  pub struct Initialize {},
  PersistentTypeId(0x9a633852de46827f),
  fn execute (&self, accessor: &mut Accessor) {
    set (accessor, &accessor.globals().detector, SimpleGridDetector::new (accessor, Space, (STRIDE*50) as collisions::Coordinate));
    for team in 0..2 {
      create_object_impl (accessor, None, DeterministicRandomId::new (& (team, 0xb2e085cd02f2f8dbu64)),
        ObjectVarying {
          object_type: ObjectType::Palace,
          team: team,
          hitpoints: 100,
          trajectory: LinearTrajectory2::constant (*accessor.now(), Vector2::new (0, PALACE_DISTANCE*team as Coordinate*2 - PALACE_DISTANCE)),
          .. Default::default()
        },
      );
    }
  }
}

define_event! {
  pub struct CompleteAction {object: ObjectHandle},
  PersistentTypeId(0x3995cd28e2829c09),
  fn execute (&self, accessor: &mut Accessor) {
    let varying = query (accessor, &self.object.varying) ;
    match varying.action.as_ref().unwrap().action_type.clone() {
      ActionType::BuildGuild => {
        let new = create_object (accessor, & self.object, 0x379661e69cdd5fe7,
          ObjectVarying {
            object_type: ObjectType::Guild,
            team: varying.team,
            home: Some (self.object.clone()),
            hitpoints: 20,
            trajectory: LinearTrajectory2::constant (*accessor.now(), varying.trajectory.evaluate (*accessor.now()) + random_vector (&mut accessor.extended_now().id.to_rng(), PALACE_RADIUS + GUILD_RADIUS + 10*STRIDE)),
            .. Default::default()
          },
        );
        modify_object (accessor, & self.object, | varying | varying.dependents.push (new));
      },
      ActionType::RecruitRanger => {
        let new = create_object (accessor, & self.object, 0x91db5029ba8b0a4e,
          ObjectVarying {
            object_type: ObjectType::Ranger,
            team: varying.team,
            home: Some (self.object.clone()),
            hitpoints: 5,
            trajectory: LinearTrajectory2::constant (*accessor.now(),varying.trajectory.evaluate (*accessor.now()) + random_vector (&mut accessor.extended_now().id.to_rng(), GUILD_RADIUS + 2*STRIDE)),
            .. Default::default()
          },
        );
        modify_object (accessor, & self.object, | varying | varying.dependents.push (new));
      },
      ActionType::Think => {
        let position = varying.trajectory.evaluate (*accessor.now());
        let mut target_location = varying.target.as_ref().and_then (| target |
          (!is_destroyed (accessor, target)).as_some(
            query_ref (accessor, & target.varying).trajectory.evaluate (*accessor.now())
          )
        ).or_else (| | varying.target_location.clone());
        if let Some(t) = target_location {
          if distance (position, t) < 10*STRIDE {
            target_location = None;
          }
        }
        if target_location.is_none() {
          let nearby = Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (position), AWARENESS_RANGE as u64), Some (& self.object));
          let closest = nearby.into_iter().filter_map (| other | {
            let other_varying = query_ref (accessor, & other.varying);
            if is_enemy (accessor, & self.object, & other) && is_building (& other_varying) {
              let other_position = other_varying.trajectory.evaluate (*accessor.now());
              let other_distance = distance (position, other_position).max();
              if other_distance <= AWARENESS_RANGE {
                return Some ((other.clone(), other_distance, other_position));
              }
            }
            None
          }).min_by_key (| t | t.1);
          if let Some(closest) = closest {
            target_location = Some(closest.2);
            modify_object (accessor, & self.object, | varying | {
              varying.target = Some (closest.0);
              varying.target_location = None;
            });
          }
          else {
            let t = position + random_vector (&mut accessor.extended_now().id.to_rng(), AWARENESS_RANGE);
            target_location = Some(t);
            modify_object (accessor, & self.object, | varying | {
              varying.target_location = Some (t);
              varying.target = None;
            });
          }
        }
        let target_location = target_location.unwrap();
        let new_velocity = normalized_to (target_location - position, 10*STRIDE/SECOND);
        if new_velocity != varying.trajectory.velocity {
          modify_object (accessor, & self.object, | varying | {
            varying.trajectory.set_velocity (*accessor.now(), new_velocity);
          });
        }
      },
      ActionType::Shoot => {
        let target = varying.action.as_ref().unwrap().target.as_ref().unwrap();
        if !is_destroyed (accessor, target) {
        //destroy_object (accessor, varying.action.as_ref().unwrap().target.as_ref().unwrap());
        let position = varying.trajectory.evaluate (*accessor.now());
        let other_position = query_ref (accessor, & target.varying).trajectory.evaluate (*accessor.now());
        let new_velocity = normalized_to (other_position - position, 50*STRIDE/SECOND);
        create_object (accessor, & self.object, 0x27706762e4201474, ObjectVarying {
          object_type: ObjectType::Arrow,
          team: varying.team,
          trajectory: LinearTrajectory2::new (*accessor.now(), varying.trajectory.evaluate (*accessor.now()), new_velocity),
          .. Default::default()
        });
        }
      },
      ActionType::Disappear => {
        destroy_object (accessor, & self.object);
        return
      },
    }
    
    choose_action (accessor, &self.object);
  }
}


define_event! {
  pub struct Collide {objects: [ObjectHandle; 2]},
  PersistentTypeId(0xe35485dcd0277599),
  fn execute (&self, accessor: &mut Accessor) {
    let (arrow, victim) = if query_ref (accessor, & self.objects [0].varying).object_type == ObjectType::Arrow {(& self.objects [0], & self.objects [1])} else {(& self.objects [1], & self.objects [0])};
    destroy_object (accessor, arrow);
    let mut dead = false;
    modify_object (accessor, victim, | varying | {varying.hitpoints -= 1; dead = varying.hitpoints == 0;});
    if dead {destroy_object (accessor, victim);}
  }
}



struct Game {
  steward: Steward,
  
  now: Time,
  last_ui_time: f64,
  time_speed: f64,
  
  display_center: Vector,
  display_radius: Coordinate,
}


fn draw_game <A: Accessor <Steward = Steward>>(accessor: &A, game: & Game) {
  let canvas_width: f64 = js! {return canvas.width;}.try_into().unwrap();
  let scale = canvas_width/(game.display_radius as f64*2.0);
  js! {
    var size = Math.min (window.innerHeight, window.innerWidth, 600);
    canvas.setAttribute ("width", size);
    canvas.setAttribute ("height", size);
    context.clearRect (0, 0, canvas.width, canvas.height);
    context.save();
    context.scale (@{scale},@{scale});
    context.translate (@{-game.display_center [0] as f64}, @{-game.display_center [1] as f64});
    
    context.translate (@{game.display_radius as f64}, @{game.display_radius as f64});
  }
  for object in Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (Vector::new (0, 0)), PALACE_DISTANCE as u64*2), None) {
    let varying = query_ref (accessor, & object.varying);
    let center = varying.trajectory.evaluate (*accessor.now());
    let center = Vector2::new (center [0] as f64, center [1] as f64);
    let object_radius = radius (& varying) as f64;
    //println!("{:?}", (varying.trajectory, center, object_radius));
    js! {
      context.beginPath();
      context.arc (@{center [0]},@{center [1]},@{object_radius}, 0, Math.PI*2);
      context.strokeStyle = "rgba("+@{varying.team as i32*255}+",0,"+@{(1-varying.team as i32)*255}+",1.0)";
      context.lineWidth = @{object_radius/30.0 + 0.5/scale};
      context.stroke();
      if (@{varying.team == 1}) {
        context.fillStyle = "rgba(255,0,0,0.2)";
        context.fill();
      }
    }
    if let Some(action) = varying.action.as_ref() {js! {
      context.beginPath();
      context.arc (@{center [0]},@{center [1]},@{object_radius}, 0, @{action.progress.evaluate (*accessor.now()) as f64/action.cost as f64}*Math.PI*2);
      context.fillStyle = "rgba("+@{varying.team as i32*255}+",0,"+@{(1-varying.team as i32)*255}+",0.2)";
      context.fill();
    }}
    if let Some(home) = varying.home.as_ref() {
      let home_center = query_ref (accessor, & home.varying).trajectory.evaluate (*accessor.now());
      let home_center = Vector2::new (home_center [0] as f64, home_center [1] as f64);
      js! {
        context.beginPath();
        context.moveTo(@{center [0]},@{center [1]});
        context.lineTo(@{home_center [0]},@{home_center [1]});
        context.lineWidth = @{0.25/scale};
        context.setLineDash([@{3.0/scale},@{3.0/scale}]);
        context.stroke();
        context.setLineDash([]);
      }
    }
  }
  js! {
    context.restore();
  }
}

fn main_loop (time: f64, game: Rc<RefCell<Game>>) {
  let continue_simulating;
  {
    let mut game = game.borrow_mut();
    let observed_duration = time - game.last_ui_time;
    let duration_to_simulate = if observed_duration < 100.0 {observed_duration} else {100.0};
    let duration_to_simulate = (duration_to_simulate*(SECOND as f64)*game.time_speed/1000.0) as Time;
    assert!(duration_to_simulate >= 0) ;
    game.last_ui_time = time;
    game.now += duration_to_simulate;
    let now = game.now.clone();
    let snapshot = game.steward.snapshot_before (&now). unwrap ();
    draw_game (& snapshot, & game);
    game.steward.forget_before (&now);
  
    let teams_alive: std::collections::HashSet <_> = Detector::objects_near_box (& snapshot, & get_detector (& snapshot), BoundingBox::centered (to_collision_vector (Vector::new (0, 0)), PALACE_DISTANCE as u64*2), None).into_iter().map (| object | query_ref (& snapshot, & object.varying).team).collect();
    continue_simulating = teams_alive.len() > 1;
  }
  if continue_simulating {
    web::window().request_animation_frame (move | time | main_loop (time, game));
  }
}

#[cfg (target_os = "emscripten")]
fn main() {
  stdweb::initialize();
  js! {
    var game_container = window.game_container = $("<div>");
    var canvas = window.canvas = document.createElement ("canvas");
    (document.querySelector("main") || document.body).appendChild (game_container[0]);
    game_container.append(canvas);
    window.context = canvas.getContext ("2d");
  }
  
  let seed: u32 = js!{return window.localStorage && parseInt(window.localStorage.getItem ("random_seed")) || 0}.try_into().unwrap();
  
  let mut steward: Steward = Steward::from_globals (Globals {detector: new_timeline()});
  steward.insert_fiat_event (0, DeterministicRandomId::new (& (seed, 0xae06fcf3129d0685u64)), Initialize {}).unwrap();
  let game = Rc::new (RefCell::new (Game {
    steward: steward,
    now: 1,
    last_ui_time: 0.0,
    time_speed: 1.0,
    display_center: Vector::new (0, 0),
    display_radius: PALACE_DISTANCE*3/2,
  }));
  
  {
    let game = game.clone();
    let wheel_callback = move |x: f64,y: f64, delta: f64 | {
      let mut game = game.borrow_mut();
      let offset = Vector2::new (
        x*game.display_radius as f64*2.0,
        y*game.display_radius as f64*2.0
      );
      let factor = (1.003f64).powf(delta);
      game.display_radius = (game.display_radius as f64*factor) as Coordinate;
      let modified_offset = offset*factor;
      let difference = offset - modified_offset;
      game.display_center += Vector2::new (difference [0] as Coordinate, difference [1] as Coordinate);
      //println!("{:?}", (x,y,game.display_center));
    };
    js! {
      var callback = @{wheel_callback};
      canvas.addEventListener ("wheel", function (event) {
        var offset = canvas.getBoundingClientRect();
        callback (
          (event.clientX - offset.left)/offset.width - 0.5,
          (event.clientY - offset.top)/offset.height - 0.5,
          event.deltaY
        );
        event.preventDefault();
      });
    }
  }
  {
    let game = game.clone();
    let time_callback = move |speed: f64| {
      let mut game = game.borrow_mut();
      game.time_speed = if speed == -10.0 { 0.0 } else { (2.0f64).powf(speed/2.0) };
      println!("{:?}", (speed));
    };
    js! {
      var callback = @{time_callback};
      game_container.append($("<div>").append(
        $("<input>", {
          type: "range",
          id: "time_speed",
          value: 0, min: -10, max: 10, step: 1
        }).on ("input", function (event) {
          callback(event.target.valueAsNumber);
        }),
        $("<label>", {
          for: "time_speed",
          text: " time speed",
        })
      ));
    }
  }

  js! {
    game_container.append($("<div>").append(
      $("<input>", {
        type: "number",
        id: "seed",
        value:@{seed},
        min: 0,
        max: @{0u32.wrapping_sub(1)}
      }).on ("input", function (event) {
        var value = Math.floor(event.target.valueAsNumber);
        if (value >= 0 && value <= @{0u32.wrapping_sub(1)}) {
          window.localStorage.setItem ("random_seed", value);
        }
      }),
      $("<label>", {
        for: "seed",
        text: " random seed (reload page to apply)",
      })
    ));
  }
  
  web::window().request_animation_frame (move | time | main_loop (time, game));

  stdweb::event_loop();
}


#[cfg (not(target_os = "emscripten"))]
fn main() {
  let mut scores = [0; 2];
  
  loop {
  
  let mut steward: Steward = Steward::from_globals (Globals {detector: new_timeline()});
  steward.insert_fiat_event (0, DeterministicRandomId::new (& (scores, 0xae06fcf3129d0685u64)), Initialize {}).unwrap();
  let mut game = Game {
    steward: steward,
    now: 1,
    last_ui_time: 0.0,
    time_speed: 1.0,
    display_center: Vector::new (0, 0),
    display_radius: PALACE_DISTANCE*3/2,
  };
  
  loop {
    game.now += SECOND /100;
    let snapshot = game.steward.snapshot_before (& game.now). unwrap ();
    game.steward.forget_before (& game.now);
  
    let teams_alive: std::collections::HashSet <_> = Detector::objects_near_box (& snapshot, & get_detector (& snapshot), BoundingBox::centered (to_collision_vector (Vector::new (0, 0)), PALACE_DISTANCE as u64*2), None).into_iter().filter (| object | is_building (&query_ref (& snapshot, & object.varying))).map (| object | query_ref (& snapshot, & object.varying).team).collect();
    if teams_alive.len() <= 1 {
      scores [teams_alive.into_iter().next().unwrap()] += 1;
      println!("{:?}", scores);
      break;
    }
  }
  }
}


// ##########################################
// ######  annoying abstraction stuff #######
// ##########################################
// 
// everything below this line is either general-use support features or annoying boilerplate.
// A lot of it should be defined, in one form or another, in TimeSteward support libraries,
// rather than this game specifically.
//
// define_event! and unwrap_object_type! should also be down here,
// they just can't because macros have ordering.
// 
// Maybe most of this could be put in a separate file if I don't move it into TimeSteward.

fn get_detector <A: Accessor <Steward = Steward>>(accessor: &A)->DataHandle <Detector> {
  query (accessor, & accessor.globals().detector)
}

fn modify<A: EventAccessor <Steward = Steward>, T: QueryResult, F: FnOnce(&mut T)>(accessor: &A, cell: &DataTimelineCell <SimpleTimeline <T, Steward>>, f: F) {
  let mut data = query (accessor, cell);
  (f)(&mut data);
  set (accessor, cell, data);
}


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
  fn set (&mut self, time: Time, added: V) {
    self.update (time) ;
    self.position = added;
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
  fn when_collides (&self, now: Time, other: & Self, distance: Coordinate)->Option <Time> {
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
