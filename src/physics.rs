use super::*;

use nalgebra::{Vector2};
use rand::Rng;
use boolinator::Boolinator;
use std::cmp::max;


use time_steward::{DeterministicRandomId};
use time_steward::{PersistentTypeId, ListedType, PersistentlyIdentifiedType, DataTimelineCellTrait, QueryResult, EventHandleTrait, Basics as BasicsTrait};
pub use time_steward::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, Event, DataHandle, DataTimelineCell, Accessor, EventAccessor, FutureCleanupAccessor, simple_timeline, bbox_collision_detection_2d as collisions};
use self::simple_timeline::{SimpleTimeline, query, query_ref, set};
use self::collisions::{NumDimensions, Detector as DetectorTrait};
use self::collisions::simple_grid::{SimpleGridDetector};


pub type Steward = steward_module::Steward <Basics>;
pub type EventHandle = <Steward as TimeSteward>::EventHandle;
pub type Timeline <T> = DataTimelineCell <SimpleTimeline <T, Steward>>;
pub fn new_timeline <T: QueryResult> ()->Timeline <T> {DataTimelineCell::new (SimpleTimeline::new())}
pub type Detector = SimpleGridDetector <Space>;
pub type BoundingBox = collisions::BoundingBox<Space>;
pub type DetectorData = collisions::simple_grid::DetectorDataPerObject<Space>;

pub type Time = i64;
pub type Progress = Time;
pub type Amount = Coordinate;
pub type Coordinate = i64;
pub type Vector = Vector2 <Coordinate>;

pub const SECOND: Time = 1 << 20;

pub const STRIDE: Coordinate = SECOND << 8;
pub const TRIVIAL_DISTANCE: Coordinate = STRIDE>>8;
pub const PALACE_RADIUS: Coordinate = STRIDE*20;
pub const GUILD_RADIUS: Coordinate = STRIDE*15;
pub const PALACE_DISTANCE: Coordinate = PALACE_RADIUS*10;
pub const INITIAL_PALACE_DISTANCE: Coordinate = PALACE_DISTANCE*3;
pub const RANGER_RANGE: Coordinate = STRIDE*20;

pub const STANDARD_ACTION_SPEED: Progress = 60;
pub const STANDARD_ACTION_SECOND: Progress = STANDARD_ACTION_SPEED*SECOND;

pub const RANGER_COST: Amount = 50;
pub const GUILD_COST: Amount = 150;
pub const PALACE_COST: Amount = 1000;
pub const BEAST_REWARD: Amount = 100;

pub const COMBAT_PRIORITY: Amount = 1<<40;


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


// ##########################################
// ######       data definitions      #######
// ##########################################

#[derive (Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Default)]
pub struct Basics {}
impl BasicsTrait for Basics {
  type Time = Time;
  type Globals = Globals;
  type Types = (ListedType <AchieveAction>);
  const MAX_ITERATION: u32 = 12;
}

#[derive (Serialize, Deserialize, Debug)]
pub struct Globals {
  pub detector: Timeline <DataHandle <Detector>>,
  pub orders: [Timeline <Orders>; 2],
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub enum ObjectType {
  Palace,
  Guild,
  Lair,
  Beast,
  Ranger,
  Arrow,
  Peasant,
}

trait ActionTrait {
  #[allow (unused_variables)]
  fn is_legal <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->bool {true}
  fn priority <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Amount;
  fn default_achieve_cost (&self)->Option <Progress> {None}
  fn default_finish_cost (&self)->Progress {self.default_achieve_cost ().expect ("we shouldn't be asking for the finish cost on an open-ended action")}
  fn cost_variability_percent (&self)->i64 {0}
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {panic!("reached undefined achieve() implementation, probably either trying to achieve an open-ended action or someone forgot to implement achieve() for something")}
  fn target_location <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Option<Vector> {None}
}

macro_rules! define_action_types_inner_call {
  ($method:ident, $self_hack:ident, [$arg: ident, $arg2: ident], $($T: ident,)*) => {
    match $self_hack.action_type {
      $($T(ref data) => data.$method ($arg, $arg2),)*
    }
  };
  ($method:ident, $self_hack:ident, [], $($T: ident,)*) => {
    match $self_hack.action_type {
      $($T(ref data) => data.$method (),)*
    }
  };
}

macro_rules! define_action_types {
  ($($T: ident,)*) => {

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub enum Action {
  $($T($T),)*
}

impl ActionTrait for Action {
  fn is_legal <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->bool {
    define_action_types_inner_call! (is_legal, self, [accessor, object], $($T,)*)
  }
  fn priority <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Amount {
    define_action_types_inner_call! (priority, self, [accessor, object], $($T,)*)
  }
  fn default_achieve_cost (&self)->Option <Progress>{
    define_action_types_inner_call! (default_achieve_cost, self, [], $($T,)*)
  }
  fn default_finish_cost (&self)->Progress {
    define_action_types_inner_call! (default_finish_cost, self, [], $($T,)*)
  }
  fn cost_variability_percent (&self)->i64 {
    define_action_types_inner_call! (default_cost_variability_percent, self, [], $($T,)*)
  }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    define_action_types_inner_call! (achieve, self, [accessor, object], $($T,)*)
  }
  fn target_location <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Option<Vector> {
    define_action_types_inner_call! (target_location, self, [accessor, object], $($T,)*)
  }
}
  
  }
}

define_action_types! {
  BuildGuild,
  RecruitRanger,
  SpawnBeast,
  Think,
  Shoot,
  Rest,
  Disappear,
  Pursue,
  Collect,
  Wait,
}
pub struct Shoot {pub target: ObjectHandle,}
pub struct Pursue {pub target: ObjectHandle, pub intention: Box <Action>}
pub struct BuildGuild;
pub struct RecruitRanger;
pub struct SpawnBeast;
pub struct Think;
pub struct Rest;
pub struct Wait;
pub struct Disappear {pub time: Time,}


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct SynchronousAction {
  pub action_type: ActionType,
  pub progress: LinearTrajectory1,
  pub achieve_cost: Progress,
  pub finish_cost: Progress,
  pub achieved: bool,
}



#[derive (Clone, Serialize, Deserialize, Debug)]
pub struct Object {
  pub id: DeterministicRandomId,
  pub varying: Timeline <ObjectVarying>,
}
impl PersistentlyIdentifiedType for Object {
  const ID: PersistentTypeId = PersistentTypeId(0x2079a1bb8d4e9d9f);
}
pub type ObjectHandle = DataHandle <Object>;

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug, Derivative)]
#[derivative (Default)]
pub struct ObjectVarying {
  #[derivative (Default (value = "ObjectType::Palace"))]
  pub object_type: ObjectType,
  
  #[derivative (Default (value = "LinearTrajectory2::constant (0, Vector::new (0, 0))"))]
  pub trajectory: LinearTrajectory2,
  pub radius: Coordinate,
  
  pub attack_range: Coordinate,
  pub interrupt_range: Coordinate,
  pub awareness_range: Coordinate,
  pub speed: Coordinate,
  
  pub team: usize,
  
  pub hitpoints: Amount,
  pub max_hitpoints: Amount,
  #[derivative (Default (value = "LinearTrajectory1::new (0, 0, 0)"))]
  pub endurance: LinearTrajectory1,
  pub max_endurance: Amount,
  
  pub food: Amount,
  
  pub is_building: bool,
  pub is_unit: bool,
  
  pub synchronous_action: Option <SynchronousAction>,
  pub ongoing_action: Option <Action>,
  
  //pub target: Option <ObjectHandle>,
  //pub target_location: Option <Vector>,
  
  pub home: Option <ObjectHandle>,
  pub dependents: Vec <ObjectHandle>,
  
  #[derivative (Default (value = "false"))]
  pub destroyed: bool,
  
  pub prediction: Option <EventHandle>,
  pub detector_data: Option <DetectorData>,
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Orders {
  pub unit_destination: Option <Vector>,
}



// ####################################################
// ###### object creation/modification protocol #######
// ####################################################

fn create_object_impl <A: EventAccessor <Steward = Steward>>(accessor: &A, source_object: Option <& ObjectHandle>, id: DeterministicRandomId, varying: ObjectVarying)->ObjectHandle {
  let created = accessor.new_handle (Object {id: id, varying: new_timeline()});
  set (accessor, & created.varying, varying);
  Detector::insert (accessor, & get_detector (accessor), & created, source_object);
  reconsider_action (accessor, & created) ;
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
    varying.synchronous_action = None;
    varying.ongoing_action = None;
    varying.destroyed = true;
    varying.dependents.clear();
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
    if reconsider {reconsider_action (accessor, & home) ;}
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

fn modify_object <A: EventAccessor <Steward = Steward>, F: FnOnce(&mut ObjectVarying)>(accessor: &A, object: & ObjectHandle, f: F) {
  modify (accessor, & object.varying, |varying| {
    (f)(varying)
  });
  object_changed (accessor, object);
}

fn object_changed <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
  assert! (!is_destroyed (accessor, & object), "destroyed objects shouldn't be changed") ;
  update_prediction (accessor, object);
  Detector::changed_course (accessor, & get_detector (accessor), object);
}





// ##################################################
// ###### inferred object attribute functions #######
// ##################################################

pub fn is_destroyed <A: Accessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle)->bool {
  query_ref (accessor, & object.varying).destroyed
}

pub fn is_enemy <A: Accessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle, other: & ObjectHandle)->bool {
  query_ref (accessor, & object.varying).team != query_ref (accessor, & other.varying).team
}

pub fn target_location <A: Accessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle)->Option <Vector> {
  let varying = query_ref (accessor, & object.varying);
  varying.target.as_ref().map (| target | query_ref (accessor, & target.varying).trajectory.evaluate (*accessor.now())).or(varying.target_location)
}

pub fn radius (varying: & ObjectVarying)->Coordinate {
  varying.radius
}
pub fn is_building(varying: & ObjectVarying)->bool {
  varying.is_building
}




// ##########################################
// ######          behavior           #######
// ##########################################


fn make_synchronous_action <A: EventAccessor <Steward = Steward>>(accessor: &A, action: Action)->SynchronousAction {
  let mut generator = DeterministicRandomId::new (& (accessor.extended_now().id, 0x7b017f025975dd1du64)).to_rng();
  let variability_percent = action.cost_variability_percent();
  let modifier = generator.gen_range (100 - variability_percent, 100 + variability_percent + 1);
  details.achieve_cost = details.achieve_cost * modifier / 100;
  details.finish_cost = max(details.achieve_cost, details.finish_cost * modifier / 100);
  details.progress = LinearTrajectory1::new (*accessor.now(), 0, STANDARD_ACTION_SPEED);
  details
}

fn make_palace (mut details: ObjectVarying)->ObjectVarying {
  details.object_type = ObjectType::Palace;
  details.hitpoints = 100;
  details.radius = PALACE_RADIUS;
  details.is_building = true;
  details.food = GUILD_COST;
  details
}


fn update_prediction <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
  let id = DeterministicRandomId::new (& (0x93562b6a9bcdca8cu64, accessor.extended_now().id, object.id));
  modify (accessor, & object.varying, | varying | {
    let mut earliest_prediction = None;
    {
      let mut consider = | prediction: EventHandle | {
        if earliest_prediction.as_ref().map_or (true, | earliest: & EventHandle | prediction.extended_time() < earliest.extended_time()) {earliest_prediction = Some (prediction);}
      };
      if let Some (action) = varying.synchronous_action.as_ref() {
        if action.achieved {
          consider (accessor.create_prediction (action.progress.when_reaches (*accessor.now(), action.finish_cost).unwrap(), id, FinishAction {object: object.clone()}));
        }
        else {
          consider (accessor.create_prediction (action.progress.when_reaches (*accessor.now(), action.achieve_cost).unwrap(), id, AchieveAction {object: object.clone()}));
        }
      }
      if varying.is_unit {
        if let Some(target_location) = target_location (accessor, object) {
          if let Some(time) = varying.trajectory.when_collides (*accessor.now(), &LinearTrajectory2::constant(*accessor.now(), target_location), TRIVIAL_DISTANCE) {
            consider (accessor.create_prediction (time, id, ReachTarget {object: object.clone()}));
          }
        }
      }
      let mut collide_target = varying.object_type == ObjectType::Arrow;
      /*if varying.object_type == ObjectType::Ranger { if let Some(target) = varying.target.as_ref() { if !is_destroyed (accessor, target) && query_ref (accessor, & target.varying).hitpoints <= 0 {
        collide_target = true;
      }}}*/
      if collide_target {
        for other in Detector::objects_near_object (accessor, & get_detector (accessor), object) {
          if varying.target.as_ref() == Some(&other) {
            let other_varying = query_ref (accessor, & other.varying);
            assert! (!is_destroyed (accessor, & other), "destroyed objects shouldn't be in the collision detection") ;
            if let Some(time) = varying.trajectory.when_collides (*accessor.now(), &other_varying.trajectory, radius (& varying) + radius (& other_varying)) {
              consider (accessor.create_prediction (time, id, Collide {objects: [object.clone(), other.clone()]}));
            }
          }
        }
      }
    };
    
    varying.prediction = earliest_prediction;
  });
}

fn interaction_choices <A: Accessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle, other: & ObjectHandle, distance: Coordinate)->Vec<Action > {
  let varying = query_ref (accessor, &object.varying) ;
  let other_varying = query_ref (accessor, & other.varying);
  let mut result = Vec::new() ;
  if varying.is_unit && other_varying.is_unit && other_varying.hitpoints >0 && distance < varying.attack_range {
    result.push (Action::Shoot (Shoot {target: other.clone()}));
  }
  let move_and_act = | action | {
    if distance <0 {action} else {Action::Pursue(Pursue {target: other.clone(), intention: action})}
  };
  if varying.object_type == ObjectType::Ranger && other_varying.object_type == ObjectType::Beast && other_varying.hitpoints == 0 {
    result.push (move_and_act(Action::Collect (Collect {target: other.clone()})));
  }
  result
}

fn choose_action <A: Accessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle)->Action {
  let varying = query_ref (accessor, &object.varying) ;
  let mut choices = Vec::new();
  let consider = | choices: &mut Vec<(Action, Amount)>, action | {
    if action.is_legal (accessor, object) {
      choices.push ((action, action.priority (accessor, object)));
    }
  };
  
  // first pass: cheapest calculations.
  if let Some(current) = varying.ongoing_action.as_ref().or (varying.synchronous_action.as_ref()) {
    consider (&mut choices, current.clone());
  }
  match varying.object_type.clone() {
    ObjectType::Palace => consider (&mut choices, Action::BuildGuild(BuildGuild)),
    ObjectType::Guild => consider (&mut choices, Action::RecruitRanger(RecruitRanger)),
    ObjectType::Lair => consider (&mut choices, Action::SpawnBeast(SpawnBeast)),
    ObjectType::Arrow => consider (&mut choices, Action::Disappear(Disappear {time: SECOND*1/2})),
    _=>(),
  }
  
  choices.sort_by_key (| choice | choice.1);
  if let Some(best) = choices.last() {
    if best.1>=COMBAT_PRIORITY {
      return best.0
    }
  }
  
  // second pass: scan surroundings for stuff.
  // Commonly used for characters on long journeys to notice nearby interesting stuff.
  // Optimization: currently, only units can interact with nearby stuff
  if varying.is_unit {
    let position = varying.trajectory.evaluate (*accessor.now());
        
    let nearby = Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (position), varying.interrupt_range as u64), Some (& object));
    for other in nearby {
      assert! (!is_destroyed (accessor, & other), "destroyed objects shouldn't be in the collision detection") ;
      let other_varying = query_ref (accessor, & other.varying);
      let other_position = other_varying.trajectory.evaluate (*accessor.now());
      let other_distance = distance (position, other_position).max() - radius (& other_varying);
      if other_distance <= varying.interrupt_range {
        for choice in interaction_choices (accessor, object, other) {
          consider (&mut choices, choice, other_distance);
        }
      }
    }
  }
    
  choices.sort_by_key (| choice | choice.1);
  if let Some(best) = choices.last() {
    if best.1> 0{
      return best.0
    }
  }
  
  // third pass: there's nothing to do nearby, so it's finally worth it to pay the larger cost of searching for a distant task
  
  if varying.is_unit {
    let position = varying.trajectory.evaluate (*accessor.now());
        
    let nearby = Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (position), varying.awareness_range as u64), Some (& object));
    for other in nearby {
      assert! (!is_destroyed (accessor, & other), "destroyed objects shouldn't be in the collision detection") ;
      let other_varying = query_ref (accessor, & other.varying);
      let other_position = other_varying.trajectory.evaluate (*accessor.now());
      let other_distance = distance (position, other_position).max() - radius (& other_varying);
      if other_distance > varying.interrupt_range && other_distance > varying.awareness_range {
        for choice in interaction_choices (accessor, object, other) {
          consider (&mut choices, choice, other_distance);
        }
      }
    }
  }
  
  choices.sort_by_key (| choice | choice.1);
  if let Some(best) = choices.last() {
    if best.1> 0{
      return best.0
    }
  }
  
  Action::Wait(Wait)

  
}

fn action_velocity (accessor: &A, object: &ObjectHandle, action: Option<&Action>) {
  let varying = query (accessor, &object.varying) ;
  
  if let Some(action) = action {
    if action.action_type == ActionType::Think {
      TODO
    }
  }
  
  Vector::new(0,0)
}

fn set_action <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle, action: Option<Action>) {
  modify_object (accessor, & object, | varying | {
    varying.action = action;
    varying.trajectory.set_velocity (*accessor.now(), action_velocity (accessor, object, varying.action.as_ref()));
  });
}


impl ActionTrait for BuildGuild {
  fn is_legal <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->bool {
    //hack: this action is still actually "attempt to build a guild or maybe a palace"
    true
  }
  fn priority <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Amount {
    1000
  }
  fn default_achieve_cost (&self)->Option <Progress>{ Some(2*STANDARD_ACTION_SECOND) }
  fn cost_variability_percent (&self)->i64 { 5 }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    let varying = query (accessor, &object.varying) ;
        let position = varying.trajectory.evaluate (*accessor.now());
        let mut generator = accessor.extended_now().id.to_rng();
        for attempt in 0..9 {
          let guild = attempt < 5;
          let cost = if guild {GUILD_COST} else {PALACE_COST};
          if varying.food < cost {break;}
          let minimum_distance = if guild { varying.radius + GUILD_RADIUS + 10*STRIDE } else {PALACE_DISTANCE};
          
          let target_position = position + random_vector_exact_length (&mut generator, if attempt <5 {minimum_distance*(/*attempt/2 +*/ 1)} else {minimum_distance});
          
          if distance (target_position, Vector::new (0, 0)).max() >INITIAL_PALACE_DISTANCE*10/9 {continue;}
          
          let nearby = Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (target_position), minimum_distance as u64), Some (object));
          
          if nearby.into_iter().all (| other | {
            let other_varying = query_ref (accessor, & other.varying);
            if is_building (& other_varying) && (guild || other_varying.object_type == ObjectType::Palace) {
              let other_position = other_varying.trajectory.evaluate (*accessor.now());
              let other_distance = distance (target_position, other_position).max();
              return other_distance + TRIVIAL_DISTANCE >minimum_distance;
            }
            true
          }) {
            let defaults = ObjectVarying {
              object_type: ObjectType::Guild,
              radius: GUILD_RADIUS,
              hitpoints: 20,
              is_building: true,
              food: RANGER_COST,
              
              team: varying.team,
              home: Some (object.clone()),
              trajectory: LinearTrajectory2::constant (*accessor.now(), target_position),
              .. Default::default()
            };
            let new = create_object (accessor, object, 0x379661e69cdd5fe7,
              if guild {defaults} else {make_palace (defaults)}
            );
            modify_object (accessor, object, | varying | {
              varying.food -= cost;
              varying.dependents.push (new);
            });
            break
          }
        }

  }
}

impl ActionTrait for RecruitRanger {
  fn is_legal <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->bool {
    let varying = query_ref (accessor, &object.varying) ;
    varying.food >= RANGER_COST && varying.dependents.len() < 4
  }
  fn priority <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Amount {
    1000
  }
  fn default_achieve_cost (&self)->Option <Progress>{ Some(10*STANDARD_ACTION_SECOND) }
  fn cost_variability_percent (&self)->i64 { 5 }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    let varying = query (accessor, &object.varying) ;
    
        let new = create_object (accessor, object, 0x91db5029ba8b0a4e,
          ObjectVarying {
            object_type: ObjectType::Ranger,
            team: varying.team,
            home: Some (object.clone()),
            hitpoints: 5,
            radius: STRIDE/2,
            attack_range: RANGER_RANGE,
            awareness_range: 200*STRIDE,
            is_unit: true,
            trajectory: LinearTrajectory2::constant (*accessor.now(),varying.trajectory.evaluate (*accessor.now()) + random_vector_exact_length (&mut accessor.extended_now().id.to_rng(), varying.radius + 2*STRIDE)),
            endurance: LinearTrajectory1::new (*accessor.now(), 600*SECOND, - 10),
            .. Default::default()
          },
        );
        modify_object (accessor, object, | varying | {
          varying.food -= RANGER_COST;
          varying.dependents.push (new);
        });

  }
}


impl ActionTrait for SpawnBeast {
  fn priority <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Amount {
    1000
  }
  fn default_achieve_cost (&self)->Option <Progress>{ Some(25*STANDARD_ACTION_SECOND) }
  fn cost_variability_percent (&self)->i64 { 35 }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    let varying = query (accessor, &object.varying) ;
    let new = create_object (accessor, object, 0x91db5029ba8b0a4e,
          ObjectVarying {
            object_type: ObjectType::Beast,
            team: varying.team,
            home: Some (object.clone()),
            hitpoints: 5,
            radius: STRIDE*2/3,
            attack_range: STRIDE*2,
            awareness_range: STRIDE*30,
            is_unit: true,
            trajectory: LinearTrajectory2::constant (*accessor.now(),varying.trajectory.evaluate (*accessor.now()) + random_vector_exact_length (&mut accessor.extended_now().id.to_rng(), varying.radius + 2*STRIDE)),
            endurance: LinearTrajectory1::new (*accessor.now(), 600*SECOND, - 10),
            .. Default::default()
          },
        );
        modify_object (accessor, object, | varying | varying.dependents.push (new));
  }
}
impl ActionTrait for Think {
  fn priority <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Amount {
    panic!("Think is a special action that's not supposed to be considered as an alternative to other actions")
  }
  fn default_achieve_cost (&self)->Option <Progress>{ Some(STANDARD_ACTION_SECOND*6/10) }
  fn cost_variability_percent (&self)->i64 { 20 }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    //let varying = query (accessor, &object.varying);
    
  }
  fn target_location <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Option<Vector> {
    let varying = query_ref (accessor, &object.varying) ;
    varying.ongoing_action.expect ("there should be an ongoing action if you are thinking").target_location(accessor, object)
  }
}
impl ActionTrait for Shoot {
  fn priority <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Amount {
    COMBAT_PRIORITY + 10
  }
  fn default_achieve_cost (&self)->Option <Progress>{ Some(STANDARD_ACTION_SECOND*6/10) }
  fn default_finish_cost (&self)->Progress{ STANDARD_ACTION_SECOND*10/10 }
  fn cost_variability_percent (&self)->i64 { 5 }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    let varying = query (accessor, &object.varying) ;
    
        let target = & self.target;
        if !is_destroyed (accessor, target) {
        //destroy_object (accessor, varying.action.as_ref().unwrap().target.as_ref().unwrap());
        let position = varying.trajectory.evaluate (*accessor.now());
        let other_position = query_ref (accessor, & target.varying).trajectory.evaluate (*accessor.now());
        let new_velocity = normalized_to (other_position - position, 50*STRIDE/SECOND);
        create_object (accessor, object, 0x27706762e4201474, ObjectVarying {
          object_type: ObjectType::Arrow,
          team: varying.team,
          target: action.target,
          radius: STRIDE/5,
          trajectory: LinearTrajectory2::new (*accessor.now(), varying.trajectory.evaluate (*accessor.now()), new_velocity),
          .. Default::default()
        });
        }

  }
}
impl ActionTrait for Disappear {
  fn priority <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Amount {
    COMBAT_PRIORITY + 1000
  }
  fn default_achieve_cost (&self)->Option <Progress>{ Some(self.time*STANDARD_ACTION_SPEED) }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    destroy_object (accessor, object);
  }
}

impl ActionTrait for Pursue {
  fn priority <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Amount {
    let varying = query_ref (accessor, &object.varying) ;
    let location = varying.trajectory.evaluate (*accessor.now());
    let target = self.target_location(accessor, object).unwrap();
    let time_to_reach = distance (location, target).max()/varying.speed;
    let time_to_perform = self.intention.default_finish_cost()/STANDARD_ACTION_SPEED;
    self.intention.priority(accessor, object)*time_to_perform/(time_to_perform + time_to_reach)
  }
  fn target_location <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Option<Vector> {
    let varying = query_ref (accessor, &object.varying) ;
    Some(query_ref (accessor, & self.target.varying).trajectory.evaluate (*accessor.now()))
  }
}

impl ActionTrait for Wait{
  fn priority <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Amount {
    0
  }
}

impl ActionTrait for Collect {
  fn priority <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Amount {
    2000
  }
  fn default_achieve_cost (&self)->Option <Progress>{ Some(STANDARD_ACTION_SECOND*10/10) }
  fn cost_variability_percent (&self)->i64 { 10 }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    destroy_object (accessor, & self.target);
    modify_object (accessor, object, | varying | {
      varying.food += BEAST_REWARD;
    });
  }
}

impl ActionTrait for Rest {
  fn priority <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Amount {
    let varying = query_ref (accessor, &object.varying) ;
    200*SECOND - varying.endurance.evaluate (*accessor.now())
  }
  fn default_achieve_cost (&self)->Option <Progress>{ Some(STANDARD_ACTION_SECOND*20) }
  fn cost_variability_percent (&self)->i64 { 5 }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    let varying = query (accessor, &object.varying) ;
    let home = varying.home.as_ref().unwrap();
        if !is_destroyed (accessor, home) {
          modify_object (accessor, object, | varying | varying.food = 0);
          modify_object (accessor, home, | other_varying | other_varying.food += varying.food);
        }
        modify_object (accessor, object, | varying | {
          varying.endurance.set (*accessor.now(), 600*SECOND);
        });
  }
}


/*

      ActionType::RecruitRanger => {
        if varying.food >= RANGER_COST && varying.dependents.len() < 4 {        }
        else if varying.food > RANGER_COST {
          let home = varying.home.as_ref().unwrap();
          if !is_destroyed (accessor, home) {
            modify_object (accessor, object, | varying | varying.food = RANGER_COST);
            modify_object (accessor, home, | other_varying | other_varying.food += varying.food - RANGER_COST);
          }
        }
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
          let nearby = Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (position), varying.awareness_range as u64), Some (object));
          let closest = (varying.endurance.evaluate (*accessor.now()) < 10*SECOND).as_option().and_then (|_| varying.home.as_ref().map(|home| (home.clone(), query(accessor, &home.varying).trajectory.evaluate (*accessor.now())))).or_else(|| nearby.into_iter().filter_map (| other | {
            let other_varying = query_ref (accessor, & other.varying);
            if is_enemy (accessor, object, & other) && 
              ((is_building (& other_varying) && other_varying.object_type != ObjectType::Lair) ||
              other_varying.object_type == ObjectType::Beast) {
              let other_position = other_varying.trajectory.evaluate (*accessor.now());
              let other_distance = distance (position, other_position).max();
              if other_distance <= varying.awareness_range {
                return Some ((other.clone(), other_distance, other_position));
              }
            }
            None
          }).min_by_key (| t | t.1).map (| triple | (triple.0, triple.2)));
          if let Some(closest) = closest {
            target_location = Some(closest.1);
            modify_object (accessor, object, | varying | {
              varying.target = Some (closest.0);
              varying.target_location = None;
            });
          }
          else {
            let t = position + random_vector_exact_length (&mut accessor.extended_now().id.to_rng(), varying.awareness_range);
            target_location = accessor.globals().orders.get(varying.team).and_then(|orders| query (accessor, orders).unit_destination).or (Some(t));
            modify_object (accessor, & self.object, | varying | {
              varying.target_location = target_location;
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
    }
*/



define_event! {
  pub struct Initialize {},
  PersistentTypeId(0x9a633852de46827f),
  fn execute (&self, accessor: &mut Accessor) {
    let mut generator = accessor.extended_now().id.to_rng();
    set (accessor, &accessor.globals().detector, SimpleGridDetector::new (accessor, Space, (STRIDE*50) as collisions::Coordinate));
    for team in 0..2 {
      set (accessor, &accessor.globals().orders[team], Orders {unit_destination: None });
      create_object_impl (accessor, None, DeterministicRandomId::new (& (team, 0xb2e085cd02f2f8dbu64)),
        make_palace (ObjectVarying {
          team: team,
          trajectory: LinearTrajectory2::constant (*accessor.now(), Vector2::new (0, INITIAL_PALACE_DISTANCE*team as Coordinate*2 - INITIAL_PALACE_DISTANCE)),
          .. Default::default()
        }),
      );
    }
    for index in 0..28 {
      create_object_impl (accessor, None, DeterministicRandomId::new (& (index, 0xb2e085cd02f2f8dbu64)),
        ObjectVarying {
          object_type: ObjectType::Lair,
          team: 6,
          radius: STRIDE*8,
          is_building: true,
          hitpoints: 20,
          trajectory: LinearTrajectory2::constant (*accessor.now(), Vector::new (0, 0) + random_vector_within_length (&mut generator, INITIAL_PALACE_DISTANCE)),
          .. Default::default()
        },
      );
    }
  }
}

define_event! {
  pub struct AchieveAction {object: ObjectHandle},
  PersistentTypeId(0x3995cd28e2829c09),
  fn execute (&self, accessor: &mut Accessor) {
    let action = query_ref(accessor, &self.object.varying).synchronous_action.clone().unwrap();
    action.action_type.achieve (accessor, &self.object);
    if is_destroyed (accessor, & self.object) {return;}
    if action.progress.evaluate (*accessor.now()) >= action.finish_cost {
      reconsider_action (accessor, & self.object);
    }
    else {
      modify_object (accessor, & self.object, | varying | {
        varying.synchronous_action.as_mut().unwrap().achieved = true;
      });
    }
  }
}

define_event! {
  pub struct FinishAction {object: ObjectHandle},
  PersistentTypeId(0x0242d450549f9245),
  fn execute (&self, accessor: &mut Accessor) {
    reconsider_action (accessor, &self.object);
  }
}

define_event! {
  pub struct ReachTarget {object: ObjectHandle},
  PersistentTypeId(0x26058edd2a3247aa),
  fn execute (&self, accessor: &mut Accessor) {
    reconsider_action (accessor, &self.object);
  }
}

define_event! {
  pub struct Collide {objects: [ObjectHandle; 2]},
  PersistentTypeId(0xe35485dcd0277599),
  fn execute (&self, accessor: &mut Accessor) {
    //let (striker, victim) = if query_ref (accessor, & self.objects [0].varying).object_type == ObjectType::Arrow {(& self.objects [0], & self.objects [1])} else {(& self.objects [1], & self.objects [0])};
    let (striker, victim) = (& self.objects [0], & self.objects [1]);
    let mut dead = false;
    destroy_object (accessor, striker);
    modify_object (accessor, victim, | varying | {varying.hitpoints -= 1; dead = dead || (varying.hitpoints == 0 && varying.object_type != ObjectType::Beast);});
    if dead {destroy_object (accessor, victim);}
  }
}

define_event! {
  pub struct ChangeOrders {team: usize, orders: Orders},
  PersistentTypeId(0x4c9df89d55f6ab36),
  fn execute (&self, accessor: &mut Accessor) {
    set (accessor, &accessor.globals().orders[self.team], self.orders.clone());
  }
}
