use super::*;

use nalgebra::{Vector2};
use rand::Rng;
//use boolinator::Boolinator;
use std::cmp::{min, max};


use time_steward::{DeterministicRandomId};
use time_steward::{PersistentTypeId, PersistentlyIdentifiedType, EventHandleTrait};
pub use time_steward::stewards::{simple_full as steward_module};
use steward_module::{Event, DataHandle, Accessor, EventAccessor, FutureCleanupAccessor, simple_timeline, bbox_collision_detection_2d as collisions};
use self::simple_timeline::{query, query_ref, set};
use self::collisions::{Detector as DetectorTrait};
use self::collisions::simple_grid::{SimpleGridDetector};

use time_steward::support::rounding_error_tolerant_math::Range;


pub const STRIDE: Coordinate = SECOND << 8;
pub const TRIVIAL_DISTANCE: Coordinate = STRIDE>>8;
pub const PALACE_RADIUS: Coordinate = STRIDE*5;
pub const BUILDING_GAP: Coordinate = STRIDE*3;
pub const GUILD_RADIUS: Coordinate = STRIDE*7/2;
pub const PALACE_DISTANCE: Coordinate = 282*STRIDE;
pub const INITIAL_PALACE_DISTANCE: Coordinate = PALACE_DISTANCE*3;
pub const RANGER_RANGE: Coordinate = STRIDE*20;

pub const STANDARD_ACTION_SPEED: Progress = 60;
pub const STANDARD_ACTION_SECOND: Progress = STANDARD_ACTION_SPEED*SECOND;

pub const SECONDS_PER_DAY: Time = 10*60;

pub const STANDARD_FOOD_UPKEEP_PER_SECOND: Amount = SECOND*60;
pub const STANDARD_UNIT_COST: Amount = STANDARD_FOOD_UPKEEP_PER_SECOND*SECONDS_PER_DAY;    
    // fruit is generated once per second in an area with a radius of INITIAL_PALACE_DISTANCE
    // 1000 square strides should support one unit
    // 
    // FRUIT_REWARD/pi*(INITIAL_PALACE_DISTANCE/STRIDE)^2 = STANDARD_FOOD_UPKEEP_PER_SECOND/1000
    //I did the math directly because we can't use sqrt in constant definitions and this is a hack anyway
pub const FRUIT_REWARD: Amount = STANDARD_FOOD_UPKEEP_PER_SECOND*2284;

pub const RANGER_COST: Amount = STANDARD_UNIT_COST;
pub const PEASANT_COST: Amount = STANDARD_UNIT_COST;
pub const GUILD_COST: Amount = STANDARD_UNIT_COST*4;
pub const PALACE_COST: Amount = STANDARD_UNIT_COST*30;
pub const BEAST_COST: Amount = STANDARD_UNIT_COST;
pub const BEAST_REWARD: Amount = STANDARD_UNIT_COST/2;

pub const COMBAT_PRIORITY: Amount = 1<<40;


// ##########################################
// ######       data definitions      #######
// ##########################################

#[derive (Serialize, Deserialize, Debug, Derivative)]
#[derivative (Default)]
pub struct Globals {
  #[derivative (Default (value = "new_timeline()"))]
  pub detector: Timeline <DataHandle <Detector>>,
  #[derivative (Default (value = "[new_timeline(), new_timeline()]"))]
  pub orders: [Timeline <Orders>; 2],
  #[derivative (Default (value = "new_timeline()"))]
  pub next_fruit_prediction: Timeline <EventHandle>,
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
  Fruit,
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug, Default)]
pub struct ActionPracticalities {
  indefinitely_impossible: bool,
  impossible_outside_range: Option <(ObjectHandle, Coordinate)>,
  priority: Amount,
  time_costs: Option <(Amount, Amount, i64)>,
}

impl ActionPracticalities {
  pub fn target_destroyed()->ActionPracticalities {ActionPracticalities {
    indefinitely_impossible: true,
    .. Default::default()
  }}
}

define_action_types! {
  Build,
  Recruit,
  Think,
  Shoot,
  Rest,
  Disappear,
  Pursue,
  Collect,
  Wait,
  ExchangeResources,
  
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Shoot {pub target: ObjectHandle,}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct ExchangeResources {pub target: ObjectHandle,}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Collect {pub target: ObjectHandle,}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Pursue {pub target: ObjectHandle, pub intention: Box <Action>}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Build {pub building_type: ObjectType}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Recruit {pub recruit_type: ObjectType}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Think;
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Rest;
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Wait;
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Disappear {pub time: Time,}


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct SynchronousAction {
  pub action_type: Action,
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
  
  #[derivative (Default (value = "1"))]
  pub hitpoints: Amount,
  #[derivative (Default (value = "1"))]
  pub max_hitpoints: Amount,
  #[derivative (Default (value = "LinearTrajectory1::new (0, 0, 0)"))]
  pub endurance: LinearTrajectory1,
  pub max_endurance: Amount,
  
  pub food: Amount,
  
  pub is_building: bool,
  pub is_unit: bool,
  
  pub food_cost: Amount,
  
  pub synchronous_action: Option <SynchronousAction>,
  pub ongoing_action: Option <Action>,
  
  //only used by arrows at the moment
  pub target: Option <ObjectHandle>,
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




// ##################################################
// ###### inferred object attribute functions #######
// ##################################################

pub fn is_destroyed <A: Accessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle)->bool {
  query_ref (accessor, & object.varying).destroyed
}

pub fn is_enemy <A: Accessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle, other: & ObjectHandle)->bool {
  query_ref (accessor, & object.varying).team != query_ref (accessor, & other.varying).team
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

#[inline]
pub fn default_stats <A: Accessor <Steward = Steward>>(accessor: &A, object_type: ObjectType)->ObjectVarying {
  let mut result = match object_type {
    ObjectType::Peasant => ObjectVarying {
            max_hitpoints: 5,
            radius: STRIDE*1/2,
            food_cost: PEASANT_COST,
            max_endurance: 200*SECOND,
            attack_range: STRIDE*2,
            interrupt_range: STRIDE*6,
            awareness_range: STRIDE*100,
            speed: 4*STRIDE/SECOND,
            is_unit: true,
            .. Default::default()
          },
          
    ObjectType::Lair => ObjectVarying {
              radius: GUILD_RADIUS*2/3,
              max_hitpoints: 20,
              is_building: true,
              .. Default::default()
            },
    ObjectType::Guild => ObjectVarying {
              radius: GUILD_RADIUS,
              food_cost: GUILD_COST,
              max_hitpoints: 20,
              is_building: true,
              .. Default::default()
            },
    ObjectType::Palace => ObjectVarying {
              radius: PALACE_RADIUS,
              food_cost: PALACE_COST,
              max_hitpoints: 100,
              is_building: true,
              .. Default::default()
            },
    ObjectType::Ranger => ObjectVarying {
            max_hitpoints: 5,
            radius: STRIDE/2,
            max_endurance: 100*SECOND,
            attack_range: RANGER_RANGE,
            interrupt_range: RANGER_RANGE,
            awareness_range: 200*STRIDE,
            speed: 10*STRIDE/SECOND,
            is_unit: true,
            food_cost: RANGER_COST,
            .. Default::default()
          },
    ObjectType::Beast => ObjectVarying {
            max_hitpoints: 5,
            radius: STRIDE*2/3,
            max_endurance: 200*SECOND,
            attack_range: STRIDE*2,
            interrupt_range: RANGER_RANGE,
            awareness_range: STRIDE*200,
            speed: 2*STRIDE/SECOND,
            is_unit: true,
            food_cost: BEAST_COST,
            .. Default::default()
          },
    ObjectType::Arrow => ObjectVarying {
          radius: STRIDE/5,
          speed: 50*STRIDE/SECOND,
          .. Default::default()
        },
    ObjectType::Fruit => ObjectVarying {
          max_hitpoints: 0,
          radius: STRIDE/4,
          .. Default::default()
        },
  };
  result.object_type = object_type;
  if result.is_unit {
    result.endurance = LinearTrajectory1::new (*accessor.now(), result.max_endurance, -1);
  }
  result.hitpoints = result.max_hitpoints;
  result
}

fn can_add_recruit <A: Accessor <Steward = Steward>>(accessor: &A, home: &ObjectVarying, recruit: &ObjectVarying)->bool {
  let dependents_varying: Vec<_> = home.dependents.iter().map (| dependent | query_ref (accessor, & dependent.varying)).collect();
  let current_recruit_varying = match home.synchronous_action.as_ref() {
    Some(&SynchronousAction { action_type: Action::Recruit(ref recruit), .. }) => Some (default_stats (accessor, recruit.recruit_type.clone())),
    _=> None,
  };
  let dependents_varying = dependents_varying.iter().map (| dependent | &**dependent).filter (| dependent | dependent.hitpoints > 0).chain (current_recruit_varying.iter());
  match home.object_type {
    ObjectType::Lair => recruit.object_type == ObjectType::Beast && dependents_varying.filter (| dependent | dependent.object_type == ObjectType::Beast).count() < 9,
    ObjectType::Guild => recruit.object_type == ObjectType::Ranger && dependents_varying.filter (| dependent | dependent.object_type == ObjectType::Ranger).count() < 4,
    ObjectType::Palace => recruit.object_type == ObjectType::Peasant && dependents_varying.filter (| dependent | dependent.object_type == ObjectType::Peasant).count() < 1,
    _ => false,
  }
}

pub fn reserved_food <A: Accessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle)->Amount {
  let varying = query_ref (accessor, &object.varying) ;
  let mut result = 0;
  if let Some(&SynchronousAction { achieved: false, action_type: Action::Recruit(ref recruit), .. }) = varying.synchronous_action.as_ref() {
    result += default_stats (accessor, recruit.recruit_type.clone()).food_cost;
  }
  for dependent in varying.dependents.iter() {
    let dependent_varying = query_ref (accessor, &dependent.varying) ;
    if dependent_varying.is_building && dependent_varying.hitpoints == 0 {
      result += dependent_varying.food_cost - dependent_varying.food;
    }
  }
  result
}


pub fn update_prediction <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
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
        if let Some(target_location) = varying.synchronous_action.as_ref().and_then (| action | action.action_type.target_location (accessor, object)) {
          if let Some(time) = varying.trajectory.when_collides (*accessor.now(), &LinearTrajectory2::constant(*accessor.now(), target_location), TRIVIAL_DISTANCE) {
            consider (accessor.create_prediction (time, id, ReachTarget {object: object.clone()}));
          }
        }
      }
      let collide_target = varying.object_type == ObjectType::Arrow;
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

fn interaction_choices <A: Accessor <Steward = Steward>>(_accessor: &A, _object: &ObjectHandle, other: & ObjectHandle)->Vec<Action > {
  let mut result = Vec::new() ;
  result.push (Action::Shoot (Shoot {target: other.clone()}));
  result.push (Action::Collect (Collect {target: other.clone()}));
  result.push (Action::ExchangeResources (ExchangeResources {target: other.clone()}));
  result
}

fn choose_action <A: Accessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle)->Action {
  let varying = query_ref (accessor, &object.varying) ;
  let position = varying.trajectory.evaluate (*accessor.now());
  let mut choices: Vec<(Action, ActionPracticalities)> = Vec::new();
  let consider = | choices: &mut Vec<(Action, ActionPracticalities)>, action: Action | consider_impl(accessor, object, choices, action);
  fn consider_impl <A: Accessor <Steward = Steward>> (accessor: &A, object: &ObjectHandle, choices: &mut Vec<(Action, ActionPracticalities)>, action: Action) {
    let varying = query_ref (accessor, &object.varying) ;
    let practicalities = action.practicalities (accessor, object);
    //if varying.object_type == ObjectType::Ranger {printlnerr!("{:?}", (&action, &practicalities));}
    if !practicalities.indefinitely_impossible {
      if let Some(limit) = practicalities.impossible_outside_range.clone() {
        let location = varying.trajectory.evaluate (*accessor.now());
        let target_location = query_ref (accessor, & limit.0.varying).trajectory.evaluate (*accessor.now());
        let target_distance = distance (location, target_location).max();
        if target_distance <= limit.1 {
          choices.push ((action.clone(), practicalities));
        }
        consider_impl (accessor, object, choices, Action::Pursue (Pursue {target: limit.0, intention: Box::new (action.clone())}));
      }
      else {
        choices.push ((action.clone(), practicalities));
      }
    }
  };
  
  // first pass: cheapest calculations.
  if let Some(current) = varying.ongoing_action.as_ref().or (varying.synchronous_action.as_ref().map (| action | &action.action_type)) {
    consider (&mut choices, current.clone());
  }
  /*match varying.object_type.clone() {
    ObjectType::Palace => consider (&mut choices, Action::BuildGuild(BuildGuild)),
    ObjectType::Guild => consider (&mut choices, Action::RecruitRanger(RecruitRanger)),
    ObjectType::Lair => consider (&mut choices, Action::SpawnBeast(SpawnBeast)),
    ObjectType::Arrow => consider (&mut choices, Action::Disappear(Disappear {time: SECOND*1/2})),
    _=>(),
  }*/
  
  consider (&mut choices, Action::Build(Build {building_type: ObjectType::Guild}));
  consider (&mut choices, Action::Build(Build {building_type: ObjectType::Palace}));
  consider (&mut choices, Action::Recruit(Recruit { recruit_type: ObjectType::Ranger }));
  consider (&mut choices, Action::Recruit(Recruit { recruit_type: ObjectType::Peasant }));
  consider (&mut choices, Action::Recruit(Recruit { recruit_type: ObjectType::Beast}));
  consider (&mut choices, Action::Disappear(Disappear {time: SECOND*1/2}));
  consider (&mut choices, Action::Rest(Rest));
  
  choices.sort_by_key (| choice | choice.1.priority);
  if let Some(best) = choices.last() {
    if best.1.priority>=COMBAT_PRIORITY {
      return best.0.clone()
    }
  }
  
  // second pass: scan surroundings for stuff.
  // Commonly used for characters on long journeys to notice nearby interesting stuff.
  // Optimization: currently, only units can interact with nearby stuff
  if varying.is_unit {        
    let nearby = objects_touching_circle (accessor, position, varying.interrupt_range);
    for other in nearby {
      assert! (!is_destroyed (accessor, & other), "destroyed objects shouldn't be in the collision detection") ;
      for choice in interaction_choices (accessor, object, &other) {
        consider (&mut choices, choice,);
      }
    }
    if let Some(home) = varying.home.as_ref() { if !is_destroyed(accessor, home) {
      for dependent in query_ref (accessor, & home.varying).dependents.iter() {
        for choice in interaction_choices (accessor, object, dependent ) {
          consider (&mut choices, choice,);
        }
      }
    }}
  }
    
  choices.sort_by_key (| choice | choice.1.priority);
  if let Some(best) = choices.last() {
    if best.1.priority> 0{
      return best.0.clone()
    }
  }
  
  // third pass: there's nothing to do nearby, so it's finally worth it to pay the larger cost of searching for a distant task
  
  if varying.is_unit {
    let nearby = objects_touching_circle (accessor, position, varying.awareness_range);
    for other in nearby {
      assert! (!is_destroyed (accessor, & other), "destroyed objects shouldn't be in the collision detection") ;
      let other_varying = query_ref (accessor, & other.varying);
      let other_position = other_varying.trajectory.evaluate (*accessor.now());
      let other_distance = distance (position, other_position).max() - radius (& other_varying);
      if other_distance > varying.interrupt_range {
        for choice in interaction_choices (accessor, object, &other) {
          consider (&mut choices, choice);
        }
      }
    }
  }
  
  choices.sort_by_key (| choice | choice.1.priority);
  if let Some(best) = choices.last() {
    if best.1.priority> 0{
      return best.0.clone()
    }
  }
  
  Action::Wait(Wait)

  
}


fn make_synchronous_action <A: EventAccessor <Steward = Steward>>(accessor: &A, action: Action, costs: (Amount, Amount, i64))->SynchronousAction {
  let mut generator = DeterministicRandomId::new (& (accessor.extended_now().id, 0x7b017f025975dd1du64)).to_rng();
  let (achieve_cost, mut finish_cost, variability_percent) = costs;
  finish_cost = max(achieve_cost, finish_cost);
  let modifier = generator.gen_range (100 - variability_percent, 100 + variability_percent + 1);
  SynchronousAction {
    action_type: action.clone(),
    achieve_cost: achieve_cost * modifier / 100,
    finish_cost: finish_cost * modifier / 100,
    progress: LinearTrajectory1::new (*accessor.now(), 0, STANDARD_ACTION_SPEED),
    achieved: false,
  }
}
pub fn set_action <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle, action: Option<Action>) {
  let (synchronous_action, ongoing_action) = match action.clone() {
    None => (None, None),
    Some (action) => match action.practicalities (accessor, object).time_costs {
      None => (Some(make_synchronous_action(accessor, Action::Think (Think), (STANDARD_ACTION_SECOND*6/10, 0, 20))), Some (action)),
      Some (costs) => (Some(make_synchronous_action(accessor, action, costs)), None),
    }
  };
  modify_object (accessor, & object, | varying | {
    varying.synchronous_action = synchronous_action;
    varying.ongoing_action = ongoing_action;
    //if varying.object_type == ObjectType::Ranger {printlnerr!("CHOSEN {:?}", (&action));}
  });
  modify_object (accessor, & object, | varying | {
    let velocity = if let Some(target) = varying.synchronous_action.as_ref().and_then (| action | action.action_type.target_location (accessor, object)) {
      let position = varying.trajectory.evaluate (*accessor.now());
      normalized_to (target - position, varying.speed)
    }
    else {
      Vector::new(0,0)
    };
    varying.trajectory.set_velocity (*accessor.now(), velocity);
  });
}

pub fn reconsider_action <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
  if !is_destroyed (accessor, object) && query_ref (accessor, &object.varying).hitpoints > 0 {
    set_action (accessor, object, Some (choose_action (accessor, object))) ;
  }
}


impl ActionTrait for Build {
  fn practicalities <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->ActionPracticalities {
    let varying = query (accessor, &object.varying) ;
    let stats = default_stats(accessor,self.building_type.clone());
    let reserved = reserved_food (accessor, object);
    let priority = if varying.food < stats.food_cost + reserved {
      -1000
    }
    else if varying.object_type == ObjectType::Palace {
      stats.food_cost
    }
    else {
      -1000
    };
    ActionPracticalities {
      indefinitely_impossible: !(
        (varying.is_unit || varying.is_building)
        && (self.building_type == ObjectType::Guild || self.building_type == ObjectType::Palace)
      ),
      priority: priority,
      time_costs: Some ((1*STANDARD_ACTION_SECOND, 0, 5)),
      .. Default::default()
    }
  }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    let varying = query (accessor, &object.varying) ;
    let stats = default_stats(accessor,self.building_type.clone());
        let position = varying.trajectory.evaluate (*accessor.now());
        let mut generator = accessor.extended_now().id.to_rng();
        
        // TODO: search for abandoned construction sites first

        for attempt in 0..50 {
          let attempt_distance = 
            if self.building_type == ObjectType::Palace {PALACE_DISTANCE} else {varying.radius + BUILDING_GAP} + stats.radius + attempt*STRIDE;
          
          let target_position = position + random_vector_exact_length (&mut generator, attempt_distance);
          
          if distance (target_position, Vector::new (0, 0)).max() >INITIAL_PALACE_DISTANCE*10/9 {continue;}
          
          // TODO: don't respect unbuilt enemy buildings
          
          let nearby = objects_touching_circle (accessor, target_position, stats.radius + BUILDING_GAP - TRIVIAL_DISTANCE );
          
          if nearby.into_iter().any(| other | {
            let other_varying = query_ref (accessor, & other.varying);
            is_building (& other_varying)
          }) { continue; }
          
          if self.building_type == ObjectType::Palace {
            let nearby = objects_touching_circle (accessor, target_position, PALACE_DISTANCE - TRIVIAL_DISTANCE );
          
            if nearby.into_iter().any(| other | {
              let other_varying = query_ref (accessor, & other.varying);
              other_varying.object_type == ObjectType::Palace
            }) { continue; }
          }
          
          let new = create_object (accessor, object, 0x379661e69cdd5fe7,
            ObjectVarying {
              team: varying.team,
              home: Some (object.clone()),
              food: 0,
              hitpoints: 0,
              trajectory: LinearTrajectory2::constant (*accessor.now(), target_position),
              .. stats.clone()
            }
          );
          modify_object (accessor, object, | varying | {
            //varying.food -= stats.food_cost;
            varying.dependents.push (new);
          });
          break
        }

  }
}

impl ActionTrait for Recruit {
  fn practicalities <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->ActionPracticalities {
    let varying = query_ref (accessor, &object.varying);
    let stats = default_stats(accessor,self.recruit_type.clone());
    let reserved = reserved_food (accessor, object);
    let priority = if varying.food < stats.food_cost + reserved {
      -1000
    }
    else {
      1000
    };
    ActionPracticalities {
      priority: priority,
      indefinitely_impossible: !(can_add_recruit (accessor, & varying, & stats) && varying.food >= stats.food_cost),
      time_costs: Some ((10*STANDARD_ACTION_SECOND, 0, 5)),
      .. Default::default()
    }
  }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    let varying = query (accessor, &object.varying) ;
    let stats = default_stats(accessor,self.recruit_type.clone());
        let new = create_object (accessor, object, 0x91db5029ba8b0a4e,
          ObjectVarying {
            team: varying.team,
            home: Some (object.clone()),
            trajectory: LinearTrajectory2::constant (*accessor.now(),varying.trajectory.evaluate (*accessor.now()) + random_vector_exact_length (&mut accessor.extended_now().id.to_rng(), varying.radius + 2*STRIDE)),
            .. stats.clone()
          },
        );
        modify_object (accessor, object, | varying | {
          varying.food -= stats.food_cost;
          varying.dependents.push (new);
        });

  }
}



impl ActionTrait for Think {
  fn practicalities <A: Accessor <Steward = Steward>> (&self, _accessor: &A, _object: &ObjectHandle)->ActionPracticalities {
    ActionPracticalities {
      time_costs: Some ((STANDARD_ACTION_SECOND*6/10, 0, 20)),
      .. Default::default()
    }
  }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, _accessor: &A, _object: &ObjectHandle) {
    //let varying = query (accessor, &object.varying);
    
  }
  fn target_location <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Option<Vector> {
    let varying = query_ref (accessor, &object.varying) ;
    varying.ongoing_action.as_ref().expect ("there should be an ongoing action if you are thinking").target_location(accessor, object)
  }
}



impl ActionTrait for Shoot {
  fn practicalities <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->ActionPracticalities {
    if is_destroyed (accessor, & self.target) {return ActionPracticalities::target_destroyed();}
    let varying = query_ref (accessor, &object.varying);
    let target_varying = query_ref (accessor, & self.target.varying);
    ActionPracticalities {
      priority: 100*if is_enemy (accessor, object, & self.target) {1} else {-1},
      indefinitely_impossible: !(varying.is_unit && varying.object_type != ObjectType::Peasant && target_varying.is_unit && *object != self.target && target_varying.hitpoints >0),
      impossible_outside_range: Some ((self.target.clone(), varying.attack_range)),
      time_costs: Some ((STANDARD_ACTION_SECOND*6/10, STANDARD_ACTION_SECOND*10/10, 5)),
      .. Default::default()
    }
  }
  
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    let varying = query (accessor, &object.varying) ;
    
        let target = & self.target;
        if !is_destroyed (accessor, target) {
        //destroy_object (accessor, varying.action.as_ref().unwrap().target.as_ref().unwrap());
        let position = varying.trajectory.evaluate (*accessor.now());
        let other_position = query_ref (accessor, & target.varying).trajectory.evaluate (*accessor.now());
        let new_velocity = normalized_to (other_position - position, 50*STRIDE/SECOND);
        create_object (accessor, object, 0x27706762e4201474, ObjectVarying {
          team: varying.team,
          target: Some(self.target.clone()),
          trajectory: LinearTrajectory2::new (*accessor.now(), varying.trajectory.evaluate (*accessor.now()), new_velocity),
          .. default_stats(accessor, ObjectType::Arrow)
        });
        }

  }
}



impl ActionTrait for Disappear {
  fn practicalities <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->ActionPracticalities {
    let varying = query_ref (accessor, &object.varying);
    ActionPracticalities {
      priority: COMBAT_PRIORITY + 1000,
      indefinitely_impossible: varying.object_type != ObjectType::Arrow,
      time_costs: Some ((self.time*STANDARD_ACTION_SPEED, 0, 0)),
      .. Default::default()
    }
  }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    destroy_object (accessor, object);
  }
  fn target_location <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Option<Vector> {
    let varying = query_ref (accessor, &object.varying) ;
    let foo = query_ref (accessor, & varying.target.as_ref().unwrap().varying).trajectory.evaluate (*accessor.now());
    Some(foo)
  }
}



// TODO: can't "pursue in order to X" if you are already in range to X
impl ActionTrait for Pursue {
  fn practicalities <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->ActionPracticalities {
    if is_destroyed (accessor, & self.target) {return ActionPracticalities::target_destroyed();}
    let varying = query_ref (accessor, &object.varying);
    //let target_varying = query_ref (accessor, & self.target.varying);
    let mut result = self.intention.practicalities(accessor, object);
    if let Some(limit) = result.impossible_outside_range.take() {
      let location = varying.trajectory.evaluate (*accessor.now());
      let target_location = query_ref (accessor, & limit.0.varying).trajectory.evaluate (*accessor.now());
      let target_distance = distance (location, target_location).max();
      let excessive_distance = target_distance - limit.1;
      if excessive_distance <= 0 || varying.speed <= 0 || *object == limit.0 {
        result.indefinitely_impossible = true;
      } else {
        let time_to_reach = (excessive_distance + varying.speed - 1)/varying.speed;
        let time_to_perform = max(result.time_costs.unwrap().0, result.time_costs.unwrap().1)/STANDARD_ACTION_SPEED;
        result.priority = result.priority*time_to_perform/(time_to_perform + time_to_reach);
      }
    }
    else {
      result.indefinitely_impossible = true;
    }
    result.time_costs = None;
    
    result
  }
  fn target_location <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Option<Vector> {
    //let varying = query_ref (accessor, &object.varying) ;
    let practicalities = self.intention.practicalities(accessor, object);
    practicalities.impossible_outside_range.map (| whatever | {
      query_ref (accessor, & whatever.0.varying).trajectory.evaluate (*accessor.now())
    })
  }
}



impl ActionTrait for Wait {
  fn practicalities <A: Accessor <Steward = Steward>> (&self, _accessor: &A, _object: &ObjectHandle)->ActionPracticalities {
    Default::default()
  }
}


impl Collect {
  fn reward <A: Accessor <Steward = Steward>> (&self, accessor: &A, _object: &ObjectHandle)->Amount {
    let target_varying = query_ref (accessor, & self.target.varying);
    if target_varying.object_type == ObjectType::Fruit {FRUIT_REWARD} else {BEAST_REWARD}
  }
}
impl ActionTrait for Collect {
  fn practicalities <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->ActionPracticalities {
    if is_destroyed (accessor, & self.target) {return ActionPracticalities::target_destroyed();}
    let varying = query_ref (accessor, &object.varying);
    let target_varying = query_ref (accessor, & self.target.varying);
    ActionPracticalities {
      priority: self.reward(accessor, object),
      indefinitely_impossible: !(
        (varying.object_type == ObjectType::Ranger && target_varying.object_type == ObjectType::Beast && target_varying.hitpoints <= 0)
        || (varying.is_unit && target_varying.object_type == ObjectType::Fruit)
      ),
      impossible_outside_range: Some ((self.target.clone(), STRIDE/2)),
      time_costs: Some ((STANDARD_ACTION_SECOND*10/10, 0, 10)),
      .. Default::default()
    }
  }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    if !is_destroyed (accessor, & self.target) {
      let reward = self.reward(accessor, object);
      destroy_object (accessor, & self.target);
      modify_object (accessor, object, | varying | {
        varying.food += reward;
      });
    }
  }
}

impl ExchangeResources {
  fn transfer_amount <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Amount {
    let varying = query_ref (accessor, &object.varying);
    let target_varying = query_ref (accessor, & self.target.varying);
    let mut transfer;
    if target_varying.object_type == ObjectType::Palace && target_varying.hitpoints > 0 {
      transfer = varying.food - RANGER_COST*4;
    }
    else if target_varying.hitpoints == 0 {
      transfer = target_varying.food_cost - target_varying.food;
    }
    else {
      transfer = RANGER_COST*2 - target_varying.food;
    }
    transfer = min (varying.food, transfer);
    transfer = max (-target_varying.food, transfer);
    transfer
  }
}
impl ActionTrait for ExchangeResources {
  fn practicalities <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->ActionPracticalities {
    if is_destroyed (accessor, & self.target) {return ActionPracticalities::target_destroyed();}
    let varying = query_ref (accessor, &object.varying);
    let target_varying = query_ref (accessor, & self.target.varying);
    let transfer = self.transfer_amount (accessor, object);
    let desire = if transfer > 0 {transfer*100} else {-transfer};
    ActionPracticalities {
      priority: desire,
      indefinitely_impossible: !(varying.object_type == ObjectType::Peasant && (target_varying.object_type == ObjectType::Palace || target_varying.object_type == ObjectType::Guild)),
      impossible_outside_range: Some ((self.target.clone(), STRIDE/2)),
      time_costs: Some ((STANDARD_ACTION_SECOND*5, 0, 10)),
      .. Default::default()
    }
  }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    if !is_destroyed (accessor, & self.target) {
      let transfer = self.transfer_amount (accessor, object);
      let mut finished_building = false;
      modify_object (accessor, object, | varying | {
        varying.food -= transfer;
      });
      modify_object (accessor, & self.target, | varying | {
        if varying.food == 0 {
          // TODO remove building construction sites
        }
        varying.food += transfer;
        if varying.hitpoints == 0 && varying.food == varying.food_cost {
          varying.food = 0;
          varying.hitpoints = varying.max_hitpoints;
          finished_building = true;
        }
      });
      if finished_building {
        let mut home = None;
        modify_object (accessor, & self.target, | varying | {
          home = varying.home.take();
        });
        if let Some(home) = home { if !is_destroyed (accessor, & home) {
          modify_object (accessor, & home, | varying | {
            varying.dependents.retain (|a| *a != self.target);
          });
        }}
        reconsider_action (accessor, & self.target);
      }
    }
  }
}



impl ActionTrait for Rest {
  fn practicalities <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->ActionPracticalities {
    let varying = query_ref (accessor, &object.varying);
    if let Some(home) = varying.home.as_ref() {
      if !is_destroyed (accessor, home) {
        return ActionPracticalities {
          priority: varying.max_endurance/3 - varying.endurance.evaluate (*accessor.now()),
          indefinitely_impossible: !varying.is_unit,
          impossible_outside_range: Some ((home.clone(), STRIDE)),
          time_costs: Some ((STANDARD_ACTION_SECOND*20, 0, 5)),
          .. Default::default()
        }
      }
    }
    ActionPracticalities {
      indefinitely_impossible: true,
      .. Default::default()
    }
  }
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {
    let varying = query (accessor, &object.varying) ;
    let home = varying.home.as_ref().unwrap();
    if !is_destroyed (accessor, home) {
      let home_varying = query (accessor, & home.varying);
      let total_food = home_varying.food + varying.food;
      let current_endurance = varying.endurance.evaluate (*accessor.now());
      let endurance_deficiency = varying.max_endurance - current_endurance;
      let desired_food = (Range::exactly (endurance_deficiency)*STANDARD_FOOD_UPKEEP_PER_SECOND/Range::exactly (SECOND)).min();
      let consumed_food = min (total_food, desired_food);
      let restored_endurance = (Range::exactly (consumed_food) * (SECOND) / Range::exactly (STANDARD_FOOD_UPKEEP_PER_SECOND)).min();
      //printlnerr!("{:?}", (consumed_food, consumed_food/STANDARD_FOOD_UPKEEP_PER_SECOND, restored_endurance,restored_endurance/SECOND, restored_endurance*100/varying.max_endurance));
      modify_object (accessor, object, | varying | varying.food = 0);
      modify_object (accessor, home, | other_varying | other_varying.food = total_food - consumed_food);
      modify_object (accessor, object, | varying | {
        varying.endurance.set (*accessor.now(), current_endurance + restored_endurance);
      });
    }
  }
}



define_event! {
  pub struct Initialize {},
  PersistentTypeId(0x9a633852de46827f),
  fn execute (&self, accessor: &mut Accessor) {
    let mut generator = accessor.extended_now().id.to_rng();
    set (accessor, &accessor.globals().detector, SimpleGridDetector::new (accessor, Space, (STRIDE*50) as collisions::Coordinate));
    set (accessor, &accessor.globals().next_fruit_prediction, accessor.create_prediction (*accessor.now(), DeterministicRandomId::new (& (accessor.extended_now().id, 0x8459d2d1496d5f23u64)), GenerateFruit {}));
    for team in 0..2 {
      set (accessor, &accessor.globals().orders[team], Orders {unit_destination: None });
      create_object_impl (accessor, None, DeterministicRandomId::new (& (team, 0xb2e085cd02f2f8dbu64)),
        ObjectVarying {
          team: team,
          food: PEASANT_COST + PEASANT_COST + GUILD_COST + RANGER_COST,
          trajectory: LinearTrajectory2::constant (*accessor.now(), Vector2::new (0, INITIAL_PALACE_DISTANCE*team as Coordinate*2 - INITIAL_PALACE_DISTANCE)),
          .. default_stats(accessor, ObjectType::Palace)
        },
      );
    }
    for index in 0..28 {
      create_object_impl (accessor, None, DeterministicRandomId::new (& (index, 0xb2e085cd02f2f8dbu64)),
        ObjectVarying {
          team: 6,
          food: BEAST_COST,
          trajectory: LinearTrajectory2::constant (*accessor.now(), Vector::new (0, 0) + random_vector_within_length (&mut generator, INITIAL_PALACE_DISTANCE)),
          .. default_stats(accessor, ObjectType::Lair)
        },
      );
    }
  }
}

define_event! {
  pub struct AchieveAction {pub object: ObjectHandle},
  PersistentTypeId(0x3995cd28e2829c09),
  fn execute (&self, accessor: &mut Accessor) {
    let action = query_ref(accessor, &self.object.varying).synchronous_action.clone().unwrap();
    action.action_type.achieve (accessor, &self.object);
    if is_destroyed (accessor, & self.object) {return;}
    modify_object (accessor, & self.object, | varying | {
      varying.synchronous_action.as_mut().unwrap().achieved = true;
    });
    if action.progress.evaluate (*accessor.now()) >= action.finish_cost {
      reconsider_action (accessor, & self.object);
    }
  }
}

define_event! {
  pub struct FinishAction {pub object: ObjectHandle},
  PersistentTypeId(0x0242d450549f9245),
  fn execute (&self, accessor: &mut Accessor) {
    reconsider_action (accessor, &self.object);
  }
}

define_event! {
  pub struct ReachTarget {pub object: ObjectHandle},
  PersistentTypeId(0x26058edd2a3247aa),
  fn execute (&self, accessor: &mut Accessor) {
    //printlnerr!("{:?}", query (accessor, & self.object.varying));
    reconsider_action (accessor, &self.object);
  }
}

define_event! {
  pub struct Collide {pub objects: [ObjectHandle; 2]},
  PersistentTypeId(0xe35485dcd0277599),
  fn execute (&self, accessor: &mut Accessor) {
    //let (striker, victim) = if query_ref (accessor, & self.objects [0].varying).object_type == ObjectType::Arrow {(& self.objects [0], & self.objects [1])} else {(& self.objects [1], & self.objects [0])};
    let (striker, victim) = (& self.objects [0], & self.objects [1]);
    let mut dead = false;
    let mut leave_corpse = false;
    destroy_object (accessor, striker);
    modify_object (accessor, victim, | varying | {
      varying.hitpoints -= 1;
      dead = varying.hitpoints == 0;
      leave_corpse = varying.object_type == ObjectType::Beast;
    });
    if dead {
      if leave_corpse {
        set_action (accessor, victim, None);
      }
      else {
        destroy_object (accessor, victim);
      }
    }
  }
}

define_event! {
  pub struct ChangeOrders {pub team: usize, pub orders: Orders},
  PersistentTypeId(0x4c9df89d55f6ab36),
  fn execute (&self, accessor: &mut Accessor) {
    set (accessor, &accessor.globals().orders[self.team], self.orders.clone());
  }
}

define_event! {
  pub struct GenerateFruit {},
  PersistentTypeId(0x0874667e420524f7),
  fn execute (&self, accessor: &mut Accessor) {
    set (accessor, &accessor.globals().next_fruit_prediction, accessor.create_prediction (accessor.now() + SECOND, DeterministicRandomId::new (& (accessor.extended_now().id, 0x8459d2d1496d5f23u64)), GenerateFruit {}));
    
    create_object_impl (accessor, None, DeterministicRandomId::new (& (accessor.extended_now().id, 0x61b3d4d2ab30e0abu64)),
      ObjectVarying {
        team: 6,
        trajectory: LinearTrajectory2::constant (*accessor.now(), Vector::new (0, 0) + random_vector_within_length (&mut accessor.extended_now().id.to_rng(), INITIAL_PALACE_DISTANCE)),
        .. default_stats(accessor, ObjectType::Fruit)
      },
    );
  }
}
