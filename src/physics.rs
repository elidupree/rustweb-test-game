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
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub enum ActionType {
  BuildGuild,
  RecruitRanger,
  SpawnBeast,
  Think,
  Shoot,
  Rest,
  Disappear,
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug, Derivative)]
#[derivative (Default)]
pub struct Action {
  #[derivative (Default (value = "ActionType::Think"))]
  pub action_type: ActionType,
  pub target: Option <ObjectHandle>,
  #[derivative (Default (value = "LinearTrajectory1::constant(0,0)"))]
  pub progress: LinearTrajectory1,
  pub achieve_cost: Progress,
  #[derivative (Default (value = "0"))]
  pub finish_cost: Progress,
  #[derivative (Default (value = "false"))]
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
  pub awareness_range: Coordinate,
  pub detector_data: Option <DetectorData>,
  pub team: usize,
  pub hitpoints: Amount,
  pub food: Amount,
  pub is_building: bool,
  pub is_unit: bool,
  pub action: Option <Action>,
  pub target: Option <ObjectHandle>,
  pub target_location: Option <Vector>,
  #[derivative (Default (value = "LinearTrajectory1::new (0, 0, 0)"))]
  pub endurance: LinearTrajectory1,
  pub home: Option <ObjectHandle>,
  pub dependents: Vec <ObjectHandle>,
  pub prediction: Option <EventHandle>,
  #[derivative (Default (value = "false"))]
  pub destroyed: bool,
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


fn make_action <A: EventAccessor <Steward = Steward>>(accessor: &A, variability_percent: Progress, mut details: Action)->Action {
  let mut generator = DeterministicRandomId::new (& (accessor.extended_now().id, 0x7b017f025975dd1du64)).to_rng();
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
      if let Some (action) = varying.action.as_ref() {
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
      if varying.object_type == ObjectType::Arrow {
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

fn choose_action <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
  modify_object (accessor, & object, | varying | {
    varying.action = match varying.object_type.clone() {
      ObjectType::Palace => Some (make_action (accessor, 5, Action {
        action_type: ActionType::BuildGuild,
        achieve_cost: 2*STANDARD_ACTION_SECOND,
        ..Default::default()
      })),
      ObjectType::Guild => Some (make_action (accessor, 5, Action {
        action_type: ActionType::RecruitRanger,
        achieve_cost: 2*STANDARD_ACTION_SECOND,
        ..Default::default()
      })),
      ObjectType::Lair => (varying.dependents.len() < 3).as_some (make_action (accessor, 35, Action {
        action_type: ActionType::SpawnBeast,
        achieve_cost: 25*STANDARD_ACTION_SECOND,
        ..Default::default()
      })),
      ObjectType::Ranger | ObjectType::Beast => {
        let position = varying.trajectory.evaluate (*accessor.now());
        let mut result = None;
        
        let nearby = Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (position), varying.attack_range as u64), Some (& object));
        let closest = nearby.into_iter().filter_map (| other | {
          assert! (!is_destroyed (accessor, & other), "destroyed objects shouldn't be in the collision detection") ;
          let other_varying = query_ref (accessor, & other.varying);
          if is_enemy (accessor, object, & other) && other_varying.object_type != ObjectType::Arrow && other_varying.object_type != ObjectType::Lair {
            let other_position = other_varying.trajectory.evaluate (*accessor.now());
            let other_distance = distance (position, other_position).max();
            if other_distance <= varying.attack_range + radius (& other_varying) {
              return Some ((other.clone(), other_distance, other_position));
            }
          }
          None
        }).min_by_key (| t | t.1);
        
        if let Some(closest) = closest {
          result = Some (make_action (accessor, 5, Action {
            action_type: ActionType::Shoot,
            target: Some(closest.0),
            achieve_cost: STANDARD_ACTION_SECOND*6/10,
            finish_cost: STANDARD_ACTION_SECOND*1,
            ..Default::default()
          }));
          varying.trajectory.set_velocity (*accessor.now(), Vector::new (0, 0));
          //varying.target = None;
        }
                  
        if result.is_none() {result = Some(make_action (accessor, 20, Action {
          action_type: ActionType::Think,
          achieve_cost: STANDARD_ACTION_SECOND*6/10,
          ..Default::default()}
        ))}
        result
      },
      ObjectType::Arrow => Some(make_action (accessor, 5, Action {
        action_type: ActionType::Disappear,
        achieve_cost: STANDARD_ACTION_SECOND*1/2,
        ..Default::default()}
      )),
    };
  });
}



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
    let varying = query (accessor, &self.object.varying) ;
    let action = varying.action.clone().unwrap();
    match action.action_type {
      ActionType::BuildGuild => {
        let position = varying.trajectory.evaluate (*accessor.now());
        let mut generator = accessor.extended_now().id.to_rng();
        for attempt in 0..9 {
          let guild = attempt < 5;
          let cost = if guild {GUILD_COST} else {PALACE_COST};
          if varying.food < cost {break;}
          let minimum_distance = if guild { varying.radius + GUILD_RADIUS + 10*STRIDE } else {PALACE_DISTANCE};
          
          let target_position = position + random_vector_exact_length (&mut generator, if attempt <5 {minimum_distance*(/*attempt/2 +*/ 1)} else {minimum_distance});
          
          if distance (target_position, Vector::new (0, 0)).max() >INITIAL_PALACE_DISTANCE*10/9 {continue;}
          
          let nearby = Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (target_position), minimum_distance as u64), Some (& self.object));
          
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
              is_building: true,
              food: RANGER_COST,
              
              team: varying.team,
              home: Some (self.object.clone()),
              trajectory: LinearTrajectory2::constant (*accessor.now(), target_position),
              .. Default::default()
            };
            let new = create_object (accessor, & self.object, 0x379661e69cdd5fe7,
              if guild {defaults} else {make_palace (defaults)}
            );
            modify_object (accessor, & self.object, | varying | {
              varying.food -= cost;
              varying.dependents.push (new);
            });
            break
          }
        }
      },
      ActionType::RecruitRanger => {
        if varying.food >= RANGER_COST && varying.dependents.len() < 4 {
        let new = create_object (accessor, & self.object, 0x91db5029ba8b0a4e,
          ObjectVarying {
            object_type: ObjectType::Ranger,
            team: varying.team,
            home: Some (self.object.clone()),
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
        modify_object (accessor, & self.object, | varying | {
          varying.food -= RANGER_COST;
          varying.dependents.push (new);
        });
        }
        else if varying.food > RANGER_COST {
          let home = varying.home.as_ref().unwrap();
          if !is_destroyed (accessor, home) {
            modify_object (accessor, & self.object, | varying | varying.food = RANGER_COST);
            modify_object (accessor, home, | other_varying | other_varying.food += varying.food - RANGER_COST);
          }
        }
      },
      ActionType::SpawnBeast => {
        let new = create_object (accessor, & self.object, 0x91db5029ba8b0a4e,
          ObjectVarying {
            object_type: ObjectType::Beast,
            team: varying.team,
            home: Some (self.object.clone()),
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
          let nearby = Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (position), varying.awareness_range as u64), Some (& self.object));
          let closest = (varying.endurance.evaluate (*accessor.now()) < 10*SECOND).as_option().and_then (|_| varying.home.as_ref().map(|home| (home.clone(), query(accessor, &home.varying).trajectory.evaluate (*accessor.now())))).or_else(|| nearby.into_iter().filter_map (| other | {
            let other_varying = query_ref (accessor, & other.varying);
            if is_enemy (accessor, & self.object, & other) && 
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
            modify_object (accessor, & self.object, | varying | {
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
          target: action.target,
          radius: STRIDE/5,
          trajectory: LinearTrajectory2::new (*accessor.now(), varying.trajectory.evaluate (*accessor.now()), new_velocity),
          .. Default::default()
        });
        }
      },
      ActionType::Disappear => {
        destroy_object (accessor, & self.object);
        return
      },
      ActionType::Rest => {
        modify_object (accessor, & self.object, | varying | {
          varying.endurance.set (*accessor.now(), 600*SECOND);
        });
      },
    }
    
    if action.progress.evaluate (*accessor.now()) >= action.finish_cost {
      choose_action (accessor, & self.object);
    }
    else {
      modify_object (accessor, & self.object, | varying | {
        varying.action.as_mut().unwrap().achieved = true;
      });
    }
  }
}

define_event! {
  pub struct FinishAction {object: ObjectHandle},
  PersistentTypeId(0x0242d450549f9245),
  fn execute (&self, accessor: &mut Accessor) {
    choose_action (accessor, &self.object);
  }
}

define_event! {
  pub struct ReachTarget {object: ObjectHandle},
  PersistentTypeId(0x26058edd2a3247aa),
  fn execute (&self, accessor: &mut Accessor) {
    modify_object (accessor, & self.object, | varying | {
      varying.target = None;
      varying.target_location = None;
      varying.trajectory.set_velocity (*accessor.now(), Vector::new (0, 0));
      varying.action = Some(make_action (accessor, 5, Action {
        action_type: ActionType::Rest,
        achieve_cost: 20*STANDARD_ACTION_SECOND,
        ..Default::default()
      }));
    });
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

define_event! {
  pub struct ChangeOrders {team: usize, orders: Orders},
  PersistentTypeId(0x4c9df89d55f6ab36),
  fn execute (&self, accessor: &mut Accessor) {
    set (accessor, &accessor.globals().orders[self.team], self.orders.clone());
  }
}
