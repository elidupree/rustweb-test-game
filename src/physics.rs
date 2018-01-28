use super::*;

use nalgebra::{Vector2};
use rand::Rng;
use boolinator::Boolinator;


use time_steward::{DeterministicRandomId};
use time_steward::{PersistentTypeId, ListedType, PersistentlyIdentifiedType, DataTimelineCellTrait, QueryResult, EventHandleTrait, Basics as BasicsTrait};
pub use time_steward::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, Event, DataHandle, DataTimelineCell, Accessor, EventAccessor, FutureCleanupAccessor, simple_timeline, bbox_collision_detection_2d as collisions};
use self::simple_timeline::{SimpleTimeline, query, query_ref, set};
use self::collisions::{NumDimensions, Detector as DetectorTrait};
use self::collisions::simple_grid::{SimpleGridDetector};


pub type Timeline <T> = DataTimelineCell <SimpleTimeline <T, Steward>>;
pub fn new_timeline <T: QueryResult> ()->Timeline <T> {DataTimelineCell::new (SimpleTimeline::new())}
pub type Detector = SimpleGridDetector <Space>;
pub type BoundingBox = collisions::BoundingBox<Space>;
pub type DetectorData = collisions::simple_grid::DetectorDataPerObject<Space>;

pub type Time = i64;
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
pub const AWARENESS_RANGE: Coordinate = STRIDE*100;


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

pub type Steward = steward_module::Steward <Basics>;
pub type EventHandle = <Steward as TimeSteward>::EventHandle;

#[derive (Serialize, Deserialize, Debug)]
pub struct Globals {
  pub detector: Timeline <DataHandle <Detector>>,
  pub orders: [Timeline <Orders>; 2],
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub enum ObjectType {
  Palace,
  Guild,
  Ranger,
  Arrow,
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub enum ActionType {
  BuildGuild,
  RecruitRanger,
  Think,
  Shoot,
  Rest,
  Disappear,
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Action {
  pub action_type: ActionType,
  pub target: Option <ObjectHandle>,
  pub progress: LinearTrajectory1,
  pub cost: Coordinate,
}

impl Default for Action {fn default()->Self {Action {
  action_type: ActionType::Think,
  target: None,
  progress: LinearTrajectory1::new (0, 0, 0),
  cost: 999*SECOND,
}}}


#[derive (Clone, Serialize, Deserialize, Debug)]
pub struct Object {
  pub id: DeterministicRandomId,
  pub varying: Timeline <ObjectVarying>,
}
impl PersistentlyIdentifiedType for Object {
  const ID: PersistentTypeId = PersistentTypeId(0x2079a1bb8d4e9d9f);
}
pub type ObjectHandle = DataHandle <Object>;

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct ObjectVarying {
  pub object_type: ObjectType,
  pub trajectory: LinearTrajectory2,
  pub detector_data: Option <DetectorData>,
  pub team: usize,
  pub hitpoints: i64,
  pub action: Option <Action>,
  pub target: Option <ObjectHandle>,
  pub target_location: Option <Vector>,
  pub endurance: LinearTrajectory1,
  pub home: Option <ObjectHandle>,
  pub dependents: Vec <ObjectHandle>,
  pub prediction: Option <EventHandle>,
  pub destroyed: bool,
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
  endurance: LinearTrajectory1::new (0, 0, 0),
  home: None,
  dependents: Vec::new(),
  prediction: None,
  destroyed: false,
}}}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Orders {
  pub unit_destination: Option <Vector>,
}



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
      if let Some(target_location) = target_location (accessor, object) {
        if let Some(time) = varying.trajectory.when_collides (*accessor.now(), &LinearTrajectory2::constant(*accessor.now(), target_location), TRIVIAL_DISTANCE) {
          consider (accessor.create_prediction (time, id, ReachTarget {object: object.clone()}));
        }
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

pub fn radius (varying: & ObjectVarying)->Coordinate {
  match varying.object_type {
    ObjectType::Palace => PALACE_RADIUS,
    ObjectType::Guild => GUILD_RADIUS,
    ObjectType::Ranger => STRIDE/2,
    ObjectType::Arrow => STRIDE/5,
  }
}
pub fn is_building(varying: & ObjectVarying)->bool {
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
      set (accessor, &accessor.globals().orders[team], Orders {unit_destination: None });
      create_object_impl (accessor, None, DeterministicRandomId::new (& (team, 0xb2e085cd02f2f8dbu64)),
        ObjectVarying {
          object_type: ObjectType::Palace,
          team: team,
          hitpoints: 100,
          trajectory: LinearTrajectory2::constant (*accessor.now(), Vector2::new (0, INITIAL_PALACE_DISTANCE*team as Coordinate*2 - INITIAL_PALACE_DISTANCE)),
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
        let position = varying.trajectory.evaluate (*accessor.now());
        let mut generator = accessor.extended_now().id.to_rng();
        for attempt in 0..9 {
          let guild = attempt < 5;
          let minimum_distance = if guild { PALACE_RADIUS + GUILD_RADIUS + 10*STRIDE } else {PALACE_DISTANCE};
          
          let target_position = position + random_vector (&mut generator, if attempt <5 {minimum_distance*(/*attempt/2 +*/ 1)} else {minimum_distance});
          
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
            
        let new = create_object (accessor, & self.object, 0x379661e69cdd5fe7,
          ObjectVarying {
            object_type: if guild {ObjectType::Guild} else {ObjectType::Palace},
            team: varying.team,
            home: Some (self.object.clone()),
            hitpoints: 20,
            trajectory: LinearTrajectory2::constant (*accessor.now(), target_position),
            .. Default::default()
          },
        );
        modify_object (accessor, & self.object, | varying | varying.dependents.push (new));
        break
          }
        }
      },
      ActionType::RecruitRanger => {
        let new = create_object (accessor, & self.object, 0x91db5029ba8b0a4e,
          ObjectVarying {
            object_type: ObjectType::Ranger,
            team: varying.team,
            home: Some (self.object.clone()),
            hitpoints: 5,
            trajectory: LinearTrajectory2::constant (*accessor.now(),varying.trajectory.evaluate (*accessor.now()) + random_vector (&mut accessor.extended_now().id.to_rng(), GUILD_RADIUS + 2*STRIDE)),
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
          let nearby = Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (position), AWARENESS_RANGE as u64), Some (& self.object));
          let closest = (varying.endurance.evaluate (*accessor.now()) < 10*SECOND).as_option().and_then (|_| varying.home.as_ref().map(|home| (home.clone(), query(accessor, &home.varying).trajectory.evaluate (*accessor.now())))).or_else(|| nearby.into_iter().filter_map (| other | {
            let other_varying = query_ref (accessor, & other.varying);
            if is_enemy (accessor, & self.object, & other) && is_building (& other_varying) {
              let other_position = other_varying.trajectory.evaluate (*accessor.now());
              let other_distance = distance (position, other_position).max();
              if other_distance <= AWARENESS_RANGE {
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
            let t = position + random_vector (&mut accessor.extended_now().id.to_rng(), AWARENESS_RANGE);
            target_location = query (accessor, & accessor.globals().orders [varying.team]).unit_destination.or (Some(t));
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
      varying.action = Some(Action {
        action_type: ActionType::Rest,
        progress: LinearTrajectory1::new (*accessor.now(), 0, 10),
        cost: 200*SECOND,
        ..Default::default()
      });
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
