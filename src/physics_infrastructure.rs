use super::*;

use nalgebra::{Vector2};
//use boolinator::Boolinator;


use time_steward::{DeterministicRandomId};
use time_steward::{PersistentTypeId, ListedType, PersistentlyIdentifiedType, DataTimelineCellTrait, QueryResult, EventHandleTrait, Basics as BasicsTrait};
pub use time_steward::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, DataHandle, DataTimelineCell, Accessor, EventAccessor, simple_timeline, bbox_collision_detection_2d as collisions};
use self::simple_timeline::{SimpleTimeline, query_ref, set};
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
  type Types = (ListedType <AchieveAction>);
  const MAX_ITERATION: u32 = 50;
}


pub trait ActionTrait: ::std::fmt::Debug {
  fn practicalities <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->ActionPracticalities;
  #[allow (unused_variables)]
  fn achieve <A: EventAccessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle) {panic!("reached undefined achieve() implementation, probably either trying to achieve an open-ended action or someone forgot to implement achieve() for something. Action: {:?}", self)}
  #[allow (unused_variables)]
  fn target_location <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->Option<Vector> {None}
}

macro_rules! define_action_types_inner_call {
  ($method:ident, $self_hack:ident, [$arg: ident, $arg2: ident], $($T: ident,)*) => {
    match $self_hack {
      $(&Action::$T(ref data) => data.$method ($arg, $arg2),)*
    }
  };
  ($method:ident, $self_hack:ident, [], $($T: ident,)*) => {
    match $self_hack {
      $(&Action::$T(ref data) => data.$method (),)*
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
  fn practicalities <A: Accessor <Steward = Steward>> (&self, accessor: &A, object: &ObjectHandle)->ActionPracticalities {
    define_action_types_inner_call! (practicalities, self, [accessor, object], $($T,)*)
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

// ####################################################
// ###### object creation/modification protocol #######
// ####################################################

pub fn create_object_impl <A: EventAccessor <Steward = Steward>>(accessor: &A, source_object: Option <& ObjectHandle>, id: DeterministicRandomId, varying: ObjectVarying)->ObjectHandle {
  let created = accessor.new_handle (Object {id: id, varying: new_timeline()});
  set (accessor, & created.varying, varying);
  Detector::insert (accessor, & get_detector (accessor), & created, source_object);
  reconsider_action (accessor, & created, false) ;
  //object_changed (accessor, & created);
  created
}

pub fn create_object <A: EventAccessor <Steward = Steward>>(accessor: &A, source_object: & ObjectHandle, unique: u64, varying: ObjectVarying)->ObjectHandle {
  create_object_impl (accessor, Some (source_object),
    DeterministicRandomId::new (& (accessor.extended_now().id, source_object.id, unique)), 
    varying)
}

pub fn destroy_object <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
  assert! (!is_destroyed (accessor, & object), "destroyed objects shouldn't be destroyed again") ;
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
    //let mut reconsider = false;
    modify_object (accessor, & home, | varying | {
      varying.dependents.retain (|a| a != object);
      //reconsider = varying.synchronous_action.is_none();
    });
    //if reconsider {reconsider_action (accessor, & home) ;}
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

pub fn modify_object <A: EventAccessor <Steward = Steward>, F: FnOnce(&mut ObjectVarying)>(accessor: &A, object: & ObjectHandle, f: F) {
  assert! (!is_destroyed (accessor, & object), "destroyed objects shouldn't be changed") ;
  let mut changed_trajectory = false;
  modify (accessor, & object.varying, |varying| {
    let old_trajectory = varying.trajectory.clone();
    (f)(varying);
    changed_trajectory = varying.trajectory != old_trajectory;
  });
  update_prediction (accessor, object);
  if changed_trajectory {Detector::changed_course (accessor, & get_detector (accessor), object);}
  //object_changed (accessor, object);
}

/*pub fn object_changed <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
  assert! (!is_destroyed (accessor, & object), "destroyed objects shouldn't be changed") ;
  
  Detector::changed_course (accessor, & get_detector (accessor), object);
}*/


pub fn objects_touching_circle <A: Accessor <Steward = Steward>>(accessor: &A, center: Vector, circle_radius: Coordinate)->Vec<ObjectHandle> {
  Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (center), circle_radius as u64), None).into_iter().filter(| object | {
    let varying = query_ref (accessor, &object.varying) ;
    let position = varying.trajectory.evaluate (*accessor.now());
    distance_less_than (center, position, circle_radius + radius (& varying))
  }).collect()
}

