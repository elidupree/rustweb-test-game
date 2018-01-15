#[macro_use]
extern crate stdweb;
#[macro_use]
extern crate serde_derive;
extern crate nalgebra;
extern crate array_ext;

use stdweb::web;
use array_ext::*;
use nalgebra::Vector2;






#[derive (Serialize, Deserialize, Debug)]
struct Globals {
  detector: Timeline <DataHandle <Detector>>,
}

#[derive (Clone, Serialize, Deserialize, Debug)]
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

#[derive (Clone, Serialize, Deserialize, Debug)]
struct ObjectVarying {
  object_type: ObjectType,
  trajectory: LinearTrajectory2,
  detector_data: DetectorData,
  prediction: Option <EventHandle>,
  destroyed: bool,
}

fn create_object <A: EventAccessor <Steward = Steward>>(accessor: &A, source_object: & ObjectHandle, unique: u64, trajectory: LinearTrajectory2, object_type: ObjectType) {
  let created = accessor::new_handle (Object {id: DeterministicRandomId::new (& (accessor.extended_now().id, source_object.id, unique)), varying: Timeline::new()});
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


fn object_changed <A: EventAccessor <Steward = Steward>>(accessor: &A, object: &ObjectHandle) {
  let id = DeterministicRandomId::new (& (0x93562b6a9bcdca8cu64, accessor.extended_now().id, object.id));
  modify (accessor, & object.varying, | varying | {
    let iterator = iter::empty().chain (
      match varying.object_type {
        Palace (palace) => {
          iter::once (accessor.create_prediction (palace.gold.when_reaches (100), id, BuildGuild {palace: object})
        },
        Guild (guild) {
          iter::once (accessor.create_prediction (guild.gold.when_reaches (100), id, RecruitRanger {guild: object})
        },
        Ranger (ranger) => {
          iter::once (accessor.create_prediction (ranger.thoughts.when_reaches (100), id, Think {ranger: object})
        },
      }
    );
    
    varying.prediction = iter.min_by_key (| prediction | prediction.extended_time());
  });
  Detector::changed_position (accessor, get_detector (accessor), object);
}


define_event! {
  struct BuildGuild {palace: ObjectHandle},
  ,
  fn execute (&self, accessor) {
    modify_object (accessor, & self.palace, | varying | {
      varying.gold.add (accessor.now(), - 100) ;
    });
    let varying = query (accessor, palace.varying) ;
    create_object (accessor, & self.palace, 0x379661e69cdd5fe7,
      LinearTrajectory2::constant (accessor.now(), varying.trajectory.evaluate (accessor.now()) + random vector (10 strides)),
      ObjectType::Guild (Guild {gold: LinearTrajectory::new (0, TODO)}),
    );
  }
}



define_event! {
  struct Think {ranger: ObjectHandle},
  ,
  fn execute (&self, accessor) {
    let attack = None;
    modify_object (accessor, & self.ranger, | varying | {
      varying.thoughts.add (accessor.now(), - 100) ;
      for object in Detector::objects_near_box (accessor, get_detector (accessor), BoundingBox::centered (varying.trajectory.evaluate (accessor.now()), RANGER_RANGE), & self.ranger) {
        if is_enemy (accessor, & self.ranger, & object) {
          if distance <RANGER_RANGE {
            attack = object.clone();
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

#[derive (Serialize, Deserialize, Debug)]
struct Game {
  tiles: [[Tile; 10]; 10],
}

js_serializable!(Game);

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

fn main_loop (mut game: Game) {
  //game.variable += 1;
  js! {
    context.clearRect (0, 0, canvas.width, canvas.height);
  }
  for (first, whatever) in game.tiles.iter_mut().enumerate() {
    for (second, tile) in whatever.iter_mut().enumerate() {
      draw_tile (Vector2::new (first, second), tile);
      tile.variable = (tile.variable + first as u32) % 23;
    }
  }
  web::window().request_animation_frame (move |_| main_loop (game));
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
  
  web::window().request_animation_frame (move |_| main_loop (game));

  stdweb::event_loop();
}
