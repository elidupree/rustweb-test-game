use super::*;

use std::rc::Rc;
use std::cell::RefCell;
use std::cmp::max;

use stdweb::web;
use stdweb::unstable::TryInto;

use nalgebra::{Vector2};


use time_steward::{DeterministicRandomId};

use steward_module::{TimeSteward, ConstructibleTimeSteward, Accessor, simple_timeline};
use self::simple_timeline::{query_ref};
use steward_module::bbox_collision_detection_2d::Detector;


pub struct Game {
  pub steward: Steward,
  
  pub now: Time,
  pub last_ui_time: f64,
  pub time_speed: f64,
  
  pub display_center: Vector,
  pub display_radius: Coordinate,
  
  pub selected_object: Option <ObjectHandle>,
}

pub fn make_game(seed_id: DeterministicRandomId)->Game {
  let mut steward: Steward = Steward::from_globals (Globals::default());
  steward.insert_fiat_event (0, seed_id, Initialize {}).unwrap();
  Game {
    steward: steward,
    now: 1,
    last_ui_time: 0.0,
    time_speed: 1.0,
    display_center: Vector::new (0, 0),
    display_radius: INITIAL_PALACE_DISTANCE*3/2,
    selected_object: None,
  }
}


pub fn draw_game <A: Accessor <Steward = Steward>>(accessor: &A, game: & Game) {
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
  for object in Detector::objects_near_box (accessor, & get_detector (accessor), BoundingBox::centered (to_collision_vector (Vector::new (0, 0)), INITIAL_PALACE_DISTANCE as u64*2), None) {
    let varying = query_ref (accessor, & object.varying);
    let center = varying.trajectory.evaluate (*accessor.now());
    let center = Vector2::new (center [0] as f64, center [1] as f64);
    let object_radius = radius (& varying) as f64;
    let selected = game.selected_object.as_ref() == Some(&object);
    //println!("{:?}", (varying.trajectory, center, object_radius));
    js! {
      context.beginPath();
      context.arc (@{center [0]},@{center [1]},@{object_radius}, 0, Math.PI*2);
      context.strokeStyle = "rgba("+@{varying.team as i32*255/6}+",0,"+@{(1-varying.team as i32)*255}+",1.0)";
      context.lineWidth = @{object_radius/30.0 + if selected {1.5} else {0.5}/scale};
      context.stroke();
      if (@{varying.team == 1}) {
        context.fillStyle = "rgba(42,0,0,0.2)";
        context.fill();
      }
    }
    if let Some(action) = varying.synchronous_action.as_ref() {js! {
      context.beginPath();
      context.arc (@{center [0]},@{center [1]},@{object_radius}, 0, @{action.progress.evaluate (*accessor.now()) as f64/action.finish_cost as f64}*Math.PI*2);
      context.fillStyle = "rgba("+@{varying.team as i32*255/6}+",0,"+@{(1-varying.team as i32)*255}+",0.2)";
      context.fill();
    }}
    if varying.attack_range >0 {js! {
      context.beginPath();
      context.arc (@{center [0]},@{center [1]},@{varying.attack_range as f64}, 0, Math.PI*2);
      context.lineWidth = @{0.3/scale};
      context.stroke();
    }}
    if varying.interrupt_range >0 {js! {
      context.beginPath();
      context.arc (@{center [0]},@{center [1]},@{varying.interrupt_range as f64}, 0, Math.PI*2);
      context.lineWidth = @{0.3/scale};
      context.stroke();
    }}
    if varying.awareness_range >0 {js! {
      context.beginPath();
      context.arc (@{center [0]},@{center [1]},@{varying.awareness_range as f64}, 0, Math.PI*2);
      context.lineWidth = @{0.3/scale};
      context.stroke();
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
  if let Some(selected) = game.selected_object.as_ref() {
    let varying = query_ref (accessor, & selected.varying);
  js! {
    selected_info.empty().append ( //.text (@{format!("{:?}", **selected)});
      $("<div>").text(@{format!("{:?}", varying.object_type)}),
      $("<div>").text(@{if varying.hitpoints == 0 { format!("Food: ~{}/{}", varying.food/STANDARD_FOOD_UPKEEP_PER_SECOND,varying.food_cost/STANDARD_FOOD_UPKEEP_PER_SECOND)} else { format!("Food: ~{} ({} available)", varying.food/STANDARD_FOOD_UPKEEP_PER_SECOND, (varying.food - reserved_food(accessor, selected))/STANDARD_FOOD_UPKEEP_PER_SECOND)}}),
      $("<div>").text(@{format!("HP: {}/{}", varying.hitpoints, varying.max_hitpoints)}),
      $("<div>").text(@{format!("Endurance: {}%", varying.endurance.evaluate (*accessor.now())*100/max(1, varying.max_endurance))}),
      $("<div>").text(@{
        match varying.synchronous_action {
          None => format!("Action: {:?}", varying.synchronous_action),
          Some (ref synchronous) => match varying.ongoing_action {
            None => format!("Action: {:?}", synchronous.action_type),
            Some (ref ongoing) =>  format!("Action: {:?}/{:?}", synchronous.action_type, ongoing),
          },
        }
      })
    );}
    let choices = analyzed_choices (accessor, & selected);
    for choice in choices {
      js! {selected_info.append ($("<div>").text(@{format!("{:?}", choice)}));}
    }
    js! {selected_info.append ($("<div>").text(@{format!("{:?}", **selected)}));}
  }
  js! {
    context.restore();
  }
}

pub fn main_loop (time: f64, game: Rc<RefCell<Game>>) {
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
  
    let teams_alive: std::collections::HashSet <_> = Detector::objects_near_box (& snapshot, & get_detector (& snapshot), BoundingBox::centered (to_collision_vector (Vector::new (0, 0)), INITIAL_PALACE_DISTANCE as u64*2), None).into_iter().map (| object | query_ref (& snapshot, & object.varying).team).collect();
    continue_simulating = teams_alive.len() > 1;
  }
  if continue_simulating {
    web::window().request_animation_frame (move | time | main_loop (time, game));
  }
}


#[cfg (target_os = "emscripten")]
pub fn run() {
  stdweb::initialize();
  js! {
    var game_container = window.game_container = $("<div>");
    var canvas = window.canvas = document.createElement ("canvas");
    (document.querySelector("main") || document.body).appendChild (game_container[0]);
    game_container.append(canvas);
    window.context = canvas.getContext ("2d");
  }
  
  let seed: u32 = js!{return window.localStorage && parseInt(window.localStorage.getItem ("random_seed")) || 0}.try_into().unwrap();
  
  let game = Rc::new (RefCell::new (make_game(DeterministicRandomId::new (& (seed, 0xae06fcf3129d0685u64)))));
  
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
  
  
  {
    let game = game.clone();
    let click_callback = move |x: f64,y: f64 | {
      let mut game = game.borrow_mut();
      let offset = Vector2::new (
        x*game.display_radius as f64*2.0,
        y*game.display_radius as f64*2.0
      );
      let location = game.display_center + Vector2::new (offset [0] as Coordinate, offset [1] as Coordinate);
      let now = game.now;
      
      //game.steward.insert_fiat_event (now, DeterministicRandomId::new (& (now)), ChangeOrders {team: 1, orders: Orders {unit_destination: Some (location)}}).unwrap();
      
      let snapshot = game.steward.snapshot_before (&now). unwrap ();
      for object in Detector::objects_near_box (& snapshot, & get_detector (& snapshot), BoundingBox::centered (to_collision_vector (location), 0), None) {
        let varying = query_ref (& snapshot, & object.varying);
        let center = varying.trajectory.evaluate (now);
        let object_radius = radius (& varying);
        if distance (location, center).max() < object_radius {
          game.selected_object = Some (object.clone());
        }
      }
    };
    js! {
      var callback = @{click_callback};
      canvas.addEventListener ("click", function (event) {
        var offset = canvas.getBoundingClientRect();
        callback (
          (event.clientX - offset.left)/offset.width - 0.5,
          (event.clientY - offset.top)/offset.height - 0.5
        );
        event.preventDefault();
      });
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
    game_container.append(window.selected_info = $("<div>", {id: "selected_info"}));
  }
  
  web::window().request_animation_frame (move | time | main_loop (time, game));

  stdweb::event_loop();
}


#[cfg (not(target_os = "emscripten"))]
pub fn run() {
  let mut scores = [0; 2];
  
  loop {
  
  
  let mut game = make_game(DeterministicRandomId::new (& (scores, 0xae06fcf3129d0685u64)));
  
  loop {
    game.now += SECOND /100;
    let snapshot = game.steward.snapshot_before (& game.now). unwrap ();
    game.steward.forget_before (& game.now);
  
    let teams_alive: std::collections::HashSet <_> = Detector::objects_near_box (& snapshot, & get_detector (& snapshot), BoundingBox::centered (to_collision_vector (Vector::new (0, 0)), INITIAL_PALACE_DISTANCE as u64*2), None).into_iter().filter (| object | is_building (&query_ref (& snapshot, & object.varying))).map (| object | query_ref (& snapshot, & object.varying).team).collect();
    if teams_alive.len() <= 1 {
      scores [teams_alive.into_iter().next().unwrap()] += 1;
      println!("{:?}", scores);
      break;
    }
  }
  }
}

