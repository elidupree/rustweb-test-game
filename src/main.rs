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
