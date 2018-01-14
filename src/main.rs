#[macro_use]
extern crate stdweb;
#[macro_use]
extern crate serde_derive;

use stdweb::web;

#[derive (Serialize, Deserialize, Debug)]
struct Game {
  variable: u32,
}

js_serializable!(Game);

fn main_loop (mut game: Game) {
  game.variable += 1;
  js! {
    context.beginPath();
    context.moveTo(0,0);
    context.lineTo(20,@{game.variable});
    context.strokeStyle = "rgba(0,0,0,255)";
    context.stroke();
    console.log ("whatever");
  }
  web::window().request_animation_frame (move |_| main_loop (game));
}

fn main() {
  stdweb::initialize();

  //let message = "Hello!";
  js! {
    var canvas = document.createElement ("canvas");
    document.body.appendChild (canvas);
    window.context = canvas.getContext ("2d");
      
    //alert( @{message} );
  }
  println!("uuu");

  
  let game = Game {variable: 20};
  
  web::window().request_animation_frame (move |_| main_loop (game));

  stdweb::event_loop();
}
