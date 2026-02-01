open Raylib

let win_height = 900
let win_width = 900

let xRule = "F-[[X]+X]+F[+FX]-X"
let fRule = "FF"
let axiom = "X"
let angle = 40.7 *. Float.pi /. 180. (* radians *)
let branch_len = 200.
let maxIterations = 8

type state = {
  branch_len: float;
  sentence : string;
  iterationCount : int;
}

type turtle = {
  x : float;
  y : float;
  angle : float;
}


let generate_l (s : string) : string = 
  let explode = s |> String.to_seq |> List.of_seq in
  List.fold_left 
  (fun acc c ->
    match c with
    | 'F' -> acc ^ fRule
    | 'X' -> acc ^ xRule
    | _ -> acc ^ String.make 1 c
  )
  ""
  explode


let turtle_draw (state : state) =

  let starting_turtle = {
    x = float_of_int win_width /. 2.;
    y = float_of_int win_height;
    angle = -.Float.pi /. 2.;
  } in 

  let chars = state.sentence |> String.to_seq |> List.of_seq in

  let rec run turtle stack chars = 
    match chars with 
    | [] -> ()
    | c :: rest ->
        match c with 
        | 'F' ->
            let x' = turtle.x +. cos turtle.angle *. state.branch_len
            in
            let y' = turtle.y +. sin turtle.angle *. state.branch_len
            in

            draw_line_ex (Vector2.create turtle.x turtle.y) (Vector2.create x' y')
            1.2
            Color.green;

            let turtle' = { turtle with x = x'; y = y' } in 
            run turtle' stack rest

        | '+' ->
            run { turtle with angle = turtle.angle +. angle } stack rest

        | '-' -> 
            run { turtle with angle = turtle.angle -. angle } stack rest

        | '[' ->
            run turtle (turtle :: stack) rest

        | ']' ->
            (match stack with 
            | t :: stack' -> run t stack' rest
            | [] -> run turtle stack rest)

        | _ -> run turtle stack rest
  in
  
  run starting_turtle [] chars


let () =
  set_config_flags [ ConfigFlags.Msaa_4x_hint ];
  init_window win_width win_height "L-system (Processing-style)";
  set_target_fps 60;

  let state = ref {
    branch_len = branch_len;
    sentence = axiom;
    iterationCount = 0;
  } in

  let frame = ref 0 in

  while not (window_should_close()) do 
    incr frame;

    begin_drawing();
    clear_background Color.black;

    turtle_draw !state;

    end_drawing();

    if !frame mod 60 = 0 && (!state).iterationCount < maxIterations then
      state := {
        branch_len = (!state).branch_len *. 0.5;
        sentence = generate_l (!state).sentence;
        iterationCount = (!state).iterationCount +1;
  }
  done;

  close_window()



