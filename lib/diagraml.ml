type point = float * float
type joint = Line | Bezier of {out_angle: int; in_angle: int}

type path_component = P of point | J of joint

type path = path_component list
type cyclic_path = path * joint

type color = Red of int | Green of int | Blue of int | Yellow of int | Black of int

type fill = {fill_color: color; fill_path: cyclic_path}
type stroke = {stroke_color: color; stroke_path: path}

type draw = Fill of fill | Stroke of stroke
type image = draw list

let rev_path path =
  List.map
    (function
     | P p -> P p
     | J j ->
        match j with
        | Line -> J Line
        | Bezier b -> J (Bezier {out_angle = b.in_angle; in_angle = b.out_angle}))
    (List.rev path)
  
type pc = Null | Pt | Jt
let make_path prepath =
  let rec loop path prev_component pp =
    match pp with
    | [] ->
       begin
         match prev_component with
         | Null  -> raise (Invalid_argument "path cannot be empty")
         | Pt -> path
         | Jt -> raise (Invalid_argument "a point is needed")
       end
    | c :: pp' ->
       match c with
       | P p ->
          begin
            match prev_component with
            | Null -> loop ((P p) :: path) Pt pp'
            | Pt   -> loop ((P p) :: (J Line) :: path) Pt pp'
            | Jt   -> loop ((P p) :: path) Pt pp'
          end
       | J j ->
          begin
            match prev_component with
            | Null -> raise (Invalid_argument "path must start with a point")
            | Pt   -> loop ((J j) :: path) Jt pp'
            | Jt   -> raise (Invalid_argument "")
          end
  in
  List.rev (loop [] Null prepath)

let color_to_string = function
  | Red n    -> Printf.sprintf "red!%d" n
  | Green n  -> Printf.sprintf "green!%d" n
  | Blue n   -> Printf.sprintf "blue!%d" n
  | Yellow n -> Printf.sprintf "yellow!%d" n
  | Black n  -> Printf.sprintf "black!%d" n

let point_to_string (x,y) = Printf.sprintf "(%.3f, %.3f)" x y

let path_to_string path =
  let rec loop s = function
    | [] -> s
    | P p :: path' -> loop (s ^ (point_to_string p)) path'
    | J j :: path' ->
       match j with
       | Line -> loop (s ^ " -- ") path'
       | Bezier b ->
          loop (s ^ (Printf.sprintf " to[out=%d, in=%d] " b.out_angle b.in_angle)) path'
  in
  loop "" path

let cyclic_path_to_string (path, j) =
  (path_to_string path)
  ^ begin
      match j with
      | Line -> " -- cycle"
      | Bezier _ -> assert false
    end

let fill_to_tikz f =
  Printf.sprintf "\\fill[%s]" (color_to_string f.fill_color)
  ^ (cyclic_path_to_string f.fill_path) ^ ";"

let stroke_to_tikz s =
  Printf.sprintf "\\path[draw=%s]" (color_to_string s.stroke_color)
  ^ (path_to_string s.stroke_path) ^ ";"

let to_tikz draw =
  "\\begin{tikzpicture}\n"
  ^ String.concat "\n"
      (List.map
         (function
          | Fill f   -> fill_to_tikz f
          | Stroke s -> stroke_to_tikz s)
         draw)
  ^ "\n"
  ^ "\\end{tikzpicture}\n"
