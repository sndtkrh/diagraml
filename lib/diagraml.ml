type point = float * float
type joint = Line | Bezier of {out_angle: int; in_angle: int}

type coordinate = string * point

type path_component = Point of point | Coordinate of coordinate | Joint of joint

type path = path_component list
type cyclic_path = path * joint

type color = Red of int | Green of int | Blue of int | Yellow of int | Black of int

type fill = {fill_color: color; fill_path: cyclic_path}
type stroke = {stroke_color: color; stroke_path: path}
type node = {node_at: coordinate; node_dir: string; node_label:string}

type draw = Fill of fill | Stroke of stroke | Node of node
type image = draw list

let rev_path path =
  List.map
    (function
     | Point p -> Point p
     | Coordinate c -> Coordinate c
     | Joint j ->
        match j with
        | Line -> Joint Line
        | Bezier b -> Joint (Bezier {out_angle = b.in_angle; in_angle = b.out_angle}))
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
       | Point _ as p ->
          begin
            match prev_component with
            | Null -> loop (p :: path) Pt pp'
            | Pt   -> loop (p :: (Joint Line) :: path) Pt pp'
            | Jt   -> loop (p :: path) Pt pp'
          end
       | Coordinate _ as c->
          begin
            match prev_component with
            | Null -> loop (c :: path) Pt pp'
            | Pt   -> loop (c :: (Joint Line) :: path) Pt pp'
            | Jt   -> loop (c :: path) Pt pp'
          end
       | Joint _ as j->
          begin
            match prev_component with
            | Null -> raise (Invalid_argument "path must start with a point")
            | Pt   -> loop (j :: path) Jt pp'
            | Jt   -> raise (Invalid_argument "")
          end
  in
  List.rev (loop [] Null prepath)

let concat_path p q =
  let coord = function
    | Point p -> p
    | Coordinate (_, p) -> p
    | _ -> assert false
  in
  let ps = List.hd p in
  let pt = List.hd (rev_path p) in
  let qs = List.hd q in
  let qt = List.hd (rev_path q) in
  if coord pt = coord qs
  then List.concat [p; List.tl q]
  else
    if coord pt = coord qt
    then List.concat [p; List.tl (rev_path q)]
    else
      if coord ps = coord qs
      then List.concat [rev_path p; List.tl q]
      else
        if coord ps = coord qt
        then List.concat [rev_path p; List.tl (rev_path q)]
        else raise (Invalid_argument "cannot concat paths")

let ( -*- ) = concat_path

let ( ** ) p q =
  match (p, q) with
  | (Joint _, _) -> raise (Invalid_argument "++ filed")
  | (_, Joint _) -> raise (Invalid_argument "++ filed")
  | _ -> [p; Joint Line; q]

let ( *- ) p path =
  match p with
  | Joint _ -> raise (Invalid_argument "++ filed")
  | _ -> p :: (Joint Line) :: path

let ( -* ) path p =
  match p with
  | Joint _ -> raise (Invalid_argument "++ filed")
  | _ -> rev_path (p *- rev_path path)

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
    | Point p :: path' -> loop (s ^ (point_to_string p)) path'
    | Coordinate (name, _) :: path' -> loop (s ^ "(" ^ name ^ ")") path'
    | Joint j :: path' ->
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

let node_to_tikz n =
  let (name, _) = n.node_at in
  Printf.sprintf "\\node[%s] at (%s) {%s};" n.node_dir name n.node_label

module M = Map.Make(String)
let coordinate_to_tikz' m draw =
  let path = match draw with
    | Fill f -> let (p, _) = f.fill_path in p
    | Stroke s -> s.stroke_path
    | Node n -> [Coordinate n.node_at]
  in
  let f c (m, l) =
    match c with
    | Coordinate (name, p) ->
       begin
         match M.find_opt name m with
         | None ->
            let m' = M.add name p m in
            let s =
              Printf.sprintf "\\coordinate (%s) at %s;" name (point_to_string p)
            in
            (m', s :: l)
         | Some q ->
            if p = q
            then (m, l)
            else assert false
       end
    | _ -> assert false
  in
  List.fold_right
    f
    (List.filter (function Coordinate _ -> true | _ -> false) path)
    (m, [])

let coordinate_to_tikz image =
  let f draw (m, ls) =
    let (m', l) = coordinate_to_tikz' m draw in
    (m', l :: ls)
  in
  let (_, ls) = List.fold_right f image (M.empty, []) in
  String.concat "\n" (List.concat ls)
  
let to_tikz image =
  "\\begin{tikzpicture}\n"
  ^ coordinate_to_tikz image ^ "\n"
  ^ String.concat "\n"
      (List.map
         (function
          | Fill f   -> fill_to_tikz f
          | Stroke s -> stroke_to_tikz s
          | Node n -> node_to_tikz n)
         image)
  ^ "\n"
  ^ "\\end{tikzpicture}\n"
