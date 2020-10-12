open Diagraml

let () =
  let top = 2.0 in
  let bottom = 0.0 in
  let left = 0.0 in
  let right = 4.0 in
  let lb = Point (left, bottom) in
  let lt = Point (left, top) in
  let rb = Point (right, bottom) in
  let rt = Point (right, top) in
  let j' = ("J", (1.0, top)) in
  let e' = ("E", (3.0, top)) in
  let eta' = ("eta", (2.0, 1.0)) in
  let j = Coordinate j' in
  let e = Coordinate e' in
  let eta = Coordinate eta' in
  let eta__j =
    make_path [
        eta;
        Joint (Bezier {out_angle=180; in_angle=(-90)});
        j
      ]
  in
  let eta__e =
    make_path [
        eta;
        Joint (Bezier {out_angle=0; in_angle=(-90)});
        e
      ]
  in
  let d =
    [
      Fill {fill_color = Blue 50;
            fill_path = (lb ** lt -* j -*- eta__j -*- eta__e -* rt -* rb, Line)};
      Fill {fill_color = Red 50; fill_path = (eta__e -*- eta__j, Line)};
      Stroke {stroke_color = Black 100; stroke_path = (eta__e -*- eta__j)};
      Node {node_at = eta'; node_dir = "below"; node_label = "$\\eta$"};
      Node {node_at = j'; node_dir = "above"; node_label = "$J$"};
      Node {node_at = e'; node_dir = "above"; node_label = "$E$"};
    ]
  in
  Printf.printf
    {|\documentclass[tikz]{standalone}
\begin{document}
%s
\end{document}
|}
  @@ to_tikz d
