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
  let j = Coordinate ("J", (1.0, top)) in
  let e = Coordinate ("E", (3.0, top)) in
  let eta = Coordinate ("eta", (2.0, 1.0)) in
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
    ]
  in
  Printf.printf
    {|\documentclass[tikz]{standalone}
\begin{document}
%s
\end{document}
|}
  @@ to_tikz d
