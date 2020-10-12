open Diagraml

let () =
  let a = P (0.0, 0.0) in
  let b = P (1.0, 0.0) in
  let c = P (1.0, 1.0) in
  let d = P (0.0, 1.0) in
  let col = Red 50 in
  let p = make_path [a; J (Bezier {out_angle = (-30); in_angle = (-150)}); b; c; d] in
  Printf.printf
    {|\documentclass[a4paper]{article}
\usepackage{amsthm, amsmath, amssymb}
\usepackage{tikz}
\begin{document}
%s
\end{document}
|}
    (to_tikz [Fill {fill_color=col; fill_path=(p,Line)};
              Stroke {stroke_color=Blue 100; stroke_path=p}])
