open Diagraml

let () =
  let a = P (0.0, 0.0) in
  let b = P (1.0, 0.0) in
  let c = P (1.0, 1.0) in
  let d = P (0.0, 1.0) in
  let col = Red 50 in
  let p = make_path [a; b; c; d] in
  Printf.printf
    {|\documentclass[a4paper]{article}
\usepackage{amsthm, amsmath, amssymb}
\usepackage{tikz}
\begin{document}
%s
\end{document}
|}
    (to_tikz [Fill {fill_color=col; fill_path=(p,Line)}])
    
    
