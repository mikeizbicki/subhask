\documentclass[tikz]{standalone}
\usepackage{color}

\definecolor{cat}{rgb}{1,1,0.5}
\definecolor{cont}{rgb}{1,0.75,0.5}
\definecolor{comp}{rgb}{1,0.8,0.8}
\definecolor{num}{rgb}{0.8,0.8,1}
\definecolor{vec}{rgb}{1,0.8,1}
\definecolor{monad}{rgb}{0.9,1,0.7}

\begin{document}
\ttfamily
\begin{tikzpicture}
    [ level distance=1.1in
    , sibling distance=0.4in
    , grow=right
    , every path/.style=
        { very thick
        , draw=black!70
        , <-
        , >=latex
        }
    , edge from parent/.style=
        { latex-
        , very thick
        , draw=black!70
        }
    , every node/.style=
        { draw
        , very thick
        , shape=rectangle
        , minimum width=0.95in
        , minimum height=0.25in
        , rounded corners=0.05in
        }
    , hidden/.style =
        { edge from parent/.style={draw=none}
        }
    , monad/.style =
        { fill=monad
        }
    , cat/.style =
        { fill=cat
        }
    , cont/.style =
        { fill=cont
        }
    , comp/.style =
        { fill=comp
        }
    , vec/.style =
        { fill=num
        }
    , num/.style =
        { fill=num
        }
    ]
