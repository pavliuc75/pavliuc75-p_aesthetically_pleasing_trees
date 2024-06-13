// Lasse Andresen, Nicolai Pavliuc, Shadman Al Islam     13-06-2024

namespace TreeVisualizationLib

open TreeLib
open Plotly.NET

module VisualizeTree =
    let plot (Node ((l, x: float), tree)) =
        let rec getPlots isroot parentx parenty subtree =
            match subtree with
            | [] -> []
            | (Node ((l', x'), subtree')) :: rest ->
                [ Chart.Line(
                      x = [ parentx; (parentx + x') ],
                      y = [ parenty; parenty - 1.0 ], // - here because we build the tree downwards starting with y=0 (root). Children have negative y's
                      MultiText =
                          [ (if isroot then (sprintf "%A" l) else "")
                            (sprintf "%A" l') ],
                      TextPosition = StyleParam.TextPosition.TopRight,
                      ShowMarkers = true
                  ) ]
                @ (getPlots false (parentx + x') (parenty - 1.0) subtree')
                  @ (getPlots false parentx parenty rest)

        Defaults.DefaultHeight <- 1000
        Defaults.DefaultWidth <- 1000

        let plots = getPlots true x 0.0 tree

        plots |> Chart.combine |> Chart.show

    let plotWithScale (scale: float) (Node ((l, x: float), tree)) =
        //we added scale param here and not to design func, because design func only has x coord, but we need to scale on y too
        let rec getPlots isroot parentx parenty subtree =
                match subtree with
                | [] -> []
                | (Node ((l', x'), subtree')) :: rest ->
                    [ Chart.Line(
                          x = [ parentx; (parentx + x' * scale) ],
                          y = [ parenty; parenty - scale ],
                          MultiText =
                              [ (if isroot then (sprintf "%A" l) else "")
                                (sprintf "%A" l') ],
                          TextPosition = StyleParam.TextPosition.TopRight,
                          ShowMarkers = true
                      ) ]
                    @ (getPlots false (parentx + x' * scale) (parenty - scale) subtree')
                      @ (getPlots false parentx parenty rest)

        Defaults.DefaultHeight <- 1000
        Defaults.DefaultWidth <- 1000

        let plots = getPlots true (x * scale) 0.0 tree

        plots |> Chart.combine |> Chart.show