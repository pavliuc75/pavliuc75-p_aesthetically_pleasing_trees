// Lasse Andresen, Nicolai Pavliuc, Shadman Al Islam     13-06-2024

namespace TreeLib

open FsCheck

// Representing trees
type Tree<'a> = Node of 'a * Tree<'a> list

// Representing extents
type Extent = (float * float) list

module Tree =
    // Move
    let movetree (Node ((label, x), subtrees), x': float) = Node((label, x + x'), subtrees)

    let moveextent (e: Extent, x) =
        List.map (fun (p, q) -> (p + x, q + x)) e

    // Merge
    let rec merge ps qs =
        match (ps, qs) with
        | ([], qs) -> qs
        | (ps, []) -> ps
        | ((p, _) :: ptail, (_, q) :: qtail) -> (p, q) :: (merge ptail qtail)

    let mergelist (es: Extent list) = List.fold merge [] es

    // Fitting extents
    let rmax (p: float, q: float) = if p > q then p else q

    let rec fit (ps: Extent, qs: Extent) =
        match (ps, qs) with
        | ((_, p) :: ptail, ((q, _) :: qtail)) -> rmax (fit (ptail, qtail), p - q + 1.0) //todo handle nan -inf inf
        | (_, _) -> 0.0

    let fitlistl es =
        let rec fitlistl' acc es' =
            match (acc, es') with
            | (_, []) -> []
            | (acc, (e' :: etail)) ->
                let x = fit (acc, e')

                x
                :: fitlistl' (merge acc (moveextent (e', x))) etail

        fitlistl' [] es

    let fitlistr es =
        let rec fitlistr' acc es' =
            match (acc, es') with
            | (_, []) -> []
            | (acc, (e' :: etail)) ->
                let x = -1.0 * fit (e', acc)

                x
                :: fitlistr' (merge (moveextent (e', x)) acc) etail

        List.rev (fitlistr' [] (List.rev es))

    // Symmetric layout
    let mean (x: float, y: float) = (x + y) / 2.0

    let fitlist es =
        List.map mean (List.zip (fitlistl es) (fitlistr es))

    // Designing the tree
    let design tree =
        let rec design' (Node (label, subtrees)) =
            let (trees, extents) = List.unzip (List.map design' subtrees)
            let positions = fitlist extents
            let ptrees = List.map movetree (List.zip trees positions)
            let pextents = List.map moveextent (List.zip extents positions)
            let resultextent = (0.0, 0.0) :: mergelist pextents
            let resulttree = Node((label, 0.0), ptrees)
            (resulttree, resultextent)

        design' (tree)

module TreeWithLabelWidthsConsidered =
    let rec maxLabelWidth (Node ((label), children)) =
        let labelWidth = String.length label
        let mappedCh = List.map (fun x -> maxLabelWidth x) children
        let maxChildWidth = if List.length mappedCh = 0 then 0 else List.max mappedCh
        
        max labelWidth maxChildWidth

    let rec fit (ps: Extent, qs: Extent, labelWidth: float) =
        match (ps, qs) with
        | ((_, p) :: ptail, (q, _) :: qtail) ->
            let diff = p - q + labelWidth + 1.0
            let diff' =
                match System.Double.IsNaN(diff), System.Double.IsNegativeInfinity(diff), System.Double.IsPositiveInfinity(diff) with
                | true, _, _ 
                | _, true, _ 
                | _, _, true -> 0.0
                | _ -> diff
            Tree.rmax (fit (ptail, qtail, labelWidth), diff')
        | (_, _) -> 0.0

    let fitlistl (es: Extent list, labelWidth: float) =
        let rec fitlistl' acc es' =
            match es' with
            | [] -> []
            | e' :: etail ->
                let x = fit (acc, e', labelWidth)
                x :: fitlistl' (Tree.merge acc (Tree.moveextent (e', x))) etail

        fitlistl' [] es

    let fitlistr (es: Extent list, labelWidth: float) =
        let rec fitlistr' acc es' =
            match es' with
            | [] -> []
            | e' :: etail ->
                let x = -1.0 * fit (e', acc, labelWidth)
                x :: fitlistr' (Tree.merge (Tree.moveextent (e', x)) acc) etail

        List.rev (fitlistr' [] (List.rev es))

    let fitlist (es: Extent list, labelWidth: float) =
        List.map Tree.mean (List.zip (fitlistl (es, labelWidth)) (fitlistr (es, labelWidth)))

    let design tree =
        let labelWidth = float (maxLabelWidth tree)
        let rec design' (Node (label, subtrees)) =
            let (trees, extents) = List.unzip (List.map design' subtrees)
            let positions = fitlist (extents, labelWidth)
            let ptrees = List.map Tree.movetree (List.zip trees positions)
            let pextents = List.map Tree.moveextent (List.zip extents positions)
            let resultextent = (0.0, 0.0) :: Tree.mergelist pextents
            let resulttree = Node((label, 0.0), ptrees)
            (resulttree, resultextent)

        design' tree


module TreeGenerator =
    let rec arbTree<'a> (genA: Gen<'a>) =
        gen {
            let! value = genA
            let! subtrees = Gen.listOf (arbTree genA)
            return Node(value, subtrees)
        }

    let genTree<'a> (genA: Gen<'a>) : Gen<Tree<'a>> = arbTree genA

    type Generators =
        static member Tree() =
            Arb.fromGen (genTree (Arb.generate<'a>))
