namespace Operations.Research.Test

open System
open TestTracks
open Google.OrTools.ConstraintSolver
open Google.Protobuf.WellKnownTypes

module GoogleSolverRouting =

    type private VehicleRoute = {
        ID:       string
        Route:    int64 list
        Distance: int64
        Capacity: int64
    }

    let tests = suite "Google Solver - Routing" [

        (*
            Capacitated Vehicle Routing (CVRP).

            A fleet of four vehicles, each with capacity 15, must serve seventeen
            stops with known demands and pairwise distances, all returning to the
            depot (node 0). Find routes that satisfy demand and minimize total
            distance.

            This bypasses our Model layer entirely — OR-Tools' routing API uses
            distance/demand callbacks rather than a flat MIP, so the test goes
            straight to RoutingModel. Bundled as a reference.

            See: https://developers.google.com/optimization/routing/cvrp
        *)
        test "capacity constraints" (fun () ->
            let distanceMatrix = array2D [
                [   0L; 548L; 776L; 696L; 582L; 274L; 502L; 194L; 308L; 194L; 536L; 502L; 388L; 354L; 468L; 776L; 662L]
                [ 548L;   0L; 684L; 308L; 194L; 502L; 730L; 354L; 696L; 742L;1084L; 594L; 480L; 674L;1016L; 868L;1210L]
                [ 776L; 684L;   0L; 992L; 878L; 502L; 274L; 810L; 468L; 742L; 400L;1278L;1164L;1130L; 788L;1552L; 754L]
                [ 696L; 308L; 992L;   0L; 114L; 650L; 878L; 502L; 844L; 890L;1232L; 514L; 628L; 822L;1164L; 560L;1358L]
                [ 582L; 194L; 878L; 114L;   0L; 536L; 764L; 388L; 730L; 776L;1118L; 400L; 514L; 708L;1050L; 674L;1244L]
                [ 274L; 502L; 502L; 650L; 536L;   0L; 228L; 308L; 194L; 240L; 582L; 776L; 662L; 628L; 514L;1050L; 708L]
                [ 502L; 730L; 274L; 878L; 764L; 228L;   0L; 536L; 194L; 468L; 354L;1004L; 890L; 856L; 514L;1278L; 480L]
                [ 194L; 354L; 810L; 502L; 388L; 308L; 536L;   0L; 342L; 388L; 730L; 468L; 354L; 320L; 662L; 742L; 856L]
                [ 308L; 696L; 468L; 844L; 730L; 194L; 194L; 342L;   0L; 274L; 388L; 810L; 696L; 662L; 320L;1084L; 514L]
                [ 194L; 742L; 742L; 890L; 776L; 240L; 468L; 388L; 274L;   0L; 342L; 536L; 422L; 388L; 274L; 810L; 468L]
                [ 536L;1084L; 400L;1232L;1118L; 582L; 354L; 730L; 388L; 342L;   0L; 878L; 764L; 730L; 388L;1152L; 354L]
                [ 502L; 594L;1278L; 514L; 400L; 776L;1004L; 468L; 810L; 536L; 878L;   0L; 114L; 308L; 650L; 274L; 844L]
                [ 388L; 480L;1164L; 628L; 514L; 662L; 890L; 354L; 696L; 422L; 764L; 114L;   0L; 194L; 536L; 388L; 730L]
                [ 354L; 674L;1130L; 822L; 708L; 628L; 856L; 320L; 662L; 388L; 730L; 308L; 194L;   0L; 342L; 422L; 536L]
                [ 468L;1016L; 788L;1164L;1050L; 514L; 514L; 662L; 320L; 274L; 388L; 650L; 536L; 342L;   0L; 764L; 194L]
                [ 776L; 868L;1552L; 560L; 674L;1050L;1278L; 742L;1084L; 810L;1152L; 274L; 388L; 422L; 764L;   0L; 798L]
                [ 662L;1210L; 754L;1358L;1244L; 708L; 480L; 856L; 514L; 468L; 354L; 844L; 730L; 536L; 194L; 798L;   0L]
            ]

            let demands = [0L; 1L; 1L; 2L; 4L; 2L; 4L; 8L; 8L; 1L; 2L; 1L; 2L; 4L; 4L; 8L; 8L]
            let vehicleCapacities = [15L; 15L; 15L; 15L]
            let totalVehicles = vehicleCapacities.Length
            let depot = 0

            let manager = new RoutingIndexManager(demands.Length, totalVehicles, depot)
            let routing = new RoutingModel(manager)

            let distance : LongLongToLong =
                let dist (fromIndex: int64) (toIndex: int64) : int64 =
                    let fromNode = manager.IndexToNode(fromIndex)
                    let toNode   = manager.IndexToNode(toIndex)
                    distanceMatrix.[fromNode, toNode]
                LongLongToLong(dist)

            let transitCallbackIndex = routing.RegisterTransitCallback(distance)
            routing.SetArcCostEvaluatorOfAllVehicles(transitCallbackIndex)

            let demandCallbackIndex =
                routing.RegisterUnaryTransitCallback(fun (fromIndex: int64) ->
                    demands.[manager.IndexToNode(fromIndex)])

            routing.AddDimensionWithVehicleCapacity(
                demandCallbackIndex,
                0L,
                List.toArray vehicleCapacities,
                true,
                "Capacity") |> ignore

            let searchParameters = operations_research_constraint_solver.DefaultRoutingSearchParameters()
            searchParameters.FirstSolutionStrategy <- FirstSolutionStrategy.Types.Value.PathCheapestArc
            searchParameters.LocalSearchMetaheuristic <- LocalSearchMetaheuristic.Types.Value.GuidedLocalSearch

            let timeLimit = Duration()
            timeLimit.Seconds <- 1L
            searchParameters.TimeLimit <- timeLimit

            let solution = routing.SolveWithParameters(searchParameters)

            let routes =
                [ 0 .. totalVehicles - 1 ]
                |> List.map (fun vehicle ->
                    let mutable index = routing.Start(vehicle)
                    let mutable route = []
                    while not (routing.IsEnd(index)) do
                        let nodeIndex = int64 (manager.IndexToNode(index))
                        route <- route @ [nodeIndex]
                        index <- solution.Value(routing.NextVar(index))
                    route <- route @ [route.Head]
                    let dist =
                        List.windowed 2 route
                        |> List.map (fun e -> routing.GetArcCostForVehicle(e.[0], e.[1], int64 vehicle))
                        |> List.sum
                    let cap =
                        route
                        |> List.map (fun e -> demands.[int e])
                        |> List.sum
                    { ID = string vehicle; Route = route; Distance = dist; Capacity = cap })

            let totalDistance = routes |> List.map (fun r -> r.Distance) |> List.sum

            assertEqual 15L                      routes.[0].Capacity "vehicle 0 capacity"
            |> combine (assertEqual 1552L                      routes.[0].Distance "vehicle 0 distance")
            |> combine (assertEqual [0L; 7L; 3L; 4L; 1L; 0L]   routes.[0].Route    "vehicle 0 route")
            |> combine (assertEqual 15L                       routes.[1].Capacity "vehicle 1 capacity")
            |> combine (assertEqual 1552L                      routes.[1].Distance "vehicle 1 distance")
            |> combine (assertEqual [0L; 14L; 16L; 10L; 9L; 0L] routes.[1].Route   "vehicle 1 route")
            |> combine (assertEqual 15L                       routes.[2].Capacity "vehicle 2 capacity")
            |> combine (assertEqual 1552L                      routes.[2].Distance "vehicle 2 distance")
            |> combine (assertEqual [0L; 12L; 11L; 15L; 13L; 0L] routes.[2].Route  "vehicle 2 route")
            |> combine (assertEqual 15L                       routes.[3].Capacity "vehicle 3 capacity")
            |> combine (assertEqual 1552L                      routes.[3].Distance "vehicle 3 distance")
            |> combine (assertEqual [0L; 8L; 2L; 6L; 5L; 0L]  routes.[3].Route    "vehicle 3 route")
            |> combine (assertEqual 6208L totalDistance "total distance across all vehicles")
        )
    ]
    