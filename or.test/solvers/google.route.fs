namespace Operations.Research.Test

module ``Google Solver - Routing`` =

  open System
  open Xunit
  open FsUnit.Xunit
  open Google.OrTools.ConstraintSolver
  open Google.Protobuf.WellKnownTypes;


  type VehicleRoute = {
      ID: string
      Route: int64 list
      Distance: int64
      Capacity: int64
    }

  [<Fact>]
  let ``capacity constraints``() =

    let distanceMatrix = array2D [
      [ 0L; 548L; 776L; 696L; 582L; 274L; 502L; 194L; 308L; 194L; 536L; 502L; 388L; 354L; 468L; 776L; 662L ];
      [ 548L; 0L; 684L; 308L; 194L; 502L; 730L; 354L; 696L; 742L; 1084L; 594L; 480L; 674L; 1016L; 868L; 1210L ];
      [ 776L; 684L; 0L; 992L; 878L; 502L; 274L; 810L; 468L; 742L; 400L; 1278L; 1164L; 1130L; 788L; 1552L; 754L ];
      [ 696L; 308L; 992L; 0L; 114L; 650L; 878L; 502L; 844L; 890L; 1232L; 514L; 628L; 822L; 1164L; 560L; 1358L ];
      [ 582L; 194L; 878L; 114L; 0L; 536L; 764L; 388L; 730L; 776L; 1118L; 400L; 514L; 708L; 1050L; 674L; 1244L ];
      [ 274L; 502L; 502L; 650L; 536L; 0L; 228L; 308L; 194L; 240L; 582L; 776L; 662L; 628L; 514L; 1050L; 708L ];
      [ 502L; 730L; 274L; 878L; 764L; 228L; 0L; 536L; 194L; 468L; 354L; 1004L; 890L; 856L; 514L; 1278L; 480L ];
      [ 194L; 354L; 810L; 502L; 388L; 308L; 536L; 0L; 342L; 388L; 730L; 468L; 354L; 320L; 662L; 742L; 856L ];
      [ 308L; 696L; 468L; 844L; 730L; 194L; 194L; 342L; 0L; 274L; 388L; 810L; 696L; 662L; 320L; 1084L; 514L ];
      [ 194L; 742L; 742L; 890L; 776L; 240L; 468L; 388L; 274L; 0L; 342L; 536L; 422L; 388L; 274L; 810L; 468L ];
      [ 536L; 1084L; 400L; 1232L; 1118L; 582L; 354L; 730L; 388L; 342L; 0L; 878L; 764L; 730L; 388L; 1152L; 354L ];
      [ 502L; 594L; 1278L; 514L; 400L; 776L; 1004L; 468L; 810L; 536L; 878L; 0L; 114L; 308L; 650L; 274L; 844L ];
      [ 388L; 480L; 1164L; 628L; 514L; 662L; 890L; 354L; 696L; 422L; 764L; 114L; 0L; 194L; 536L; 388L; 730L ];
      [ 354L; 674L; 1130L; 822L; 708L; 628L; 856L; 320L; 662L; 388L; 730L; 308L; 194L; 0L; 342L; 422L; 536L ];
      [ 468L; 1016L; 788L; 1164L; 1050L; 514L; 514L; 662L; 320L; 274L; 388L; 650L; 536L; 342L; 0L; 764L; 194L ];
      [ 776L; 868L; 1552L; 560L; 674L; 1050L; 1278L; 742L; 1084L; 810L; 1152L; 274L; 388L; 422L; 764L; 0L; 798L ];
      [ 662L; 1210L; 754L; 1358L; 1244L; 708L; 480L; 856L; 514L; 468L; 354L; 844L; 730L; 536L; 194L; 798L; 0L ]
    ]

    let demands = [0L; 1L; 1L; 2L; 4L; 2L; 4L; 8L; 8L; 1L; 2L; 1L; 2L; 4L; 4L; 8L; 8L]
    let vehicleCapacities = [15L; 15L; 15L; 15L]
    let totalVehicles = vehicleCapacities.Length
    let depot = 0


    // Create Routing Index Manager
    let manager = new RoutingIndexManager(demands.Length, totalVehicles, depot)

    // Create Routing Model.
    let routing = new RoutingModel(manager)

    // Create and register a transit callback
    let distance : LongLongToLong =
      let dist (fromIndex:int64) ( toIndex:int64): int64 =
        let fromNode = manager.IndexToNode(fromIndex)
        let toNode = manager.IndexToNode(toIndex)
        distanceMatrix.[fromNode, toNode]

      LongLongToLong(dist)

    // Define cost of each arc
    let transitCallbackIndex = routing.RegisterTransitCallback(distance)
    routing.SetArcCostEvaluatorOfAllVehicles(transitCallbackIndex)

    // Add Capacity constraint
    let demandCallbackIndex = routing.RegisterUnaryTransitCallback(fun (fromIndex:int64) -> demands.[manager.IndexToNode(fromIndex)])

    routing.AddDimensionWithVehicleCapacity(demandCallbackIndex, 0L, List.toArray vehicleCapacities, true, "Capacity") |> ignore


    // Setting first solution heuristic.
    let searchParameters = operations_research_constraint_solver.DefaultRoutingSearchParameters()
    searchParameters.FirstSolutionStrategy <- FirstSolutionStrategy.Types.Value.PathCheapestArc
    searchParameters.LocalSearchMetaheuristic <- LocalSearchMetaheuristic.Types.Value.GuidedLocalSearch

    let timeLimit = Duration()
    timeLimit.Seconds <- 1L
    searchParameters.TimeLimit <- timeLimit

    // Solve
    let solution = routing.SolveWithParameters(searchParameters)

    let routes =
      seq { 0 .. 1 .. (totalVehicles-1)}
      |> Seq.map (fun vehicle ->

        // get route
        let mutable index = routing.Start(vehicle)
        let mutable route = []
        while (routing.IsEnd(index).Equals(false)) do
          let nodeIndex = int64(manager.IndexToNode(index))
          route <- route@[nodeIndex]
          index <- solution.Value(routing.NextVar(index))

        route <- route@[route.Head]

        // get distance
        let dist = List.windowed 2 route |> List.map (fun e -> routing.GetArcCostForVehicle(e.[0], e.[1], int64(vehicle)) ) |> List.sum

        // get capacity
        let cap = route |> List.map (fun e -> demands.[int(e)]) |> List.sum

        {ID= vehicle.ToString(); Route=route; Distance=dist; Capacity=cap}
      )
      |> Seq.toList

    routes.[0].Capacity |> should equal 15L
    routes.[0].Distance |> should equal 1552L
    routes.[0].Route |> should equal [0L; 4L; 3L; 1L; 7L; 0L]

    routes.[1].Capacity |> should equal 15L
    routes.[1].Distance |> should equal 1552L
    routes.[1].Route |> should equal [0L; 14L; 16L; 10L; 9L; 0L]

    routes.[2].Capacity |> should equal 15L
    routes.[2].Distance |> should equal 1552L
    routes.[2].Route |> should equal [0L; 12L; 11L; 15L; 13L; 0L]

    routes.[3].Capacity |> should equal 15L
    routes.[3].Distance |> should equal 1552L
    routes.[3].Route |> should equal [0L; 8L; 2L; 6L; 5L; 0L]

    let totalDistance = routes |> List.map (fun f -> f.Distance) |> List.sum
    totalDistance |> should equal 6208L



