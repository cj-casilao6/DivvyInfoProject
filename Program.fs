//  CJ Casilao - 678596465 - ccasi4
//
//  Given a CSV file which contains a list of different values which denotes different aspects of a Divvy trip, 
//  create functions that return various information that takes specific elements from the list:
// 
//  (Num of trips, num & percent of both male and female riders, average age, ride durations, and histogram of start times)


// --------------------------------------------------------------
// 
// ParseLine and ParseInput
//
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists. Each
// sub-list denotes one bike ride.
// 
// Example:
//   [ [21864678; 6035; 3000; 23; 317; 338; 1; 1; 1974]; ... ]
//
// The values are:
//    trip_id
//    bike_id
//    trip_duration   (seconds)
//    starting_hour   (between 0 and 23)
//    from_station_id
//    to_station_id
//    is_subscriber   (1 if yes, 0 if no)
//    gender          (0 if not specified, 1 if identifies as male, 2 if identifies as female)
//    birth_year      (0 if not specified)
// 
let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides


// --------------------------------------------------------------
// main function
// 

[<EntryPoint>]
let main argv =

  printfn "Project 3: Divvy Rides Data Analysis with F#"
  printfn "CS 341, Spring 2025"
  printfn ""
  printfn "This application allows you to analyze and visualize"
  printfn "information about Divvy bike rides in Chicago, such as"
  printfn "the number of male/female riders, the average age, etc."
  printfn ""

  printf "Enter the name of the file with the Divvy ride data: "
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents


  // --------------------------------------------------------------
  // NUMBER OF TRIPS
  //

  // # of trips function definition
  let numTrips ridedata =
    List.length ridedata    // Use .length with # of parsed input

  // Print # of trips
  printfn ""
  printfn "Number of Trips: %A" (numTrips ridedata)   // Call numTrips function with ridedata as parameter
  printfn ""
  

  // --------------------------------------------------------------
  //  MALE RIDERS (NUMBER & PERCENTAGE)
  // 

  // # of male riders function definition (Index 7 in list -> Value 1 = male)
  let maleRiders ridedata =
    let maleFilter = 
      List.filter (fun e ->                                 // Go through each element in list
        match e with
        | [ _; _; _; _; _; _; _; gender; _ ] -> gender = 1  // Ignore everything in list until gender index. If gender = 1; then male
        | _ -> false                                        
      ) ridedata
    List.length maleFilter                                  // Based on result of above list, return length (num of males)

  // Calculate male riders percentage (with respect to maleRiders and numTrips functions)
  let malePercent ridedata =
    let total = float (numTrips ridedata)                   // Type cast to float
    let numMales = float (maleRiders ridedata)              // Type cast to float
    (numMales / total) * 100.0                              // Percent computation

  // Print # of male riders
  printfn "Number of Riders Identifying as Male: %A (%A%%)" (maleRiders ridedata) (malePercent ridedata)    // # of males & percent of males


  // --------------------------------------------------------------
  // FEMALE RIDERS (NUMBER & PERCENTAGE) -> (same format as male portion but when gender = 2 in list)
  // 

  // # of female riders function definition (Index 7 in list -> Value 2 = female)
  let femaleRiders ridedata =
    let femaleFilter =
      List.filter (fun f ->
        match f with
        | [ _; _; _; _; _; _; _; gender; _ ] -> gender = 2
        | _ -> false
      ) ridedata
    List.length femaleFilter

  // Caclculate female riders percentage (using femaleRiders and numTrips functions)
  let femalePercent ridedata =
    let total = float (numTrips ridedata)
    let numFemales = float (femaleRiders ridedata)
    (numFemales / total) * 100.0

  printfn "Number of Riders Identifying as Female: %A (%A%%)" (femaleRiders ridedata) (femalePercent ridedata)  // # of females & percent of females
  printfn ""
  

  // --------------------------------------------------------------
  // AVERAGE AGE (FLOAT)
  // 

  // Function definition to calculate and print the average age of all combined riders
  let avgAge ridedata =
    let currYear = System.DateTime.Now.Year   // get current year for later age calculation later
    let birthYears = 
      List.filter (fun e -> 
        match e with
        | [ _; _; _; _; _; _; _; _; birthYear] -> birthYear > 0   // Validate birthyear is logical
        | _ -> false
        ) ridedata 

    if List.isEmpty birthYears then 0.0         // Handle edge case when birthYears is empty
    else    
      let ages =
        List.map (fun f ->                      // Go through each element in list and get their age
          match f with
          | [ _; _; _; _; _; _; _; _; birthYear] -> currYear - birthYear  // Get age by computing current year - birth year
          | _ -> 0                                                       
        ) birthYears  

      let totAges = float (List.sum ages)           // Get total # of ages (as a float)
      let totRiders = float (List.length ages)      // Get total # of riders (as a float)
      (totAges / totRiders)                         // Compute average age (as a float)

  printfn "Average Age: %A" (avgAge ridedata)
  printfn ""
  

  // --------------------------------------------------------------
  // RIDE DURATIONS (NUMBER & PERCENTAGE)
  // 

  // Function definition to define (ONLY - NO PRINT) each different duration and their percent
  let durations ridedata =
    let secDurations =        // Return duration of trip (in seconds due to list definition)
      List.map(fun e -> 
        match e with
        |  [ _; _; tripDuration; _; _; _; _; _; _ ] -> tripDuration   // Ignore everything in list until trip duration (seconds)
        | _ -> 0
        ) ridedata

    let totRides = float (List.length secDurations)   // Get total # of rides for final percent computation later (as a float for final formatting)

    // Define each different time duration (measured in seconds)
    let zeroToThirty = 
      List.filter (fun a -> a <= 1800 && a > 0) secDurations
    let thirtyToSixty =
      List.filter (fun b -> b > 1800 && b <= 3600) secDurations
    let sixtyToOneTwenty =
      List.filter (fun c -> c > 3600 && c <= 7200) secDurations
    let plusTwoHours =
      List.filter (fun d -> d > 7200) secDurations

    // Get # of rides for each time duration
    let zeroToThirtyCount = List.length zeroToThirty
    let thirtyToSixtyCount = List.length thirtyToSixty
    let sixtyToOneTwentyCount = List.length sixtyToOneTwenty
    let plusTwoHoursCount = List.length plusTwoHours

    // Calculate each percent for each different time duration
    let zeroToThirtyPercent = (float zeroToThirtyCount / float totRides) * 100.0
    let thirtyToSixtyPercent = (float thirtyToSixtyCount / float totRides) * 100.0
    let sixtyToOneTwentyPercent = (float sixtyToOneTwentyCount / float totRides) * 100.0
    let plusTwoHoursPercent = (float plusTwoHoursCount / float totRides) * 100.0

    // Return all results as a tuple
    (zeroToThirtyCount, zeroToThirtyPercent,
     thirtyToSixtyCount, thirtyToSixtyPercent,
     sixtyToOneTwentyCount, sixtyToOneTwentyPercent,
     plusTwoHoursCount, plusTwoHoursPercent)

  // Call the durations function to get results
  let (z30Count, z30Pct, 
      t60Count, t60Pct, 
      o120Count, o120Pct, 
      p2hCount, p2hPct) = durations ridedata
    
  // Print results
  printfn "Ride Durations: "
  printfn "   0-30 mins: %A (%A%%)" z30Count z30Pct
  printfn "   30-60 mins: %A (%A%%)" t60Count t60Pct
  printfn "   60-120 mins: %A (%A%%)" o120Count o120Pct
  printfn "   > 2 hours: %A (%A%%)" p2hCount p2hPct
  printfn ""
  

  // --------------------------------------------------------------
  // HISTOGRAM PORTION 
  // 

  // Function definition to extract the starting hour from the list
  let histogramData ridedata =
    let startHours =                                                                        // Extract starting hour from parsed list (0 - 23)
        List.map (fun ride -> match ride with 
                              | [ _; _; _; startingHour; _; _; _; _; _ ] -> startingHour    // Ignore everything but starting hour
                              | _ -> -1 
                 ) ridedata
    
    // Function to count rides for a given hour (helper function with buildHistogram)
    let rec countRides hour hours count =
        match hours with
        | [] -> count                                                   // Base case -> from buildHistogram if empty, else return updated count
        | h :: t -> 
            let newCount = if h = hour then count + 1 else count        // If head = start hour, add 1
            countRides hour t newCount                                  // Else, recursive call without updating anything

    // Function to build the histogram list manually
    let rec buildHistogram hour acc =
        if hour > 23 then acc                                           // Edge case
        else 
            let count = countRides hour startHours 0                    // Setup count 
            let bars = count / 100                                      // Convert to asterisks scale
            let newAcc = (hour, bars, count) :: acc                     // Setup newAcc for recursive call
            buildHistogram (hour + 1) newAcc                            // Actual recursive call

    let result = buildHistogram 0 []                                    // Set default values for buildHistogram
    List.rev result                                                     // Reverse the list for correct order

  // Print histogram with List.iter instead of having each individual start hour 
  printfn "Histogram of Start Times:"
  List.iter (fun entry -> 
      match entry with
      | (hour, bars, count) -> printfn " %A: %s%A" hour (String.replicate bars "*") count   // Display start hour, correct # of asterisks, and count for specified start hour                                                           
      ) (histogramData ridedata)                                                              // Get data from histogramData function using ridedata as parameter


  // End program
  printfn ""
  printfn "Exiting program."
  0