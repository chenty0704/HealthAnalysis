BeginPackage["HabitifyLink`", "Utilities`"];

HabitifyConnect::usage = UsageString@"HabitifyConnect[`key`] creates a connection to Habitify.";

HabitifyDisconnect::usage = UsageString@"HabitifyDisconnect[] disconnects from Habitify.";

HabitifyExecute::usage = UsageString@"HabitifyExecute[`req`, {`par_1` -> `val_1`, ...}] executes `req` with the specified settings for parameters.";

Begin["`Private`"];

HabitifyConnect[key_String] := (LocalSymbol["HabitifyAPIKey"] = key;);

HabitifyDisconnect[] := Remove@LocalSymbol["HabitifyAPIKey"];

auth := <|"Headers" -> <|"Authorization" -> LocalSymbol["HabitifyAPIKey"]|>|>;

habitID[name_String] := Module[{data},
    If[MissingQ@PersistentSymbol["HabitList"],
        data = URLExecute["https://api.habitify.me/habits", "RawJSON", Authentication -> auth]["data"];
        PersistentSymbol["HabitList", PersistenceTime -> Quantity[1, "Days"]] =
                Association @@ Table[habit["name"] -> habit["id"], {habit, data}];
    ];
    Return@PersistentSymbol["HabitList"][name];
];

HabitifyExecute["HabitEventSeries", {
    "Name" -> name_String,
    "FirstDate" -> firstDate_,
    "LastDate" -> lastDate_
}] := Module[{data},
    data = URLExecute["https://api.habitify.me/logs/" ~~ habitID[name], {
        "from" -> DateString[DateObject[firstDate, "Day"], {"ISODateTime", "ISOTimeZone"}],
        "to" -> DateString[NextDate[lastDate, "Day"], {"ISODateTime", "ISOTimeZone"}]
    }, "RawJSON", Authentication -> auth]["data"];
    Return@EventSeries@Table[{
        DateObject[FromDateString@log["created_date"], "Day"],
        Switch[log["unit_type"],
            "rep", log["value"],
            "min", Quantity[log["value"], "Minutes"]
        ]
    }, {log, data}];
];

End[];

EndPackage[];
