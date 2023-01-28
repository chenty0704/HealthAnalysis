BeginPackage["HabitifyLink`", "Utilities`"];

HabitifyConnect::usage = UsageString@"HabitifyConnect[`key`] creates a connection to Habitify.";

HabitifyDisconnect::usage = UsageString@"HabitifyDisconnect[] disconnects from Habitify.";

HabitifyExecute::usage = UsageString@"HabitifyExecute[`req`] executes `req` on Habitify.
HabitifyExecute[`req`, {`par_1` -> `val_1`, ...}] executes `req` with the specified settings for parameters.";

Begin["`Private`"];

HabitifyConnect[key_String] := (LocalSymbol["HabitifyAPIKey"] = key;);

HabitifyDisconnect[] := Remove@LocalSymbol["HabitifyAPIKey"];

auth := <|"Headers" -> <|"Authorization" -> LocalSymbol["HabitifyAPIKey"]|>|>;

habitID[name_String] := Module[{data},
    data = URLExecute["https://api.habitify.me/habits", "RawJSON", Authentication -> auth]["data"];
    data = Association @@ Table[habit["name"] -> habit["id"], {habit, data}];
    Return@data[name];
];

HabitifyExecute["HabitList"] := Module[{data},
    data = URLExecute["https://api.habitify.me/habits", "RawJSON", Authentication -> auth]["data"];
    Return@Sort@Table[habit["name"], {habit, Select[data, !#["is_archived"] &]}];
];

HabitifyExecute["HabitEventSeries", {
    "Name" -> name_String,
    "FirstDate" -> DateObject[firstDate_, "Day"],
    "LastDate" -> DateObject[lastDate_, "Day"]
}] := Module[{data},
    data = URLExecute["https://api.habitify.me/logs/" ~~ habitID[name], {
        "from" -> DateString[firstDate, {"ISODateTime", "ISOTimeZone"}],
        "to" -> DateString[NextDate[lastDate, "Day"], {"ISODateTime", "ISOTimeZone"}]
    }, "RawJSON", Authentication -> auth]["data"];
    Return@TimeSeriesResample[EventSeries@Table[{
        DateObject[FromDateString@log["created_date"], "Day"],
        Switch[log["unit_type"],
            "rep", log["value"],
            "min", Quantity[log["value"], "Minutes"]
        ]
    }, {log, data}], {firstDate, lastDate, "Day"}];
];

HabitifyExecute["MoodEventSeries", {
    "FirstDate" -> DateObject[firstDate_, "Day"],
    "LastDate" -> DateObject[lastDate_, "Day"]
}] := Module[{dates, values, data},
    dates = DayRange[firstDate, lastDate];
    values = Table[(
        data = URLExecute["https://api.habitify.me/moods", {
            "target_date" -> DateString[date, {"ISODateTime", "ISOTimeZone"}]
        }, "RawJSON", Authentication -> auth]["data"];
        If[Length[data] > 0, Mean[Table[log["value"], {log, data}]], Missing[]]
    ), {date, dates}];
    Return@EventSeries[values, {dates}];
];

End[];

EndPackage[];
