BeginPackage["HabitifyLink`", {"Utilities`"}];

HabitifyConnect::usage = UsageString@"HabitifyConnect[`key`] creates a connection to Habitify.";

HabitifyDisconnect::usage = UsageString@"HabitifyDisconnect[] disconnects from Habitify.";

HabitifyExecute::usage = UsageString@"HabitifyExecute[`req`] executes `req` on Habitify.
HabitifyExecute[`req`, {`par_1` -> `val_1`, ...}] executes `req` with the specified settings for parameters.";

Begin["`Private`"];

HabitifyConnect[key_String] := (LocalSymbol["HabitifyAPIKey"] = key;);

HabitifyDisconnect[] := Remove@LocalSymbol["HabitifyAPIKey"];

auth := <|"Headers" -> <|"Authorization" -> LocalSymbol["HabitifyAPIKey"]|>|>;

habitList[] := Module[{data},
    data = URLExecute["https://api.habitify.me/habits", "RawJSON", Authentication -> auth]["data"];
    Return@Association@Table[habit["name"] -> <|
        "ID" -> habit["id"],
        "Unit" -> Switch[habit["goal"]["unit_type"],
            "rep", Null,
            "min", "Minutes"
        ],
        "StartDate" -> DateObject[habit["start_date"], "Day"]
    |>, {habit, Select[data, !#["is_archived"] &]}];
];

HabitifyExecute["HabitList"] := KeySort[KeyDrop["ID"] /@ habitList[]];

habitEventSeries[id_String, unit : Null | _String, startDate_DateObject, endDate_DateObject] := Module[{data},
    data = URLExecute["https://api.habitify.me/logs/" ~~ id, {
        "from" -> DateString[startDate, {"ISODateTime", "ISOTimeZone"}],
        "to" -> DateString[NextDate[endDate, "Day"], {"ISODateTime", "ISOTimeZone"}]
    }, "RawJSON", Authentication -> auth]["data"];
    Return@TimeSeriesResample[
        EventSeries@Table[{
            DateObject[log["created_date"], "Day"],
            If[unit === Null, log["value"], Quantity[log["value"], unit]]
        }, {log, data}], {startDate, endDate, "Day"},
        ResamplingMethod -> {"Constant", If[unit === Null, 0, Quantity[0, unit]]}
    ];
];

HabitifyExecute["HabitEventSeries", {"Name" -> name_String}] := Module[{info},
    info = habitList[][name];
    Return@habitEventSeries[info["ID"], info["Unit"], info["StartDate"], Today];
];

HabitifyExecute["HabitEventSeries", {
    "Name" -> name_String,
    "StartDate" -> startDate_DateObject
}] := HabitifyExecute["HabitEventSeries", {"Name" -> name, "StartDate" -> startDate, "EndDate" -> Today}];

HabitifyExecute["HabitEventSeries", {
    "Name" -> name_String,
    "StartDate" -> startDate_DateObject,
    "EndDate" -> endDate_DateObject
}] := Module[{info},
    info = habitList[][name];
    Return@habitEventSeries[info["ID"], info["Unit"], startDate, endDate];
];

HabitifyExecute["MoodEventSeries", {"StartDate" -> startDate_DateObject}] :=
        HabitifyExecute["MoodEventSeries", {"StartDate" -> startDate, "EndDate" -> Today}];

HabitifyExecute["MoodEventSeries", {
    "StartDate" -> startDate_DateObject,
    "EndDate" -> endDate_DateObject
}] := Module[{dates, values, data},
    dates = DayRange[startDate, endDate];
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
