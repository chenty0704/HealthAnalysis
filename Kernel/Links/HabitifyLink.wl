BeginPackage["HabitifyLink`", {"Utilities`"}];

HabitifyConnect::usage = UsageString@"HabitifyConnect[`key`] creates a connection to Habitify.";

HabitifyDisconnect::usage = UsageString@"HabitifyDisconnect[] disconnects from Habitify.";

HabitifyExecute::usage = UsageString@"HabitifyExecute[`req`] executes `req` on Habitify.
HabitifyExecute[`req`, {`par_1` -> `val_1`, ...}] executes `req` with the specified settings for parameters.";

Begin["`Private`"];

HabitifyConnect[key_String] := (LocalSymbol["HabitifyAPIKey"] = key;);

HabitifyDisconnect[] := Remove@LocalSymbol["HabitifyAPIKey"];

auth := <|"Headers" -> <|"Authorization" -> LocalSymbol["HabitifyAPIKey"]|>|>;
DistributeDefinitions[auth];

habitList[] := Module[{data},
    data = URLExecute["https://api.habitify.me/habits", "RawJSON", Authentication -> auth]["data"];
    Return@Association@Table[habit["name"] -> <|
        "ID" -> habit["id"],
        "StartDate" -> DateObject[habit["start_date"], "Day"]
    |>, {habit, Select[data, !#["is_archived"] &]}];
];

HabitifyExecute["HabitList"] := KeySort[KeyDrop["ID"] /@ habitList[]];

habitEventSeries[id_String, startDate_DateObject, endDate_DateObject] := Module[{dates, values, progress},
    dates = DayRange[startDate, endDate];
    values = ParallelTable[(
        progress = URLExecute["https://api.habitify.me/status/" ~~ id, {
            "target_date" -> DateString[date, {"ISODateTime", "ISOTimeZone"}]
        }, "RawJSON", Authentication -> auth]["data"]["progress"];
        N[progress["current_value"] / progress["target_value"]]
    ), {date, dates}];
    Return@EventSeries[values, {dates}];
];

HabitifyExecute["HabitEventSeries", {"Name" -> name_String}] := Module[{info},
    info = habitList[][name];
    Return@habitEventSeries[info["ID"], info["StartDate"], Today];
];

HabitifyExecute["HabitEventSeries", {
    "Name" -> name_String,
    "StartDate" -> startDate_DateObject
}] := HabitifyExecute["HabitEventSeries", {"Name" -> name, "StartDate" -> startDate, "EndDate" -> Today}];

HabitifyExecute["HabitEventSeries", {
    "Name" -> name_String,
    "StartDate" -> startDate_DateObject,
    "EndDate" -> endDate_DateObject
}] := habitEventSeries[habitList[][name]["ID"], startDate, endDate];

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
