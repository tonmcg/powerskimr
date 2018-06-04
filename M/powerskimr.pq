/*
numeric
    missing,
    complete,
    n,
    mean,
    sd,
    p0,
    p25,
    p50,
    p75,
    p100,
    hist

factor
    missing,
    complete,
    n,
    n_unique,
    top_counts,
    ordered

character
    missing,
    complete,
    n,
    min,
    max,
    empty,
    n_unique

logical
    missing,
    complete,
    n,
    mean,
    count

complex
    missing,
    complete,
    n

date
    missing,
    complete,
    n,
    min,
    max,
    median,
    n_unique
    
timeseries
    missing,
    complete,
    n,
    start,
    end,
    frequency,
    deltat,
    mean,
    sd,
    min,
    max,
    median

*/

let
    Percentile = Expression.Evaluate(
        Text.FromBinary(Web.Contents("https://tinyurl.com/ListPercentile")),
        [
            #"List.Buffer" = List.Buffer,
            #"List.Count" = List.Count,
            #"List.Sort" = List.Sort,
            #"Number.IntegerDivide" = Number.IntegerDivide,
            #"Number.Mod" = Number.Mod,
            #"Order.Ascending" = Order.Ascending,
            #"Value.ReplaceMetadata" = Value.ReplaceMetadata,
            #"Value.ReplaceType" = Value.ReplaceType,
            #"Value.Type" = Value.Type
        ]
    ),
    source = Table1,
    sourceColumns = Table.ColumnNames(source),
    columnCount = List.Count(sourceColumns),
    skimTable = 
        Table.FromList(
            List.Generate(
                ()=>
                    [
                        t = 0,
                        variable = sourceColumns{t},
                        currentColumn = List.Buffer(Table.Column(source,variable)),
                        type = Table.Schema(source)[Kind]{t},
                        n = List.Count(currentColumn),
                        complete = List.NonNullCount(currentColumn),
                        missing = n - complete,
                        n_unique = List.Count(List.Distinct(currentColumn)),
                        mean = List.Average(currentColumn),
                        sd = List.StandardDeviation(currentColumn),
                        p0 = Percentile(currentColumn,0),
                        p25 = Percentile(currentColumn,.25),
                        p50 = Percentile(currentColumn,.5),
                        p75 = Percentile(currentColumn,.75),
                        p100 = Percentile(currentColumn,1)
                    ], // initial
                each [t] < columnCount, // condition
                each 
                    [
                        t = [t] + 1,
                        variable = sourceColumns{t},
                        currentColumn = List.Buffer(Table.Column(source,variable)),
                        type = Table.Schema(source)[Kind]{t},
                        n = List.Count(currentColumn),
                        complete = List.NonNullCount(currentColumn),
                        missing = n - complete,
                        n_unique = List.Count(List.Distinct(currentColumn)),
                        mean = List.Average(currentColumn),
                        sd = List.StandardDeviation(currentColumn),
                        p0 = Percentile(currentColumn,0),
                        p25 = Percentile(currentColumn,.25),
                        p50 = Percentile(currentColumn,.5),
                        p75 = Percentile(currentColumn,.75),
                        p100 = Percentile(currentColumn,1)
                    ], // next
                each if [type] = "number" then [[variable],[type],[n],[complete],[missing],[mean],[sd],[p0],[p25],[p50],[p75],[p100]] else [[variable],[type],[n],[complete],[missing],[n_unique]] // selector
            ),
        Splitter.SplitByNothing(), {"skim"}, null, ExtraValues.Error),
    expandedSkimTable = Table.ExpandRecordColumn(skimTable, "skim", {"variable", "type", "n", "complete", "missing", "mean", "sd", "p0", "p25", "p50", "p75", "p100", "n_unique"}, {"variable", "type", "n", "complete", "missing", "mean", "sd", "p0", "p25", "p50", "p75", "p100", "n_unique"}),
    unpivotedSkimr = Table.UnpivotOtherColumns(expandedSkimTable, {"variable", "type"}, "stat", "value"),
    addedFormatted = Table.AddColumn(unpivotedSkimr, "formatted", each Text.From(Number.Round([value],2)),type text)
in
    addedFormatted