type stats

val lines : stats -> int

val characters : stats -> int

val words : stats -> int

val sentences : stats -> int

val c_hist : stats -> int array

val stats_from_file : string -> stats
