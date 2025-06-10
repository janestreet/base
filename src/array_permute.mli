@@ portable

val permute
  :  ?random_state:Random.State.t
  -> ?pos:int
  -> ?len:int
  -> 'a array @ local
  -> unit
