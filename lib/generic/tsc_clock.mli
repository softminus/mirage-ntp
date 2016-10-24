exception Counter_causality of int64
type counter = Cstruct.uint64

type 'a sync_state (* intentionally an abstract type *)


type timestamps =
  Tsc_types.timestamps = {
  ta : counter;
  tb : float;
  te : float;
  tf : counter;
}
type quality = Tsc_types.quality = NG | OK
type 'a sample =
  'a Tsc_types.sample = {
  quality : quality;
  timestamps : timestamps;
  private_data : 'a;
}

type output =
  Tsc_types.output = {
  skm_scale : float;
  freshness : counter;
  rate : float * float;
  local_rate : (float * float) option;
  ca_and_error : float * float;
}


val blank_state : 'a sync_state
val output_of_state : 'a sync_state -> output option
val add_sample : 'a sync_state -> 'a sample -> 'a sync_state

val latest_sample : 'a sync_state -> ('a sample * counter) option


(* only ppx_deriving stuff below *)
val pp_counter : Format.formatter -> counter -> Ppx_deriving_runtime.unit
val show_counter : counter -> Ppx_deriving_runtime.string
val pp_timestamps :
  Format.formatter -> timestamps -> Ppx_deriving_runtime.unit
val show_timestamps : timestamps -> Ppx_deriving_runtime.string
val pp_quality : Format.formatter -> quality -> Ppx_deriving_runtime.unit
val show_quality : quality -> Ppx_deriving_runtime.string
val pp_sample :
  (Format.formatter -> 'a -> Ppx_deriving_runtime.unit) ->
  Format.formatter -> 'a sample -> Ppx_deriving_runtime.unit
val show_sample :
  (Format.formatter -> 'a -> Ppx_deriving_runtime.unit) ->
  'a sample -> Ppx_deriving_runtime.string
val pp_output : Format.formatter -> output -> Ppx_deriving_runtime.unit
val show_output : output -> Ppx_deriving_runtime.string
val pp_sync_state :
  (Format.formatter -> 'a -> Ppx_deriving_runtime.unit) ->
  Format.formatter -> 'a sync_state -> Ppx_deriving_runtime.unit
val show_sync_state :
  (Format.formatter -> 'a -> Ppx_deriving_runtime.unit) ->
  'a sync_state -> Ppx_deriving_runtime.string
