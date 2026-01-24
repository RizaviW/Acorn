open! Core
open! Ctypes
open! Foreign

(** [Acorn]: A high-performance hardware abstraction layer for VFIO. *)
module Acorn : sig
  type t

  val connect : 
    group:int -> address:string -> base_byte_offset:int -> t

  val read_register : 
    t -> int -> int32

  val write_register : 
    t -> int -> int32 -> unit

  val set_register_bits : 
    t -> int -> int32 -> unit

  val clear_register_bits : 
    t -> int -> int32 -> unit

  val wait_with_timeout : 
    timeout_milliseconds:int -> t -> int -> int32 -> (unit, [> `Hardware_Timeout ]) result

  val get_vfio_context : 
    t -> unit ptr
end = struct

  type t = {
    memory_mapped_io : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
    base_index       : int;
    vfio_context     : unit ptr;
  }

  module Backend = struct
    let context_type = structure "vfio_context_t"
    let _padding_1   = field context_type "padding_1" int
    let _padding_2   = field context_type "padding_2" int
    let device_fd    = field context_type "device_fd" int
    let ()           = seal context_type

    let initialize = 
      foreign "c_init_vfio" (int @-> string @-> ptr context_type @-> returning int)

    let get_bar_info = 
      foreign "c_get_bar_info" (int @-> int @-> ptr ulong @-> ptr ulong @-> returning int)
  end

  let connect ~group ~address ~base_byte_offset =
    let open Backend in
    let context_pointer = allocate_n context_type ~count:1 in
    
    if initialize group address context_pointer <> 0 
    then failwith "Acorn: VFIO initialization failed";

    let fd     = getf (!@ context_pointer) device_fd in
    let offset = allocate ulong Unsigned.ULong.zero in
    let size   = allocate ulong Unsigned.ULong.zero in

    if get_bar_info fd 0 offset size <> 0 
    then failwith "Acorn: BAR mapping failed";

    let memory_mapped_io =
      let length   = Unsigned.ULong.to_int (!@ size) / 4 in
      let position = Int64.of_int (Unsigned.ULong.to_int (!@ offset)) in
      Core_unix.map_file 
        (Core_unix.File_descr.of_int fd) 
        ~pos:position 
        ~shared:true 
        Bigarray.int32 
        Bigarray.c_layout 
        [| length |]
      |> Bigarray.array1_of_genarray
    in

    let device = { 
      memory_mapped_io; 
      base_index = base_byte_offset / 4; 
      vfio_context = to_voidp context_pointer; 
    } in

    Stdlib.at_exit (fun () -> 
      try device.memory_mapped_io.{device.base_index} <- 0l with _ -> ());

    device

  let[@inline always] read_register device register_index =
    device.memory_mapped_io.{device.base_index + register_index}

  let[@inline always] write_register device register_index value =
    device.memory_mapped_io.{device.base_index + register_index} <- value

  let[@inline always] set_register_bits device register_index mask =
    let open Int32 in
    let current = read_register device register_index in
    write_register device register_index (bit_or current mask)

  let[@inline always] clear_register_bits device register_index mask =
    let open Int32 in
    let current = read_register device register_index in
    write_register device register_index (bit_and current (bit_not mask))

  let rec wait_loop ~deadline device register_index bit_mask =
    if Float.(Core_unix.gettimeofday () > deadline) 
    then Error `Hardware_Timeout
    else if Int32.(bit_and (read_register device register_index) bit_mask <> 0l) 
    then Ok ()
    else (Domain.cpu_relax (); wait_loop ~deadline device register_index bit_mask)

  let wait_with_timeout ~timeout_milliseconds device register_index bit_mask =
    let deadline = Core_unix.gettimeofday () +. (Float.of_int timeout_milliseconds /. 1000.0) in
    wait_loop ~deadline device register_index bit_mask

  let get_vfio_context device = device.vfio_context
end
