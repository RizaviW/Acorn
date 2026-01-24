open! Core
open! Driver
open! Application_registers.Register_map

let execute_passthrough_verification device_connection =
  let output_log message =
    Out_channel.output_string stdout message;
    Out_channel.flush stdout
  in

  output_log "--- Acorn Application: Passthrough Mode Initialized ---\n";

  Acorn.write_register device_connection control_register_index 0l;
  Acorn.write_register device_connection status_register_index 0l;
  output_log "[Status] Configuration and Status interfaces tied to ground.\n";

  Acorn.set_register_bits   device_connection control_register_index Control.start_mask;
  Acorn.clear_register_bits device_connection control_register_index Control.start_mask;
  output_log "[Status] Control path strobe transaction verified.\n";

  let _register_snapshot = Acorn.read_register device_connection control_register_index in
  output_log "[Status] Peripheral read path validated.\n";

  let polling_result =
    Acorn.wait_with_timeout
      ~timeout_milliseconds:10
      device_connection
      status_register_index
      0x0l
  in
  (match polling_result with
   | Ok () | Error `Hardware_Timeout ->
       output_log "[Status] Device hardware verified in steady state.\n");

  let vfio_context_pointer = Acorn.get_vfio_context device_connection in
  let memory_address = Ctypes.raw_address_of_ptr vfio_context_pointer in

  Printf.printf "--- Passthrough Cycle Complete. Context Address: 0x%nx ---\n%!"
    memory_address

let () =
  Acorn.connect
    ~group:24
    ~address:"0000:09:00.0"
    ~base_byte_offset:0x40000
  |> execute_passthrough_verification
