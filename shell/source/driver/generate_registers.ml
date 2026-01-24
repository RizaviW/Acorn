(*
  generate_registers.ml

  Generate a strongly validated OCaml register map from annotated
  SystemVerilog source.

  Usage:
    generate_registers.exe <input.sv> <output_register_map.ml>

  Design principles:
    - SystemVerilog syntax is never parsed.
    - Only explicitly tagged comments are considered.
    - All invariants are validated eagerly.
    - Errors report exact source locations.
    - No abbreviated names anywhere.
*)

open Printf

(* ---------- Types ---------- *)

type source_location =
  { line_number : int
  ; _line_text : string
  }

type register_field =
  { field_name : string
  ; most_significant_bit : int
  ; least_significant_bit : int
  ; _declared_at : source_location
  }

type register_definition =
  { register_name : string
  ; register_index : int64 option
  ; _index_declared_at : source_location option
  ; fields : register_field list
  ; first_referenced_at : source_location
  }

(* ---------- String utilities ---------- *)

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let trim string_value =
  let length = String.length string_value in
  let left_index = ref 0 in
  while !left_index < length && is_whitespace string_value.[!left_index] do
    incr left_index
  done;
  let right_index = ref (length - 1) in
  while !right_index >= !left_index && is_whitespace string_value.[!right_index] do
    decr right_index
  done;
  if !right_index < !left_index then ""
  else String.sub string_value !left_index (!right_index - !left_index + 1)

let split_on_spaces string_value =
  string_value
  |> String.split_on_char ' '
  |> List.filter (fun token -> token <> "")

let find_substring ~needle ~haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec search index =
    if index + needle_length > haystack_length then None
    else if String.sub haystack index needle_length = needle then Some index
    else search (index + 1)
  in
  search 0

let take_while predicate string_value start_index =
  let length = String.length string_value in
  let index = ref start_index in
  while !index < length && predicate string_value.[!index] do
    incr index
  done;
  String.sub string_value start_index (!index - start_index)

let uppercase = String.uppercase_ascii
let lowercase = String.lowercase_ascii
let module_name register_name =
  String.capitalize_ascii (lowercase register_name)

(* ---------- Parsing primitives ---------- *)

let parse_hex_literal_32h _line_text =
  match find_substring ~needle:"32'h" ~haystack:_line_text with
  | None ->
      Error "Expected a 32'h hexadecimal literal on this line."
  | Some position ->
      let start_index = position + 4 in
      let raw_hex_digits =
        take_while
          (fun character ->
            ('0' <= character && character <= '9')
            || ('a' <= character && character <= 'f')
            || ('A' <= character && character <= 'F')
            || character = '_')
          _line_text
          start_index
      in
      if raw_hex_digits = "" then
        Error "Found 32'h but no hexadecimal digits followed."
      else
        let cleaned_digits =
          String.concat "" (String.split_on_char '_' raw_hex_digits)
        in
        Ok (Int64.of_string ("0x" ^ cleaned_digits))

let parse_bit_specification specification =
  match String.split_on_char ':' specification with
  | [ most_significant; least_significant ] ->
      Ok (int_of_string most_significant, int_of_string least_significant)
  | [ bit_index ] ->
      let value = int_of_string bit_index in
      Ok (value, value)
  | _ ->
      Error ("Invalid bit specification: " ^ specification)

let strip_comment_prefix comment_text =
  let trimmed = trim comment_text in
  if String.length trimmed >= 2 && String.sub trimmed 0 2 = "//" then
    trim (String.sub trimmed 2 (String.length trimmed - 2))
  else
    trimmed

(* ---------- REGMAP block extraction ---------- *)

let extract_register_map_block numbered_lines =
  let rec find_beginning = function
    | [] ->
        Error "Missing // REGMAP-BEGIN marker."
    | (_line_number, _line_text) :: remaining_lines ->
        if trim _line_text = "// REGMAP-BEGIN" then
          collect_body remaining_lines
        else
          find_beginning remaining_lines
  and collect_body = function
    | [] ->
        Error "Missing // REGMAP-END marker."
    | (line_number, _line_text) :: remaining_lines ->
        if trim _line_text = "// REGMAP-END" then
          Ok []
        else
          collect_body remaining_lines
          |> Result.map (fun tail -> (line_number, _line_text) :: tail)
  in
  find_beginning numbered_lines

(* ---------- Core parsing ---------- *)

let parse_register_map_block numbered_lines =
  let register_table : (string, register_definition) Hashtbl.t =
    Hashtbl.create 64
  in

  let get_or_create_register register_name source_location =
    match Hashtbl.find_opt register_table register_name with
    | Some existing_register -> existing_register
    | None ->
        let new_register =
          { register_name
          ; register_index = None
          ; _index_declared_at = None
          ; fields = []
          ; first_referenced_at = source_location
          }
        in
        Hashtbl.add register_table register_name new_register;
        new_register
  in

  let update_register register_definition =
    Hashtbl.replace
      register_table
      register_definition.register_name
      register_definition
  in

  List.iter
    (fun (line_number, full_line_text) ->
      match find_substring ~needle:"//" ~haystack:full_line_text with
      | None -> ()
      | Some comment_index ->
          let comment_text =
            strip_comment_prefix
              (String.sub
                 full_line_text
                 comment_index
                 (String.length full_line_text - comment_index))
          in
          let tokens = split_on_spaces comment_text in
          let source_location =
            { line_number; _line_text = full_line_text }
          in
          match tokens with
          | "@reg" :: register_name :: _ ->
              let register_name = uppercase register_name in
              let index_value =
                match parse_hex_literal_32h full_line_text with
                | Ok value -> value
                | Error message ->
                    failwith
                      (sprintf "Line %d: %s" line_number message)
              in
              let register_definition =
                get_or_create_register register_name source_location
              in
              update_register
                { register_definition with
                  register_index = Some index_value
                ; _index_declared_at = Some source_location
                }

          | "@field" :: register_name :: field_name :: bit_specification :: _ ->
              let register_name = uppercase register_name in
              let field_name = uppercase field_name in
              let most_significant_bit, least_significant_bit =
                match parse_bit_specification bit_specification with
                | Ok values -> values
                | Error message ->
                    failwith
                      (sprintf "Line %d: %s" line_number message)
              in
              if most_significant_bit < least_significant_bit then
                failwith
                  (sprintf
                     "Line %d: most_significant_bit < least_significant_bit"
                     line_number);
              if least_significant_bit < 0 || most_significant_bit > 31 then
                failwith
                  (sprintf
                     "Line %d: bit index out of range (0..31)"
                     line_number);
              let register_definition =
                get_or_create_register register_name source_location
              in
              let field_definition =
                { field_name
                ; most_significant_bit
                ; least_significant_bit
                ; _declared_at = source_location
                }
              in
              update_register
                { register_definition with
                  fields = field_definition :: register_definition.fields
                }

          | _ -> ()
    )
    numbered_lines;

  Hashtbl.to_seq_values register_table |> List.of_seq

(* ---------- Validation ---------- *)

let validate_register_definitions register_definitions =
  let index_table : (int64, string) Hashtbl.t = Hashtbl.create 64 in

  List.iter
    (fun register_definition ->
      match register_definition.register_index with
      | None ->
          let location = register_definition.first_referenced_at in
          failwith
            (sprintf
               "Register %s referenced at line %d but never assigned an index."
               register_definition.register_name
               location.line_number)
      | Some index ->
          match Hashtbl.find_opt index_table index with
          | None ->
              Hashtbl.add
                index_table
                index
                register_definition.register_name
          | Some other_register ->
              failwith
                (sprintf
                   "Duplicate register index 0x%Lx for registers %s and %s."
                   index
                   other_register
                   register_definition.register_name)
    )
    register_definitions;

  List.iter
    (fun register_definition ->
      let used_bits = Array.make 32 "" in
      List.iter
        (fun field_definition ->
          for bit_index =
            field_definition.least_significant_bit
            to field_definition.most_significant_bit
          do
            if used_bits.(bit_index) <> "" then
              failwith
                (sprintf
                   "Register %s has overlapping fields at bit %d (%s vs %s)."
                   register_definition.register_name
                   bit_index
                   used_bits.(bit_index)
                   field_definition.field_name)
            else
              used_bits.(bit_index) <- field_definition.field_name
          done
        )
        register_definition.fields
    )
    register_definitions

(* ---------- Emission ---------- *)

let emit_register_map output_path register_definitions =
  let output_channel = open_out output_path in

  fprintf output_channel "(* Autogenerated. Do not edit by hand. *)\n";
  fprintf output_channel "module Register_map = struct\n\n";

  fprintf
    output_channel
    "  (* Register indices corresponding directly to CSR address indices. *)\n";
  List.iter
    (fun register_definition ->
      let register_name = lowercase register_definition.register_name in
      let index =
        match register_definition.register_index with
        | Some value -> value
        | None -> assert false
      in
      fprintf
        output_channel
        "  let %s_register_index : int = %Ld\n"
        register_name
        index)
    register_definitions;

  fprintf output_channel "\n  module Field = struct\n";
  fprintf
    output_channel
    "    let mask ~(bit_width : int) ~(least_significant_bit : int) : Int32.t =\n";
  fprintf
    output_channel
    "      let ones = Int32.sub (Int32.shift_left 1l bit_width) 1l in\n";
  fprintf
    output_channel
    "      Int32.shift_left ones least_significant_bit\n";
  fprintf output_channel "  end\n\n";

  List.iter
    (fun register_definition ->
      let sorted_fields =
        List.sort
          (fun left right ->
            compare
              left.least_significant_bit
              right.least_significant_bit)
          register_definition.fields
      in
      if sorted_fields <> [] then begin
        fprintf
          output_channel
          "  module %s = struct\n"
          (module_name register_definition.register_name);
        List.iter
          (fun field_definition ->
            let field_name = lowercase field_definition.field_name in
            if
              field_definition.most_significant_bit
              = field_definition.least_significant_bit
            then begin
              fprintf
                output_channel
                "    let %s_bit_index : int = %d\n"
                field_name
                field_definition.least_significant_bit;
              fprintf
                output_channel
                "    let %s_mask : Int32.t = Int32.shift_left 1l %d\n"
                field_name
                field_definition.least_significant_bit
            end else begin
              let bit_width =
                field_definition.most_significant_bit
                - field_definition.least_significant_bit
                + 1
              in
              fprintf
                output_channel
                "    let %s_least_significant_bit : int = %d\n"
                field_name
                field_definition.least_significant_bit;
              fprintf
                output_channel
                "    let %s_most_significant_bit : int = %d\n"
                field_name
                field_definition.most_significant_bit;
              fprintf
                output_channel
                "    let %s_bit_width : int = %d\n"
                field_name
                bit_width;
              fprintf
                output_channel
                "    let %s_mask : Int32.t = Field.mask ~bit_width:%d ~least_significant_bit:%d\n"
                field_name
                bit_width
                field_definition.least_significant_bit
            end;
            fprintf output_channel "\n")
          sorted_fields;
        fprintf output_channel "  end\n\n"
      end)
    register_definitions;

  fprintf output_channel "end\n";
  close_out output_channel

(* ---------- Entry point ---------- *)

let () =
  if Array.length Sys.argv <> 3 then begin
    eprintf
      "Usage: %s <input.sv> <output_register_map.ml>\n%!"
      Sys.argv.(0);
    exit 1
  end;

  let input_path = Sys.argv.(1) in
  let output_path = Sys.argv.(2) in

  let numbered_lines =
    In_channel.with_open_text input_path (fun input_channel ->
      let rec read_lines accumulated_lines current_line_number =
        match In_channel.input_line input_channel with
        | None -> List.rev accumulated_lines
        | Some _line_text ->
            read_lines
              ((current_line_number, _line_text) :: accumulated_lines)
              (current_line_number + 1)
      in
      read_lines [] 1)
  in

  match extract_register_map_block numbered_lines with
  | Error message ->
      eprintf "ERROR: %s\n%!" message;
      exit 2
  | Ok register_map_block ->
      let register_definitions =
        parse_register_map_block register_map_block
      in
      validate_register_definitions register_definitions;
      let sorted_register_definitions =
        List.sort
          (fun left right ->
            Int64.compare
              (Option.get left.register_index)
              (Option.get right.register_index))
          register_definitions
      in
      emit_register_map output_path sorted_register_definitions;
      eprintf
        "Wrote %s (%d registers)\n%!"
        output_path
        (List.length sorted_register_definitions)
