(* This file was taken from https://github.com/jaredly/reason-websocket *)
(*
 * Copyright (c) 2012-2016 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

val websocket_uuid : string
val b64_encoded_sha1sum : string -> string
val upgrade_present : string list -> bool
val make_handshake_response : string -> string

exception Protocol_error of string

module Option : sig
  val value : default:'a -> 'a option -> 'a
  val value_exn : 'a option -> 'a
  val value_map : default:'b -> f:('a -> 'b) -> 'a option -> 'b
  val map : f:('a -> 'b) -> 'a option -> 'b option
end

module Rng : sig
  val init : ?state:Random.State.t -> unit -> (int -> string)
  (** [init ?state ()] is a function that returns a string of random
      bytes of length equal to its argument. *)
end

module Frame : sig
  module Opcode : sig
    type t =
      | Continuation
      | Text
      | Binary
      | Close
      | Ping
      | Pong
      | Ctrl of int
      | Nonctrl of int

    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
  end

  type t = {
    opcode: Opcode.t ;
    extension: int ;
    final: bool ;
    content: string ;
  }

  val create :
    ?opcode:Opcode.t ->
    ?extension:int ->
    ?final:bool ->
    ?content:string ->
    unit -> t

  val close : int -> t
  val show : t -> string
end

module IO(IO: IO.IO) : sig
  type mode =
    | Client of (int -> string)
    | Server

  val make_read_frame :
    ?buf:Buffer.t -> mode:mode -> IO.ic -> IO.oc -> (unit -> Frame.t IO.t)

  val write_frame_to_buf : mode:mode -> Buffer.t -> Frame.t -> unit
end