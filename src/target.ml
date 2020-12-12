open Core

module type Intf = sig

  val cell_size: int

  val align: Buffer.t -> unit

  val add_enter: int -> Buffer.t -> unit

  val add_exit: int -> Buffer.t -> unit

  val append_placeholder_address: Buffer.t -> unit

  val update_address: int -> int -> Buffer.t -> unit

  val append_inline_constant: int -> Buffer.t -> unit

  val append_call: int -> Buffer.t -> unit

  val append_address: int -> Buffer.t -> unit

  val append_number: int -> Buffer.t -> unit

  val append_code: int -> Buffer.t -> unit

  val create_image_data: Buffer.t

  val finalize_image_data:
    Ast1.definition list -> Buffer.t -> int -> int -> int -> int -> int -> bytes

end

module type Conf = sig

  val base: int
  val info: int
  val ram: int
  val user: int

end
