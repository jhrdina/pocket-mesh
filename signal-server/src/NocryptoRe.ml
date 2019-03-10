module Rsa = struct
  (* We need to alias this for use in Reason because 'pub' is a reserved keyword in Reason. *)
  type pub_ = Nocrypto.Rsa.pub
end