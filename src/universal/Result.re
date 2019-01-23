[@bs.deriving accessors]
type t('a, 'b) =
  | Ok('a)
  | Error('b);