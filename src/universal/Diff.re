// Inspired by symmetric_diff in https://github.com/janestreet/base/blob/master/src/set.ml and https://github.com/janestreet/base/blob/master/src/map.ml

module Map = {
  module type S = {
    include Map.S;
    type diff_result('v) =
      | Left('v)
      | Right('v)
      | Unequal('v, 'v);
    let symmetric_diff:
      (
        t('v),
        t('v),
        ~f: ((key, diff_result('v)), 'acc) => 'acc,
        ~veq: ('v, 'v) => bool,
        ~acc: 'acc
      ) =>
      'acc;
  };
  module Make: (Ord: Map.OrderedType) => S with type key = Ord.t =
    (Ord: Map.OrderedType) => {
      include Map.Make(Ord);

      type diff_result('v) =
        | Left('v)
        | Right('v)
        | Unequal('v, 'v);

      // Keep in sync with Map.Make.t
      type t_i('v) =
        | Empty
        | Node(t_i('v), key, 'v, t_i('v), int);

      external impl_of_t: t('a) => t_i('a) = "%identity";

      type enumeration_i('v) =
        | End
        | More(key, 'v, t_i('v), enumeration_i('v));

      let rec cons_enum = (m, e) =>
        switch (m) {
        | Empty => e
        | [@implicit_arity] Node(l, v, d, r, _) =>
          cons_enum(l, [@implicit_arity] More(v, d, r, e))
        };

      let of_tree = t => cons_enum(t, End);

      let symmetric_diff:
        (
          t('v),
          t('v),
          ~f: (('k, diff_result('v)), 'acc) => 'acc,
          ~veq: ('v, 'v) => bool,
          ~acc: 'acc
        ) =>
        'acc =
        (t1, t2, ~f, ~veq, ~acc) => {
          let rec step = (~state, ~acc) => {
            switch (state) {
            | (End, End) => acc
            | (End, More(key, data, tree, enum)) =>
              step(
                ~acc=f((key, Right(data)), acc),
                ~state=(End, cons_enum(tree, enum)),
              )
            | (More(key, data, tree, enum), End) =>
              step(
                ~acc=f((key, Left(data)), acc),
                ~state=(cons_enum(tree, enum), End),
              )
            | (
                More(k1, v1, tree1, enum1) as left,
                More(k2, v2, tree2, enum2) as right,
              ) =>
              let compare_result = Ord.compare(k1, k2);
              if (compare_result == 0) {
                let next_state =
                  if (tree1 === tree2) {
                    (enum1, enum2);
                  } else {
                    (cons_enum(tree1, enum1), cons_enum(tree2, enum2));
                  };

                if (veq(v1, v2)) {
                  step(~acc, ~state=next_state);
                } else {
                  step(
                    ~acc=f((k1, Unequal(v1, v2)), acc),
                    ~state=next_state,
                  );
                };
              } else if (compare_result < 0) {
                step(
                  ~acc=f((k1, Left(v1)), acc),
                  ~state=(cons_enum(tree1, enum1), right),
                );
              } else {
                step(
                  ~acc=f((k2, Right(v2)), acc),
                  ~state=(left, cons_enum(tree2, enum2)),
                );
              };
            };
          };
          step(
            ~state=(of_tree(t1 |> impl_of_t), of_tree(t2 |> impl_of_t)),
            ~acc,
          );
        };
    };
};

module Set = {
  module type S = {
    include Set.S;
    type diff_result =
      | Left(elt)
      | Right(elt);
    let symmetric_diff:
      (t, t, ~f: (diff_result, 'acc) => 'acc, ~acc: 'acc) => 'acc;
  };
  module Make: (Ord: Set.OrderedType) => S with type elt = Ord.t =
    (Ord: Set.OrderedType) => {
      include Set.Make(Ord);

      type diff_result =
        | Left(elt)
        | Right(elt);

      // Keep in sync with Set.Make.t
      type t_i =
        | Empty
        | Node(t_i, elt, t_i, int);

      external impl_of_t: t => t_i = "%identity";

      type enumeration_i =
        | End
        | More(elt, t_i, enumeration_i);

      let rec cons_enum = (s, e) =>
        switch (s) {
        | Empty => e
        | [@implicit_arity] Node(l, v, r, _) =>
          cons_enum(l, [@implicit_arity] More(v, r, e))
        };

      let of_set = t => cons_enum(t, End);

      let symmetric_diff:
        (t, t, ~f: (diff_result, 'acc) => 'acc, ~acc: 'acc) => 'acc =
        (t1, t2, ~f, ~acc) => {
          let rec step = (~state, ~acc) => {
            switch (state) {
            | (End, End) => acc
            | (End, More(elt, tree, enum)) =>
              step(
                ~acc=f(Right(elt), acc),
                ~state=(End, cons_enum(tree, enum)),
              )
            | (More(elt, tree, enum), End) =>
              step(
                ~acc=f(Left(elt), acc),
                ~state=(cons_enum(tree, enum), End),
              )
            | (
                More(a1, tree1, enum1) as left,
                More(a2, tree2, enum2) as right,
              ) =>
              let compare_result = Ord.compare(a1, a2);
              if (compare_result == 0) {
                let next_state =
                  if (tree1 === tree2) {
                    (enum1, enum2);
                  } else {
                    (cons_enum(tree1, enum1), cons_enum(tree2, enum2));
                  };

                step(~acc, ~state=next_state);
              } else if (compare_result < 0) {
                step(
                  ~acc=f(Left(a1), acc),
                  ~state=(cons_enum(tree1, enum1), right),
                );
              } else {
                step(
                  ~acc=f(Right(a2), acc),
                  ~state=(left, cons_enum(tree2, enum2)),
                );
              };
            };
          };
          step(
            ~state=(of_set(t1 |> impl_of_t), of_set(t2 |> impl_of_t)),
            ~acc,
          );
        };
    };
};