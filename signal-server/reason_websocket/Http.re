/* This file was taken from https://github.com/jaredly/reason-websocket */
/*
 * Copyright (c) 2018 Jared Forsyth <jared@jaredforsyth.com>
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
 */

let parse_top = top => {
  let parts = Str.split(Str.regexp("[ \t]+"), top);
  switch (parts) {
  | [method, path, ..._others] => Some((method, path))
  | _ => None
  };
};

module StringMap = Map.Make(String);

let parse_headers = headers => {
  List.fold_left(
    (map, line) => {
      let parts = Str.split(Str.regexp(":"), line);
      switch (parts) {
      | []
      | [_] => map
      | [name, ...rest] =>
        StringMap.add(name, String.concat(":", rest), map)
      };
    },
    StringMap.empty,
    headers,
  );
};

let parse_request = text => {
  let items = Str.split(Str.regexp("\r?\n"), text);
  switch (items) {
  | [] => failwith("Invalid request")
  | [top, ...headers] =>
    switch (parse_top(top)) {
    | None => failwith("Invalid top: " ++ top)
    | Some((method, path)) =>
      let header_map = parse_headers(headers);
      (method, path, header_map);
    }
  };
};