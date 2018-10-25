let getTimeoutMs = attemptsMade => {
  let timeoutSec =
    switch (attemptsMade) {
    | c when c <= 4 => 2
    | c when c <= 4 + 6 => 5
    | _ => 10
    };
  timeoutSec * 1000;
};