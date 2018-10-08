/*!

This crate provides a library for generating efficient regular expressions
programmatically that can represent a simple, non-recursive grammar. It uses
the [`regex`](https://crates.io/crates/regex) crate for its parsing engine.

# Usage

This crate is [on crates.io](https://crates.io/crates/pidgin) and can be
used by adding `pidgin` to your dependencies in your project's `Cargo.toml`.

```toml
[dependencies]
pidgin = "0.1"
```

and this to your crate root:

```rust
extern crate pidgin;
```

# Example: find a date

This is like the `regex` example, but considerably more expressive, and once
you've matched a date with a `Pidgin` matcher it is easier to determine *how*
you matched it and thus convert the match into useful semantics.

```rust
use pidgin::Pidgin;

// set up the initial building for our pidgin grammar
let mut p = Pidgin::new()
    .enclosed(true)
    .word_bound()
    .case_insensitive(true);
// you can build word lists and add them in
let weekdays = vec![
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday",
];
p.add(&weekdays);
// various abbreviations
for s in weekdays {
    p.add_str(&s[0..3]);
    p.add_str(&s[0..2]);
}
let g = p.compile();
p.rule("weekday", &g);
// for these ones we care about case
p = p.case_insensitive(false);
// you can build and compile all in one go
let g = p.grammar(&vec!["M", "T", "W", "R", "F", "S", "U"]);
// add a case to an existing rule
p.rule("weekday", &g);
// back to case insensitivity
p = p.case_insensitive(true);
// we can add words to a rule piecemeal
let months = vec![
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
];
p.add(&months);
for m in months {
    p.add_str(&m[0..3]);
}
let g = p.compile();
p.rule("month", &g);
for i in 1..31 {
    p.add_str(i.to_string().as_str());
}
let g = p.compile();
p.rule("monthday", &g);
for i in 1..31 {
    p.add_str(i.to_string().as_str());
    // allow both 1 and 01, etc.
    // adding a word such as "10" twice has no ill effect
    p.add_str(&format!("{:02}", i));
}
let g = p.compile();
p.rule("numeric_days", &g);
for i in 1..12 {
    p.add_str(i.to_string().as_str());
    p.add_str(&format!("{:02}", i));
}
let g = p.compile();
p.rule("numeric_months", &g);
// sometimes you may need to add in a handwritten regex
// take care with named groups -- names cannot be repeated
p.foreign_rule("year", "[12][0-9]{3}|[0-9]{2}").unwrap();
// for the following patterns make whitespace optional
p = p.normalize_whitespace(false);
let g = p.grammar(&vec![
    "year / numeric_months / numeric_days",
    "numeric_months / numeric_days / year",
    "numeric_days / numeric_months / year",
    "year - numeric_months - numeric_days",
    "numeric_months - numeric_days - year",
    "numeric_days - numeric_months - year",
]);
p.rule("numeric_date", &g);
// for the remaining rules, whitespace is required if present
p = p.normalize_whitespace(true);
// and finally, the pattern we've been working towards
let date = p.grammar(&vec![
    "weekday, month monthday, year",
    "month monthday",
    "weekday",
    "monthday month year",
    "month monthday, year",
    "numeric_date",
]);

// now test it

let matcher = date.matcher().unwrap();

// we let whitespace vary
assert!(matcher.is_match(" June   6,    1969 "));
// we made it case-insensitive
assert!(matcher.is_match("june 6, 1969"));
// but we want to respect word boundaries
assert!(!matcher.is_match("jejune 6, 1969"));
// we can inspect the parse tree
let m = matcher.parse("2018/10/6").unwrap();
assert!(m.name("numeric_date").is_some());
assert_eq!(m.name("year").unwrap().as_str(), "2018");
let m = matcher.parse("Friday").unwrap();
assert!(!m.name("numeric_date").is_some());
assert!(m.name("weekday").is_some());
// still more crazy things we allow
assert!(matcher.is_match("F"));
assert!(matcher.is_match("friday"));
assert!(matcher.is_match("Fri"));
// but we said single-letter days had to be capitalized
assert!(!matcher.is_match("f"));
```

*/

extern crate regex;
#[macro_use]
extern crate lazy_static;

mod grammar;
mod matching;
mod pidgin;
mod util;
pub use grammar::Grammar;
pub use matching::{Match, Matcher};
pub use pidgin::Pidgin;
