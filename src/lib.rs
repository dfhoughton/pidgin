/*!

This crate provides a library for generating efficient regular expressions
represent a non-recursive grammar and a mechanism to build a parse tree from capturing groups in
the expression. It uses the [`regex`](https://crates.io/crates/regex) crate for its parsing engine.

# Usage

This crate is [on crates.io](https://crates.io/crates/pidgin) and can be
used by adding `pidgin` to your dependencies in your project's `Cargo.toml`.

```toml
[dependencies]
pidgin = "1.0.0"
```

and this to your crate root:

```rust
#[macro_use]
extern crate pidgin;
# fn main() {}
```

# Example: find a date

```rust
# #![recursion_limit="256"]
# #[macro_use]
# extern crate pidgin;
# fn main() {
let date = grammar!{
    (?ibB)

    date -> <weekday> (",") <month> <monthday> (",") <year>
    date -> <month> <monthday> | <weekday> | <monthday> <month> <year>
    date -> <month> <monthday> (",") <year>
    date -> <numeric_date>

    numeric_date -> <year> ("/") <numeric_month> ("/") <numeric_day>
    numeric_date -> <year> ("-") <numeric_month> ("-") <numeric_day>
    numeric_date -> <numeric_month> ("/") <numeric_day> ("/") <year>
    numeric_date -> <numeric_month> ("-") <numeric_day> ("-") <year>
    numeric_date -> <numeric_day> ("/") <numeric_month> ("/") <year>
    numeric_date -> <numeric_day> ("-") <numeric_month> ("-") <year>

    year    => r(r"\b[12][0-9]{3}|[0-9]{2}\b")
    weekday => [
            "Sunday Monday Tuesday Wednesday Thursday Friday Saturday"
                .split(" ")
                .into_iter()
                .flat_map(|s| vec![s, &s[0..2], &s[0..3]])
                .collect::<Vec<_>>()
        ]
    weekday     => (?-i) [["M", "T", "W", "R", "F", "S", "U"]]
    monthday    => [(1..=31).into_iter().collect::<Vec<_>>()]
    numeric_day => [
            (1..=31)
                .into_iter()
                .flat_map(|i| vec![i.to_string(), format!("{:02}", i)])
                .collect::<Vec<_>>()
        ]
    month => [
        vec![
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
        ].into_iter().flat_map(|s| vec![s, &s[0..3]]).collect::<Vec<_>>()
      ]
    numeric_month => [
            (1..=31)
                .into_iter()
                .flat_map(|i| vec![i.to_string(), format!("{:02}", i)])
                .collect::<Vec<_>>()
        ]
};
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
# }
```

This macro is the raison d'etre of pidgin. It gives you a [`Grammar`] which can itself be used in other
[`Grammar`]s via the `g(grammar)` element, it can server as a library of [`Grammar`]s via the [`rule`] method,
or via its [`matcher`] method it can give you a [`Matcher`] object which will allow you to
parse a string to produce a [`Match`] parse tree.

[`Grammar`]: ../pidgin/struct.Grammar.html
[`rule`]: ../pidgin/struct.Grammar.html#method.rule
[`matcher`]: ../pidgin/struct.Grammar.html#method.matcher
[`Matcher`]: ../pidgin/struct.Matcher.html
[`Match`]: ../pidgin/struct.Match.html
*/

extern crate regex;
#[macro_use]
extern crate lazy_static;

mod grammar;
#[macro_use]
#[doc(hidden)]
pub mod macros;
mod matching;
mod pidgin;
mod util;
pub use self::grammar::Grammar;
pub use self::matching::{Match, Matcher};
