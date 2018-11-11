# pidgin
build non-recursive grammars

Pidgin is a Rust library for generating non-recursive grammars which allow one to
convert strings to parse trees.

Pidgin's grammars are implemented underlyingly as regular expressions with
named matching groups. Rust's regular expression engine is very fast but it has
a limitation which makes parsing hierarchically structured patterns with
repeating components difficult: you cannot use a particular match name more
than once in a pattern. This makes a simple grammar such as
```
foo -> bar baz | baz bar
bar -> '1'
baz -> '2'
```
problematic. The `foo` rule uses both `bar` and `baz` twice.

Pidgin allows you to work around this restriction by managing group renaming.

Another disadvantage of regular expressions is that the more expressive they
are the more difficult they are to read. Pidgin allows you to construct
expressive regular expressions without obscuring your intention.

## Example

```rust
#![recursion_limit="256"]
#[macro_use]
extern crate pidgin;

fn experiment() -> Result<(), Box<Error>> {
	let date = grammar!{
	
		// comments are legal in grammars
	
	    (?ibB)   // default flags for all rules -- case insensitive and enforce leading and trailing word boundaries
	
		// the master rule; it has multiple alternates
	    date -> <weekday> (",") <month> <monthday> (",") <year>
	    date -> <month> <monthday> | <weekday> | <monthday> <month> <year>
	    date -> <month> <monthday> (",") <year>
	    date -> <numeric_date>
	
		// sub-rules
	
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

	// compile this into a matcher
	// (uncompiled grammar's can be used as elements of other grammars)
	let matcher = date.matcher()?;
	
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

    Ok(())
}
```

## Limitations

### Recursion
Because you must define sub-grammars before you use them, and because Rust's
regular expression engine and formalism cannot represent it in any case, you
cannot produce a recursive grammar in Pidgin. Something like this is impossible:
```
XP -> XP conj XP
```

### Named Captures in Regex Elements

Pidgin allows you to provide regular expression elements to rules. Such an element
is a Rust expression which may be converted to a `String` via `to_string` bracketed
in the `grammar!` macro body by `r(` and `)`, so, for example `r(r"\d+")`. Regex
elements allow you to regex expressions into the grammar that the grammar formalism
doesn't provide itself, like anchors and character classes.

Rust's regular expression engine does not allow the reuse of names in named capturing
groups, however. `(?<foo>bar) baz (?<foo>qux)` won't compile into a usable regex because
the name "foo" is reused. The following grammar produces just this situation:

```rust
grammar!{
	TOP -> <foo> ("bar") <foo>
	foo => r("(?P<baz>foo)")
};
```

The solution is to avoid named capturing groups in grammar regex elements. The grammar
itself will handle the named capturing, so you should never need to do this.

## Benchmarks

The principle motivation behind Pidgin is simply to produce something like an
abstract syntax tree one can use to better understand matched text, but the
regular expressions its `[vec]` elements generate generally match as well as or better than
naively constructed regular expressions represented as alternations of
expressions.

```rust
// good
let rx = regex::Regex::new("cat|camel|canteloupe").unwrap();

// better!
let rx = grammar!{ TOP => [["cat", "camel", "canteloupe"]] }.rx().unwrap();
```


There is a simple benchmark suite included in the `benches/`
directory demonstrating this. The suite compares matching
speed for "naive" regular expressions of the form `foo|bar|baz` to those
generated by Pidgin, which involve no backtracking and, in this case, would
be something like `foo|ba[rz]`. The benchmark suite compares non-matching to
matching and bounded to unbounded patterns, where a bounded pattern must match
from the first character to the last and an unbounded pattern must match
somewhere in the string searched. The mean match/non-match times for these 8
cases were

|pidgin|match|bounded|time|
|:------:|:-----:|:-------:|:----:|
|✓|✓| |**4.8857 ms**|
| |✓| |26.189 ms|
|✓| | |**15.930 ms**|
| | | |156.43 ms|
|✓|✓|✓|**2.2982 m**s|
| |✓|✓|3.9700 ms|
|✓| |✓|**443.04 us**|
| | |✓|1.0646 ms|

The better time is bolded for each pair. The Pidgin regex outperforms the
naive regex in every case.

The full report can be generated by running

```bash
cargo bench
```

## Full API

The complete Pidgin API is available at https://docs.rs/pidgin/.

## Acknowledgments

In writing this I received considerable advice from [TurkeyMcMac](https://github.com/TurkeyMcMac).
