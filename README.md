# pidgin
build non-recursive grammars

Pidgin is a Rust port of [list_matcher](https://github.com/dfhoughton/list_matcher).
It can be used to generate simple, non-recursive grammars which allow one to
obtain structured matches: parse trees.

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
expressive regular expressions programmatically such that the intention of
the pattern doesn't become obscured.

## Example

```rust
use pidgin::Pidgin;
use std::error::Error;

fn experiment() -> Result<(), Box<Error>> {
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
    p.foreign_rule("year", "[12][0-9]{3}|[0-9]{2}")?;
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

    println!("{}", date);

    Ok(())
}
```

The final `println!` in the example above produces a BNF-esque description of the
grammar:

```
           TOP := (?i) \b{monthday}(?-i:\s+){month}(?-i:\s+){year}|{month}(?-i:\s+){monthday}(:?,(?-i:\s+){year})?|{weekday}(:?,(?-i:\s+){month}(?-i:\s+){monthday},(?-i:\s+){year})?|{numeric_date}\b
      monthday := (?i) \b[4-9]|30?|1[0-9]?|2[0-9]?\b
         month := (?i) \bMa(?:y|r(:?ch)?)|Oct(:?ober)?|Dec(:?ember)?|Feb(:?ruary)?|Nov(:?ember)?|Sep(:?tember)?|A(?:pr(:?il)?|ug(:?ust)?)|J(?:u(?:ly?|ne?)|an(:?uary)?)\b
          year :=      [12][0-9]{3}|[0-9]{2}
       weekday := (?i) (?:\b(?:Fr(:?i(:?day)?)?|Mo(:?n(:?day)?)?|We(:?d(:?nesday)?)?|S(?:u(:?n(:?day)?)?|a(:?t(:?urday)?)?)|T(?:u(:?e(:?sday)?)?|h(:?u(:?rsday)?)?))\b)|(?-i:\b[FMR-UW]\b)
  numeric_date := (?i) \b{year}(?-i:\s*)(?:\-(?-i:\s*){numeric_months}(?-i:\s*)\-|/(?-i:\s*){numeric_months}(?-i:\s*)/)(?-i:\s*){numeric_days}|{numeric_days}(?-i:\s*)(?:\-(?-i:\s*){numeric_months}(?-i:\s*)\-|/(?-i:\s*){numeric_months}(?-i:\s*)/)(?-i:\s*){year}|{numeric_months}(?-i:\s*)(?:\-(?-i:\s*){numeric_days}(?-i:\s*)\-|/(?-i:\s*){numeric_days}(?-i:\s*)/)(?-i:\s*){year}\b
  numeric_days := (?i) \b[4-9]|30?|0[1-9]|1[0-9]?|2[0-9]?\b
numeric_months := (?i) \b[2-9]|1[01]?|0[1-9]\b
```

## Limitations

### Recursion
Because you must define sub-grammars before you use them, and because Rust's
regular expression engine and formalism cannot represent it in any case, you
cannot produce a recursive grammar in Pidgin. Something like this is impossible:
```
XP -> XP conj XP
```
### Unbounded Repetition
Currently you can produce something like this
```
foo -> bar{1,3}
```
but not
```
foo -> bar+
```
I am planning to introduce Pidgin macros which will allow this, but I am not
sure when those will arrive.

### Order of Definition
Typically a grammar is defined top down like so:
```
TOP   -> foo | bar
foo   -> baz plugh | xyzzy
bar   -> xyzzy baz plugh
baz   -> '1' | '2'
plugh -> 'cat' | 'dog'
xyzzy -> 'green' | 'blue' | 'grue'
```
Because in Pidgin sub-grammars need to be defned before they can be used in the
definition of other rules, construction has to go bottom up.

I am planning to write Pidgin macros which will allow one to define a grammar
top-down, but this is currently still in the pie-in-the-sky stage.

## Gotchas

### Named Captures in Foreign Rules

Pidgin does not generate unbounded repetitions and it only attempts to match
the examples you present it. Sometimes this is inconvenient: you want to match
all phone numbers but you don't want to provide all phone numbers as patterns,
say. To work around this you can supply a "foreign rule". This is just a regular
expression you have acquired outside of pidgin.
```rust
pidgin.foreign_rule("local_us_phone", r"\d{3}-?\d{4}").unwrap();
```
The `unwrap` is necessary in this case because your expression might not
compile, so `foreign_rule` returns a `Result<(),regex::Error>` so you can
handle this exception.

This is all fine, but suppose you provide a foreign rule with a named capture:
```rust
pidgin.foreign_rule("foo", "(?<foo>bar)").unwrap();
```
This will compile when you define the rule, but if you use `foo` in more than
one place in your grammar, this will cause a `regex::Error` when you attempt
to generate a `pidgin::Matcher` from your grammar.

### Manual Rule Ordering

Pidgin attempts alternations easiest to hardest. In effect it always tries to
pick low-hanging fruit. This means if told to match "elephant" and "cat" it
will generate the pattern `cat|elephant`. Generally this is what you want, but
it may happen that you want to prioritize rules differently. Pidgin provides
a mechanism for this: if you at the same rule more than once, this makes the
rule an alternation with the alternate order you specify:
```rust
pidgin.rule("foo", &bar);
pidgin.rule("foo", &baz);
```
This is all fine and good, but it means you cannot replace one rule with
another simply by redefining it. If you wish to do this, you must first remove
the rule:
```rust
pidgin.remove_rule("foo");
```

### Phrase List Clearing on Compilation

Because the general pattern of use for Pidgin is that you compile a bunch of
sub-grammars and then use them in other grammars, and because you generall don't
want the alternates in one sub-grammar to be included in the next you compile,
the `compile` method clears the phrase list. It does not clear the rule list.
You usually don't *want* to clear the rule list, but there is a `clear` method
which clears both.

The `clear` method does not clear any flags you have set. If you want that as
well, you're better off just building a fresh Pidgin.

## Effect on Matching Efficiency

The principle motivation behind Pidgin is simply to produce something like an
abstract syntax tree one can use to better understand matched text, but the
regular expressions it generates generally match as well as or better than
naively constructed regular expressions represented as alternations of
expressions. There is a simple benchmark suite included in the `benches/`
directory demonstrating this for certain cases. The suite compares matching
speed for "naive" regular expressions of the form `foo|bar|baz` to those
generated by Pidgin, which involve no backtracking and, in this case, would
be something like `foo|ba[rz]`. The benchmark suite compares non-matching to
matching and bounded to unbounded patterns, where a bounded pattern must match
from the first character to the last and an unbounded pattern must match
somewhere in the string searched. The mean match/non-match times for these 8
cases were

|pidgin|match|bounded|time|
|:------:|:-----:|:-------:|:----:|
|✓|✓| |5.1287 ms|
| |✓| |27.151 ms|
|✓| | |15.757 ms|
| | | |168.24 ms|
|✓|✓|✓|3.0683 ms|
| |✓|✓|4.0253 ms|
|✓| |✓|5.0138 ms|
| | |✓|1.5124 ms|

The full report can be generated by running

```bash
cargo bench
```
