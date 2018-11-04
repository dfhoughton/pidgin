#![feature(test)]
#![recursion_limit = "256"]
#[macro_use]
extern crate pidgin;

#[test]
fn foo_bar() {
    let g = grammar!{
        foo => ("bar")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("bar"), g.describe());
    let p = matcher.parse("bar").unwrap();
    assert_eq!(p.as_str(), "bar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
}

#[test]
fn foo_foo() {
    let g = grammar!{
        foo => ("foo")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foo"), g.describe());
    let p = matcher.parse("foo").unwrap();
    assert_eq!(p.as_str(), "foo");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
}

#[test]
fn foo_foo2() {
    let g = grammar!{
        foo => ("foo"){2}
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foofoo"), g.describe());
    let p = matcher.parse("foofoo").unwrap();
    assert_eq!(p.as_str(), "foofoo");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
}

#[test]
fn foo_maybe_bar() {
    let g = grammar!{
        foo => ("foo")? ("bar")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foobar"), g.describe());
    let p = matcher.parse("foobar").unwrap();
    assert_eq!(p.as_str(), "foobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("bar"), g.describe());
    let p = matcher.parse("bar").unwrap();
    assert_eq!(p.as_str(), "bar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
}

#[test]
fn foo_star_bar() {
    let g = grammar!{
        foo => ("foo")* ("bar")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foofoobar"), g.describe());
    let p = matcher.parse("foofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("foobar"), g.describe());
    let p = matcher.parse("foobar").unwrap();
    assert_eq!(p.as_str(), "foobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("bar"), g.describe());
    let p = matcher.parse("bar").unwrap();
    assert_eq!(p.as_str(), "bar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
}

#[test]
fn foo_plus_bar() {
    let g = grammar!{
        foo => ("foo")+ ("bar")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foofoobar"), g.describe());
    let p = matcher.parse("foofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("foobar"), g.describe());
    let p = matcher.parse("foobar").unwrap();
    assert_eq!(p.as_str(), "foobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(!matcher.is_match("bar"), g.describe());
}

#[test]
fn foo_at_least_2_bar() {
    let g = grammar!{
        foo => ("foo"){2,} ("bar")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foofoobar"), g.describe());
    let p = matcher.parse("foofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("foofoofoobar"), g.describe());
    let p = matcher.parse("foofoofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(!matcher.is_match("foobar"), g.describe());
}

#[test]
fn foo_at_2_to_4_bar() {
    let g = grammar!{
        foo => ("foo"){2,4} ("bar")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foofoobar"), g.describe());
    let p = matcher.parse("foofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("foofoofoobar"), g.describe());
    let p = matcher.parse("foofoofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("foofoofoofoobar"), g.describe());
    let p = matcher.parse("foofoofoofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoofoofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("foofoofoofoofoobar"), g.describe());
    let p = matcher.parse("foofoofoofoofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoofoofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(!matcher.is_match("foobar"), g.describe());
}

#[test]
fn foo_bar_foo() {
    let g = grammar!{
        foo => ("bar") | ("foo")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("bar"), g.describe());
    let p = matcher.parse("bar").unwrap();
    assert_eq!(p.as_str(), "bar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("foo"), g.describe());
    let p = matcher.parse("foo").unwrap();
    assert_eq!(p.as_str(), "foo");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
}

#[test]
fn rule_specific_flags() {
    let g = grammar!{
        TOP => <foo> <bar>
        foo => (?i) ("foo")
        bar => ("bar")
    };
    let matcher = g.matcher().unwrap();
    let p = matcher.parse("foobar").unwrap();
    assert_eq!(p.as_str(), "foobar");
    assert!(p.name("foo").is_some());
    assert!(p.name("bar").is_some());
    let p = matcher.parse("FOObar").unwrap();
    assert_eq!(p.as_str(), "FOObar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.name("foo").unwrap().as_str(), "FOO");
    assert!(p.name("bar").is_some());
    assert!(!matcher.is_match("fooBAR"));
}

#[test]
fn rule_general_flags() {
    let g = grammar!{
        (?i)
        TOP => <foo> <bar>
        foo => ("foo")
        bar => ("bar")
    };
    let matcher = g.matcher().unwrap();
    let p = matcher.parse("foobar").unwrap();
    assert_eq!(p.as_str(), "foobar");
    assert!(p.name("foo").is_some());
    assert!(p.name("bar").is_some());
    let p = matcher.parse("FOObar").unwrap();
    assert_eq!(p.as_str(), "FOObar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.name("foo").unwrap().as_str(), "FOO");
    assert!(p.name("bar").is_some());
    let p = matcher.parse("fooBAR").unwrap();
    assert_eq!(p.as_str(), "fooBAR");
    assert!(p.name("foo").is_some());
    assert_eq!(p.name("foo").unwrap().as_str(), "foo");
    assert!(p.name("bar").is_some());
    assert_eq!(p.name("bar").unwrap().as_str(), "BAR");
}

#[test]
fn rule_general_and_specific_flags() {
    let g = grammar!{
        (?i)
        TOP => <foo> <bar>
        foo => ("foo")
        bar => (?-i) ("bar")
    };
    let matcher = g.matcher().unwrap();
    let p = matcher.parse("foobar").unwrap();
    assert_eq!(p.as_str(), "foobar");
    assert!(p.name("foo").is_some());
    assert!(p.name("bar").is_some());
    let p = matcher.parse("FOObar").unwrap();
    assert_eq!(p.as_str(), "FOObar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.name("foo").unwrap().as_str(), "FOO");
    assert!(p.name("bar").is_some());
    println!("g: {}", g);
    println!("rx: {}", matcher.rx);
    assert!(!matcher.is_match("fooBAR"));
}

#[test]
fn multiple_definitions() {
    let g = grammar!{
        foo => ("foo")
        foo => ("bar")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foo"));
    assert!(matcher.is_match("bar"));
}

#[test]
fn regex_rule() {
    let g = grammar!{
        foo => r(r"\d+")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("123"));
}

#[test]
fn vec_rule_simple() {
    let words = vec!["cat", "camel", "canteloupe"];
    let g = grammar!{
        foo => [&words]
    };
    let matcher = g.matcher().unwrap();
    for w in words {
        assert!(matcher.is_match(w));
    }
}

#[test]
fn vec_rule_with_repetition() {
    let words = vec!["cat", "camel", "canteloupe"];
    let g = grammar!{
        foo => [&words]+
    };
    let matcher = g.matcher().unwrap();
    for w in words {
        let p = matcher.parse(w).unwrap();
        assert_eq!(p.as_str(), w);
        let t = &w.repeat(2);
        let p = matcher.parse(t).unwrap();
        assert_eq!(p.as_str(), t);
        let t = &w.repeat(3);
        let p = matcher.parse(t).unwrap();
        assert_eq!(p.as_str(), t);
    }
    let t = "catcamelcanteloupe";
    let p = matcher.parse(t).unwrap();
    assert_eq!(p.as_str(), t);
}

#[test]
fn word_bound_left() {
    let words = vec!["cat", "@cat"];
    let g = grammar!{
        foo => (?b) [&words]
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("cat"));
    assert!(matcher.is_match("@cat"));
    assert!(matcher.is_match("catatonia"));
    assert!(matcher.is_match("a@cat"));
    assert!(!matcher.is_match("scat"));
}

#[test]
fn word_bound_right() {
    let words = vec!["cat", "cat@"];
    let g = grammar!{
        foo => (?B) [&words]
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("cat"));
    assert!(matcher.is_match("cat@"));
    assert!(matcher.is_match("scat"));
    assert!(matcher.is_match("cat@s"));
    assert!(!matcher.is_match("catatonia"));
}

#[test]
fn some_space() {
    let words = vec!["cat a log"];
    let g = grammar!{
        foo => (?w) [&words]
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("cat  a   log"));
    assert!(!matcher.is_match("catalog"));
}

#[test]
fn maybe_space() {
    let words = vec!["cat a log"];
    let g = grammar!{
        foo => (?W) [&words]
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("cat  a   log"));
    assert!(matcher.is_match("catalog"));
}

#[test]
fn stingy() {
    let g = grammar!{
        foo => ("foo"){1,3}?
    };
    let matcher = g.matcher().unwrap();
    let p = matcher.parse("foofoofoo").unwrap();
    assert_eq!(p.as_str(), "foo");
}

#[test]
fn optional_space() {
    let g = grammar!{
        foo -> ("foo") ("bar")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foobar"));
    assert!(matcher.is_match("foo bar"));
}

#[test]
fn optional_space_with_repetition() {
    let g = grammar!{
        foo -> ("foo")+
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foofoo"));
    assert!(matcher.is_match("foo foofoo foo"));
}

#[test]
fn foreign_grammar() {
    let g = grammar!{
        thing => (?bB) [&vec!["cat", "crouton", "caveman", "pomegranate"]]
    };
    let g = grammar!{
        things -> g(g)+
    };
    let matcher = g.matcher().unwrap();
    let p = matcher.parse("cat caveman").unwrap();
    assert!(p.name("thing").is_some());
    assert_eq!(p.as_str(), "cat caveman");
    assert_eq!(
        p.name("thing").unwrap().as_str(),
        "caveman",
        "thing is last thing"
    );
}

#[test]
fn complex_example() {
    let mdays = &(1..=31)
        .into_iter()
        .map(|i| i.to_string())
        .collect::<Vec<_>>();
    let g = grammar!{
        (?i)
        // top rule -- each thing matched against is expected only to be a time expression
        time -> r(r"\A") <type> r(r"\z")

        // sub-rules
        type           => <relative> | <absolute>
        relative       -> <modifier> <unit> | <modifier> <period>
        period         => <weekday> | <month>
        absolute       => <month_day>
        absolute       => <month_day> | <day_month_year> | <month_year>
        absolute       => <month_day_year> | <year>
        month_day      -> <month> <mday>
        day_month_year -> <mday> <month> <year>
        month_year     -> <month> <year>
        month_day_year -> <month> <mday> (",") <year>

        // leaves
        mday     => (?bB) [&mdays.iter().map(|s| s.as_str()).collect::<Vec<_>>()]
        modifier => (?bB) [&vec!["this", "last", "next"]]
        unit     => (?bB) [&vec!["day", "week", "month", "year"]]
        year     => r(r"\b\d{4}\b")
        weekday  => (?bB) [&vec![
                            "sunday",
                            "monday",
                            "tuesday",
                            "wednesday",
                            "thursday",
                            "friday",
                            "saturday"
                          ]]
        month    => (?bB) [&vec![
                            "january",
                            "february",
                            "march",
                            "april",
                            "may",
                            "june",
                            "july",
                            "august",
                            "september",
                            "october",
                            "november",
                            "december",
                          ]]
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("May 6, 1969"));
    assert!(matcher.is_match("May 6"));
    assert!(matcher.is_match("1969"));
    assert!(matcher.is_match("last Saturday"));
    let p = matcher.parse("May 6, 1969").unwrap();
    assert!(p.name("absolute").is_some());
    assert!(p.name("month").is_some());
}
