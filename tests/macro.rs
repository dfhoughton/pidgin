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
        foo => [words]
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
        foo => [words]+
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
    let g = grammar!{
        foo => (?b) [["cat", "@cat"]]
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
    let g = grammar!{
        foo => (?B) [["cat", "cat@"]]
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
    let g = grammar!{
        foo => (?w) [["cat a log"]]
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("cat  a   log"));
    assert!(!matcher.is_match("catalog"));
}

#[test]
fn maybe_space() {
    let g = grammar!{
        foo => (?W) [["cat a log"]]
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
        thing => (?bB) [["cat", "crouton", "caveman", "pomegranate"]]
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
        absolute       => <month_day> | <day_month_year> | <month_year>
        absolute       => <month_day_year> | <year>
        month_day      -> <month> <mday>
        day_month_year -> <mday> <month> <year>
        month_year     -> <month> <year>
        month_day_year -> <month> <mday> (",") <year>

        // leaves
        mday     => (?bB) [mdays]
        modifier => (?bB) [["this", "last", "next"]]
        unit     => (?bB) [["day", "week", "month", "year"]]
        year     => r(r"\b\d{4}\b")
        weekday  => (?bB) [[
                            "sunday",
                            "monday",
                            "tuesday",
                            "wednesday",
                            "thursday",
                            "friday",
                            "saturday"
                          ]]
        month    => (?bB) [[
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

#[test]
fn using_sub_rule() {
    let library = grammar!{
        books => <cat> | <dog> | <camel>
        cat   => [["persian", "siamese", "calico", "tabby"]]
        dog   => [["dachshund", "chihuahua", "corgi", "malamute"]]
        camel => [["bactrian", "dromedary"]]
    };
    let g = grammar!{
        seen -> ("I saw a") g(library.rule("cat").unwrap()) (".")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("I saw a calico."));
}

#[test]
fn rx() {
    let g = grammar!{
        foo -> r(r"\A") <bar> r(r"\z")
        bar => (?i) [["cat", "camel", "corn"]]
    };
    let rx = g.rx().unwrap().to_string();
    assert_eq!(r"\A(?i:\s*c(?:orn|a(?:t|mel)))\s*\z", rx);
}

#[test]
fn rx_example() {
    let g = grammar!{
        sentence    -> <capitalized_word> <other_words>? <terminal_punctuation>
        other_words -> <other_word>+
        other_word  -> <non_terminal_punctuation>? <word>
        capitalized_word         => r(r"\b[A-Z]\w*\b")
        word                     => r(r"\b\w+\b")
        terminal_punctuation     => r(r"[.?!]")
        non_terminal_punctuation => r("(?:--?|[,;'\"])")
    };
    let rx = g.rule("word").unwrap().rx().unwrap();
    let p = g
        .matcher()
        .unwrap()
        .parse("John, don't forget to pick up chips.")
        .unwrap();
    let other_words = p.name("other_words").unwrap().as_str();
    let other_words = rx
        .find_iter(other_words)
        .map(|m| m.as_str())
        .collect::<Vec<_>>();
    assert_eq!(
        vec!["don", "t", "forget", "to", "pick", "up", "chips"],
        other_words
    );
}

#[test]
fn namespace_collision() {
    let g1 = grammar!{
        foo => ("bar")
    };
    let g2 = grammar!{
        words -> <word>+
        word  => <foo> | <bar>
        foo   => ("baz")
        bar   => g(g1)
    };
    let matcher = g2.matcher().unwrap();
    let p = matcher.parse("bar baz").unwrap();
    assert_eq!(
        vec!["bar", "baz"],
        p.all_names("foo")
            .iter()
            .map(|m| m.as_str())
            .collect::<Vec<_>>()
    );
}

#[test]
fn messing_about() {
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
}
