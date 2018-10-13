#![feature(test)]
extern crate lazy_static;
extern crate pidgin;
use pidgin::{gf, sf, Grammar, Pidgin};
extern crate regex;
use regex::Regex;

fn all_equal(words: &[&str], pattern: &Grammar) {
    let rx = pattern.matcher().unwrap();
    for w in words {
        assert!(rx.is_match(w))
    }
}

#[test]
fn simple_alternation() {
    let words = vec!["cat", "dog", "camel"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    all_equal(&words, &pattern);
}

#[test]
fn common_suffix() {
    let words = vec!["cats", "dogs"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert!(pattern.to_string().as_str().ends_with("s"));
    all_equal(&words, &pattern);
}

#[test]
fn common_prefix() {
    let words = vec!["scat", "spore"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert!(pattern.to_string().as_str().starts_with("s"));
    all_equal(&words, &pattern);
}

#[test]
fn both_prefix_and_suffix() {
    let words = vec!["scats", "spores"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    let rx = pattern.to_string();
    assert!(rx.as_str().starts_with("s"));
    assert!(rx.as_str().ends_with("s"));
    all_equal(&words, &pattern);
}

#[test]
fn short_character_class() {
    let words = vec!["a", "b"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("[ab]", &pattern.to_string());
    all_equal(&words, &pattern);
}

#[test]
fn long_character_class() {
    let words = vec!["a", "b", "c"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("[a-c]", &pattern.to_string());
    all_equal(&words, &pattern);
}

#[test]
fn complex_character_class() {
    let words = vec!["a", "b", "c", "g"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("[a-cg]", &pattern.to_string());
    all_equal(&words, &pattern);
}

#[test]
fn character_class_in_alternation() {
    let words = vec!["Ant", "a", "b", "c", "g"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("(?:[a-cg]|Ant)", &pattern.to_string());
    all_equal(&words, &pattern);
}

#[test]
fn small_repeat_ignored() {
    let words = vec!["aaa"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("aaa", &pattern.to_string());
    all_equal(&words, &pattern);
}

#[test]
fn longer_repeat_found() {
    let words = vec!["aaaa"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("a{4}", &pattern.to_string());
    all_equal(&words, &pattern);
}

#[test]
fn complex_repeat() {
    let words = vec!["aaaabbbbaaaabbbb"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("(?:a{4}b{4}){2}", &pattern.to_string());
    all_equal(&words, &pattern);
}

#[test]
fn simple_string_symbol_capturing() {
    let words = vec!["foo"];
    let mut p = Pidgin::new();
    p.rule("foo", &Pidgin::new().grammar(&["bar"]));
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("(?P<foo>bar)", pattern.to_string());
}

#[test]
fn simple_rx_symbol_capturing() {
    let words = vec!["foo bar"];
    let mut p = Pidgin::new();
    p.foreign_rx_rule(r"\s+", r"\s+", Some("ws")).unwrap();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("foo(?P<ws>(?:\\s+))bar", pattern.to_string());
}

#[test]
fn simple_rx_symbol_non_capturing() {
    let words = vec!["foo bar"];
    let mut p = Pidgin::new();
    p.foreign_rx_rule(r"\s+", r"\s+", None).unwrap();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("foo(?:\\s+)bar", pattern.to_string());
}

#[test]
fn string_string_symbol_ordering() {
    let words = vec!["foo f"];
    let mut p = Pidgin::new();
    p.rule("foo", &Pidgin::new().grammar(&["bar"]));
    p.rule("f", &Pidgin::new().grammar(&["plugh"]));
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("(?P<foo>bar) (?P<f>plugh)", pattern.to_string());
}

#[test]
fn string_regex_symbol_ordering() {
    let words = vec!["foo f"];
    let mut p = Pidgin::new();
    p.foreign_rx_rule("foo", "bar", None).unwrap();
    p.rule("f", &Pidgin::new().grammar(&["plugh"]));
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("(?P<f>plugh)oo (?P<f>plugh)", pattern.to_string());
}

#[test]
fn regex_regex_symbol_ordering() {
    let words = vec!["foo f"];
    let mut p = Pidgin::new();
    p.foreign_rx_rule("foo", "bar", None).unwrap();
    p.foreign_rx_rule("f", "plugh", None).unwrap();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("(?:bar) (?:plugh)", pattern.to_string());
}

#[test]
fn normalize_whitespace() {
    let words = vec!["foo bar", "baz   plugh"];
    let mut p = Pidgin::new().normalize_whitespace(true);
    p.add(&words);
    let pattern = p.compile().to_string();
    let rx = Regex::new(&pattern).unwrap();
    assert!(rx.is_match("foo    bar"));
    assert!(rx.is_match("baz plugh"));
}

#[test]
fn word_boundaries() {
    let words = vec!["tardigrade", "onomatopoeia"];
    let mut p = Pidgin::new().word_bound();
    p.add(&words);
    let pattern = p.compile().to_string();
    let rx = Regex::new(&pattern).unwrap();
    for w in words {
        assert!(rx.is_match(w));
        let s = String::from("a") + w;
        assert!(!rx.is_match(&s));
        let s = w.to_string() + "a";
        assert!(!rx.is_match(&s));
        let s = String::from(" ") + w;
        assert!(rx.is_match(&s));
        let s = w.to_string() + " ";
        assert!(rx.is_match(&s));
    }
}

#[test]
fn line_boundaries() {
    let words = vec!["tardigrade", "onomatopoeia"];
    let mut p = Pidgin::new().line_bound();
    p.add(&words);
    let pattern = p.compile().to_string();
    let rx = Regex::new(&pattern).unwrap();
    for w in words {
        assert!(rx.is_match(w), format!("{} matches '{}", pattern, w));
        let s = String::from(" ") + w;
        assert!(
            !rx.is_match(&s),
            format!("{} doesn't match '{}' with space before", pattern, w)
        );
        let s = w.to_string() + " ";
        assert!(
            !rx.is_match(&s),
            "{} doesn't match '{}' with space after",
            pattern,
            w
        );
        let s = String::from("\n") + w;
        assert!(
            rx.is_match(&s),
            format!("{} matches '{}' with newline before", pattern, w)
        );
        let s = w.to_string() + "\n";
        assert!(
            rx.is_match(&s),
            format!("{} matches '{}' with newline after", pattern, w)
        );
    }
}

#[test]
fn string_boundaries() {
    let words = vec!["tardigrade", "onomatopoeia"];
    let mut p = Pidgin::new().string_bound();
    p.add(&words);
    let pattern = p.compile().to_string();
    let rx = Regex::new(&pattern).unwrap();
    for w in words {
        assert!(rx.is_match(w), format!("{} matches '{}", pattern, w));
        let s = String::from(" ") + w;
        assert!(
            !rx.is_match(&s),
            format!("{} doesn't match '{}' with space before", pattern, w)
        );
        let s = w.to_string() + " ";
        assert!(
            !rx.is_match(&s),
            "{} doesn't match '{}' with space after",
            pattern,
            w
        );
        let s = String::from("\n") + w;
        assert!(
            !rx.is_match(&s),
            format!("{} doesn't match '{}' with newline before", pattern, w)
        );
        let s = w.to_string() + "\n";
        assert!(
            !rx.is_match(&s),
            format!("{} doesn't match '{}' with newline after", pattern, w)
        );
    }
}

#[test]
fn rule_ordering() {
    let mut p = Pidgin::new();
    p.foreign_rule("foo", "(?P<alpha>[a-zA-Z]+)").unwrap();
    p.foreign_rule("foo", r"(?P<numeric>\d+)").unwrap();
    p.foreign_rule("foo", r"(?P<alphanumeric>[\da-zA-Z]+)")
        .unwrap();
    p.add(&["foo"]);
    let pattern = p.compile().to_string();
    let rx = Regex::new(&pattern).unwrap();
    let cap = rx.captures("1234").unwrap();
    assert!(cap.name("foo").is_some(), format!("{} matched", rx));
    assert!(cap.name("numeric").is_some(), "right order");
    let cap = rx.captures("abc").unwrap();
    assert!(cap.name("foo").is_some(), "pattern matched");
    assert!(cap.name("alpha").is_some(), "right order");
    p.clear();
    p.foreign_rule("foo", r"(?P<alphanumeric>[\da-zA-Z]+)")
        .unwrap();
    p.foreign_rule("foo", "(?P<alpha>[a-zA-Z]+)").unwrap();
    p.foreign_rule("foo", r"(?P<numeric>\d+)").unwrap();
    p.add(&["foo"]);
    let pattern = p.compile().to_string();
    let rx = Regex::new(&pattern).unwrap();
    let cap = rx.captures("1234").unwrap();
    assert!(cap.name("foo").is_some(), "pattern matched");
    assert!(cap.name("numeric").is_none(), "right order");
    let cap = rx.captures("abc").unwrap();
    assert!(cap.name("foo").is_some(), "pattern matched");
    assert!(cap.name("alpha").is_none(), "right order");
}

#[test]
fn case_sensitivity() {
    let words = vec!["cat", "dog"];
    let mut p = Pidgin::new().case_insensitive(true);
    p.add(&words);
    let pattern = p.compile();
    all_equal(&vec!["CAT", "DOG"], &pattern);
}

#[test]
fn nested_capturing() {
    let mut p = Pidgin::new()
        .word_bound()
        .normalize_whitespace(true)
        .case_insensitive(true);
    let apples = p.add(&vec!["pippin", "northern spy", "crab"]).compile();
    let oranges = p.add(&vec!["blood", "navel", "valencia"]).compile();
    p.rule("apple", &apples);
    p.rule("orange", &oranges);
    let fruit = p.add(&vec!["apple", "orange"]).compile();
    let lettuces = p.add(&vec!["red", "green", "boston", "romaine"]).compile();
    let tomatoes = p
        .add(&vec!["cherry", "brandywine", "beefsteak", "roma"])
        .compile();
    p.rule("lettuce", &lettuces);
    p.rule("tomatoe", &tomatoes);
    let vegetables = p.add(&vec!["lettuce", "tomatoe"]).compile();
    p.rule("vegetable", &vegetables);
    p.rule("fruit", &fruit);
    let matcher = p
        .add(&vec!["fruit", "vegetable"])
        .compile()
        .matcher()
        .unwrap();
    assert!(matcher.is_match("cherry"));
    let captures = matcher.parse("cherry").unwrap();
    assert_eq!(captures.name("vegetable").unwrap().as_str(), "cherry");
    assert_eq!(captures.name("tomatoe").unwrap().as_str(), "cherry");
    assert!(matcher.is_match("Brandywine"));
    let captures = matcher.parse("  Northern  Spy  ").unwrap();
    assert_eq!(captures.name("fruit").unwrap().as_str(), "Northern  Spy");
    assert_eq!(captures.name("apple").unwrap().as_str(), "Northern  Spy");
    assert!(!matcher.is_match("tomatoes"));
}

#[test]
fn condense_repeated_non_capturing_grammars() {
    let mut p = Pidgin::new();
    let foo = p.add(&vec!["foo", "bar", "baz", "plugh"]).compile();
    p.rule("foo", &foo);
    let pattern = p.add_str("foofoo").compile_non_capturing();
    assert_eq!(pattern.to_string(), "(?:foo|plugh|ba[rz]){2}");
}

#[test]
fn repeated_grammars_not_condensed() {
    let mut p = Pidgin::new();
    let foo = p.add(&vec!["foo", "bar", "baz", "plugh"]).compile();
    p.rule("foo", &foo);
    let pattern = p.add_str("foofoo").compile();
    assert_eq!(
        pattern.to_string(),
        "(?P<foo>(?:foo|plugh|ba[rz]))(?P<foo>(?:foo|plugh|ba[rz]))"
    );
}

#[test]
fn foreign_rule() -> Result<(), Box<std::error::Error>> {
    let mut p = Pidgin::new();
    let g = p.grammar(&vec!["foo", "bar"]);
    p.rx_rule(r"\s+", &g, Some("baz"))?;
    let m = p.add_str("this is weird").matcher()?;
    assert!(m.is_match("thisfooisbarweird"));
    Ok(())
}

#[test]
fn description() {
    let mut p = Pidgin::new();
    let g = p.grammar(&vec!["bar", "baz"]);
    p.rule("foo", &g);
    p = p.case_insensitive(true);
    let g = p.grammar(&vec!["ping", "pang", "pong"]);
    p = p.case_insensitive(false);
    p.rule("xyzzy", &g);
    let g = p.grammar(&vec!["xyzzy", "qux"]);
    p.rule("plugh", &g);
    let g = p.grammar(&vec!["foo", "plugh"]);
    println!("{}", g.describe());
    assert_eq!(
        "  TOP :=      {foo}|{plugh}\n  foo :=      ba[rz]\nplugh :=      qux|{xyzzy}\nxyzzy := (?i) p[aio]ng\n",
        g.describe()
    );
}

#[test]
fn grammar_format() {
    let mut p = Pidgin::new();
    let g = p.grammar(&vec!["bar", "baz"]);
    p.rule("foo", &g);
    p = p.case_insensitive(true);
    let g = p.grammar(&vec!["ping", "pang", "pong"]);
    p = p.case_insensitive(false);
    p.rule("xyzzy", &g);
    let g = p.grammar(&vec!["xyzzy", "qux"]);
    p.rule("plugh", &g);
    let g = p.grammar(&vec!["foo", "plugh"]);
    println!("{}", g.describe());
    assert_eq!(
        "  TOP :=      {foo}|{plugh}\n  foo :=      ba[rz]\nplugh :=      qux|{xyzzy}\nxyzzy := (?i) p[aio]ng\n",
        format!("{}", g)
    );
}

#[test]
fn reverse_greed() {
    let mut p = Pidgin::new().reverse_greed(true);
    let g = p.grammar(&vec!["bar"]);
    assert_eq!("(?U:bar)", g.to_string());
}

#[test]
fn reps() {
    let mut p = Pidgin::new();
    let g = p.grammar(&vec!["foo", "bar", "baz"]);
    p.build_rule("foo", vec![sf("xyzzy "), gf(g.reps(3)), sf(" plugh")]);
    let g = p.grammar(&vec!["foo"]);
    let m = g.matcher().unwrap();
    assert!(m.is_match("xyzzy foobarbaz plugh"));
    assert!(!m.is_match("xyzzy foo plugh"));
    assert!(!m.is_match("xyzzy foobarbazfoo plugh"));
}

#[test]
fn min_reps_0() {
    let mut p = Pidgin::new();
    let g = p.grammar(&vec!["foo", "bar", "baz"]);
    p.build_rule(
        "foo",
        vec![sf("xyzzy "), gf(g.reps_min(0).unwrap()), sf(" plugh")],
    );
    let g = p.grammar(&vec!["foo"]);
    assert!(g.describe().contains("*"));
    let m = g.matcher().unwrap();
    assert!(m.is_match("xyzzy  plugh"));
    assert!(m.is_match("xyzzy foo plugh"));
    assert!(m.is_match("xyzzy foobar plugh"));
    assert!(m.is_match("xyzzy foobarbaz plugh"));
}

#[test]
fn min_reps_1() {
    let mut p = Pidgin::new();
    let g = p.grammar(&vec!["foo", "bar", "baz"]);
    p.build_rule(
        "foo",
        vec![sf("xyzzy "), gf(g.reps_min(1).unwrap()), sf(" plugh")],
    );
    let g = p.grammar(&vec!["foo"]);
    assert!(g.describe().contains("+"));
    let m = g.matcher().unwrap();
    assert!(!m.is_match("xyzzy  plugh"));
    assert!(m.is_match("xyzzy foo plugh"));
    assert!(m.is_match("xyzzy foobar plugh"));
    assert!(m.is_match("xyzzy foobarbaz plugh"));
}

#[test]
fn min_reps_2() {
    let mut p = Pidgin::new();
    let g = p.grammar(&vec!["foo", "bar", "baz"]);
    p.build_rule(
        "foo",
        vec![sf("xyzzy "), gf(g.reps_min(2).unwrap()), sf(" plugh")],
    );
    let g = p.grammar(&vec!["foo"]);
    assert!(g.describe().contains("{2,}"));
    let m = g.matcher().unwrap();
    assert!(!m.is_match("xyzzy  plugh"));
    assert!(!m.is_match("xyzzy foo plugh"));
    assert!(m.is_match("xyzzy foobar plugh"));
    assert!(m.is_match("xyzzy foobarbaz plugh"));
}

#[test]
fn max_reps_1() {
    let mut p = Pidgin::new();
    let g = p.grammar(&vec!["foo", "bar", "baz"]);
    p.build_rule(
        "foo",
        vec![sf("xyzzy "), gf(g.reps_max(1).unwrap()), sf(" plugh")],
    );
    let g = p.grammar(&vec!["foo"]);
    assert!(g.describe().contains("?"));
    let m = g.matcher().unwrap();
    assert!(m.is_match("xyzzy  plugh"));
    assert!(m.is_match("xyzzy foo plugh"));
    assert!(!m.is_match("xyzzy foobar plugh"));
    assert!(!m.is_match("xyzzy foobarbaz plugh"));
}

#[test]
fn max_reps_2() {
    let mut p = Pidgin::new();
    let g = p.grammar(&vec!["foo", "bar", "baz"]);
    p.build_rule(
        "foo",
        vec![sf("xyzzy "), gf(g.reps_max(2).unwrap()), sf(" plugh")],
    );
    let g = p.grammar(&vec!["foo"]);
    assert!(g.describe().contains("{0,2}"));
    let m = g.matcher().unwrap();
    assert!(m.is_match("xyzzy  plugh"));
    assert!(m.is_match("xyzzy foo plugh"));
    assert!(m.is_match("xyzzy foobar plugh"));
    assert!(!m.is_match("xyzzy foobarbaz plugh"));
}

#[test]
fn min_max_reps_0_1() {
    let mut p = Pidgin::new();
    let g = p.grammar(&vec!["foo", "bar", "baz"]);
    p.build_rule(
        "foo",
        vec![
            sf("xyzzy "),
            gf(g.reps_min_max(0, 1).unwrap()),
            sf(" plugh"),
        ],
    );
    let g = p.grammar(&vec!["foo"]);
    assert!(g.describe().contains("?"));
    let m = g.matcher().unwrap();
    assert!(m.is_match("xyzzy  plugh"));
    assert!(m.is_match("xyzzy foo plugh"));
    assert!(!m.is_match("xyzzy foobar plugh"));
    assert!(!m.is_match("xyzzy foobarbaz plugh"));
}

#[test]
fn min_max_reps_1_1() {
    let mut p = Pidgin::new();
    let g = p.grammar(&vec!["foo", "bar", "baz"]);
    p.build_rule(
        "foo",
        vec![
            sf("xyzzy "),
            gf(g.reps_min_max(1, 1).unwrap()),
            sf(" plugh"),
        ],
    );
    let g = p.grammar(&vec!["foo"]);
    let m = g.matcher().unwrap();
    assert!(!m.is_match("xyzzy  plugh"));
    assert!(m.is_match("xyzzy foo plugh"));
    assert!(!m.is_match("xyzzy foobar plugh"));
    assert!(!m.is_match("xyzzy foobarbaz plugh"));
}

#[test]
fn min_max_reps_2_2() {
    let mut p = Pidgin::new();
    let g = p.grammar(&vec!["foo", "bar", "baz"]);
    p.build_rule(
        "foo",
        vec![
            sf("xyzzy "),
            gf(g.reps_min_max(2, 2).unwrap()),
            sf(" plugh"),
        ],
    );
    let g = p.grammar(&vec!["foo"]);
    assert!(g.describe().contains("{2}"));
    let m = g.matcher().unwrap();
    assert!(!m.is_match("xyzzy  plugh"));
    assert!(!m.is_match("xyzzy foo plugh"));
    assert!(m.is_match("xyzzy foobar plugh"));
    assert!(!m.is_match("xyzzy foobarbaz plugh"));
}

#[test]
fn min_max_reps_1_2() {
    let mut p = Pidgin::new();
    let g = p.grammar(&vec!["foo", "bar", "baz"]);
    p.build_rule(
        "foo",
        vec![
            sf("xyzzy "),
            gf(g.reps_min_max(1, 2).unwrap()),
            sf(" plugh"),
        ],
    );
    let g = p.grammar(&vec!["foo"]);
    assert!(g.describe().contains("{1,2}"));
    let m = g.matcher().unwrap();
    assert!(!m.is_match("xyzzy  plugh"));
    assert!(m.is_match("xyzzy foo plugh"));
    assert!(m.is_match("xyzzy foobar plugh"));
    assert!(!m.is_match("xyzzy foobarbaz plugh"));
}

#[test]
fn build_rule_bound() {
    let mut p = Pidgin::new();
    let g = p.grammar(&vec!["foo", "bar", "baz"]);
    p = p.word_bound();
    p.build_rule("foo", vec![sf("xyzzy"), gf(g), sf("plugh")]);
    let g = p.grammar(&vec!["foo"]);
    print!("{}", g);
    let m = g.matcher().unwrap();
    assert!(m.is_match("xyzzyfooplugh"));
    assert!(!m.is_match("_xyzzyfooplugh"));
    assert!(!m.is_match("xyzzyfooplugh_"));
    assert!(!m.is_match("xyzzy foo plugh"));
}

#[test]
fn boundaries_only_on_leaves() {
    let mut p = Pidgin::new().word_bound();
    let words = vec!["foo", "@bar", "baz"];
    let g = p.grammar(&words);
    p.rule("foo", &g);
    let m = p.grammar(&vec!["foo"]).matcher().unwrap();
    for w in words {
        assert!(m.is_match(w), w);
        let right_bound = w.to_string() + "b";
        assert!(!m.is_match(&right_bound), right_bound);
    }
    assert!(!m.is_match("bfoo"), "bfoo");
    assert!(!m.is_match("bbaz"), "bbaz");
    assert!(m.is_match("b@bar"), "b@bar");
}
