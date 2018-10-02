extern crate pidgin;
use pidgin::{Grammar, Pidgin};
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
    assert!(pattern.as_str().starts_with("s"));
    all_equal(&words, &pattern);
}

#[test]
fn both_prefix_and_suffix() {
    let words = vec!["scats", "spores"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert!(pattern.as_str().starts_with("s"));
    assert!(pattern.as_str().ends_with("s"));
    all_equal(&words, &pattern);
}

#[test]
fn short_character_class() {
    let words = vec!["a", "b"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("[ab]", &pattern);
    all_equal(&words, &pattern);
}

#[test]
fn long_character_class() {
    let words = vec!["a", "b", "c"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("[a-c]", &pattern);
    all_equal(&words, &pattern);
}

#[test]
fn complex_character_class() {
    let words = vec!["a", "b", "c", "g"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("[a-cg]", &pattern);
    all_equal(&words, &pattern);
}

#[test]
fn character_class_in_alternation() {
    let words = vec!["Ant", "a", "b", "c", "g"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("(?:[a-cg]|Ant)", &pattern);
    all_equal(&words, &pattern);
}

#[test]
fn small_repeat_ignored() {
    let words = vec!["aaa"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("aaa", &pattern);
    all_equal(&words, &pattern);
}

#[test]
fn longer_repeat_found() {
    let words = vec!["aaaa"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("a{4}", &pattern);
    all_equal(&words, &pattern);
}

#[test]
fn complex_repeat() {
    let words = vec!["aaaabbbbaaaabbbb"];
    let mut p = Pidgin::new();
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("(?:a{4}b{4}){2}", &pattern);
    all_equal(&words, &pattern);
}

#[test]
fn simple_string_symbol_capturing() {
    let words = vec!["foo"];
    let mut p = Pidgin::new();
    p.rule("foo", "bar");
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("(?P<foo>bar)", pattern);
}

#[test]
fn simple_string_symbol_non_capturing() {
    let words = vec!["foo"];
    let mut p = Pidgin::new();
    p.add(&words);
    p.nm_rule("foo", "bar");
    let pattern = p.compile();
    assert_eq!("(?:bar)", pattern);
}

#[test]
fn simple_rx_symbol_capturing() {
    let words = vec!["foo bar"];
    let mut p = Pidgin::new();
    p.rx_rule(r"\s+", r"\s+", Some("ws"));
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("foo(?P<ws>\\s+)bar", pattern);
}

#[test]
fn simple_rx_symbol_non_capturing() {
    let words = vec!["foo bar"];
    let mut p = Pidgin::new();
    p.rx_rule(r"\s+", r"\s+", None);
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("foo(?:\\s+)bar", pattern);
}

#[test]
fn string_string_symbol_ordering() {
    let words = vec!["foo f"];
    let mut p = Pidgin::new();
    p.nm_rule("foo", "bar");
    p.nm_rule("f", "plugh");
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("(?:bar) (?:plugh)", pattern);
}

#[test]
fn capturing_versus_non_capturing_symbol_ordering() {
    let words = vec!["foo"];
    let mut p = Pidgin::new();
    p.rule("foo", "plugh");
    p.nm_rule("foo", "bar");
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("(?P<foo>plugh)", pattern);
}

#[test]
fn string_regex_symbol_ordering() {
    let words = vec!["foo f"];
    let mut p = Pidgin::new();
    p.rx_rule("foo", "bar", None);
    p.nm_rule("f", "plugh");
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("(?:plugh)oo (?:plugh)", pattern);
}

#[test]
fn regex_regex_symbol_ordering() {
    let words = vec!["foo f"];
    let mut p = Pidgin::new();
    p.rx_rule("foo", "bar", None);
    p.rx_rule("f", "plugh", None);
    p.add(&words);
    let pattern = p.compile();
    assert_eq!("(?:bar) (?:plugh)", pattern);
}

#[test]
fn normalize_whitespace() {
    let words = vec!["foo bar", "baz   plugh"];
    let mut p = Pidgin::new().normalize_whitespace();
    p.add(&words);
    let pattern = p.compile();
    let rx = Regex::new(&pattern).unwrap();
    assert!(rx.is_match("foo    bar"));
    assert!(rx.is_match("baz plugh"));
}

#[test]
fn word_boundaries() {
    let words = vec!["tardigrade", "onomatopoeia"];
    let mut p = Pidgin::new().word_bound();
    p.add(&words);
    let pattern = p.compile();
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
    let pattern = p.compile();
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
    let pattern = p.compile();
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
    p.rule("foo", "(?P<alpha>[a-zA-Z]+)");
    p.rule("foo", r"(?P<numeric>\d+)");
    p.rule("foo", r"(?P<alphanumeric>[\da-zA-Z]+)");
    p.add(&["foo"]);
    let pattern = p.compile();
    let rx = Regex::new(&pattern).unwrap();
    let cap = rx.captures("1234").unwrap();
    assert!(cap.name("foo").is_some(), "pattern matched");
    assert!(cap.name("numeric").is_some(), "right order");
    let cap = rx.captures("abc").unwrap();
    assert!(cap.name("foo").is_some(), "pattern matched");
    assert!(cap.name("alpha").is_some(), "right order");
    p.clear();
    p.rule("foo", r"(?P<alphanumeric>[\da-zA-Z]+)");
    p.rule("foo", "(?P<alpha>[a-zA-Z]+)");
    p.rule("foo", r"(?P<numeric>\d+)");
    p.add(&["foo"]);
    let pattern = p.compile();
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
