extern crate pidgin;
use pidgin::Pidgin;
extern crate regex;
use regex::Regex;

fn all_equal(words: &[&str], pattern: &str) {
    let rx = Regex::new(pattern).unwrap();
    for w in words {
        assert!(rx.is_match(w))
    }
}

#[test]
fn simple_alternation() {
    let words = vec!["cat", "dog", "camel"];
    let pattern = Pidgin::new().compile(&words);
    all_equal(&words, &pattern);
}

#[test]
fn common_suffix() {
    let words = vec!["cats", "dogs"];
    let pattern = Pidgin::new().compile(&words);
    assert!(pattern.as_str().ends_with("s"));
    all_equal(&words, &pattern);
}

#[test]
fn common_prefix() {
    let words = vec!["scat", "spore"];
    let pattern = Pidgin::new().compile(&words);
    assert!(pattern.as_str().starts_with("s"));
    all_equal(&words, &pattern);
}

#[test]
fn both_prefix_and_suffix() {
    let words = vec!["scats", "spores"];
    let pattern = Pidgin::new().compile(&words);
    assert!(pattern.as_str().starts_with("s"));
    assert!(pattern.as_str().ends_with("s"));
    all_equal(&words, &pattern);
}

#[test]
fn short_character_class() {
    let words = vec!["a", "b"];
    let pattern = Pidgin::new().compile(&words);
    assert_eq!("[ab]", &pattern);
    all_equal(&words, &pattern);
}

#[test]
fn long_character_class() {
    let words = vec!["a", "b", "c"];
    let pattern = Pidgin::new().compile(&words);
    assert_eq!("[a-c]", &pattern);
    all_equal(&words, &pattern);
}

#[test]
fn complex_character_class() {
    let words = vec!["a", "b", "c", "g"];
    let pattern = Pidgin::new().compile(&words);
    assert_eq!("[a-cg]", &pattern);
    all_equal(&words, &pattern);
}

#[test]
fn character_class_in_alternation() {
    let words = vec!["Ant", "a", "b", "c", "g"];
    let pattern = Pidgin::new().compile(&words);
    assert_eq!("(?:[a-cg]|Ant)", &pattern);
    all_equal(&words, &pattern);
}

#[test]
fn small_repeat_ignored() {
    let words = vec!["aaa"];
    let pattern = Pidgin::new().compile(&words);
    assert_eq!("aaa", &pattern);
    all_equal(&words, &pattern);
}

#[test]
fn longer_repeat_found() {
    let words = vec!["aaaa"];
    let pattern = Pidgin::new().compile(&words);
    assert_eq!("a{4}", &pattern);
    all_equal(&words, &pattern);
}

#[test]
fn complex_repeat() {
    let words = vec!["aaaabbbbaaaabbbb"];
    let pattern = Pidgin::new().compile(&words);
    assert_eq!("(?:a{4}b{4}){2}", &pattern);
    all_equal(&words, &pattern);
}

#[test]
fn simple_string_symbol_capturing() {
    let words = vec!["foo"];
    let mut p = Pidgin::new();
    p.rule("foo", "bar");
    let pattern = p.compile(&words);
    assert_eq!("(?P<foo>bar)", pattern);
}

#[test]
fn simple_string_symbol_non_capturing() {
    let words = vec!["foo"];
    let mut p = Pidgin::new();
    p.nm_rule("foo", "bar");
    let pattern = p.compile(&words);
    assert_eq!("(?:bar)", pattern);
}

#[test]
fn simple_rx_symbol_capturing() {
    let words = vec!["foo bar"];
    let mut p = Pidgin::new();
    p.rx_rule(r"\s+", r"\s+", Some("ws"));
    let pattern = p.compile(&words);
    assert_eq!("foo(?P<ws>\\s+)bar", pattern);
}

#[test]
fn simple_rx_symbol_non_capturing() {
    let words = vec!["foo bar"];
    let mut p = Pidgin::new();
    p.rx_rule(r"\s+", r"\s+", None);
    let pattern = p.compile(&words);
    assert_eq!("foo(?:\\s+)bar", pattern);
}

#[test]
fn string_string_symbol_ordering() {
    let words = vec!["foo f"];
    let mut p = Pidgin::new();
    p.nm_rule("foo", "bar");
    p.nm_rule("f", "plugh");
    let pattern = p.compile(&words);
    assert_eq!("(?:bar) (?:plugh)", pattern);
}

#[test]
fn capturing_versus_non_capturing_symbol_ordering() {
    let words = vec!["foo"];
    let mut p = Pidgin::new();
    p.rule("foo", "plugh");
    p.nm_rule("foo", "bar");
    let pattern = p.compile(&words);
    assert_eq!("(?P<foo>plugh)", pattern);
}

#[test]
fn string_regex_symbol_ordering() {
    let words = vec!["foo f"];
    let mut p = Pidgin::new();
    p.rx_rule("foo", "bar", None);
    p.nm_rule("f", "plugh");
    let pattern = p.compile(&words);
    assert_eq!("(?:plugh)oo (?:plugh)", pattern);
}

#[test]
fn regex_regex_symbol_ordering() {
    let words = vec!["foo f"];
    let mut p = Pidgin::new();
    p.rx_rule("foo", "bar", None);
    p.rx_rule("f", "plugh", None);
    let pattern = p.compile(&words);
    assert_eq!("(?:bar) (?:plugh)", pattern);
}

#[test]
fn normalize_whitespace() {
    let words = vec!["foo bar", "baz   plugh"];
    let p = Pidgin::new().normalize_whitespace();
    let pattern = p.compile(&words);
    let rx = Regex::new(&pattern).unwrap();
    assert!(rx.is_match("foo    bar"));
    assert!(rx.is_match("baz plugh"));
}

#[test]
fn word_boundaries() {
    let words = vec!["tardigrade", "onomatopoeia"];
    let p = Pidgin::new().word_bound();
    let pattern = p.compile(&words);
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
    let p = Pidgin::new().line_bound();
    let pattern = p.compile(&words);
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
    let p = Pidgin::new().string_bound();
    let pattern = p.compile(&words);
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
