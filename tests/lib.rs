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
