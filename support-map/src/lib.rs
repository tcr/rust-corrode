#![allow(non_snake_case)]
#![allow(unused_variables)]

#[macro_use] extern crate maplit;
extern crate itertools;

pub mod Map {
    use std::hash::Hash;
    use std::collections::HashMap;
    use itertools::Itertools;

    pub type Map<A, B> = HashMap<A, B>;

    pub fn empty<A: Hash + Eq, B>() -> HashMap<A, B> {
        hashmap![]
    }

    pub fn lookup<A: Eq + Hash, B: Clone>(value: A, inside: HashMap<A, B>) -> Option<B> {
        inside.get(&value).map(|x| x.clone())
    }

    #[test]
    fn test_lookup() {
        let mut a = empty();
        a.insert("key", "value".to_string());

        assert_eq!(lookup("key", a), Some("value".to_string()));
    }

    pub fn singleton<A: Eq + Hash, B>(a: A, b: B) -> HashMap<A, B> {
        hashmap![a => b]
    }

    pub fn member<A: Eq + Hash, B>(a: A, input: HashMap<A, B>) -> bool {
        input.contains_key(&a)
    }

    pub fn delete<A: Eq + Hash, B>(key: A, mut input: HashMap<A, B>) -> HashMap<A, B> {
        input.remove(&key);
        input
    }

    pub fn insert<A: Eq + Hash, B>(key: A, value: B, mut input: HashMap<A, B>) -> HashMap<A, B> {
        input.insert(key, value);
        input
    }

    pub fn fromList<A: Hash + Eq, B>(input: Vec<(A, B)>) -> HashMap<A, B> {
        input.into_iter().collect()
    }

    pub fn toList<A: Hash + Eq, B>(input: HashMap<A, B>) -> Vec<(A, B)> {
        input.into_iter().collect()
    }

    #[test]
    fn test_toList() {
        let mut a = empty();
        a.insert("key", "value");

        assert_eq!(&toList(a), &[("key", "value")]);
    }

    pub fn unionWith<A: Hash + Eq + Clone, B>(apply: Box<Fn(B, B) -> B>, left: HashMap<A, B>, right: HashMap<A, B>) -> HashMap<A, B> {
        //TODO
        unreachable!();
    }

    pub fn fromListWith<A: Hash + Eq + Clone, B>(apply: Box<Fn(B, B) -> B>, input: Vec<(A, B)>) -> HashMap<A, B> {
        input
        .into_iter().group_by(|&(ref k, _)| k.clone())
        .into_iter()
        .map(|(_, values)| {
            let mut list: Vec<_> = values.into_iter().collect();
            let (key, first) = list.remove(0);
            (key, list.into_iter().fold(first, |l, r| { apply(l, r.1) }))
        })
        .collect()
    }
}


pub mod Set {
    use std::hash::Hash;
    use std::fmt::Debug;
    use std::collections::HashSet;

    pub type Set<T> = HashSet<T>;

    pub fn member<T: Eq + Hash + Debug>(item: T, list: Set<T>) -> bool {
        list.contains(&item)
    }

    pub fn fromList<T: Eq + Hash + Debug>(input: Vec<T>) -> Set<T> {
        input.into_iter().collect()
    }

    pub fn toList<T: Eq + Hash + Debug>(input: Set<T>) -> Vec<T> {
        input.into_iter().collect()
    }

    pub fn insert<T: Eq + Hash + Debug>(item: T, mut list: Set<T>) -> Set<T> {
        list.insert(item);
        list
    }

    pub fn delete<T: Eq + Hash + Debug>(item: T, mut list: Set<T>) -> Set<T> {
        list.remove(&item);
        list
    }

    pub fn empty<T: Eq + Hash + Debug>() -> Set<T> {
        hashset![]
    }

    pub fn difference<T: Eq + Hash + Debug + Clone>(a: Set<T>, b: Set<T>) -> Set<T> {
        let mut c = hashset![];
        for item in a.clone() {
            if !b.contains(&item) {
                c.insert(item);
            }
        }
        for item in b.clone() {
            if !a.contains(&item) {
                c.insert(item);
            }
        }
        c
    }

    pub fn notMember<T: Eq + Hash + Debug + Clone>(key: T, input: Set<T>) -> bool {
        input.contains(&key)
    }

    pub fn singleton<T: Eq + Hash + Debug + Clone>(key: T) -> Set<T> {
        hashset![key]
    }
}




pub mod IntMap {
    use std::collections::{HashMap, HashSet};

    pub type IntMap<B> = HashMap<isize, B>;

    pub fn lookup<B: Clone>(value: isize, inside: HashMap<isize, B>) -> Option<B> {
        inside.get(&value).map(|x| x.clone())
    }

    pub fn delete<B>(key: isize, mut input: HashMap<isize, B>) -> HashMap<isize, B> {
        input.remove(&key);
        input
    }

    pub fn insert<B>(key: isize, value: B, mut input: HashMap<isize, B>) -> HashMap<isize, B> {
        input.insert(key, value);
        input
    }

    pub fn fromList<B>(input: Vec<(isize, B)>) -> HashMap<isize, B> {
        input.into_iter().collect()
    }

    pub fn unionWith<B>(apply: Box<Fn(B, B) -> B>, left: HashMap<isize, B>, right: HashMap<isize, B>) -> HashMap<isize, B> {
        //TODO
        unreachable!();
    }

    pub fn difference<B: Clone>(left: HashMap<isize, B>, right: HashMap<isize, B>) -> HashMap<isize, B> {
        let mut c = hashmap![];
        for item in left.keys().cloned() {
            if !right.contains_key(&item) {
                c.insert(item, left[&item].clone());
            }
        }
        for item in right.keys().cloned() {
            if !left.contains_key(&item) {
                c.insert(item, right[&item].clone());
            }
        }
        c
    }

    pub fn size<B>(left: HashMap<isize, B>) -> isize {
        left.len() as isize
    }

    pub fn empty<B>() -> HashMap<isize, B> {
        hashmap![]
    }

    pub fn singleton<B>(a: isize, b: B) -> HashMap<isize, B> {
        hashmap![a => b]
    }

    pub fn toList<B>(input: HashMap<isize, B>) -> Vec<(isize, B)> {
        input.into_iter().collect()
    }

    pub fn keys<B>(input: HashMap<isize, B>) -> Vec<isize> {
        input.keys().cloned().into_iter().collect()
    }

    pub fn unions<B>(inputs: Vec<IntMap<B>>) -> IntMap<B> {
        let mut a = hashmap![];
        for input in inputs {
            a.extend(input);
        }
        a.into_iter().collect()
    }

    pub fn null<B>(input: HashMap<isize, B>) -> bool {
        input.len() == 0
    }

    pub fn fromDistinctAscList<B>(input: Vec<(isize, B)>) -> IntMap<B> {
        // TODO
        unreachable!();
    }

    pub fn intersectionWith<A, B, C>(apply: Box<Fn(A, B) -> C>, left: IntMap<A>, right: IntMap<B>) -> IntMap<C> {
        // TODO
        unreachable!();
    }

    pub fn fromListWith<B>(apply: Box<Fn(B, B) -> B>, input: Vec<(isize, B)>) -> IntMap<B> {
        // TODO
        unreachable!();
    }

    pub fn findWithDefault<B>(value: B, key: isize, input: IntMap<B>) -> B {
        // TODO
        unreachable!();
    }

    pub fn keysSet<B>(input: IntMap<B>) -> HashSet<isize> {
        // TODO
        unreachable!();
    }

    pub fn elems<B>(input: IntMap<B>) -> Vec<B> {
        // TODO
        unreachable!();
    }

    pub fn mapMaybeWithKey<A, B>(apply: Box<Fn(isize, A) -> Option<B>>, input: IntMap<A>) -> IntMap<B> {
        // TODO
        unreachable!();
    }

    pub fn intersection<A, B>(left: IntMap<A>, right: IntMap<B>) -> IntMap<A> {
        // TODO
        unreachable!();
    }

    pub fn fromSet<B>(apply: Box<Fn(isize) -> B>, input: HashSet<isize>) -> IntMap<B> {
        // TODO
        unreachable!();
    }

    pub fn map<A, B>(apply: Box<Fn(A) -> B>, input: IntMap<A>) -> IntMap<B> {
        // TODO
        unreachable!();
    }

    pub fn union<B>(left: IntMap<B>, right: IntMap<B>) -> IntMap<B> {
        // TODO
        unreachable!();
    }

    pub fn unionsWith<B>(apply: Box<Fn(B, B) -> B>, left: IntMap<B>, right: IntMap<B>) -> IntMap<B> {
        // TODO
        unreachable!();
    }

    pub fn updateLookupWithKey<B>(apply: Box<Fn(isize, B) -> Option<B>>, key: isize, input: IntMap<B>) -> (Option<B>, IntMap<B>) {
        // TODO
        unreachable!();
    }

    pub fn minViewWithKey<B>(input: IntMap<B>) -> Option<((isize, B), IntMap<B>)> {
        // TODO
        unreachable!();
    }

    pub fn splitLookup<B>(key: isize, input: IntMap<B>) -> (IntMap<B>, Option<B>, IntMap<B>) {
        // TODO
        unreachable!();
    }

    pub fn filterWithKey<B>(apply: Box<Fn(isize, B) -> bool>, input: IntMap<B>) -> IntMap<B> {
        // TODO
        unreachable!();
    }

    pub fn mapMaybe<A, B>(apply: Box<Fn(A) -> Option<B>>, input: IntMap<A>) -> IntMap<B> {
        // TODO
        unreachable!();
    }

    pub fn mapEither<A, B, C>(apply: Box<Fn(A) -> Result<C, B>>, input: IntMap<A>) -> (IntMap<B>, IntMap<C>) {
        // TODO
        unreachable!();
    }

    pub fn deleteFindMax<B>(input: IntMap<B>) -> ((isize, B), IntMap<B>) {
        // TODO
        unreachable!();
    }

    pub fn mapWithKey<A, B>(apply: Box<Fn(isize, A) -> B>, input: IntMap<A>) -> IntMap<B> {
        // TODO
        unreachable!();
    }

    pub fn filter<B>(apply: Box<Fn(B) -> bool>, input: IntMap<B>) -> IntMap<B> {
        // TODO
        unreachable!();
    }
}

pub mod IntSet {
    use std::collections::HashSet;

    pub type IntSet = HashSet<isize>;

    pub fn empty() -> HashSet<isize> {
        hashset![]
    }

    pub fn insert(value: isize, mut input: IntSet) -> IntSet {
        input.insert(value);
        input
    }

    pub fn difference(a: IntSet, b: IntSet) -> IntSet {
        let mut c = hashset![];
        for item in a.clone() {
            if !b.contains(&item) {
                c.insert(item);
            }
        }
        for item in b.clone() {
            if !a.contains(&item) {
                c.insert(item);
            }
        }
        c
    }

    pub fn unions(inputs: Vec<IntSet>) -> IntSet {
        let mut a = vec![];
        for input in inputs {
            a.extend(input);
        }
        a.into_iter().collect()
    }

    pub fn member(key: isize, input: IntSet) -> bool {
        input.contains(&key)
    }

    pub fn null(input: IntSet) -> bool {
        input.is_empty()
    }

    pub fn singleton(value: isize) -> IntSet {
        hashset![value]
    }

    pub fn fromList(input: Vec<isize>) -> IntSet {
        input.into_iter().collect()
    }

    pub fn intersection(a: IntSet, b: IntSet) -> IntSet {
        let mut c = hashset![];
        for item in a.clone() {
            if b.contains(&item) {
                c.insert(item);
            }
        }
        c
    }

    pub fn union(a: IntSet, b: IntSet) -> IntSet {
        let mut c = hashset![];
        c.extend(a);
        c.extend(b);
        c
    }

    pub fn toList(input: IntSet) -> Vec<isize> {
        input.into_iter().collect()
    }

    pub fn findMin(input: IntSet) -> isize {
        input.into_iter().min().unwrap()
    }

    pub fn size(input: IntSet) -> isize {
        input.len() as isize
    }
}
