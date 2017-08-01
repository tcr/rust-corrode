#![allow(non_snake_case)]

#[macro_use] extern crate maplit;

pub mod Map {
    use std::hash::Hash;
    use std::collections::HashMap;

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

    pub fn singleton() -> () {
        // TODO
    }

    pub fn member() -> () {
        // TODO
    }

    pub fn delete<A: Eq + Hash, B>(key: A, mut input: HashMap<A, B>) -> HashMap<A, B> {
        input.remove(&key);
        input
    }

    pub fn insert<A: Eq + Hash, B>(key: A, value: B, mut input: HashMap<A, B>) -> HashMap<A, B> {
        input.insert(key, value);
        input
    }

    pub fn fromList<A: Hash + Eq, B>(_: Vec<(A, B)>) -> HashMap<A, B> {
        // TODO
        hashmap![]
    }

    pub fn toList() -> () {
        // TODO
    }

    pub fn unionWith() -> () {
        // TODO
    }

    pub fn fromListWith() -> () {
        // TODO
    }
}


pub mod Set {
    use std::hash::Hash;
    use std::fmt::Debug;
    use std::collections::HashSet;

    #[derive(Clone, Debug)]
    pub struct Set<T: Eq + Hash>(HashSet<T>);

    pub fn member<T: Eq + Hash + Debug>(item: T, list: Set<T>) -> bool {
        list.0.contains(&item)
    }

    pub fn fromList<T: Eq + Hash + Debug>(_: Vec<T>) -> Set<T> {
        // TODO
        Set(HashSet::new())
    }

    pub fn toList() -> () {
        // TODO
    }

    pub fn insert<T: Eq + Hash + Debug>(item: T, mut list: Set<T>) -> Set<T> {
        list.0.insert(item);
        list
    }

    pub fn delete<T: Eq + Hash + Debug>(item: T, mut list: Set<T>) -> Set<T> {
        list.0.remove(&item);
        list
    }

    pub fn empty<T: Eq + Hash + Debug>() -> Set<T> {
        Set(HashSet::new())
    }

    pub fn difference() -> () {}

    pub fn notMember() -> () {}

    pub fn singleton() -> () {}
}




pub mod IntMap {
    use std::collections::HashMap;

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

    pub fn fromList<B>(_: Vec<(isize, B)>) -> HashMap<isize, B> {
        // TODO
        hashmap![]
    }

    pub fn unionWith() -> () {
        // TODO
    }

    pub fn difference() -> () {
        // TODO
    }

    pub fn fromDistinctAscList() -> () {
        // TODO
    }

    pub fn size() -> () {
        // TODO
    }

    pub fn intersectionWith() -> () {
        // TODO
    }

    pub fn fromListWith() -> () {
        // TODO
    }

    pub fn findWithDefault() -> () {
        // TODO
    }

    pub fn keysSet() -> () {
        // TODO
    }

    pub fn elems() -> () {
        // TODO
    }

    pub fn mapMaybeWithKey() -> () {
        // TODO
    }

    pub fn empty<B>() -> HashMap<isize, B> {
        hashmap![]
    }

    pub fn intersection() -> () {
        // TODO
    }

    pub fn fromSet() -> () {
        // TODO
    }

    pub fn map() -> () {
        // TODO
    }

    pub fn union() -> () {
        // TODO
    }

    pub fn unionsWith() -> () {
        // TODO
    }

    pub fn singleton() -> () {
        // TODO
    }

    pub fn toList() -> () {
        // TODO
    }

    pub fn updateLookupWithKey() -> () {
        // TODO
    }

    pub fn minViewWithKey() -> () {
        // TODO
    }

    pub fn keys() -> () {
        // TODO
    }

    pub fn splitLookup() -> () {
        // TODO
    }

    pub fn filterWithKey() -> () {
        // TODO
    }

    pub fn mapMaybe() -> () {
        // TODO
    }

    pub fn mapEither() -> () {
        // TODO
    }

    pub fn deleteFindMax() -> () {
        // TODO
    }

    pub fn mapWithKey() -> () {
        // TODO
    }

    pub fn unions() -> () {
        // TODO
    }

    pub fn filter() -> () {
        // TODO
    }

    pub fn null() -> () {
        // TODO
    }
}

pub mod IntSet {
    use std::collections::HashSet;

    pub type IntSet = HashSet<isize>;

    pub fn empty() -> HashSet<isize> {
        hashset![]
    }

    pub fn insert() -> () {
        // TODO
    }

    pub fn difference() -> () {
        // TODO
    }

    pub fn unions() -> () {
        // TODO
    }

    pub fn member() -> () {
        // TODO
    }

    pub fn null() -> bool {
        // TODO
        false
    }

    pub fn singleton() -> () {
        // TODO
    }

    pub fn fromList() -> () {
        // TODO
    }

    pub fn intersection() -> () {
        // TODO
    }

    pub fn union() -> () {
        // TODO
    }

    pub fn toList() -> () {
        // TODO
    }

    pub fn findMin() -> () {
        // TODO
    }

    pub fn size() -> () {
        // TODO
    }
}
