#![allow(non_snake_case)]
#![allow(unused_variables)]
#![allow(non_camel_case_types)]

extern crate num;

use num::ToPrimitive;
use std;
use std::str::FromStr;

pub trait OpAddable {
    fn add(self, right: Self) -> Self;
}

pub fn __op_addadd<A: OpAddable>(left: A, right: A) -> A {
    OpAddable::add(left, right)
}
impl OpAddable for String {
    fn add(mut self, right: Self) -> Self {
        self.push_str(&right);
        self
    }
}
impl<A> OpAddable for Vec<A> {
    fn add(mut self, right: Self) -> Self {
        self.extend(right);
        self
    }
}


pub trait OpConcatable {
    type Item;
    fn concat(self, right: Self::Item) -> Self;
}
pub fn __op_concat<A: OpConcatable>(left: A::Item, right: A) -> A {
    OpConcatable::concat(right, left)
}
impl OpConcatable for String {
    type Item = char;
    fn concat(self, right: Self::Item) -> Self {
        format!("{}{}", right, self)
    }
}
impl<A> OpConcatable for Vec<A> {
    type Item = A;
    fn concat(mut self, right: Self::Item) -> Self {
        self.insert(0, right);
        self
    }
}

pub fn __op_ne<A: PartialEq, B: PartialEq>(left: A, right: B) -> bool {
    left != right
}


pub struct IO<A: Sized>(A);

pub fn assertEqual<A: Eq + Sized>(desc: String, left: A, right: A) -> IO<()> {
    if left != right {
        panic!("{}", desc);
    }
    IO(())
}

pub fn putStrLn(line: String) -> IO<()> {
    println!("{}", line);
    IO(())
}

pub mod List {
    pub fn reverse<A>(mut input: Vec<A>) -> Vec<A> {
        input.reverse();
        input
    }
}

pub fn __op_index<F, T: ::std::ops::Index<F>>(a: T, pos: F) -> (<T as std::ops::Index<F>>::Output)
where
    <T as std::ops::Index<F>>::Output: std::marker::Sized + Clone,
{
    a[pos].clone()
}

#[macro_export]
macro_rules! __assign {
    ($left: expr, {
        $($field_name:ident: $field_type:expr),+ $(,)*
    }) => {
        // TODO
        {
            let mut left = $left;
            $( left.$field_name = $field_type; )+
            left
        }
    }
}

#[derive(Clone, Debug)]
pub enum Either<A, B> {
    Left(A),
    Right(B),
}
pub use self::Either::*;

impl<A, B> Either<A, B> {
    pub fn map<C, F: Fn(B) -> C>(self, f: F) -> Either<A, C> {
        match self {
            Either::Right(b) => Either::Right(f(b)),
            Either::Left(a) => Either::Left(a),
        }
    }
}


use std::fmt::Display;
pub fn show<A: Display>(a: A) -> String {
    format!("{}", a)
}

pub enum ExitCode {
    ExitSuccess,
    ExitFailure(isize),
}
pub use self::ExitCode::*;

pub fn isSuffixOf(a: String, r: String) -> bool {
    r.ends_with(&a)
}

pub fn isPrefixOf(a: String, r: String) -> bool {
    r.starts_with(&a)
}

pub fn elem<T: PartialEq>(item: T, value: Vec<T>) -> bool {
    value.contains(&item)
}

pub fn replicate<T: Clone>(rep: isize, item: T) -> Vec<T> {
    (0..rep).map(|_| item.clone()).collect()
}

pub fn words(input: String) -> Vec<String> {
    input.split_whitespace().map(|x| x.to_string()).collect()
}

pub fn unwords(input: Vec<String>) -> String {
    input.join(" ")
}

pub trait Lengthable {
    fn get_len(&self) -> isize;
}
pub fn length<A: Lengthable>(left: A) -> isize {
    Lengthable::get_len(&left)
}
impl Lengthable for String {
    fn get_len(&self) -> isize {
        self.len() as isize
    }
}
impl<T> Lengthable for Vec<T> {
    fn get_len(&self) -> isize {
        self.len() as isize
    }
}

pub trait Bindable<T> {
    fn bind_it(self, right: T) -> Self;
}
pub fn __op_bind<A: Bindable<B>, B>(left: A, b: B) -> A {
    Bindable::bind_it(left, b)
}
impl<T: Display> Bindable<T> for String {
    fn bind_it(mut self, right: T) -> Self {
        // TODO
        self.push_str(&format!("{}", right));
        self
    }
}


pub fn __op_forwardslash<A, B>(left: A, right: B) -> B {
    // TODO
    right
}

pub fn __op_dollarnot<A, B>(left: A, right: B) -> B {
    // TODO
    right
}


pub fn union<A: PartialEq>(mut left: Vec<A>, right: Vec<A>) -> Vec<A> {
    for item in right {
        if left.iter().position(|x| *x == item).is_none() {
            left.push(item);
        }
    }
    left
}

pub fn toInteger<T: Display>(left: T) -> isize {
    // TODO
    0
}

pub fn fromInteger(left: isize) -> String {
    // TODO
    "".to_string()
}

pub fn shiftL(l: isize, r: isize) -> isize {
    l << r
}

pub fn shiftR(l: isize, r: isize) -> isize {
    l >> r
}

pub fn fromEnum<A: ToPrimitive>(arg: A) -> isize {
    arg.to_isize().unwrap()
}

pub fn __op_dotted_and(l: isize, r: isize) -> isize {
    l & r
}

pub fn __op_dotted_or(l: isize, r: isize) -> isize {
    l | r
}

pub fn __op_assign_div(l: isize, r: isize) -> isize {
    l / r
}

pub fn __op_tuple2<A, B>(left: A, right: B) -> (A, B) {
    (left, right)
}

pub fn __op_power(l: isize, r: isize) -> isize {
    //TODO
    l
}

pub fn __mod(l: isize, r: isize) -> isize {
    // TODO
    l
}

pub fn not(left: bool) -> bool {
    !left
}

pub fn __break<T: Clone, F: Fn(T) -> bool>(cond: F, input: Vec<T>) -> (Vec<T>, Vec<T>) {
    let mut left = vec![];
    let mut right = vec![];
    for item in input.into_iter() {
        if right.is_empty() && cond(item.clone()) {
            left.push(item);
        } else {
            right.push(item);
        }
    }
    (left, right)
}

pub fn __break_str<F: Fn(char) -> bool>(cond: F, input: String) -> (String, String) {
    let mut left = vec![];
    let mut right = vec![];
    for item in input.chars() {
        if right.is_empty() && cond(item) {
            left.push(item);
        } else {
            right.push(item);
        }
    }
    (left.into_iter().collect(), right.into_iter().collect())
}

pub fn any<T: Clone, F: Fn(T) -> bool>(cond: F, input: Vec<T>) -> bool {
    input.iter().any(|x| cond(x.clone()))
}

pub fn isJust<T>(input: Option<T>) -> bool {
    input.is_some()
}

pub fn fromJust<T>(input: Option<T>) -> T {
    input.unwrap()
}

pub fn null<T>(input: Vec<T>) -> bool {
    input.is_empty()
}

pub fn lines(input: String) -> Vec<String> {
    input.lines().map(|x| x.to_string()).collect()
}

pub fn unlines(input: Vec<String>) -> String {
    input.join("\n")
}

pub fn ord(input: char) -> isize {
    input as isize
}

pub fn isAscii(input: char) -> bool {
    // TODO
    false
}

pub fn isPrint(input: char) -> bool {
    // TODO
    false
}

pub fn isOctDigit(input: char) -> bool {
    // TODO
    false
}

pub fn isDigit(input: char) -> bool {
    // TODO
    false
}

pub fn head(input: Vec<char>) -> char {
    input[0]
}

pub fn head_str(input: String) -> char {
    input.chars().nth(0).unwrap()
}

pub fn init(mut input: Vec<char>) -> Vec<char> {
    input.pop();
    input
}

pub fn init_str(input: String) -> String {
    let mut v: Vec<_> = input.chars().collect();
    v.pop();
    v.into_iter().collect()
}

pub fn tail(input: Vec<char>) -> Vec<char> {
    input[1..].to_vec()
}

pub fn tail_str(input: String) -> String {
    input.chars().skip(1).collect()
}

pub fn fst<A, B>(input: (A, B)) -> A {
    input.0
}

pub fn snd<A, B>(input: (A, B)) -> B {
    input.1
}

pub fn flip<A, B, C, F: Fn(A, B) -> C>(input: F, b: B, a: A) -> C {
    input(a, b)
}

pub fn take(len: isize, input: Vec<String>) {
    // TODO
}

pub fn take_str(len: isize, input: String) -> String {
    input.chars().take(len as usize).collect()
}

pub fn hasExtension(fp: FilePath) -> bool {
    // TODO
    false
}

pub fn replaceExtension(fp: FilePath, ext: String) -> FilePath {
    // TODO
    fp
}

pub fn addExtension(fp: FilePath, ext: String) -> FilePath {
    // TODO
    fp
}


pub fn takeWhile<T: Clone, F: Fn(T) -> bool>(cond: F, input: Vec<T>) -> Vec<T> {
    let mut left = vec![];
    for item in input.into_iter() {
        if cond(item.clone()) {
            left.push(item);
        } else {
            return left;
        }
    }
    left
}

pub fn takeWhile_str<F: Fn(char) -> bool>(cond: F, input: String) -> String {
    let mut left = vec![];
    for item in input.chars() {
        if cond(item.clone()) {
            left.push(item);
        } else {
            return left.into_iter().collect();
        }
    }
    left.into_iter().collect()
}


pub fn fromIntegral(left: isize) -> isize {
    left
}

pub fn drop<T>(len: isize, mut input: Vec<T>) -> Vec<T> {
    for _ in 0..len {
        input.remove(0);
    }
    input
}

pub fn drop_str(len: isize, input: String) -> String {
    input.chars().skip(len as usize).collect()
}

pub fn dropWhile<F: Fn(char) -> bool>(cond: F, input: String) -> String {
    let mut out = vec![];
    for item in input.chars() {
        if cond(item.clone()) && out.is_empty() {
            // skip
        } else {
            out.push(item);
        }
    }
    out.into_iter().collect()
}

pub fn span<F: Fn(char) -> bool>(cond: F, input: String) -> (String, String) {
    let mut left = vec![];
    let mut right = vec![];
    for item in input.chars() {
        if cond(item.clone()) && right.is_empty() {
            left.push(item);
        } else {
            right.push(item);
        }
    }
    (left.into_iter().collect(), right.into_iter().collect())
}

pub fn chr(input: isize) -> char {
    input as u8 as char
}

pub fn id<A>(input: A) -> A {
    input
}

pub fn __boxed_chars(input: String) -> Box<[char]> {
    input.chars().collect::<Vec<_>>().into_boxed_slice()
}

pub fn __boxed_slice<T: Sized>(input: Vec<T>) -> Box<[T]> {
    input.into_boxed_slice()
}

// bits

pub fn setBit(left: isize, right: isize) -> isize {
    left | (1 << right)
}

pub fn clearBit(left: isize, right: isize) -> isize {
    left & !(1 << right)
}

pub fn testBit(left: isize, right: isize) -> bool {
    left & (1 << right) != 0
}






// Monads

pub fn __return<A: Into<B>, B>(left: A) -> B {
    left.into()
}
// pub trait Functor {
//   fmap = liftM
// }

// pub trait Applicative P where
//   pure = return
//   (<*>) = ap

// pub trait Monad<P> {
//   fn ret(Self) -> Self;
//   fn bind(Self) -> Self;
//   fn fail(m) -> Self;
// }






















// ShowS, ReadS

pub trait ShowS {
    fn show_s(&self, String) -> String;
}


pub struct showOct(pub isize);
impl ShowS for showOct {
    fn show_s(&self, input: String) -> String {
        format!("{:o}{}", self.0, input)
    }
}

pub struct showHex(pub isize);
impl ShowS for showHex {
    fn show_s(&self, input: String) -> String {
        format!("{:x}{}", self.0, input)
    }
}

pub struct showString(pub String);
impl ShowS for showString {
    fn show_s(&self, input: String) -> String {
        format!("{}{}", self.0, input)
    }
}


pub trait ReadS<A> {
    fn read_s(&self) -> Vec<(A, String)>;
    fn map<F: Fn((isize, String)) -> (isize, String)>(self, f: F) -> Self
    where
        Self: Sized,
    {
        // TODO
        self
    }
}

// TODO
use std::fmt;
impl<A> fmt::Display for ReadS<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "...")
    }
}

pub struct readHex(pub String);
impl ReadS<isize> for readHex {
    fn read_s(&self) -> Vec<(isize, String)> {
        // TODO
        vec![]
    }
}
impl ReadS<char> for readHex {
    fn read_s(&self) -> Vec<(char, String)> {
        // TODO
        vec![]
    }
}

pub struct readOct(pub String);
impl ReadS<isize> for readOct {
    fn read_s(&self) -> Vec<(isize, String)> {
        // TODO
        vec![]
    }
}
impl ReadS<char> for readOct {
    fn read_s(&self) -> Vec<(char, String)> {
        // TODO
        vec![]
    }
}

pub struct readDec(pub String);
impl ReadS<isize> for readDec {
    fn read_s(&self) -> Vec<(isize, String)> {
        if let Ok(left) = isize::from_str(&self.0) {
            let right = self.0.chars().skip_while(|x| x.is_digit(10)).collect();
            vec![(left, right)]
        } else {
            vec![]
        }
    }
}






// BSC


// Char8
//TODO make this deal with u8's, not chars
pub mod BSC {
    pub fn head(input: Vec<u8>) -> char {
        input[0] as char
    }

    pub fn tail(input: Vec<u8>) -> Vec<u8> {
        if input.len() > 0 {
            input[1..].to_vec()
        } else {
            vec![]
        }
    }

    pub fn null(input: Vec<u8>) -> bool {
        input.is_empty()
    }

    pub fn lines(input: Vec<u8>) -> Vec<Vec<u8>> {
        //TODO
        vec![]
    }

    pub fn pack(input: String) -> Vec<u8> {
        input.chars().map(|x| x as u8).collect()
    }

    pub fn unpack(input: Vec<u8>) -> String {
        input.into_iter().map(|x| x as char).collect()
    }

    pub fn take(len: isize, input: Vec<u8>) -> Vec<u8> {
        input.into_iter().take(len as usize).collect()
    }
}

// ByteString
pub mod BSW {
    use FilePath;

    pub fn null(input: Vec<u8>) -> bool {
        input.is_empty()
    }

    pub fn head(input: Vec<u8>) -> u8 {
        input[0]
    }

    pub fn tail(input: Vec<u8>) -> Vec<u8> {
        if input.len() > 0 {
            input[1..].to_vec()
        } else {
            vec![]
        }
    }

    pub fn readFile(f: FilePath) -> Vec<u8> {
        use std::fs::File;
        use std::io::Read;

        // TODO
        let mut items = vec![];
        File::open(f.path).unwrap().read_to_end(&mut items);
        items
    }
}

pub type ByteString = Vec<u8>;

pub type Word8 = u8;



// Map stuff


#[macro_export]
macro_rules! __map {
    ($fn: expr) => {
        //TODO reject this
        panic!("need two arguments for map")
    };
    ($fn: expr, $target: expr) => {
        $target.into_iter()
            .map($fn)
            .collect::<Vec<_>>()
    }
}

#[macro_export]
macro_rules! __mapM {
    ($fn: expr) => {
        //TODO reject this
        panic!("need two arguments for map")
    };
    ($fn: expr, $target: expr) => {
        $target.into_iter()
            .map($fn)
            .collect::<Vec<_>>()
    }
}

#[macro_export]
macro_rules! __forM {
    ($target: expr, $fn: expr) => {
        $target.into_iter()
            .map($fn)
            .collect::<Vec<_>>()
    }
}

#[macro_export]
macro_rules! __fmap {
    ($fn: expr, $target: expr) => {
        $target.into_iter()
            .map($fn)
            .collect::<Vec<_>>()
    }
}

#[macro_export]
macro_rules! __filter {
    ($fn: expr) => {
        //TODO reject this
        panic!("need two arguments filter")
    };
    ($fn: expr, $target: expr) => {
        $target.into_iter()
            .filtermap($fn)
            .collect::<Vec<_>>()
    }
}

#[macro_export]
macro_rules! __foldrM {
    ($fn: expr, $base: expr, $target: expr) => {
        $target.into_iter()
            .rev()
            .fold($base, $fn)
            .collect::<Vec<_>>()
    }
}

#[macro_export]
macro_rules! __foldr {
    ($fn: expr, $base: expr, $target: expr) => {
        $target.into_iter()
            .rev()
            .fold($base, $fn)
            .collect::<Vec<_>>()
    }
}

#[macro_export]
macro_rules! __concatMap {
    ($fn: expr, $target: expr) => {
        $target.into_iter()
            .flat_map($fn)
            .collect::<Vec<_>>()
    }
}

#[macro_export]
macro_rules! __error {
    ($fn: expr) => {
        // TODO
        panic!("ERROR!")
    }
}





// IO fns

#[allow(dead_code)]
#[derive(Clone)]
pub struct FilePath {
    pub path: String,
}

impl From<String> for FilePath {
    fn from(value: String) -> Self {
        FilePath { path: value }
    }
}

impl From<FilePath> for String {
    fn from(value: FilePath) -> Self {
        value.path
    }
}

impl ToString for FilePath {
    fn to_string(&self) -> String {
        return self.path.clone();
    }
}

pub struct FileHandle {
    pub path: (),
}

pub fn openTempFile(t: FilePath, template: FilePath) -> (FilePath, FileHandle) {
    // TODO
    (
        FilePath {
            path: "".to_string(),
        },
        FileHandle { path: () },
    )
}

pub fn hClose(h: FileHandle) {
    // TODO
}

pub fn removeFile(p: FilePath) {
    // TODO
}

pub fn getTemporaryDirectory() -> FilePath {
    // TODO
    FilePath {
        path: "TODO".to_string(),
    }
}

pub fn takeFileName(h: FilePath) -> FilePath {
    // TODO
    h
}




// TODO what do we do here:

pub fn maybe<A, B, F: Fn(A) -> B>(default_: B, method: F, maybe: Option<A>) -> B {
    maybe.map(|x| method(x)).unwrap_or(default_)
}

pub fn fromMaybe<A>(left: A, right: Option<A>) -> A {
    right.unwrap_or(left)
}

pub fn bracket<A, B, C>(a: A, b: B, c: C) -> C {
    // TODO these are all methods
    c
}


pub fn seq<A, B>(a: A, b: B) -> B {
    // we don't do lazy
    b
}






// Map things

use std::hash::Hash;
use std::fmt::Debug;
use std::collections::HashSet;

pub mod Set {
    use std::hash::Hash;
    use std::fmt::Debug;
    use std::collections::HashSet;

    #[derive(Clone, Debug)]
    pub struct Set<T: Eq + Hash>(HashSet<T>);

    pub fn member<T: Eq + Hash + Debug>(item: T, list: Set<T>) -> Set<T> {
        list.0.contains(&item)
    }

    pub fn fromList<T: Eq + Hash + Debug>(list: Vec<T>) -> Set<T> {
        // TODO
        Set(HashSet::new())
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


// Array things

pub fn array<T>(dim: (isize, isize), mut list: Vec<(isize, T)>) -> Vec<T> {
    // Only supports an ordered array for now
    list.sort_by(|a, b| a.0.cmp(&b.0));
    assert_eq!(list.last().unwrap().0, dim.1 - 1);
    list.into_iter().map(|x| x.1).collect()
}

pub fn listArray<T>(dim: (isize, isize), list: Vec<T>) -> Vec<T> {
    list
}

pub struct Array<T, U> {
    idx: Vec<T>,
    inner: Vec<U>,
}

pub fn __op_array_index<T>(mut arr: Vec<T>, idx: isize) -> T {
    arr.remove(idx as usize)
}

pub fn __op_rshift(left: isize, right: isize) {
    // TODO
    unreachable!();
}


// Just Map things~*

pub mod Map {
    use std::collections::HashMap;

    pub fn singleton() -> () {}

    pub fn member() -> () {}

    pub fn lookup<A, B: Clone>(value: A, inside: HashMap<A, B>) -> Option<B> {
        inside.get(value).clone()
    }

    pub fn delete<A, B>(key: A, mut input: HashMap<A, B>) -> HashMap<A, B> {
        input.remove(key);
        input
    }

    pub fn insert<A, B>(key: A, value: B, mut input: HashMap<A, B>) -> HashMap<A, B> {
        input.insert(key, value);
        input
    }

    pub fn fromList<A, B>(_: Vec<(A, B)>) -> HashMap<A, B> {
        // TODO
        hashmap![]
    }

    pub fn unionWith() -> () {}

    pub fn fromListWith() -> () {}

    pub fn empty<A, B>() -> HashMap<A, B> {
        hashmap![]
    }

    pub type Map<A, B> = HashMap<A, B>;
}



pub mod IntMap {
    use std::collections::HashMap;

    pub fn lookup<B: Clone>(value: isize, inside: HashMap<isize, B>) -> Option<B> {
        inside.get(value).clone()
    }

    pub fn delete<B>(key: isize, mut input: HashMap<isize, B>) -> HashMap<isize, B> {
        input.remove(key);
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

    pub fn unionWith() -> () {}

    pub fn difference() -> () {}

    pub fn fromDistinctAscList() -> () {}

    pub fn size() -> () {}


    pub fn intersectionWith() -> () {}

    pub fn fromListWith() -> () {}

    pub fn findWithDefault() -> () {}

    pub fn keysSet() -> () {}

    pub fn elems() -> () {}

    pub fn mapMaybeWithKey() -> () {}

    pub fn empty<B>() -> HashMap<isize, B> {
        hashmap![]
    }

    pub fn intersection() -> () {}

    pub fn fromSet() -> () {}

    pub fn map() -> () {}

    pub fn union() -> () {}

    pub fn unionsWith() -> () {}

    pub fn singleton() -> () {}

    pub fn toList() -> () {}

    pub fn updateLookupWithKey() -> () {}

    pub fn minViewWithKey() -> () {}

    pub fn keys() -> () {}

    pub fn splitLookup() -> () {}

    pub fn filterWithKey() -> () {}

    pub fn mapMaybe() -> () {}

    pub fn mapEither() -> () {}

    pub fn deleteFindMax() -> () {}

    pub fn mapWithKey() -> () {}

    pub fn unions() -> () {}

    pub fn filter() -> () {}

    pub type IntMap<B> = HashMap<isize, B>;
}

pub mod IntSet {
    use std::collections::HashSet;

    pub fn empty<B>() -> HashSet<isize, B> {
        hashset![]
    }

    pub fn insert() -> () {}

    pub fn difference() -> () {}

    pub fn unions() -> () {}

    pub fn member() -> () {}

    pub fn null() -> bool {
        false
    }

    pub fn singleton() -> () {}

    pub fn fromList() -> () {}

    pub fn intersection() -> () {}

    pub fn union() -> () {}

    pub fn toList() -> () {}

    pub fn findMin() -> () {}

    pub fn size() -> () {}

    pub type IntSet = HashSet<isize>;
}


// Text
// https://hackage.haskell.org/package/pretty-1.1.3.5/docs/Text-PrettyPrint-HughesPJ.html#t:Doc

pub struct Doc {
    contents: String,
}

pub fn text(input: String) -> Doc {
    Doc { contents: input }
}




// TODO delete

pub fn lookup() -> () {}

pub fn sep() -> () {}

pub fn pPrint() -> () {}

pub fn nest() -> () {}

pub fn punctuate() -> () {}

pub fn pretty() -> () {}

pub fn render() -> () {}

pub fn __op_line_concat() -> () {}

pub fn __op_doc_concat() -> () {}

pub fn hsep() -> () {
    // TODO delete
}

pub fn step() -> () {}

pub fn prettyGroup() -> () {
    // TODO delete
}

pub fn put() -> () {}

pub fn modify() -> () {}

pub fn get() -> () {}

pub fn lift() -> () {
    // TODO delete
}

pub fn tell() -> () {
    // TODO delete
}

pub fn asks() -> () {
    // TODO delete
}

pub fn gets() -> () {
    // TODO delete
}

pub fn mappend() -> () {
    // TODO delete
}

pub fn vcat() -> () {
    // TODO delete
}

pub fn concat() -> () {}

pub fn fail() -> () {}

pub fn subscript() -> () {}

pub fn writeSTRef() -> () {}

pub fn readSTRef() -> () {}

pub fn newSTRef() -> () {}

pub fn evalRWST() -> () {}

pub fn mapRWST() -> () {}

pub struct RWST {}

pub fn sequence() -> () {}

pub fn zeroInitialize() -> () {}

pub fn foldl() -> () {}

pub fn unfoldl() -> () {}

pub fn unfoldr() -> () {}

pub fn foldM() -> () {}

pub fn forM() -> () {}

pub fn __pure() -> () {}

pub fn __op_dollar_arrow() -> () {}

pub fn __op_dollar() -> () {}

pub fn __op_div() -> () {}

pub fn catMaybes() -> () {}

pub fn mapStateT() -> () {}

pub fn runStateT() -> () {}

pub struct StateT<a, b, c> {}

pub fn join() -> () {}

pub trait Pretty {}

pub fn last() -> () {}

pub fn notElem() -> () {}

pub fn censor() -> () {}

pub fn local() -> () {}

pub fn listen() -> () {}


// Ordering

pub enum Ordering {
    LT,
    EQ,
    GT,
}
pub use self::Ordering::*;
