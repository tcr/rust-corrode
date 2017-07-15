// Original file: "CrateMap.hs"
// File auto-generated using Corollary.

#[macro_use] use corollary_support::*;

#[macro_export]
macro_rules! __fmap {
    ($fn: expr, $target: expr) => {
        $target.into_iter()
            .map($fn)
            .collect::<Vec<_>>()
    }
}

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Data::Foldable;
// use Data::List;
// use Data::Map;
// use Data::Maybe;

#[derive(Debug, Eq, Ord)]
pub enum ItemKind {
    Enum,
    Struct,
    Union,
    Type,
    Symbol
}
pub use self::ItemKind::*;

pub type ModuleMap = Vec<((ItemKind, String), String)>;

pub type CrateMap = Map::Map<String, ModuleMap>;

pub type CratesMap = Map::Map<String, CrateMap>;

pub type ItemRewrites = Map::Map<(ItemKind, String), Vec<String>>;

pub fn parseCrateMap() -> Either<String, CrateMap> {

    let root = |_0| {
        match (_0) {
            (__crate, right) if right.len() == 0 => {
                __crate
            },
            (__crate, unassigned) => {
                Map::insert("".to_string(), unassigned, __crate)
            },
        }
    };

    let cleanLine = words(takeWhile((__op_assign_div('#'))));

    let parseLine = |_0, _1| {
        match (_0, _1) {
            (__crate, items) if _0.len() == 2 && _0[0] == "-" => {
                item = _0[1];
                /*do*/ {
                    let item_q = parseItem(item);

                    (__crate, __op_concat(item_q, items))
                }
            },
            (__crate, items) if _0.len() == 1 && isSuffixOf(":", _0[0]) => {
                __return(Map::insert(init(_0[0]), items(__crate), []))
            },
            _ => {
                Left(unwords(vec!["invalid crate map entry:"].extend(_0)))
                /*do*/ {
                    let item_q = parseItem(item);

                    (__crate, __op_concat(item_q, items))
                }
            },
        }
    };

    fn parseItem(contents: Vec<String>) -> Either<String, ((ItemKind, String), String)> {
        let (kind, rest) = parseItemKind(contents);
        if &rest == &[name] {
            Right(((kind, name), name))
        } else if &rest = &[old, "as", new] {
            Right(((kind, old), new))
        } else {
            Left((unwords((__op_concat("unsupported crate map item:".to_string(), contents)))))
        }
    }

    fn parseItemKind(mut _0: Vec<String>) -> (ItemKind, Vec<String>) {
        let left = _0.remove(0);
        //TODO check out arms
        match (left, _0) {
            ("enum", rest) => {
                (Enum, rest)
            },
            ("struct", rest) => {
                (Enum, rest)
            },
            ("union", rest) => {
                (Enum, rest)
            },
            ("typedef", rest) => {
                (Enum, rest)
            },
            (left, mut rest) => {
                rest.insert(0, left);
                (Enum, rest)
            },
        }
    }

    __fmap!(root, __foldrM!(parseLine, 
        (Map::empty, vec![]), 
        __filter!(|x| not(null(x)), __map!(cleanLine, lines))))
}

pub fn mergeCrateMaps() -> Map::Map<String, CrateMap> {
    Map::fromListWith((Map::unionWith((__op_addadd))))
}

pub fn splitModuleMap(modName: String, crates: CratesMap) -> (ModuleMap, CratesMap) {
    if let Some(thisCrate) = Map::lookup("".to_string(), crates) {
        if let Some(thisModule) = Map::lookup(modName, thisCrate) {
            let thisCrate_q = Map::delete(modName, thisCrate);

            let crates_q = Map::insert("".to_string(), thisCrate_q, crates);

            return (thisModule, crates_q);
        }
    }

    //default
    (vec![], crates)
}

pub fn rewritesFromCratesMap(crates: CratesMap) -> ItemRewrites {
    //TODO
    // Map::fromList
    //     [ (item, setCrate [modName, new])
    //     | (crateName, mods) <- Map::toList(crates)
    //     , let setCrate = match crateName 
    //             "" -> id
    //             _ -> (crateName :)
    //     , (modName, items) <- Map::toList(mods)
    //     , (item, new) <- items
    //     ]
    // )
}



