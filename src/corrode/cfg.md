```rust
// Original file: "CFG.lhs"
// File auto-generated using Corollary.

use corollary_support::*;

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Control::Monad;
// use Control::Monad::Trans::State;
// use Data::Foldable;
// use Data::IntMap::Lazy;
// use Data::IntSet;
// use Data::Maybe;
// use Data::Traversable;
// use Text::PrettyPrint::HughesPJClass;

pub use ast as Rust;

pub struct BasicBlock<s, c>(s, Terminator<c>);


pub type Label = isize;

#[derive(Debug)]
pub enum Terminator_q<c, l> {
    Unreachable,
    Branch(l),
    CondBranch(c, l, l),
}
pub use self::Terminator_q::*;

pub type Terminator<c> = Terminator_q<c, Label>;

pub struct Unordered;


pub struct DepthFirst;


pub struct CFG<k, s, c>(Label, IntMap::IntMap<BasicBlock<s, c>>);


pub fn prettyCFG<k, s, c>(
    fmtS: fn(s) -> Doc,
    fmtC: fn(c) -> Doc,
    CFG(entry, blocks): CFG<k, s, c>,
) -> Doc {

    let blocks_q = /*do*/ {
            let (label, BasicBlock(stmts, term)) = IntMap::toList(blocks);

            let blockHead = __op_ne(text((show(label))), text(":".to_string()));

            let blockBody = fmtS(stmts);

            let blockTail = match term {
                    Unreachable => {
                        text("// unreachable".to_string())
                    },
                    Branch(to) => {
                        text((__op_addadd("goto ".to_string(), __op_addadd(show(to), ";".to_string()))))
                    },
                    CondBranch(cond, true, false) => {
                        __op_ne(text("if(".to_string()), __op_ne(fmtC(cond), __op_ne(text(") goto ".to_string()), __op_ne(text((show(true))), __op_ne(text("; else goto ".to_string()), __op_ne(text((show(false))), text(";".to_string())))))))
                    },
                };

            __op_concat(blockHead, __op_addadd(__map!((nest(4)), vec![blockBody, blockTail]), vec![text("".to_string())]))
        };

    vcat(__op_concat(
        (__op_ne(text("start @".to_string()), text((show(entry))))),
        blocks_q,
    ))
}

pub type BuildCFGT<m, s, c, k> = StateT<BuildState<s, c>, m, k>;

pub fn mapBuildCFGT<n, s, c, a, b>(input: BuildCFGT<n, s, c, a>) -> BuildCFGT<n, s, c, b> {
    // TODO
    mapStateT
}

pub struct BuildState<s, c>(Label, IntMap::IntMap<BasicBlock<s, c>>);
fn buildLabel<s, c>(a: BuildState<s, c>) -> Label {
    a.0
}
fn buildBlocks<s, c>(a: BuildState<s, c>) -> IntMap::IntMap<BasicBlock<s, c>> {
    a.1
}

pub fn newLabel<m, s, c>() -> BuildCFGT<m, s, c, Label> {
    /*do*/
    {
        let old = get;

        put(__assign!(old, {
                buildLabel: (buildLabel(old) + 1)
            }));
        (buildLabel(old))
    }
}

pub fn addBlock<m, s, c>(
    label: Label,
    stmt: s,
    terminator: Terminator<c>,
) -> BuildCFGT<m, s, c, ()> {
    /*do*/
    {
        modify(|st| {
            __assign!(st, {
                    buildBlocks: IntMap::insert(label, (BasicBlock(stmt, terminator)), (buildBlocks(st)))
                })
        })
    }
}

pub fn buildCFG<m, s, c>(root: BuildCFGT<m, s, c, Label>) -> CFG<Unordered, s, c> {
    /*do*/
    {
        let (label, __final) = runStateT(root, (BuildState(0, IntMap::empty)));

        (CFG(label, (buildBlocks(__final))))
    }
}

pub fn removeEmptyBlocks<k, f, c>(CFG(start, blocks): CFG<k, f, c>) -> CFG<Unordered, f, c> {

    fn go() -> () {
        /*do*/
        {
            let (empties, done) = get;

            match IntMap::minViewWithKey(empties) {
                None => (),
                Some(((from, to), empties_q)) => {
                    /*do*/
                    {
                        put((empties_q, done));
                        step(from, to);
                        go()
                    }
                }
            }
        }
    }

    let step = |from, to| {
        /*do*/
        {
            let (empties, done) = get;

            match IntMap::splitLookup(to, empties) {
                (_, None, _) => (),
                (e1, Some(to_q), e2) => {
                    /*do*/
                    {
                        put((IntMap::union(e1, e2), done));
                        step(to, to_q)
                    }
                }
            };
            let (empties_q, done_q) = get;

            let to_q = IntMap::findWithDefault(to, to, done_q);

            put((empties_q, IntMap::insert(from, to_q, done_q)))
        }
    };

    fn isBlockEmpty<s, l>(input: BasicBlock<s, l>) -> Option<l> {
        match input {
            BasicBlock(s, Branch(to)) if null(s) => Some(to),
            _ => None,
        }
    }

    let rewrites = snd(execState(
        go,
        (IntMap::mapMaybe(isBlockEmpty, blocks), IntMap::empty),
    ));

    let rewrite = |to| IntMap::findWithDefault(to, to, rewrites);

    let discards = IntMap::keysSet((IntMap::filterWithKey((__op_assign_div), rewrites)));

    let rewriteBlock = |_0, _1| match (_0, _1) {
        (from, _) if IntSet::member(from, discards) => None,
        (_, BasicBlock(b, term)) => Some(BasicBlock(b, __fmap!(rewrite, term))),
        _ => panic!("Irrefutable pattern"),
    };

    let blocks_q = IntMap::mapMaybeWithKey(rewriteBlock, blocks);

    CFG((rewrite(start)), blocks_q)
}

#[derive(Debug)]
pub enum StructureLabel<s, c> {
    GoTo(Label),   // structureLabel
    ExitTo(Label), // structureLabel
    Nested(Vec<Structure<s, c>>),
}
pub use self::StructureLabel::*;

pub fn structureLabel<s, c>(input: StructureLabel<s, c>) -> Label {
    match input {
        GoTo(l) => l,
        ExitTo(l) => l,
        _ => panic!("Does not have structureLabel"),
    }
}

pub type StructureTerminator<s, c> = Terminator_q<c, StructureLabel<s, c>>;

pub type StructureBlock<s, c> = (s, StructureTerminator<s, c>);

#[derive(Debug)]
pub enum Structure_q<s, c, a> {
    Simple(s, StructureTerminator<s, c>),
    Loop(a),
    Multiple(IntMap::IntMap<a>, a),
}
pub use self::Structure_q::*;

#[derive(Debug)]
pub struct Structure<s, c>(IntSet::IntSet, Structure_q<s, c, Vec<Structure<s, c>>>);
fn structureEntries<s, c>(a: Structure<s, c>) -> IntSet::IntSet {
    a.0
}
fn structureBody<s, c>(a: Structure<s, c>) -> Structure_q<s, c, Vec<Structure<s, c>>> {
    a.1
}

impl<s, c> Structure<s, c> {
    pub fn new(_0: IntSet::IntSet, _1: Structure_q<s, c, Vec<Structure<s, c>>>) -> Self {
        Structure {
            structureEntries: _0,
            structureBody: _1,
        }
    }
}

pub fn prettyStructure() -> Doc {

    let go = |_0| {
        match (_0) {
            Structure(_, Simple(s, term)) => {
                __op_line_concat(
                    text((__op_addadd(show(s), ";".to_string()))),
                    text((show(term))),
                )
            }
            Structure(entries, Loop(body)) => {
                prettyGroup(entries, "loop".to_string(), prettyStructure(body))
            }
            Structure(entries, Multiple(handlers, unhandled)) => {
                prettyGroup(
                    entries,
                    "match".to_string(),
                    /*TODO*/
                    (),
                )
                // vcat [ text (show entry ++ " =>") $+$ nest 2 (prettyStructure handler) | (entry, handler) <- IntMap.toList handlers ]
                // $+$ if null unhandled then mempty else (text "_ =>" $+$ nest 2 (prettyStructure unhandled))
            }
        }
    };

    let prettyGroup = |entries, kind, body| {
        __op_ne(
            text("{".to_string()),
            __op_ne(
                hsep(
                    (punctuate(
                        (text(",".to_string())),
                        (__map!((text(show)), (IntSet::toList(entries)))),
                    )),
                ),
                __op_line_concat(text((__op_addadd("} ".to_string(), kind))), nest(2, body)),
            ),
        )
    };

    vcat(__map!(go))
}

pub fn relooperRoot<k, c, s>(CFG(entry, blocks): CFG<k, s, c>) -> Vec<Structure<s, c>> {
    relooper(
        (IntSet::singleton(entry)),
        IntMap::map(
            (|BasicBlock(s, term)| (s, __fmap!(|x| GoTo(x), term))),
            blocks,
        ),
    )
}

pub fn relooper<c, s>(
    entries: IntSet::IntSet,
    blocks: IntMap::IntMap<StructureBlock<s, c>>,
) -> Vec<Structure<s, c>> {
    //TODO unsure if input type is correct
    fn grow<T>(r: IntSet::IntSet) -> IntMap::IntMap<T> {
        IntMap::map(
            (box |seen| IntSet::unions(__op_concat(seen, IntMap::elems((restrictKeys(r, seen)))))),
            r,
        )
    };

    fn go<T>(r: IntSet::IntSet) -> IntMap::IntMap<T> {
        //TODO unsure if output type is correct
        let r_q = grow(r);

        if __op_assign_div(r, r_q) {
            go(r_q)
        } else {
            r_q
        }
    };

    let strictReachableFrom = flipEdges((go((IntMap::map(successors, blocks)))));

    {
        let (returns, noreturns) = partitionMembers(
            entries,
            IntSet::unions(__map!(successors, IntMap::elems(blocks))),
        );

        let (present, absent) = partitionMembers(entries, (IntMap::keysSet(blocks)));

        match (IntSet::toList(noreturns), IntSet::toList(returns)) {
            (l, r) if l.is_empty() && r.is_empty() => vec![],
            (l, r) if l.len() == 1 && r.is_empty() => {
                let entry = l[0];
                match IntMap::updateLookupWithKey((|_, _| None), entry, blocks) {
                    (Some((s, term)), blocks_q) => {
                        __op_concat(
                            Structure {
                                structureEntries: entries,
                                structureBody: Simple(s, term),
                            },
                            relooper((successors((s, term))), blocks_q),
                        )
                    }
                    (None, _) => {
                        __op_concat(
                            Structure {
                                structureEntries: entries,
                                structureBody: Simple(vec![], (Branch((GoTo(entry))))),
                            },
                            vec![],
                        )
                    }
                }
            }
            _ if not((IntSet::null(absent))) => {
                __op_concat(
                    if IntSet::null(present) {
                        vec![]
                    } else {
                        Structure {
                            structureEntries: entries,
                            structureBody: Multiple(
                                (IntMap::fromSet(box |_| vec![], absent)),
                                (relooper(present, blocks)),
                            ),
                        }
                    },
                    vec![],
                )
            }
            (l, _) if l.len() == 0 => {
                let returns_q =
                    restrictKeys((IntMap::intersection(strictReachableFrom, blocks)), entries);

                let bodyBlocks = restrictKeys(
                    blocks,
                    IntSet::unions(
                        (__op_concat(IntMap::keysSet(returns_q), IntMap::elems(returns_q))),
                    ),
                );

                let followBlocks = IntMap::difference(blocks, bodyBlocks);

                let followEntries = outEdges(bodyBlocks);

                let markEdge = |_0: Label| -> Label {
                    match (_0) {
                        GoTo(label)
                            if IntSet::member(label, IntSet::union(followEntries, entries)) => {
                            ExitTo(label)
                        }
                        edge => edge,
                    }
                };

                let blocks_q = IntMap::map((|(s, term)| (s, __fmap!(markEdge, term))), bodyBlocks);

                __op_concat(
                    Structure {
                        structureEntries: entries,
                        structureBody: Loop((relooper(entries, blocks_q))),
                    },
                    relooper(followEntries, followBlocks),
                )
            }
            _ => {
                let reachableFrom = IntMap::unionWith(
                    IntSet::union,
                    (IntMap::fromSet(IntSet::singleton, entries)),
                    strictReachableFrom,
                );

                let singlyReached = flipEdges(IntMap::filter(
                    (box |r| (IntSet::size(r) == 1)),
                    IntMap::map((IntSet::intersection(entries)), reachableFrom),
                ));

                let handledEntries =
                    IntMap::map((box |within| restrictKeys(blocks, within)), singlyReached);

                let unhandledEntries = IntSet::difference(entries, IntMap::keysSet(handledEntries));

                let handledBlocks = IntMap::unions((IntMap::elems(handledEntries)));

                let followBlocks = IntMap::difference(blocks, handledBlocks);

                let followEntries = IntSet::union(unhandledEntries, outEdges(handledBlocks));

                let makeHandler = |entry, blocks_q| relooper((IntSet::singleton(entry)), blocks_q);

                let allHandlers = IntMap::mapWithKey(makeHandler, handledEntries);

                let (unhandled, handlers) = if (IntMap::keysSet(allHandlers) == entries) {
                    let (lastHandler, otherHandlers) = IntMap::deleteFindMax(allHandlers);
                    (snd(lastHandler), otherHandlers)
                } else {
                    (vec![], allHandlers)
                };

                __op_concat(
                    Structure {
                        structureEntries: entries,
                        structureBody: Multiple(handlers, unhandled),
                    },
                    relooper(followEntries, followBlocks),
                )
            }
        }
    }
}

pub fn restrictKeys<a>(m: IntMap::IntMap<a>, s: IntSet::IntSet) -> IntMap::IntMap<a> {
    IntMap::intersection(m, IntMap::fromSet(|_| (), s))
}

pub fn outEdges<s, c>(blocks: IntMap::IntMap<StructureBlock<s, c>>) -> IntSet::IntSet {
    IntSet::difference(
        IntSet::unions((__map!(successors, IntMap::elems(blocks)))),
        IntMap::keysSet(blocks),
    )
}

pub fn partitionMembers<a, b>(
    a: IntSet::IntSet,
    b: IntSet::IntSet,
) -> (IntSet::IntSet, IntSet::IntSet) {
    (IntSet::intersection(a, b), IntSet::difference(a, b))
}

pub fn successors<s, c>((_, term): StructureBlock<s, c>) -> IntSet::IntSet {
    IntSet::fromList(
        // TODO toList?
        ::std::iter::once(term)
            .map(|x| match x {
                GoTo(target) => target,
                _ => panic!("Irrefutable pattern"),
            })
            .collect(),
    )
}

pub fn flipEdges(edges: IntMap::IntMap<IntSet::IntSet>) -> IntMap::IntMap<IntSet::IntSet> {
    IntMap::unionsWith(
        IntSet::union,
        IntMap::toList(edges)
            .into_iter()
            .map(|(from, to)| {
                IntMap::fromSet(|_| IntSet::singleton(from), to)
            })
            .collect(),
    )
}

pub fn simplifyStructure<s, c>() -> Vec<Structure<s, c>> {

    let descend = |structure| {
        __assign!(structure, {
            structureBody: match structureBody(structure) {
                    Simple(s, term) => {
                        Simple(s, term)
                    },
                    Multiple(handlers, unhandled) => {
                        Multiple((IntMap::map(simplifyStructure, handlers)), (simplifyStructure(unhandled)))
                    },
                    Loop(body) => {
                        Loop((simplifyStructure(body)))
                    },
                }
        })
    };

    let go = |_0, _1| match (_0, _1) {
        (Structure(entries, Simple(s, term)), mut rest) if rest.len() == 2 => {
            let Structure(_, Multiple(handlers, unhandled)) = rest.remove(0);
            let rewrite = move |_0: Label| match (_0) {
                GoTo(to) => {
                    Nested(__op_concat(
                        Structure(
                            (IntSet::singleton(to)),
                            (Simple(vec![], (Branch((GoTo(to)))))),
                        ),
                        IntMap::findWithDefault(unhandled, to, handlers),
                    ))
                }
                _ => {
                    __error!((__op_addadd("simplifyStructure: Simple/Multiple invariants violated in ".to_string(), show(entries))))
                }
            };

            __op_concat(
                Structure::new(entries, (Simple(s, (__fmap!(rewrite, term))))),
                rest,
            )
        }
        (block, rest) => __op_concat(block, rest),
    };

    __foldr!(go, vec![], __map!(descend))
}

pub fn depthFirstOrder<k, s, c>(CFG(start, blocks): CFG<k, s, c>) -> CFG<DepthFirst, s, c> {

    fn search<s, c>(
        blocks: IntMap::IntMap<BasicBlock<s, c>>,
        label: Label,
        mut state: (IntSet::IntSet, Vec<Label>),
    ) -> (IntSet::IntSet, Vec<Label>) {
        let (seen, order) = state.clone();

        if !((IntSet::member(label, seen))) {
            state = (IntSet::insert(label, seen), order);
            match IntMap::lookup(label, blocks) {
                Some(BasicBlock(_, term)) => {
                    for item in term {
                        state = search(blocks, term, state);
                    }
                }
                _ => {}
            };
            state.1 = __op_concat(label, state.1);
        }

        state
    }

    let __final = search(blocks, start, (IntSet::empty, vec![])).1;

    let start_q = 0;

    let mapping = IntMap::fromList((__final.enumerate().map(|(a, b)| (b, a + start_q))));

    let rewrite = |label| {
        IntMap::findWithDefault(
            (__error!("basic block disappeared".to_string())),
            label,
            mapping,
        )
    };

    let rewriteBlock =
        |label, BasicBlock(body, term)| (label, BasicBlock(body, (__fmap!(rewrite, term))));

    let blocks_q = IntMap::fromList(
        (IntMap::elems((IntMap::intersectionWith(rewriteBlock, mapping, blocks)))),
    );

    CFG(start_q, blocks_q)
}

pub fn structureCFG<c, s>(
    mkBreak: fn(Option<Label>) -> s,
    mkContinue: fn(Option<Label>) -> s,
    mkLoop: fn(Label, s) -> s,
    mkIf: fn(c, s, s) -> s,
    mkGoto: fn(Label) -> s,
    mkMatch: fn(Vec<(Label, s)>, s) -> s,
    cfg: CFG<DepthFirst, s, c>,
) -> (bool, s) {

    let root = simplifyStructure((relooperRoot(cfg)));

    // TODO these types are not correct
    fn foo<s, c, m>(
        mkBreak: fn(Option<Label>) -> s,
        mkContinue: fn(Option<Label>) -> s,
        mkLoop: fn(Label, s) -> s,
        mkIf: fn(c, s, s) -> s,
        mkGoto: fn(Label) -> s,
        mkMatch: fn(Vec<(Label, s)>, s) -> s,
        exits: Vec<Label>,
        next_q: Vec<Rust::Stmt>,
        x: Structure<s, c>,
    ) {

        let go_q = |_0, _1| {
            match (_0, _1) {
                (Structure(entries, Simple(mut body, term)), next) => {
                    fn insertGoto<s>(
                        mkGoto: fn(Label) -> s,
                        _0: Label,
                        _1: (IntSet::IntSet, Vec<Rust::Stmt>),
                    ) -> Vec<Rust::Stmt> {
                        match (_0, _1) {
                            (_, (target, s)) if IntSet::size(target) == 1 => s,
                            (to, (_, s)) => {
                                let left = mkGoto(to);
                                left.extend(s);
                                left
                            }
                        }
                    };

                    let branch = |_0| {
                        match (_0) {
                            Nested(nested) => {
                                foo(
                                    mkBreak,
                                    mkContinue,
                                    mkLoop,
                                    mkIf,
                                    mkGoto,
                                    mkMatch,
                                    exits,
                                    next,
                                    nested,
                                )
                            }
                            to if structureLabel(IntSet::member(to, next)) => {
                                insertGoto(mkGoto, structureLabel(to), (next, vec![]))
                            }
                            ExitTo(to) => {
                                let inScope = |(label, local)| {
                                    /*do*/
                                    {
                                        IntMap::lookup(to, local)
                                            .map(|(follow, mkStmt)| (follow, mkStmt(label)))
                                    }
                                };

                                // target = msum (zipWith inScope (const Nothing : repeat Just) exits)
                                let target = exits
                                    .into_iter()
                                    .enumate()
                                    .map(|(i, (label, local))| if i == 0 {
                                        (None, local)
                                    } else {
                                        (Some(label), local)
                                    })
                                    .map(inScope)
                                    .fold(None, |mut left, input| {
                                        if let Some(input) = input {
                                            if left.is_none() {
                                                left = Some(vec![]);
                                            }
                                            left.as_mut().unwrap().extend(input);
                                        }
                                        left
                                    });

                                if isJust(target) {
                                    insertGoto(mkGoto, to, fromJust(target))
                                } else {
                                    __error!((__op_addadd("structureCFG: label ".to_string(), __op_addadd(show((structureLabel(to))), __op_addadd(" is not a valid exit from ".to_string(), show(entries))))))
                                }
                            }
                        }
                    };

                    body.extend(match term {
                        Unreachable => vec![],
                        Branch(to) => branch(to),
                        CondBranch(c, t, f) => mkIf(c, (branch(t)), (branch(f))),
                    })
                }
                (Structure(_, Multiple(handlers, unhandled)), next) => {
                    mkMatch(
                        IntMap::toList(handlers)
                            .into_iter()
                            .map(|x| match x {
                                (label, body) => (
                                    label,
                                    foo(
                                        mkBreak,
                                        mkContinue,
                                        mkLoop,
                                        mkIf,
                                        mkGoto,
                                        mkMatch,
                                        exits,
                                        next,
                                        body,
                                    ),
                                ),
                                _ => panic!("Irrefutable pattern"),
                            })
                            .collect(),
                        (foo(
                            mkBreak,
                            mkContinue,
                            mkLoop,
                            mkIf,
                            mkGoto,
                            mkMatch,
                            exits,
                            next,
                            unhandled,
                        )),
                    )
                }
                (Structure(entries, Loop(body)), next) => {
                    let label = IntSet::findMin(entries);

                    let exits_q = __op_concat(
                        (
                            label,
                            IntMap::union(
                                (IntMap::fromSet(|_| (entries, mkContinue), entries)),
                                (IntMap::fromSet(|_| (next, mkBreak), next)),
                            ),
                        ),
                        exits,
                    );

                    mkLoop(
                        label,
                        (foo(
                            mkBreak,
                            mkContinue,
                            mkLoop,
                            mkIf,
                            mkGoto,
                            mkMatch,
                            exits_q,
                            entries,
                            body,
                        )),
                    )
                }
            }
        };

        let go = |structure, (next, rest)| {
            (
                structureEntries(structure),
                mappend(go_q(structure, next), rest),
            )
        };

        snd(__foldr!(go, (next_q, vec![]), x))

    };

    (
        hasMultiple(root),
        foo(
            mkBreak,
            mkContinue,
            mkLoop,
            mkIf,
            mkGoto,
            mkMatch,
            vec![],
            vec![],
            root,
        ),
    )
}

pub fn hasMultiple<s, c>(list: Vec<Structure<s, c>>) -> bool {

    let go = |_0| match (_0) {
        Multiple {} => true,
        Simple(_, term) => vec![term].map(|x| hasMultiple(x)).any(|x| x),
        Loop(body) => hasMultiple(body),
    };

    list.into_iter().any(|x| go(structureBody(x)))
}
```
