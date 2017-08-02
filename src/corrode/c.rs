// Original file: "C.lhs"
// File auto-generated using Corollary.

#[macro_use]
use corollary_support::*;

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Control::Monad;
// use Control::Monad::ST;
// use Control::Monad::Trans::Class;
// use Control::Monad::Trans::Except;
// use Control::Monad::Trans::RWS::Strict;
// use Data::Foldable;
// use Data::Map::Lazy;
// use Data::IntMap::Strict;
// use Data::Maybe;
// use Data::List;
// use Data::STRef;
// use Data::Set;
// use Language::C;
// use Language::C::Data::Ident;
// use Language::Rust::AST;
// use Language::Rust::Corrode::CFG;
// use Language::Rust::Corrode::CrateMap;
// use Text::PrettyPrint::HughesPJClass;

use ast as Rust;
use parser_c::syntax::ast::*;
use corrode::cfg::*;
use corrode::crate_map::*;
use parser_c::syntax::ops::*;
use parser_c::data::ident::*;
use parser_c::data::node::*;
use parser_c::data::position::*;
use parser_c::syntax::constants::*;

pub type EnvMonad<s, x> = ExceptT<String, RWST<FunctionContext, Output, EnvState<s>, ST<s>>, x>;

pub struct FunctionContext {
    functionReturnType: Option<CType>,
    functionName: Option<String>,
    itemRewrites: ItemRewrites,
}
fn functionReturnType(a: FunctionContext) -> Option<CType> {
    a.functionReturnType
}
fn functionName(a: FunctionContext) -> Option<String> {
    a.functionName
}
fn itemRewrites(a: FunctionContext) -> ItemRewrites {
    a.itemRewrites
}

pub struct Output {
    outputItems: Vec<Rust::Item>,
    outputExterns: Map::Map<String, Rust::ExternItem>,
    outputIncomplete: Set::Set<String>,
}
fn outputItems(a: Output) -> Vec<Rust::Item> {
    a.outputItems
}
fn outputExterns(a: Output) -> Map::Map<String, Rust::ExternItem> {
    a.outputExterns
}
fn outputIncomplete(a: Output) -> Set::Set<String> {
    a.outputIncomplete
}

pub fn emitItems<s>(items: Vec<Rust::Item>) -> EnvMonad<s, ()> {
    lift(tell(Output {
        outputItems: items,
        outputExterns: Map::empty(),
        outputIncomplete: Set::empty(),
    }))
}

pub fn emitIncomplete<s>(kind: ItemKind, ident: Ident) -> EnvMonad<s, CType> {
    /*do*/
    {
        let rewrites = lift((asks(itemRewrites)));

        if !((Map::member((kind, identToString(ident)), rewrites))) {
            lift(tell(Output {
                outputItems: vec![],
                outputExterns: Map::empty(),
                outputIncomplete: Set::singleton((identToString(ident))),
            }))
        };
        __return((IsIncomplete(ident)))
    }
}

pub fn completeType<s>(orig: CType) -> EnvMonad<s, CType> {
    match orig {
        IsIncomplete(ident) => {
            /*do*/
            {
                let mty = getTagIdent(ident);

                fromMaybe((__return(orig)), mty)
            }
        }
        ty => __return(ty),
    }
}

pub struct GlobalState {
    unique: isize,
    usedForwardRefs: Set::Set<Ident>,
}
fn unique(a: GlobalState) -> isize {
    a.unique
}
fn usedForwardRefs(a: GlobalState) -> Set::Set<Ident> {
    a.usedForwardRefs
}

pub fn uniqueName<s>(base: String) -> EnvMonad<s, String> {
    modifyGlobal(box |st| {
        (
            __assign!(st, {
                unique: (unique(st) + 1)
            }),
            __op_addadd(base, show((unique(st)))),
        )
    })
}

pub fn useForwardRef<s>(ident: Ident) -> EnvMonad<s, ()> {
    modifyGlobal(box |st| {
        (
            __assign!(st, {
                usedForwardRefs: Set::insert(ident, (usedForwardRefs(st)))
            }),
            (),
        )
    })
}

pub struct EnvState<s> {
    symbolEnvironment: Vec<(Ident, EnvMonad<s, Result>)>,
    typedefEnvironment: Vec<(Ident, EnvMonad<s, IntermediateType>)>,
    tagEnvironment: Vec<(Ident, EnvMonad<s, CType>)>,
    globalState: GlobalState,
}
fn symbolEnvironment<s>(a: EnvState<s>) -> Vec<(Ident, EnvMonad<s, Result>)> {
    a.symbolEnvironment
}
fn typedefEnvironment<s>(a: EnvState<s>) -> Vec<(Ident, EnvMonad<s, IntermediateType>)> {
    a.typedefEnvironment
}
fn tagEnvironment<s>(a: EnvState<s>) -> Vec<(Ident, EnvMonad<s, CType>)> {
    a.tagEnvironment
}
fn globalState<s>(a: EnvState<s>) -> GlobalState {
    a.globalState
}

pub fn modifyGlobal<a, f, s>(f: Box<Fn(GlobalState) -> (GlobalState, a)>) -> EnvMonad<s, a> {
    lift(
        /*do*/
        {
            let st = get;

            let (global_q, a) = f((globalState(st)));

            put(__assign!(st, {
                    globalState: global_q
                }));
            __return(a)
        },
    )
}

pub fn applyRenames(ident: Ident) -> String {
    match identToString(ident) {
        "final" => "final_".to_string(),
        "fn" => "fn_".to_string(),
        "in" => "in_".to_string(),
        "let" => "let_".to_string(),
        "main" => "_c_main".to_string(),
        "match" => "match_".to_string(),
        "mod" => "mod_".to_string(),
        "proc" => "proc_".to_string(),
        "type" => "type_".to_string(),
        "where" => "where_".to_string(),
        name => name,
    }
}

pub fn getSymbolIdent<s>(ident: Ident) -> EnvMonad<s, Option<Result>> {

    let getFunctionName = |def| {
        /*do*/
        {
            let name = lift((asks(functionName)));

            let name_q = fromMaybe(def, name);

            __return(Some(Result {
                resultType: IsArray(Rust::Immutable, ((length(name_q) + 1)), charType),
                resultMutable: Rust::Immutable,
                result: Rust::Deref(
                    (Rust::Lit(
                        (Rust::LitByteStr((__op_addadd(name_q, "\u{0}".to_string())))),
                    )),
                ),
            }))
        }
    };

    let builtinSymbols = __op_addadd(
        /*do*/ {
            let w = vec![16, 32, 64];

            __return((__op_addadd("__builtin_bswap".to_string(), show(w)), Result {
                    resultType: IsFunc((IsInt(Unsigned, (BitWidth(w)))), vec![(None, IsInt(Unsigned, (BitWidth(w))))], false),
                    resultMutable: Rust::Immutable,
                    result: Rust::Path((Rust::PathSegments(vec![__op_addadd("u".to_string(), show(w)), "swap_bytes".to_string()])))
                }))
        },
        vec![
            ("__FILE__".to_string(), Result {
            resultType: IsPtr(Rust::Immutable, charType),
            resultMutable: Rust::Immutable,
            result: Rust::MethodCall((Rust::Call((Rust::Var((Rust::VarName("file!".to_string())))), vec![])), (Rust::VarName("as_ptr".to_string())), vec![])
        }),
            ("__LINE__".to_string(), Result {
            resultType: IsInt(Unsigned, (BitWidth(32))),
            resultMutable: Rust::Immutable,
            result: Rust::Call((Rust::Var((Rust::VarName("line!".to_string())))), vec![])
        }),
        ],
    );

    /*do*/
    {
        let env = lift(get);

        match lookup(ident, (symbolEnvironment(env))) {
            Some(symbol) => __fmap!(|x| Some(x), symbol),
            None => {
                match identToString(ident) {
                    "__func__" => getFunctionName("".to_string()),
                    "__FUNCTION__" => getFunctionName("".to_string()),
                    "__PRETTY_FUNCTION__" => getFunctionName("top level".to_string()),
                    name => __return(lookup(name, builtinSymbols)),
                }
            }
        }
    }
}

pub fn getTypedefIdent<s>(
    ident: Ident,
) -> EnvMonad<s, (String, Option<EnvMonad<s, IntermediateType>>)> {
    lift(
        /*do*/
        {
            let env = gets(typedefEnvironment);

            __return((identToString(ident), lookup(ident, env)))
        },
    )
}

pub fn getTagIdent<s>(ident: Ident) -> EnvMonad<s, Option<EnvMonad<s, CType>>> {
    lift(
        /*do*/
        {
            let env = gets(tagEnvironment);

            __return(lookup(ident, env))
        },
    )
}

pub fn addSymbolIdent<s>(ident: Ident, (__mut, ty): (Rust::Mutable, CType)) -> EnvMonad<s, String> {
    /*do*/
    {
        let name = applyRenames(ident);

        addSymbolIdentAction(
            ident,
            __return(Result {
                resultType: ty,
                resultMutable: __mut,
                result: Rust::Path((Rust::PathSegments(vec![name]))),
            }),
        );
        __return(name)
    }
}

pub fn addSymbolIdentAction<s>(ident: Ident, action: EnvMonad<s, Result>) -> EnvMonad<s, ()> {
    lift(
        /*do*/
        {
            modify(box |st| {
                __assign!(st, {
                        symbolEnvironment: __op_concat((ident, action), symbolEnvironment(st))
                    })
            })
        },
    )
}

pub fn addTypedefIdent<s>(ident: Ident, ty: EnvMonad<s, IntermediateType>) -> EnvMonad<s, ()> {
    lift(
        /*do*/
        {
            modify(box |st| {
                __assign!(st, {
                        typedefEnvironment: __op_concat((ident, ty), typedefEnvironment(st))
                    })
            })
        },
    )
}

pub fn addTagIdent<s>(ident: Ident, ty: EnvMonad<s, CType>) -> EnvMonad<s, ()> {
    lift(
        /*do*/
        {
            modify(box |st| {
                __assign!(st, {
                        tagEnvironment: __op_concat((ident, ty), tagEnvironment(st))
                    })
            })
        },
    )
}

pub fn addExternIdent<s>(
    ident: Ident,
    deferred: EnvMonad<s, IntermediateType>,
    mkItem: Box<Fn(String, (Rust::Mutable, CType)) -> Rust::ExternItem>,
) -> EnvMonad<s, ()> {
    /*do*/
    {
        let action = runOnce(
            /*do*/
            {
                let itype = deferred;

                let rewrites = lift(asks(itemRewrites));

                let path = match Map::lookup((Symbol, identToString(ident)), rewrites) {
                    Some(renamed) => __return((__op_concat("".to_string(), renamed))),
                    None => {
                        /*do*/
                        {
                            let name = applyRenames(ident);

                            let ty = (typeMutable(itype), typeRep(itype));

                            lift(tell(Output {
                                outputItems: vec![],
                                outputIncomplete: Set::empty(),
                                                outputExterns: Map::singleton(name, (mkItem(name, ty)))
                                            }));
                            __return(vec![name])
                        }
                    }
                };

                __return(
                    (typeToResult(itype, (Rust::Path((Rust::PathSegments(path)))))),
                )
            },
        );

        addSymbolIdentAction(ident, action)
    }
}

pub fn noTranslation<a, s, node: Pretty + Pos>(node: node, msg: String) -> EnvMonad<s, a> {
    throwE(concat(vec![
                show((posOf(node))),
                ": ".to_string(),
                msg,
                ":\n".to_string(),
                render((nest(4, (pretty(node))))),
            ]))
}

pub fn unimplemented<a, s, node: Pretty + Pos>(node: node) -> EnvMonad<s, a> {
    noTranslation(node, "Corrode doesn\'t handle this yet".to_string())
}

pub fn badSource<a, s, node: Pretty + Pos>(node: node, msg: String) -> EnvMonad<s, a> {
    noTranslation(
        node,
        (__op_addadd(
            "illegal ".to_string(),
            __op_addadd(
                msg,
                "; check whether a real C compiler accepts this".to_string(),
            ),
        )),
    )
}

pub fn interpretTranslationUnit(
    _thisModule: ModuleMap,
    rewrites: ItemRewrites,
    CTranslationUnit(decls, _): CTranslationUnit<NodeInfo>,
) -> Either<String, Vec<Rust::Item>> {

    let initFlow = FunctionContext {
        functionReturnType: None,
        functionName: None,
        itemRewrites: rewrites,
    };

    let initState = EnvState {
        symbolEnvironment: vec![],
        typedefEnvironment: vec![],
        tagEnvironment: vec![],
        globalState: GlobalState {
            unique: 1,
            usedForwardRefs: Set::empty,
        },
    };

    let perDecl = |_0| {
        match (_0) {
            CFDefExt(f) => interpretFunction(f),
            CDeclExt(decl_q) => {
                /*do*/
                {
                    let binds = interpretDeclarations(makeStaticBinding, decl_q);

                    emitItems(binds)
                }
            }
            decl => unimplemented(decl),
        }
    };

    let (err, output) = runST(
        (evalRWST((runExceptT((mapM_(perDecl, decls)))), initFlow, initState)),
    );

    let completeTypes = Set::fromList(catMaybes(
        /*do*/ {
            let item = outputItems(output);

            __return(match item {
                    Rust::Item(_, _, Rust::Struct(name, _)) => {
                        Some(name)
                    },
                    _ => {
                        None
                    },
                })
        }
    ));

    let incompleteTypes = Set::difference(outputIncomplete(output), completeTypes);

    let incompleteItems = /*do*/ {
        let name = Set::toList(incompleteTypes);

        __return(Rust::Item(vec![], Rust::Private, (Rust::Enum(name, vec![]))))
    };

    let itemNames = catMaybes(
       /*do*/ {
            let item = outputItems(output);

            __return(match item {
                    Rust::Item(_, _, Rust::Function(_, name, _, _, _)) => {
                        Some(name)
                    },
                    Rust::Item(_, _, Rust::Static(_, Rust::VarName(name), _, _)) => {
                        Some(name)
                    },
                    _ => {
                        None
                    },
                })
        }
    );

    let externs_q = /*do*/ {
        let (name, __extern) = Map::toList((outputExterns(output)));

        notElem(name, itemNames);
        __return(__extern)
    };

    let items = __op_addadd(incompleteItems, outputItems(output));

    let items_q = if null(externs_q) {
        items
    } else {
        __op_concat(
            Rust::Item(vec![], Rust::Private, (Rust::Extern(externs_q))),
            items,
        )
    };

    match err {
        Left(msg) => Left(msg),
        Right(_) => Right(items_q),
    }
}

pub type MakeBinding<s, a> = (
    Box<Fn(Rust::ItemKind) -> a>,
    Box<
        Fn(Rust::Mutable, Rust::VarName, CType, NodeInfo, Option<CInit>)
            -> EnvMonad<s, a>,
    >,
);

pub fn makeStaticBinding<s>() -> MakeBinding<s, Rust::Item> {
    let attrs = vec![Rust::Attribute("no_mangle".to_string())];

    let makeBinding = |__mut, var, ty, node, minit| {
        /*do*/
        {
            let expr = interpretInitializer(ty, (fromMaybe((CInitList(vec![], node)), minit)));

            __return(Rust::Item(
                attrs,
                Rust::Public,
                (Rust::Static(__mut, var, (toRustType(ty)), expr)),
            ))
        }
    };

    (Rust::Item(vec![], Rust::Private), makeBinding)
}

pub fn makeLetBinding<s>() -> MakeBinding<s, Rust::Stmt> {

    let makeBinding = |__mut, var, ty, _, minit| {
        /*do*/
        {
            let mexpr = __mapM!((interpretInitializer(ty)), minit);

            __return(Rust::Let(__mut, var, (Some((toRustType(ty)))), mexpr))
        }
    };

    (Rust::StmtItem(vec![]), makeBinding)
}

pub fn interpretDeclarations<b, s>(
    _0: MakeBinding<s, b>,
    declaration: CDecl,
) -> EnvMonad<s, Vec<b>> {
    match (_0, declaration) {
        ((fromItem, makeBinding), CDecl(specs, decls, _)) => {
            /*do*/
            {
                let (storagespecs, baseTy) = baseTypeOf(specs);

                let mbinds = __forM!(decls, box |declarator| { /*do*/ {
                                let (decl, minit) = match declarator {
                                        (Some(decl), minit, None) => {
                                            __return((decl, minit))
                                        },
                                        (None, _, _) => {
                                            badSource(declaration, "absent declarator".to_string())
                                        },
                                        (_, _, Some(_)) => {
                                            badSource(declaration, "bitfield declarator".to_string())
                                        },
                                    };

                                let (ident, derived) = match decl {
                                        CDeclarator(Some(ident), derived, _, _, _) => {
                                            __return((ident, derived))
                                        },
                                        _ => {
                                            badSource(decl, "abstract declarator".to_string())
                                        },
                                    };

                                let deferred = derivedDeferredTypeOf(baseTy, decl, vec![]);

                                match (storagespecs, derived) {
                                    (Some(CTypedef(_)), _) => {
                                        /*do*/ {
                                            if (isJust(minit)) { (badSource(decl, "initializer on typedef".to_string())) };
                                            addTypedefIdent(ident, deferred);
                                            __return(None)
                                        }
                                    },
                                    (Some(CStatic(_)), [CFunDeclr {

                                                    }, _]) => {
                                        /*do*/ {
                                            addSymbolIdentAction(ident, /*do*/ {
                                                    let itype = deferred;

                                                    useForwardRef(ident);
                                                    __return((typeToResult(itype, (Rust::Path((Rust::PathSegments(vec![applyRenames(ident)])))))))
                                                });
                                            __return(None)
                                        }
                                    },
                                    (_, [CFunDeclr {

                                                    }, _]) => {
                                        /*do*/ {
                                            addExternIdent(ident, deferred, box |name, (_mut, ty)| { match ty {
                                                        IsFunc(retTy, args, variadic) => {
                                                            {
                                                                let formals = /*do*/ {
        let (idx, (mname, argTy)) = zip(vec![1], args);

        let argName = maybe((__op_addadd("arg".to_string(), show(idx))), (applyRenames(snd)), mname);

        __return((Rust::VarName(argName), toRustType(argTy)))
    };

                                                            Rust::ExternFn(name, formals, variadic, (toRustRetType(retTy)))                                                            }
                                                        },
                                                        _ => {
                                                            __error!((__op_addadd(show(ident), " is both a function and not a function?".to_string())))
                                                        },
                                                    } });
                                            __return(None)
                                        }
                                    },
                                    (Some(CExtern(_)), _) => {
                                        /*do*/ {
                                            addExternIdent(ident, deferred, box |name, (__mut, ty)| { Rust::ExternStatic(__mut, (Rust::VarName(name)), (toRustType(ty))) });
                                            __return(None)
                                        }
                                    },
                                    (Some(CStatic(_)), _) => {
                                        /*do*/ {
                                            let IntermediateType {
                                                typeMutable: __mut,
                                                typeRep: ty
                                            } = deferred;

                                            let name = addSymbolIdent(ident, (__mut, ty));

                                            let expr = interpretInitializer(ty, (fromMaybe((CInitList(vec![], (nodeInfo(decl)))), minit)));

                                            __return((Some((fromItem((Rust::Static(__mut, (Rust::VarName(name)), (toRustType(ty)), expr)))))))
                                        }
                                    },
                                    _ => {
                                        /*do*/ {
                                            let IntermediateType {
                                                typeMutable: __mut,
                                                typeRep: ty
                                            } = deferred;

                                            let name = addSymbolIdent(ident, (__mut, ty));

                                            let binding = makeBinding(__mut, (Rust::VarName(name)), ty, (nodeInfo(decl)), minit);

                                            __return((Some(binding)))
                                        }
                                    },
                                }
                            } });

                __return((catMaybes(mbinds)))
            }
        }
        (_, CStaticAssert { .. }) => {
            let node = declaration;
            unimplemented(node)
        }
    }
}

pub struct Initializer(Option<Rust::Expr>, IntMap::IntMap<Initializer>);


pub fn scalar(expr: Rust::Expr) -> Initializer {
    Initializer((Some(expr)), IntMap::empty)
}

pub type CurrentObject = Option<Designator>;

#[derive(Debug)]
pub enum Designator {
    Base(CType),
    From(CType, isize, Vec<CType>, Designator),
}
pub use self::Designator::*;

pub fn designatorType(_0: Designator) -> CType {
    match (_0) {
        Base(ty) => ty,
        From(ty, _, _, _) => ty,
    }
}

pub fn objectFromDesignators<s>(_0: CType, _1: Vec<CDesignator>) -> EnvMonad<s, CurrentObject> {
    pub fn go<s>(_0: CType, _1: Vec<CDesignator>, _2: Designator) -> EnvMonad<s, Designator> {
        match (_0, _1, _2) {
            (_, [], obj) => {
                __pure(obj)
            },
            (IsArray(_, size, el), [CArrDesig(idxExpr, _), ds], obj) => {
                /*do*/ {
                    let idx = interpretConstExpr(idxExpr);

                    go(el, ds, (From(el, (fromInteger(idx)), (replicate(((size - (fromInteger(idx) - 1))), el)), obj)))
                }
            },
            (IsStruct(name, fields), [CMemberDesig(ident, _), ds], obj) => {
                /*do*/ {
                    let d = _1[0];
                    match span((box |(field, _)| { applyRenames }), fields) {
                        (_, []) => {
                            badSource(d, (__op_addadd("designator for field not in struct ".to_string(), name)))
                        },
                        (earlier, [(_, ty_q), rest]) => {
                            go(ty_q, ds, (From(ty_q, (length(earlier)), (__map!(snd, rest)), obj)))
                        },
                    }
                }
            },
            (ty_q, [d, _], _) => {
                badSource(d, (__op_addadd("designator for ".to_string(), show(ty_q))))
            },
        }
    }

    match (_0, _1) {
        (_, []) => __pure(None),
        (ty, desigs) => __op_dollar_arrow(Some, go(ty, desigs, (Base(ty)))),
    }
}

pub fn nextObject(_0: Designator) -> CurrentObject {
    match (_0) {
        Base {} => None,
        From(_, i, [ty, remaining], base) => Some((From(ty, ((i + 1)), remaining, base))),
        From(_, _, [], base) => nextObject(base),
    }
}

pub fn compatibleInitializer(_0: CType, _1: CType) -> bool {
    match (_0, _1) {
        (IsStruct(name1, _), IsStruct(name2, _)) => (name1 == name2),
        (IsStruct {}, _) => false,
        (_, IsStruct {}) => false,
        (_, _) => true,
    }
}

pub fn nestedObject(ty: CType, desig: Designator) -> Option<Designator> {
    match designatorType(desig) {
        IsArray(_, size, el) => Some((From(el, 0, (replicate(((size - 1)), el)), desig))),
        ty_q if compatibleInitializer(ty, ty_q) => Some(desig),
        IsStruct(_, [(_, ty_q), fields]) => {
            nestedObject(ty, (From(ty_q, 0, (__map!(snd, fields)), desig)))
        }
        _ => None,
    }
}

pub fn translateInitList<s>(ty: CType, list: CInitList) -> EnvMonad<s, Initializer> {
    /*do*/
    {
        let objectsAndInitializers = forM(list, box |(desigs, initial)| {
            /*do*/
            {
                let currObj = objectFromDesignators(ty, desigs);

                __pure((currObj, initial))
            }
        });

        let base = match ty {
            IsArray(_, size, el) => From(el, 0, (replicate(((size - 1)), el)), (Base(ty))),
            IsStruct(_, [(_, ty_q), fields]) => From(ty_q, 0, (__map!(snd, fields)), (Base(ty))),
            _ => Base(ty),
        };

        let (_, initializer) = foldM(
            resolveCurrentObject,
            (Some(base), Set::empty()),
            objectsAndInitializers,
        );

        __return(initializer)
    }
}

pub fn resolveCurrentObject<s>(
    (obj0, prior): (CurrentObject, Initializer),
    (obj1, cinitial): (CurrentObject, CInit),
) -> EnvMonad<s, (CurrentObject, Initializer)> {
    match obj1.or(obj0) {
        None => __return((None, prior)),
        Some(obj) => {
            /*do*/
            {
                let (obj_q, initial) = match cinitial {
                    CInitList(list_q, _) => {
                        /*do*/
                        {
                            let initial = translateInitList((designatorType(obj)), list_q);

                            __return((obj, initial))
                        }
                    }
                    CInitExpr(expr, _) => {
                        /*do*/
                        {
                            let expr_q = interpretExpr(true, expr);

                            match nestedObject((resultType(expr_q)), obj) {
                                None => badSource(cinitial, "type in initializer".to_string()),
                                Some(obj_q) => {
                                    /*do*/
                                    {
                                        let s = castTo((designatorType(obj_q)), expr_q);

                                        __return((obj_q, scalar(s)))
                                    }
                                }
                            }
                        }
                    }
                };

                let indices = unfoldr(
                    (box |o| match o {
                         Base {} => None,
                         From(_, j, _, p) => Some((j, p)),
                     }),
                    obj_q,
                );

                let initializer = foldl(
                    (box |a, j| Initializer(None, (IntMap::singleton(j, a)))),
                    initial,
                    indices,
                );

                __return((nextObject(obj_q), mappend(prior, initializer)))
            }
        }
    }
}

pub fn interpretInitializer<s>(ty: CType, initial: CInit) -> EnvMonad<s, Rust::Expr> {
    // TODO function prototype is wrong
    pub fn helper(initial: CInit, _0: Result, _1: Rust::Expr) -> Rust::Expr {
        match (_0, _1) {
            (_, Initializer(Some(expr), initials)) if IntMap::null(initials) => __return(expr),
            (IsArray(_, _, el), Initializer(expr, initials)) => {
                match expr {
                    None => {
                        __op_dollar_arrow(
                            Rust::ArrayExpr,
                            mapM(|x| helper(initial, el, x), (IntMap::elems(initials))),
                        )
                    }
                    Some(_) => unimplemented(initial),
                }
            }
            (IsStruct(__str, fields), Initializer(expr, initials)) => {
                let fields_q = forM((IntMap::toList(initials)), box |(idx, value)| {
                    match drop(idx, fields) {
                        [(field, ty_q), _] => {
                            /*do*/
                            {
                                let value_q = helper(initial, ty_q, value);

                                __return((field, value_q))
                            }
                        }
                        [] => {
                            noTranslation(
                                initial,
                                (__op_addadd(
                                    "internal error: ".to_string(),
                                    __op_addadd(
                                        show(_0),
                                        __op_addadd(
                                            " doesn\'t have enough fields to initialize field "
                                                .to_string(),
                                            show(idx),
                                        ),
                                    ),
                                )),
                            )
                        }
                    }
                });

                __op_dollar_arrow(
                    Rust::StructExpr(__str),
                    __op_mul_arrow(fields_q, __pure(expr)),
                )
            }
            (_, _) => badSource(initial, "initializer".to_string()),
        }
    }

    let zeroInitialize = |_0, _1, _2, _3| {
        match (_0, _1, _2, _3) {
            (i, __OP__, Initializer(None, initials), origTy) => {
                __op_bind(completeType(origTy), box |t| {
                    match t {
                        IsBool {} => __return(scalar((Rust::Lit((Rust::LitBool(false)))))),
                        IsVoid {} => badSource(initial, "initializer for void".to_string()),
                        IsInt {} => {
                            __return(scalar(
                                (Rust::Lit((Rust::LitInt(0, Rust::DecRepr, (toRustType(t)))))),
                            ))
                        }
                        IsFloat {} => {
                            __return(scalar(
                                (Rust::Lit((Rust::LitFloat("0".to_string(), (toRustType(t)))))),
                            ))
                        }
                        IsPtr {} => __return(scalar((Rust::Cast(0, (toRustType(t)))))),
                        IsArray(_, size, _) if (IntMap::size(initials) == size) => __return(i),
                        IsArray(_, size, elTy) => {
                            /*do*/
                            {
                                let elInit =
                                    zeroInitialize((Initializer(None, IntMap::empty)), elTy);

                                let el = helper(initial, elTy, elInit);

                                __return(
                                    (Initializer(
                                        (Some((Rust::RepeatArray(el, (fromIntegral(size)))))),
                                        initials,
                                    )),
                                )
                            }
                        }
                        IsFunc {} => __return(scalar((Rust::Cast(0, (toRustType(t)))))),
                        IsStruct(_, fields) => {
                            /*do*/
                            {
                                let fields_q = IntMap::fromDistinctAscList(
                                    __map!(snd, fields).into_iter().enumerate(),
                                );

                                let missing = IntMap::difference(fields_q, initials);

                                let zeros = __mapM!((zeroInitialize((Initializer(None, IntMap::empty)))), missing);

                                __return((Initializer(None, (IntMap::union(initials, zeros)))))
                            }
                        }
                        IsEnum {} => unimplemented(initial),
                        IsIncomplete(_) => {
                            badSource(initial, "initialization of incomplete type".to_string())
                        }
                    }
                })
            }
            (i, _) => __return(i),
        }
    };

    /*do*/
    {
        let initial_q = match initial {
            CInitExpr(expr, _) => {
                /*do*/
                {
                    let expr_q = interpretExpr(true, expr);

                    if compatibleInitializer(resultType(expr_q), ty) {
                        __pure(scalar((castTo(ty, expr_q))))
                    } else {
                        badSource(initial, "initializer for incompatible type".to_string())
                    }
                }
            }
            CInitList(list, _) => translateInitList(ty, list),
        };

        let zeroed = zeroInitialize(initial_q, ty);

        helper(initial, ty, zeroed)
    }
}

pub fn interpretFunction<s>(CFunctionDef(specs, declr, __OP__, CDeclarator(mident, _, _, _, _), argtypes, body, _): CFunctionDef<NodeInfo>) -> EnvMonad<s, ()>{
    /*do*/
    {
        let (storage, baseTy) = baseTypeOf(specs);

        let (attrs, vis) = match storage {
            None => {
                __return((
                    vec![Rust::Attribute("no_mangle".to_string())],
                    Rust::Public,
                ))
            }
            Some(CStatic(_)) => __return((vec![], Rust::Private)),
            Some(s) => badSource(s, "storage class specifier for function".to_string()),
        };

        let go = |name, funTy| {
            /*do*/
            {
                let (retTy, args) = match funTy {
                    IsFunc(_, _, true) => unimplemented(declr),
                    IsFunc(retTy, args, false) => __return((retTy, args)),
                    _ => badSource(declr, "function definition".to_string()),
                };

                if ((name == "_c_main".to_string())) {
                    (wrapMain(declr, name, (__map!(snd, args))))
                };
                let setRetTy = |flow| {
                    __assign!(flow, {
                        functionReturnType: Some(retTy),
                        functionName: Some(name)
                    })
                };

                let f_q = mapExceptT(
                    (local(setRetTy)),
                    scope(
                        /*do*/
                        {
                            let formals = sequence(
                                /*do*/ {
                                    let (arg, ty) = args;

                                    __return(match arg {
                                            Some((__mut, argident)) => {
                                                /*do*/ {
                                                    let argname = addSymbolIdent(argident, (__mut, ty));

                                                    __return((__mut, Rust::VarName(argname), toRustType(ty)))
                                                }
                                            },
                                            None => {
                                                badSource(declr, "anonymous parameter".to_string())
                                            },
                                        })
                                }
                            );

                            let returnValue = if (name == "_c_main".to_string()) {
                                Some(0)
                            } else {
                                None
                            };

                            let returnStatement = Rust::Stmt((Rust::Return(returnValue)));

                            let body_q = cfgToRust(
                                declr,
                                (interpretStatement(
                                    body,
                                    (__return((vec![returnStatement], Unreachable))),
                                )),
                            );

                            __return(
                                (Rust::Item(
                                    attrs,
                                    vis,
                                    (Rust::Function(
                                        vec![Rust::UnsafeFn, Rust::ExternABI(None)],
                                        name,
                                        formals,
                                        (toRustRetType(retTy)),
                                        (statementsToBlock(body_q)),
                                    )),
                                )),
                            )
                        },
                    ),
                );

                emitItems(vec![f_q])
            }
        };

        let ident = match mident {
            None => badSource(declr, "anonymous function definition".to_string()),
            Some(ident) => __return(ident),
        };

        let name = applyRenames(ident);

        let funTy = |itype| typeToResult(itype, (Rust::Path((Rust::PathSegments(vec![name])))));

        let deferred = __fmap!(|x| __fmap!(funTy, x), (derivedDeferredTypeOf(baseTy, declr, argtypes)));

        let alreadyUsed = lift(gets((usedForwardRefs(globalState))));

        match vis {
            Rust::Private if Set::notMember(ident, alreadyUsed) => {
                /*do*/
                {
                    let action = runOnce(
                        /*do*/
                        {
                            let ty = deferred;

                            go(name, (resultType(ty)));
                            __return(ty)
                        },
                    );

                    addSymbolIdentAction(ident, action)
                }
            }
            _ => {
                /*do*/
                {
                    let ty = deferred;

                    addSymbolIdentAction(ident, __return(ty));
                    go(name, (resultType(ty)))
                }
            }
        }
    }
}

pub fn wrapMain<s>(
    declr: CDeclarator<NodeInfo>,
    realName: String,
    argTypes: Vec<CType>,
) -> EnvMonad<s, ()> {

    let bind = |__mut, var, val| Rust::Let(__mut, var, None, (Some(val)));

    let call = |__fn, args| Rust::Call((Rust::Var((Rust::VarName(__fn)))), args);

    let chain = |method, args, obj| Rust::MethodCall(obj, (Rust::VarName(method)), args);

    let wrapEnvp = |arg| {
        match arg {
            [] => __return((vec![], vec![])),
            [IsPtr(Rust::Mutable, IsPtr(Rust::Mutable, ty))] if ty == charType => {
                let environ = Rust::VarName("environ".to_string());

                let setup = vec![
                        Rust::StmtItem(vec![], Rust::Extern(vec![Rust::ExternStatic(Rust::Immutable, environ, (toRustType(arg)))])),
                    ];

                let args = vec![Rust::Var(environ)];

                /*do*/ {
                    __return((setup, args))
                }
            }
            _ => unimplemented(declr),
        }
    };

    let wrapArgv = |_0| {
        match _0 {
            [] => __return((vec![], vec![])),
            [IsInt(Signed, BitWidth(32)), [IsPtr(Rust::Mutable, IsPtr(Rust::Mutable, ty)), rest]]
            if ty == charType => {
                let argcType = _0[0];
                
                let argv_storage = Rust::VarName("argv_storage".to_string());

                let argv = Rust::VarName("argv".to_string());

                let __str = Rust::VarName("str".to_string());

                let vec = Rust::VarName("vec".to_string());

                let setup = vec![
                        Rust::StmtItem(vec![], (Rust::Use("::std::os::unix::ffi::OsStringExt".to_string()))),
                        bind(Rust::Mutable, argv_storage, chain("collect::<Vec<_>>".to_string(), vec![], chain("map".to_string(), vec![
                                    Rust::Lambda(vec![__str], (Rust::BlockExpr((Rust::Block((__op_concat(bind(Rust::Mutable, vec, (chain("into_vec".to_string(), vec![], (Rust::Var(__str))))), exprToStatements((chain("push".to_string(), vec![Rust::Lit((Rust::LitByteChar('\u{0}')))], (Rust::Var(vec))))))), (Some((Rust::Var(vec))))))))),
                                ], call("::std::env::args_os".to_string(), vec![])))),
                        bind(Rust::Mutable, argv, chain("collect::<Vec<_>>".to_string(), vec![], chain("chain".to_string(), vec![
                                    call("Some".to_string(), vec![call("::std::ptr::null_mut".to_string(), vec![])]),
                                ], chain("map".to_string(), vec![
                                        Rust::Lambda(vec![vec], (chain("as_mut_ptr".to_string(), vec![], (Rust::Var(vec))))),
                                    ], chain("iter_mut".to_string(), vec![], Rust::Var(argv_storage)))))),
                    ];

                let args = vec![
                        Rust::Cast((chain("len".to_string(), vec![], (Rust::Var(argv_storage)))), (toRustType(argcType))),
                        chain("as_mut_ptr".to_string(), vec![], (Rust::Var(argv))),
                    ];

                /*do*/ {
                    let (envSetup, envArgs) = wrapEnvp(rest);

                    __return((__op_addadd(setup, envSetup), __op_addadd(args, envArgs)))
                }
            }
            _ => unimplemented(declr),
        }
    };

    /*do*/
    {
        let (setup, args) = wrapArgv(argTypes);

        let ret = Rust::VarName("ret".to_string());

        emitItems(vec![
                Rust::Item(vec![], Rust::Private, (Rust::Function(vec![], "main".to_string(), vec![], (Rust::TypeName("()".to_string())), (statementsToBlock((__op_addadd(setup, __op_addadd(vec![
                            bind(Rust::Immutable, ret, Rust::UnsafeExpr(Rust::Block(vec![], Some(call(realName, args))))),
                        ], exprToStatements((call("::std::process::exit".to_string(), vec![Rust::Var(ret)]))))))))))),
            ])
    }
}

pub struct OuterLabels(Option<Label>, Option<Label>, Option<CExpr>);
fn onBreak(a: OuterLabels) -> Option<Label> {
    a.0
}
fn onContinue(a: OuterLabels) -> Option<Label> {
    a.1
}
fn switchExpression(a: OuterLabels) -> Option<CExpr> {
    a.2
}

pub struct SwitchCases(IntMap::IntMap<Option<Result>>);


pub type CSourceBuildCFGT<s, a> = BuildCFGT<
    RWST<OuterLabels, SwitchCases, Map::Map<Ident, Label>, EnvMonad<s>>,
    Vec<Rust::Stmt>,
    Result,
    a,
>;

pub fn interpretStatement<s>(
    _0: CStat,
    _1: CSourceBuildCFGT<s, (Vec<Rust::Stmt>, Terminator<Result>)>,
) -> CSourceBuildCFGT<s, (Vec<Rust::Stmt>, Terminator<Result>)> {
    match (_0, _1) {
        (CLabel(ident, body, _, _), next) => {
            /*do*/
            {
                let label = gotoLabel(ident);

                let (rest, end) = interpretStatement(body, next);

                addBlock(label, rest, end);
                __return((vec![], Branch(label)))
            }
        }
        (CCase(expr, body, node), next) => {
            let stmt = _0;
            /*do*/
            {
                let selector = getSwitchExpression(stmt);

                let condition = CBinary(CEqOp, selector, expr, node);

                addSwitchCase((Some(condition)), body, next)
            }
        }
        (CCases(lower, upper, body, node), next) => {
            let stmt = _0;
            /*do*/
            {
                let selector = getSwitchExpression(stmt);

                let condition = CBinary(
                    CLndOp,
                    (CBinary(CGeqOp, selector, lower, node)),
                    (CBinary(CLeqOp, selector, upper, node)),
                    node,
                );

                addSwitchCase((Some(condition)), body, next)
            }
        }
        (CDefault(body, _), next) => addSwitchCase(None, body, next),
        (CExpr(None, _), next) => next,
        (CExpr(Some(expr), _), next) => {
            /*do*/
            {
                let expr_q = lift(lift(interpretExpr(false, expr)));

                let (rest, end) = next;

                __return((__op_addadd(resultToStatements(expr_q), rest), end))
            }
        }
        (CCompound([], items, _), next) => {
            mapBuildCFGT(
                (mapRWST(scope)),
                /*do*/
                { __foldr!(interpretBlockItem, next, items) },
            )
        }
        (CIf(c, t, mf, _), next) => {
            /*do*/
            {
                let c_q = lift(lift(interpretExpr(true, c)));

                let after = newLabel;

                let falseLabel = match mf {
                    None => __return(after),
                    Some(f) => {
                        /*do*/
                        {
                            let (falseEntry, falseTerm) =
                                interpretStatement(f, (__return((vec![], Branch(after)))));

                            let falseLabel = newLabel;

                            addBlock(falseLabel, falseEntry, falseTerm);
                            __return(falseLabel)
                        }
                    }
                };

                let (trueEntry, trueTerm) =
                    interpretStatement(t, (__return((vec![], Branch(after)))));

                let trueLabel = newLabel;

                addBlock(trueLabel, trueEntry, trueTerm);
                let (rest, end) = next;

                addBlock(after, rest, end);
                __return((vec![], CondBranch(c_q, trueLabel, falseLabel)))
            }
        }
        (CSwitch(expr, body, node), next) => {
            let stmt = _0;
            /*do*/
            {
                let (bindings, expr_q) = match expr {
                    CVar {} => __return((vec![], expr)),
                    _ => {
                        lift(lift(
                            /*do*/
                            {
                                let ident = __fmap!(internalIdent, (uniqueName("switch".to_string())));

                                let rhs = interpretExpr(true, expr);

                                let var = addSymbolIdent(ident, (Rust::Immutable, resultType(rhs)));

                                __return((
                                    vec![
                                                Rust::Let(Rust::Immutable, (Rust::VarName(var)), None, (Some((result(rhs))))),
                                            ],
                                    CVar(ident, node),
                                ))
                            },
                        ))
                    }
                };

                let after = newLabel;

                let (_, SwitchCases(cases)) = getSwitchCases(
                    expr_q,
                    setBreak(
                        after,
                        interpretStatement(body, (__return((vec![], Branch(after))))),
                    ),
                );

                fn isDefault<T>(value: Option<T>) -> Either<T, ()> {
                    match value {
                        Some(condition) => Left(condition),
                        None => Right(()),
                    }
                };

                let (conditions, defaults) = IntMap::mapEither(isDefault, cases);

                let defaultCase = match IntMap::keys(defaults) {
                    [] => __return(after),
                    [defaultCase] => __return(defaultCase),
                    _ => lift(lift(badSource(stmt, "duplicate default cases".to_string()))),
                };

                let conditionBlock = |(target, condition), defaultCase| {
                    /*do*/
                    {
                        let label = newLabel;

                        addBlock(label, vec![], (CondBranch(condition, target, defaultCase)));
                        __return(label)
                    }
                };

                let entry = __foldrM!(conditionBlock, defaultCase, (IntMap::toList(conditions)));

                let (rest, end) = next;

                addBlock(after, rest, end);
                __return((bindings, Branch(entry)))
            }
        }
        (CWhile(c, body, doWhile, _), next) => {
            /*do*/
            {
                let c_q = lift(lift(interpretExpr(true, c)));

                let after = newLabel;

                let headerLabel = newLabel;

                let (bodyEntry, bodyTerm) = setBreak(
                    after,
                    setContinue(
                        headerLabel,
                        interpretStatement(body, (__return((vec![], Branch(headerLabel))))),
                    ),
                );

                let bodyLabel = newLabel;

                addBlock(bodyLabel, bodyEntry, bodyTerm);
                addBlock(
                    headerLabel,
                    vec![],
                    match toBool(c_q) {
                        Rust::Lit(Rust::LitBool(cont)) if __op_assign_div(cont, doWhile) => {
                            Branch((if cont { bodyLabel } else { after }))
                        }
                        _ => CondBranch(c_q, bodyLabel, after),
                    },
                );
                let (rest, end) = next;

                addBlock(after, rest, end);
                __return((
                    vec![],
                    Branch((if doWhile { bodyLabel } else { headerLabel })),
                ))
            }
        }
        (CFor(initial, mcond, mincr, body, _), next) => {
            /*do*/
            {
                let after = newLabel;

                let ret = mapBuildCFGT(
                    (mapRWST(scope)),
                    /*do*/
                    {
                        let prefix = match initial {
                            Left(None) => __return(vec![]),
                            Left(Some(expr)) => {
                                /*do*/
                                {
                                    let expr_q = lift(lift(interpretExpr(false, expr)));

                                    __return((resultToStatements(expr_q)))
                                }
                            }
                            Right(decls) => {
                                lift(lift(interpretDeclarations(makeLetBinding, decls)))
                            }
                        };

                        let headerLabel = newLabel;

                        let incrLabel = match mincr {
                            None => __return(headerLabel),
                            Some(incr) => {
                                /*do*/
                                {
                                    let incr_q = lift(lift(interpretExpr(false, incr)));

                                    let incrLabel = newLabel;

                                    addBlock(
                                        incrLabel,
                                        (resultToStatements(incr_q)),
                                        (Branch(headerLabel)),
                                    );
                                    __return(incrLabel)
                                }
                            }
                        };

                        let (bodyEntry, bodyTerm) = setBreak(
                            after,
                            setContinue(
                                incrLabel,
                                interpretStatement(body, (__return((vec![], Branch(incrLabel))))),
                            ),
                        );

                        let bodyLabel = newLabel;

                        addBlock(bodyLabel, bodyEntry, bodyTerm);
                        let cond = match mcond {
                            Some(cond) => {
                                /*do*/
                                {
                                    let cond_q = lift(lift(interpretExpr(true, cond)));

                                    __return((CondBranch(cond_q, bodyLabel, after)))
                                }
                            }
                            None => __return((Branch(bodyLabel))),
                        };

                        addBlock(headerLabel, vec![], cond);
                        __return((prefix, Branch(headerLabel)))
                    },
                );

                let (rest, end) = next;

                addBlock(after, rest, end);
                __return(ret)
            }
        }
        (CGoto(ident, _), next) => {
            /*do*/
            {
                let _ = next;

                let label = gotoLabel(ident);

                __return((vec![], Branch(label)))
            }
        }
        (CCont(_), next) => {
            let stmt = _0;
            /*do*/
            {
                let _ = next;

                let val = lift((asks(onContinue)));

                match val {
                    Some(label) => __return((vec![], Branch(label))),
                    None => lift(lift(badSource(stmt, "continue outside loop".to_string()))),
                }
            }
        }
        (CBreak(_), next) => {
            let stmt = _0;
            /*do*/
            {
                let _ = next;

                let val = lift((asks(onBreak)));

                match val {
                    Some(label) => __return((vec![], Branch(label))),
                    None => lift(lift(badSource(stmt, "break outside loop".to_string()))),
                }
            }
        }
        (CReturn(expr, _), next) => {
            let stmt = _0;
            /*do*/
            {
                let _ = next;

                lift(lift(
                    /*do*/
                    {
                        let val = lift((asks(functionReturnType)));

                        match val {
                            None => {
                                badSource(stmt, "return statement outside function".to_string())
                            }
                            Some(retTy) => {
                                /*do*/
                                {
                                    let expr_q = __mapM!((__fmap!((castTo(retTy)), interpretExpr(true))), expr);

                                    __return(
                                        (exprToStatements((Rust::Return(expr_q))), Unreachable),
                                    )
                                }
                            }
                        }
                    },
                ))
            }
        }
        (stmt, _) => lift(lift(unimplemented(stmt))),
    }
}

pub fn setBreak<a, s>(label: Label, _curry_1: CSourceBuildCFGT<s, a>) -> CSourceBuildCFGT<s, a> {
    mapBuildCFGT(
        (local(
            (box |flow| {
                 __assign!(flow, {
                    onBreak: Some(label)
                })
             }),
        )),
        _curry_1,
    )
}

pub fn setContinue<a, s>(label: Label, _curry_1: CSourceBuildCFGT<s, a>) -> CSourceBuildCFGT<s, a> {
    mapBuildCFGT(
        (local(
            (box |flow| {
                 __assign!(flow, {
                    onContinue: Some(label)
                })
             }),
        )),
        _curry_1,
    )
}

pub fn getSwitchExpression<s>(stmt: CStat) -> CSourceBuildCFGT<s, CExpr> {
    /*do*/
    {
        let mexpr = lift(asks(switchExpression));

        match mexpr {
            None => lift(lift(badSource(stmt, "case outside switch".to_string()))),
            Some(expr) => __return(expr),
        }
    }
}

pub fn addSwitchCase<s>(
    condition: Option<CExpr>,
    body: CStat,
    next: CSourceBuildCFGT<s, (Vec<Rust::Stmt>, Terminator<Result>)>,
) -> CSourceBuildCFGT<s, (Vec<Rust::Stmt>, Terminator<Result>)> {
    /*do*/
    {
        let condition_q = lift(lift(__mapM!((interpretExpr(true)), condition)));

        let next_q = interpretStatement(body, next);

        let label = match next_q {
            ([], Branch(to)) => __return(to),
            (rest, end) => {
                /*do*/
                {
                    let label = newLabel;

                    addBlock(label, rest, end);
                    __return(label)
                }
            }
        };

        lift(tell(SwitchCases(IntMap::singleton(label, condition_q))));
        __return((vec![], Branch(label)))
    }
}

pub fn getSwitchCases<a, s>(
    expr: CExpr,
    _curry_1: CSourceBuildCFGT<s, a>,
) -> CSourceBuildCFGT<s, (a, SwitchCases)> {

    let wrap = |body| {
        /*do*/
        {
            let ((a, st), cases) = censor(
                |_| vec![],
                local(
                    (box |flow| {
                         __assign!(flow, {
                                switchExpression: Some(expr)
                            })
                     }),
                    listen(body),
                ),
            );

            __return(((a, cases), st))
        }
    };

    mapBuildCFGT(wrap, _curry_1)
}

pub fn gotoLabel<s>(ident: Ident) -> CSourceBuildCFGT<s, Label> {
    /*do*/
    {
        let labels = lift(get);

        match Map::lookup(ident, labels) {
            None => {
                /*do*/
                {
                    let label = newLabel;

                    lift((put((Map::insert(ident, label, labels)))));
                    __return(label)
                }
            }
            Some(label) => __return(label),
        }
    }
}

pub fn cfgToRust<s, node: Pretty + Pos>(
    _node: node,
    build: CSourceBuildCFGT<s, (Vec<Rust::Stmt>, Terminator<Result>)>,
) -> EnvMonad<s, Vec<Rust::Stmt>> {

    let loopLabel = |l| Rust::Lifetime((__op_addadd("loop".to_string(), show(l))));

    let mkBreak = |l| exprToStatements((Rust::Break((__fmap!(loopLabel, l)))));

    let mkContinue = |l| exprToStatements((Rust::Continue((__fmap!(loopLabel, l)))));

    let mkLoop =
        |l, b| exprToStatements((Rust::Loop((Some((loopLabel(l)))), (statementsToBlock(b)))));

    // TODO make fn
    let simplifyIf = |_0, _1, _2| match (_0, _1, _2) {
        (c, Rust::Block([], None), Rust::Block([], None)) => result(c),
        (c, Rust::Block([], None), f) => {
            Rust::IfThenElse((toNotBool(c)), f, (Rust::Block(vec![], None)))
        }
        (c, t, f) => Rust::IfThenElse((toBool(c)), t, f),
    };

    let mkIf = |c, t, f| {
        exprToStatements(
            (simplifyIf(c, (statementsToBlock(t)), (statementsToBlock(f)))),
        )
    };

    let currentBlock = Rust::VarName("_currentBlock".to_string());

    let declCurrent = Rust::Let(Rust::Mutable, currentBlock, None, None);

    let mkGoto = |l| {
        exprToStatements(
            (Rust::Assign(
                (Rust::Var(currentBlock)),
                Rust::__id_3a3d,
                (fromIntegral(l)),
            )),
        )
    };

    let go = |(l, t), f| {
        exprToStatements((Rust::IfThenElse((Rust::CmpEQ((Rust::Var(currentBlock)), (fromIntegral(l)))), (statementsToBlock(t)), (statementsToBlock(f)))))
    };

    let simplifyIf = |_0, _1, _2| {
        match (_0, _1, _2) {
            (c, Rust::Block([], None), Rust::Block([], None)) => {
                result(c)
            },
            (c, Rust::Block([], None), f) => {
                Rust::IfThenElse((toNotBool(c)), f, (Rust::Block(vec![], None)))
            },
            (c, t, f) => {
                Rust::IfThenElse((toBool(c)), t, f)
            },
        }
    };

    let mkMatch = |x, y| __foldr!(go, y, x);

    /*do*/
    {
        let builder = buildCFG(
            /*do*/
            {
                let (early, term) = build;

                let entry = newLabel;

                addBlock(entry, early, term);
                __return(entry)
            },
        );

        let (rawCFG, _) = evalRWST(builder, (OuterLabels(None, None, None)), Map::empty);

        let cfg = depthFirstOrder((removeEmptyBlocks(rawCFG)));

        let (hasGoto, structured) =
            structureCFG(mkBreak, mkContinue, mkLoop, mkIf, mkGoto, mkMatch, cfg);

        __return(if hasGoto {
            __op_concat(declCurrent, structured)
        } else {
            structured
        })
    }
}

pub fn interpretBlockItem<s>(
    _0: CBlockItem,
    _1: CSourceBuildCFGT<s, (Vec<Rust::Stmt>, Terminator<Result>)>,
) -> CSourceBuildCFGT<s, (Vec<Rust::Stmt>, Terminator<Result>)> {
    match (_0, _1) {
        (CBlockStmt(stmt), next) => interpretStatement(stmt, next),
        (CBlockDecl(decl), next) => {
            /*do*/
            {
                let decl_q = lift(lift((interpretDeclarations(makeLetBinding, decl))));

                let (rest, end) = next;

                __return((__op_addadd(decl_q, rest), end))
            }
        }
        (item, _) => lift(lift((unimplemented(item)))),
    }
}

pub fn scope<a, m, s>(m: EnvMonad<s, a>) -> EnvMonad<s, a> {
    /*do*/
    {
        let old = lift(get);

        let a = m;

        lift(
            (modify(
                (box |st| {
                     __assign!(old, {
                        globalState: globalState(st)
                    })
                 }),
            )),
        );
        __return(a)
    }
}

pub fn blockToStatements(Rust::Block(stmts, mexpr): Rust::Block) -> Vec<Rust::Stmt> {
    match mexpr {
        Some(expr) => __op_addadd(stmts, exprToStatements(expr)),
        None => stmts,
    }
}

pub fn statementsToBlock(_0: Vec<Rust::Stmt>) -> Rust::Block {
    match (_0) {
        [Rust::Stmt(Rust::BlockExpr(stmts))] => stmts,
        stmts => Rust::Block(stmts, None),
    }
}

pub fn exprToStatements(_0: Rust::Expr) -> Vec<Rust::Stmt> {
    let extractExpr = |x| statementsToBlock(blockToStatements(x));

    match (_0) {
        Rust::IfThenElse(c, t, f) => {
            vec![Rust::Stmt((Rust::IfThenElse(c, (extractExpr(t)), (extractExpr(f)))))]
        }
        Rust::BlockExpr(b) => blockToStatements(b),
        e => vec![Rust::Stmt(e)],
    }
}

pub struct Result {
    resultType: CType,
    resultMutable: Rust::Mutable,
    result: Rust::Expr,
}
fn resultType(a: Result) -> CType {
    a.resultType
}
fn resultMutable(a: Result) -> Rust::Mutable {
    a.resultMutable
}
fn result(a: Result) -> Rust::Expr {
    a.result
}

pub fn resultToStatements(_curry_0: Result) -> Vec<Rust::Stmt> {
    exprToStatements(result(_curry_0))
}

pub fn typeToResult(itype: IntermediateType, expr: Rust::Expr) -> Result {
    Result {
        resultType: typeRep(itype),
        resultMutable: typeMutable(itype),
        result: expr,
    }
}

pub fn interpretExpr<s>(_0: bool, expr: CExpr) -> EnvMonad<s, Result> {
    let literalNumber = |ty, lit| {
        Result {
            resultType: ty,
            resultMutable: Rust::Immutable,
            result: Rust::Lit(lit, toRustType(ty)),
        }
    };

    match (_0, expr) {
        (demand, CComma(exprs, _)) => {
            /*do*/
            {
                let (effects, mfinal) = if demand {
                    (init(exprs), Some((last(exprs))))
                } else {
                    (exprs, None)
                };

                let effects_q = __mapM!((__fmap!(resultToStatements, interpretExpr(false))), effects);

                let mfinal_q = __mapM!((interpretExpr(true)), mfinal);

                __return(Result {
                    resultType: maybe(IsVoid, resultType, mfinal_q),
                    resultMutable: maybe(Rust::Immutable, resultMutable, mfinal_q),
                    result: Rust::BlockExpr(
                        (Rust::Block((concat(effects_q)), (__fmap!(result, mfinal_q)))),
                    ),
                })
            }
        }
        (demand, CAssign(op, lhs, rhs, _)) => {
            /*do*/
            {
                let lhs_q = interpretExpr(true, lhs);

                let rhs_q = interpretExpr(true, rhs);

                compound(expr, false, demand, op, lhs_q, rhs_q)
            }
        }
        (demand, CCond(c, Some(t), f, _)) => {
            /*do*/
            {
                let c_q = __fmap!(toBool, (interpretExpr(true, c)));

                let t_q = interpretExpr(demand, t);

                let f_q = interpretExpr(demand, f);

                let mkIf = |c_q, t_q, f_q| {
                    Rust::IfThenElse(
                        c_q,
                        (Rust::Block(vec![], (Some(t_q)))),
                        (Rust::Block(vec![], (Some(f_q)))),
                    )
                };

                if demand {
                    promotePtr(expr, (mkIf(c_q)), t_q, f_q)
                } else {
                    __return(Result {
                        resultType: IsVoid,
                        resultMutable: Rust::Immutable,
                        result: mkIf(c_q, (result(t_q)), (result(f_q))),
                    })
                }
            }
        }
        (_, expr, __OP__, CBinary(op, lhs, rhs, _)) => {
            /*do*/
            {
                let lhs_q = interpretExpr(true, lhs);

                let rhs_q = interpretExpr(true, rhs);

                binop(expr, op, lhs_q, rhs_q)
            }
        }
        (_, CCast(decl, expr, _)) => {
            /*do*/
            {
                let (_mut, ty) = typeName(decl);

                let expr_q = interpretExpr((__op_assign_div(ty, IsVoid)), expr);

                __return(Result {
                    resultType: ty,
                    resultMutable: Rust::Immutable,
                    result: (if (ty == IsVoid) { result } else { castTo(ty) })(expr_q),
                })
            }
        }
        (demand, CUnary(op, expr, _)) => {
            let node = expr;
            let incdec = |returnOld, assignop| {
                /*do*/
                {
                    let expr_q = interpretExpr(true, expr);

                    compound(
                        node,
                        returnOld,
                        demand,
                        assignop,
                        expr_q,
                        Result {
                            resultType: IsInt(Signed, (BitWidth(32))),
                            resultMutable: Rust::Immutable,
                            result: 1,
                        },
                    )
                }
            };

            let simple = |f| {
                /*do*/
                {
                    let expr_q = interpretExpr(true, expr);

                    let ty_q = intPromote((resultType(expr_q)));

                    __return(Result {
                        resultType: ty_q,
                        resultMutable: Rust::Immutable,
                        result: f((castTo(ty_q, expr_q))),
                    })
                }
            };

            match op {
                CPreIncOp => incdec(false, CAddAssOp),
                CPreDecOp => incdec(false, CSubAssOp),
                CPostIncOp => incdec(true, CAddAssOp),
                CPostDecOp => incdec(true, CSubAssOp),
                CAdrOp => {
                    /*do*/
                    {
                        let expr_q = interpretExpr(true, expr);

                        let ty_q = IsPtr((resultMutable(expr_q)), (resultType(expr_q)));

                        __return(Result {
                            resultType: ty_q,
                            resultMutable: Rust::Immutable,
                            result: Rust::Cast(
                                (Rust::Borrow((resultMutable(expr_q)), (result(expr_q)))),
                                (toRustType(ty_q)),
                            ),
                        })
                    }
                }
                CIndOp => {
                    /*do*/
                    {
                        let expr_q = interpretExpr(true, expr);

                        match resultType(expr_q) {
                            IsPtr(mut_q, ty_q) => {
                                __return(Result {
                                    resultType: ty_q,
                                    resultMutable: mut_q,
                                    result: Rust::Deref((result(expr_q))),
                                })
                            }
                            IsFunc {} => __return(expr_q),
                            _ => badSource(node, "dereference of non-pointer".to_string()),
                        }
                    }
                }
                CPlusOp => {
                    /*do*/
                    {
                        let expr_q = interpretExpr(demand, expr);

                        let ty_q = intPromote((resultType(expr_q)));

                        __return(Result {
                            resultType: ty_q,
                            resultMutable: Rust::Immutable,
                            result: castTo(ty_q, expr_q),
                        })
                    }
                }
                CMinOp => __fmap!(wrapping, simple(Rust::Neg)),
                CCompOp => simple(Rust::Not),
                CNegOp => {
                    /*do*/
                    {
                        let expr_q = interpretExpr(true, expr);

                        __return(Result {
                            resultType: IsBool,
                            resultMutable: Rust::Immutable,
                            result: toNotBool(expr_q),
                        })
                    }
                }
            }
        }
        (_, CSizeofExpr(e, _)) => {
            /*do*/
            {
                let e_q = interpretExpr(true, e);

                __return((rustSizeOfType((toRustType((resultType(e_q)))))))
            }
        }
        (_, CSizeofType(decl, _)) => {
            /*do*/
            {
                let (_mut, ty) = typeName(decl);

                __return((rustSizeOfType((toRustType(ty)))))
            }
        }
        (_, CAlignofExpr(e, _)) => {
            /*do*/
            {
                let e_q = interpretExpr(true, e);

                __return((rustAlignOfType((toRustType((resultType(e_q)))))))
            }
        }
        (_, CAlignofType(decl, _)) => {
            /*do*/
            {
                let (_mut, ty) = typeName(decl);

                __return((rustAlignOfType((toRustType(ty)))))
            }
        }
        (_, CIndex(lhs, rhs, _)) => {
            /*do*/
            {
                let lhs_q = interpretExpr(true, lhs);

                let rhs_q = interpretExpr(true, rhs);

                match (resultType(lhs_q), resultType(rhs_q)) {
                    (IsArray(__mut, _, el), _) => {
                        __return((subscript(__mut, el, (result(lhs_q)), rhs_q)))
                    }
                    (_, IsArray(__mut, _, el)) => {
                        __return((subscript(__mut, el, (result(rhs_q)), lhs_q)))
                    }
                    _ => {
                        /*do*/
                        {
                            let ptr = binop(expr, CAddOp, lhs_q, rhs_q);

                            match resultType(ptr) {
                                IsPtr(__mut, ty) => {
                                    __return(Result {
                                        resultType: ty,
                                        resultMutable: __mut,
                                        result: Rust::Deref((result(ptr))),
                                    })
                                }
                                _ => badSource(expr, "array subscript of non-pointer".to_string()),
                            }
                        }
                    }
                }
            }
        }
        (_, CCall(func, args, _)) => {
            /*do*/
            {
                let func_q = interpretExpr(true, func);

                match resultType(func_q) {
                    IsFunc(retTy, argTys, variadic) => {
                        // TODO argumnt types are wrong hre
                        fn castArgs(expr: CExpr, _0: bool, _1: Vec<()>, _2: Vec<CExpr>) -> () {
                            match (_0, _1, _2) {
                                (_, [], []) => __return(vec![]),
                                (variadic, [ty, tys], [arg, rest]) => {
                                    /*do*/
                                    {
                                        let arg_q = interpretExpr(true, arg);

                                        let args_q = castArgs(expr, variadic, tys, rest);

                                        __return((__op_concat(castTo(ty, arg_q), args_q)))
                                    }
                                }
                                (true, [], rest) => {
                                    __mapM!((__fmap!(promoteArg, interpretExpr(true))), rest)
                                }
                                (false, [], _) => {
                                    badSource(expr, "arguments (too many)".to_string())
                                }
                                (_, _, []) => badSource(expr, "arguments (too few)".to_string()),
                            }
                        };

                        pub fn promoteArg<r>(r: Result) -> Rust::Expr {
                            match resultType(r) {
                                IsFloat(_) => castTo((IsFloat(64)), r),
                                IsArray(__mut, _, el) => castTo((IsPtr(__mut, el)), r),
                                ty => castTo((intPromote(ty)), r),
                            }
                        }

                        /*do*/
                        {
                            let args_q = castArgs(expr, variadic, (__map!(snd, argTys)), args);

                            __return(Result {
                                resultType: retTy,
                                resultMutable: Rust::Immutable,
                                result: Rust::Call((result(func_q)), args_q),
                            })
                        }
                    }
                    _ => badSource(expr, "function call to non-function".to_string()),
                }
            }
        }
        (_, CMember(obj, ident, deref, node)) => {
            /*do*/
            {
                let obj_q = interpretExpr(
                    true,
                    if deref {
                        CUnary(CIndOp, obj, node)
                    } else {
                        obj
                    },
                );

                let objTy = completeType((resultType(obj_q)));

                let fields = match objTy {
                    IsStruct(_, fields) => __return(fields),
                    _ => badSource(expr, "member access of non-struct".to_string()),
                };

                let field = applyRenames(ident);

                let ty = match lookup(field, fields) {
                    Some(ty) => __return(ty),
                    None => badSource(expr, "request for non-existent field".to_string()),
                };

                __return(Result {
                    resultType: ty,
                    resultMutable: resultMutable(obj_q),
                    result: Rust::Member((result(obj_q)), (Rust::VarName(field))),
                })
            }
        }
        (_, CVar(ident, _)) => {
            /*do*/
            {
                let sym = getSymbolIdent(ident);

                maybe(
                    (badSource(expr, "undefined variable".to_string())),
                    __return,
                    sym,
                )
            }
        }
        (_, CConst(c)) => {
            match c {
                CIntConst(CInteger(v, repr, flags), _) => {
                    {
                        let allow_signed = not((testFlag(FlagUnsigned, flags)));

                        let allow_unsigned = (not(allow_signed) || __op_assign_div(repr, DecRepr));

                        let widths = vec![
                                (32, if any((testFlag(flags)), vec![FlagLongLong, FlagLong]) {                             
WordWidth} else {
BitWidth(32)
                            }),
                                (64, BitWidth(64)),
                            ];

                        let allowed_types = /*do*/ {
                            let (bits, w) = widths;

                            let (true, s) = vec![(allow_signed, Signed), (allow_unsigned, Unsigned)];

                            (v < __op_power(2, ((bits - if (s == Signed) {
                    1} else {
                    0
                            }))));
                            __return(IsInt(s, w))
                        };

                        let repr_q = match repr {
                            DecRepr => Rust::DecRepr,
                            OctalRepr => Rust::OctalRepr,
                            HexRepr => Rust::HexRepr,
                        };

                        match allowed_types {
                            [] => badSource(expr, "integer (too big)".to_string()),
                            [ty, _] => __return((literalNumber(ty, (Rust::LitInt(v, repr_q))))),
                        }
                    }
                }
                CFloatConst(CFloat(__str), _) => {
                    match span((notElem("fF".to_string())), __str) {
                        (v, "") => __return((literalNumber((IsFloat(64)), (Rust::LitFloat(v))))),
                        (v, [_]) => __return((literalNumber((IsFloat(32)), (Rust::LitFloat(v))))),
                        _ => badSource(expr, "float".to_string()),
                    }
                }
                CCharConst(CChar(ch, false), _) => {
                    __return(Result {
                        resultType: charType,
                        resultMutable: Rust::Immutable,
                        result: Rust::Lit((Rust::LitByteChar(ch))),
                    })
                }
                CStrConst(CString(__str, false), _) => {
                    __return(Result {
                        resultType: IsArray(Rust::Immutable, ((length(__str) + 1)), charType),
                        resultMutable: Rust::Immutable,
                        result: Rust::Deref(
                            (Rust::Lit(
                                (Rust::LitByteStr((__op_addadd(__str, "\u{0}".to_string())))),
                            )),
                        ),
                    })
                }
                _ => unimplemented(expr),
            }
        }
        (_, CCompoundLit(decl, initials, info)) => {
            /*do*/
            {
                let (__mut, ty) = typeName(decl);

                let __final = interpretInitializer(ty, (CInitList(initials, info)));

                __return(Result {
                    resultType: ty,
                    resultMutable: __mut,
                    result: __final,
                })
            }
        }
        (demand, CStatExpr(CCompound([], stmts, _), _)) => {
            let stat = expr;
            scope(
                /*do*/
                {
                    let (effects, __final) = match last(stmts) {
                        CBlockStmt(CExpr(expr, _)) if demand => (init(stmts), expr),
                        _ => (stmts, None),
                    };

                    let effects_q = cfgToRust(
                        stat,
                        (__foldr!(interpretBlockItem, (__return((vec![], Unreachable))), effects)),
                    );

                    let final_q = __mapM!((interpretExpr(true)), __final);

                    __return(Result {
                        resultType: maybe(IsVoid, resultType, final_q),
                        resultMutable: maybe(Rust::Immutable, resultMutable, final_q),
                        result: Rust::BlockExpr(
                            (Rust::Block(effects_q, (__fmap!(result, final_q)))),
                        ),
                    })
                },
            )
        }
        (_, expr) => unimplemented(expr),
    }
}

pub fn wrapping(r: Result) -> Result {
    match r {
        Result {
            resultType: IsInt(Unsigned, _),
            ..
        } => {
            match result(r) {
                Rust::Add(lhs, rhs) => {
                    __assign!(r, {
                        result: Rust::MethodCall(lhs, (Rust::VarName("wrapping_add".to_string())), vec![rhs])
                    })
                }
                Rust::Sub(lhs, rhs) => {
                    __assign!(r, {
                        result: Rust::MethodCall(lhs, (Rust::VarName("wrapping_sub".to_string())), vec![rhs])
                    })
                }
                Rust::Mul(lhs, rhs) => {
                    __assign!(r, {
                        result: Rust::MethodCall(lhs, (Rust::VarName("wrapping_mul".to_string())), vec![rhs])
                    })
                }
                Rust::Div(lhs, rhs) => {
                    __assign!(r, {
                        result: Rust::MethodCall(lhs, (Rust::VarName("wrapping_div".to_string())), vec![rhs])
                    })
                }
                Rust::Mod(lhs, rhs) => {
                    __assign!(r, {
                        result: Rust::MethodCall(lhs, (Rust::VarName("wrapping_rem".to_string())), vec![rhs])
                    })
                }
                Rust::Neg(e) => {
                    __assign!(r, {
                        result: Rust::MethodCall(e, (Rust::VarName("wrapping_neg".to_string())), vec![])
                    })
                }
                _ => r,
            }
        }
        r => r,
    }
}

pub fn toPtr(ptr: Result) -> Option<Result> {
    match ptr {
        Result {
            resultType: IsArray(__mut, _, el),
            ..
        } => {
            Some(__assign!(ptr, {
                    resultType: IsPtr(__mut, el),
                    result: castTo((IsPtr(__mut, el)), ptr)
                }))
        }
        Result {
            resultType: IsPtr(..),
            ..
        } => Some(ptr),
        _ => None,
    }
}

pub fn binop<s>(expr: CExpr, op: CBinaryOp, lhs: Result, rhs: Result) -> EnvMonad<s, Result> {

    let shift = |op_q| {
        let lhsTy = intPromote(resultType(lhs));
        let rhsTy = intPromote(resultType(rhs));

        __return(Result {
            resultType: lhsTy,
            resultMutable: Rust::Immutable,
            result: op_q((castTo(lhsTy, lhs)), (castTo(rhsTy, rhs))),
        })
    };

    let comparison = |op_q| {
        /*do*/
        {
            let res = promotePtr(expr, op_q, lhs, rhs);

            __return(__assign!(res, {
                    resultType: IsBool
                }))
        }
    };

    __fmap!(wrapping, match op {
            CMulOp => {
                promote(expr, Rust::Mul, lhs, rhs)
            },
            CDivOp => {
                promote(expr, Rust::Div, lhs, rhs)
            },
            CRmdOp => {
                promote(expr, Rust::Mod, lhs, rhs)
            },
            CAddOp => {
                match (toPtr(lhs), toPtr(rhs)) {
                    (Some(ptr), _) => {
                        __return((offset(ptr, rhs)))
                    },
                    (_, Some(ptr)) => {
                        __return((offset(ptr, lhs)))
                    },
                    _ => {
                        promote(expr, Rust::Add, lhs, rhs)
                    },
                }
            },
            CSubOp => {
                match (toPtr(lhs), toPtr(rhs)) {
                    (Some(lhs_q), Some(rhs_q)) => {
                        /*do*/ {
                            let ptrTo = match compatiblePtr((resultType(lhs_q)), (resultType(rhs_q))) {
                                    IsPtr(_, ptrTo) => {
                                        __return(ptrTo)
                                    },
                                    _ => {
                                        badSource(expr, "pointer subtraction of incompatible pointers".to_string())
                                    },
                                };

                            let ty = IsInt(Signed, WordWidth);

                            let size = rustSizeOfType((toRustType(ptrTo)));

                            __return(Result {
                                    resultType: ty,
                                    resultMutable: Rust::Immutable,
                                    result: __op_div((Rust::MethodCall((castTo(ty, lhs_q)), (Rust::VarName("wrapping_sub".to_string())), vec![castTo(ty, rhs_q)])), castTo(ty, size))
                                })
                        }
                    },
                    (Some(ptr), _) => {
                        __return(__assign!(ptr, {
                                result: Rust::MethodCall((result(ptr)), (Rust::VarName("offset".to_string())), vec![Rust::Neg((castTo((IsInt(Signed, WordWidth)), rhs)))])
                            }))
                    },
                    _ => {
                        promote(expr, Rust::Sub, lhs, rhs)
                    },
                }
            },
            CShlOp => {
                shift(Rust::ShiftL)
            },
            CShrOp => {
                shift(Rust::ShiftR)
            },
            CLeOp => {
                comparison(Rust::CmpLT)
            },
            CGrOp => {
                comparison(Rust::CmpGT)
            },
            CLeqOp => {
                comparison(Rust::CmpLE)
            },
            CGeqOp => {
                comparison(Rust::CmpGE)
            },
            CEqOp => {
                comparison(Rust::CmpEQ)
            },
            CNeqOp => {
                comparison(Rust::CmpNE)
            },
            CAndOp => {
                promote(expr, Rust::And, lhs, rhs)
            },
            CXorOp => {
                promote(expr, Rust::Xor, lhs, rhs)
            },
            COrOp => {
                promote(expr, Rust::Or, lhs, rhs)
            },
            CLndOp => {
                __return(Result {
                        resultType: IsBool,
                        resultMutable: Rust::Immutable,
                        result: Rust::LAnd((toBool(lhs)), (toBool(rhs)))
                    })
            },
            CLorOp => {
                __return(Result {
                        resultType: IsBool,
                        resultMutable: Rust::Immutable,
                        result: Rust::LOr((toBool(lhs)), (toBool(rhs)))
                    })
            },
        })
}

pub fn compound<s>(
    expr: CExpr,
    returnOld: bool,
    demand: bool,
    op: CAssignOp,
    lhs: Result,
    rhs: Result,
) -> EnvMonad<s, Result> {

    fn hasNoSideEffects(_0: Rust::Expr) -> bool {
        match (_0) {
            Rust::Var {} => true,
            Rust::Path {} => true,
            Rust::Member(e, _) => hasNoSideEffects(e),
            Rust::Deref(p) => hasNoSideEffects(p),
            _ => false,
        }
    }

    /*do*/
    {
        let op_q = match op {
            CAssignOp => None,
            CMulAssOp => Some(CMulOp),
            CDivAssOp => Some(CDivOp),
            CRmdAssOp => Some(CRmdOp),
            CAddAssOp => Some(CAddOp),
            CSubAssOp => Some(CSubOp),
            CShlAssOp => Some(CShlOp),
            CShrAssOp => Some(CShrOp),
            CAndAssOp => Some(CAndOp),
            CXorAssOp => Some(CXorOp),
            COrAssOp => Some(COrOp),
        };

        let duplicateLHS = (isJust(op_q) || demand);

        let (bindings1, dereflhs, boundrhs) =
            if (not(duplicateLHS) || hasNoSideEffects((result(lhs)))) {
                (vec![], lhs, rhs)
            } else {
                {
                    let lhsvar = Rust::VarName("_lhs".to_string());

                    let rhsvar = Rust::VarName("_rhs".to_string());

                    (
                        vec![
                    Rust::Let(Rust::Immutable, rhsvar, None, (Some((result(rhs))))),
                    Rust::Let(Rust::Immutable, lhsvar, None, (Some((Rust::Borrow(Rust::Mutable, (result(lhs))))))),
                ],
                        __assign!(lhs, {
                    result: Rust::Deref((Rust::Var(lhsvar)))
                }),
                        __assign!(rhs, {
                    result: Rust::Var(rhsvar)
                }),
                    )
                }
            };

        let rhs_q = match op_q {
            Some(o) => binop(expr, o, dereflhs, boundrhs),
            None => __return(boundrhs),
        };

        let assignment = Rust::Assign(
            (result(dereflhs)),
            Rust::__id_3a3d,
            (castTo((resultType(lhs)), rhs_q)),
        );

        let (bindings2, ret) = if not(demand) {
            (vec![], None)
        } else {
            if not(returnOld) {
                (vec![], Some((result(dereflhs))))
            } else {
                {
                    let oldvar = Rust::VarName("_old".to_string());

                    (
                        vec![Rust::Let(Rust::Immutable, oldvar, None, (Some((result(dereflhs)))))],
                        Some((Rust::Var(oldvar))),
                    )
                }
            }
        };

        __return({
            let b = Rust::Block(
                (__op_addadd(
                    bindings1,
                    __op_addadd(bindings2, exprToStatements(assignment)),
                )),
                ret,
            );
            match b {
                Rust::Block(body, None) => {
                    Result {
                        resultType: IsVoid,
                        resultMutable: Rust::Immutable,
                        result: match body {
                            [Rust::Stmt(e)] => e,
                            _ => Rust::BlockExpr(b),
                        },
                    }
                }
                b => {
                    __assign!(lhs, {
                        result: Rust::BlockExpr(b)
                    })
                }
            }
        })
    }
}

pub fn rustSizeOfType(Rust::TypeName(ty): Rust::TypeName) -> Result {
    Result {
        resultType: IsInt(Unsigned, WordWidth),
        resultMutable: Rust::Immutable,
        result: Rust::Call(
            (Rust::Var(
                (Rust::VarName(
                    (__op_addadd(
                        "::std::mem::size_of::<".to_string(),
                        __op_addadd(ty, ">".to_string()),
                    )),
                )),
            )),
            vec![],
        ),
    }
}

pub fn rustAlignOfType(Rust::TypeName(ty): Rust::TypeName) -> Result {
    Result {
        resultType: IsInt(Unsigned, WordWidth),
        resultMutable: Rust::Immutable,
        result: Rust::Call(
            (Rust::Var(
                (Rust::VarName(
                    (__op_addadd(
                        "::std::mem::align_of::<".to_string(),
                        __op_addadd(ty, ">".to_string()),
                    )),
                )),
            )),
            vec![],
        ),
    }
}

pub fn interpretConstExpr<s>(_0: CExpr) -> EnvMonad<s, isize> {
    match (_0) {
        CConst(CIntConst(CInteger(v, _, _), _)) => __return(v),
        expr => unimplemented(expr),
    }
}

pub fn castTo(target: CType, source: Result) -> Rust::Expr {
    match (target, source) {
        (target, source) if (resultType(source) == target) => result(source),
        (
            target,
            Result {
                resultType: IsArray(__mut, _, el),
                result: source,
            },
        ) => {

            let method = match __mut {
                Rust::Immutable => "as_ptr".to_string(),
                Rust::Mutable => "as_mut_ptr".to_string(),
            };

            castTo(
                target,
                Result {
                    resultType: IsPtr(__mut, el),
                    resultMutable: Rust::Immutable,
                    result: Rust::MethodCall(source, (Rust::VarName(method)), vec![]),
                },
            )
        }
        (IsBool, source) => toBool(source),
        (
            IsInt(..),
            Result {
                result: Rust::Lit(Rust::LitInt(n, repr, _)),
                ..
            },
        ) => Rust::Lit((Rust::LitInt(n, repr, (toRustType(target))))),
        (
            IsInt(Signed, w),
            Result {
                result: Rust::Neg(Rust::Lit(Rust::LitInt(n, repr, _))),
                ..
            },
        ) => {
            Rust::Neg(
                (Rust::Lit((Rust::LitInt(n, repr, (toRustType((IsInt(Signed, w)))))))),
            )
        }
        (target, source) => Rust::Cast((result(source)), (toRustType(target))),
    }
}

pub fn toBool(_0: Result) -> Rust::Expr {
    match (_0) {
        Result {
            result: Rust::Lit(Rust::LitInt(0, _, _)),
            ..
        } => Rust::Lit((Rust::LitBool(false))),
        Result {
            result: Rust::Lit(Rust::LitInt(1, _, _)),
            ..
        } => Rust::Lit((Rust::LitBool(true))),
        Result {
            resultType: t,
            result: v,
            ..
        } => {
            match t {
                IsBool => v,
                IsPtr(_, _) => {
                    Rust::Not(
                        (Rust::MethodCall(v, (Rust::VarName("is_null".to_string())), vec![])),
                    )
                }
                _ => Rust::CmpNE(v, 0),
            }
        }
    }
}

pub fn toNotBool(_0: Result) -> Rust::Expr {
    match (_0) {
        Result {
            result: Rust::Lit(Rust::LitInt(0, _, _)),
            ..
        } => Rust::Lit((Rust::LitBool(true))),
        Result {
            result: Rust::Lit(Rust::LitInt(1, _, _)),
            ..
        } => Rust::Lit((Rust::LitBool(false))),
        Result {
            resultType: t,
            result: v,
        } => {
            match t {
                IsBool => Rust::Not(v),
                IsPtr(_, _) => Rust::MethodCall(v, (Rust::VarName("is_null".to_string())), vec![]),
                _ => Rust::CmpEQ(v, 0),
            }
        }
    }
}

pub fn intPromote(_0: CType) -> CType {
    match (_0) {
        IsBool => IsInt(Signed, (BitWidth(32))),
        IsEnum(_) => enumReprType,
        IsInt(_, BitWidth(w)) if w < 32 => {
            IsInt(Signed, BitWidth(32))
        }
        x => x,
    }
}

pub fn usual(_0: CType, _1: CType) -> Option<CType> {
    match (_0, _1) {
        (IsFloat(aw), IsFloat(bw)) => Some((IsFloat((max(aw, bw))))),
        (a, __OP__, IsFloat(_), _) => Some(a),
        (_, b, __OP__, IsFloat(_)) => Some(b),
        (origA, origB) => {
            let mixedSign = |sw, uw| {
                /*do*/
                {
                    let rank = integerConversionRank(uw, sw);

                    Some(match rank {
                        GT => IsInt(Unsigned, uw),
                        EQ => IsInt(Unsigned, uw),
                        _ if (bitWidth(64, uw) < bitWidth(32, sw)) => IsInt(Signed, sw),
                        _ => IsInt(Unsigned, sw),
                    })
                }
            };

            match (intPromote(origA), intPromote(origB)) {
                (a, b) if (a == b) => Some(a),
                (IsInt(Signed, sw), IsInt(Unsigned, uw)) => mixedSign(sw, uw),
                (IsInt(Unsigned, uw), IsInt(Signed, sw)) => mixedSign(sw, uw),
                (IsInt(__as, aw), IsInt(_bs, bw)) => {
                    /*do*/
                    {
                        let rank = integerConversionRank(aw, bw);

                        Some((IsInt(__as, (if (rank == GT) { aw } else { bw }))))
                    }
                }
                _ => None,
            }
        }
    }
}

pub fn integerConversionRank(_0: IntWidth, _1: IntWidth) -> Option<Ordering> {
    match (_0, _1) {
        (BitWidth(a), BitWidth(b)) => Some((compare(a, b))),
        (WordWidth, WordWidth) => Some(EQ),
        (BitWidth(a), WordWidth) if a <= 32 => Some(LT),
        (BitWidth(a), WordWidth) if a >= 64 => Some(GT),
        (WordWidth, BitWidth(b)) if b <= 32 => Some(GT),
        (WordWidth, BitWidth(b)) if b >= 64 => Some(LT),
        (_, _) => None,
    }
}

pub fn promote<a, b, s, node: Pretty + Pos>(
    node: node,
    op: Box<Fn(Rust::Expr, Rust::Expr) -> Rust::Expr>,
    a: Result,
    b: Result,
) -> EnvMonad<s, Result> {
    match usual((resultType(a)), (resultType(b))) {
        Some(rt) => {
            __return(Result {
                resultType: rt,
                resultMutable: Rust::Immutable,
                result: op((castTo(rt, a)), (castTo(rt, b))),
            })
        }
        None => {
            badSource(
                node,
                concat(vec![
                        "arithmetic combination for ".to_string(),
                        show((resultType(a))),
                        " and ".to_string(),
                        show((resultType(b))),
                    ]),
            )
        }
    }
}

pub fn compatiblePtr(_0: CType, _1: CType) -> CType {
    match (_0, _1) {
        (IsPtr(_, IsVoid), b) => b,
        (IsArray(__mut, _, el), b) => compatiblePtr((IsPtr(__mut, el)), b),
        (a, IsPtr(_, IsVoid)) => a,
        (a, IsArray(__mut, _, el)) => compatiblePtr(a, (IsPtr(__mut, el))),
        (IsPtr(m1, a), IsPtr(m2, b)) => {
            fn leastMutable((a, b): (Rust::Mutable, Rust::Mutable)) {
                match (a, b) {
                    (Rust::Mutable::Mutable, Rust::Mutable::Mutable) => Rust::Mutable::Mutable,
                    (_, _) => Rust::Mutable::Immutable,
                }
            }

            IsPtr((leastMutable(m1, m2)), (compatiblePtr(a, b)))
        }
        (a, b) if a == b => {
            a
        }
        (_, _) => IsVoid,
    }
}

pub fn promotePtr<a, b, s, node: Pretty + Pos>(
    node: node,
    op: Box<Fn(Rust::Expr, Rust::Expr) -> Rust::Expr>,
    a: Result,
    b: Result,
) -> EnvMonad<s, Result> {

    let ptrOrVoid = |r| {
        let t = resultType(r);
        match t {
            IsArray(_, _, _) => t,
            IsPtr(_, _) => t,
            _ => IsPtr(Rust::Mutable, IsVoid),
        }
    };

    let ty = compatiblePtr((ptrOrVoid(a)), (ptrOrVoid(b)));

    let ptrs = __return(Result {
        resultType: ty,
        resultMutable: Rust::Immutable,
        result: op((castTo(ty, a)), (castTo(ty, b))),
    });

    match (resultType(a), resultType(b)) {
        (IsArray(_, _, _), _) => ptrs,
        (IsPtr(_, _), _) => ptrs,
        (_, IsArray(_, _, _)) => ptrs,
        (_, IsPtr(_, _)) => ptrs,
        _ => promote(node, op, a, b),
    }
}

#[derive(Debug, Eq)]
pub enum Signed {
    Signed,
    Unsigned,
}
pub use self::Signed::*;

#[derive(Debug, Eq)]
pub enum IntWidth {
    BitWidth(isize),
    WordWidth,
}
pub use self::IntWidth::*;

pub fn bitWidth(_0: isize, _1: IntWidth) -> isize {
    match (_0, _1) {
        (wordWidth, WordWidth) => wordWidth,
        (_, BitWidth(w)) => w,
    }
}

#[derive(Debug)]
pub enum CType {
    IsBool,
    IsInt(Signed, IntWidth),
    IsFloat(isize),
    IsVoid,
    IsFunc(CType, Vec<(Option<(Rust::Mutable, Ident)>, CType)>, bool),
    IsPtr(Rust::Mutable, CType),
    IsArray(Rust::Mutable, isize, CType),
    IsStruct(String, Vec<(String, CType)>),
    IsEnum(String),
    IsIncomplete(Ident),
}
pub use self::CType::*;

pub fn toRustType(_0: CType) -> Rust::TypeName {
    match (_0) {
        IsBool => Rust::TypeName("bool".to_string()),
        IsInt(s, w) => {
            Rust::TypeName(
                (__op_concat(
                    (match s {
                         Signed => 'i',
                         Unsigned => 'u',
                     }),
                    (match w {
                         BitWidth(b) => show(b),
                         WordWidth => "size".to_string(),
                     }),
                )),
            )
        }
        IsFloat(w) => Rust::TypeName((__op_concat('f', show(w)))),
        IsVoid => Rust::TypeName("::std::os::raw::c_void".to_string()),
        IsFunc(retTy, args, variadic) => {
            let typename = |view| {
                match toRustType(view) {
                    Rust::TypeName(t) => t,
                    _ => unreachable!(),
                }
            };

            let args_q = intercalate(
                ", ".to_string(),
                (__op_addadd(
                    __map!((typename(snd)), args),
                    if variadic {
                        vec!["...".to_string()]
                    } else {
                        vec![]
                    },
                )),
            );

            Rust::TypeName(concat(vec![
                    "unsafe extern fn(".to_string(),
                    args_q,
                    ")".to_string(),
                    if __op_assign_div(retTy, IsVoid) {                     
                        __op_addadd(" -> ".to_string(), typename(retTy))
                    } else {
                        "".to_string()
                },
            ]))
        }
        IsPtr(__mut, to) => {
            let rustMut = |_0| match (_0) {
                Rust::Mutable => "*mut ".to_string(),
                Rust::Immutable => "*const ".to_string(),
            };

            let to_q: Rust::TypeName = toRustType(to);

            Rust::TypeName((__op_addadd(rustMut(__mut), to_q)))
        }
        IsArray(_, size, el) => {
            let typename = |view| {
                match toRustType(view) {
                    Rust::TypeName(t) => t,
                    _ => unreachable!(),
                }
            };

            Rust::TypeName(
                (__op_addadd(
                    "[".to_string(),
                    __op_addadd(
                        typename(el),
                        __op_addadd("; ".to_string(), __op_addadd(show(size), "]".to_string())),
                    ),
                )),
            )
        }
        IsStruct(name, _fields) => Rust::TypeName(name),
        IsEnum(name) => Rust::TypeName(name),
        IsIncomplete(ident) => Rust::TypeName((identToString(ident))),
    }
}

pub fn toRustRetType(_0: CType) -> Rust::TypeName {
    match (_0) {
        IsVoid => Rust::TypeName("()".to_string()),
        ty => toRustType(ty),
    }
}

pub fn charType() -> CType {
    IsInt(Unsigned, (BitWidth(8)))
}

pub fn enumReprType() -> CType {
    IsInt(Signed, (BitWidth(32)))
}

pub struct IntermediateType {
    typeMutable: Rust::Mutable,
    typeIsFunc: bool,
    typeRep: CType,
}
fn typeMutable(a: IntermediateType) -> Rust::Mutable {
    a.typeMutable
}
fn typeIsFunc(a: IntermediateType) -> bool {
    a.typeIsFunc
}
fn typeRep(a: IntermediateType) -> CType {
    a.typeRep
}

pub fn runOnce<a, s>(action: EnvMonad<s, a>) -> EnvMonad<s, EnvMonad<s, a>> {
    /*do*/
    {
        let cacheRef = lift(lift(newSTRef((Left(action)))));

        __return(
            /*do*/
            {
                let cache = lift(lift(readSTRef(cacheRef)));

                match cache {
                    Left(todo) => {
                        /*do*/
                        {
                            lift(lift(writeSTRef(cacheRef, Left(fail("internal error: runOnce action depends on itself, leading to an infinite loop".to_string())))));
                            let val = todo;

                            lift(lift(writeSTRef(cacheRef, (Right(val)))));
                            __return(val)
                        }
                    }
                    Right(val) => __return(val),
                }
            },
        )
    }
}

pub fn baseTypeOf<s>(
    specs: Vec<CDeclSpec>,
) -> EnvMonad<s, (Option<CStorageSpec>, EnvMonad<s, IntermediateType>)> {

    let typedef = |_0, _1| {
        match (_0, _1) {
            (__mut, [CTypeDef(ident, _)]) => {
                /*do*/
                {
                    let spec = _1.clone();

                    let (name, mty) = getTypedefIdent(ident);

                    match mty {
                        Some(deferred) if (__mut == Rust::Immutable) => {
                            __return(
                                (__fmap!((box |itype| { __assign!(itype, {
                                        typeMutable: Rust::Immutable
                                    }) }), deferred)),
                            )
                        }
                        Some(deferred) => __return(deferred),
                        None if (name == "__builtin_va_list".to_string()) => {
                            runOnce(
                                /*do*/
                                {
                                    let ty = emitIncomplete(Type, ident);

                                    __return(IntermediateType {
                                        typeMutable: __mut,
                                        typeIsFunc: false,
                                        typeRep: IsPtr(Rust::Mutable, ty),
                                    })
                                },
                            )
                        }
                        None => badSource(spec, "undefined type".to_string()),
                    }
                }
            }
            (__mut, other) => {
                /*do*/
                {
                    let deferred = singleSpec(other);

                    __return((__fmap!((simple(__mut)), deferred)))
                }
            }
        }
    };

    fn simple(__mut: Rust::Mutable, ty: CType) -> IntermediateType {
        IntermediateType {
            typeMutable: __mut,
            typeIsFunc: false,
            typeRep: ty,
        }
    }

    // TODO wrong rturn type
    fn singleSpec<T>(_0: Vec<CTypeSpecifier<T>>) -> () {
        match (_0) {
            [CVoidType(_)] => __return((__return(IsVoid))),
            [CBoolType(_)] => __return((__return(IsBool))),
            [CSUType(CStructureUnion(CStructTag, Some(ident), None, _, _), _)] => {
                /*do*/
                {
                    let mty = getTagIdent(ident);

                    __return(fromMaybe((emitIncomplete(Struct, ident)), mty))
                }
            }
            [CSUType(CStructureUnion(CStructTag, mident, Some(declarations), _, _), _)] => {
                /*do*/
                {
                    let deferredFields = __fmap!(concat, __forM!(declarations, box |declaration| { match declaration {
                                        CStaticAssert {

                                        } => {
                                            __return(vec![])
                                        },
                                        CDecl(spec, decls, _) => {
                                            /*do*/ {
                                                let (storage, base) = baseTypeOf(spec);

                                                match storage {
                                                    Some(s) => {
                                                        badSource(s, "storage class specifier in struct".to_string())
                                                    },
                                                    None => {
                                                        __return(())
                                                    },
                                                };
                                                __forM!(decls, box |decl| { match decl {
                                                            (Some(declr, __OP__, CDeclarator(Some(field), _, _, _, _)), None, None) => {
                                                                /*do*/ {
                                                                    let deferred = derivedDeferredTypeOf(base, declr, vec![]);

                                                                    __return((applyRenames(field), deferred))
                                                                }
                                                            },
                                                            (_, None, Some(_size)) => {
                                                                /*do*/ {
                                                                    __return(("<bitfield>".to_string(), unimplemented(declaration)))
                                                                }
                                                            },
                                                            _ => {
                                                                badSource(declaration, "field in struct".to_string())
                                                            },
                                                        } })
                                            }
                                        },
                                    } }));

                    let deferred = runOnce(
                        /*do*/
                        {
                            let (shouldEmit, name) = match mident {
                                Some(ident) => {
                                    /*do*/
                                    {
                                        let rewrites = lift((asks(itemRewrites)));

                                        match Map::lookup(
                                            (Struct, identToString(ident)),
                                            rewrites,
                                        ) {
                                            Some(renamed) => {
                                                __return((
                                                    false,
                                                    __concatMap!(("::".to_string()(__op_addadd)), renamed),
                                                ))
                                            }
                                            None => __return((true, identToString(ident))),
                                        }
                                    }
                                }
                                None => {
                                    /*do*/
                                    {
                                        let name = uniqueName("Struct".to_string());

                                        __return((true, name))
                                    }
                                }
                            };

                            let fields = __forM!(deferredFields, box |(fieldName, deferred)| { /*do*/ {
                                                let itype = deferred;

                                                __return((fieldName, typeRep(itype)))
                                            } });

                            let attrs = vec![
                                        Rust::Attribute("derive(Copy)".to_string()),
                                        Rust::Attribute("repr(C)".to_string()),
                                    ];

                            if shouldEmit {
                                emitItems(vec![
                                        Rust::Item(attrs, Rust::Public, (Rust::Struct(name, /*do*/ {
                                            let (field, fieldTy) = fields;

                                            __return((field, toRustType(fieldTy)))
                                        }
                                        ))),
                                        Rust::Item(vec![], Rust::Private, (Rust::CloneImpl((Rust::TypeName(name))))),
                                    ])
                            };
                            __return((IsStruct(name, fields)))
                        },
                    );

                    match mident {
                        Some(ident) => addTagIdent(ident, deferred),
                        None => __return(()),
                    };
                    __return(deferred)
                }
            }
            [CSUType(CStructureUnion(CUnionTag, mident, _, _, _), node)] => {
                runOnce(
                    /*do*/
                    {
                        let ident = match mident {
                            Some(ident) => __return(ident),
                            None => {
                                /*do*/
                                {
                                    let name = uniqueName("Union".to_string());

                                    __return((internalIdentAt((node.posOf()), name)))
                                }
                            }
                        };

                        emitIncomplete(Union, ident)
                    },
                )
            }
            [CEnumType(CEnumeration(Some(ident), None, _, _), _)] => {
                /*do*/
                {
                    let spec = _0.clone();

                    let mty = getTagIdent(ident);

                    match mty {
                        Some(ty) => __return(ty),
                        None => badSource(spec, "undefined enum".to_string()),
                    }
                }
            }
            [CEnumType(CEnumeration(mident, Some(items), _, _), _)] => {
                /*do*/
                {
                    let deferred = runOnce(
                        /*do*/
                        {
                            let (shouldEmit, name) = match mident {
                                Some(ident) => {
                                    /*do*/
                                    {
                                        let rewrites = lift((asks(itemRewrites)));

                                        match Map::lookup((Enum, identToString(ident)), rewrites) {
                                            Some(renamed) => {
                                                __return((
                                                    false,
                                                    __concatMap!(("::".to_string()(__op_addadd)), renamed),
                                                ))
                                            }
                                            None => __return((true, identToString(ident))),
                                        }
                                    }
                                }
                                None => {
                                    /*do*/
                                    {
                                        let name = uniqueName("Enum".to_string());

                                        __return((true, name))
                                    }
                                }
                            };

                            let enums = __forM!(items, box |(ident, mexpr)| { /*do*/ {
                                                let enumName = applyRenames(ident);

                                                match mexpr {
                                                    None => {
                                                        __return((Rust::EnumeratorAuto(enumName)))
                                                    },
                                                    Some(expr) => {
                                                        /*do*/ {
                                                            let expr_q = interpretExpr(true, expr);

                                                            __return((Rust::EnumeratorExpr(enumName, (castTo(enumReprType, expr_q)))))
                                                        }
                                                    },
                                                }
                                            } });

                            let repr: Rust::TypeName = toRustType(enumReprType);

                            let attrs = vec![
                                        Rust::Attribute("derive(Clone, Copy)".to_string()),
                                        Rust::Attribute((concat(vec!["repr(".to_string(), repr, ")".to_string()]))),
                                    ];

                            if shouldEmit {
                                emitItems(
                                    vec![Rust::Item(attrs, Rust::Public, (Rust::Enum(name, enums)))],
                                )
                            };
                            __return((IsEnum(name)))
                        },
                    );

                    __forM!(items, box |(ident, _mexpr)| { addSymbolIdentAction });
                    match mident {
                        Some(ident) => addTagIdent(ident, deferred),
                        None => __return(()),
                    };
                    __return(deferred)
                }
            }
            other => {
                __return(
                    (__foldrM!(arithmetic, (IsInt(Signed, (BitWidth(32)))), other)),
                )
            }
        }
    };

    pub fn arithmetic<s>(_0: CTypeSpec, _1: CType) -> EnvMonad<s, CType> {
        match (_0, _1) {
            (CSignedType(_), IsInt(_, width)) => __return((IsInt(Signed, width))),
            (CUnsigType(_), IsInt(_, width)) => __return((IsInt(Unsigned, width))),
            (CCharType(_), _) => __return(charType),
            (CShortType(_), IsInt(s, _)) => __return((IsInt(s, (BitWidth(16))))),
            (CIntType(_), IsInt(s, _)) => __return((IsInt(s, (BitWidth(32))))),
            (CLongType(_), IsInt(s, _)) => __return((IsInt(s, WordWidth))),
            (CLongType(_), IsFloat(w)) => __return((IsFloat(w))),
            (CFloatType(_), _) => __return((IsFloat(32))),
            (CDoubleType(_), _) => __return((IsFloat(64))),
            (spec, _) => unimplemented(spec),
        }
    }

    /*do*/
    {
        let (storage, _attributes, basequals, basespecs, _inlineNoReturn, _align) =
            partitionDeclSpecs(specs);

        let mstorage = match storage {
            [] => __return(None),
            [spec] => __return((Some(spec))),
            [_, [excess, _]] => badSource(excess, "extra storage class specifier".to_string()),
        };

        let base = typedef((mutable(basequals)), basespecs);

        __return((mstorage, base))
    }
}

pub fn derivedTypeOf<s>(
    deferred: EnvMonad<s, IntermediateType>,
    declr: CDeclarator<NodeInfo>,
) -> EnvMonad<s, IntermediateType> {
    join((derivedDeferredTypeOf(deferred, declr, vec![])))
}

pub fn derivedDeferredTypeOf<s>(
    deferred: EnvMonad<s, IntermediateType>,
    declr: CDeclarator<NodeInfo>,
    argtypes: Vec<CDecl>,
) -> EnvMonad<s, EnvMonad<s, IntermediateType>> {
    let derived = argtypes.1.clone();

    let derive = |_0| {
        match (_0) {
            CPtrDeclr(quals, _) => {
                __return(box |itype| {
                    __assign!(if typeIsFunc(itype) {                             
__return(__assign!(itype, {
                                    typeIsFunc: false
                                }))} else {
__return(itype)
                            }, {
                            typeMutable: mutable(quals),
                            typeRep: IsPtr((typeMutable(itype)), (typeRep(itype)))
                        })
                })
            }
            CArrDeclr(quals, arraySize, _) => {
                __return(box |itype| {
                    if typeIsFunc(itype) {
                        badSource(declr, "function as array element type".to_string())
                    } else {
                        /*do*/
                        {
                            let sizeExpr = match arraySize {
                                CArrSize(_, sizeExpr) => __return(sizeExpr),
                                CNoArrSize(_) => unimplemented(declr),
                            };

                            let size = interpretConstExpr(sizeExpr);

                            __return(__assign!(itype, {
                                    typeMutable: mutable(quals),
                                    typeRep: IsArray((typeMutable(itype)), (fromInteger(size)), (typeRep(itype)))
                                }))
                        }
                    }
                })
            }
            CFunDeclr(foo, _, _) => {
                /*do*/
                {
                    let preAnsiArgs = Map::fromList(
                        /*do*/ {
                            let (argspecs, declrs) = if let CDeclaration::CDecl(argspecs, declrs, _) = argtypes {
                                (argspecs, declrs)
                            } else {
                                unreachable!();
                            };

                            let (argname, pos) = if let (Some(CDeclarator(Some(argname), _, _, _, pos)), None, None) = declrs {
                                (argname, pos)
                            } else {
                                unreachable!();
                            };

                            let declr_q = declrs.0;

                            __return((argname, CDecl(argspecs, vec![(Some(declr_q), None, None)], pos)))
                        }
                    );

                    let (args, variadic) = match foo {
                        Right((args, variadic)) => __return((args, variadic)),
                        Left(argnames) => {
                            /*do*/
                            {
                                let argdecls = __forM!(argnames, box |argname| { match Map::lookup(argname, preAnsiArgs) {
                                                    None => {
                                                        badSource(declr, (__op_addadd("undeclared argument ".to_string(), show((identToString(argname))))))
                                                    },
                                                    Some(arg) => {
                                                        __return(arg)
                                                    },
                                                } });

                                __return((argdecls, false))
                            }
                        }
                    };

                    let args_q = sequence(
                        /*do*/ {
                            let arg = match args {
                                [CDecl([CTypeSpec(CVoidType(_))], [], _)] => {
                                    vec![]
                                },
                                _ => {
                                    args
                                },
                            };

                            let (argspecs, declr_q) = if let CDecl(argspecs, declr_q, _) = arg {
                                (argspecs, declr_q)
                            } else {
                                unreachable!()
                            };

                            __return(/*do*/ {
                                    let (storage, base_q) = baseTypeOf(argspecs);

                                    match storage {
                                        None => {
                                            __return(())
                                        },
                                        Some(CRegister(_)) => {
                                            __return(())
                                        },
                                        Some(s) => {
                                            badSource(s, "storage class specifier on argument".to_string())
                                        },
                                    };
                                    let (argname, argTy) = match declr_q {
                                            [] => {
                                                __return((None, base_q))
                                            },
                                            [(Some(argdeclr, __OP__, CDeclarator(argname, _, _, _, _)), None, None)] => {
                                                /*do*/ {
                                                    let argTy = derivedDeferredTypeOf(base_q, argdeclr, vec![]);

                                                    __return((argname, argTy))
                                                }
                                            },
                                            _ => {
                                                badSource(arg, "function argument".to_string())
                                            },
                                        };

                                    __return(/*do*/ {
                                            let itype = argTy;

                                            if (typeIsFunc(itype)) { (badSource(arg, "function as function argument".to_string())) };
                                            let ty = match typeRep(itype) {
                                                    IsArray(__mut, _, el) => {
                                                        IsPtr(__mut, el)
                                                    },
                                                    orig => {
                                                        orig
                                                    },
                                                };

                                            __return((__fmap!((__op_tuple2((typeMutable(itype)))), argname), ty))
                                        })
                                })
                        }
                    );

                    __return(box |itype| {
                        /*do*/
                        {
                            if (typeIsFunc(itype)) {
                                (badSource(declr, "function as function return type".to_string()))
                            };
                            let args_q_q = sequence(args_q);

                            __return(__assign!(itype, {
                                        typeIsFunc: true,
                                        typeRep: IsFunc((typeRep(itype)), args_q_q, variadic)
                                    }))
                        }
                    })
                }
            }
        }
    };

    /*do*/
    {
        let derived_q = __mapM!(derive, derived);

        __return(
            /*do*/
            {
                let basetype = deferred;

                __foldrM!((__op_dollar), basetype, derived_q)
            },
        )
    }
}

pub fn mutable<a>(quals: Vec<CTypeQualifier<a>>) -> Rust::Mutable {
    if any(
        (box |q| match q {
             CConstQual(_) => true,
             _ => false,
         }),
        quals,
    ) {
        Rust::Immutable
    } else {
        Rust::Mutable
    }
}

pub fn typeName<s>(decl: CDecl) -> EnvMonad<s, (Rust::Mutable, CType)> {
    match decl {
        CStaticAssert { .. } => badSource(decl, "static assert in type name ".to_string()),
        CDecl(spec, declarators, _) => {
            /*do*/
            {
                let (storage, base) = baseTypeOf(spec);

                match storage {
                    Some(s) => badSource(s, "storage class specifier in type name".to_string()),
                    None => __return(()),
                };
                let itype = match declarators {
                    [] => base,
                    [(Some(declr, __OP__, CDeclarator(None, _, _, _, _)), None, None)] => {
                        derivedTypeOf(base, declr)
                    }
                    _ => badSource(decl, "type name".to_string()),
                };

                if (typeIsFunc(itype)) {
                    (badSource(decl, "use of function type".to_string()))
                };
                __return((typeMutable(itype), typeRep(itype)))
            }
        }
    }
}
