/// A trait that can be used to check if an item is generic or depends on generic.
pub trait IsGeneric {
    fn is_generic(&self, generics: &syn::Generics) -> bool;
}

impl IsGeneric for syn::Ident {
    fn is_generic(&self, generics: &syn::Generics) -> bool {
        generics.params.iter().any(|param| match param {
            syn::GenericParam::Type(type_param) => type_param.ident == *self,
            syn::GenericParam::Const(const_param) => const_param.ident == *self,
            _ => false,
        })
    }
}

impl IsGeneric for syn::GenericArgument {
    fn is_generic(&self, generics: &syn::Generics) -> bool {
        match self {
            syn::GenericArgument::Lifetime(_) => false,
            syn::GenericArgument::Type(ty) => ty.is_generic(generics),
            syn::GenericArgument::Const(expr) => expr.is_generic(generics),
            syn::GenericArgument::AssocType(assoc_type) => {
                assoc_type
                    .generics
                    .iter()
                    .any(|g| g.args.iter().any(|arg| arg.is_generic(generics)))
                    || assoc_type.ty.is_generic(generics)
            }
            syn::GenericArgument::AssocConst(assoc_const) => {
                assoc_const
                    .generics
                    .iter()
                    .any(|g| g.args.iter().any(|arg| arg.is_generic(generics)))
                    || assoc_const.value.is_generic(generics)
            }
            syn::GenericArgument::Constraint(constraint) => {
                constraint
                    .generics
                    .iter()
                    .any(|g| g.args.iter().any(|arg| arg.is_generic(generics)))
                    || constraint.bounds.iter().any(|bound| match bound {
                        syn::TypeParamBound::Trait(trait_bound) => {
                            trait_bound.path.is_generic(generics)
                        }
                        _ => false,
                    })
            }
            _ => false,
        }
    }
}

impl IsGeneric for syn::Path {
    fn is_generic(&self, generics: &syn::Generics) -> bool {
        if self.leading_colon.is_none() {
            if let Some(segment) = self.segments.first() {
                if segment.ident.is_generic(generics) {
                    return true;
                }
            }
        }

        self.segments
            .iter()
            .any(|segment| match &segment.arguments {
                syn::PathArguments::AngleBracketed(angle) => {
                    angle.args.iter().any(|arg| arg.is_generic(generics))
                }
                syn::PathArguments::Parenthesized(paren) => {
                    paren.inputs.iter().any(|ty| ty.is_generic(generics))
                        || paren.output.is_generic(generics)
                }
                _ => false,
            })
    }
}

impl IsGeneric for syn::Type {
    fn is_generic(&self, generics: &syn::Generics) -> bool {
        match self {
            syn::Type::Array(array) => {
                array.elem.is_generic(generics) || array.len.is_generic(generics)
            }
            syn::Type::BareFn(bare_fn) => {
                bare_fn.inputs.iter().any(|arg| arg.ty.is_generic(generics))
                    || match &bare_fn.output {
                        syn::ReturnType::Default => false,
                        syn::ReturnType::Type(_, ty) => ty.is_generic(generics),
                    }
            }
            syn::Type::Group(group) => group.elem.is_generic(generics),
            syn::Type::ImplTrait(impl_trait) => impl_trait.bounds.iter().any(|bound| match bound {
                syn::TypeParamBound::Trait(trait_bound) => trait_bound.path.is_generic(generics),
                _ => false,
            }),
            syn::Type::Infer(_) => false,
            syn::Type::Macro(_) => false,
            syn::Type::Never(_) => false,
            syn::Type::Paren(paren) => paren.elem.is_generic(generics),
            syn::Type::Path(path) => {
                path.qself
                    .as_ref()
                    .map_or(false, |qself| qself.ty.is_generic(generics))
                    || path.path.is_generic(generics)
            }
            syn::Type::Ptr(ptr) => ptr.elem.is_generic(generics),
            syn::Type::Reference(reference) => reference.elem.is_generic(generics),
            syn::Type::Slice(slice) => slice.elem.is_generic(generics),
            syn::Type::TraitObject(trait_object) => {
                trait_object.bounds.iter().any(|bound| match bound {
                    syn::TypeParamBound::Trait(trait_bound) => {
                        trait_bound.path.is_generic(generics)
                    }
                    _ => false,
                })
            }
            syn::Type::Tuple(tuple) => tuple.elems.iter().any(|ty| ty.is_generic(generics)),

            _ => false,
        }
    }
}

impl IsGeneric for syn::Block {
    fn is_generic(&self, generics: &syn::Generics) -> bool {
        self.stmts.iter().any(|stmt| match stmt {
            syn::Stmt::Local(syn::Local {
                init: Some(init), ..
            }) => {
                init.expr.is_generic(generics)
                    || init
                        .diverge
                        .as_ref()
                        .map_or(false, |diverge| diverge.1.is_generic(generics))
            }
            syn::Stmt::Expr(expr, _) => expr.is_generic(generics),
            _ => false,
        })
    }
}

impl IsGeneric for syn::Expr {
    fn is_generic(&self, generics: &syn::Generics) -> bool {
        match self {
            syn::Expr::Array(array) => array.elems.iter().any(|elem| elem.is_generic(generics)),
            syn::Expr::Assign(assign) => {
                assign.left.is_generic(generics) || assign.right.is_generic(generics)
            }
            syn::Expr::Async(async_expr) => async_expr.block.is_generic(generics),
            syn::Expr::Await(await_expr) => await_expr.base.is_generic(generics),
            syn::Expr::Binary(binary) => {
                binary.left.is_generic(generics) || binary.right.is_generic(generics)
            }
            syn::Expr::Block(block) => block.block.is_generic(generics),
            syn::Expr::Break(syn::ExprBreak {
                expr: Some(expr), ..
            }) => expr.is_generic(generics),
            syn::Expr::Call(call) => {
                call.func.is_generic(generics)
                    || call.args.iter().any(|arg| arg.is_generic(generics))
            }
            syn::Expr::Cast(cast) => cast.expr.is_generic(generics) || cast.ty.is_generic(generics),
            syn::Expr::Closure(closure) => {
                closure
                    .inputs
                    .iter()
                    .any(|input| input.is_generic(generics))
                    || closure.output.is_generic(generics)
                    || closure.body.is_generic(generics)
            }
            syn::Expr::Const(const_expr) => const_expr.block.is_generic(generics),
            syn::Expr::Continue(_) => false,
            syn::Expr::Field(field) => field.base.is_generic(generics),
            syn::Expr::ForLoop(for_loop) => {
                for_loop.pat.is_generic(generics)
                    || for_loop.expr.is_generic(generics)
                    || for_loop.body.is_generic(generics)
            }
            syn::Expr::If(if_expr) => {
                if_expr.cond.is_generic(generics)
                    || if_expr.then_branch.is_generic(generics)
                    || if_expr
                        .else_branch
                        .as_ref()
                        .map_or(false, |branch| branch.1.is_generic(generics))
            }
            syn::Expr::Index(index_expr) => {
                index_expr.expr.is_generic(generics) || index_expr.index.is_generic(generics)
            }
            syn::Expr::Infer(_) => false,
            syn::Expr::Let(let_expr) => {
                let_expr.pat.is_generic(generics) || let_expr.expr.is_generic(generics)
            }
            syn::Expr::Lit(_) => false,
            syn::Expr::Loop(loop_expr) => loop_expr.body.is_generic(generics),
            syn::Expr::Macro(_) => false,
            syn::Expr::Match(match_expr) => {
                match_expr.expr.is_generic(generics)
                    || match_expr.arms.iter().any(|arm| {
                        arm.pat.is_generic(generics)
                            || arm
                                .guard
                                .as_ref()
                                .map_or(false, |(_, guard)| guard.is_generic(generics))
                            || arm.body.is_generic(generics)
                    })
            }
            syn::Expr::MethodCall(method_call) => {
                method_call.receiver.is_generic(generics)
                    || method_call.turbofish.as_ref().map_or(false, |turbofish| {
                        turbofish.args.iter().any(|arg| arg.is_generic(generics))
                    })
                    || method_call.args.iter().any(|arg| arg.is_generic(generics))
            }
            syn::Expr::Paren(paren) => paren.expr.is_generic(generics),
            syn::Expr::Path(path) => {
                path.qself
                    .as_ref()
                    .map_or(false, |qself| qself.ty.is_generic(generics))
                    || path.path.is_generic(generics)
            }
            syn::Expr::Range(range) => {
                range
                    .start
                    .as_ref()
                    .map_or(false, |start| start.is_generic(generics))
                    || range
                        .end
                        .as_ref()
                        .map_or(false, |end| end.is_generic(generics))
            }
            syn::Expr::Reference(reference) => reference.expr.is_generic(generics),
            syn::Expr::Repeat(repeat) => {
                repeat.expr.is_generic(generics) || repeat.len.is_generic(generics)
            }
            syn::Expr::Return(return_expr) => return_expr
                .expr
                .as_ref()
                .map_or(false, |expr| expr.is_generic(generics)),
            syn::Expr::Struct(struct_expr) => {
                struct_expr.path.is_generic(generics)
                    || struct_expr
                        .fields
                        .iter()
                        .any(|field| field.expr.is_generic(generics))
            }
            syn::Expr::Try(try_expr) => try_expr.expr.is_generic(generics),
            syn::Expr::TryBlock(try_block) => try_block.block.is_generic(generics),
            syn::Expr::Tuple(tuple) => tuple.elems.iter().any(|elem| elem.is_generic(generics)),
            syn::Expr::Unary(unary) => unary.expr.is_generic(generics),
            syn::Expr::Unsafe(unsafe_expr) => unsafe_expr.block.is_generic(generics),
            syn::Expr::While(while_expr) => {
                while_expr.cond.is_generic(generics) || while_expr.body.is_generic(generics)
            }
            syn::Expr::Yield(yield_expr) => yield_expr
                .expr
                .as_ref()
                .map_or(false, |expr| expr.is_generic(generics)),
            _ => false,
        }
    }
}

impl IsGeneric for syn::ReturnType {
    fn is_generic(&self, generics: &syn::Generics) -> bool {
        match self {
            syn::ReturnType::Default => false,
            syn::ReturnType::Type(_, ty) => ty.is_generic(generics),
        }
    }
}

impl IsGeneric for syn::Pat {
    fn is_generic(&self, generics: &syn::Generics) -> bool {
        match self {
            syn::Pat::Const(pat) => pat.block.is_generic(generics),
            syn::Pat::Ident(ident) => ident.ident.is_generic(generics),
            syn::Pat::Path(path) => path.path.is_generic(generics),
            syn::Pat::Reference(reference) => reference.pat.is_generic(generics),
            syn::Pat::Rest(_) => false,
            syn::Pat::Struct(struct_pat) => {
                struct_pat
                    .qself
                    .as_ref()
                    .map_or(false, |qself| qself.ty.is_generic(generics))
                    || struct_pat.path.is_generic(generics)
                    || struct_pat
                        .fields
                        .iter()
                        .any(|field| field.pat.is_generic(generics))
            }
            syn::Pat::TupleStruct(tuple_struct) => tuple_struct
                .elems
                .iter()
                .any(|pat| pat.is_generic(generics)),
            syn::Pat::Tuple(tuple) => tuple.elems.iter().any(|pat| pat.is_generic(generics)),
            syn::Pat::Type(pat) => pat.pat.is_generic(generics) || pat.ty.is_generic(generics),
            syn::Pat::Wild(_) => false,
            _ => false,
        }
    }
}
