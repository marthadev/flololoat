extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Expr};


/// Converts all variables, literals and calls in an expression to an f64 using `f64::from`
///
/// # Examples
///
/// ```
/// use flololoat::f64;
/// 
/// let x: u32 = 5;
/// let y: i32 = 2;
/// let z: f64 = 3.0;
/// assert_eq!(
///     f64!(x / (y + z) / 2),
///     f64::from(x) / (f64::from(y) + z) / f64::from(2)
/// )
/// ```


#[proc_macro]
pub fn f64(input: TokenStream) -> TokenStream {
    fn recur(expr: Expr) -> Expr {
        match expr {
            Expr::Binary(expr_binary) => Expr::Binary(syn::ExprBinary {
                attrs: Vec::new(),
                left: Box::new(recur(*expr_binary.left)),
                op: expr_binary.op,
                right: Box::new(recur(*expr_binary.right)),
            }),
            Expr::Unary(expr_unary) => Expr::Unary(syn::ExprUnary {
                attrs: Vec::new(),
                op: expr_unary.op,
                expr: Box::new(recur(*expr_unary.expr)),
            }),
            Expr::Group(expr_group) => Expr::Group(syn::ExprGroup {
                attrs: Vec::new(),
                group_token: expr_group.group_token,
                expr: Box::new(recur(*expr_group.expr)),
            }),
            Expr::Paren(expr_paren) => Expr::Paren(syn::ExprParen {
                attrs: Vec::new(),
                paren_token: expr_paren.paren_token,
                expr: Box::new(recur(*expr_paren.expr)),
            }),
            Expr::Reference(_) | Expr::Path(_) | Expr::Lit(_) | Expr::Call(_) => {
                let f64_from = quote! { f64::from(#expr) };
                syn::parse2(f64_from).unwrap()
            }
            _ => expr,
        }
    }

    let result_expr = recur(parse_macro_input!(input as Expr));
    TokenStream::from(quote!(#result_expr))
}

/// Converts all variables, literals and calls in an expression to an f32 using `f32::from`
///
/// # Examples
///
/// ```
/// use flololoat::f32;
/// 
/// let x: u16 = 5;
/// let y: i16 = 2;
/// let z: f32 = 3.0;
/// assert_eq!(
///     f32!(x / (y + z) / 2_i16),
///     f32::from(x) / (f32::from(y) + z) / f32::from(2_i16)
/// )
/// ```

#[proc_macro]
pub fn f32(input: TokenStream) -> TokenStream {
    fn recur(expr: Expr) -> Expr {
        match expr {
            Expr::Binary(expr_binary) => Expr::Binary(syn::ExprBinary {
                attrs: Vec::new(),
                left: Box::new(recur(*expr_binary.left)),
                op: expr_binary.op,
                right: Box::new(recur(*expr_binary.right)),
            }),
            Expr::Unary(expr_unary) => Expr::Unary(syn::ExprUnary {
                attrs: Vec::new(),
                op: expr_unary.op,
                expr: Box::new(recur(*expr_unary.expr)),
            }),
            Expr::Group(expr_group) => Expr::Group(syn::ExprGroup {
                attrs: Vec::new(),
                group_token: expr_group.group_token,
                expr: Box::new(recur(*expr_group.expr)),
            }),
            Expr::Paren(expr_paren) => Expr::Paren(syn::ExprParen {
                attrs: Vec::new(),
                paren_token: expr_paren.paren_token,
                expr: Box::new(recur(*expr_paren.expr)),
            }),
            Expr::Reference(_) | Expr::Path(_) | Expr::Lit(_) | Expr::Call(_) => {
                let f64_from = quote! { f32::from(#expr) };
                syn::parse2(f64_from).unwrap()
            }
            _ => expr,
        }
    }

    let result_expr = recur(parse_macro_input!(input as Expr));
    TokenStream::from(quote!(#result_expr))
}
