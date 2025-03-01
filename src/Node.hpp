#pragma once
#include <vector>
#include <variant>
#include <optional>

#include "Token.hpp"

namespace Node {

    struct Scope;
    struct Expr;
    struct Stmt;
    struct IFPred;

    struct BinaryExpression {
        Expr* lhs {};
        Expr* rhs {};
        std::string sign;
    };

    struct Scope {
        std::vector<Stmt*> stmts;
    };

    struct IFPredElseIf {
        Expr* condition{};
        Scope* scope{};
        std::optional<IFPred*> pred;
    };

    struct IFPredElse {
        Scope* scope;
    };

    using IFPredVariant = std::variant<IFPredElse*, IFPredElseIf*>;

    struct IFPred {
        IFPredVariant pred;
    };

    struct IF {
        Expr* condition{};
        Scope* scope{};
        std::optional<IFPred*> pred;
    };

    struct StmtPrint {
        Expr* exp;
    };

    struct TermIntLit {
        Token int_lit;
    };

    struct TermIdent {
        Token ident;
        std::string type;
    };

    struct TermParen {
        Expr* expr;
    };

    struct TermStringLit {
        Token string_lit;
    };

    using TermVariant = std::variant<TermIntLit*, TermIdent*, TermParen*, TermStringLit*>;

    struct Term {
        TermVariant term;
    };

    using ExprVariant = std::variant<Term*, BinaryExpression*>;

    struct Expr {
        ExprVariant expr;
    };

    struct StmtExit {
        Expr* exp;
    };

    struct VarDecl {
        Expr* expr;
        Token ident;
    };

    struct VarAssign {
        Expr* expr;
        Token ident;
    };

    struct FunVar {
        Expr* expr;
        Token ident;
    };

    struct StmtReturn {
        Expr* expr;
    };

    struct FunCall {
        std::string name;
        std::vector<Expr*> args;
    };

    using StmtVarVariant = std::variant<VarDecl*, VarAssign*, FunVar*>;

    struct StmtVar {
        StmtVarVariant var;
    };

    struct StmtFor {
        StmtVar* init;
        Expr* condition;
        Scope* scope;
        Stmt* increment;
    };

    struct StmtWhile {
        Expr* condition;
        Scope* scope;
    };

    struct Function {
        std::string name;
        std::vector<StmtVar*> args;
        std::string returnType;
        Scope* scope;
        StmtReturn* result;
    };

    using StmtVariant = std::variant<StmtExit*, StmtPrint*, Scope*, IF*, StmtVar*, StmtFor*, StmtWhile*, StmtReturn*, Function*, FunCall*>;

    struct Stmt {
        StmtVariant stmt;
    };

    struct Prog {
        std::vector<Stmt*> stmts;
    };

};
