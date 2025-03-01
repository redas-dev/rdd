#pragma once

#include <vector>
#include <iostream>
#include <optional>
#include <variant>

#include "Token.hpp"
#include "Arena.hpp"
#include "Node.hpp"

inline std::optional<int> Precedence(const Token& token)
{
    if (token.value == "<" || token.value == ">" || token.value == "==" || token.value == "!=" || token.value == "<=" || token.value == ">=" || token.value == "||" || token.value == "&&" | token.value == "&" || token.value == "|")
        return 0;
    if (token.value == "*" || token.value == "/")
        return 2;
    if (token.value == "+" || token.value == "-")
        return 1;
    return {};
}

class SyntaxAnalyzer {
public:
     explicit SyntaxAnalyzer(std::vector<Token> tokens) : tokens(std::move(tokens)), arena(1024 * 1024 * 4) {}
    
    std::optional<Node::Term*> ParseTerm() // NOLINT(*-no-recursion)
    {
        if (const auto intLit = TryConsume(Tokens::NUMBER())) {
            auto term_int_lit = arena.allocate<Node::TermIntLit>();
            term_int_lit->int_lit = intLit.value();

            auto term = arena.allocate<Node::Term>();
            term->term = term_int_lit;

            return term;
        }

         if (const auto stringLit = TryConsume(Tokens::STRING())) {
            auto term_string_lit = arena.allocate<Node::TermStringLit>();
            term_string_lit->string_lit = stringLit.value();

            auto term = arena.allocate<Node::Term>();
            term->term = term_string_lit;

            return term;
         }

        if (const auto ident = TryConsume(Tokens::IDENTIFIER())) {
            auto node = arena.allocate<Node::TermIdent>();
            node->ident = ident.value();

            auto term = arena.allocate<Node::Term>();
            term->term = node;

            return term;
        }

        if(TryConsume(Tokens::DELIMITER(), "("))
        {
            if (const auto expr = ParseExpr()) {
                TryConsumeErr(Tokens::DELIMITER(), ")");

                auto term_paren = arena.allocate<Node::TermParen>();
                term_paren->expr = expr.value();

                auto term = arena.allocate<Node::Term>();
                term->term = term_paren;

                return term;
            }
        }

        return {};
    }

    std::optional<Node::Expr*> ParseExpr(const int min_prec = 0){ // NOLINT(*-no-recursion)

        std::optional<Node::Term*> term_lhs = ParseTerm();

        if (!term_lhs.has_value())
            ErrorExpected("expression", Peek(-1).value().lineNum);

        auto lhs_exp = arena.allocate<Node::Expr>();
        lhs_exp->expr = term_lhs.value();

        while(true){
            std::optional<Token> tok = Peek();
            std::optional<int> prec;

            if (tok.has_value())
            {
                prec = Precedence(tok.value());
                if (!prec.has_value() || prec.value() < min_prec)
                    break;
            }
            else break;
            Token op = Consume();

            const int next_min_prec = prec.value() + 1;

            auto exp_rhs = ParseExpr(next_min_prec);

            if (!exp_rhs.has_value())
                ErrorExpected("expression", Peek(-1).value().lineNum);

            const auto exp = arena.allocate<Node::BinaryExpression>();
            const auto _exp_lhs = arena.allocate<Node::Expr>();

            _exp_lhs->expr = lhs_exp->expr;
            exp->lhs = _exp_lhs;
            exp->rhs = exp_rhs.value();
            exp->sign = op.value;
            lhs_exp->expr = exp;
        }

        return lhs_exp;
    }

    std::optional<Node::Scope*> ParseScope(const bool functionScope = false, const bool isVoid = false) // NOLINT(*-no-recursion)
    {
        TryConsumeErr(Tokens::DELIMITER(), "{");

        auto scope = arena.allocate<Node::Scope>();
        while (auto stmt = ParseStmt())
        {
            scope->stmts.push_back(stmt.value());
        }

        if (functionScope && !isVoid)
        {
            if (scope->stmts.empty() || !std::holds_alternative<Node::StmtReturn*>(scope->stmts.back()->stmt))
            {
                ErrorExpected("return statement", Peek(-1).value().lineNum);
            }
        }

        TryConsumeErr(Tokens::DELIMITER(), "}");
        
        return scope;
    }

    std::optional<Node::IFPred*>ParseIfPred() // NOLINT(*-no-recursion)
    {
        if (TryConsume(Tokens::KEYWORD(), "else if"))
        {
            TryConsumeErr(Tokens::DELIMITER(), "(");
            auto stmt_if = arena.allocate<Node::IFPredElseIf>();

            if (const auto exp = ParseExpr())
            {
                stmt_if->condition = exp.value();
                TryConsumeErr(Tokens::DELIMITER(), ")");
                
                if (const auto scope = ParseScope())
                    stmt_if->scope = scope.value();
                else ErrorExpected("scope", Peek(-1).value().lineNum);

                stmt_if->pred = ParseIfPred();

                auto stmt = arena.allocate<Node::IFPred>();
                stmt->pred = stmt_if;

                return stmt;
            }

            ErrorExpected("expression", Peek(-1).value().lineNum);
        }
        else if (TryConsume(Tokens::KEYWORD(), "else"))
        {
            if (const auto scope = ParseScope())
            {
                auto stmt_else = arena.allocate<Node::IFPredElse>();
                stmt_else->scope = scope.value();

                auto stmt = arena.allocate<Node::IFPred>();
                stmt->pred = stmt_else;

                return stmt;
            }

            ErrorExpected("scope", Peek(-1).value().lineNum);
        }

        return {};
    }

    std::optional<Node::StmtVar*> ParseVar(const bool funcVar = false) {
        if (const auto variable = TryConsume(Tokens::IDENTIFIER())){
            if (TryConsume(Tokens::OPERATOR(), ":")){ // Declaration
                const Token type = TryConsumeErr(Tokens::TYPENAME());

                if (TryConsume(Tokens::OPERATOR(), "=")){  // Declaration with assignment
                    if (const auto expr = ParseExpr()){
                        auto var_decl = arena.allocate<Node::VarDecl>();
                        var_decl->ident = variable.value();
                        var_decl->ident.varType = type.varType.value();
                        var_decl->expr = expr.value();

                        auto stmt_var = arena.allocate<Node::StmtVar>();
                        stmt_var->var = var_decl;

                        return stmt_var;
                    }
                }
                else if (TryPeek(Tokens::DELIMITER(), ";")){  // Declaration without assignment
                    auto var_decl = arena.allocate<Node::VarDecl>();
                    var_decl->ident = variable.value();
                    var_decl->expr = nullptr;
                    var_decl->ident.varType = type.varType.value();

                    auto stmt_var = arena.allocate<Node::StmtVar>();
                    stmt_var->var = var_decl;

                    return stmt_var;
                }
                else if (funcVar) { // Variable declarations in functions
                    auto var_decl = arena.allocate<Node::FunVar>();
                    var_decl->ident = variable.value();
                    var_decl->expr = nullptr;
                    var_decl->ident.varType = type.varType.value();

                    auto stmt_var = arena.allocate<Node::StmtVar>();
                    stmt_var->var = var_decl;

                    return stmt_var;
                }
                else {
                    ErrorExpected("= or ;", Peek(-1).value().lineNum);
                }
            }
            else if (TryConsume(Tokens::OPERATOR(), "=")){ // Assignment
                if (const auto expr = ParseExpr()){
                    auto var_decl = arena.allocate<Node::VarAssign>();
                    var_decl->ident = variable.value();
                    var_decl->expr = expr.value();

                    auto stmt_var = arena.allocate<Node::StmtVar>();
                    stmt_var->var = var_decl;

                    return stmt_var;
                }

                ErrorExpected("expression", Peek(-1).value().lineNum);
            }

            ErrorExpected("= or :", Peek(-1).value().lineNum);
        }
        return {};
    }

    std::optional<Node::Stmt*> ParseStmt() // NOLINT(*-no-recursion)
    {
        if (TryConsume(Tokens::KEYWORD(), "exit")){
            TryConsumeErr(Tokens::DELIMITER(), "(");
            if (const auto expr = ParseExpr()) {
                TryConsumeErr(Tokens::DELIMITER(), ")");
                
                TryConsumeErr(Tokens::DELIMITER(), ";");

                auto stmt = arena.allocate<Node::StmtExit>();
                stmt->exp = expr.value();

                auto node = arena.allocate<Node::Stmt>();
                node->stmt = stmt;

                return node;
            } 
            ErrorExpected("expression", Peek(-1).value().lineNum);
        }
        else if (TryConsume(Tokens::KEYWORD(), "while")) {
            if (const auto exp = ParseExpr()) {
                if (const auto scope = ParseScope()) {
                    auto stmt_while = arena.allocate<Node::StmtWhile>();
                    stmt_while->condition = exp.value();
                    stmt_while->scope = scope.value();

                    auto stmt = arena.allocate<Node::Stmt>();
                    stmt->stmt = stmt_while;

                    return stmt;
                } ErrorExpected("scope", Peek(-1).value().lineNum);
            } else ErrorExpected("expression", Peek(-1).value().lineNum);
        }
        else if (TryConsume(Tokens::KEYWORD(), "from")) {
            if (const auto varStmt = ParseVar()) {
                TryConsumeErr(Tokens::DELIMITER(), ";");
                TryConsumeErr(Tokens::KEYWORD(), "to");

                if (const auto exp2 = ParseExpr()) {
                    TryConsumeErr(Tokens::DELIMITER(), ";");
                    TryConsumeErr(Tokens::KEYWORD(), "by");

                    if (const auto inc = ParseVar()) {
                        TryConsumeErr(Tokens::DELIMITER(), ";");
                        auto stmt_for = arena.allocate<Node::StmtFor>();
                        stmt_for->condition = exp2.value();
                        stmt_for->init = varStmt.value();

                        auto stmt_inc = arena.allocate<Node::Stmt>();
                        stmt_inc->stmt = inc.value();
                        stmt_for->increment = stmt_inc;

                        if (const auto scope = ParseScope()) {
                            stmt_for->scope = scope.value();
                        } else ErrorExpected("scope", Peek(-1).value().lineNum);

                        auto stmt = arena.allocate<Node::Stmt>();
                        stmt->stmt = stmt_for;

                        return stmt;
                    }
                    ErrorExpected("expression", Peek(-1).value().lineNum);
                }
            } else ErrorExpected("expression", Peek(-1).value().lineNum);
        }
        else if (TryPeek(Tokens::IDENTIFIER())) {
            if (TryPeek(Tokens::OPERATOR(), ":", 1) || TryPeek(Tokens::OPERATOR(), "=", 1)) {
                auto var = ParseVar();
                TryConsumeErr(Tokens::DELIMITER(), ";");
                auto stmt = arena.allocate<Node::Stmt>();
                stmt->stmt = var.value();
                return stmt;
            }

            ErrorExpected("variable declaration", Peek(-1).value().lineNum);
        }
        else if (TryConsume(Tokens::KEYWORD(), "print")){
            TryConsumeErr(Tokens::DELIMITER(), "(");
            auto stmt_print = arena.allocate<Node::StmtPrint>();
            if (const auto exp = ParseExpr()){
                TryConsumeErr(Tokens::DELIMITER(), ")");
                TryConsumeErr(Tokens::DELIMITER(), ";");

                stmt_print->exp = exp.value();

                auto stmt = arena.allocate<Node::Stmt>();
                stmt->stmt = stmt_print;

                return stmt;
            }

            ErrorExpected("expression", Peek(-1).value().lineNum);
        }
        else if (TryPeek(Tokens::DELIMITER(), "{"))
        {
            if (auto scope = ParseScope())
            {
                auto stmt = arena.allocate<Node::Stmt>();
                stmt->stmt = scope.value();

                return stmt;
            }
            ErrorExpected("scope", Peek(-1).value().lineNum);
        }
        else if (TryConsume(Tokens::KEYWORD(), "if"))
        {
            TryConsumeErr(Tokens::DELIMITER(), "(");
            auto stmt_if = arena.allocate<Node::IF>();
            if (const auto exp = ParseExpr())
            {
                stmt_if->condition = exp.value();
                TryConsumeErr(Tokens::DELIMITER(), ")");

                if (const auto scope = ParseScope())
                {
                    stmt_if->scope = scope.value();
                }
                else ErrorExpected("scope", Peek(-1).value().lineNum);

                stmt_if->pred = ParseIfPred();

                auto stmt = arena.allocate<Node::Stmt>();
                stmt->stmt = stmt_if;
                return stmt;

            }
            ErrorExpected("expression", Peek(-1).value().lineNum);
        }
         return {};
    }

    std::optional<Node::Prog*> ParseProg()
    {
        std::vector<Node::Stmt*> stmts;
        while (Peek().has_value()) {
            if (auto stmt = ParseStmt()) {
                stmts.push_back(stmt.value());
            }
            else ErrorExpected("statement", Peek(-1).value().lineNum);
        }
        
        auto prog = arena.allocate<Node::Prog>();
        prog->stmts = stmts;

        return prog;
    }

    ~SyntaxAnalyzer() = default;
private:
    const std::vector<Token> tokens;
    size_t index = 0;
    Arena arena;

    [[nodiscard]] std::optional<Token> Peek(const int offset = 0) const {
        if (index + offset >= tokens.size())
            return {};
        return tokens.at(index + offset);
    }

    Token Consume()
    {
        return tokens[index++];
    }

    void ErrorExpected(const std::string& msg, const int line)
    {
        std::cerr << "[Parser error]: Expected " << msg << " on line " << line << std::endl;

        exit(EXIT_FAILURE);
    }

    template <typename T> bool TryPeek(T type, int offset = 0)
    {
        return Peek(offset).has_value() && Peek(offset).value().type == type;
    }

    template <typename T> bool TryPeek(T type, const std::string& val, int offset = 0)
    {
        return Peek(offset).has_value() && Peek(offset).value().type == type && Peek(offset).value().value == val;
    }

    template <typename T> Token TryConsumeErr(T type)
    {
        if (Peek().has_value() && Peek().value().type == type)
            return Consume();

        ErrorExpected("type", Peek(-1).value().lineNum);
        return Peek(-1).value();
    }

    template <typename T> Token TryConsumeErr(T type, const std::string& val)
    {
        if (Peek().has_value() && Peek().value().type == type && Peek().value().value == val)
            return Consume();

        ErrorExpected(val, Peek(-1).value().lineNum);
        return Peek(-1).value();
    }

    template <typename T> std::optional<Token> TryConsume(T type, const std::string& val)
    {
        if (Peek().has_value() && Peek().value().type == type && Peek().value().value == val)
            return Consume();
        return {};
    }

    template <typename T> std::optional<Token> TryConsume(T type)
    {
        if (Peek().has_value() && Peek().value().type == type)
            return Consume();
        return {};
    }
};