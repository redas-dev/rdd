#pragma once

#include <cassert>
#include <vector>

#include "SyntaxAnalyzer.hpp"

class Generator {
    public:
        explicit Generator(Node::Prog* root): root(root) {}

        void GenerateExp(const Node::Expr* exp) {
            struct ExpVisitor {
                Generator& generator;

                void operator()(const Node::Term* term) const
                {
                    generator.GenerateTerm(term);
                }
                void operator()(const Node::CompExp* comp) const {
                    if (std::holds_alternative<Node::CompExpEq*>(comp->comp_exp)) {
                        const Node::CompExpEq* eq = std::get<Node::CompExpEq*>(comp->comp_exp);

                        generator.GenerateExp(eq->rhs);
                        generator.GenerateExp(eq->lhs);

                        generator.pop("rax");
                        generator.pop("rbx");

                        generator.output << "    cmp rax, rbx\n";
                        generator.output << "    sete al\n";
                        generator.output << "    movzx rax, al\n";
                        generator.push("rax");
                    } else if (std::holds_alternative<Node::CompExpNeq*>(comp->comp_exp)) {
                        const Node::CompExpNeq* neq = std::get<Node::CompExpNeq*>(comp->comp_exp);

                        generator.GenerateExp(neq->rhs);
                        generator.GenerateExp(neq->lhs);

                        generator.pop("rax");
                        generator.pop("rbx");

                        generator.output << "    cmp rax, rbx\n";
                        generator.output << "    setne al\n";
                        generator.output << "    movzx rax, al\n";
                        generator.push("rax");
                    } else if (std::holds_alternative<Node::CompExpGt*>(comp->comp_exp)) {
                        const Node::CompExpGt* gt = std::get<Node::CompExpGt*>(comp->comp_exp);

                        generator.GenerateExp(gt->rhs);
                        generator.GenerateExp(gt->lhs);

                        generator.pop("rax");
                        generator.pop("rbx");

                        generator.output << "    cmp rax, rbx\n";
                        generator.output << "    setg al\n";
                        generator.output << "    movzx rax, al\n";
                        generator.push("rax");
                    } else if (std::holds_alternative<Node::CompExpGte*>(comp->comp_exp)) {
                        const Node::CompExpGte* gte = std::get<Node::CompExpGte*>(comp->comp_exp);

                        generator.GenerateExp(gte->rhs);
                        generator.GenerateExp(gte->lhs);

                        generator.pop("rax");
                        generator.pop("rbx");

                        generator.output << "    cmp rax, rbx\n";
                        generator.output << "    setge al\n";
                        generator.output << "    movzx rax, al\n";
                        generator.push("rax");
                    } else if (std::holds_alternative<Node::CompExpLt*>(comp->comp_exp)) {
                        const Node::CompExpLt* lt = std::get<Node::CompExpLt*>(comp->comp_exp);

                        generator.GenerateExp(lt->rhs);
                        generator.GenerateExp(lt->lhs);

                        generator.pop("rax");
                        generator.pop("rbx");

                        generator.output << "    cmp rax, rbx\n";
                        generator.output << "    setl al\n";
                        generator.output << "    movzx rax, al\n";
                        generator.push("rax");
                    } else if (std::holds_alternative<Node::CompExpLte*>(comp->comp_exp)) {
                        const Node::CompExpLte* lte = std::get<Node::CompExpLte*>(comp->comp_exp);

                        generator.GenerateExp(lte->rhs);
                        generator.GenerateExp(lte->lhs);

                        generator.pop("rax");
                        generator.pop("rbx");

                        generator.output << "    cmp rax, rbx\n";
                        generator.output << "    setle al\n";
                        generator.output << "    movzx rax, al\n";
                        generator.push("rax");
                    } else if (std::holds_alternative<Node::CompExpAnd*>(comp->comp_exp)) {
                        const Node::CompExpAnd* and_ = std::get<Node::CompExpAnd*>(comp->comp_exp);

                        generator.GenerateExp(and_->rhs);
                        generator.GenerateExp(and_->lhs);

                        generator.pop("rax");
                        generator.pop("rbx");

                        generator.output << "    and rax, rbx\n";
                        generator.push("rax");
                    } else if (std::holds_alternative<Node::CompExpOr*>(comp->comp_exp)) {
                        const Node::CompExpOr* or_ = std::get<Node::CompExpOr*>(comp->comp_exp);

                        generator.GenerateExp(or_->rhs);
                        generator.GenerateExp(or_->lhs);

                        generator.pop("rax");
                        generator.pop("rbx");

                        generator.output << "    or rax, rbx\n";
                        generator.push("rax");
                    }
                }
                void operator()(const Node::BinExp* bin) const {
                    if (std::holds_alternative<Node::BinExpAdd*>(bin->bin_exp)) {
                        const Node::BinExpAdd* add = std::get<Node::BinExpAdd*>(bin->bin_exp);

                        generator.GenerateExp(add->rhs);
                        generator.GenerateExp(add->lhs);

                        generator.pop("rax");
                        generator.pop("rbx");

                        generator.output << "    add rax, rbx\n";
                        generator.push("rax");

                    } else if (std::holds_alternative<Node::BinExpMulti*>(bin->bin_exp)) {
                        const Node::BinExpMulti* multi = std::get<Node::BinExpMulti*>(bin->bin_exp);

                        generator.GenerateExp(multi->rhs);
                        generator.GenerateExp(multi->lhs);

                        generator.pop("rax");
                        generator.pop("rbx");

                        generator.output << "    imul rax, rbx\n";
                        generator.push("rax");
                    } else if (std::holds_alternative<Node::BinExpDiv*>(bin->bin_exp))
                    {
                        const Node::BinExpDiv* div = std::get<Node::BinExpDiv*>(bin->bin_exp);

                        generator.GenerateExp(div->rhs);
                        generator.GenerateExp(div->lhs);

                        generator.pop("rax");
                        generator.pop("rbx");

                        generator.output << "    cqo\n";
                        generator.output << "    idiv rbx\n";
                        generator.push("rax");
                    } else if (std::holds_alternative<Node::BinExpSub*>(bin->bin_exp)) {
                        const Node::BinExpSub* sub = std::get<Node::BinExpSub*>(bin->bin_exp);

                        generator.GenerateExp(sub->rhs);
                        generator.GenerateExp(sub->lhs);

                        generator.pop("rax");
                        generator.pop("rbx");

                        generator.output << "    sub rax, rbx\n";
                        generator.push("rax");
                    }
                }
            };

            if (exp == nullptr) {
                output << "    mov rax, 0\n";
                push("rax");
                return;
            }

            ExpVisitor visitor({.generator = *this});
            std::visit(visitor, exp->expr);
        }

        void GenerateTerm(const Node::Term* term)
        {
            struct TermVisitor {
                Generator& generator;
                void operator()(const  Node::TermIntLit* term_int_lit) const {
                    generator.output << ";; TermIntLit\n";
                    generator.output << "    mov rax, " << term_int_lit->int_lit.value << "\n";
                    generator.push("rax");
                }

                void operator()(const Node::TermStringLit* term_string_lit) const {
                    generator.output << ";; TermStringLit\n";
                    const std::string label = "str_" + std::to_string(generator.stringCounter++);
                    generator.strings.push_back({term_string_lit->string_lit.value, label});
                    generator.output << "     mov edx, " << label << "Len\n";
                    generator.output << "     mov ecx, " << label << "\n";
                }

                void operator()(const Node::TermIdent* term_ident) const {
                    generator.output << ";; TermIdent\n";
                    const auto variable = std::ranges::find_if(generator.variables, [&](const auto& var) { return var.name == term_ident->ident.value; });

                    if (variable == generator.variables.cend()) {
                        std::cerr << "Error: Variable " << term_ident->ident.value << " not declared" << std::endl;
                        exit(EXIT_FAILURE);
                    }

                    generator.push(
                        "QWORD [rsp + " + std::to_string((generator.stackPointer - variable->stackLocation - 1) * 8)
                        + "]");
                }

                void operator()(const Node::TermParen* term_paren) const {
                    generator.output << ";; TermParen\n";
                    generator.GenerateExp(term_paren->expr);
                }
            };

            TermVisitor visitor {.generator = *this};
            std::visit(visitor, term->term);
        }

        void GenerateIFPred(const Node::IFPred* pred, const std::string& end_label)
        {
            struct PredVisitor
            {
                Generator& generator;
                const std::string& end_label;
                void operator()(const Node::IFPredElseIf* elseif) const
                {
                    generator.output << ";; IFPREDELSEIF\n";
                    generator.GenerateExp(elseif->condition);
                    generator.pop("rax");

                    const std::string labelJmp = generator.CreateLabel();

                    generator.output << "    test rax, rax\n";
                    generator.output << "    jz " << labelJmp << "\n";
                    generator.GenerateScope(elseif->scope);
                    generator.output << "    jmp " << end_label << "\n";
                    if (elseif->pred.has_value())
                    {
                        generator.output << labelJmp << ":\n";
                        generator.GenerateIFPred(elseif->pred.value(), end_label);
                    }
                    else
                    {
                        generator.output << labelJmp << ":\n";
                    }
                }

                void operator()(const Node::IFPredElse* else_) const
                {
                    generator.output << ";; IFPREDELSE\n";
                    generator.GenerateScope(else_->scope);
                }
            };

            PredVisitor visitor {.generator = *this, .end_label = end_label};
            std::visit(visitor, pred->pred);
        }

        std::string GenerateVarDecl(const Node::StmtVar* var_decl) {
            struct VarVisitor {
                Generator& generator;
                std::string& var;
                void operator()(const Node::VarAssign* assign) const
                {
                    generator.output << ";; Assign\n";
                    const auto variable = std::ranges::find_if(generator.variables, [&](const auto& var) { return var.name == assign->ident.value; });
                    if (variable == generator.variables.end())
                    {
                        std::cerr << "Error: Variable " << assign->ident.value << " not declared" << std::endl;
                        exit(EXIT_FAILURE);
                    }

                    generator.GenerateExp(assign->expr);

                    generator.pop("rax");
                    generator.output << "    mov QWORD [rsp + " << (generator.stackPointer - variable->stackLocation - 1) * 8 << "], rax\n";

                    var = assign->ident.value;
                }
                void operator()(const Node::VarDecl* var_decl) const {
                    generator.output << ";; VarDecl\n";
                    const auto variable = std::ranges::find_if(generator.variables, [&](const auto& var) { return var.name == var_decl->ident.value; });
                    if (variable != generator.variables.cend())
                    {
                        std::cerr << "Error: Variable " << var_decl->ident.value << " already declared" << std::endl;
                        exit(EXIT_FAILURE);
                    }

                    generator.variables.push_back({
                        .name = var_decl->ident.value,
                        .stackLocation = generator.stackPointer,
                        .type = var_decl->ident.varType.value()
                    });

                    generator.GenerateExp(var_decl->expr);
                    var = var_decl->ident.value;
                }

                void operator()(const Node::FunVar* fun_var) const {
                    generator.output << ";; FunVar\n";
                    const auto variable = std::ranges::find_if(generator.variables, [&](const auto& var) { return var.name == fun_var->ident.value; });
                    if (variable != generator.variables.cend())
                    {
                        std::cerr << "Error: Variable " << fun_var->ident.value << " already declared" << std::endl;
                        exit(EXIT_FAILURE);
                    }

                    generator.variables.push_back({
                        .name = fun_var->ident.value,
                        .stackLocation = generator.stackPointer,
                        .type = fun_var->ident.varType.value()
                    });

                    var = fun_var->ident.value;
                }
            };

            std::string var;

            struct VarVisitor visitor {.generator = *this, .var = var};
            std::visit(visitor, var_decl->var);
            return visitor.var;
        }

        void GenerateLoop(const Node::Expr* exp, const Node::Scope* scope, const Node::Stmt* increment = nullptr) {
            const std::string labelJmp = CreateLabel();
            const std::string end_label = CreateLabel();

            output << labelJmp << ":\n";
            GenerateExp(exp);
            pop("rax");
            output << "    test rax, rax\n";
            output << "    jz " << end_label << "\n";
            GenerateScope(scope);
            if (increment != nullptr) {
                GenerateStmt(increment);
            }
            output << "    jmp " << labelJmp << "\n";
            output << end_label << ":\n";
        }

        void GenerateStmt(const Node::Stmt* stmt) 
        {
            struct StmtVisitor {
                Generator& generator;
                void operator()(const Node::StmtExit* exit) const
                {
                    generator.output << ";; Exit\n";
                    generator.GenerateExp(exit->exp);
                    generator.output << "    mov rax, 60\n";
                    generator.pop("rdi");
                    generator.output << "    syscall\n";
                }

                void operator()(const Node::StmtReturn* return_) const {
                    generator.output << ";; Return\n";
                    generator.GenerateExp(return_->expr);
                    generator.output << "    mov rsp, rbp\n";
                    generator.output << "    pop rbp\n";
                    generator.output << "    ret\n";
                }

                void operator()(const Node::Function* function) const {
                    generator.begin_scope();
                    generator.output << ";; Function\n";

                    const auto function_ = std::ranges::find_if(generator.functions, [&](const auto& f) { return f.name == function->name; });

                    if (function_ != generator.functions.cend()) {
                        std::cerr << "Error: Function " << function->name << " already declared" << std::endl;
                        exit(EXIT_FAILURE);
                    }

                    std::vector<Variable> args;

                    for (const auto& arg : function->args) {
                        args.push_back({
                            .name = arg->ident.value,
                            .stackLocation = generator.stackPointer,
                            .type = arg->ident.varType.value()
                        });

                        generator.stackPointer++;
                    }

                    generator.functions.push_back(Function {
                        .name = function->name,
                        .args = args,
                        .returnType = function->returnType,
                        .scope = function->scope
                    });

                    generator.output << function->name << ":\n";
                    generator.GenerateScope(function->scope);

                    generator.end_scope();
                }

                void operator()(const Node::FunCall* fun_call) const {
                    generator.output << ";; FunCall\n";
                    const auto function = std::ranges::find_if(generator.functions, [&](const auto& f) { return f.name == fun_call->name; });

                    if (function == generator.functions.cend()) {
                        std::cerr << "Error: Function " << fun_call->name << " not declared" << std::endl;
                        exit(EXIT_FAILURE);
                    }

                    if (fun_call->args.size() != function->args.size()) {
                        std::cerr << "Error: Function " << fun_call->name << " expected " << function->args.size() << " arguments, got " << fun_call->args.size() << std::endl;
                        exit(EXIT_FAILURE);
                    }

                    for (size_t i = 0; i < fun_call->args.size(); i++) {
                        generator.GenerateExp(fun_call->args[i]);
                        generator.pop("rax");
                        generator.output << "    mov QWORD [rsp + " << (generator.stackPointer + i) * 8 << "], rax\n";
                    }

                    generator.output << "    call " << fun_call->name << "\n";
                }

                void operator()(const Node::StmtWhile* while_) const {
                    generator.output << ";; While\n";

                    generator.GenerateLoop(while_->condition, while_->scope);
                }

                void operator()(const Node::StmtFor* for_) const {
                    generator.output << ";; For\n";
                    std::string var = generator.GenerateVarDecl(for_->init);

                    generator.GenerateLoop(for_->condition, for_->scope, for_->increment);

                    auto variable = std::ranges::find_if(generator.variables, [&](const auto& v) { return v.name == var; });
                    if (variable != generator.variables.cend())
                    {
                        generator.variables.pop_back();
                    }
                }

                void operator()(const Node::StmtVar* var_decl) const
                {
                    generator.GenerateVarDecl(var_decl);
                }

                void operator()(const Node::StmtPrint* print) const
                {
                    /*generator.output << ";; Print\n";

                    generator.GenerateExp(print->exp);

                    generator.output << "    mov ebx, 1\n";
                    generator.output << "    mov eax, 4\n";
                    generator.output << "    int 0x80\n";*/
                    assert(false);
                }

                void operator()(const Node::Scope* scope) const
                {
                    generator.output << ";; Scope\n";
                    generator.GenerateScope(scope);
                }

                void operator()(const Node::IF* if_statement) const
                {
                    generator.output << ";; IF\n";
                    generator.GenerateExp(if_statement->condition);
                    generator.pop("rax");

                    const std::string labelJmp = generator.CreateLabel();

                    generator.output << "    test rax, rax\n";
                    generator.output << "    jz " << labelJmp << "\n";
                    generator.GenerateScope(if_statement->scope);
                    if (if_statement->pred.has_value())
                    {
                        const std::string end_label = generator.CreateLabel();
                        generator.output << "    jmp " << end_label << "\n";
                        generator.output << labelJmp << ":\n";
                        generator.GenerateIFPred(if_statement->pred.value(), end_label);
                        generator.output << end_label << ":\n";
                    }
                    else
                    {
                        generator.output << labelJmp << ":\n";
                    }
                }
            };

            StmtVisitor visitor {.generator = *this};
            std::visit(visitor, stmt->stmt); 
        }

        void GenerateScope(const Node::Scope* scope) {
            begin_scope();

            for(const Node::Stmt* stmt : scope->stmts) {
                GenerateStmt(stmt);
            }

            end_scope();
        }

        std::string GenerateProg() {
            output << "%macro print 1 ; macro with one argument\n push dword %1 ; %1 means first argument\ncall printf\nadd  esp, 4\n%endmacro\n";
            output << "section .bss\n";
            output << "section .text\n";
            output << "    global _start\n    extern printf\n_start:\n";

            for(const Node::Stmt* stmt : root->stmts) {
                GenerateStmt(stmt);
            }

            output << "    mov rax, 60\n";
            output << "    xor rdi, rdi\n";
            output << "    syscall\n";

            output << "section .data\n";
            output << "    format_int: db \"%d\", 0\n";
            output << "    format_string: db \"%s\", 0\n";
            output << "    format_float: db \"%f\", 0\n";
            for(const auto& str : strings) {
                output << "    " << str.label << ": db " << str.value << ", 0xA\n";
                output << "    " << str.label << "Len: equ $-" << str.label << "\n";
            }

            return output.str();
        }

        std::string CreateLabel() {
            std::stringstream ss;
            ss << "label" << labelCounter++;
            return ss.str();
        }
    private:
        void push(const std::string& reg) {
            output << "    push " << reg << "\n";
            stackPointer++;
        }

        void pop(const std::string& reg) {
            output << "    pop " << reg << "\n";
            stackPointer--;
        }

        void begin_scope()
        {
            scopes.push_back(variables.size());
        }

        void end_scope()
        {
            const size_t popCount = variables.size() - scopes.back();

            output << "    add rsp, " << popCount * 8 << "\n";
            stackPointer -= popCount;

            for (size_t i = 0; i < popCount; i++)
            {
                variables.pop_back();
            }

            scopes.pop_back();
        }

        struct Variable {
            std::string name;
            size_t stackLocation;
            Tokens::TYPENAME::Types type;
        };

        struct Strings {
            std::string value;
            std::string label;
        };

        struct Function {
            std::string name;
            std::vector<Variable> args;
            std::string returnType;
            Node::Scope* scope;
        };

        std::vector<Variable> variables {};
        std::vector<size_t> scopes {};
        std::vector<Strings> strings {};
        std::vector<Function> functions {};

        Node::Prog* root;
        std::stringstream output;
        size_t stackPointer = 0;
        int labelCounter = 0;
        int stringCounter = 0;
};