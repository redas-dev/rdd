#pragma once

#include <cassert>
#include <vector>
#include <sstream>
#include <algorithm>
#include <iostream>
#include <string>
#include <optional>

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
                void operator()(const Node::BinaryExpression* binExp) const {
                    // For logical operators, use short-circuit evaluation
                    if (binExp->sign == "&&") {
                        const std::string false_label = generator.CreateLabel();
                        const std::string end_label = generator.CreateLabel();

                        generator.GenerateExp(binExp->lhs);
                        generator.pop("rax");
                        generator.output << "    test rax, rax\n";
                        generator.output << "    je " << false_label << "\n";
                        
                        generator.GenerateExp(binExp->rhs);
                        generator.pop("rax");
                        generator.output << "    test rax, rax\n";
                        generator.output << "    je " << false_label << "\n";
                        
                        generator.output << "    mov rax, 1\n";
                        generator.output << "    jmp " << end_label << "\n";
                        generator.output << false_label << ":\n";
                        generator.output << "    mov rax, 0\n";
                        generator.output << end_label << ":\n";
                        generator.push("rax");
                        return;
                    } 
                    else if (binExp->sign == "||") {
                        const std::string true_label = generator.CreateLabel();
                        const std::string end_label = generator.CreateLabel();

                        generator.GenerateExp(binExp->lhs);
                        generator.pop("rax");
                        generator.output << "    test rax, rax\n";
                        generator.output << "    jne " << true_label << "\n";
                        
                        generator.GenerateExp(binExp->rhs);
                        generator.pop("rax");
                        generator.output << "    test rax, rax\n";
                        generator.output << "    jne " << true_label << "\n";
                        
                        generator.output << "    mov rax, 0\n";
                        generator.output << "    jmp " << end_label << "\n";
                        generator.output << true_label << ":\n";
                        generator.output << "    mov rax, 1\n";
                        generator.output << end_label << ":\n";
                        generator.push("rax");
                        return;
                    }

                    // For non-logical operators
                    generator.GenerateExp(binExp->lhs);
                    generator.GenerateExp(binExp->rhs);
                    generator.pop("rbx");  // rhs in rbx
                    generator.pop("rax");  // lhs in rax

                    if (binExp->sign == "==") {
                        generator.output << "    cmp rax, rbx\n";
                        generator.output << "    sete al\n";
                        generator.output << "    movzx rax, al\n";
                    } else if (binExp->sign == "!=") {
                        generator.output << "    cmp rax, rbx\n";
                        generator.output << "    setne al\n";
                        generator.output << "    movzx rax, al\n";
                    } else if (binExp->sign == ">") {
                        generator.output << "    cmp rax, rbx\n";
                        generator.output << "    setg al\n";
                        generator.output << "    movzx rax, al\n";
                    } else if (binExp->sign == ">=") {
                        generator.output << "    cmp rax, rbx\n";
                        generator.output << "    setge al\n";
                        generator.output << "    movzx rax, al\n";
                    } else if (binExp->sign == "<") {
                        generator.output << "    cmp rax, rbx\n";
                        generator.output << "    setl al\n";
                        generator.output << "    movzx rax, al\n";
                    } else if (binExp->sign == "<=") {
                        generator.output << "    cmp rax, rbx\n";
                        generator.output << "    setle al\n";
                        generator.output << "    movzx rax, al\n";
                    } else if (binExp->sign == "&") {
                        generator.output << "    and rax, rbx\n";
                    } else if (binExp->sign == "|") {
                        generator.output << "    or rax, rbx\n";
                    } else if (binExp->sign == "+") {
                        generator.output << "    add rax, rbx\n";
                    } else if (binExp->sign == "*") {
                        generator.output << "    imul rax, rbx\n";
                    } else if (binExp->sign == "/") {
                        generator.output << "    cqo\n";
                        generator.output << "    idiv rbx\n";
                    } else if (binExp->sign == "-") {
                        generator.output << "    sub rax, rbx\n";
                    } else {
                        std::cerr << "Error: Unknown binary operator: " << binExp->sign << std::endl;
                        exit(EXIT_FAILURE);
                    }

                    generator.push("rax");
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
                    std::string label = generator.CreateStringLabel();
                    generator.stringLiterals.push_back({label, term_string_lit->string_lit.value});
                    generator.output << "    lea rax, [" << label << "]\n";
                    generator.push("rax");
                }

                void operator()(const Node::TermIdent* term_ident) const {
                    auto variable = std::find_if(
                        generator.variables.cbegin(),
                        generator.variables.cend(),
                        [&term_ident](const Generator::Variable& var) {
                            return var.name == term_ident->ident.value;
                        }
                    );

                    if (variable == generator.variables.cend()) {
                        std::cerr << "Error: Variable " << term_ident->ident.value << " not found" << std::endl;
                        exit(EXIT_FAILURE);
                    }

                    size_t offset = (generator.stackPointer - variable->stackLocation - 1) * 8;
                    generator.output << "    ; Accessing global variable " << variable->name << " at [rsp+" << offset << "]\n";
                    generator.push("QWORD [rsp + " + std::to_string(offset) + "]");
                    
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
                    assert(false); // TODO
                }
            };

            std::string var;

            VarVisitor visitor {.generator = *this, .var = var};
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
                    assert(false); // TODO
                }

                void operator()(const Node::Function* function) const {
                    assert(false); // TODO
                }

                void operator()(const Node::FunCall* fun_call) const {
                    assert(false); // TODO
                }

                void operator()(const Node::StmtWhile* while_) const {
                    generator.output << ";; While\n";
                    generator.GenerateLoop(while_->condition, while_->scope);
                }

                void operator()(const Node::StmtFor* for_) const {
                    generator.output << ";; For\n";
                    std::string var = generator.GenerateVarDecl(for_->init);
                    generator.GenerateLoop(for_->condition, for_->scope, for_->increment);
                    
                    // Clean up the variable
                    auto variable = std::ranges::find_if(generator.variables, [&](const auto& v) { 
                        return v.name == var; 
                    });
                    if (variable != generator.variables.cend()) {
                        generator.variables.pop_back();
                    }
                }

                void operator()(const Node::StmtVar* var_decl) const
                {
                    generator.GenerateVarDecl(var_decl);
                }

                void operator()(const Node::StmtPrint* print) const
                {
                    generator.output << ";; StmtPrint\n";
                    generator.GenerateExp(print->exp);
                    generator.pop("rax");  // Value to print is now in rax

                    std::string baseLabel = generator.CreateLabel();
                    
                    // Try to determine if we're printing a string or an integer
                    // This is a simple heuristic - a better solution would track types through the AST
                    generator.output << "    ; Check if we're printing a string or integer\n";
                    generator.output << "    ; Strings typically have high addresses, numbers are typically small\n";
                    generator.output << "    cmp rax, 0x1000000\n";
                    generator.output << "    jae .print_as_string_" << baseLabel << "\n";
                    
                    // Print as integer
                    generator.output << "    ; Print as integer\n";
                    generator.output << "    mov rdi, rax\n"; // Save the value to print
                    
                    // Check if negative
                    generator.output << "    cmp rdi, 0\n";
                    generator.output << "    jge .print_positive_" << baseLabel << "\n";
                    generator.output << "    mov byte [print_buffer], '-'\n";
                    generator.output << "    mov rsi, 1\n"; // Number of characters written (the '-')
                    generator.output << "    neg rdi\n"; // Make rdi positive
                    generator.output << "    jmp .print_convert_" << baseLabel << "\n";

                    generator.output << ".print_positive_" << baseLabel << ":\n";
                    generator.output << "    mov rsi, 0\n"; // No '-' sign

                    generator.output << ".print_convert_" << baseLabel << ":\n";
                    generator.output << "    mov rax, rdi\n"; // rax = number
                    generator.output << "    mov rcx, 10\n"; // divisor
                    generator.output << "    xor rbx, rbx\n"; // counter for digits

                    generator.output << ".print_loop_" << baseLabel << ":\n";
                    generator.output << "    xor rdx, rdx\n";
                    generator.output << "    div rcx\n"; // rax = quotient, rdx = remainder
                    generator.output << "    add dl, '0'\n"; // convert to ASCII
                    generator.output << "    push rdx\n"; // push the digit
                    generator.output << "    inc rbx\n"; // increment digit count
                    generator.output << "    test rax, rax\n";
                    generator.output << "    jnz .print_loop_" << baseLabel << "\n";

                    // Now, pop digits into the buffer
                    generator.output << "    mov rcx, rbx\n"; // number of digits
                    generator.output << "    lea rdi, [print_buffer + rsi]\n"; // buffer position after '-' if any

                    generator.output << ".print_store_" << baseLabel << ":\n";
                    generator.output << "    pop rax\n"; // get the digit
                    generator.output << "    mov [rdi], al\n"; // store the byte
                    generator.output << "    inc rdi\n";
                    generator.output << "    loop .print_store_" << baseLabel << "\n";

                    // Calculate total length: rbx (digits) + rsi (sign)
                    generator.output << "    add rbx, rsi\n";
                    generator.output << "    mov rdx, rbx\n"; // length

                    // sys_write syscall
                    generator.output << "    mov rax, 1\n"; // syscall 1: write
                    generator.output << "    mov rdi, 1\n"; // fd 1 (stdout)
                    generator.output << "    lea rsi, [print_buffer]\n";
                    generator.output << "    syscall\n";
                    
                    // Print newline and return
                    generator.output << "    jmp .print_newline_" << baseLabel << "\n";
                    
                    // String printing logic
                    generator.output << ".print_as_string_" << baseLabel << ":\n";
                    generator.output << "    ; Print as string\n";
                    generator.output << "    mov rsi, rax\n";  // String pointer is in rax
                    generator.output << "    mov rdx, 0\n";    // Initialize length counter

                    // Safely calculate string length with a maximum to prevent potential issues
                    generator.output << "    mov rcx, 1024\n"; // Maximum string length to check
                    generator.output << ".strlen_loop_" << baseLabel << ":\n";
                    generator.output << "    cmp byte [rsi + rdx], 0\n";
                    generator.output << "    je .strlen_done_" << baseLabel << "\n";
                    generator.output << "    inc rdx\n";
                    generator.output << "    dec rcx\n";
                    generator.output << "    jz .strlen_done_" << baseLabel << "\n"; // Safety check
                    generator.output << "    jmp .strlen_loop_" << baseLabel << "\n";

                    generator.output << ".strlen_done_" << baseLabel << ":\n";
                    generator.output << "    test rdx, rdx\n";
                    generator.output << "    jz .print_newline_" << baseLabel << "\n"; // Skip empty string
                    generator.output << "    mov rax, 1\n";    // syscall: write
                    generator.output << "    mov rdi, 1\n";    // file descriptor: stdout
                    generator.output << "    syscall\n";
                    
                    // Print newline for both integer and string
                    generator.output << ".print_newline_" << baseLabel << ":\n";
                    generator.output << "    mov rax, 1\n";
                    generator.output << "    mov rdi, 1\n";
                    generator.output << "    lea rsi, [newline]\n";
                    generator.output << "    mov rdx, 1\n";
                    generator.output << "    syscall\n";
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
            output << "section .data\n";
            output << "newline db 0xA\n";
            
            // Generate string literals
            for (const auto& strLit : stringLiterals) {
                output << strLit.label << " db \"" << strLit.value << "\", 0\n";
            }
            
            output << "section .bss\n";
            output << "print_buffer resb 32\n"; // Buffer for integer printing (increased size)
            
            output << "section .text\n";
            output << "    global _start\n_start:\n";
            
            for (const Node::Stmt* stmt : root->stmts) {
                GenerateStmt(stmt);
            }
            
            // Add default exit
            output << "    mov rax, 60\n";
            output << "    xor rdi, rdi\n";
            output << "    syscall\n";
            
            return output.str();
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

        struct StringLiteral {
            std::string label;
            std::string value;
        };

        std::vector<Variable> variables {};
        std::vector<size_t> scopes {};
        std::vector<StringLiteral> stringLiterals;

        Node::Prog* root;
        std::stringstream output;
        size_t stackPointer = 0;
        int labelCounter = 0;
        int stringCounter = 0;

        std::string CreateLabel() {
            std::stringstream ss;
            ss << "label" << labelCounter++;
            return ss.str();
        }
    
        std::string CreateStringLabel() {
            std::stringstream ss;
            ss << "L.str." << stringCounter++;
            return ss.str();
        }
};