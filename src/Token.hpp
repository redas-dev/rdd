#pragma once

#include <map>
#include <optional>
#include <string>
#include <regex>
#include <utility>

namespace Tokens {
    class Token{
        public:
            Token() = default;
            [[nodiscard]] virtual std::regex regex() const = 0;
            virtual ~Token() = default;
    };

    class WHITESPACE final: public Token {
        public:
        WHITESPACE() = default;
        [[nodiscard]] std::regex regex() const override {
            return std::regex("^[\\s|\t|\n|\r]");
        }
        
        bool operator==(const Token* other){
            return typeid(*this) == typeid(*other);
        }

        bool operator !=(const Token* other){
            return typeid(*this) != typeid(*other);
        }
    };

    class KEYWORD final: public Token {
        public:
        explicit KEYWORD(std::string  value): value(std::move(value)) {}
        explicit KEYWORD() = default;
        std::string value{};

        enum Keywords {
            VOID,
            IF,
            ELSE,
            ELSE_IF,
            WHILE,
            FROM,
            TO,
            RETURN,
            BREAK,
            PRINT,
            EXIT,
        };

        [[nodiscard]]std::regex regex() const override {
            return std::regex("^(void|if|else if|else|while|from|to|return|break|print|exit|by|fun)");
        }

        bool operator ==(const Token* other){
            return typeid(*this) == typeid(*other);
        }
    };

    class TYPENAME final: public Token {
        public:
        explicit TYPENAME(std::string value): value(std::move(value)) {}
        explicit TYPENAME() = default;
        std::string value{};

        enum Types {
            INT,
            FLOAT,
            CHAR,
            BOOL,
            STRING,
            VOID
        };

        static std::optional<Types> GetType(const std::string& str) {
            if (str == "int")
                return INT;
            if (str == "float")
                return FLOAT;
            if (str == "char")
                return CHAR;
            if (str == "bool")
                return BOOL;
            if (str == "string")
                return STRING;

            return {};
        }

        [[nodiscard]] std::regex regex() const override {
            return std::regex("^(int|float|char|string|bool)");
        }

        bool operator==(const Token* other) {
            return typeid(*this) == typeid(*other);
        }
    };

    class DELIMITER final: public Token {
        public:
        
        explicit DELIMITER() = default;
        explicit DELIMITER(std::string value): value(std::move(value)) {}
        std::string value{};

        [[nodiscard]] std::regex regex() const override {
            return std::regex(R"(^(;|\(|\)|\{|\}))");
        }
        
        bool operator==(const Token* other) {
            return typeid(*this) == typeid(*other);
        }
    };

    class LOGICAL final: public Token {
        public:
        [[nodiscard]] std::regex regex() const override {
            return std::regex("^true|false");
        }
        
        bool operator==(const Token* other) {
            return typeid(*this) == typeid(*other);
        }
    };

    class NUMBER final: public Token {
        public:
        [[nodiscard]] std::regex regex() const override {
            return std::regex("^[0-9]+");
        }

        bool operator==(const Token* other) {
            return typeid(*this) == typeid(*other);
        }
    };

    class FLOAT final: public Token {
        public:
        [[nodiscard]] std::regex regex() const override {
            return std::regex("^[0-9]+\\.[0-9]+");
        }

        bool operator==(const Token* other) {
            return typeid(*this) == typeid(*other);
        }
    };

    class STRING final: public Token {
        public:
        [[nodiscard]] std::regex regex() const override {
            return std::regex("^\"([^\"]*)\"");
        }

        bool operator==(const Token* other) {
            return typeid(*this) == typeid(*other);
        }
    };

    class IDENTIFIER final: public Token {
        public:
        [[nodiscard]] std::regex regex() const override {
            return std::regex("^[a-zA-Z_][a-zA-Z0-9_]*");
        }

        bool operator==(const Token* other) {
            return typeid(*this) == typeid(*other);
        }
    };

    class OPERATOR final: public Token {
        public:
        explicit OPERATOR(std::string value): value(std::move(value)) {}
        explicit OPERATOR() = default;
        std::string value{};
        [[nodiscard]] std::regex regex() const override {
            return std::regex(R"(^(\+|-|\*|\/|={1,2}|:|<=|>=|<|>|\|{2}|&{2}|!=))");
        }

        bool operator==(const Token* other) {
            return typeid(*this) == typeid(*other);
        }
    };

    class COMMENT final: public Token {
        public:
        [[nodiscard]] std::regex regex() const override {
            return std::regex(R"(^(//.*)|^(/\*[\s\S]*\*/)|^(/\*[\s\S]*))");
        }

        bool operator==(const Token* other) {
            return typeid(*this) == typeid(*other);
        }
    };

    inline std::map<Token*, std::regex> TokenTypeRegex = {
        {new WHITESPACE(), WHITESPACE().regex()},
        {new KEYWORD(), KEYWORD().regex()},
        {new TYPENAME(), TYPENAME().regex()},
        {new DELIMITER(), DELIMITER().regex()},
        {new LOGICAL(), LOGICAL().regex()},
        {new NUMBER(), NUMBER().regex()},
        {new FLOAT(), FLOAT().regex()},
        {new COMMENT(), COMMENT().regex()},
        {new STRING(), STRING().regex()},
        {new IDENTIFIER(), IDENTIFIER().regex()},
        {new OPERATOR(), OPERATOR().regex()},
    };
}

class Token {
public:
    Tokens::Token* type;
    std::optional<Tokens::TYPENAME::Types> varType;
    std::string value;
    int lineNum;

    explicit Token(Tokens::Token* type, std::string value, const int lineNum, const auto varType) : type(type), varType(varType), value(std::move(value)), lineNum(lineNum) {}
    explicit Token(Tokens::Token* type, std::string value, const int lineNum) : type(type), value(std::move(value)), lineNum(lineNum) {}

    ~Token() = default;
};