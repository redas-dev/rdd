#pragma once

#include <string>
#include <utility>
#include <vector>
#include <regex>

#include "Token.hpp"

class Tokenizer {
public:
    explicit Tokenizer(std::string source) : sourceCode(std::move(source)) {}
    int lineNum = 1;
    unsigned long Next(const unsigned long position)
    {
        const std::string nextToken = this->sourceCode.substr(position);
        for(auto&[fst, snd] : Tokens::TokenTypeRegex) {
            if (std::smatch match; std::regex_search(nextToken, match, snd)) {
                //std::cout << match.str() << std::endl;
                if (Tokens::WHITESPACE() != fst && Tokens::COMMENT() != fst) {
                    tokens.emplace_back(fst, match.str(), lineNum, Tokens::TYPENAME::GetType(match.str()));
                }
                if (Tokens::WHITESPACE() == fst && match.str() == "\n") {
                    lineNum++;
                }

                return match.str().length();
            }
        }

        throw std::runtime_error("Error: Invalid token at position " + std::to_string(position));
    }

    std::vector<Token> tokenize()
    {
        unsigned long pos = 0;
        while (pos < sourceCode.length()) {
            pos += Next(pos);
        }

        return tokens;
    }

    ~Tokenizer() = default;

private:
    std::vector<Token> tokens;
    std::string sourceCode;
};