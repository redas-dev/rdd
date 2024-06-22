#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

#include "Token.hpp"
#include "Tokenizer.hpp"
#include "SyntaxAnalyzer.hpp"
#include "Generator.hpp"

int main(int argc, char *argv[])
{
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <source file>" << std::endl;
        return EXIT_FAILURE;
    }

    std::fstream file(argv[1], std::ios::in);

    if (!file.is_open()) {
        std::cerr << "Error: Could not open file " << argv[1] << std::endl;
        return EXIT_FAILURE;
    }

    std::string sourceCode;
    std::stringstream ss;

    ss << file.rdbuf();
    sourceCode = ss.str();

    auto tokenizer = Tokenizer(sourceCode);
    std::vector<Token> tokens = tokenizer.tokenize();

    auto syntaxAnalyzer = SyntaxAnalyzer(tokens);
    std::optional<Node::Prog*> root = syntaxAnalyzer.ParseProg();

    if (!root.has_value()) {
        std::cerr << "Error: Could not parse source code" << std::endl;
        return EXIT_FAILURE;
    }

    //std::cout << prog << std::endl;

    {
        auto generator = Generator(root.value());
        std::string prog = generator.GenerateProg();
        std::ofstream output("output.asm");
        output << prog;
    }

    system("nasm -felf64 output.asm -o output.o");
    system("gcc output.o -o output -no-pie -nostartfiles -lc");

    return EXIT_SUCCESS;
}