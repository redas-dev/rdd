$$
    \begin{align}
        [\text{Prog}] &\to [\text{Stmt}]^* \\
        [\text{Stmt}] &\to
        \begin{cases}
            \text{exit}([\text{Exp}]);\\
            \text{if} ([\text{Exp}])[\text{Scope}]\text{[IfPred]}\\
            [\text{Scope}];\\
            \text{Print([Exp]);}\\
            \text{from [Ident]; to [Exp][Scope]}\\
            \text{while [Exp][Scope]}\\
            \text{fun \{name\}}(\text{[Ident]}^*[,]...) \text{: type}\space\text{[Scope]}\\
            \text{return [Exp];}
        \end{cases}\\
        \text{[Ident]} &\to
        \begin{cases}
            \text{ident: type} = [\text{Exp}];\\
            \text{ident} = \text{[Exp]};\\
        \end{cases}\\
        \text{type} &\to
        \begin{cases}
            \text{int}\\
            \text{string}\\
            \text{float}\\
        \end{cases}\\
        \text{[Scope]} &\to \{[\text{Stmt}]^*\} \\
        \text{[IfPred]} &\to
        \begin{cases}
            \text{else if}(\text{[Exp]})\text{[Scope]}\text{[IfPred]} \\
            \text{else}\text{[Scope]} \\
            \epsilon
        \end{cases} \\
        [\text{Exp}] &\to
        \begin{cases}
            [\text{Term}] \\
            [\text{BinExp}]\\
            [\text{CompExp}]\\
        \end{cases} \\
        [\text{BinExp}] &\to
        \begin{cases}
            [\text{Exp}] * [\text{Exp}] & \text{prec} = 2 \\
            [\text{Exp}] / [\text{Exp}] & \text{prec} = 2 \\
            [\text{Exp}] + [\text{Exp}] & \text{prec} = 1 \\
            [\text{Exp}] - [\text{Exp}] & \text{prec} = 1 \\
        \end{cases} \\
        \text{[CompExp]} &\to
        \begin{cases}
            \text{[Exp] == [Exp];} & \text{prec} = 0\\
            \text{[Exp] != [Exp];} & \text{prec} = 0\\
            \text{[Exp] < [Exp];} & \text{prec} = 0\\
            \text{[Exp] > [Exp];} & \text{prec} = 0\\
            \text{[Exp] <= [Exp];} & \text{prec} = 0\\
            \text{[Exp] >= [Exp];} & \text{prec} = 0\\
            \text{[CompExp] \&\& [CompExp]} & \text{prec} = 0\\
            \text{[CompExp] || [CompExp]} & \text{prec} = 0\\
        \end{cases}\\
        [\text{Term}] &\to
        \begin{cases}
            \text{stringLit}\\
            \text{intLit} \\
            \text{ident} \\
            ([\text{Exp}])
        \end{cases}
    \end{align}
$$
