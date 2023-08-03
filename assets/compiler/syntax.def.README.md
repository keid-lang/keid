# Keid Syntax DSL

This is the official documentation for the Keid Syntax Definition Language (KSDL).

## Rule Definitions

```ksdl
rule [RuleName]
    Statement1
    Statement2
    ...
end
```

## Statements

### Text Matching

```ksdl
[$GroupName = ] [optional | repeated] <RuleName | "StringLiteral" | 'Char' | 'CharStart-CharEnd'>
```

### Moving

When a match fails within a rule, the default behavior is to stop parsing the rule. The parser state reverts back to what it was before it began parsing the rule, and the parent rule attempts to continue parsing other children rules when possible.

The `move` statement prevents this behavior from happening. After a rule has reached a `move` statement, any failures to parse further statements within that rule will cause an immediate parsing error.

Assume an example KSDL schema written as such:

```ksdl
rule Identifier
    repeated 'A-Z' | 'a-z' | '0-9'
end

rule ClassDecl
    "class"
    $name = Identifier
}

rule Statement
    $st = ClassDecl | FooDecl | BarDecl | ...
end
```

If our text to parse was 'class Foo$Bar', the `Identifier` rule will fail because '$' is not an acceptable character. As a result, the `ClassDecl` rule will revert its state back to the start and allow the `Statement` rule to continue parsing, attempting `FooDecl` next.

If we used a `move` statement like this:

```ksdl
rule ClassDecl
    "class"
    move
    $name = Identifier
end
```

After the `class` literal is reached, `move` is excuted, and the failure to parse the identifier will cause a parsing error immediately, and `FooDecl` will never be evaluted.

### Catchall
