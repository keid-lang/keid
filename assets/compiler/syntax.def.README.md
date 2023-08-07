# Keid Syntax DSL

This is the official documentation for the Keid Syntax Definition Language (KSDL).

## Rule Definitions

```ksdl
RULE [RuleName]
    Statement1
    Statement2
    ...
END RULE
```

## Statements

### LITERAL and RANGE

`LITERAL foo` expects the literal identifier `foo` to immediately follow. Special literals `NEWLINE`, `COMMA`, and `SPACE` exist.

`RANGE A-Z` expects one literal character within the range [A-Z] inclusively to immediately follow.

### OPTIONAL and REPEATED

`REPEATED ...` matches the following statement one or more times.

`OPTIONAL ...` matches the following statement zero or one time.

`OPTIONAL REPEATED ...` matches the following statement zero or more times.

### OR

`OR(STATEMENT A, STATEMENT B, ...)` matches only the first matching statement within the `OR`.

### COMPOUND

`COMPOUND(STATEMENT A, STATEMENT B, ...)` matches two values consecutively. This is often useful in conjunction with `OPTIONAL`, `REPEATED`, etc. By default, `COMPOUND` statements return no matching result, and thus cannot be used at the right-hand side of a matching operation.

`COMPOUND(STATEMENT A, STATEMENT B, ...) SELECT N` performs a normal compound match, but yields the matched value of the `Nth` inner `STATEMENT`, with `STATEMENT A` being `N = 1`, `STATEMENT B` being `N = 2`, etc.

### Group

`$varname = STATEMENT` stores the result from `STATEMENT` into the associated group `$varname`. Repeatedly storing statements into the same group name is an appending (i.e. not overwriting) operation. All groups can hold multiple match results.

### Move

When a match fails within a rule, the default behavior is to stop parsing the rule. The parser state reverts back to what it was before it began parsing the rule, and the parent rule attempts to continue parsing other children rules when possible.

The `MOVE` statement prevents this behavior from happening. After a rule has reached a `MOVE` statement, any failures to parse further statements within that rule will cause an immediate parsing error.

Assume an example KSDL schema written as such:

```ksdl
RULE Identifier
    REPEATED OR(RANGE A-Z, RANGE a-z)
END RULE

RULE ClassDecl
    LITERAL class
    $name = RULE Identifier
END RULE

RULE Statement
    $st = OR(RULE ClassDecl, RULE FooDecl, RULE BarDecl, ...)
END RULE
```

If our text to parse was 'class Foo$Bar', the `Identifier` rule will fail because '$' is not an acceptable character. As a result, the `ClassDecl` rule will revert its state back to the start and allow the `Statement` rule to continue parsing, attempting `FooDecl` next.

If we used a `move` statement like this:

```ksdl
RULE ClassDecl
    LITERAL class
    MOVE
    $name = RULE Identifier
END RULE
```

After the `class` literal is reached, `MOVE` is excuted, and the failure to parse the identifier will cause a parsing error immediately, and `FooDecl` will never be evaluted.
