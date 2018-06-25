# Surjective

[![Build Status](https://travis-ci.org/gelisam/surjective.svg?branch=master)](https://travis-ci.org/gelisam/surjective)

Here is a parsing function which is missing a case:

    parseBool :: String -> Maybe Bool
    parseBool = \case
      "true" -> Just True
      _      -> Nothing

And here is how to annotate the function using `surjective` so the compiler
warns us about that missing case:

    -- Warning: Pattern match(es) are non-exhaustive
    -- In a case alternative: Patterns not matched: (Just False)
    parseBool :: String -> Maybe Bool
    parseBool = $$(surjective
      [||\covers -> \case
        "true" -> covers $ \(Just True) -> Just True
        _      -> covers $ \Nothing     -> Nothing
      ||])

Since the check is entirely syntactic, `surjective` can be used to check other
kinds of coverage conditions, not just surjectivity. For example, here we
attempt to list all the values of type `Maybe Bool`, but we are missing a case:

    listMaybeBools :: [Maybe Bool]
    listMaybeBools = [Just True, Nothing]

Here is how to annotate the list using `surjective` so the compiler warns us
about that missing case:

    -- Warning: Pattern match(es) are non-exhaustive
    -- In a case alternative: Patterns not matched: (Just False)
    listMaybeBools :: [Maybe Bool]
    listMaybeBools = $$(surjective
      [||\covers -> [ covers $ \(Just True) -> Just True
                    , covers $ \Nothing     -> Nothing
                    ]
      ||])


## Alternatives

*   The [exhaustive](http://hackage.haskell.org/package/exhaustive) package uses a
    different approach. Instead of a syntactic check, it provides you with
    higher-order functions which are guaranteed to cover all the constructors
    of your datatype because the type of those higher-order functions expects
    one input function per constructor.

    Compared to `surjective`, `exhaustive`:

    *   Accepts fewer incorrect programs. A higher-order function from the
        `exhaustive` package tells you exactly what it will do with the
        constructor-specific functions you are giving it, so the guarantee that
        you are indeed giving it one function per constructor has a very
        precise meaning. `surjective`'s syntactic check, on the other hand,
        only makes sure that your program mentions `covers` with patterns which
        cover all the cases, which is only indirectly related to surjectivity.
    *   Accepts fewer correct programs. If `exhaustive` doesn't have a
        higher-order function corresponding to what you want to do, you're out
        of luck. With `surjective`, you can write your own function and then
        annotate it with calls to `covers`.
    *   Fails with a type error instead of a warning.
    *   Doesn't support nested patterns such as `Just (Left _)`. With
        `exhaustive`, you would have to use one higher-order function for the
        `Maybe`, and then in the `Just` case, use another higher-order function
        for the `Either`. With `surjective`, you don't have to nest the two
        `Either` cases inside the `Just` case, you could handle `Just (Left _)`,
        then `Nothing`, then `Just (Right _)`.

    Both use Template Haskell.
