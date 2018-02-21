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
