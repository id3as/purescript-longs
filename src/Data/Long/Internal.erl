-module(data_long_Internal@foreign).

-export([ numberBitsToInt/1
        , '_safeReadLong'/3
        , isWholeNumber/1
        ]
  ).

numberBitsToInt(A) -> A.

'_safeReadLong'(S, _IsUnsigned, Radix) ->
  try
    binary_to_integer (S, Radix)
  catch
    error:badarg -> null
  end.

isWholeNumber(A) -> A == trunc(A).
