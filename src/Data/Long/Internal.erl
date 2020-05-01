-module(data_long_internal@foreign).

-export([ numberBitsToInt/1
        , '_safeReadLong'/0
        , isWholeNumber/1
        ]
  ).

numberBitsToInt(A) -> A.

'_safeReadLong'() ->
  fun (S, _IsUnsigned, Radix) ->
    try
      binary_to_integer (S, Radix)
    catch
      error:badarg -> null
    end
  end.

isWholeNumber(A) -> A == trunc(A).
