-module(data_long_internal@foreign).

-export([ numberBitsToInt/1
        , '_safeReadLong'/0
        , isWholeNumber/1
        ]
  ).

numberBitsToInt(A) when A >= 16#7fffffff -> <<X:32/signed-integer>> = <<(trunc(A)):32/unsigned-integer>>, X;
numberBitsToInt(A) when A >= 0 -> trunc(A);
numberBitsToInt(A) when A < 0 -> (((trunc(A) * -1) band 16#ffffffff) * -1).

'_safeReadLong'() ->
  fun (S, IsUnsigned, Radix) ->
    try
      Int = binary_to_integer(S, Radix),
      if IsUnsigned andalso (Int < 0) ->
          null;
         IsUnsigned andalso (Int > 16#ffffffffffffffff) ->
          null;
         (not IsUnsigned) andalso (Int > 16#7fffffffffffffff) ->
          null;
         (not IsUnsigned) andalso (Int < -16#8000000000000000) ->
          null;
         true ->
          Int
      end
    catch
      error:badarg -> null
    end
  end.

isWholeNumber(A) -> A == trunc(A).
