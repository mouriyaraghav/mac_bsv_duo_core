# MAC Design and verification

## Verification

- The testing of the MAC module designed will take a lot of effort and time to get a full functional coverage given the large amount of input space it has.
- So, it’s not possible to actually iterate through all the possible inputs and verify the functionality. So the coverage is to be defined appropriately.
- To account for all the possible inputs and to attain functional coverage, I test the multiplication part of the MAC exhaustively ensuring it covers all the values for $A, B, A \times B$.
- So, to account for C which is of 32-bits, comes the issue. Here, we define bins in the range of possible values for C and we say if at least one value is hit in this bin, it means all the values in the bin are covered with some precision of bits.
- If the bin size is let’s say 64, then it means that if one value in the bin is verified as on input, it’s considered all the values are hit with a bit precision of 6.
- Also, the corner cases are exclusively handled separately to ensure proper working apart from this CRV (constrained random vector generation) of inputs.
- This way the coverage definition is done.
- As for the python reference model is concerned, the model is defined as a expression for the integer MAC and is defined using `tensorflow` module in python which performs bfloat16 and float32 operations.
