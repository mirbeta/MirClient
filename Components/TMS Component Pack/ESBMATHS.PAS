unit ESBMaths; {for TMS Software}

{:
 ESBMaths for TMS Software 4.0.0 - contains useful Mathematical routines for Delphi 4 and above.

 Copyright ©1997-2012 ESB Consultancy<p>

 These routines are used by ESB Consultancy within the
 development of their Customised Applications, and have been
 under Development since the early Turbo Pascal days. Many of the routines were developed
 for specific needs.<p>

 ESB Consultancy retains full copyright and allows TMS Software to includ as they see fit.<p>

 ESB Consultancy grants users of this code royalty free rights
 to do with this code as they wish.<p>

 ESB Consultancy makes no guarantees nor excepts any liabilities
 due to the use of these routines<p>

 We does ask that if this code helps you in you development
 that you send as an email mailto:support@esbconsult.com or even
 a local postcard. It would also be nice if you gave us a
 mention in your About Box or Help File.<p>

 ESB Consultancy Home Page: http://www.esbconsult.com<p>

 Check out our new ESBPCS for VCL which provides a huge collection of Routines and Components to make
     Data Entry and Manipulation easier for both Developers and their end-users.<p>
 http://www.esbconsult.com/esbpcs<p>

 Marcel Martin has been helping out to optimise and improve routines.<p>

 Rory Daulton has generously donated and helped with many optimised
 routines. Our thanks to him as well.<p>

 Marcel van Brakel has also been very helpful has includes ESBMaths
 into the Jedi Collection. http://www.delphi-jedi.org/<p>

 Any mistakes made are mine rather than Rory's or the Marcels'.<p>

 History: See ESBMaths-Whatsnew.txt
}

interface
{$IFDEF VER120}
{$DEFINE D4andAbove}
{$ENDIF}

{$IFDEF VER125}
{$DEFINE D4andAbove}
{$ENDIF}

{$IFDEF VER130}
{$DEFINE D4andAbove}
{$ENDIF}

{$IFDEF VER140}
{$DEFINE D4andAbove}
{$ENDIF}

{$J-} // Constants from here are not assignable
const
  //: Smallest Magnitude Single Available.
  MinSingle: Single = 1.5e-45;
  //: Largest Magnitude Single Available.
  MaxSingle: Single = 3.4e+38;
  //: Smallest Magnitude Double Available.
  MinDouble: Double = 5.0e-324;
  //: Largest Magnitude Double Available.
  MaxDouble: Double = 1.7e+308;
{$IFDEF Win64}
  //: Smallest Magnitude Extended Available.
  MinExtended: Extended = 5.0e-324;
  //: Largest Magnitude Extended Available.
  MaxExtended: Extended = 1.7e+308;
{$ELSE}
  //: Smallest Magnitude Extended Available.
  MinExtended: Extended = 3.6e-4951;
  //: Largest Magnitude Extended Available.
  MaxExtended: Extended = 1.1e+4932;
{$ENDIF}
  // Smallest Delphi Currency Value.
  MinCurrency: Currency = -922337203685477.5807;
  // Largest Delphi Currency Value.
  MaxCurrency: Currency = 922337203685477.5807;

  {--- Mathematical Constants ---}

  {--- Taken from Abramowitz & Stegun: "Handbook of Mathematical Functions"
   and other sources ----}

const
  //: Square Root of 2.
  Sqrt2: Extended = 1.4142135623730950488;

  //: Square Root of 3.
  Sqrt3: Extended = 1.7320508075688772935;

  //: Square Root of 5.
  Sqrt5: Extended = 2.2360679774997896964;

  //: Square Root of 10.
  Sqrt10: Extended = 3.1622776601683793320;

  //: Square Root of Pi.
  SqrtPi: Extended = 1.77245385090551602729;

  //: Cube Root of 2.
  Cbrt2: Extended = 1.2599210498948731648;

  //: Cube Root of 3.
  Cbrt3: Extended = 1.4422495703074083823;

  //: Cube Root of 10.
  Cbrt10: Extended = 2.1544346900318837219;

  //: Cube Root of 100.
  Cbrt100: Extended = 4.6415888336127788924;

  //: Cube Root of Pi.
  CbrtPi: Extended = 1.4645918875615232630;

  //: Inverse of Square Root of 2.
  InvSqrt2: Extended = 0.70710678118654752440;

  //: Inverse of Square Root of 3.
  InvSqrt3: Extended = 0.57735026918962576451;

  //: Inverse of Square Root of 5.
  InvSqrt5: Extended = 0.44721359549995793928;

  //: Inverse of Square Root of Pi.
  InvSqrtPi: Extended = 0.56418958354775628695;

  //: Inverse of Cube Root of Pi.
  InvCbrtPi: Extended = 0.68278406325529568147;

  //: Natural Constant.
  ESBe: Extended = 2.7182818284590452354;

  //: Square of Natural Constant.
  ESBe2: Extended = 7.3890560989306502272;

  //: Natural Constant raised to Pi.
  ESBePi: Extended = 23.140692632779269006;

  //: Natural Constant raised to Pi/2.
  ESBePiOn2: Extended = 4.8104773809653516555;

  //: Natural Constant raised to Pi/4.
  ESBePiOn4: Extended = 2.1932800507380154566;

  //: Natural Log of 2.
  Ln2: Extended = 0.69314718055994530942;

  //: Natural Log of 10.
  Ln10: Extended = 2.30258509299404568402;

  //: Natural Log of Pi.
  LnPi: Extended = 1.14472988584940017414;

  //: Log to Base 2 of 10.
  Log10Base2 = 3.3219280948873623478;

  //: Log to Base 10 of 2.
  Log2Base10: Extended = 0.30102999566398119521;

  //: Log to Base 10 of 3.
  Log3Base10: Extended = 0.47712125471966243730;

  //: Log to Base 10 of Pi.
  LogPiBase10: Extended = 0.4971498726941339;

  //: Log to Base 10 of Natural Constant.
  LogEBase10: Extended = 0.43429448190325182765;

  //: Accurate Pi Constant.
  ESBPi: Extended = 3.1415926535897932385;

  //: Inverse of Pi.
  InvPi: Extended = 3.1830988618379067154e-1;

  //: Two * Pi.
  TwoPi: Extended = 6.2831853071795864769;

  //: Three * Pi.
  ThreePi: Extended = 9.4247779607693797153;

  //: Square of Pi.
  Pi2: Extended = 9.8696044010893586188;

  //: Pi raised to the Natural Constant.
  PiToE: Extended = 22.459157718361045473;

  //: Half of Pi.
  PiOn2: Extended = 1.5707963267948966192;

  //: Third of Pi.
  PiOn3: Extended = 1.0471975511965977462;

  //: Quarter of Pi.
  PiOn4: Extended = 0.7853981633974483096;

  //: Three Halves of Pi.
  ThreePiOn2: Extended = 4.7123889803846898577;

  //: Four Thirds of Pi.
  FourPiOn3: Extended = 4.1887902047863909846;

  //: 2^63.
  TwoToPower63: Extended = 9223372036854775808.0;

  //: One Radian in Degrees.
  OneRadian: Extended = 57.295779513082320877;

  //: One Degree in Radians.
  OneDegree: Extended = 1.7453292519943295769E-2;

  //: One Minute in Radians.
  OneMinute: Extended = 2.9088820866572159615E-4;

  //: One Second in Radians.
  OneSecond: Extended = 4.8481368110953599359E-6;

  //: Gamma Constant.
  ESBGamma: Extended = 0.57721566490153286061;

  //: Natural Log of the Square Root of (2 * Pi)
  LnRt2Pi: Extended = 9.189385332046727E-1;

{$IFNDEF D4andAbove}
type
  LongWord = Cardinal;
{$ENDIF}

type
  {: Used for a Bit List of 16 bits from 15 -> 0 }
  TBitList = Word;

var
  {: Tolerance used to decide when float close enough to zero.
   For MAXIMUM Tolerance use  MinExtended (3.6e-4951). }
  ESBTolerance: Extended = 5.0e-324; // MinDouble

  {--- Integer Operations ---}

{$IFDEF D4andAbove}
  {: Multiply two unsigned 32-bit integers, checking for overflow.
   EXCEPTIONS: EIntOverflow  if the product will not fit into a LongWord.<p>
   NOTES: 1. This is needed in Delphi 5.01, since an attempt to multiply two
    LongWords results in signed multiplication.<p>
   Developed By Rory Daulton - used with permission.
  }
function UMul(const Num1, Num2: LongWord): LongWord;

{: Multiply two unsigned 32-bit integers then divide by 2**32 (ie
 return the upper DWORD of the product).
 EXCEPTIONS: None, since the answer always fits into 32 bits.<p>
 NOTES: This is useful in randomizing routines, to scale one integer by
  the other relative to 32 bits (reduce one integer according to the
  size of a second integer).<p>
 Developed By Rory Daulton - used with permission.
}
function UMulDiv2p32(const Num1, Num2: LongWord): LongWord;

{: Multiply two unsigned 32-bit integers then divide by another unsigned 32-bit integer.
 EXCEPTIONS: EDivByZero  if the divisor is zero (see Note 1). <p>
 NOTES: 1. The product followed by division will cause an unflagged integer
  overflow if the answer will not fit into 32 bits. In this case, the
  returned value is the lower 32-bits of the true answer.  This will
  never be a problem if the two multipliers are below the divisor.<p>
  2. The result is truncated toward zero (as in most integer divides).<p>
  3. This is useful in randomizing routines, to scale one integer by
  the other relative to a third (reduce one integer according to
  the relative size of a second integer compared to a third integer).<p>
  4. The Win32 API has a similar function MulDiv(), but it works on
  signed integers and rounds the quotient.<p>
 Developed By Rory Daulton - used with permission.
}
function UMulDiv(const Num1, Num2, Divisor: LongWord): LongWord;

{: Multiply two unsigned 32-bit integers then take the modulus by another
 unsigned 32-bit integer.
 EXCEPTIONS: EDivByZero   if the divisor is zero (See Note 1).<p>
 NOTES: 1. There is never an overflow error, since the answer always fits
  into 32 bits.<p>
  2. This is useful in linear congruential generators.<p>
 Developed By Rory Daulton - used with permission.
}
function UMulMod(const Num1, Num2, Modulus: LongWord): LongWord;

{$ENDIF}

{: Returns True if X1 and X2 are within ESBTolerance of each other }
function SameFloat(const X1, X2: Extended): Boolean;

{: Returns True if X is within ESBTolerance of 0 }
function FloatIsZero(const X: Extended): Boolean;

{: Returns True if X is Positive, ie X > ESBTolerance.
}
function FloatIsPositive(const X: Extended): Boolean;

{: Returns True if X is Negative, ie X < -ESBTolerance.
}
function FloatIsNegative(const X: Extended): Boolean;

{: Increments a Byte up to Limit. If B >= Limit no increment occurs. }
procedure IncLim(var B: Byte; const Limit: Byte);

{: Increments a ShortInt up to Limit. If B >= Limit no increment occurs. }
procedure IncLimSI(var B: ShortInt; const Limit: ShortInt);

{: Increments a Word up to Limit. If B >= Limit no increment occurs. }
procedure IncLimW(var B: Word; const Limit: Word);

{: Increments an Integer up to Limit. If B >= Limit no increment occurs. }
procedure IncLimI(var B: Integer; const Limit: Integer);

{: Increments a LongInt up to Limit. If B >= Limit no increment occurs. }
procedure IncLimL(var B: LongInt; const Limit: LongInt);

{: Decrements a Byte down to Limit. If B <= Limit no increment occurs. BASM }
procedure DecLim(var B: Byte; const Limit: Byte);

{: Decrements a ShortInt down to Limit. If B <= Limit no increment occurs. }
procedure DecLimSI(var B: ShortInt; const Limit: ShortInt);

{: Decrements a Word down to Limit. If B <= Limit no increment occurs. }
procedure DecLimW(var B: Word; const Limit: Word);

{: Decrements an Integer down to Limit. If B <= Limit no increment occurs. }
procedure DecLimI(var B: Integer; const Limit: Integer);

{: Decrements a LongInt down to Limit. If B <= Limit no increment occurs. }
procedure DecLimL(var B: LongInt; const Limit: LongInt);

{: Returns the maximum value between two Bytes. BASM }
function MaxB(const B1, B2: Byte): Byte;

{: Returns the minimum value between two Bytes. BASM }
function MinB(const B1, B2: Byte): Byte;

{: Returns the maximum value between two ShortInts. }
function MaxSI(const B1, B2: ShortInt): ShortInt;

{: Returns the minimum value between two ShortInts. }
function MinSI(const B1, B2: ShortInt): ShortInt;

{: Returns the maximum value between two Words. BASM }
function MaxW(const B1, B2: Word): Word;

{: Returns the minimum value between two Words. BASM }
function MinW(const B1, B2: Word): Word;

{: Returns the maximum value between two Integers. }
function MaxI(const B1, B2: Integer): Integer;

{: Returns the minimum value between two Integers. }
function MinI(const B1, B2: Integer): Integer;

{: Returns the maximum value between two LongInts. }
function MaxL(const B1, B2: LongInt): LongInt;

{: Returns the minimum value between two LongInts. }
function MinL(const B1, B2: LongInt): LongInt;

{: Swap Two Bytes. }
procedure SwapB(var B1, B2: Byte);

{: Swap Two ShortInts. }
procedure SwapSI(var B1, B2: ShortInt);

{: Swap Two Words. }
procedure SwapW(var B1, B2: Word);

{: Swap Two Integers. }
procedure SwapI(var B1, B2: SmallInt);

{: Swap Two LongInts. }
procedure SwapL(var B1, B2: LongInt);

{: Swap Two Integers (32-bit). }
procedure SwapI32(var B1, B2: Integer);

{: Swap Two LongWords. }
procedure SwapC(var B1, B2: LongWord);

{$IFDEF D4andAbove}
{: Swap Two Int64's }
procedure SwapInt64(var X, Y: Int64);
{$ENDIF}

{: Returns: <p>
 -1  if B < 0 <p>
  0  if B = 0 <p>
  1  if B > 0  BASM }
function Sign(const B: LongInt): ShortInt;

{: Returns the Maximum of 4 Words - BASM }
function Max4Word(const X1, X2, X3, X4: Word): Word;

{: Returns the Minimum of 4 Words - BASM }
function Min4Word(const X1, X2, X3, X4: Word): Word;

{: Returns the Maximum of 3 Words - BASM }
function Max3Word(const X1, X2, X3: Word): Word;

{: Returns the Minimum of 3 Words - BASM }
function Min3Word(const X1, X2, X3: Word): Word;

{: Returns the Maximum of an array of Bytes }
function MaxBArray(const B: array of Byte): Byte;

{: Returns the Maximum of an array of Words }
function MaxWArray(const B: array of Word): Word;

{: Returns the Maximum of an array of ShortInts }
function MaxSIArray(const B: array of ShortInt): ShortInt;

{: Returns the Maximum of an array of Integers }
function MaxIArray(const B: array of Integer): Integer;

{: Returns the Maximum of an array of LongInts }
function MaxLArray(const B: array of LongInt): LongInt;

{: Returns the Minimum of an array of Bytes }
function MinBArray(const B: array of Byte): Byte;

{: Returns the Minimum of an array of Words }
function MinWArray(const B: array of Word): Word;

{: Returns the Minimum of an array of ShortInts }
function MinSIArray(const B: array of ShortInt): ShortInt;

{: Returns the Minimum of an array of Integers }
function MinIArray(const B: array of Integer): Integer;

{: Returns the Minimum of an array of LongInts }
function MinLArray(const B: array of LongInt): LongInt;

{: Returns the Sum of an array of Bytes. All Operation in Bytes }
function SumBArray(const B: array of Byte): Byte;

{: Returns the Sum of an array of Bytes. All Operation in Words }
function SumBArray2(const B: array of Byte): Word;

{: Returns the Sum of an array of ShortInts. All Operation in ShortInts }
function SumSIArray(const B: array of ShortInt): ShortInt;

{: Returns the Sum of an array of ShortInts. All Operation in Integers }
function SumSIArray2(const B: array of ShortInt): Integer;

{: Returns the Sum of an array of Words. All Operation in Words }
function SumWArray(const B: array of Word): Word;

{: Returns the Sum of an array of Words. All Operation in Longints }
function SumWArray2(const B: array of Word): LongInt;

{: Returns the Sum of an array of Integers. All Operation in Integers }
function SumIArray(const B: array of Integer): Integer;

{: Returns the Sum of an array of Longints. All Operation in Longints }
function SumLArray(const B: array of LongInt): LongInt;

{: Returns the Sum of an array of LongWord. All Operation in LongWords }
function SumLWArray(const B: array of LongWord): LongWord;

{: Returns the number of digits (Magnitude) of a Positive Integer}
function ESBDigits(const X: LongWord): Byte;

{: Returns the highest Bit set within a LongWord. Donated by Michael Schnell }
function BitsHighest(const X: LongWord): Integer;

{: Returns the number of Bits needed to represent a given positive integer}
function ESBBitsNeeded(const X: LongWord): Integer;

{: Returns the Greatest Common (Positive) Divisor (GCD)of two Integers. Also
 Refered to as the Highest Common Factor (HCF). Uses Euclid's Algorithm.
     BASM Routine donated by Marcel Martin }
function GCD(const X, Y: LongWord): LongWord;

{$IFDEF D4andAbove}
{: Returns the Least Common Multiple of two Integers. }
function LCM(const X, Y: LongInt): Int64;
{$ELSE}
{: Returns the Least Common Multiple of two Integers. }
function LCM(const X, Y: LongInt): LongInt;
{$ENDIF}

{: If two Integers are Relative Prime to each other then GCD (X, Y) = 1.
 CoPrime is another term for Relative Prime. Some interpretive
     problems may arise when '0' and/or '1' are used. }
function RelativePrime(const X, Y: LongWord): Boolean;

{--- Floating Point Operations ---}

{: Returns the 80x87 Control Word <p>
  15-12 Reserved  <p>
 On 8087/80287 12 was Infinity Control <p>
  0 Projective <p>
  1 Affine <p>
  11-10 Rounding Control <p>
    00 Round to nearest even <p>
    01 Round Down <p>
    10 Round Up <p>
    11 Chop - Truncate towards Zero <p>
  9-8  Precision Control <p>
    00 24 bits Single Precision <p>
    01 Reserved <p>
    10 53 bits Double Precision <p>
    11 64 bits Extended Precision (Default) <p>
  7-6  Reserved <p>
 On 8087 7 was Interrupt Enable Mask <p>
  5  Precesion Exception Mask <p>
  4  Underflow Exception Mask <p>
  3  Overflow Exception Mask <p>
  2  Zero Divide Exception Mask <p>
  1  Denormalised Operand Exception Mask <p>
  0  Invalid Operation Exception Mask <p>
  BASM }
function Get87ControlWord: TBitList;

{: Sets the 80x87 Control Word <p>
  15-12 Reserved <p>
 On 8087/80287 12 was Infinity Control <p>
  0 Projective <p>
  1 Affine <p>
  11-10 Rounding Control <p>
    00 Round to nearest even <p>
    01 Round Down <p>
    10 Round Up <p>
    11 Chop - Truncate towards Zero <p>
  9-8  Precision Control <p>
    00 24 bits Single Precision <p>
    01 Reserved <p>
    10 53 bits Double Precision <p>
    11 64 bits Extended Precision (Default) <p>
  7-6  Reserved <p>
 On 8087 7 was Interrupt Enable Mask <p>
  5  Precesion Exception Mask <p>
  4  Underflow Exception Mask <p>
  3  Overflow Exception Mask <p>
  2  Zero Divide Exception Mask <p>
  1  Denormalised Operand Exception Mask <p>
  0  Invalid Operation Exception Mask <p>
  BASM }
procedure Set87ControlWord(const CWord: TBitList);

{: Swap two Extendeds. }
procedure SwapExt(var X, Y: Extended);

{: Swap two Doubles. }
procedure SwapDbl(var X, Y: Double);

{: Swap two Singles. }
procedure SwapSing(var X, Y: Single);

{: Returns  <p>
 -1 if X < 0 <p>
  0 if X = 0 <p>
  1 if X > 0 <p>
 }
function Sgn(const X: Extended): ShortInt;

{: Returns the straight line Distance between (X1, Y1) and (X2, Y2) }
function Distance(const X1, Y1, X2, Y2: Extended): Extended;

{: Performs Floating Point Modulus. ExtMod := X - Floor ( X / Y ) * Y }
function ExtMod(const X, Y: Extended): Extended;

{: Performs Floating Point Remainder. ExtRem := X - Int ( X / Y ) * Y }
function ExtRem(const X, Y: Extended): Extended;

{: Returns X mod Y for Comp Data Types }
function CompMOD(const X, Y: Comp): Comp;

{: Converts Polar Co-ordinates into Cartesion Co-ordinates }
procedure Polar2XY(const Rho, Theta: Extended; var X, Y: Extended);

{: Converts Cartesian Co-ordinates to Polar Co-ordinates }
procedure XY2Polar(const X, Y: Extended; var Rho, Theta: Extended);

{: Converts Degrees/Minutes/Seconds into an Extended Real }
function DMS2Extended(const Degs, Mins, Secs: Extended): Extended;

{: Converts an Extended Real into Degrees/Minutes/Seconds }
procedure Extended2DMS(const X: Extended; var Degs, Mins, Secs: Extended);

{: Returns the Maximum of two Extended Reals }
function MaxExt(const X, Y: Extended): Extended;

{: Returns the Minimum of two Extended Reals }
function MinExt(const X, Y: Extended): Extended;

{: Returns the Maximum of an array of Extended Reals }
function MaxEArray(const B: array of Extended): Extended;

{: Returns the Minimum of an array of Extended Reals }
function MinEArray(const B: array of Extended): Extended;

{: Returns the Maximum of an array of Single Reals }
function MaxSArray(const B: array of Single): Single;

{: Returns the Maximum of an array of Single Reals }
function MinSArray(const B: array of Single): Single;

{: Returns the Maximum of an array of Comp Reals }
function MaxCArray(const B: array of Comp): Comp;

{: Returns the Minimum of an array of Comp Reals }
function MinCArray(const B: array of Comp): Comp;

{: Returns the Sum of an Array of Single Reals }
function SumSArray(const B: array of Single): Single;

{: Returns the Sum of an Array of Extended Reals }
function SumEArray(const B: array of Extended): Extended;

{: Returns the Sum of the Square of an Array of Extended Reals }
function SumSqEArray(const B: array of Extended): Extended;

{: Returns the Sum of the Square of the difference of
 an Array of Extended Reals from a given Value }
function SumSqDiffEArray(const B: array of Extended; Diff: Extended): Extended;

{: Returns the Sum of the Pairwise Product of two
 Arrays of Extended Reals }
function SumXYEArray(const X, Y: array of Extended): Extended;

{: Returns the Sum of an Array of Comp Reals }
function SumCArray(const B: array of Comp): Comp;

{: Returns A! i.e Factorial of A - only values up to 1754 are handled
 returns 0 if larger }
function FactorialX(A: LongWord): Extended;

{: Returns nPr i.e Permutation of r objects from n.
 Only values of N up to 1754 are handled	returns 0 if larger
 If R > N  then 0 is returned }
function PermutationX(N, R: LongWord): Extended;

{: Returns nCr i.e Combination of r objects from n.
 These are also known as the Binomial Coefficients
 Only values of N up to 1754 are handled	returns 0 if larger
 If R > N  then 0 is returned }

function BinomialCoeff(N, R: LongWord): Extended;

{: Returns True if all elements of X > ESBTolerance }
function IsPositiveEArray(const X: array of Extended): Boolean;

{: Returns the Geometric Mean of the values }
function GeometricMean(const X: array of Extended): Extended;

{: Returns the Harmonic Mean of the values }
function HarmonicMean(const X: array of Extended): Extended;

{: Returns the Arithmetic Mean of the Values }
function ESBMean(const X: array of Extended): Extended;

{: Returns the Variance of the Values, assuming a Sample.
 Square root this value to get Standard Deviation }
function SampleVariance(const X: array of Extended): Extended;

{: Returns the Variance of the Values, assuming a Population.
 Square root this value to get Standard Deviation }
function PopulationVariance(const X: array of Extended): Extended;

{: Returns the Mean and Variance of the Values, assuming a Sample.
 Square root the Variance to get Standard Deviation }
procedure SampleVarianceAndMean(const X: array of Extended;
  var Variance, Mean: Extended);

{: Returns the Mean and Variance of the Values, assuming a Population.
 Square root the Variance to get Standard Deviation }
procedure PopulationVarianceAndMean(const X: array of Extended;
  var Variance, Mean: Extended);

{: Returns the Median (2nd Quartiles) of the Values. The array
 MUST be sorted before using this operation }
function GetMedian(const SortedX: array of Extended): Extended;

{: Returns the Mode (most frequent) of the Values. The array
 MUST be sorted before using this operation. Function is False
 if no Mode exists }
function GetMode(const SortedX: array of Extended; var Mode: Extended): Boolean;

{: Returns the 1st and 3rd Quartiles - Median is 2nd Quartiles - of the Values.
 The array	MUST be sorted before using this operation }
procedure GetQuartiles(const SortedX: array of Extended; var Q1, Q3: Extended);

{: Returns the Magnitued of a given Value }
function ESBMagnitude(const X: Extended): Integer;

{-- Trigonometric Functions ---}

//: Returns Tangent of Angle given in Radians.
function ESBTan(Angle: Extended): Extended;

//: Returns CoTangent of the Angle given in Radians.
function ESBCot(Angle: Extended): Extended;

//: Returns CoSecant of the Angle given in Radians.
function ESBCosec(const Angle: Extended): Extended;

//: Returns Secant of the Angle given in Radians.
function ESBSec(const Angle: Extended): Extended;

//: Returns the ArcTangent of Y / X - Result is in Radians
function ESBArcTan(X, Y: Extended): Extended;

//: Fast Computation of Sin and Cos, where Angle is in Radians
procedure ESBSinCos(Angle: Extended; var SinX, CosX: Extended);

//: Given a Value returns the Angle whose Cosine it is, in Radians
function ESBArcCos(const X: Extended): Extended;

//: Given a Value returns the Angle whose Sine it is, in Radians
function ESBArcSin(const X: Extended): Extended;

{: Given a Value returns the Angle whose Secant it is, in Radians.
}
function ESBArcSec(const X: Extended): Extended;

{: Given a Value returns the Angle whose Cosecant it is, in Radians.
}
function ESBArcCosec(const X: Extended): Extended;

{-- Logarithm & Power Functions ---}

//: Returns Logarithm of X to Base 10
function ESBLog10(const X: Extended): Extended;

//: Returns Logarithm of X to Base 2
function ESBLog2(const X: Extended): Extended;

//: Returns Logarithm of X to Given Base
function ESBLogBase(const X, Base: Extended): Extended;

{: Calculate 2 to the given floating point power. Developed by Rory Daulton
 and used with permission. December 1998.<p>
 The algorithm used is to scale the power of the fractional part
 of X, using FPU commands. Although the FPU (Floating Point Unit) is used,
 the answer is exact for integral X, since the FSCALE FPU command is.<p>
 EOverflow Exception when X > Log2(MaxExtended) = 11356.5234062941439494
 (if there was no other FPU error condition, such as underflow or denormal,
 before entry to this routine)<p>
 EInvalidOp Exception on some occasions when EOverflow would be expected,
 due to some other FPU error condition (such as underflow) before entry to
 this routine.
}
function Pow2(const X: Extended): Extended;

{: Calculate any float to non-negative integer power. Developed by Rory Daulton
 and used with permission. Last modified December 1998.<p>
}
function IntPow(const Base: Extended; const Exponent: LongWord): Extended;

{: Raises Values to an Integer Power. Thanks to Rory Daulton for improvements.
}
function ESBIntPower(const X: Extended; const N: LongInt): Extended;

//: Returns X^Y - handles all cases
function XtoY(const X, Y: Extended): Extended;

//: Returns 10^Y - handles all cases
function TenToY(const Y: Extended): Extended;

//: Returns 2^Y - handles all cases
function TwoToY(const Y: Extended): Extended;

//: Returns log X using base Y - handles all valid cases
function LogXtoBaseY(const X, Y: Extended): Extended;

{: ISqrt (I) computes INT (SQRT (I)), that is, the integral part of the
 square root of integer I.
 Code  originally developed by Marcel Martin, used with permission.
 Rory Daulton introduced a faster routine (based on Marcel's) for most
 occassions and this is now used with Permission.
}
function ISqrt(const I: LongWord): Longword;

{: Calculate the integer part of the logarithm base 2 of an integer.
 Developed by Rory Daulton and used with Permission.<p>
 An Exception is raised if I is Zero.
}
function ILog2(const I: LongWord): LongWord;

{: Calculate the greatest integral power of two that is less than or equal to
 the parameter.
 EXCEPTIONS:  EInvalidArgument for argument zero.<P>
 Developed By Rory Daulton - used with permission.
}
function IGreatestPowerOf2(const N: LongWord): LongWord;

{--- Hyperbolic Functions ---}

//: Returns the inverse hyperbolic cosine of X
function ESBArCosh(X: Extended): Extended;

//: Returns the inverse hyperbolic sine of X
function ESBArSinh(X: Extended): Extended;

//: Returns the inverse hyperbolic tangent of X
function ESBArTanh(X: Extended): Extended;

//: Returns the hyperbolic cosine of X
function ESBCosh(X: Extended): Extended;

//: Returns the hyperbolic sine of X
function ESBSinh(X: Extended): Extended;

//: Returns the hyperbolic tangent of X
function ESBTanh(X: Extended): Extended;

{--- Special Functions ---}

{: Returns 1/Gamma(X) using a Series Expansion as defined in Abramowitz &
 Stegun. Defined for all values of X.<p>
 Accuracy: Gives about 15 digits.
}
function InverseGamma(const X: Extended): Extended;

{: Returns Gamma(X) using a Series Expansion for 1/Gamma (X) as defined in
 Abramowitz & Stegun. Defined for all values of X except negative
 integers and 0.<p>
 Accuracy: Gives about 15 digits.
}
function Gamma(const X: Extended): Extended;

{: Logarithm to base e of the gamma function.
 Accurate to about 1.e-14.
 Programmer: Alan Miller - developed for Fortan 77, converted with permission.
}
function LnGamma(const X: Extended): Extended;

{: Returns Beta(X,Y) using a Series Expansion for 1/Gamma (X) as defined in
 Abramowitz & Stegun. Defined for all values of X, Y except negative
 integers and 0.
 Accuracy: Gives about 15 digits.
}
function Beta(const X, Y: Extended): Extended;

{: Returns the Incomplete Beta Ix(P, Q), where 0 <= X <= 1 and
 P and Q are positive.
 Accuracy: Gives about 17 digits.<p>
 Adapted From Collected Algorithms from CACM,
 Algorithm 179 - Incomplete Beta Function Ratios,
 Oliver G Ludwig.
}
function IncompleteBeta(X: Extended; P, Q: Extended): Extended;

implementation

uses
{$IFDEF Win64}
  Math,
{$ENDIF}
  SysUtils;

procedure IncLim(var B: Byte; const Limit: Byte);
begin
  if B < Limit then
    Inc(B);
end;

procedure IncLimSI(var B: ShortInt; const Limit: ShortInt);
begin
  if B < Limit then
    Inc(B);
end;

procedure IncLimW(var B: Word; const Limit: Word);
begin
  if B < Limit then
    Inc(B);
end;

procedure IncLimI(var B: Integer; const Limit: Integer);
begin
  if B < Limit then
    Inc(B);
end;

procedure IncLimL(var B: LongInt; const Limit: LongInt);
begin
  if B < Limit then
    Inc(B);
end;

procedure DecLim(var B: Byte; const Limit: Byte);
begin
  if B > Limit then
    Dec(B);
end;

procedure DecLimSI(var B: ShortInt; const Limit: ShortInt);
begin
  if B > Limit then
    Dec(B);
end;

procedure DecLimW(var B: Word; const Limit: Word);
begin
  if B > Limit then
    Dec(B);
end;

procedure DecLimI(var B: Integer; const Limit: Integer);
begin
  if B > Limit then
    Dec(B);
end;

procedure DecLimL(var B: LongInt; const Limit: LongInt);
begin
  if B > Limit then
    Dec(B);
end;

function MaxB(const B1, B2: Byte): Byte;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

function MinB(const B1, B2: Byte): Byte;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

function MaxSI(const B1, B2: ShortInt): ShortInt;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

function MinSI(const B1, B2: ShortInt): ShortInt;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

function MaxW(const B1, B2: Word): Word;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

function MinW(const B1, B2: Word): Word;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

function MaxI(const B1, B2: Integer): Integer;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

function MinI(const B1, B2: Integer): Integer;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

function MaxL(const B1, B2: LongInt): LongInt;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

function MinL(const B1, B2: LongInt): LongInt;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

procedure SwapB(var B1, B2: Byte);
var
  Temp: Byte;
begin
  Temp := B1;
  B1 := B2;
  B2 := Temp;
end;

procedure SwapSI(var B1, B2: ShortInt);
var
  Temp: ShortInt;
begin
  Temp := B1;
  B1 := B2;
  B2 := Temp;
end;

procedure SwapW(var B1, B2: Word);
var
  Temp: Word;
begin
  Temp := B1;
  B1 := B2;
  B2 := Temp;
end;

procedure SwapI(var B1, B2: SmallInt);
var
  Temp: SmallInt;
begin
  Temp := B1;
  B1 := B2;
  B2 := Temp;
end;

procedure SwapL(var B1, B2: LongInt);
var
  Temp: LongInt;
begin
  Temp := B1;
  B1 := B2;
  B2 := Temp;
end;

procedure SwapI32(var B1, B2: Integer);
var
  Temp: Integer;
begin
  Temp := B1;
  B1 := B2;
  B2 := Temp;
end;

procedure SwapC(var B1, B2: LongWord);
var
  Temp: LongWord;
begin
  Temp := B1;
  B1 := B2;
  B2 := Temp;
end;

function Sign(const B: LongInt): ShortInt;
begin
  if B < 0 then
    Result := -1
  else if B = 0 then
    Result := 0
  else
    Result := 1;
end;

function Max4Word(const X1, X2, X3, X4: Word): Word;
begin
  Result := X1;
  if X2 > Result then
    Result := X2;
  if X3 > Result then
    Result := X3;
  if X4 > Result then
    Result := X4;
end;

function Min4Word(const X1, X2, X3, X4: Word): Word;
begin
  Result := X1;
  if X2 < Result then
    Result := X2;
  if X3 < Result then
    Result := X3;
  if X4 < Result then
    Result := X4;
end;

function Max3Word(const X1, X2, X3: Word): Word;
begin
  Result := X1;
  if X2 > Result then
    Result := X2;
  if X3 > Result then
    Result := X3;
end;

function Min3Word(const X1, X2, X3: Word): Word;
begin
  Result := X1;
  if X2 < Result then
    Result := X2;
  if X3 < Result then
    Result := X3;
end;

function MaxBArray(const B: array of Byte): Byte;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] > Result then
      Result := B[I];
end;

function MaxWArray(const B: array of Word): Word;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] > Result then
      Result := B[I];
end;

function MaxIArray(const B: array of Integer): Integer;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] > Result then
      Result := B[I];
end;

function MaxSIArray(const B: array of ShortInt): ShortInt;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] > Result then
      Result := B[I];
end;

function MaxLArray(const B: array of LongInt): LongInt;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] > Result then
      Result := B[I];
end;

function MinBArray(const B: array of Byte): Byte;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] < Result then
      Result := B[I];
end;

function MinWArray(const B: array of Word): Word;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] < Result then
      Result := B[I];
end;

function MinIArray(const B: array of Integer): Integer;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] < Result then
      Result := B[I];
end;

function MinSIArray(const B: array of ShortInt): ShortInt;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] < Result then
      Result := B[I];
end;

function MinLArray(const B: array of LongInt): LongInt;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] < Result then
      Result := B[I];
end;

{$IFDEF Win64}

function ISqrt(const I: LongWord): LongWord;
const
  Estimates: array[0..31] of Word = (
    1, 1, 2, 3, 5, 7, 11, 15,
    22, 31, 45, 63, 90, 127, 181, 255,
    362, 511, 724, 1023, 1448, 2047, 2896, 4095,
    5792, 8191, 11585, 16383, 23170, 32767, 46340, 65535);
var
  X, XNew: LongWord;
  Done: Boolean;
begin
  if I <= 1 then
  begin
    Result := I;
    Exit;
  end;

  X := Estimates[ILog2(I)];
  Done := False;
  repeat
    XNew := (I div X + X) div 2;
    if XNew < X then
      X := XNew
    else
      Done := True;
  until Done;

  Result := X;
end;

{$ELSE}

function ISqrt(const I: LongWord): LongWord;
const
  Estimates: array[0..31] of Word = (
    // for index i, constant := Trunc(Sqrt((Int64(2) shl i) - 1.0)), which is
    //   the largest possible ISqrt(n) for  2**i <= n < 2**(i+1)
    1, 1, 2, 3, 5, 7, 11, 15,
    22, 31, 45, 63, 90, 127, 181, 255,
    362, 511, 724, 1023, 1448, 2047, 2896, 4095,
    5792, 8191, 11585, 16383, 23170, 32767, 46340, 65535);
  // eax  // ebx  // ecx  // edx
asm  // entry:   // eax = I
  // calc the result quickly for zero or one (sqrt equals the argument)
	   cmp     eax, 1
	   jbe     @@end
  // save registers and the argument
	   push    ebx
	   mov     ebx, eax                // ebx = I
  // use the logarithm base 2 to load an initial estimate, which is greater
  //   than or equal to the actual value
	   bsr     eax, ebx        // eax = ILog2(I) (note upper WORD is now zero)
	   mov     ax, [word ptr Estimates + eax * 2]
						  // eax = X
  // repeat ...
@@repeat:
  // --  save the last estimate [ X ]
	   mov     ecx, eax                        // ecx = X
  // --  calc the new estimate [ (I/X + X) / 2 ; the Newton-Raphson formula ]
	   xor     edx, edx                                // edx = 0
	   mov     eax, ebx        // eax = I (so edx:eax = I)
	   div     ecx             // eax = I/X            // edx = I mod X
	   add     eax, ecx        // eax = I/X + X
	   shr     eax, 1          // eax = XNew = (I/X+X)/2
  // until the new estimate >= the last estimate
  //   [which can never happen in exact floating-point arithmetic, and can
  //   happen due to truncation only if the last estimate <= Sqrt(I) ]
	   cmp     eax, ecx
	   jb      @@repeat
  // use the next-to-last estimate as the result
	   mov     eax, ecx        // eax = X
  // restore registers
	   pop     ebx
@@end:              //exit:     // eax = Result
end {ISqrt};
{$ENDIF}

function ILog2(const I: LongWord): LongWord;
  procedure BadILog2;
  begin
    raise EMathError.Create('Division By Zero');
  end {BadILog2};
asm
    bsr     eax,eax
    jz      BadILog2
end {ILog2};

function SumBArray(const B: array of Byte): Byte;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    Result := Result + B[I];
end;

function SumBArray2(const B: array of Byte): Word;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    Result := Result + B[I];
end;

function SumSIArray(const B: array of ShortInt): ShortInt;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    Result := Result + B[I];
end;

function SumSIArray2(const B: array of ShortInt): Integer;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    Result := Result + B[I];
end;

function SumWArray(const B: array of Word): Word;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := 0 to High(B) do
    Result := Result + B[I];
end;

function SumWArray2(const B: array of Word): LongInt;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    Result := Result + B[I];
end;

function SumIArray(const B: array of Integer): Integer;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    Result := Result + B[I];
end;

function SumLArray(const B: array of LongInt): LongInt;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    Result := Result + B[I];
end;

function SumLWArray(const B: array of LongWord): LongWord;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    Result := Result + B[I];
end;

function Get87ControlWord: TBitList;
var
  Temp: Word;
asm
	fstcw [Temp]  		{ Get '87 Control Word }
	mov  ax, [Temp] 	{ Leave in AX for function }
end;

procedure Set87ControlWord(const CWord: TBitList);
var
  Temp: Word;
asm
	mov   [Temp], ax
	fldcw [Temp] 		{ Load '87 Control Word }
end;

procedure Polar2XY(const Rho, Theta: Extended; var X, Y: Extended);
begin
  ESBSinCos(Theta, Y, X); { quite fast }
  X := Rho * X;
  Y := Rho * Y;
end;

procedure XY2Polar(const X, Y: Extended; var Rho, Theta: Extended);
begin
  Rho := Sqrt(Sqr(X) + Sqr(Y));
  if Abs(X) > ESBTolerance then
  begin
    Theta := ArcTan(abs(Y) / abs(X));
    if Sgn(X) = 1 then
    begin
      if Sgn(Y) = -1 then
        Theta := TwoPi - Theta
    end
    else
    begin
      if Sgn(Y) = 1 then
        Theta := ESBPi - Theta
      else
        Theta := ESBPi + Theta
    end;
  end
  else
    Theta := Sgn(Y) * Pion2
end;

function Sgn(const X: Extended): ShortInt;
begin
  if X < 0.0 then
    Result := -1
  else if X = 0.0 then
    Result := 0
  else
    Result := 1
end;

function DMS2Extended(const Degs, Mins, Secs: Extended): Extended;
begin
  Result := Degs + Mins / 60.0 + Secs / 3600.0
end;

procedure Extended2DMS(const X: Extended; var Degs, Mins, Secs: Extended);
var
  Y: Extended;
begin
  Degs := Int(X);
  Y := Frac(X) * 60;
  Mins := Int(Y);
  Secs := Frac(Y) * 60;
end;

function Distance(const X1, Y1, X2, Y2: Extended): Extended;
{ Rory Daulton suggested this more tolerant routine }
var
  X, Y: Extended;
begin
  X := Abs(X1 - X2);
  Y := Abs(Y1 - Y2);
  if X > Y then
    Result := X * Sqrt(1 + Sqr(Y / X))
  else if Y <> 0 then
    Result := Y * Sqrt(1 + Sqr(X / Y))
  else
    Result := 0
end;

function ExtMod(const X, Y: Extended): Extended;
var
  Z: Extended;
begin
  Result := X / Y;
  Z := Int(Result);
  if Result < 0 then
    Z := Z - 1.0;
  { Z now has Floor (X / Y) }
  Result := X - Z * Y
end;

function ExtRem(const X, Y: Extended): Extended;
begin
  Result := X - Int(X / Y) * Y
end;

function MaxExt(const X, Y: Extended): Extended;
begin
  if X > Y then
    Result := X
  else
    Result := Y
end;

function MinExt(const X, Y: Extended): Extended;
begin
  if X < Y then
    Result := X
  else
    Result := Y
end;

function CompMOD(const X, Y: Comp): Comp;
begin
  Result := X - Y * Int(X / Y)
end;

function MaxEArray(const B: array of Extended): Extended;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] > Result then
      Result := B[I];
end;

function MinEArray(const B: array of Extended): Extended;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] < Result then
      Result := B[I];
end;

function MaxSArray(const B: array of Single): Single;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] > Result then
      Result := B[I];
end;

function MinSArray(const B: array of Single): Single;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] < Result then
      Result := B[I];
end;

function MaxCArray(const B: array of Comp): Comp;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] > Result then
      Result := B[I];
end;

function MinCArray(const B: array of Comp): Comp;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    if B[I] < Result then
      Result := B[I];
end;

function SumSArray(const B: array of Single): Single;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    Result := Result + B[I];
end;

function SumEArray(const B: array of Extended): Extended;
var
  I: Integer;
begin
  Result := B[Low(B)];
  for I := Low(B) + 1 to High(B) do
    Result := Result + B[I];
end;

function SumSqEArray(const B: array of Extended): Extended;
var
  I: Integer;
begin
  Result := Sqr(B[Low(B)]);
  for I := Low(B) + 1 to High(B) do
    Result := Result + Sqr(B[I]);
end;

function SumSqDiffEArray(const B: array of Extended; Diff: Extended): Extended;
var
  I: Integer;
begin
  Result := Sqr(B[Low(B)] - Diff);
  for I := Low(B) + 1 to High(B) do
    Result := Result + Sqr(B[I] - Diff);
end;

function SumXYEArray(const X, Y: array of Extended): Extended;
var
  I: Integer;
  M, N: Integer;
begin
  M := MaxL(Low(X), Low(Y));
  N := MinL(High(X), High(Y));
  Result := X[M] * Y[M];
  for I := M + 1 to N do
    Result := Result + X[I] * Y[I];
end;

function SumCArray(const B: array of Comp): Comp;
var
  I: Integer;
begin
  Result := Low(B);
  for I := Low(B) + 1 to High(B) do
    Result := Result + B[I];
end;

function SameFloat(const X1, X2: Extended): Boolean;
begin
  Result := abs(X1 - X2) < ESBTolerance
end;

function FloatIsZero(const X: Extended): Boolean;
begin
  Result := abs(X) < ESBTolerance
end;

function FloatIsPositive(const X: Extended): Boolean;
begin
  Result := (X >= ESBTolerance);
end;

function FloatIsNegative(const X: Extended): Boolean;
begin
  Result := (X <= -ESBTolerance);
end;

function FactorialX(A: LongWord): Extended;
var
  I: Integer;
begin
  if A > 1754 then
  begin
    Result := 0.0;
    Exit;
  end;
  Result := 1.0;
  for I := 2 to A do
    Result := Result * I;
end;

function PermutationX(N, R: LongWord): Extended;
var
  I: Integer;
begin
  if (N = 0) or (R > N) or (N > 1754) then
  begin
    Result := 0.0;
    Exit;
  end;
  Result := 1.0;
  if (R = 0) then
    Exit;
  try
    for I := N downto N - R + 1 do
      Result := Result * I;
    Result := Int(Result + 0.5);
  except
    Result := -1.0
  end;
end;

function BinomialCoeff(N, R: LongWord): Extended;
var
  I: Integer;
  K: LongWord;
begin
  if (N = 0) or (R > N) or (N > 1754) then
  begin
    Result := 0.0;
    Exit;
  end;
  Result := 1.0;
  if (R = 0) or (R = N) then
    Exit;
  if R > N div 2 then
    R := N - R;
  K := 2;
  try
    for I := N - R + 1 to N do
    begin
      Result := Result * I;
      if K <= R then
      begin
        Result := Result / K;
        Inc(K);
      end;
    end;
    Result := Int(Result + 0.5);
  except
    Result := -1.0
  end;
end;

function IsPositiveEArray(const X: array of Extended): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(X) do
    if X[I] <= ESBTolerance then
      Exit;
  Result := True;
end;

function GeometricMean(const X: array of Extended): Extended;
var
  I: Integer;
begin
  if High(X) < 0 then
    raise Exception.Create('Array is Empty!')
  else if not IsPositiveEArray(X) then
    raise Exception.Create('Array contains values <= 0!')
  else
  begin
    Result := 1;
    for I := 0 to High(X) do
      Result := Result * X[I];
    Result := XtoY(Result, 1 / (High(X) + 1));
  end;
end;

function HarmonicMean(const X: array of Extended): Extended;
var
  I: Integer;
begin
  if High(X) < 0 then
    raise Exception.Create('Array is Empty!')
  else if not IsPositiveEArray(X) then
    raise Exception.Create('Array contains values <= 0!')
  else
  begin
    Result := 0;
    for I := 0 to High(X) do
      Result := Result + 1 / X[I];
    Result := Result / (High(X) + 1);
    Result := 1 / Result;
  end;
end;

function ESBMean(const X: array of Extended): Extended;
begin
  Result := SumEArray(X) / (High(X) - Low(X) + 1)
end;

function SampleVariance(const X: array of Extended): Extended;
var
  I: Integer;
  SumSq: Extended;
  Mean: Extended;
begin
  Mean := ESBMean(X);
  SumSq := 0.0;
  for I := Low(X) to High(X) do
    SumSq := SumSq + Sqr(X[I] - Mean);
  Result := SumSq / (High(X) - Low(X))
end;

function PopulationVariance(const X: array of Extended): Extended;
var
  I: Integer;
  SumSq: Extended;
  Mean: Extended;
begin
  Mean := ESBMean(X);
  SumSq := 0.0;
  for I := Low(X) to High(X) do
    SumSq := SumSq + Sqr(X[I] - Mean);
  Result := SumSq / (High(X) - Low(X) + 1)
end;

procedure SampleVarianceAndMean(const X: array of Extended;
  var Variance, Mean: Extended);
var
  I: Integer;
  SumSq: Extended;
begin
  Mean := ESBMean(X);
  SumSq := 0.0;
  for I := Low(X) to High(X) do
    SumSq := SumSq + Sqr(X[I] - Mean);
  if High(X) > Low(X) then
    Variance := SumSq / (High(X) - Low(X))
  else
    Variance := 0;
end;

procedure PopulationVarianceAndMean(const X: array of Extended;
  var Variance, Mean: Extended);
var
  I: Integer;
  SumSq: Extended;
begin
  Mean := ESBMean(X);
  SumSq := 0.0;
  for I := Low(X) to High(X) do
    SumSq := SumSq + Sqr(X[I] - Mean);
  Variance := SumSq / (High(X) - Low(X) + 1)
end;

function GetMedian(const SortedX: array of Extended): Extended;
var
  N: Integer;
begin
  N := High(SortedX) + 1;
  if N <= 0 then
    raise Exception.Create('Array is Empty!')
  else if N = 1 then
    Result := SortedX[0]
  else if Odd(N) then
    Result := SortedX[N div 2]
  else
    Result := (SortedX[N div 2 - 1] + SortedX[N div 2]) / 2;
end;

function GetMode(const SortedX: array of Extended; var Mode: Extended): Boolean;
var
  I, Freq, HiFreq: Integer;
  Matched: Boolean;
begin
  if High(SortedX) < 0 then
  begin
    raise Exception.Create('Array is Empty!')
  end
  else if High(SortedX) = 0 then
  begin
    Mode := SortedX[0];
    Result := True;
  end
  else
  begin
    Mode := -MaxExtended;
    Freq := 1;
    HiFreq := 0;
    Matched := False;
    for I := 1 to High(SortedX) do
    begin
      if SameFloat(SortedX[I - 1], SortedX[I]) then
        Inc(Freq)
      else
      begin
        if Freq <> 1 then
        begin
          if Freq = HiFreq then
            Matched := True
          else if Freq > HiFreq then
          begin
            Mode := SortedX[I - 1];
            HiFreq := Freq;
            Matched := False;
          end;
          Freq := 1;
        end;
      end;
    end;
    if HiFreq > 0 then
    begin
      if Freq = HiFreq then
        Matched := True
      else if Freq > HiFreq then
      begin
        Mode := SortedX[High(SortedX)];
        Matched := False;
      end;
    end
    else if Freq > 1 then
    begin
      HiFreq := Freq;
      Mode := SortedX[High(SortedX)];
      Matched := False;
    end;
    Result := (HiFreq > 0) and not Matched;
  end;
end;

procedure GetQuartiles(const SortedX: array of Extended; var Q1, Q3: Extended);
var
  N: Single;
  I: Integer;
begin
  if High(SortedX) < 0 then
    raise Exception.Create('Array is Empty!')
  else if High(SortedX) = 0 then
  begin
    Q1 := SortedX[0];
    Q3 := SortedX[0];
  end
  else
  begin
    N := (High(SortedX) + 1) / 4 + 0.5;
    I := Trunc(N);
    N := Frac(N);
    if I - 1 < High(SortedX) then
      Q1 := SortedX[I - 1] + (SortedX[I] - SortedX[I - 1]) * N
    else
      Q1 := SortedX[I - 1];

    N := 3 * (High(SortedX) + 1) / 4 + 0.5;
    I := Trunc(N);
    N := Frac(N);
    if I - 1 < High(SortedX) then
      Q3 := SortedX[I - 1] + (SortedX[I] - SortedX[I - 1]) * N
    else
      Q3 := SortedX[I - 1];
  end;
end;

function ESBDigits(const X: LongWord): Byte;
// Returns the number of digits in a given positive integer
begin
  if X = 0 then
    Result := 0
  else
    Result := Trunc(ESBLog10(X)) + 1;
end;

function ESBMagnitude(const X: Extended): Integer;
// Returns the number of digits in a given positive integer
begin
  if X = 0 then
    Result := 0
  else
  begin
    if X >= 1 then
      Result := Trunc(ESBLog10(Abs(X))) + 1
    else
      Result := Trunc(ESBLog10(Abs(X)) - 0.00000000001) - 1
  end;
end;

function BitsHighest(const X: LongWord): Integer;
asm
	   MOV     ECX, EAX
	   MOV     EAX, -1
	   BSR     EAX, ECX
end;

function ESBBitsNeeded(const X: LongWord): Integer;
begin
  Result := BitsHighest(X) + 1;
end;

{--- Trigonometric Functions ---}

function ESBTan(Angle: Extended): Extended;
  function FTan(Angle: Extended): Extended;
  asm
		fld		[Angle]	// St(0) <- Angle
		ffree     st(7)	// Ensure st(7) is free
		fptan		     // St(1) <- Tan (Angle), St(0) <- 1
		fstp		st(0)	// Dispose of 1
		fwait
  end;
begin
  if abs(Angle) >= TwoToPower63 then // must be less then 2^63
    raise EMathError.Create('Angle Magnitude too large for ESBTan')
  else
    Result := FTan(Angle);
end;

function ESBCot(Angle: Extended): Extended;
  function FCot(Angle: Extended): Extended;
  asm
		fld		[Angle]	// St(0) <- Angle
		ffree     st(7)	// Ensure st(7) is free
		fptan		     // St(1) <- Tan (Angle), St(0) <- 1
		fdivrp			// St(0) <- St(0)/St(1) which is Cot
		fwait
  end;
begin
  if abs(Angle) >= TwoToPower63 then // must be less then 2^63
    raise EMathError.Create('Angle Magnitude too large for ESBCot')
  else
    Result := FCot(Angle);
end;

function ESBArcTan(X, Y: Extended): Extended;
  function FArcTan(X, Y: Extended): Extended;
  asm
		fld		[Y]		// St(0) <- Y
		fld	     [X]		// St(0) <- X, St (1) <- Y
		fpatan			// St(0) <- ArcTan (Y/X)
		fwait
  end;
begin
  if X = 0 then
  begin
    if Y = 0 then
      Result := 0
    else if Y > 0 then
      Result := PiOn2
    else
      Result := -PiOn2
  end
  else if Y = 0 then
  begin
    if X > 0 then
      Result := 0
    else
      Result := ESBPi
  end
  else
    Result := FArcTan(X, Y);
end;

procedure ESBSinCos(Angle: Extended; var SinX, CosX: Extended);
  procedure FSinCos(Angle: Extended; var SinX, CosX: Extended);
  asm
		fld		[Angle]			// St(0) <- Angle
		fsincos
		fstp		tbyte ptr [edx]    // St(0) -> CosX
		fstp    	tbyte ptr [eax]    // St(0) -> SinX
		fwait
  end;
begin
  if abs(Angle) >= TwoToPower63 then // must be less then 2^63
    raise EMathError.Create('Angle Magnitude too large for ESBSinCos')
  else
    FSinCos(Angle, SinX, CosX);
end;

function ESBArcCos(const X: Extended): Extended;
var
  Y: Extended;
begin
  if abs(X) > 1 then
    raise EMathError.Create('ESBArcCos requires values with magnitude <= 1')
  else
  begin
    Y := Sqrt(1 - Sqr(X));
    if abs(X) <= Y then
      Result := PiOn2 - ESBArcTan(Y, X)
    else if X < 0 then
      Result := ESBArcTan(X, Y) + ESBPi
    else
      Result := ESBArcTan(X, Y)
  end;
end;

function ESBArcSin(const X: Extended): Extended;
var
  Y: Extended;
begin
  if abs(X) > 1 then
    raise EMathError.Create('ESBArcSin requires values with magnitude <= 1')
  else
  begin
    Y := Sqrt(1 - Sqr(X));
    if abs(X) <= Y then
      Result := ESBArcTan(Y, X)
    else
      Result := Sgn(X) * PiOn2 - ESBArcTan(X, Y)
  end;
end;

function ESBCosec(const Angle: Extended): Extended;
var
  Y: Extended;
begin
  Y := Sin(Angle);
  if Y = 0 then
    raise EMathError.Create('ESBCosec is Undefined')
  else
    Result := 1 / Y;
end;

function ESBSec(const Angle: Extended): Extended;
var
  Y: Extended;
begin
  Y := Cos(Angle);
  if Y = 0 then
    raise EMathError.Create('ESBSec is Undefined')
  else
    Result := 1 / Y;
end;

function ESBArcSec(const X: Extended): Extended;
begin
  if FloatIsZero(X) then
    raise EMathError.Create('Divide By Zero');
  Result := ESBArcCos(1 / Abs(X));
  if X < 0 then
    Result := Result + ESBPI;
end;

function ESBArcCosec(const X: Extended): Extended;
begin
  if FloatIsZero(X) then
    raise EMathError.Create('Divide By Zero');
  Result := ESBArcSin(1 / Abs(X));
  if X < 0 then
    Result := Result + ESBPI;
end;

{--- Logarithm & Power Functions ---}

{------------------------------------------------------------------------------
PURPOSE:     Calculate 2 to the given floating point power.
AUTHOR:      Rory Daulton
DATE:        December 1998
PARAMETERS:  X: power to take of 2 [so the result is 2**X]
EXCEPTIONS:  EOverflow  when X > Log2(MaxExtended) = 11356.5234062941439494
        (if there was no other FPU error condition, such as
        underflow or denormal, before entry to this routine)
     EInvalidOp on some occasions when EOverflow would be expected, due
        to some other FPU error condition (such as underflow)
        before entry to this routine
NOTES:       1. The algorithm used is to scale the power of the fractional part
    of X, using FPU commands.
     2. Although the FPU (Floating Point Unit) is used, the answer is
    exact for integral X, since the FSCALE FPU command is.
------------------------------------------------------------------------------}
{$IFDEF Win64}

function Pow2(const X: Extended): Extended;
begin
  Result := Power(2, X);
end;
{$ELSE}

function Pow2(const X: Extended): Extended;
asm
	   fld     X
// find Round(X)
	   fld     st
	   frndint
// find _Frac(X) [minimal fractional part of X, between -0.5 and 0.5]
	   fsub    st(1),st
	   fxch    st(1)
// Find 2**_Frac(X)
	   f2xm1
	   fld1
	   fadd
// Result := 2**_Frac(X) * 2**Round(X)
	   fScale
	   fstp    st(1)
	   fwait
end {Pow2};
{$ENDIF}

{$IFDEF Win64}

function IntPow(const Base: Extended; const Exponent: LongWord): Extended;
begin
  Result := ESBIntPower(Base, Exponent);
end;
{$ELSE}

function IntPow(const Base: Extended; const Exponent: LongWord): Extended;
{ Heart of Rory Daulton's IntPower: assumes valid parameters &
non-negative exponent }
asm
		fld1                      { Result := 1 }
		cmp     eax, 0            { eax := Exponent }
		jz      @@3
		fld     Base
		jmp     @@2
  @@1:    fmul    ST, ST            { X := Base * Base }
  @@2:    shr     eax,1
		jnc     @@1
		fmul    ST(1),ST          { Result := Result * X }
		jnz     @@1
		fstp    st                { pop X from FPU stack }
  @@3:
		fwait
end;
{$ENDIF}

function ESBLog10(const X: Extended): Extended;
  function FLog10(X: Extended): Extended;
  asm
		fldlg2			// St(0) <- Log10 of 2
		fld		[X]		// St(0) <- X, St(1) <- Log10 of 2
		fyl2x			// St(0) <- log10 (2) * Log2 (X)
		fwait
  end;
  function AltFLog10(X: Extended): Extended;
  asm
		fldlg2			// St(0) <- Log10 of 2
		fld		[X]		// St(0) <- X, St(1) <- Log10 of 2
		fyl2xp1			// St(0) <- log10 (2) * Log2 (X+1)
		fwait
  end;
begin
  if not FloatIsPositive(X) then // must be Positive
    raise EMathError.Create('Value must be > 0')
  else if abs(X - 1) < 0.1 then
    Result := AltFLog10(X - 1)
  else
    Result := FLog10(X);
end;

function ESBLog2(const X: Extended): Extended;
  function FLog2(X: Extended): Extended;
  asm
		fld1				// St(0) <- 1
		fld		[X]		// St(0) <- X, St(1) <-1
		fyl2x			// St(0) <- 1 * Log2 (X)
		fwait
  end;
  function AltFLog2(X: Extended): Extended;
  asm
		fld1				// St(0) <- 1
		fld		[X]		// St(0) <- X, St(1) <-1
		fyl2xp1			// St(0) <- 1 * Log2 (X+1)
		fwait
  end;
begin
  if not FloatIsPositive(X) then // must be Positive
    raise EMathError.Create('Value must be > 0')
  else if abs(X - 1) < 0.1 then
    Result := AltFLog2(X - 1)
  else
    Result := FLog2(X);
end;

function ESBLogBase(const X, Base: Extended): Extended;
begin
  if not FloatIsPositive(X) then // must be Positive
    raise EMathError.Create('Value must be > 0')
  else if not FloatIsPositive(Base) then // must be Positive
    raise EMathError.Create('Value must be > 0')
  else
    Result := ESBLog2(X) / ESBLog2(Base);
end;

function LogXtoBaseY(const X, Y: Extended): Extended;
begin
  Result := ESBLog2(X) / ESBLog2(Y);
end;

function ESBIntPower(const X: Extended; const N: LongInt): Extended;
var
  P: LongWord;
begin
  if N = 0 then
    Result := 1
  else if (X = 0) then
  begin
    if N < 0 then
      raise EMathError.Create('Zero cannot be raised to a Negative Power')
    else
      Result := 0
  end
  else if (X = 1) then
    Result := 1
  else if X = -1 then
  begin
    if Odd(N) then
      Result := -1
    else
      Result := 1
  end
  else if N > 0 then
    Result := IntPow(X, N)
  else
  begin
    if N <> Low(LongInt) then
      P := abs(N)
    else
{$IFDEF D4andAbove}
      P := LongWord(High(LongInt)) + 1;
{$ELSE}
      raise EMathError.Create('Cannot be raised to Low (LongInt)');
{$ENDIF}
    try
      Result := IntPow(X, P);
    except
      on EMathError do
      begin
        Result := IntPow(1 / X, P); { try again with another method, }
        Exit; {   perhaps less precise         }
      end {on};
    end {try};
    Result := 1 / Result;
  end;
end;

function XtoY(const X, Y: Extended): Extended;
  function PowerAbs: Extended; // Routine developed by Rory Daulton
  var
    ExponentPow2: Extended; // equivalent exponent to power 2
  begin
    try
      ExponentPow2 := ESBLog2(Abs(X)) * Y;
    except
      on EMathError do
        // allow underflow, when ExponentPow2 would have been negative
        if (Abs(X) > 1)<>(Y > 0) then
        begin
          Result := 0;
          Exit;
        end {if}
        else
          raise;
    end {try};
    Result := Pow2(ExponentPow2);
  end;
begin
  if FloatIsZero(Y) then
    Result := 1
  else if FloatIsZero(X) then
  begin
    if Y < 0 then
      raise EMathError.Create('Zero cannot be raised to a Negative Power')
    else
      Result := 0
  end
  else if FloatIsZero(Frac(Y)) then
  begin
    if (Y >= Low(LongInt)) and (Y <= High(LongInt)) then
      Result := ESBIntPower(X, LongInt(Round(Y)))
    else
    begin
      if (X > 0) or FloatIsZero(Frac(Y / 2.0)) then
        Result := PowerAbs
      else
        Result := -PowerAbs
    end;
  end
  else if X > 0 then
    Result := PowerAbs
  else
    raise EMathError.Create('Invalid X^Y')
end;

function TenToY(const Y: Extended): Extended;
begin
  if FloatIsZero(Y) then
    Result := 1
  else if Y < -3.58e4931 then
    Result := 0
  else
    Result := Pow2(Y * Log10Base2)
end;

function TwoToY(const Y: Extended): Extended;
begin
  if FloatIsZero(Y) then
    Result := 1
  else
    Result := Pow2(Y)
end;

{--- Hyperbolic Functions ---}

function ESBArCosh(X: Extended): Extended;
begin
  Result := Ln(X + Sqrt(Sqr(X) - 1));
end;

function ESBArSinh(X: Extended): Extended;
begin
  Result := Ln(X + Sqrt(Sqr(X) + 1));
end;

function ESBArTanh(X: Extended): Extended;
var
  Y: Extended;
begin
  if X = 1 then
    raise EMathError.Create('ESBArTanh is not Defined for X = 1')
  else
  begin
    Y := (1 + X) / (1 - X);
    if Y <= 0 then
      raise EMathError.Create('ESBArTanh is not Defined for that Value of X')
    else
      Result := Ln(Y) / 2;
  end;
end;

function ESBCosh(X: Extended): Extended;
begin
  Result := (Exp(X) + Exp(-X)) / 2.0;
end;

function ESBSinh(X: Extended): Extended;
begin
  Result := (Exp(X) - Exp(-X)) / 2.0;
end;

function ESBTanh(X: Extended): Extended;
var
  Y: Extended;
begin
  Y := ESBCosh(X);
  if Y = 0 then
    raise EMathError.Create('X has a Cosh of 0, illegal for ESBTanh')
  else
    Result := ESBSinh(X) / Y;
end;

function GCD(const X, Y: LongWord): LongWord;
asm
     jmp   @01      // We start with EAX <- X, EDX <- Y, and check to see if Y = 0
@00: mov   ecx, edx	// ECX <- EDX prepare for division
     xor   edx, edx // clear EDX for Division
     div   ecx		// EAX <- EDX:EAX div ECX, EDX <- EDX:EAX mod ECX
     mov   eax, ecx	// EAX <- ECX, and repeat if EDX <> 0
@01: and   edx, edx // test to see if EDX is zero, without changing EDX
     jnz   @00		// when EDX is zero EAX has the result
end;

{$IFDEF D4andAbove}

function LCM(const X, Y: LongInt): Int64;
begin
  Result := (X div LongInt(GCD(Abs(X), Abs(Y)))) * Int64(Y);
end;
{$ELSE}

function LCM(const X, Y: LongInt): LongInt;
begin
  Result := (X div LongInt(GCD(Abs(X), Abs(Y)))) * Y;
end;
{$ENDIF}

function RelativePrime(const X, Y: LongWord): Boolean;
begin
  Result := GCD(X, Y) = 1;
end;

{$IFDEF D4andAbove}

procedure SwapInt64(var X, Y: Int64);
var
  Temp: Int64;
begin
  Temp := X;
  X := Y;
  Y := Temp;
end;
{$ENDIF}

procedure SwapExt(var X, Y: Extended);
var
  Temp: Extended;
begin
  Temp := X;
  X := Y;
  Y := Temp;
end;

procedure SwapDbl(var X, Y: Double);
var
  Temp: Double;
begin
  Temp := X;
  X := Y;
  Y := Temp;
end;

procedure SwapSing(var X, Y: Single);
var
  Temp: Single;
begin
  Temp := X;
  X := Y;
  Y := Temp;
end;

{$IFDEF D4andAbove}
{---------------------------------------------------------------------------
---
PURPOSE: Show an unsigned-integer overflow error with an exception
NOTES:   This is an internal routine, not declared in the interface section
----------------------------------------------------------------------------
--}

procedure FlagUIntOverflow;
begin
  raise EIntOverflow.Create('Unsigned integer overflow in RDMath');
end {FlagUIntOverflow};

{---------------------------------------------------------------------------
---
PURPOSE:     Multiply two unsigned 32-bit integers, checking for overflow
DATE:        March 2001
EXCEPTIONS:  EIntOverflow  if the product will not fit into a LongWord
NOTES:       1. This is needed in Delphi 5.01, since an attempt to multiply two
    LongWords results in signed multiplication.
----------------------------------------------------------------------------
--}

function UMul(const Num1, Num2: LongWord): LongWord;
asm
	// Num1 is in eax, Num2 is in edx
	mul     edx               // product now in edx:eax
	jc      FlagUIntOverflow  // error on overflow
end {UMul};

{---------------------------------------------------------------------------
---
PURPOSE:     Multiply two unsigned 32-bit integers then divide by 2**32 (i.e.
     return the upper DWORD of the product)
DATE:        Modified 13 Apr 2001 to use LongWords instead of Integers
EXCEPTIONS:  None, since the answer always fits into 32 bits
NOTES:       This is useful in randomizing routines, to scale one integer by
     the other relative to 32 bits (reduce one integer according to the
     size of a second integer).
----------------------------------------------------------------------------
--}

function UMulDiv2p32(const Num1, Num2: LongWord): LongWord;
asm
	// Num1 is in eax, Num2 is in edx
	mul     edx           // result is in edx:eax
	mov     eax,edx       // return high dword
end {UMulDiv2p32};

{---------------------------------------------------------------------------
---
PURPOSE:     Multiply two unsigned 32-bit integers then divide by another
     unsigned 32-bit integer.
DATE:        Created December 1998
     Modified 13 Apr 2001 to use LongWords instead of Integers
EXCEPTIONS:  EDivByZero  if the divisor is zero
     (see Note 1)
NOTES:       1. The product followed by division will cause an unflagged
    integer overflow if the answer will not fit into 32 bits. In
    this case, the returned value is the lower 32-bits of the true
    answer.  This will never be a problem if the two multipliers
    are below the divisor.
     2. The result is truncated toward zero (as in most integer
    divides).
     3. This is useful in randomizing routines, to scale one integer by
    the other relative to a third (reduce one integer according to
    the relative size of a second integer compared to a third
    integer).
     4. The Win32 API has a similar function MulDiv(), but it works on
    signed integers and rounds the quotient.
----------------------------------------------------------------------------
--}

function UMulDiv(const Num1, Num2, Divisor: LongWord): LongWord;
asm
	// Num1 is in eax, Num2 is in edx, Divisor is in ecx
	mul     edx           // product now in edx:eax
	div     ecx           // quotient now in eax, remainder in edx
end {UMulMod};

{---------------------------------------------------------------------------
---
PURPOSE:     Multiply two unsigned 32-bit integers then take the modulus by
     another unsigned 32-bit integer.
DATE:        Created December 1998
     Modified 13 Apr 2001 to use LongWords instead of Integers
EXCEPTIONS:  EDivByZero   if the divisor is zero
     (See Note 1)
NOTES:       1. There is never an overflow error, since the answer always fits
    into 32 bits.
     2. This is useful in linear congruential generators.
----------------------------------------------------------------------------
--}

function UMulMod(const Num1, Num2, Modulus: LongWord): LongWord;
asm
	// Num1 is in eax, Num2 is in edx, Modulus is in ecx
	mul     edx           // product now in edx:eax
	div     ecx           // quotient now in eax, remainder in edx
	mov     eax,edx       // return the remainder
end {UMulMod};
{$ENDIF}

{--- Special Functions ---}

function InverseGamma(const X: Extended): Extended;
var
  C: array[1..26] of Extended;
  Z: Extended;
  XF: Extended;
  I: Integer;
begin
  C[1] := 1;
  C[2] := 0.5772156649015329;
  C[3] := -0.6558780715202538;
  C[4] := -0.0420026350340952;
  C[5] := 0.1665386113822915;
  C[6] := -0.0421977345555443;
  C[7] := -0.0096219715278770;
  C[8] := 0.0072189432466630;
  C[9] := -0.0011651675918591;
  C[10] := -0.0002152416741149;
  C[11] := 0.0001280502823882;
  C[12] := -0.0000201348547807;
  C[13] := -0.0000012504934821;
  C[14] := 0.0000011330272320;
  C[15] := -0.0000002056338417;
  C[16] := 0.0000000061160950;
  C[17] := 0.0000000050020075;
  C[18] := -0.0000000011812746;
  C[19] := 0.0000000001043427;
  C[20] := 0.0000000000077823;
  C[21] := -0.0000000000036968;
  C[22] := 0.0000000000005100;
  C[23] := -0.0000000000000206;
  C[24] := -0.0000000000000054;
  C[25] := 0.0000000000000014;
  C[26] := 0.0000000000000001;
  Result := 0;
  Z := 1;
  XF := Frac(X);
  if XF = 0 then
    XF := Sgn(X);
  for I := 1 to 26 do
  begin
    Z := Z * XF;
    Result := Result + C[I] * Z;
  end;
  if X > 0 then
  begin
    while XF < X do
    begin
      Result := Result / XF;
      XF := XF + 1;
    end;
  end
  else if X < 0 then
  begin
    while XF > X do
    begin
      XF := XF - 1;
      Result := XF * Result;
    end;
  end
end;

function Gamma(const X: Extended): Extended;
begin
  Result := InverseGamma(X);
  if abs(Result) < 1e-4000 then
    raise EMathError.Create('Not Defined for given Value')
  else
    Result := 1 / Result;
end;

{ Logarithm to base e of the gamma function.

  Accurate to about 1.e-14.
  Programmer: Alan Miller

  Latest revision of Fortran 77 version - 28 February 1988
}

function LnGamma(const X: Extended): Extended;
const
  A1 = -4.166666666554424E-02;
  A2 = 2.430554511376954E-03;
  A3 = -7.685928044064347E-04;
  A4 = 5.660478426014386E-04;
var
  Temp, Arg, Product: Extended;
  Reflect: Boolean;
begin
  //  lngamma is not defined if x = 0 or a negative integer.
  if FloatIsZero(X) or FloatIsNegative(X) and (Abs(X - Int(X)) < ESBTolerance)
    then
    raise EMathError.Create('Invalid Value');

  // If X < 0, use the reflection formula:
  //        gamma(x) * gamma(1-x) = pi * cosec(pi.x)

  Reflect := X < 0.0;
  if Reflect then
    Arg := 1.0 - X
  else
    Arg := X;

  // Increase the argument, if necessary, to make it > 10.

  Product := 1.0;
  while (Arg <= 10.0) do
  begin
    Product := Product * Arg;
    Arg := Arg + 1.0;
  end;

  // Use a polynomial approximation to Stirling's formula.
  // N.B. The real Stirling's formula is used here, not the simpler, but less
  //     accurate formula given by De Moivre in a letter to Stirling, which
  //     is the one usually quoted.

  Arg := Arg - 0.5;
  Temp := 1.0 / Sqr(Arg);
  Result := LnRt2Pi + Arg * (Ln(Arg) - 1.0 +
    (((A4 * Temp + A3) * Temp + A2) * Temp + A1) * Temp) - Ln(Product);

  if Reflect then
  begin
    Temp := Sin(ESBPi * X);
    Result := Ln(ESBPi / Temp) - Result;
  end;
end;

function Beta(const X, Y: Extended): Extended;
var
  R1, R2: Extended;
begin
  R1 := InverseGamma(X);
  R2 := InverseGamma(Y);
  if FloatIsZero(R1) or FloatIsZero(R2) then
    raise EMathError.Create('Not Defined for given Value')
  else
    Result := InverseGamma(X + Y) / (R1 * R2);
end;

function IncompleteBeta(X: Extended; P, Q: Extended): Extended;
{ Adapted From Collected Algorithms from CACM
 Algorithm 179 - Incomplete Beta Function Ratios
 Oliver G Ludwig
}
const
  Epsilon: Extended = 0.5e-18;
  MaxIterations = 1000;
var
  FinSum, InfSum, Temp, Temp1, Term, Term1, QRecur, Index: Extended;
  I: Integer;
  Alter: Boolean;
begin
  if (P <= 0) or (Q <= 0) or (X < 0) or (X > 1) then
    raise EMathError.Create('Not Defined for given Value')
  else
  begin
    if (X = 0) or (X = 1) then
      Result := X
    else
    begin
      // Interchange arguments if necessary to get better convergence
      if X <= 0.5 then
        Alter := False
      else
      begin
        Alter := True;
        SwapExt(P, Q);
        X := 1 - X;
      end;

      // Recurs on the (effective) Q until the Power Series doesn't alternate
      FinSum := 0;
      Term := 1;
      Temp := 1 - X;
      QRecur := Q;
      Index := Q;
      repeat
        Index := Index - 1;
        if Index <= 0 then
          Break;
        QRecur := Index;
        Term := Term * (QRecur + 1) / (Temp * (P + QRecur));
        FinSum := FinSum + Term;
      until False;

      // Sums a Power Series for non-integral effective Q and yields unity for integer Q
      InfSum := 1;
      Term := 1;
      for I := 1 to MaxIterations do
      begin
        if Term <= Epsilon then
          Break;
        Index := I;
        Term := Term * X * (Index - QRecur) * (P + Index - 1) /
          (Index * (P + Index));
        InfSum := InfSum + Term;
      end;

      // Evaluates Gammas
      Temp := Gamma(QRecur);
      Temp1 := Temp;
      Term := Gamma(QRecur + P);
      Term1 := Term;
      Index := QRecur;
      repeat
        Temp1 := Temp1 * Index;
        Term1 := Term1 * (Index + P);
        Index := Index + 1;
      until Index >= Q - 0.5;

      Temp := XtoY(X, P) * (InfSum * Term / (P * Temp) + FinSum * Term1
        * XtoY(1 - X, Q) / (Q * Temp1)) / Gamma(P);

      if Alter then
        Result := 1 - Temp
      else
        Result := Temp
    end;
  end;
end;

{------------------------------------------------------------------------
PURPOSE:     Calculate the greatest integral power of two that is less
     than or equal to the parameter
DATE:        April 2000
EXCEPTIONS:  EInvalidArgument for argument zero
------------------------------------------------------------------------}

function IGreatestPowerOf2(const N: LongWord): LongWord;
begin
  Result := LongWord(1) shl ILog2(N);
end {GreatestPowerOf2};

end.

