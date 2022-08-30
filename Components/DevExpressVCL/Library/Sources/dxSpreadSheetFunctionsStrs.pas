{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxSpreadSheetFunctionsStrs;

{$I cxVer.Inc}

interface

uses
  dxCore, cxClasses, Graphics;

resourcestring
  // Categories
  sfnCategoryCommon = 'Common';
  sfnCategoryCompatibility = 'Compatibility';
  sfnCategoryCube = 'Cube';
  sfnCategoryDatabase = 'Database';
  sfnCategoryDateTime = 'Date & Time';
  sfnCategoryEngineering = 'Engineering';
  sfnCategoryFinancial = 'Financial';
  sfnCategoryInformation = 'Information';
  sfnCategoryLogical = 'Logical';
  sfnCategoryLookupAndReference = 'Lookup & Reference';
  sfnCategoryMath = 'Math & Trig';
  sfnCategoryStatistical = 'Statistical';
  sfnCategoryText = 'Text';

  // General
  sfnParamArray = 'array';
  sfnParamValue = 'value';

  // Information function names
  sfnCell = 'CELL';
  sfnCellDescription = 'Returns information about the cell’s formatting, location, or content.';
  sfnError_Type = 'ERROR.TYPE';
  sfnError_TypeDescription = 'Returns a number corresponding to the specified error code.';
  sfnInfo = 'INFO';
  sfnInfoDescription = 'Returns information about the current operating environment.';
  sfnIsBlank = 'ISBLANK';
  sfnIsBlankDescription = 'Returns TRUE if cell is empty.';
  sfnIsErr = 'ISERR';
  sfnIsErrDescription = 'Returns TRUE if the cell contains any error code except #N/A.';
  sfnIsError = 'ISERROR';
  sfnIsErrorDescription = 'Returns TRUE if the cell contains any error code (#N/A, #VALUE!, #REF!, #DIV/0!, #NUM!, #NAME?, or #NULL!).';
  sfnIsEven = 'ISEVEN';
  sfnIsEvenDescription = 'Returns TRUE if the number is even.';
  sfnIsFormula = 'ISFORMULA';
  sfnIsFormulaDescription = 'Returns TRUE if the cell contains a formula expression.';
  sfnIsLogical = 'ISLOGICAL';
  sfnIsLogicalDescription = 'Returns TRUE if the specified value refers to a logical value.';
  sfnIsNA = 'ISNA';
  sfnIsNADescription = 'Returns TRUE if the cell contains the #N/A (a value is not available) error code.';
  sfnIsNonText = 'ISNONTEXT';
  sfnIsNonTextDescription = 'Returns TRUE if the cell does not contain text. Returns TRUE for blank cells.';
  sfnIsNumber = 'ISNUMBER';
  sfnIsNumberDescription = 'Returns TRUE if the cell contains a number.';
  sfnIsOdd = 'ISODD';
  sfnIsOddDescription = 'Returns TRUE if the number is odd.';
  sfnIsRef = 'ISREF';
  sfnIsRefDescription = 'Returns TRUE if the cell contains a reference.';
  sfnIsText = 'ISTEXT';
  sfnIsTextDescription = 'Returns TRUE if the specified cell contains text.';
  sfnN = 'N';
  sfnNDescription = 'Returns a value converted to a number.';
  sfnNA = 'NA';
  sfnNADescription = 'Returns the error code #N/A.';
  sfnSheet = 'SHEET';
  sfnSheetDescription = 'Returns the referenced sheet’s number.';
  sfnSheets = 'SHEETS';
  sfnSheetsDescription = 'Returns the number of sheets in a reference.';
  sfnType = 'TYPE';
  sfnTypeDescription = 'Returns the specified value’s type.';

  // Math and Trigonometry function names
  sfnAbs = 'ABS';
  sfnAbsDescription = 'Returns the absolute value.';
  sfnAcos = 'ACOS';
  sfnAcosDescription = 'Returns the arccosine.';
  sfnAcosh = 'ACOSH';
  sfnAcoshDescription = 'Returns the inverse hyperbolic cosine.';
  sfnAcot = 'ACOT';
  sfnAcotDescription = 'Returns the principal value of the arccotangent.';
  sfnAcoth = 'ACOTH';
  sfnAcothDescription = 'Returns the inverse hyperbolic cotangent.';
  sfnAggregate = 'AGGREGATE';
  sfnAggregateDescription = 'Returns an aggregate in the specified list with an option to ignore hidden rows and errors.';
  sfnArabic = 'ARABIC';
  sfnArabicDescription = 'Converts a Roman numeral to an Arabic numeral.';
  sfnAsin = 'ASIN';
  sfnAsinDescription = 'Returns the arcsine.';
  sfnAsinh = 'ASINH';
  sfnAsinhDescription = 'Returns the inverse hyperbolic sine.';
  sfnAtan = 'ATAN';
  sfnAtanDescription = 'Returns the arctangent.';
  sfnAtan2 = 'ATAN2';
  sfnAtan2Description = 'Returns the arctangent using the specified X and Y coordinates.';
  sfnAtanh = 'ATANH';
  sfnAtanhDescription = 'Returns the inverse hyperbolic tangent.';
  sfnBase = 'BASE';
  sfnBaseDescription = 'Converts a number into a text representation with the specified base (radix).';
  sfnCeiling = 'CEILING';
  sfnCeilingDescription = 'Rounds the value up to the nearest multiple based on the specified significance.';
  sfnCeiling_Math = 'CEILING.MATH';
  sfnCeiling_MathDescription = 'Rounds a number up to the nearest integer or to the nearest multiple of significance.';
  sfnCeiling_Precise = 'CEILING.PRECISE';
  sfnCeiling_PreciseDescription = 'Returns a number that is rounded up to the nearest integer or to the nearest multiple of significance.';
  sfnCombin = 'COMBIN';
  sfnCombinDescription = 'Returns the number of combinations for the specified number of items.';
  sfnCombinA = 'COMBINA';
  sfnCombinADescription = 'Returns the number of combinations for the specified number of items (including repetitions).';
  sfnCos = 'COS';
  sfnCosDescription = 'Returns the cosine.';
  sfnCosh = 'COSH';
  sfnCoshDescription = 'Returns the hyperbolic cosine.';
  sfnCot = 'COT';
  sfnCotDescription = 'Returns the cotangent.';
  sfnCoth = 'COTH';
  sfnCothDescription = 'Returns the hyperbolic cotangent.';
  sfnCsc = 'CSC';
  sfnCscDescription = 'Returns the cosecant.';
  sfnCsch = 'CSCH';
  sfnCschDescription = 'Returns the hyperbolic cosecant.';
  sfnDecimal = 'DECIMAL';
  sfnDecimalDescription = 'Converts a text representation of a number into a number using the specified base (radix).';
  sfnDegrees = 'DEGREES';
  sfnDegreesDescription = 'Converts radians to degrees.';
  sfnEven = 'EVEN';
  sfnEvenDescription = 'Rounds the specified value up to the nearest even integer.';
  sfnExp = 'EXP';
  sfnExpDescription = 'Returns the exponent of the specified value.';
  sfnFact = 'FACT';
  sfnFactDescription = 'Returns the factorial.';
  sfnFactDouble = 'FACTDOUBLE';
  sfnFactDoubleDescription = 'Returns the double factorial.';
  sfnFloor = 'FLOOR';
  sfnFloorDescription = 'Rounds the value down to the nearest multiple of the specified significance.';
  sfnFloor_Math = 'FLOOR.MATH';
  sfnFloor_MathDescription = 'Rounds the number down to the nearest integer or to the nearest multiple of significance.';
  sfnFloor_Precise = 'FLOOR.PRECISE';
  sfnFloor_PreciseDescription = 'Returns a number that is rounded up to the nearest integer or to the nearest multiple of significance.';
  sfnGCD = 'GCD';
  sfnGCDDescription = 'Returns the greatest common divisor of two or more integer values.';
  sfnInt = 'INT';
  sfnIntDescription = 'Rounds the value down to the nearest integer.';
  sfnIso_Ceiling = 'ISO.CEILING';
  sfnIso_CeilingDescription =
                     'Rounds the value up to the nearest integer or to the nearest multiple of significance. The function always rounds the specified ' +
                     'numeric value up.';
  sfnLCM = 'LCM';
  sfnLCMDescription = 'Returns the least common multiple of integer values.';
  sfnLn = 'LN';
  sfnLnDescription = 'Returns the natural logarithm.';
  sfnLog = 'LOG';
  sfnLogDescription = 'Returns the value’s logarithm to the specified base.';
  sfnLog10 = 'LOG10';
  sfnLog10Description = 'Returns the value’s base-10 logarithm.';
  sfnMDeterm = 'MDETERM';
  sfnMDetermDescription = 'Calculates the determinant for the specified matrix.';
  sfnMInverse = 'MINVERSE';
  sfnMInverseDescription = 'Returns the inverse matrix for the matrix stored in the specified array.';
  sfnMMult = 'MMULT';
  sfnMMultDescription = 'Returns the matrix product of two arrays.';
  sfnMod = 'MOD';
  sfnModDescription = 'Returns the remainder after one specified number is divided by the other.';
  sfnMRound = 'MROUND';
  sfnMRoundDescription = 'Rounds a numeric value to the specified multiple.';
  sfnMultiNomial = 'MULTINOMIAL';
  sfnMultiNomialDescription = 'Returns the multinomial of a set of numbers.';
  sfnMUnit = 'MUNIT';
  sfnMUnitDescription = 'Returns the unit matrix for the  specified dimension.';
  sfnOdd = 'ODD';
  sfnOddDescription = 'Rounds the numeric value up to the nearest integer.';
  sfnPi = 'PI';
  sfnPiDescription = 'Returns the value of Pi.';
  sfnPower = 'POWER';
  sfnPowerDescription = 'Raises the numeric value to the specified power.';
  sfnProduct = 'PRODUCT';
  sfnProductDescription = 'Multiplies all the parameter values and returns the product.';
  sfnQuotient = 'QUOTIENT';
  sfnQuotientDescription = 'Discards the remainder of a division and returns its integer portion.';
  sfnRadians = 'RADIANS';
  sfnRadiansDescription = 'Converts degrees to radians.';
  sfnRand = 'RAND';
  sfnRandDescription = 'Returns a random number within the range between 0 and 1, inclusive.';
  sfnRandBetween = 'RANDBETWEEN';
  sfnRandBetweenDescription = 'Returns a random integer within the specified range.';
  sfnRoman = 'ROMAN';
  sfnRomanDescription = 'Converts an Arabic numeral to a Roman numeral (as a text string).';
  sfnRound = 'ROUND';
  sfnRoundDescription = 'Rounds a numeric value to the specified number of digits.';
  sfnRoundDown = 'ROUNDDOWN';
  sfnRoundDownDescription = 'Rounds a numeric value towards zero.';
  sfnRoundUp = 'ROUNDUP';
  sfnRoundUpDescription = 'Rounds a numeric value towards infinity.';
  sfnSec = 'SEC';
  sfnSecDescription = 'Returns the secant.';
  sfnSech = 'SECH';
  sfnSechDescription = 'Returns the hyperbolic secant.';
  sfnSeriesSum = 'SERIESSUM';
  sfnSeriesSumDescription = 'Returns the sum of a power series based on the specified set of coefficients.';
  sfnSign = 'SIGN';
  sfnSignDescription = 'Returns the specified number’s sign.';
  sfnSin = 'SIN';
  sfnSinDescription = 'Returns the sine.';
  sfnSinh = 'SINH';
  sfnSinhDescription = 'Returns the hyperbolic sine.';
  sfnSqrt = 'SQRT';
  sfnSqrtDescription = 'Returns the square root.';
  sfnSqrtPi = 'SQRTPI';
  sfnSqrtPiDescription = 'Returns the square root of Pi multiplied by the specified value.';
  sfnSubTotal = 'SUBTOTAL';
  sfnSubTotalDescription = 'Returns a subtotal.';
  sfnSum = 'SUM';
  sfnSumDescription = 'Sums all the specified numeric values.';
  sfnSumIF = 'SUMIF';
  sfnSumIFDescription = 'Sums the numeric values within an array that meet a specific criterion.';
  sfnSumIFS = 'SUMIFS';
  sfnSumIFSDescription = 'Sums the numeric values within an array that meet multiple criteria.';
  sfnSumProduct = 'SUMPRODUCT';
  sfnSumProductDescription = 'Multiplies the corresponding numeric values in all specified arrays and sums the products.';
  sfnSumSQ = 'SUMSQ';
  sfnSumSQDescription = 'Sums the squares of values in a series of numbers.';
  sfnSumX2MY2 = 'SUMX2MY2';
  sfnSumX2MY2Description = 'Sums the differences of the corresponding squared values in two specified arrays.';
  sfnSumX2PY2 = 'SUMX2PY2';
  sfnSumX2PY2Description = 'Sums the sums of the corresponding squared values in two specified arrays.';
  sfnSumXMY2 = 'SUMXMY2';
  sfnSumXMY2Description = 'Sums the squares of differences of the corresponding values in two specified arrays.';
  sfnTan = 'TAN';
  sfnTanDescription = 'Returns the tangent.';
  sfnTanh = 'TANH';
  sfnTanhDescription = 'Returns the hyperbolic tangent.';
  sfnTrunc = 'TRUNC';
  sfnTruncDescription = 'Truncates a fractional part of the specified numeric value.';

  // Text function names
  sfnASC = 'ASC';
  sfnASCDescription =
                 'Replaces the full-width (that is, double-byte) English letters or katakana with the corresponding half-width (that is, ' +
                 'single-byte) characters.';
  sfnBahtText = 'BAHTTEXT';
  sfnBahtTextDescription = 'Converts a number to text, using the “baht?currency format.';
  sfnChar = 'CHAR';
  sfnCharDescription =
                 'Returns the character specified y a number. Use CHAR to translate code page numbers you might get from files on other types of ' +
                 'computers into characters.';
  sfnClean = 'CLEAN';
  sfnCleanDescription =
                 'Removes all non-printable characters from text. Use CLEAN on text imported from other applications that contains characters ' +
                 'that may not print with your operating system.';
  sfnCode = 'CODE';
  sfnCodeDescription =
                 'Returns a numeric code for the first character in a text string. The returned code corresponds to the character set used by ' +
                 'your computer.';
  sfnConcatenate = 'CONCATENATE';
  sfnConcatenateDescription = 'Joins several text strings in one text string. An alternative to ??';
  sfnDBCS = 'DBCS';
  sfnDBCSDescription =
                 'Replaces the half-width (that is, single-byte) English characters or katakana with the corresponding full-width (that is, ' +
                 'double-byte) characters.';
  sfnDollar = 'DOLLAR';
  sfnDollarDescription =
                 'Converts the number to text using currency format $#,##0.00_);($#,##0.00), with the decimals rounded to the specified number of ' +
                 'places.';
  sfnExact = 'EXACT';
  sfnExactDescription =
                 'Compares two text strings and returns TRUE if they are exactly the same, FALSE otherwise. EXACT is case-sensitive but ignores ' +
                 'formatting differences. Use EXACT to test text being entered into a document.';
  sfnFind = 'FIND';
  sfnFindDescription =
                 'FIND locates one text string within a second text string, and returns the number of the starting position of the first text ' +
                 'string from the first character of the second text string. The single-byte version.';
  sfnFindB = 'FINDB';
  sfnFindBDescription =
                 'FINDB locates one text string within a second text string, and returns the number of the starting position of the first text ' +
                 'string from the first character of the second text string. The double-byte version.';
  sfnFixed = 'FIXED';
  sfnFixedDescription =
                 'Rounds the first parameter to the number of decimals determined by the second parameter and returns it as a string. The third ' +
                 'parameter specifies whether to omit commas in the output string.';
  sfnLeft = 'LEFT';
  sfnLeftDescription =
                 'Returns the first character or characters in a text string. The second parameter defines the number of characters to extract. ' +
                 'The single-byte version.';
  sfnLeftB = 'LEFTB';
  sfnLeftBDescription =
                 'Returns the first character or characters in a text string. The second parameter defines the number of characters to extract. ' +
                 'The double-byte version.';
  sfnLen = 'LEN';
  sfnLenDescription = 'Returns the specified text string’s length, in characters. The single-byte version.';
  sfnLenB = 'LENB';
  sfnLenBDescription = 'Returns the specified text string’s length, in characters. The double-byte version.';
  sfnLower = 'LOWER';
  sfnLowerDescription = 'Converts a text string to lowercase.';
  sfnMid = 'MID';
  sfnMidDescription = 'Returns the substring of the specified text string. The single-byte version.';
  sfnMidB = 'MIDB';
  sfnMidBDescription = 'Returns the substring of the specified text string. The double-byte version.';
  sfnNumberValue = 'NUMBERVALUE';
  sfnNumberValueDescription = 'Converts text to a number using a locale-independent algorithm.';
  sfnPhonetic = 'PHONETIC';
  sfnPhoneticDescription = 'Extracts the phonetic (furigana) characters from a text string.';
  sfnProper = 'PROPER';
  sfnProperDescription =
                 'Capitalizes the first letter in a text string any other letters that follow any non-letter character. Converts all other letters' +
                 ' to lowercase.';
  sfnReplace = 'REPLACE';
  sfnReplaceDescription =
                 'Replaces a part of a text string with the specified text, based on the provided text size and character position. The ' +
                 'single-byte version.';
  sfnReplaceB = 'REPLACEB';
  sfnReplaceBDescription =
                 'Replaces a part of a text string with the specified text, based on the provided text size and character position. The ' +
                 'double-byte version.';
  sfnRept = 'REPT';
  sfnReptDescription = 'Repeats the text string a specific number of times.';
  sfnRight = 'RIGHT';
  sfnRightDescription = 'Returns one or more last characters in a text string. The single-byte version.';
  sfnRightB = 'RIGHTB';
  sfnRightBDescription = 'Returns one or more last characters in a text string. The double-byte version.';
  sfnSearch = 'SEARCH';
  sfnSearchDescription = 'Searches one text string within another. The single-byte version.';
  sfnSearchB = 'SEARCHB';
  sfnSearchBDescription = 'Searches one text string within another. The double-byte version.';
  sfnSubstitute = 'SUBSTITUTE';
  sfnSubstituteDescription = 'Substitutes a specific text with another text in a string.';
  sfnT = 'T';
  sfnTDescription = 'Returns text to which the specified value refers.';
  sfnText = 'TEXT';
  sfnTextDescription = 'Formats a specific value as text.';
  sfnTrim = 'TRIM';
  sfnTrimDescription = 'Removes all spaces from text, except for single spaces between individual words.';
  sfnUniChar = 'UNICHAR';
  sfnUniCharDescription = 'Returns the character corresponding to the specified number in the Unicode table.';
  sfnUniCode = 'UNICODE';
  sfnUniCodeDescription = 'Returns the code corresponding to the first character in the specified text string.';
  sfnUpper = 'UPPER';
  sfnUpperDescription = 'Converts a text string to uppercase.';
  sfnValue = 'VALUE';
  sfnValueDescription = 'Converts a text string to a numeric value.';

  // Statistical function names
  sfnAveDev = 'AVEDEV';
  sfnAveDevDescription =
                      'Returns the average of the absolute deviations of a numeric value series from their mean. The calculated value is a measure of ' +
                      'the data set variability.';
  sfnAverage = 'AVERAGE';
  sfnAverageDescription = 'Calculates the average value of a numeric value series.';
  sfnAverageA = 'AVERAGEA';
  sfnAverageADescription = 'Calculates the average value of numeric values in all non-empty cells.';
  sfnAverageIF = 'AVERAGEIF';
  sfnAverageIFDescription = 'Returns the average (that is, the arithmetic mean) of all cells within the cell range that meet a specific criterion.';
  sfnAverageIFS = 'AVERAGEIFS';
  sfnAverageIFSDescription = 'Returns the average (that is, the arithmetic mean) of all cells that meet multiple criteria.';
  sfnBeta_Dist = 'BETA.DIST';
  sfnBeta_DistDescription =
                      'Returns the cumulative beta probability density distribution. The beta distribution is useful to study variation of a specific ' +
                      'indicator (as a percentage) across samples.';
  sfnBeta_Inv = 'BETA.INV';
  sfnBeta_InvDescription = 'Returns the inverse of the cumulative beta probability density function for the specified beta distribution probability.';
  sfnBinom_Dist = 'BINOM.DIST';
  sfnBinom_DistDescription =
                      'Returns the individual term binomial distribution probability. You can use this function to evaluate a fixed number of independent ' +
                      'trials that can result only in success or failure, provided that the probability of success does not change throughout the ' +
                      'experiment.';
  sfnBinom_Dist_Range = 'BINOM.DIST.RANGE';
  sfnBinom_Dist_RangeDescription = 'Returns the probability of a trial’s result by using a binomial distribution.';
  sfnBinom_Inv = 'BINOM.INV';
  sfnBinom_InvDescription = 'Returns the smallest value for which the cumulative binomial distribution is greater than or equals a criterion value.';
  sfnChiSQ_Dist = 'CHISQ.DIST';
  sfnChiSQ_DistDescription =
                      'Returns the chi-squared distribution which is commonly used to study variation in the percentage of a specific indicator across ' +
                      'samples.';
  sfnChiSQ_Dist_RT = 'CHISQ.DIST.RT';
  sfnChiSQ_Dist_RTDescription =
                      'Returns the right-tailed probability of the chi-squared distribution. You can use this function to calculate the variation of a ' +
                      'certain indicator across the experimental data.';
  sfnChiSQ_Inv = 'CHISQ.INV';
  sfnChiSQ_InvDescription = 'Returns the inverse of the left-tailed probability of the chi-squared distribution.';
  sfnChiSQ_Inv_RT = 'CHISQ.INV.RT';
  sfnChiSQ_Inv_RTDescription = 'Iteratively calculates the inverse of the right-tailed probability of the chi-squared distribution.';
  sfnChiSQ_Test = 'CHISQ.TEST';
  sfnChiSQ_TestDescription =
                      'Returns the test for independence as the value from the chi-squared distribution for the statistic and the appropriate degrees ' +
                      'of freedom. You can use this function to identify if the specific hypothesis results are verified by an experiment.';
  sfnConfidence_Norm = 'CONFIDENCE.NORM';
  sfnConfidence_NormDescription = 'Returns the confidence interval for a population mean using a normal distribution.';
  sfnConfidence_T = 'CONFIDENCE.T';
  sfnConfidence_TDescription = 'Returns the confidence interval for a population mean using a Student’s T-distribution.';
  sfnCorrel = 'CORREL';
  sfnCorrelDescription =
                      'Returns the correlation coefficient between two specified arrays of numeric values. Use this function to determine the ' +
                      'relationship between data sets.';
  sfnCount = 'COUNT';
  sfnCountDescription = 'Returns the number of cells that contain numbers or the total number of numeric values within the specified array.';
  sfnCountA = 'COUNTA';
  sfnCountADescription = 'Returns the total number of non-empty cells within the specified cell range.';
  sfnCountBlank = 'COUNTBLANK';
  sfnCountBlankDescription = 'Returns the total number of blank cells within the specified cell range.';
  sfnCountIF = 'COUNTIF';
  sfnCountIFDescription = 'Returns the number of cells within the specified range that meet a specific criterion.';
  sfnCountIFS = 'COUNTIFS';
  sfnCountIFSDescription = 'Returns the number of cells within the specified range that meet multiple criteria.';
  sfnCovariance_P = 'COVARIANCE.P';
  sfnCovariance_PDescription =
                      'Calculates the population average of the products of deviations (that is, population covariance) for each pair of values in the ' +
                      'specified series of numeric values.';
  sfnCovariance_S = 'COVARIANCE.S';
  sfnCovariance_SDescription =
                      'Calculates the sample average of the products of deviations (that is, sample covariance) for each pair of values in the specified ' +
                      'series of numeric values.';
  sfnDevSQ = 'DEVSQ';
  sfnDevSQDescription = 'Returns the sum of squares of deviations of a numbers in an array from their sample mean.';
  sfnExpon_Dist = 'EXPON.DIST';
  sfnExpon_DistDescription = 'Returns the exponential distribution. This function is useful if you need to evaluate the process duration probabilities.';
  sfnF_Dist = 'F.DIST';
  sfnF_DistDescription = 'Returns the F probability distribution.';
  sfnF_Dist_RT = 'F.DIST.RT';
  sfnF_Dist_RTDescription =
                      'Returns the right-tailed F probability distribution. This function is useful if you need to identify if two data sets have ' +
                      'different degrees of diversity.';
  sfnF_Inv = 'F.INV';
  sfnF_InvDescription = 'Returns the inverse of the F probability distribution.';
  sfnF_Inv_RT = 'F.INV.RT';
  sfnF_Inv_RTDescription =
                      'Returns the inverse of the right-tailed F probability distribution. You can use the calculated result in an F-test to compare ' +
                      'the degree of variability in two data sets.';
  sfnF_Test = 'F.TEST';
  sfnF_TestDescription =
                      'Performs an F-test for two specified sets (arrays) of numeric values, returning the two-tailed probability that the variances in' +
                      ' both arrays are similar.';
  sfnFisher = 'FISHER';
  sfnFisherDescription = 'Returns the Fisher transformation.';
  sfnFisherInv = 'FISHERINV';
  sfnFisherInvDescription = 'Returns the inverse of the Fisher transformation.';
  sfnForecast = 'FORECAST';
  sfnForecastDescription = 'Forecasts a future value in a linear trend by using existing pairs of X and Y values.';
  sfnFrequency = 'FREQUENCY';
  sfnFrequencyDescription = 'Returns a frequency distribution as a vertical array.';
  sfnGamma = 'GAMMA';
  sfnGammaDescription = 'Returns the Gamma function value.';
  sfnGamma_Dist = 'GAMMA.DIST';
  sfnGamma_DistDescription =
                      'Returns the gamma distribution. You can use this function to study a series of numbers that may have a skewed distribution. The ' +
                      'gamma distribution is commonly used in queuing analysis.';
  sfnGamma_Inv = 'GAMMA.INV';
  sfnGamma_InvDescription =
                      'Iteratively calculates the inverse of the gamma cumulative distribution. The gamma distribution is useful to study values that ' +
                      'have a skewed distribution.';
  sfnGammaLn = 'GAMMALN';
  sfnGammaLnDescription = 'Returns the natural logarithm of the Gamma function.';
  sfnGammaLn_Precise = 'GAMMALN.PRECISE';
  sfnGammaLn_PreciseDescription = 'Returns the natural logarithm of the Gamma function (a high precision version).';
  sfnGauss = 'GAUSS';
  sfnGaussDescription =
                      'Calculates the probability that a member of a standard normal population is between the mean and Z standard deviation from the ' +
                      'mean.';
  sfnGeomean = 'GEOMEAN';
  sfnGeomeanDescription = 'Returns the geometric mean of an array of positive numeric values.';
  sfnGrowth = 'GROWTH';
  sfnGrowthDescription = 'Calculates the predicted exponential growth by using the known numeric values.';
  sfnHarmean = 'HARMEAN';
  sfnHarmeanDescription =
                      'Returns the harmonic mean of a series of numeric values. The harmonic mean is the reciprocal of the arithmetic mean of ' +
                      'reciprocals.';
  sfnHypgeom_Dist = 'HYPGEOM.DIST';
  sfnHypgeom_DistDescription =
                      'Returns the hypergeometric distribution, that is, the probability of a specified number of sample successes at a specific sample' +
                      ' size, number of population successes, and population size.';
  sfnIntercept = 'INTERCEPT';
  sfnInterceptDescription =
                      'Calculates the point at which a line will intersect the Y-axis by using the known X and Y values. The intercept point is based ' +
                      'on a best-fit regression line plotted through the known pairs of numeric values.';
  sfnKurt = 'KURT';
  sfnKurtDescription =
                      'Returns the kurtosis of a series of numeric values. Kurtosis characterizes the relative peakedness or flatness of a distribution' +
                      ' compared to the normal distribution. Positive kurtosis indicates a relatively peaked distribution. Negative kurtosis corresponds' +
                      ' to a relatively flat distribution.';
  sfnLarge = 'LARGE';
  sfnLargeDescription = 'Returns the K-th largest value in a series of numeric values.';
  sfnLinest = 'LINEST';
  sfnLinestDescription =
                      'Calculates the statistics for a line by using the “least squares?method to calculate a straight line that best fits your data, ' +
                      'and returns an array that describes the line.';
  sfnLogest = 'LOGEST';
  sfnLogestDescription =
                      'Calculates an exponential curve that fits your data and returns an array of values that describes the curve. This array function' +
                      ' is used in regression analysis.';
  sfnLogNorm_Dist = 'LOGNORM.DIST';
  sfnLogNorm_DistDescription = 'Returns the cumulative lognormal distribution.';
  sfnLogNorm_Inv = 'LOGNORM.INV';
  sfnLogNorm_InvDescription = 'Calculates the inverse of the lognormal cumulative distribution.';
  sfnMax = 'MAX';
  sfnMaxDescription = 'Returns the maximum value in an array of numeric values.';
  sfnMaxA = 'MAXA';
  sfnMaxADescription = 'Returns the maximum value in an array, including numeric values, text, and logical values.';
  sfnMedian = 'MEDIAN';
  sfnMedianDescription = 'Returns the median of a series of numeric values.';
  sfnMin = 'MIN';
  sfnMinDescription = 'Returns the minimum value in an array of numeric values.';
  sfnMinA = 'MINA';
  sfnMinADescription = 'Returns the minimum value in an array, including numeric values, text, and logical values.';
  sfnMode_Mult = 'MODE.MULT';
  sfnMode_MultDescription = 'Returns a vertical array populated with the most frequently occurring (repetitive) values in an array of numeric values.';
  sfnMode_SNGL = 'MODE.SNGL';
  sfnMode_SNGLDescription =
                      'Calculates the mode of a group of numbers, that is, returns the most frequently occurring (repetitive) value among all specified' +
                      ' numeric values.';
  sfnNegBinom_Dist = 'NEGBINOM.DIST';
  sfnNegBinom_DistDescription =
                      'Returns the negative binomial distribution, that is, the probability that there is a specific number of failures prior to ' +
                      'achieving a threshold number of successful trials at the constant probability of success.';
  sfnNorm_Dist = 'NORM.DIST';
  sfnNorm_DistDescription =
                      'Returns the normal distribution for the specified mean and standard deviation. This function is widely used in statistics, ' +
                      'including hypothesis testing.';
  sfnNorm_Inv = 'NORM.INV';
  sfnNorm_InvDescription = 'Iteratively calculates the inverse of the normal distribution for the specified mean and standard deviation.';
  sfnNorm_S_Dist = 'NORM.S.DIST';
  sfnNorm_S_DistDescription =
                      'Returns the standard normal cumulative distribution function. The standard distribution’s arithmetic mean and standard deviation' +
                      ' are 0 and 1, respectively.';
  sfnNorm_S_Inv = 'NORM.S.INV';
  sfnNorm_S_InvDescription =
                      'Iteratively calculates the inverse of the standard normal cumulative distribution. The standard distribution’s arithmetic mean ' +
                      'and standard deviation are 0 and 1, respectively.';
  sfnPearson = 'PEARSON';
  sfnPearsonDescription =
                      'Returns the Pearson product moment correlation coefficient (R), a dimensionless index that ranges within the range between -1.0 ' +
                      'and 1.0, inclusive, and reflects the extent of a linear relationship between two data sets.';
  sfnPercentile_Exc = 'PERCENTILE.EXC';
  sfnPercentile_ExcDescription = 'Returns the K-th percentile of values in an array of numbers, where K is the range between 0 and 1, exclusive.';
  sfnPercentile_Inc = 'PERCENTILE.INC';
  sfnPercentile_IncDescription = 'Returns the K-th percentile of values in an array of numbers.';
  sfnPercentRank_Exc = 'PERCENTRANK.EXC';
  sfnPercentRank_ExcDescription = 'Returns the rank of a value in a data set as a percentage (0..1, exclusive) of a data set.';
  sfnPercentRank_Inc = 'PERCENTRANK.INC';
  sfnPercentRank_IncDescription = 'Returns the percentage rank of the specified value in an array of numbers.';
  sfnPermut = 'PERMUT';
  sfnPermutDescription = 'Returns the number of permutations for the specified number of objects.';
  sfnPermutationA = 'PERMUTATIONA';
  sfnPermutationADescription =
                      'Returns the number of permutations for the specified number of objects (including repetitions) that can be selected from the ' +
                      'total number of objects.';
  sfnPHI = 'PHI';
  sfnPHIDescription = 'Returns the value of the density function for a standard normal distribution.';
  sfnPoisson_Dist = 'POISSON.DIST';
  sfnPoisson_DistDescription = 'Returns the Poisson distribution that is often used to predict the number of events occurring over  a specific time.';
  sfnProb = 'PROB';
  sfnProbDescription = 'Returns the probability that values in a range are between two limits.';
  sfnQuartile_Exc = 'QUARTILE.EXC';
  sfnQuartile_ExcDescription = 'Returns the quartile of a series of numeric values, based on percentile values from the range between 0 and 1, exclusive.';
  sfnQuartile_Inc = 'QUARTILE.INC';
  sfnQuartile_IncDescription = 'Returns the quartile of a series of numeric values.';
  sfnRank_Avg = 'RANK.AVG';
  sfnRank_AvgDescription =
                      'Returns the rank of a specified number in a series of values. The number’s rank (magnitude) is relative to other values in the ' +
                      'list. The function returns the average rank if more than one value has the same rank.';
  sfnRank_Eq = 'RANK.EQ';
  sfnRank_EqDescription =
                      'Returns the rank of a specified number in a series of values. The rank is a magnitude of a numeric value in relation to all other' +
                      ' values in the source array. The rank matches the value’s number in the sorted array.';
  sfnRSQ = 'RSQ';
  sfnRSQDescription = 'Returns the square of the Pearson product moment correlation coefficient through data points.';
  sfnSkew = 'SKEW';
  sfnSkewDescription = 'Returns the skewness of a distribution. Skewness characterizes the degree of asymmetry of a distribution around its mean.';
  sfnSkew_P = 'SKEW.P';
  sfnSkew_PDescription =
                      'Returns the skewness of a distribution based on a population: a characterization of the degree of asymmetry of a distribution ' +
                      'around its mean.';
  sfnSlope = 'SLOPE';
  sfnSlopeDescription = 'Returns the slope of the linear regression line.';
  sfnSmall = 'SMALL';
  sfnSmallDescription = 'Returns the K-th smallest value in a series of numeric values.';
  sfnStandardize = 'STANDARDIZE';
  sfnStandardizeDescription = 'Returns a normalized value from a distribution characterized by the mean and standard deviation values.';
  sfnStDev_P = 'STDEV.P';
  sfnStDev_PDescription =
                      'Calculates the standard deviation based on the entire population passed as an array of numeric values. The standard deviation ' +
                      'indicates how widely values within a series of numbers disperse from the average value (that is, the mean).';
  sfnStDev_S = 'STDEV.S';
  sfnStDev_SDescription =
                      'Estimates the standard deviation based on the specified array of numeric values (a sample of the population). The standard ' +
                      'deviation indicates how widely values within a series of numbers disperse from the average value (that is, the mean).';
  sfnStDevA = 'STDEVA';
  sfnStDevADescription =
                      'Estimates the standard deviation based on the specified array (a sample of the population), including numeric values, text, and ' +
                      'logical values.';
  sfnStDevPA = 'STDEVPA';
  sfnStDevPADescription =
                      'Calculates the standard deviation based on the entire population passed as an array, including numeric values, text, and logical' +
                      ' values.';
  sfnSTEYX = 'STEYX';
  sfnSTEYXDescription =
                      'Returns the standard error of the predicted Y-value for each X in the regression. The standard error is a measure of the amount ' +
                      'of error in the prediction of Y for an individual X.';
  sfnT_Dist = 'T.DIST';
  sfnT_DistDescription =
                      'Returns the Percentage Points (that is, the probability) for the Student’s T-distribution. The T-distribution is used in the ' +
                      'hypothesis testing of small sample data sets.';
  sfnT_Dist_2T = 'T.DIST.2T';
  sfnT_Dist_2TDescription =
                      'Returns the two-tailed Student’s T-distribution. You can use this function instead of a table of critical values for the ' +
                      'T-distribution.';
  sfnT_Dist_RT = 'T.DIST.RT';
  sfnT_Dist_RTDescription = 'Returns the right-tailed T-distribution.';
  sfnT_Inv = 'T.INV';
  sfnT_InvDescription = 'Returns the T-value of the Student’s T-distribution as a function of the probability and the degrees of freedom.';
  sfnT_Inv_2T = 'T.INV.2T';
  sfnT_Inv_2TDescription = 'Iteratively calculates the two-tailed inverse of the Student’s T-distribution.';
  sfnT_Test = 'T.TEST';
  sfnT_TestDescription = 'Returns the probability associated with a Student’s T-test.';
  sfnTrend = 'TREND';
  sfnTrendDescription = 'Returns values along a linear trend.';
  sfnTrimMean = 'TRIMMEAN';
  sfnTrimMeanDescription =
                      'Returns the mean of the interior of a series of numeric values. The function calculates the mean from an array whose top and ' +
                      'bottom tails are excluded.';
  sfnVar_P = 'VAR.P';
  sfnVar_PDescription = 'Calculates the variance based on the entire population specified as an array of numeric values.';
  sfnVar_S = 'VAR.S';
  sfnVar_SDescription = 'Estimates variance based on the specified array of numeric values (a sample of the population).';
  sfnVarA = 'VARA';
  sfnVarADescription =
                      'Estimates variance based on the specified array (a sample of the population), including numeric values, text, and logical values.';
  sfnVarPA = 'VARPA';
  sfnVarPADescription =
                      'Calculates the variance based on the entire population specified as an array, including numeric values, text, and logical values.';
  sfnWeibull_Dist = 'WEIBULL.DIST';
  sfnWeibull_DistDescription =
                      'Returns the Weibull distribution used in reliability analysis. You can use this function to calculate a device’s MBTF (mean time' +
                      ' between failures).';
  sfnZ_Test = 'Z.TEST';
  sfnZ_TestDescription =
                      'Returns the one-tailed probability associated with a Z-test, that is the probability that the sample mean is greater than the ' +
                      'average of all numeric values in the source array.';

  // Compatibility function names
  sfnBetaDist = 'BETADIST';
  sfnBetaDistDescription =
                      'Returns the cumulative beta probability density function. The beta distribution is useful to study variation of a specific ' +
                      'indicator (as a percentage) across samples.';
  sfnBetaInv = 'BETAINV';
  sfnBetaInvDescription = 'Returns the inverse of the cumulative beta probability density function for the specified beta distribution probability.';
  sfnBinomDist = 'BINOMDIST';
  sfnBinomDistDescription =
                      'Returns the individual term binominal distribution probability. You can use this function to evaluate a mixed number of ' +
                      'independent trials that can result only in success or failure, provided that the probability of success does not change throughout ' +
                      'the experiment.';
  sfnChiDist = 'CHIDIST';
  sfnChiDistDescription =
                      'Returns the two-tailed probability of the chi-squared distribution. You can use this function to calculate the variation of a ' +
                      'certain indicator across the experimental data.';
  sfnChiInv = 'CHIINV';
  sfnChiInvDescription = 'Iteratively calculates the inverse of the right-tailed probability of the chi-squared distribution.';
  sfnChiTest = 'CHITEST';
  sfnChiTestDescription =
                      'Returns the test for independence as the value from the chi-squared distribution for the statistic and the appropriate degrees ' +
                      'of freedom. You can use this function to identify if the specified hypothetic results are verified by an experiment.';
  sfnConfidence = 'CONFIDENCE';
  sfnCovar = 'COVAR';
  sfnCovarDescription =
                      'Calculates the average of the products of deviations (that is, covariance) for each pair of values in the specified arrays of ' +
                      'integers. You can use this function to identify the relationship between two sets of values.';
  sfnCritBinom = 'CRITBINOM';
  sfnCritBinomDescription = 'Calculates the lowest value for which the cumulative binomial distribution is equal to or greater than a specific value.';
  sfnExponDist = 'EXPONDIST';
  sfnExponDistDescription = 'Returns the exponential distribution. This function is useful if you need to evaluate the process duration probabilities.';
  sfnFDist = 'FDIST';
  sfnFDistDescription =
                      'Returns the right-tailed F probability distribution. This function is useful if you need to identify if two data sets have ' +
                      'different degrees of diversity.';
  sfnFInv = 'FINV';
  sfnFInvDescription =
                      'Returns the inverse of the right-tailed F probability distribution. You can use the calculated result in an F-test to compare ' +
                      'the degree of variability in two data sets.';
  sfnFTest = 'FTEST';
  sfnFTestDescription =
                      'Performs an F-test for two specified sets (arrays) of numeric values, returning the two-tailed probability that the variances in' +
                      ' both arrays are similar.';
  sfnGammaDist = 'GAMMADIST';
  sfnGammaDistDescription =
                      'Returns the gamma distribution. You can use this function to study a series of numbers that may have a skewed distribution. The ' +
                      'gamma distribution is commonly used in queuing analysis.';
  sfnGammaInv = 'GAMMAINV';
  sfnGammaInvDescription =
                      'Iteratively calculates the inverse of the gamma cumulative distribution. The gamma distribution is useful to study values that ' +
                      'have a skewed distribution.';
  sfnHypgeomDist = 'HYPGEOMDIST';
  sfnHypgeomDistDescription =
                      'Returns the hypergeometric distribution, that is, the probability of a specified number of sample successes at a specific sample' +
                      ' size, number of population successes, and population size.';
  sfnLogInv = 'LOGINV';
  sfnLogInvDescription = 'Calculates the inverse of the lognormal cumulative distribution.';
  sfnLogNormDist = 'LOGNORMDIST';
  sfnLogNormDistDescription = 'Returns the cumulative lognormal distribution.';
  sfnMode = 'MODE';
  sfnModeDescription =
                      'Calculates the mode of a group of numbers, that is, returns the most frequently occurring (repetitive) value among all specified' +
                      ' numeric values.';
  sfnNegBinomDist = 'NEGBINOMDIST';
  sfnNegBinomDistDescription =
                      'Returns the negative binomial distribution, that is, the probability that there is a specific number of failures prior achieving' +
                      ' a threshold number of successful trials at the constant probability of a success.';
  sfnNormDist = 'NORMDIST';
  sfnNormDistDescription =
                      'Returns the normal distribution for the specified mean and standard distribution. This function is widely used in statistics, ' +
                      'including hypothesis testing.';
  sfnNormInv = 'NORMINV';
  sfnNormInvDescription = 'Iteratively calculates the inverse of the normal distribution of the specified mean and standard deviation.';
  sfnNormSDist = 'NORMSDIST';
  sfnNormSDistDescription =
                      'Returns the standard normal cumulative distribution function. The standard distribution’s arithmetic mean and standard deviation' +
                      ' are 0 and 1, respectively.';
  sfnNormSInv = 'NORMSINV';
  sfnNormSInvDescription =
                      'Iteratively calculates the inverse of the standard normal cumulative distribution. The standard distribution’s arithmetic mean ' +
                      'and standard deviation are 0 and 1, respectively.';
  sfnPercentile = 'PERCENTILE';
  sfnPercentileDescription = 'Returns the K-th percentile of values in an array of numbers.';
  sfnPercentRank = 'PERCENTRANK';
  sfnPercentRankDescription = 'Returns the percentage rank of the specified value in an array of numbers.';
  sfnPoisson = 'POISSON';
  sfnPoissonDescription = 'Returns the Poisson distribution that is often used to predict the number of events occurring over a specific time.';
  sfnQuartile = 'QUARTILE';
  sfnQuartileDescription = 'Returns the quartile of a series of numeric values.';
  sfnRank = 'RANK';
  sfnRankDescription =
                      'Returns the rank of a specific number in a series of values. The rank is a magnitude of a numeric value in relation to all other' +
                      ' values in the source array. This rank matches the value’s number in a sorted array.';
  sfnStDev = 'STDEV';
  sfnStDevDescription =
                      'Estimates the standard deviation based on the specified array of numeric values (a sample of the population). The standard ' +
                      'deviation indicates how widely values within a series of numbers disperse from the average value (that is, the mean).';
  sfnStDevP = 'STDEVP';
  sfnStDevPDescription =
                      'Calculates the standard deviation based on the entire population specified as an array of numeric values. The standard deviation' +
                      ' indicates how widely values within a series of numbers disperse from the average value (that is, the mean).';
  sfnTDist = 'TDIST';
  sfnTDistDescription =
                      'Returns the Percentage Points (that is, the probability) for the Student T-distribution. The T-distribution is used in the ' +
                      'hypothesis testing of small sample data sets.';
  sfnTInv = 'TINV';
  sfnTInvDescription = 'Iteratively calculates the two-tailed inverse of the Student’s T-distribution.';
  sfnTTest = 'TTEST';
  sfnTTestDescription = 'Returns the probability associated with a Student’s T-test.';
  sfnVar = 'VAR';
  sfnVarDescription = 'Estimates variance based on the specified array of numeric values (a sample of the population).';
  sfnVarP = 'VARP';
  sfnVarPDescription = 'Calculates variance based on the entire population specified as an array of numeric values.';
  sfnWeibull = 'WEIBULL';
  sfnWeibullDescription =
                      'Returns the Weibull distribution used in reliability analysis. You can use this function to calculate a device’s MBTF (mean time' +
                      ' between failures).';
  sfnZTest = 'ZTEST';
  sfnZTestDescription =
                      'Returns the one-tailed probability associated with a Z-test, that is the probability that the sample mean is greater than the ' +
                      'average of all numeric values in the source array.';

  // Date and Time function names
  sfnDate = 'DATE';
  sfnDateDescription = 'Calculates the serial number corresponding to the specified date.';
  sfnDateValue = 'DATEVALUE';
  sfnDateValueDescription = 'Converts a date specified as a text string to the corresponding serial number.';
  sfnDay = 'DAY';
  sfnDayDescription = 'Converts a serial number to the corresponding date (day).';
  sfnDays = 'DAYS';
  sfnDaysDescription = 'Returns the number of days between two specified dates.';
  sfnDays360 = 'DAYS360';
  sfnDays360Description =
                      'Returns the number of days between two specified dates based on a 360-day year (that includes twelve 30-day months), which is ' +
                      'used in some accounting calculations.';
  sfnEDate = 'EDATE';
  sfnEDateDescription = 'Returns the serial number of the date that is the indicated number of months before or after the start date.';
  sfnEOMonth = 'EOMONTH';
  sfnEOMonthDescription = 'Returns the serial number of the last day of the month before or after a specified number of months.';
  sfnHour = 'HOUR';
  sfnHourDescription = 'Converts a serial number to the corresponding hour.';
  sfnIsoWeekNum = 'ISOWEEKNUM';
  sfnIsoWeekNumDescription = 'Returns the ISO number of the week to which the specified date belongs.';
  sfnMinute = 'MINUTE';
  sfnMinuteDescription = 'Converts a serial number to the corresponding minute.';
  sfnMonth = 'MONTH';
  sfnMonthDescription = 'Converts a serial number to the corresponding month.';
  sfnNetworkDays = 'NETWORKDAYS';
  sfnNetworkDaysDescription = 'This function is useful for calculating employee benefits that accrue based on the number of days worked under specific terms.';
  sfnNetworkDays_Intl = 'NETWORKDAYS.INTL';
  sfnNetworkDays_IntlDescription = 'Returns the number of whole workdays between two specified dates.';
  sfnNow = 'NOW';
  sfnNowDescription = 'Returns the current time as a date/time value.';
  sfnSecond = 'SECOND';
  sfnSecondDescription = 'Converts a serial number to the corresponding second.';
  sfnTime = 'TIME';
  sfnTimeDescription = 'Converts the specified time to the corresponding serial number.';
  sfnTimeValue = 'TIMEVALUE';
  sfnTimeValueDescription = 'Converts a text time representation into a date/time value.';
  sfnToday = 'TODAY';
  sfnTodayDescription = 'Returns the serial number corresponding to the current date.';
  sfnWeekDay = 'WEEKDAY';
  sfnWeekDayDescription = 'Returns the day of the week corresponding to the specified date value.';
  sfnWeekNum = 'WEEKNUM';
  sfnWeekNumDescription = 'Returns the week number corresponding to the specified date.';
  sfnWorkDay = 'WORKDAY';
  sfnWorkDayDescription =
                      'Returns the workday separated from the initial date by the specified number of workdays. The returned day precedes or follows ' +
                      'the initial date, depending on the day count’s sign.';
  sfnWorkDay_Intl = 'WORKDAY.INTL';
  sfnWorkDay_IntlDescription =
                      'Returns the serial number of the date before or after the specified number of workdays, taking a custom set of holidays into account.';
  sfnYear = 'YEAR';
  sfnYearDescription = 'Returns the serial number corresponding to the specified year.';
  sfnYearFrac = 'YEARFRAC';
  sfnYearFracDescription = 'Calculates the fraction of the year within the range between two specified dates.';

  // Financial function names
  sfnAccrInt = 'ACCRINT';
  sfnAccrIntDescription = 'Returns the accrued interest for a security that pays a periodic interest.';
  sfnAccrIntM = 'ACCRINTM';
  sfnAccrIntMDescription = 'Returns the accrued interest for a security that pays at maturity.';
  sfnAmorDegr = 'AMORDEGRC';
  sfnAmorDegrDescription = 'Returns the depreciation of an asset for each accounting period. This function is provided for the French accounting system.';
  sfnAmorLinc = 'AMORLINC';
  sfnAmorLincDescription = 'Returns the depreciation for each accounting period.';
  sfnCoupDayBS = 'COUPDAYBS';
  sfnCoupDayBSDescription = 'Returns the number of days from the beginning of the coupon period to the settlement date.';
  sfnCoupDays = 'COUPDAYS';
  sfnCoupDaysDescription = 'Returns the number of days in the coupon period that contains the settlement date.';
  sfnCoupDaysNC = 'COUPDAYSNC';
  sfnCoupDaysNCDescription = 'Returns the number of days from the settlement date to the next coupon date.';
  sfnCoupNCD = 'COUPNCD';
  sfnCoupNCDDescription = 'Returns the next coupon date after the settlement date.';
  sfnCoupNum = 'COUPNUM';
  sfnCoupNumDescription = 'Returns the number of coupons payable between the settlement date and maturity date.';
  sfnCoupPCD = 'COUPPCD';
  sfnCoupPCDDescription = 'Returns the previous coupon date before the settlement date.';
  sfnCoupIPMT = 'CUMIPMT';
  sfnCoupIPMTDescription = 'Calculates the cumulative interest paid between two specified periods.';
  sfnCoupRINC = 'CUMPRINC';
  sfnCoupRINCDescription = 'Calculates the cumulative principal paid on a loan, between two specified periods.';
  sfnDB = 'DB';
  sfnDBDescription = 'Returns the depreciation of an asset for a specified period by using the fixed-declining balance method.';
  sfnDDB = 'DDB';
  sfnDDBDescription =
                     'Returns the depreciation of an asset for a specified period by using the double-declining balance method or some other method that you specify.';
  sfnDisc = 'DISC';
  sfnDiscDescription = 'Returns the discount rate for a security.';
  sfnDollarDe = 'DOLLARDE';
  sfnDollarDeDescription = 'Converts a dollar price, expressed as a fraction, into a dollar price, expressed as a decimal number.';
  sfnDollarFr = 'DOLLARFR';
  sfnDollarFrDescription = 'Converts a dollar price, expressed as a decimal number, into a dollar price, expressed as a fraction.';
  sfnDuration = 'DURATION';
  sfnDurationDescription = 'Returns the annual duration of a security with periodic interest payments.';
  sfnEffect = 'EFFECT';
  sfnEffectDescription = 'Returns the effective annual interest rate.';
  sfnFV = 'FV';
  sfnFVDescription = 'Calculates the future value of an investment with periodic constant payments and a constant interest rate.';
  sfnFVSchedule = 'FVSCHEDULE';
  sfnFVScheduleDescription = 'Returns the future value of an initial principal after applying a series of compound interest rates.';
  sfnIntRate = 'INTRATE';
  sfnIntRateDescription = 'Returns the interest rate for a fully invested security.';
  sfnIPMT = 'IPMT';
  sfnIPMTDescription =
                     'Calculates the interest payment for a given period of an investment, with periodic constant payments and a constant interest rate.';
  sfnIRR = 'IRR';
  sfnIRRDescription = 'Calculates the internal rate of return for a series of cash flows.';
  sfnIsPMT = 'ISPMT';
  sfnIsPMTDescription = 'Calculates the interest paid during a specific period of an investment.';
  sfnMDuration = 'MDURATION';
  sfnMDurationDescription = 'Returns the Macauley modified duration for a security with an assumed par value of $100.';
  sfnMIRR = 'MIRR';
  sfnMIRRDescription =
                     'Calculates the internal rate of return for a series of periodic cash flows, considering the cost of the investment and the ' +
                     'interest on the reinvestment of cash.';
  sfnNominal = 'NOMINAL';
  sfnNominalDescription = 'Returns the annual nominal interest rate.';
  sfnNPer = 'NPER';
  sfnNPerDescription = 'Returns the number of periods for an investment with periodic constant payments and a constant interest rate.';
  sfnNPV = 'NPV';
  sfnNPVDescription = 'Calculates the net present value of an investment, based on a supplied discount rate, and a series of future payments and income.';
  sfnOddFPrice = 'ODDFPRICE';
  sfnOddFPriceDescription = 'Returns the price per $100 face value of a security with an odd first period.';
  sfnOddFYield = 'ODDFYIELD';
  sfnOddFYieldDescription = 'Returns the yield of a security with an odd first period.';
  sfnOddLPrice = 'ODDLPRICE';
  sfnOddLPriceDescription = 'Returns the price per $100 face value of a security with an odd last period.';
  sfnOddLYield = 'ODDLYIELD';
  sfnOddLYieldDescription = 'Returns the yield of a security with an odd last period.';
  sfnPDuration = 'PDURATION';
  sfnPDurationDescription = 'Returns the number of periods required by an investment to reach a specified value.';
  sfnPMT = 'PMT';
  sfnPMTDescription = 'Calculates the payments required to reduce a loan, from a supplied present value to a specified future value.';
  sfnPPMT = 'PPMT';
  sfnPPMTDescription = 'Calculates the payment on the principal for a given investment, with periodic constant payments and a constant interest rate.';
  sfnPrice = 'PRICE';
  sfnPriceDescription = 'Returns the price per $100 face value of a security that pays a periodic interest.';
  sfnPriceDisc = 'PRICEDISC';
  sfnPriceDiscDescription = 'Returns the price per $100 face value of a discounted security.';
  sfnPriceMat = 'PRICEMAT';
  sfnPriceMatDescription = 'Returns the price per $100 face value of a security that pays interest at maturity.';
  sfnPV = 'PV';
  sfnPVDescription = 'Calculates the present value of an investment (i.e., the total amount that a series of future payments is worth now).';
  sfnRate = 'RATE';
  sfnRateDescription =
                     'Calculates the interest rate required to pay off a specified amount of a loan, or reach a target amount on an investment over a ' +
                     'given period.';
  sfnReceived = 'RECEIVED';
  sfnReceivedDescription = 'Returns the amount received at maturity for a fully invested security.';
  sfnRRI = 'RRI';
  sfnRRIDescription = 'Returns an equivalent interest rate for the growth of an investment.';
  sfnSLN = 'SLN';
  sfnSLNDescription = 'Returns the straight-line depreciation of an asset for one period.';
  sfnSYD = 'SYD';
  sfnSYDDescription = 'Returns the sum-of-years'' digits depreciation of an asset for a specified period.';
  sfnTBillEq = 'TBILLEQ';
  sfnTBillEqDescription = 'Returns the bond-equivalent yield for a Treasury bill.';
  sfnTBillPrice = 'TBILLPRICE';
  sfnTBillPriceDescription = 'Returns the price per $100 face value for a Treasury bill.';
  sfnTBillYield = 'TBILLYIELD';
  sfnTBillYieldDescription = 'Returns the yield for a Treasury bill.';
  sfnVDB = 'VDB';
  sfnVDBDescription = 'Returns the depreciation of an asset for a specified or partial period by using a declining balance method.';
  sfnXIRR = 'XIRR';
  sfnXIRRDescription = 'Returns the internal rate of return for a schedule of cash flows that is not necessarily periodic.';
  sfnXNPV = 'XNPV';
  sfnXNPVDescription = 'Returns the net present value for a schedule of cash flows that is not necessarily periodic.';
  sfnYield = 'YIELD';
  sfnYieldDescription = 'Returns the yield on a security that pays a periodic interest.';
  sfnYieldDisc = 'YIELDDISC';
  sfnYieldDiscDescription = 'Returns the annual yield for a discounted security; for example, a Treasury bill.';
  sfnYieldMat = 'YIELDMAT';
  sfnYieldMatDescription = 'Returns the annual yield of a security that pays interest at maturity.';

  // Logical function names
  sfnAnd = 'AND';
  sfnAndDescription = 'Performs the logical AND operation.';
  sfnFalse = 'FALSE';
  sfnFalseDescription = 'Returns the logical value FALSE.';
  sfnIF = 'IF';
  sfnIFDescription = 'Performs a logical test and returns either of the specified values depending on the test’s result.';
  sfnIfError = 'IFERROR';
  sfnIfErrorDescription = 'Checks the specified formula expression for errors and returns the special value instead of an error code if an error occurs.';
  sfnIfNA = 'IFNA';
  sfnIfNADescription = 'Checks if the specified formula expression returns the #N/A error code and returns the special value instead.';
  sfnNot = 'NOT';
  sfnNotDescription = 'Performs the logical negation operation.';
  sfnOr = 'OR';
  sfnOrDescription = 'Performs the logical OR operation.';
  sfnTrue = 'TRUE';
  sfnTrueDescription = 'Returns the logical value TRUE.';
  sfnXor = 'XOR';
  sfnXorDescription = 'Returns the logical exclusive OR of all specified values.';

  // Cube function names
  sfnCubeKPIMember = 'CUBEKPIMEMBER';
  sfnCubeMember = 'CUBEMEMBER';
  sfnCubeMemberProperty = 'CUBEMEMBERPROPERTY';
  sfnCubeRankedMember = 'CUBERANKEDMEMBER';
  sfnCubeSet = 'CUBESET';
  sfnCubeSetCount = 'CUBESETCOUNT';
  sfnCubeValue = 'CUBEVALUE';

  // Database function names
  sfnDAverage = 'DAVERAGE';
  sfnDCount = 'DCOUNT';
  sfnDCountA = 'DCOUNTA';
  sfnDGet = 'DGET';
  sfnDMax = 'DMAX';
  sfnDMin = 'DMIN';
  sfnDProduct = 'DPRODUCT';
  sfnDStDev = 'DSTDEV';
  sfnDStDevP = 'DSTDEVP';
  sfnDSum = 'DSUM';
  sfnDVar = 'DVAR';
  sfnDVarP = 'DVARP';

  // Engineering functions names
  sfnBesselI = 'BESSELI';
  sfnBesselJ = 'BESSELJ';
  sfnBesselK = 'BESSELK';
  sfnBesselY = 'BESSELY';
  sfnBin2Dec = 'BIN2DEC';
  sfnBin2Hex = 'BIN2HEX';
  sfnBin2Oct = 'BIN2OCT';
  sfnBitAnd = 'BITAND';
  sfnBitLShift = 'BITLSHIFT';
  sfnBitOr = 'BITOR';
  sfnBitRShift = 'BITRSHIFT';
  sfnBitXor = 'BITXOR';
  sfnComplex = 'COMPLEX';
  sfnConvert = 'CONVERT';
  sfnDec2Bin = 'DEC2BIN';
  sfnDec2Hex = 'DEC2HEX';
  sfnDec2Oct = 'DEC2OCT';
  sfnDelta = 'DELTA';
  sfnERF = 'ERF';
  sfnERF_Precise = 'ERF.PRECISE';
  sfnERFC = 'ERFC';
  sfnERFC_Precise = 'ERFC.PRECISE';
  sfnGestep = 'GESTEP';
  sfnHex2Bin = 'HEX2BIN';
  sfnHex2Dec = 'HEX2DEC';
  sfnHex2Oct = 'HEX2OCT';
  sfnImAbs = 'IMABS';
  sfnImAginary = 'IMAGINARY';
  sfnImArgument = 'IMARGUMENT';
  sfnImConjugate = 'IMCONJUGATE';
  sfnImCos = 'IMCOS';
  sfnImCosh = 'IMCOSH';
  sfnImCot = 'IMCOT';
  sfnImCsc = 'IMCSC';
  sfnImCsch = 'IMCSCH';
  sfnImDiv = 'IMDIV';
  sfnImExp = 'IMEXP';
  sfnImLn = 'IMLN';
  sfnImLog10 = 'IMLOG10';
  sfnImLog2 = 'IMLOG2';
  sfnImPower = 'IMPOWER';
  sfnImProduct = 'IMPRODUCT';
  sfnImReal = 'IMREAL';
  sfnImSec = 'IMSEC';
  sfnImSech = 'IMSECH';
  sfnImSin = 'IMSIN';
  sfnImSinh = 'IMSINH';
  sfnImSqrt = 'IMSQRT';
  sfnImSub = 'IMSUB';
  sfnImSum = 'IMSUM';
  sfnImTan = 'IMTAN';
  sfnOct2Bin = 'OCT2BIN';
  sfnOct2Dec = 'OCT2DEC';
  sfnOct2Hex = 'OCT2HEX';

  // Lookup and Reference
  sfnAddress = 'ADDRESS';
  sfnAddressDescription = 'Returns a reference as text to a single cell in a worksheet.';
  sfnAreas = 'AREAS';
  sfnAreasDescription = 'Returns the number of areas in a reference.';
  sfnChoose = 'CHOOSE';
  sfnChooseDescription =
                     'Returns a value from the list of value parameters. You can use this function to select one of the specified values based on the ' +
                     'index number.';
  sfnColumn = 'COLUMN';
  sfnColumnDescription = 'Returns the column number of a reference.';
  sfnColumns = 'COLUMNS';
  sfnColumnsDescription = 'Returns the number of columns in a reference.';
  sfnFormulaText = 'FORMULATEXT';
  sfnFormulaTextDescription = 'Returns a text representation of the specified formula expression.';
  sfnGetPivotData = 'GETPIVOTDATA';
  sfnGetPivotDataDescription = 'Returns data stored in a Pivot Table report.';
  sfnHLookup = 'HLOOKUP';
  sfnHLookupDescription = 'Looks up a value in the first table row, and returns a value in the same column from another row.';
  sfnHyperlink = 'HYPERLINK';
  sfnHyperlinkDescription = 'Creates a hyperlink.';
  sfnIndex = 'INDEX';
  sfnIndexDescription = 'Returns the value of an element in a table or an array, selected by the row and column number indexes.';
  sfnIndirect = 'INDIRECT';
  sfnIndirectDescription = 'Returns the reference specified by a text string.';
  sfnLookup = 'LOOKUP';
  sfnLookupDescription = 'Returns a value from a cell in a position found by lookup in a search table.';
  sfnMatch = 'MATCH';
  sfnMatchDescription = 'Searches for a specified item in a range of cells and returns the relative position of that item in the range.';
  sfnOffset = 'OFFSET';
  sfnOffsetDescription = 'Returns a reference to a range that is located a specified number of rows and columns away from a cell or cell range.';
  sfnRow = 'ROW';
  sfnRowDescription = 'Returns the row number of a reference.';
  sfnRows = 'ROWS';
  sfnRowsDescription = 'Returns the number of rows in a reference or array.';
  sfnRTD = 'RTD';
  sfnRTDDescription = 'Retrieves real-time data from a program that supports COM automation.';
  sfnVLookup = 'VLOOKUP';
  sfnVLookupDescription = 'Looks up a value in the first column of a table, and returns a value in the same row from another column.';
  sfnTranspose = 'TRANSPOSE';
  sfnTransposeDescription = 'Transforms a horizontal range of cells into a vertical range, and vice versa.';

implementation

procedure AddSpreadSheetResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  // Categories
  AProduct.Add('sfnCategoryCommon', @sfnCategoryCommon);
  AProduct.Add('sfnCategoryCompatibility', @sfnCategoryCompatibility);
  AProduct.Add('sfnCategoryCube', @sfnCategoryCube);
  AProduct.Add('sfnCategoryDatabase', @sfnCategoryDatabase);
  AProduct.Add('sfnCategoryDateTime', @sfnCategoryDateTime);
  AProduct.Add('sfnCategoryEngineering', @sfnCategoryEngineering);
  AProduct.Add('sfnCategoryFinancial', @sfnCategoryFinancial);
  AProduct.Add('sfnCategoryInformation', @sfnCategoryInformation);
  AProduct.Add('sfnCategoryLogical', @sfnCategoryLogical);
  AProduct.Add('sfnCategoryLookupAndReference', @sfnCategoryLookupAndReference);
  AProduct.Add('sfnCategoryMath', @sfnCategoryMath);
  AProduct.Add('sfnCategoryStatistical', @sfnCategoryStatistical);
  AProduct.Add('sfnCategoryText', @sfnCategoryText);

  // General
  AProduct.Add('sfnParamArray', @sfnParamArray);
  AProduct.Add('sfnParamValue', @sfnParamValue);


  // Text Functions
  AProduct.Add('sfnASC', @sfnASC);
  AProduct.Add('sfnASCDescription', @sfnASCDescription);
  AProduct.Add('sfnBahtText', @sfnBahtText);
  AProduct.Add('sfnBahtTextDescription', @sfnBahtTextDescription);
  AProduct.Add('sfnChar', @sfnChar);
  AProduct.Add('sfnCharDescription', @sfnCharDescription);
  AProduct.Add('sfnClean', @sfnClean);
  AProduct.Add('sfnCleanDescription', @sfnCleanDescription);
  AProduct.Add('sfnCode', @sfnCode);
  AProduct.Add('sfnCodeDescription', @sfnCodeDescription);
  AProduct.Add('sfnConcatenate', @sfnConcatenate);
  AProduct.Add('sfnConcatenateDescription', @sfnConcatenateDescription);
  AProduct.Add('sfnDBCS', @sfnDBCS);
  AProduct.Add('sfnDBCSDescription', @sfnDBCSDescription);
  AProduct.Add('sfnDollar', @sfnDollar);
  AProduct.Add('sfnDollarDescription', @sfnDollarDescription);
  AProduct.Add('sfnExact', @sfnExact);
  AProduct.Add('sfnExactDescription', @sfnExactDescription);
  AProduct.Add('sfnFind', @sfnFind);
  AProduct.Add('sfnFindDescription', @sfnFindDescription);
  AProduct.Add('sfnFindB', @sfnFindB);
  AProduct.Add('sfnFindBDescription', @sfnFindBDescription);
  AProduct.Add('sfnFixed', @sfnFixed);
  AProduct.Add('sfnFixedDescription', @sfnFixedDescription);
  AProduct.Add('sfnLeft', @sfnLeft);
  AProduct.Add('sfnLeftDescription', @sfnLeftDescription);
  AProduct.Add('sfnLeftB', @sfnLeftB);
  AProduct.Add('sfnLeftBDescription', @sfnLeftBDescription);
  AProduct.Add('sfnLen', @sfnLen);
  AProduct.Add('sfnLenDescription', @sfnLenDescription);
  AProduct.Add('sfnLenB', @sfnLenB);
  AProduct.Add('sfnLenBDescription', @sfnLenBDescription);
  AProduct.Add('sfnLower', @sfnLower);
  AProduct.Add('sfnLowerDescription', @sfnLowerDescription);
  AProduct.Add('sfnMid', @sfnMid);
  AProduct.Add('sfnMidDescription', @sfnMidDescription);
  AProduct.Add('sfnMidB', @sfnMidB);
  AProduct.Add('sfnMidBDescription', @sfnMidBDescription);
  AProduct.Add('sfnNumberValue', @sfnNumberValue);
  AProduct.Add('sfnNumberValueDescription', @sfnNumberValueDescription);
  AProduct.Add('sfnPhonetic', @sfnPhonetic);
  AProduct.Add('sfnPhoneticDescription', @sfnPhoneticDescription);
  AProduct.Add('sfnProper', @sfnProper);
  AProduct.Add('sfnProperDescription', @sfnProperDescription);
  AProduct.Add('sfnReplace', @sfnReplace);
  AProduct.Add('sfnReplaceDescription', @sfnReplaceDescription);
  AProduct.Add('sfnReplaceB', @sfnReplaceB);
  AProduct.Add('sfnReplaceBDescription', @sfnReplaceBDescription);
  AProduct.Add('sfnRept', @sfnRept);
  AProduct.Add('sfnReptDescription', @sfnReptDescription);
  AProduct.Add('sfnRight', @sfnRight);
  AProduct.Add('sfnRightDescription', @sfnRightDescription);
  AProduct.Add('sfnRightB', @sfnRightB);
  AProduct.Add('sfnRightBDescription', @sfnRightBDescription);
  AProduct.Add('sfnSearch', @sfnSearch);
  AProduct.Add('sfnSearchDescription', @sfnSearchDescription);
  AProduct.Add('sfnSearchB', @sfnSearchB);
  AProduct.Add('sfnSearchBDescription', @sfnSearchBDescription);
  AProduct.Add('sfnSubstitute', @sfnSubstitute);
  AProduct.Add('sfnSubstituteDescription', @sfnSubstituteDescription);
  AProduct.Add('sfnT', @sfnT);
  AProduct.Add('sfnTDescription', @sfnTDescription);
  AProduct.Add('sfnText', @sfnText);
  AProduct.Add('sfnTextDescription', @sfnTextDescription);
  AProduct.Add('sfnTrim', @sfnTrim);
  AProduct.Add('sfnTrimDescription', @sfnTrimDescription);
  AProduct.Add('sfnUniChar', @sfnUniChar);
  AProduct.Add('sfnUniCharDescription', @sfnUniCharDescription);
  AProduct.Add('sfnUniCode', @sfnUniCode);
  AProduct.Add('sfnUniCodeDescription', @sfnUniCodeDescription);
  AProduct.Add('sfnUpper', @sfnUpper);
  AProduct.Add('sfnUpperDescription', @sfnUpperDescription);
  AProduct.Add('sfnValue', @sfnValue);
  AProduct.Add('sfnValueDescription', @sfnValueDescription);

  // Math and Trigonometry functions
  AProduct.Add('sfnAbs', @sfnAbs);
  AProduct.Add('sfnAbsDescription', @sfnAbsDescription);
  AProduct.Add('sfnAcos', @sfnAcos);
  AProduct.Add('sfnAcosDescription', @sfnAcosDescription);
  AProduct.Add('sfnAcosh', @sfnAcosh);
  AProduct.Add('sfnAcoshDescription', @sfnAcoshDescription);
  AProduct.Add('sfnAcot', @sfnAcot);
  AProduct.Add('sfnAcotDescription', @sfnAcotDescription);
  AProduct.Add('sfnAcoth', @sfnAcoth);
  AProduct.Add('sfnAcothDescription', @sfnAcothDescription);
  AProduct.Add('sfnAggregate', @sfnAggregate);
  AProduct.Add('sfnAggregateDescription', @sfnAggregateDescription);
  AProduct.Add('sfnArabic', @sfnArabic);
  AProduct.Add('sfnArabicDescription', @sfnArabicDescription);
  AProduct.Add('sfnAsin', @sfnAsin);
  AProduct.Add('sfnAsinDescription', @sfnAsinDescription);
  AProduct.Add('sfnAsinh', @sfnAsinh);
  AProduct.Add('sfnAsinhDescription', @sfnAsinhDescription);
  AProduct.Add('sfnAtan', @sfnAtan);
  AProduct.Add('sfnAtanDescription', @sfnAtanDescription);
  AProduct.Add('sfnAtan2', @sfnAtan2);
  AProduct.Add('sfnAtan2Description', @sfnAtan2Description);
  AProduct.Add('sfnAtanh', @sfnAtanh);
  AProduct.Add('sfnAtanhDescription', @sfnAtanhDescription);
  AProduct.Add('sfnBase', @sfnBase);
  AProduct.Add('sfnBaseDescription', @sfnBaseDescription);
  AProduct.Add('sfnCeiling', @sfnCeiling);
  AProduct.Add('sfnCeilingDescription', @sfnCeilingDescription);
  AProduct.Add('sfnCeiling_Math', @sfnCeiling_Math);
  AProduct.Add('sfnCeiling_MathDescription', @sfnCeiling_MathDescription);
  AProduct.Add('sfnCeiling_Precise', @sfnCeiling_Precise);
  AProduct.Add('sfnCeiling_PreciseDescription', @sfnCeiling_PreciseDescription);
  AProduct.Add('sfnCos', @sfnCos);
  AProduct.Add('sfnCosDescription', @sfnCosDescription);
  AProduct.Add('sfnCosh', @sfnCosh);
  AProduct.Add('sfnCoshDescription', @sfnCoshDescription);
  AProduct.Add('sfnCombin', @sfnCombin);
  AProduct.Add('sfnCombinDescription', @sfnCombinDescription);
  AProduct.Add('sfnCombinA', @sfnCombinA);
  AProduct.Add('sfnCombinADescription', @sfnCombinADescription);
  AProduct.Add('sfnCot', @sfnCot);
  AProduct.Add('sfnCotDescription', @sfnCotDescription);
  AProduct.Add('sfnCoth', @sfnCoth);
  AProduct.Add('sfnCothDescription', @sfnCothDescription);
  AProduct.Add('sfnCsc', @sfnCsc);
  AProduct.Add('sfnCscDescription', @sfnCscDescription);
  AProduct.Add('sfnCsch', @sfnCsch);
  AProduct.Add('sfnCschDescription', @sfnCschDescription);
  AProduct.Add('sfnDecimal', @sfnDecimal);
  AProduct.Add('sfnDecimalDescription', @sfnDecimalDescription);
  AProduct.Add('sfnDegrees', @sfnDegrees);
  AProduct.Add('sfnDegreesDescription', @sfnDegreesDescription);
  AProduct.Add('sfnEven', @sfnEven);
  AProduct.Add('sfnEvenDescription', @sfnEvenDescription);
  AProduct.Add('sfnExp', @sfnExp);
  AProduct.Add('sfnExpDescription', @sfnExpDescription);
  AProduct.Add('sfnFact', @sfnFact);
  AProduct.Add('sfnFactDescription', @sfnFactDescription);
  AProduct.Add('sfnFactDouble', @sfnFactDouble);
  AProduct.Add('sfnFactDoubleDescription', @sfnFactDoubleDescription);
  AProduct.Add('sfnFloor', @sfnFloor);
  AProduct.Add('sfnFloorDescription', @sfnFloorDescription);
  AProduct.Add('sfnFloor_Math', @sfnFloor_Math);
  AProduct.Add('sfnFloor_MathDescription', @sfnFloor_MathDescription);
  AProduct.Add('sfnFloor_Precise', @sfnFloor_Precise);
  AProduct.Add('sfnFloor_PreciseDescription', @sfnFloor_PreciseDescription);
  AProduct.Add('sfnGCD', @sfnGCD);
  AProduct.Add('sfnGCDDescription', @sfnGCDDescription);
  AProduct.Add('sfnInt', @sfnInt);
  AProduct.Add('sfnIntDescription', @sfnIntDescription);
  AProduct.Add('sfnIso_Ceiling', @sfnIso_Ceiling);
  AProduct.Add('sfnIso_CeilingDescription', @sfnIso_CeilingDescription);
  AProduct.Add('sfnLCM', @sfnLCM);
  AProduct.Add('sfnLCMDescription', @sfnLCMDescription);
  AProduct.Add('sfnLn', @sfnLn);
  AProduct.Add('sfnLnDescription', @sfnLnDescription);
  AProduct.Add('sfnLog', @sfnLog);
  AProduct.Add('sfnLogDescription', @sfnLogDescription);
  AProduct.Add('sfnLog10', @sfnLog10);
  AProduct.Add('sfnLog10Description', @sfnLog10Description);
  AProduct.Add('sfnMDeterm', @sfnMDeterm);
  AProduct.Add('sfnMDetermDescription', @sfnMDetermDescription);
  AProduct.Add('sfnMInverse', @sfnMInverse);
  AProduct.Add('sfnMInverseDescription', @sfnMInverseDescription);
  AProduct.Add('sfnMMult', @sfnMMult);
  AProduct.Add('sfnMMultDescription', @sfnMMultDescription);
  AProduct.Add('sfnMod', @sfnMod);
  AProduct.Add('sfnModDescription', @sfnModDescription);
  AProduct.Add('sfnMRound', @sfnMRound);
  AProduct.Add('sfnMRoundDescription', @sfnMRoundDescription);
  AProduct.Add('sfnMultiNomial', @sfnMultiNomial);
  AProduct.Add('sfnMultiNomialDescription', @sfnMultiNomialDescription);
  AProduct.Add('sfnMUnit', @sfnMUnit);
  AProduct.Add('sfnMUnitDescription', @sfnMUnitDescription);
  AProduct.Add('sfnOdd', @sfnOdd);
  AProduct.Add('sfnOddDescription', @sfnOddDescription);
  AProduct.Add('sfnPi', @sfnPi);
  AProduct.Add('sfnPiDescription', @sfnPiDescription);
  AProduct.Add('sfnPower', @sfnPower);
  AProduct.Add('sfnPowerDescription', @sfnPowerDescription);
  AProduct.Add('sfnProduct', @sfnProduct);
  AProduct.Add('sfnProductDescription', @sfnProductDescription);
  AProduct.Add('sfnQuotient', @sfnQuotient);
  AProduct.Add('sfnQuotientDescription', @sfnQuotientDescription);
  AProduct.Add('sfnRadians', @sfnRadians);
  AProduct.Add('sfnRadiansDescription', @sfnRadiansDescription);
  AProduct.Add('sfnRand', @sfnRand);
  AProduct.Add('sfnRandDescription', @sfnRandDescription);
  AProduct.Add('sfnRandBetween', @sfnRandBetween);
  AProduct.Add('sfnRandBetweenDescription', @sfnRandBetweenDescription);
  AProduct.Add('sfnRound', @sfnRound);
  AProduct.Add('sfnRoundDescription', @sfnRoundDescription);
  AProduct.Add('sfnRoundDown', @sfnRoundDown);
  AProduct.Add('sfnRoundDownDescription', @sfnRoundDownDescription);
  AProduct.Add('sfnRoundUp', @sfnRoundUp);
  AProduct.Add('sfnRoundUpDescription', @sfnRoundUpDescription);
  AProduct.Add('sfnRoman', @sfnRoman);
  AProduct.Add('sfnRomanDescription', @sfnRomanDescription);
  AProduct.Add('sfnSec', @sfnSec);
  AProduct.Add('sfnSecDescription', @sfnSecDescription);
  AProduct.Add('sfnSech', @sfnSech);
  AProduct.Add('sfnSechDescription', @sfnSechDescription);
  AProduct.Add('sfnSeriesSum', @sfnSeriesSum);
  AProduct.Add('sfnSeriesSumDescription', @sfnSeriesSumDescription);
  AProduct.Add('sfnSign', @sfnSign);
  AProduct.Add('sfnSignDescription', @sfnSignDescription);
  AProduct.Add('sfnSin', @sfnSin);
  AProduct.Add('sfnSinDescription', @sfnSinDescription);
  AProduct.Add('sfnSinh', @sfnSinh);
  AProduct.Add('sfnSinhDescription', @sfnSinhDescription);
  AProduct.Add('sfnSqrt', @sfnSqrt);
  AProduct.Add('sfnSqrtDescription', @sfnSqrtDescription);
  AProduct.Add('sfnSqrtPi', @sfnSqrtPi);
  AProduct.Add('sfnSqrtPiDescription', @sfnSqrtPiDescription);
  AProduct.Add('sfnSubTotal', @sfnSubTotal);
  AProduct.Add('sfnSubTotalDescription', @sfnSubTotalDescription);
  AProduct.Add('sfnSum', @sfnSum);
  AProduct.Add('sfnSumDescription', @sfnSumDescription);
  AProduct.Add('sfnSumIF', @sfnSumIF);
  AProduct.Add('sfnSumIFDescription', @sfnSumIFDescription);
  AProduct.Add('sfnSumIFS', @sfnSumIFS);
  AProduct.Add('sfnSumIFSDescription', @sfnSumIFSDescription);
  AProduct.Add('sfnSumProduct', @sfnSumProduct);
  AProduct.Add('sfnSumProductDescription', @sfnSumProductDescription);
  AProduct.Add('sfnSumSQ', @sfnSumSQ);
  AProduct.Add('sfnSumSQDescription', @sfnSumSQDescription);
  AProduct.Add('sfnSumX2MY2', @sfnSumX2MY2);
  AProduct.Add('sfnSumX2MY2Description', @sfnSumX2MY2Description);
  AProduct.Add('sfnSumX2PY2', @sfnSumX2PY2);
  AProduct.Add('sfnSumX2PY2Description', @sfnSumX2PY2Description);
  AProduct.Add('sfnSumXMY2', @sfnSumXMY2);
  AProduct.Add('sfnSumXMY2Description', @sfnSumXMY2Description);
  AProduct.Add('sfnTan', @sfnTan);
  AProduct.Add('sfnTanDescription', @sfnTanDescription);
  AProduct.Add('sfnTanh', @sfnTanh);
  AProduct.Add('sfnTanhDescription', @sfnTanhDescription);
  AProduct.Add('sfnTrunc', @sfnTrunc);
  AProduct.Add('sfnTruncDescription', @sfnTruncDescription);

  // Statistical functions
  AProduct.Add('sfnAveDev', @sfnAveDev);
  AProduct.Add('sfnAveDevDescription', @sfnAveDevDescription);
  AProduct.Add('sfnAverage', @sfnAverage);
  AProduct.Add('sfnAverageDescription', @sfnAverageDescription);
  AProduct.Add('sfnAverageA', @sfnAverageA);
  AProduct.Add('sfnAverageADescription', @sfnAverageADescription);
  AProduct.Add('sfnAverageIF', @sfnAverageIF);
  AProduct.Add('sfnAverageIFDescription', @sfnAverageIFDescription);
  AProduct.Add('sfnAverageIFS', @sfnAverageIFS);
  AProduct.Add('sfnAverageIFSDescription', @sfnAverageIFSDescription);
  AProduct.Add('sfnBeta_Dist', @sfnBeta_Dist);
  AProduct.Add('sfnBeta_DistDescription', @sfnBeta_DistDescription);
  AProduct.Add('sfnBeta_Inv', @sfnBeta_Inv);
  AProduct.Add('sfnBeta_InvDescription', @sfnBeta_InvDescription);
  AProduct.Add('sfnBinom_Dist', @sfnBinom_Dist);
  AProduct.Add('sfnBinom_DistDescription', @sfnBinom_DistDescription);
  AProduct.Add('sfnBinom_Dist_Range', @sfnBinom_Dist_Range);
  AProduct.Add('sfnBinom_Dist_RangeDescription', @sfnBinom_Dist_RangeDescription);
  AProduct.Add('sfnBinom_Inv', @sfnBinom_Inv);
  AProduct.Add('sfnBinom_InvDescription', @sfnBinom_InvDescription);
  AProduct.Add('sfnChiSQ_Dist', @sfnChiSQ_Dist);
  AProduct.Add('sfnChiSQ_DistDescription', @sfnChiSQ_DistDescription);
  AProduct.Add('sfnChiSQ_Dist_RT', @sfnChiSQ_Dist_RT);
  AProduct.Add('sfnChiSQ_Dist_RTDescription', @sfnChiSQ_Dist_RTDescription);
  AProduct.Add('sfnChiSQ_Inv', @sfnChiSQ_Inv);
  AProduct.Add('sfnChiSQ_InvDescription', @sfnChiSQ_InvDescription);
  AProduct.Add('sfnChiSQ_Inv_RT', @sfnChiSQ_Inv_RT);
  AProduct.Add('sfnChiSQ_Inv_RTDescription', @sfnChiSQ_Inv_RTDescription);
  AProduct.Add('sfnChiSQ_Test', @sfnChiSQ_Test);
  AProduct.Add('sfnChiSQ_TestDescription', @sfnChiSQ_TestDescription);
  AProduct.Add('sfnConfidence_Norm', @sfnConfidence_Norm);
  AProduct.Add('sfnConfidence_NormDescription', @sfnConfidence_NormDescription);
  AProduct.Add('sfnConfidence_T', @sfnConfidence_T);
  AProduct.Add('sfnConfidence_TDescription', @sfnConfidence_TDescription);
  AProduct.Add('sfnCorrel', @sfnCorrel);
  AProduct.Add('sfnCorrelDescription', @sfnCorrelDescription);
  AProduct.Add('sfnCount', @sfnCount);
  AProduct.Add('sfnCountDescription', @sfnCountDescription);
  AProduct.Add('sfnCountA', @sfnCountA);
  AProduct.Add('sfnCountADescription', @sfnCountADescription);
  AProduct.Add('sfnCountBlank', @sfnCountBlank);
  AProduct.Add('sfnCountBlankDescription', @sfnCountBlankDescription);
  AProduct.Add('sfnCountIF', @sfnCountIF);
  AProduct.Add('sfnCountIFDescription', @sfnCountIFDescription);
  AProduct.Add('sfnCountIFS', @sfnCountIFS);
  AProduct.Add('sfnCountIFSDescription', @sfnCountIFSDescription);
  AProduct.Add('sfnCovariance_P', @sfnCovariance_P);
  AProduct.Add('sfnCovariance_PDescription', @sfnCovariance_PDescription);
  AProduct.Add('sfnCovariance_S', @sfnCovariance_S);
  AProduct.Add('sfnCovariance_SDescription', @sfnCovariance_SDescription);
  AProduct.Add('sfnDevSQ', @sfnDevSQ);
  AProduct.Add('sfnDevSQDescription', @sfnDevSQDescription);
  AProduct.Add('sfnExpon_Dist', @sfnExpon_Dist);
  AProduct.Add('sfnExpon_DistDescription', @sfnExpon_DistDescription);
  AProduct.Add('sfnF_Dist', @sfnF_Dist);
  AProduct.Add('sfnF_DistDescription', @sfnF_DistDescription);
  AProduct.Add('sfnF_Dist_RT', @sfnF_Dist_RT);
  AProduct.Add('sfnF_Dist_RTDescription', @sfnF_Dist_RTDescription);
  AProduct.Add('sfnF_Inv', @sfnF_Inv);
  AProduct.Add('sfnF_InvDescription', @sfnF_InvDescription);
  AProduct.Add('sfnF_Inv_RT', @sfnF_Inv_RT);
  AProduct.Add('sfnF_Inv_RTDescription', @sfnF_Inv_RTDescription);
  AProduct.Add('sfnF_Test', @sfnF_Test);
  AProduct.Add('sfnF_TestDescription', @sfnF_TestDescription);
  AProduct.Add('sfnFisher', @sfnFisher);
  AProduct.Add('sfnFisherDescription', @sfnFisherDescription);
  AProduct.Add('sfnFisherInv', @sfnFisherInv);
  AProduct.Add('sfnFisherInvDescription', @sfnFisherInvDescription);
  AProduct.Add('sfnForecast', @sfnForecast);
  AProduct.Add('sfnForecastDescription', @sfnForecastDescription);
  AProduct.Add('sfnFrequency', @sfnFrequency);
  AProduct.Add('sfnFrequencyDescription', @sfnFrequencyDescription);
  AProduct.Add('sfnGamma', @sfnGamma);
  AProduct.Add('sfnGammaDescription', @sfnGammaDescription);
  AProduct.Add('sfnGamma_Dist', @sfnGamma_Dist);
  AProduct.Add('sfnGamma_DistDescription', @sfnGamma_DistDescription);
  AProduct.Add('sfnGamma_Inv', @sfnGamma_Inv);
  AProduct.Add('sfnGamma_InvDescription', @sfnGamma_InvDescription);
  AProduct.Add('sfnGammaLn', @sfnGammaLn);
  AProduct.Add('sfnGammaLnDescription', @sfnGammaLnDescription);
  AProduct.Add('sfnGammaLn_Precise', @sfnGammaLn_Precise);
  AProduct.Add('sfnGammaLn_PreciseDescription', @sfnGammaLn_PreciseDescription);
  AProduct.Add('sfnGauss', @sfnGauss);
  AProduct.Add('sfnGaussDescription', @sfnGaussDescription);
  AProduct.Add('sfnGeomean', @sfnGeomean);
  AProduct.Add('sfnGeomeanDescription', @sfnGeomeanDescription);
  AProduct.Add('sfnGrowth', @sfnGrowth);
  AProduct.Add('sfnGrowthDescription', @sfnGrowthDescription);
  AProduct.Add('sfnHarmean', @sfnHarmean);
  AProduct.Add('sfnHarmeanDescription', @sfnHarmeanDescription);
  AProduct.Add('sfnHypgeom_Dist', @sfnHypgeom_Dist);
  AProduct.Add('sfnHypgeom_DistDescription', @sfnHypgeom_DistDescription);
  AProduct.Add('sfnIntercept', @sfnIntercept);
  AProduct.Add('sfnInterceptDescription', @sfnInterceptDescription);
  AProduct.Add('sfnKurt', @sfnKurt);
  AProduct.Add('sfnKurtDescription', @sfnKurtDescription);
  AProduct.Add('sfnLarge', @sfnLarge);
  AProduct.Add('sfnLargeDescription', @sfnLargeDescription);
  AProduct.Add('sfnLinest', @sfnLinest);
  AProduct.Add('sfnLinestDescription', @sfnLinestDescription);
  AProduct.Add('sfnLogest', @sfnLogest);
  AProduct.Add('sfnLogestDescription', @sfnLogestDescription);
  AProduct.Add('sfnLogNorm_Dist', @sfnLogNorm_Dist);
  AProduct.Add('sfnLogNorm_DistDescription', @sfnLogNorm_DistDescription);
  AProduct.Add('sfnLogNorm_Inv', @sfnLogNorm_Inv);
  AProduct.Add('sfnLogNorm_InvDescription', @sfnLogNorm_InvDescription);
  AProduct.Add('sfnMax', @sfnMax);
  AProduct.Add('sfnMaxDescription', @sfnMaxDescription);
  AProduct.Add('sfnMaxA', @sfnMaxA);
  AProduct.Add('sfnMaxADescription', @sfnMaxADescription);
  AProduct.Add('sfnMedian', @sfnMedian);
  AProduct.Add('sfnMedianDescription', @sfnMedianDescription);
  AProduct.Add('sfnMin', @sfnMin);
  AProduct.Add('sfnMinDescription', @sfnMinDescription);
  AProduct.Add('sfnMinA', @sfnMinA);
  AProduct.Add('sfnMinADescription', @sfnMinADescription);
  AProduct.Add('sfnMode_Mult', @sfnMode_Mult);
  AProduct.Add('sfnMode_MultDescription', @sfnMode_MultDescription);
  AProduct.Add('sfnMode_SNGL', @sfnMode_SNGL);
  AProduct.Add('sfnMode_SNGLDescription', @sfnMode_SNGLDescription);
  AProduct.Add('sfnNegBinom_Dist', @sfnNegBinom_Dist);
  AProduct.Add('sfnNegBinom_DistDescription', @sfnNegBinom_DistDescription);
  AProduct.Add('sfnNorm_Dist', @sfnNorm_Dist);
  AProduct.Add('sfnNorm_DistDescription', @sfnNorm_DistDescription);
  AProduct.Add('sfnNorm_Inv', @sfnNorm_Inv);
  AProduct.Add('sfnNorm_InvDescription', @sfnNorm_InvDescription);
  AProduct.Add('sfnNorm_S_Dist', @sfnNorm_S_Dist);
  AProduct.Add('sfnNorm_S_DistDescription', @sfnNorm_S_DistDescription);
  AProduct.Add('sfnNorm_S_Inv', @sfnNorm_S_Inv);
  AProduct.Add('sfnNorm_S_InvDescription', @sfnNorm_S_InvDescription);
  AProduct.Add('sfnPearson', @sfnPearson);
  AProduct.Add('sfnPearsonDescription', @sfnPearsonDescription);
  AProduct.Add('sfnPercentile_Exc', @sfnPercentile_Exc);
  AProduct.Add('sfnPercentile_ExcDescription', @sfnPercentile_ExcDescription);
  AProduct.Add('sfnPercentile_Inc', @sfnPercentile_Inc);
  AProduct.Add('sfnPercentile_IncDescription', @sfnPercentile_IncDescription);
  AProduct.Add('sfnPercentRank_Exc', @sfnPercentRank_Exc);
  AProduct.Add('sfnPercentRank_ExcDescription', @sfnPercentRank_ExcDescription);
  AProduct.Add('sfnPercentRank_Inc', @sfnPercentRank_Inc);
  AProduct.Add('sfnPercentRank_IncDescription', @sfnPercentRank_IncDescription);
  AProduct.Add('sfnPermut', @sfnPermut);
  AProduct.Add('sfnPermutDescription', @sfnPermutDescription);
  AProduct.Add('sfnPermutationA', @sfnPermutationA);
  AProduct.Add('sfnPermutationADescription', @sfnPermutationADescription);
  AProduct.Add('sfnPHI', @sfnPHI);
  AProduct.Add('sfnPHIDescription', @sfnPHIDescription);
  AProduct.Add('sfnPoisson_Dist', @sfnPoisson_Dist);
  AProduct.Add('sfnPoisson_DistDescription', @sfnPoisson_DistDescription);
  AProduct.Add('sfnProb', @sfnProb);
  AProduct.Add('sfnProbDescription', @sfnProbDescription);
  AProduct.Add('sfnQuartile_Exc', @sfnQuartile_Exc);
  AProduct.Add('sfnQuartile_ExcDescription', @sfnQuartile_ExcDescription);
  AProduct.Add('sfnQuartile_Inc', @sfnQuartile_Inc);
  AProduct.Add('sfnQuartile_IncDescription', @sfnQuartile_IncDescription);
  AProduct.Add('sfnRank_Avg', @sfnRank_Avg);
  AProduct.Add('sfnRank_AvgDescription', @sfnRank_AvgDescription);
  AProduct.Add('sfnRank_Eq', @sfnRank_Eq);
  AProduct.Add('sfnRank_EqDescription', @sfnRank_EqDescription);
  AProduct.Add('sfnRSQ', @sfnRSQ);
  AProduct.Add('sfnRSQDescription', @sfnRSQDescription);
  AProduct.Add('sfnSkew', @sfnSkew);
  AProduct.Add('sfnSkewDescription', @sfnSkewDescription);
  AProduct.Add('sfnSkew_P', @sfnSkew_P);
  AProduct.Add('sfnSkew_PDescription', @sfnSkew_PDescription);
  AProduct.Add('sfnSlope', @sfnSlope);
  AProduct.Add('sfnSlopeDescription', @sfnSlopeDescription);
  AProduct.Add('sfnSmall', @sfnSmall);
  AProduct.Add('sfnSmallDescription', @sfnSmallDescription);
  AProduct.Add('sfnStandardize', @sfnStandardize);
  AProduct.Add('sfnStandardizeDescription', @sfnStandardizeDescription);
  AProduct.Add('sfnStDev_P', @sfnStDev_P);
  AProduct.Add('sfnStDev_PDescription', @sfnStDev_PDescription);
  AProduct.Add('sfnStDev_S', @sfnStDev_S);
  AProduct.Add('sfnStDev_SDescription', @sfnStDev_SDescription);
  AProduct.Add('sfnStDevA', @sfnStDevA);
  AProduct.Add('sfnStDevADescription', @sfnStDevADescription);
  AProduct.Add('sfnStDevPA', @sfnStDevPA);
  AProduct.Add('sfnStDevPADescription', @sfnStDevPADescription);
  AProduct.Add('sfnSTEYX', @sfnSTEYX);
  AProduct.Add('sfnSTEYXDescription', @sfnSTEYXDescription);
  AProduct.Add('sfnT_Dist', @sfnT_Dist);
  AProduct.Add('sfnT_DistDescription', @sfnT_DistDescription);
  AProduct.Add('sfnT_Dist_2T', @sfnT_Dist_2T);
  AProduct.Add('sfnT_Dist_2TDescription', @sfnT_Dist_2TDescription);
  AProduct.Add('sfnT_Dist_RT', @sfnT_Dist_RT);
  AProduct.Add('sfnT_Dist_RTDescription', @sfnT_Dist_RTDescription);
  AProduct.Add('sfnT_Inv', @sfnT_Inv);
  AProduct.Add('sfnT_InvDescription', @sfnT_InvDescription);
  AProduct.Add('sfnT_Inv_2T', @sfnT_Inv_2T);
  AProduct.Add('sfnT_Inv_2TDescription', @sfnT_Inv_2TDescription);
  AProduct.Add('sfnT_Test', @sfnT_Test);
  AProduct.Add('sfnT_TestDescription', @sfnT_TestDescription);
  AProduct.Add('sfnTrend', @sfnTrend);
  AProduct.Add('sfnTrendDescription', @sfnTrendDescription);
  AProduct.Add('sfnTrimMean', @sfnTrimMean);
  AProduct.Add('sfnTrimMeanDescription', @sfnTrimMeanDescription);
  AProduct.Add('sfnVar_P', @sfnVar_P);
  AProduct.Add('sfnVar_PDescription', @sfnVar_PDescription);
  AProduct.Add('sfnVar_S', @sfnVar_S);
  AProduct.Add('sfnVar_SDescription', @sfnVar_SDescription);
  AProduct.Add('sfnVarA', @sfnVarA);
  AProduct.Add('sfnVarADescription', @sfnVarADescription);
  AProduct.Add('sfnVarPA', @sfnVarPA);
  AProduct.Add('sfnVarPADescription', @sfnVarPADescription);
  AProduct.Add('sfnWeibull_Dist', @sfnWeibull_Dist);
  AProduct.Add('sfnWeibull_DistDescription', @sfnWeibull_DistDescription);
  AProduct.Add('sfnZ_Test', @sfnZ_Test);
  AProduct.Add('sfnZ_TestDescription', @sfnZ_TestDescription);

  // Information functions
  AProduct.Add('sfnCell', @sfnCell);
  AProduct.Add('sfnCellDescription', @sfnCellDescription);
  AProduct.Add('sfnError_Type', @sfnError_Type);
  AProduct.Add('sfnError_TypeDescription', @sfnError_TypeDescription);
  AProduct.Add('sfnInfo', @sfnInfo);
  AProduct.Add('sfnInfoDescription', @sfnInfoDescription);
  AProduct.Add('sfnIsBlank', @sfnIsBlank);
  AProduct.Add('sfnIsBlankDescription', @sfnIsBlankDescription);
  AProduct.Add('sfnIsErr', @sfnIsErr);
  AProduct.Add('sfnIsErrDescription', @sfnIsErrDescription);
  AProduct.Add('sfnIsError', @sfnIsError);
  AProduct.Add('sfnIsErrorDescription', @sfnIsErrorDescription);
  AProduct.Add('sfnIsEven', @sfnIsEven);
  AProduct.Add('sfnIsEvenDescription', @sfnIsEvenDescription);
  AProduct.Add('sfnIsFormula', @sfnIsFormula);
  AProduct.Add('sfnIsFormulaDescription', @sfnIsFormulaDescription);
  AProduct.Add('sfnIsLogical', @sfnIsLogical);
  AProduct.Add('sfnIsLogicalDescription', @sfnIsLogicalDescription);
  AProduct.Add('sfnIsNA', @sfnIsNA);
  AProduct.Add('sfnIsNADescription', @sfnIsNADescription);
  AProduct.Add('sfnIsNonText', @sfnIsNonText);
  AProduct.Add('sfnIsNonTextDescription', @sfnIsNonTextDescription);
  AProduct.Add('sfnIsNumber', @sfnIsNumber);
  AProduct.Add('sfnIsNumberDescription', @sfnIsNumberDescription);
  AProduct.Add('sfnIsOdd', @sfnIsOdd);
  AProduct.Add('sfnIsOddDescription', @sfnIsOddDescription);
  AProduct.Add('sfnIsRef', @sfnIsRef);
  AProduct.Add('sfnIsRefDescription', @sfnIsRefDescription);
  AProduct.Add('sfnIsText', @sfnIsText);
  AProduct.Add('sfnIsTextDescription', @sfnIsTextDescription);
  AProduct.Add('sfnN', @sfnN);
  AProduct.Add('sfnNDescription', @sfnNDescription);
  AProduct.Add('sfnNA', @sfnNA);
  AProduct.Add('sfnNADescription', @sfnNADescription);
  AProduct.Add('sfnSheet', @sfnSheet);
  AProduct.Add('sfnSheetDescription', @sfnSheetDescription);
  AProduct.Add('sfnSheets', @sfnSheets);
  AProduct.Add('sfnSheetsDescription', @sfnSheetsDescription);
  AProduct.Add('sfnType', @sfnType);
  AProduct.Add('sfnTypeDescription', @sfnTypeDescription);

  // Compatibility functions
  AProduct.Add('sfnBetaDist', @sfnBetaDist);
  AProduct.Add('sfnBetaDistDescription', @sfnBetaDistDescription);
  AProduct.Add('sfnBetaInv', @sfnBetaInv);
  AProduct.Add('sfnBetaInvDescription', @sfnBetaInvDescription);
  AProduct.Add('sfnBinomDist', @sfnBinomDist);
  AProduct.Add('sfnBinomDistDescription', @sfnBinomDistDescription);
  AProduct.Add('sfnChiDist', @sfnChiDist);
  AProduct.Add('sfnChiDistDescription', @sfnChiDistDescription);
  AProduct.Add('sfnChiInv', @sfnChiInv);
  AProduct.Add('sfnChiInvDescription', @sfnChiInvDescription);
  AProduct.Add('sfnChiTest', @sfnChiTest);
  AProduct.Add('sfnChiTestDescription', @sfnChiTestDescription);
  AProduct.Add('sfnConfidence', @sfnConfidence);
  AProduct.Add('sfnCovar', @sfnCovar);
  AProduct.Add('sfnCovarDescription', @sfnCovarDescription);
  AProduct.Add('sfnCritBinom', @sfnCritBinom);
  AProduct.Add('sfnCritBinomDescription', @sfnCritBinomDescription);
  AProduct.Add('sfnExponDist', @sfnExponDist);
  AProduct.Add('sfnExponDistDescription', @sfnExponDistDescription);
  AProduct.Add('sfnFDist', @sfnFDist);
  AProduct.Add('sfnFDistDescription', @sfnFDistDescription);
  AProduct.Add('sfnFInv', @sfnFInv);
  AProduct.Add('sfnFInvDescription', @sfnFInvDescription);
  AProduct.Add('sfnFTest', @sfnFTest);
  AProduct.Add('sfnFTestDescription', @sfnFTestDescription);
  AProduct.Add('sfnGammaDist', @sfnGammaDist);
  AProduct.Add('sfnGammaDistDescription', @sfnGammaDistDescription);
  AProduct.Add('sfnGammaInv', @sfnGammaInv);
  AProduct.Add('sfnGammaInvDescription', @sfnGammaInvDescription);
  AProduct.Add('sfnHypgeomDist', @sfnHypgeomDist);
  AProduct.Add('sfnHypgeomDistDescription', @sfnHypgeomDistDescription);
  AProduct.Add('sfnLogInv', @sfnLogInv);
  AProduct.Add('sfnLogInvDescription', @sfnLogInvDescription);
  AProduct.Add('sfnLogNormDist', @sfnLogNormDist);
  AProduct.Add('sfnLogNormDistDescription', @sfnLogNormDistDescription);
  AProduct.Add('sfnMode', @sfnMode);
  AProduct.Add('sfnModeDescription', @sfnModeDescription);
  AProduct.Add('sfnNegBinomDist', @sfnNegBinomDist);
  AProduct.Add('sfnNegBinomDistDescription', @sfnNegBinomDistDescription);
  AProduct.Add('sfnNormDist', @sfnNormDist);
  AProduct.Add('sfnNormDistDescription', @sfnNormDistDescription);
  AProduct.Add('sfnNormInv', @sfnNormInv);
  AProduct.Add('sfnNormInvDescription', @sfnNormInvDescription);
  AProduct.Add('sfnNormSDist', @sfnNormSDist);
  AProduct.Add('sfnNormSDistDescription', @sfnNormSDistDescription);
  AProduct.Add('sfnNormSInv', @sfnNormSInv);
  AProduct.Add('sfnNormSInvDescription', @sfnNormSInvDescription);
  AProduct.Add('sfnPercentile', @sfnPercentile);
  AProduct.Add('sfnPercentileDescription', @sfnPercentileDescription);
  AProduct.Add('sfnPercentRank', @sfnPercentRank);
  AProduct.Add('sfnPercentRankDescription', @sfnPercentRankDescription);
  AProduct.Add('sfnPoisson', @sfnPoisson);
  AProduct.Add('sfnPoissonDescription', @sfnPoissonDescription);
  AProduct.Add('sfnQuartile', @sfnQuartile);
  AProduct.Add('sfnQuartileDescription', @sfnQuartileDescription);
  AProduct.Add('sfnRank', @sfnRank);
  AProduct.Add('sfnRankDescription', @sfnRankDescription);
  AProduct.Add('sfnStDev', @sfnStDev);
  AProduct.Add('sfnStDevDescription', @sfnStDevDescription);
  AProduct.Add('sfnStDevP', @sfnStDevP);
  AProduct.Add('sfnStDevPDescription', @sfnStDevPDescription);
  AProduct.Add('sfnTDist', @sfnTDist);
  AProduct.Add('sfnTDistDescription', @sfnTDistDescription);
  AProduct.Add('sfnTInv', @sfnTInv);
  AProduct.Add('sfnTInvDescription', @sfnTInvDescription);
  AProduct.Add('sfnTTest', @sfnTTest);
  AProduct.Add('sfnTTestDescription', @sfnTTestDescription);
  AProduct.Add('sfnVar', @sfnVar);
  AProduct.Add('sfnVarDescription', @sfnVarDescription);
  AProduct.Add('sfnVarP', @sfnVarP);
  AProduct.Add('sfnVarPDescription', @sfnVarPDescription);
  AProduct.Add('sfnWeibull', @sfnWeibull);
  AProduct.Add('sfnWeibullDescription', @sfnWeibullDescription);
  AProduct.Add('sfnZTest', @sfnZTest);
  AProduct.Add('sfnZTestDescription', @sfnZTestDescription);

  // DateTime functions
  AProduct.Add('sfnDate', @sfnDate);
  AProduct.Add('sfnDateDescription', @sfnDateDescription);
  AProduct.Add('sfnDateValue', @sfnDateValue);
  AProduct.Add('sfnDateValueDescription', @sfnDateValueDescription);
  AProduct.Add('sfnDay', @sfnDay);
  AProduct.Add('sfnDayDescription', @sfnDayDescription);
  AProduct.Add('sfnDays', @sfnDays);
  AProduct.Add('sfnDaysDescription', @sfnDaysDescription);
  AProduct.Add('sfnDays360', @sfnDays360);
  AProduct.Add('sfnDays360Description', @sfnDays360Description);
  AProduct.Add('sfnEDate', @sfnEDate);
  AProduct.Add('sfnEDateDescription', @sfnEDateDescription);
  AProduct.Add('sfnEOMonth', @sfnEOMonth);
  AProduct.Add('sfnEOMonthDescription', @sfnEOMonthDescription);
  AProduct.Add('sfnHour', @sfnHour);
  AProduct.Add('sfnHourDescription', @sfnHourDescription);
  AProduct.Add('sfnIsoWeekNum', @sfnIsoWeekNum);
  AProduct.Add('sfnIsoWeekNumDescription', @sfnIsoWeekNumDescription);
  AProduct.Add('sfnMinute', @sfnMinute);
  AProduct.Add('sfnMinuteDescription', @sfnMinuteDescription);
  AProduct.Add('sfnMonth', @sfnMonth);
  AProduct.Add('sfnMonthDescription', @sfnMonthDescription);
  AProduct.Add('sfnNetworkDays', @sfnNetworkDays);
  AProduct.Add('sfnNetworkDaysDescription', @sfnNetworkDaysDescription);
  AProduct.Add('sfnNetworkDays_Intl', @sfnNetworkDays_Intl);
  AProduct.Add('sfnNetworkDays_IntlDescription', @sfnNetworkDays_IntlDescription);
  AProduct.Add('sfnNow', @sfnNow);
  AProduct.Add('sfnNowDescription', @sfnNowDescription);
  AProduct.Add('sfnSecond', @sfnSecond);
  AProduct.Add('sfnSecondDescription', @sfnSecondDescription);
  AProduct.Add('sfnTime', @sfnTime);
  AProduct.Add('sfnTimeDescription', @sfnTimeDescription);
  AProduct.Add('sfnTimeValue', @sfnTimeValue);
  AProduct.Add('sfnTimeValueDescription', @sfnTimeValueDescription);
  AProduct.Add('sfnToday', @sfnToday);
  AProduct.Add('sfnTodayDescription', @sfnTodayDescription);
  AProduct.Add('sfnWeekDay', @sfnWeekDay);
  AProduct.Add('sfnWeekDayDescription', @sfnWeekDayDescription);
  AProduct.Add('sfnWeekNum', @sfnWeekNum);
  AProduct.Add('sfnWeekNumDescription', @sfnWeekNumDescription);
  AProduct.Add('sfnWorkDay', @sfnWorkDay);
  AProduct.Add('sfnWorkDayDescription', @sfnWorkDayDescription);
  AProduct.Add('sfnWorkDay_Intl', @sfnWorkDay_Intl);
  AProduct.Add('sfnWorkDay_IntlDescription', @sfnWorkDay_IntlDescription);
  AProduct.Add('sfnYear', @sfnYear);
  AProduct.Add('sfnYearDescription', @sfnYearDescription);
  AProduct.Add('sfnYearFrac', @sfnYearFrac);
  AProduct.Add('sfnYearFracDescription', @sfnYearFracDescription);

  // Financial functions
  AProduct.Add('sfnAccrInt', @sfnAccrInt);
  AProduct.Add('sfnAccrIntDescription', @sfnAccrIntDescription);
  AProduct.Add('sfnAccrIntM', @sfnAccrIntM);
  AProduct.Add('sfnAccrIntMDescription', @sfnAccrIntMDescription);
  AProduct.Add('sfnAmorDegr', @sfnAmorDegr);
  AProduct.Add('sfnAmorDegrDescription', @sfnAmorDegrDescription);
  AProduct.Add('sfnAmorLinc', @sfnAmorLinc);
  AProduct.Add('sfnAmorLincDescription', @sfnAmorLincDescription);
  AProduct.Add('sfnCoupDayBS', @sfnCoupDayBS);
  AProduct.Add('sfnCoupDayBSDescription', @sfnCoupDayBSDescription);
  AProduct.Add('sfnCoupDays', @sfnCoupDays);
  AProduct.Add('sfnCoupDaysDescription', @sfnCoupDaysDescription);
  AProduct.Add('sfnCoupDaysNC', @sfnCoupDaysNC);
  AProduct.Add('sfnCoupDaysNCDescription', @sfnCoupDaysNCDescription);
  AProduct.Add('sfnCoupNCD', @sfnCoupNCD);
  AProduct.Add('sfnCoupNCDDescription', @sfnCoupNCDDescription);
  AProduct.Add('sfnCoupNum', @sfnCoupNum);
  AProduct.Add('sfnCoupNumDescription', @sfnCoupNumDescription);
  AProduct.Add('sfnCoupPCD', @sfnCoupPCD);
  AProduct.Add('sfnCoupPCDDescription', @sfnCoupPCDDescription);
  AProduct.Add('sfnCoupIPMT', @sfnCoupIPMT);
  AProduct.Add('sfnCoupIPMTDescription', @sfnCoupIPMTDescription);
  AProduct.Add('sfnCoupRINC', @sfnCoupRINC);
  AProduct.Add('sfnCoupRINCDescription', @sfnCoupRINCDescription);
  AProduct.Add('sfnDB', @sfnDB);
  AProduct.Add('sfnDBDescription', @sfnDBDescription);
  AProduct.Add('sfnDDB', @sfnDDB);
  AProduct.Add('sfnDDBDescription', @sfnDDBDescription);
  AProduct.Add('sfnDisc', @sfnDisc);
  AProduct.Add('sfnDiscDescription', @sfnDiscDescription);
  AProduct.Add('sfnDollarDe', @sfnDollarDe);
  AProduct.Add('sfnDollarDeDescription', @sfnDollarDeDescription);
  AProduct.Add('sfnDollarFr', @sfnDollarFr);
  AProduct.Add('sfnDollarFrDescription', @sfnDollarFrDescription);
  AProduct.Add('sfnDuration', @sfnDuration);
  AProduct.Add('sfnDurationDescription', @sfnDurationDescription);
  AProduct.Add('sfnEffect', @sfnEffect);
  AProduct.Add('sfnEffectDescription', @sfnEffectDescription);
  AProduct.Add('sfnFV', @sfnFV);
  AProduct.Add('sfnFVDescription', @sfnFVDescription);
  AProduct.Add('sfnIPMT', @sfnIPMT);
  AProduct.Add('sfnIPMTDescription', @sfnIPMTDescription);
  AProduct.Add('sfnFVSchedule', @sfnFVSchedule);
  AProduct.Add('sfnFVScheduleDescription', @sfnFVScheduleDescription);
  AProduct.Add('sfnIntRate', @sfnIntRate);
  AProduct.Add('sfnIntRateDescription', @sfnIntRateDescription);
  AProduct.Add('sfnIRR', @sfnIRR);
  AProduct.Add('sfnIRRDescription', @sfnIRRDescription);
  AProduct.Add('sfnIsPMT', @sfnIsPMT);
  AProduct.Add('sfnIsPMTDescription', @sfnIsPMTDescription);
  AProduct.Add('sfnMDuration', @sfnMDuration);
  AProduct.Add('sfnMDurationDescription', @sfnMDurationDescription);
  AProduct.Add('sfnMIRR', @sfnMIRR);
  AProduct.Add('sfnMIRRDescription', @sfnMIRRDescription);
  AProduct.Add('sfnNominal', @sfnNominal);
  AProduct.Add('sfnNominalDescription', @sfnNominalDescription);
  AProduct.Add('sfnNPer', @sfnNPer);
  AProduct.Add('sfnNPerDescription', @sfnNPerDescription);
  AProduct.Add('sfnNPV', @sfnNPV);
  AProduct.Add('sfnNPVDescription', @sfnNPVDescription);
  AProduct.Add('sfnOddFPrice', @sfnOddFPrice);
  AProduct.Add('sfnOddFPriceDescription', @sfnOddFPriceDescription);
  AProduct.Add('sfnOddFYield', @sfnOddFYield);
  AProduct.Add('sfnOddFYieldDescription', @sfnOddFYieldDescription);
  AProduct.Add('sfnOddLPrice', @sfnOddLPrice);
  AProduct.Add('sfnOddLPriceDescription', @sfnOddLPriceDescription);
  AProduct.Add('sfnOddLYield', @sfnOddLYield);
  AProduct.Add('sfnOddLYieldDescription', @sfnOddLYieldDescription);
  AProduct.Add('sfnPDuration', @sfnPDuration);
  AProduct.Add('sfnPDurationDescription', @sfnPDurationDescription);
  AProduct.Add('sfnPMT', @sfnPMT);
  AProduct.Add('sfnPMTDescription', @sfnPMTDescription);
  AProduct.Add('sfnPPMT', @sfnPPMT);
  AProduct.Add('sfnPPMTDescription', @sfnPPMTDescription);
  AProduct.Add('sfnPrice', @sfnPrice);
  AProduct.Add('sfnPriceDescription', @sfnPriceDescription);
  AProduct.Add('sfnPriceDisc', @sfnPriceDisc);
  AProduct.Add('sfnPriceDiscDescription', @sfnPriceDiscDescription);
  AProduct.Add('sfnPriceMat', @sfnPriceMat);
  AProduct.Add('sfnPriceMatDescription', @sfnPriceMatDescription);
  AProduct.Add('sfnPV', @sfnPV);
  AProduct.Add('sfnPVDescription', @sfnPVDescription);
  AProduct.Add('sfnRate', @sfnRate);
  AProduct.Add('sfnRateDescription', @sfnRateDescription);
  AProduct.Add('sfnReceived', @sfnReceived);
  AProduct.Add('sfnReceivedDescription', @sfnReceivedDescription);
  AProduct.Add('sfnRRI', @sfnRRI);
  AProduct.Add('sfnRRIDescription', @sfnRRIDescription);
  AProduct.Add('sfnSLN', @sfnSLN);
  AProduct.Add('sfnSLNDescription', @sfnSLNDescription);
  AProduct.Add('sfnSYD', @sfnSYD);
  AProduct.Add('sfnSYDDescription', @sfnSYDDescription);
  AProduct.Add('sfnTBillEq', @sfnTBillEq);
  AProduct.Add('sfnTBillEqDescription', @sfnTBillEqDescription);
  AProduct.Add('sfnTBillPrice', @sfnTBillPrice);
  AProduct.Add('sfnTBillPriceDescription', @sfnTBillPriceDescription);
  AProduct.Add('sfnTBillYield', @sfnTBillYield);
  AProduct.Add('sfnTBillYieldDescription', @sfnTBillYieldDescription);
  AProduct.Add('sfnVDB', @sfnVDB);
  AProduct.Add('sfnVDBDescription', @sfnVDBDescription);
  AProduct.Add('sfnXIRR', @sfnXIRR);
  AProduct.Add('sfnXIRRDescription', @sfnXIRRDescription);
  AProduct.Add('sfnXNPV', @sfnXNPV);
  AProduct.Add('sfnXNPVDescription', @sfnXNPVDescription);
  AProduct.Add('sfnYield', @sfnYield);
  AProduct.Add('sfnYieldDescription', @sfnYieldDescription);
  AProduct.Add('sfnYieldDisc', @sfnYieldDisc);
  AProduct.Add('sfnYieldDiscDescription', @sfnYieldDiscDescription);
  AProduct.Add('sfnYieldMat', @sfnYieldMat);
  AProduct.Add('sfnYieldMatDescription', @sfnYieldMatDescription);

  // Cube functions
  AProduct.Add('sfnCubeKPIMember', @sfnCubeKPIMember);
  AProduct.Add('sfnCubeMember', @sfnCubeMember);
  AProduct.Add('sfnCubeMemberProperty', @sfnCubeMemberProperty);
  AProduct.Add('sfnCubeRankedMember', @sfnCubeRankedMember);
  AProduct.Add('sfnCubeSet', @sfnCubeSet);
  AProduct.Add('sfnCubeSetCount', @sfnCubeSetCount);
  AProduct.Add('sfnCubeValue', @sfnCubeValue);

  // Database functions
  AProduct.Add('sfnDAverage', @sfnDAverage);
  AProduct.Add('sfnDCount', @sfnDCount);
  AProduct.Add('sfnDCountA', @sfnDCountA);
  AProduct.Add('sfnDGet', @sfnDGet);
  AProduct.Add('sfnDMax', @sfnDMax);
  AProduct.Add('sfnDMin', @sfnDMin);
  AProduct.Add('sfnDProduct', @sfnDProduct);
  AProduct.Add('sfnDStDev', @sfnDStDev);
  AProduct.Add('sfnDStDevP', @sfnDStDevP);
  AProduct.Add('sfnDSum', @sfnDSum);
  AProduct.Add('sfnDVar', @sfnDVar);
  AProduct.Add('sfnDVarP', @sfnDVarP);

  // Engineering functions
  AProduct.Add('sfnBesselI', @sfnBesselI);
  AProduct.Add('sfnBesselJ', @sfnBesselJ);
  AProduct.Add('sfnBesselK', @sfnBesselK);
  AProduct.Add('sfnBesselY', @sfnBesselY);
  AProduct.Add('sfnBin2Dec', @sfnBin2Dec);
  AProduct.Add('sfnBin2Hex', @sfnBin2Hex);
  AProduct.Add('sfnBin2Oct', @sfnBin2Oct);
  AProduct.Add('sfnBitAnd', @sfnBitAnd);
  AProduct.Add('sfnBitLShift', @sfnBitLShift);
  AProduct.Add('sfnBitOr', @sfnBitOr);
  AProduct.Add('sfnBitRShift', @sfnBitRShift);
  AProduct.Add('sfnBitXor', @sfnBitXor);
  AProduct.Add('sfnComplex', @sfnComplex);
  AProduct.Add('sfnConvert', @sfnConvert);
  AProduct.Add('sfnDec2Bin', @sfnDec2Bin);
  AProduct.Add('sfnDec2Hex', @sfnDec2Hex);
  AProduct.Add('sfnDec2Oct', @sfnDec2Oct);
  AProduct.Add('sfnDelta', @sfnDelta);
  AProduct.Add('sfnERF', @sfnERF);
  AProduct.Add('sfnERF_Precise', @sfnERF_Precise);
  AProduct.Add('sfnERFC', @sfnERFC);
  AProduct.Add('sfnERFC_Precise', @sfnERFC_Precise);
  AProduct.Add('sfnGestep', @sfnGestep);
  AProduct.Add('sfnHex2Bin', @sfnHex2Bin);
  AProduct.Add('sfnHex2Dec', @sfnHex2Dec);
  AProduct.Add('sfnHex2Oct', @sfnHex2Oct);
  AProduct.Add('sfnImAbs', @sfnImAbs);
  AProduct.Add('sfnImAginary', @sfnImAginary);
  AProduct.Add('sfnImArgument', @sfnImArgument);
  AProduct.Add('sfnImConjugate', @sfnImConjugate);
  AProduct.Add('sfnImCos', @sfnImCos);
  AProduct.Add('sfnImCosh', @sfnImCosh);
  AProduct.Add('sfnImCot', @sfnImCot);
  AProduct.Add('sfnImCsc', @sfnImCsc);
  AProduct.Add('sfnImCsch', @sfnImCsch);
  AProduct.Add('sfnImDiv', @sfnImDiv);
  AProduct.Add('sfnImExp', @sfnImExp);
  AProduct.Add('sfnImLn', @sfnImLn);
  AProduct.Add('sfnImLog10', @sfnImLog10);
  AProduct.Add('sfnImLog2', @sfnImLog2);
  AProduct.Add('sfnImPower', @sfnImPower);
  AProduct.Add('sfnImProduct', @sfnImProduct);
  AProduct.Add('sfnImReal', @sfnImReal);
  AProduct.Add('sfnImSec', @sfnImSec);
  AProduct.Add('sfnImSech', @sfnImSech);
  AProduct.Add('sfnImSin', @sfnImSin);
  AProduct.Add('sfnImSinh', @sfnImSinh);
  AProduct.Add('sfnImSqrt', @sfnImSqrt);
  AProduct.Add('sfnImSub', @sfnImSub);
  AProduct.Add('sfnImSum', @sfnImSum);
  AProduct.Add('sfnImTan', @sfnImTan);
  AProduct.Add('sfnOct2Bin', @sfnOct2Bin);
  AProduct.Add('sfnOct2Dec', @sfnOct2Dec);
  AProduct.Add('sfnOct2Hex', @sfnOct2Hex);

  // Logical functions
  AProduct.Add('sfnAnd', @sfnAnd);
  AProduct.Add('sfnAndDescription', @sfnAndDescription);
  AProduct.Add('sfnFalse', @sfnFalse);
  AProduct.Add('sfnFalseDescription', @sfnFalseDescription);
  AProduct.Add('sfnIF', @sfnIF);
  AProduct.Add('sfnIFDescription', @sfnIFDescription);
  AProduct.Add('sfnIfError', @sfnIfError);
  AProduct.Add('sfnIfErrorDescription', @sfnIfErrorDescription);
  AProduct.Add('sfnIfNA', @sfnIfNA);
  AProduct.Add('sfnIfNADescription', @sfnIfNADescription);
  AProduct.Add('sfnNot', @sfnNot);
  AProduct.Add('sfnNotDescription', @sfnNotDescription);
  AProduct.Add('sfnOr', @sfnOr);
  AProduct.Add('sfnOrDescription', @sfnOrDescription);
  AProduct.Add('sfnTrue', @sfnTrue);
  AProduct.Add('sfnTrueDescription', @sfnTrueDescription);
  AProduct.Add('sfnXor', @sfnXor);
  AProduct.Add('sfnXorDescription', @sfnXorDescription);
  AProduct.Add('sfnAddress', @sfnAddress);
  AProduct.Add('sfnAddressDescription', @sfnAddressDescription);
  AProduct.Add('sfnAreas', @sfnAreas);
  AProduct.Add('sfnAreasDescription', @sfnAreasDescription);
  AProduct.Add('sfnChoose', @sfnChoose);
  AProduct.Add('sfnChooseDescription', @sfnChooseDescription);
  AProduct.Add('sfnColumn', @sfnColumn);
  AProduct.Add('sfnColumnDescription', @sfnColumnDescription);
  AProduct.Add('sfnColumns', @sfnColumns);
  AProduct.Add('sfnColumnsDescription', @sfnColumnsDescription);
  AProduct.Add('sfnFormulaText', @sfnFormulaText);
  AProduct.Add('sfnFormulaTextDescription', @sfnFormulaTextDescription);
  AProduct.Add('sfnGetPivotData', @sfnGetPivotData);
  AProduct.Add('sfnGetPivotDataDescription', @sfnGetPivotDataDescription);
  AProduct.Add('sfnHLookup', @sfnHLookup);
  AProduct.Add('sfnHLookupDescription', @sfnHLookupDescription);
  AProduct.Add('sfnHyperlink', @sfnHyperlink);
  AProduct.Add('sfnHyperlinkDescription', @sfnHyperlinkDescription);
  AProduct.Add('sfnLookup', @sfnLookup);
  AProduct.Add('sfnLookupDescription', @sfnLookupDescription);
  AProduct.Add('sfnIndex', @sfnIndex);
  AProduct.Add('sfnIndexDescription', @sfnIndexDescription);
  AProduct.Add('sfnIndirect', @sfnIndirect);
  AProduct.Add('sfnIndirectDescription', @sfnIndirectDescription);
  AProduct.Add('sfnMatch', @sfnMatch);
  AProduct.Add('sfnMatchDescription', @sfnMatchDescription);
  AProduct.Add('sfnOffset', @sfnOffset);
  AProduct.Add('sfnOffsetDescription', @sfnOffsetDescription);
  AProduct.Add('sfnRow', @sfnRow);
  AProduct.Add('sfnRowDescription', @sfnRowDescription);
  AProduct.Add('sfnRows', @sfnRows);
  AProduct.Add('sfnRowsDescription', @sfnRowsDescription);
  AProduct.Add('sfnRTD', @sfnRTD);
  AProduct.Add('sfnRTDDescription', @sfnRTDDescription);
  AProduct.Add('sfnTranspose', @sfnTranspose);
  AProduct.Add('sfnTransposeDescription', @sfnTransposeDescription);
  AProduct.Add('sfnVLookup', @sfnVLookup);
  AProduct.Add('sfnVLookupDescription', @sfnVLookupDescription);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressSpreadSheet 2', @AddSpreadSheetResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressSpreadSheet 2', @AddSpreadSheetResourceStringNames);
end.
