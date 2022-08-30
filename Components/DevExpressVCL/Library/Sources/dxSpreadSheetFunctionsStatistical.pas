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

unit dxSpreadSheetFunctionsStatistical;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, Classes, DateUtils, Variants, Math, Generics.Defaults, Generics.Collections,
  cxVariants, dxHashUtils, cxDateUtils, dxCore, dxCoreClasses, cxClasses,
  // SpreadSheet
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetTypes,
  dxSpreadSheetUtils;

procedure fnAveDev(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnAverage(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnAverageA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnAverageIF(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnAverageIFS(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnBetaDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnBeta_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnBeta_Inv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnBinom_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnBinom_Dist_Range(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnChiSQ_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnChiSQ_Dist_RT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnChiSQ_Inv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnChiSQ_Inv_RT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCorrel(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCovariance_P(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCovariance_S(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCount(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCountA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCountBlank(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCountIF(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnCountIFS(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnDevSQ(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnExpon_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnForecast(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnGamma(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnGamma_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnGamma_Inv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnGammaLn(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnGammaLn_Precise(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnGauss(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnGeomean(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnHypgeomDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnHypgeom_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnIntercept(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnLarge(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnMax(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnMaxA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnMedian(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnMin(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnMinA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnMode_Mult(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnMode_Sngl(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnNorm_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnNorm_Inv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnNormSDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnNorm_S_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnNorm_S_Inv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnPearson(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnPercentile_Exc(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnPercentile_Inc(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnPermut(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnPoisson_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnProduct(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnQuartile_Exc(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnQuartile_Inc(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnRank_AVG(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnRank_EQ(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnRSQ(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSkew(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSkew_P(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSlope(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSmall(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnStandardize(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnStDev_S(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnStDev_P(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnStDevA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnStDevPA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSTEYX(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSubTotal(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSum(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSumIF(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSumIFS(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSumProduct(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSumSQ(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSumX2MY2(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSumX2PY2(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnSumXMY2(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnTDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnT_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnT_Dist_2T(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnT_Dist_RT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnTInv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnT_Inv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnT_Inv_2T(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnVar_P(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnVar_S(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnVarA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnVarPA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnWeibull_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

procedure fpiAveDev(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiAverage(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiAverageA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiAverageIF(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiAverageIFS(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiBeta_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiBeta_Inv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiBinom_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiBinom_Dist_Range(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiChiSQ_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiChiSQ_Dist_RT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiChiSQ_Inv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiChiSQ_Inv_RT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCorrel(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCount(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCountA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCountBlank(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCountIF(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCountIFS(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCovariance_P(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCovariance_S(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiDevSQ(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiExpon_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiForecast(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiGamma(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiGamma_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiGamma_Inv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiGammaLn(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiGammaLn_Precise(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiGauss(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiGeomean(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiHypgeom_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiIntercept(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiLarge(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiMax(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiMaxA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiMedian(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiMin(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiMinA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiMode_Mult(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiMode_Sngl(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiNorm_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiNorm_Inv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiNorm_S_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiNorm_S_Inv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiPearson(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiPercentile_Exc(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiPercentile_Inc(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiPermut(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiPoisson_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiQuartile_Exc(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiQuartile_Inc(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiRank_Avg(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiRank_Eq(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiRSQ(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSkew(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSkew_P(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSlope(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSmall(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiStandardize(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiStDev_S(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiStDev_P(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiStDevA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiStDevPA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSTEYX(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiSubTotal(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiT_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiT_Dist_2T(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiT_Dist_RT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiT_Inv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiT_Inv_2T(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiVar_P(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiVar_S(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiVarA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiVarPA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiWeibull_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);

function dxGetCombin(N, K: Integer): Extended; inline;
function dxGetFactorial(AValue: Integer): Extended; inline;
implementation

uses
  // SpreadSheet
  dxSpreadSheetClasses,
  dxSpreadSheetCoreFormulasHelpers,
  dxSpreadSheetCoreFormulasTokens,
  dxSpreadSheetFunctions,
  dxSpreadSheetFunctionsStrs,
  dxSpreadSheetFunctionsText;

const
  dxBetaDistMachineEpsilon = 1.11022302462515654042E-16;
  dxBetaDistMaxLog = 7.09782712893383996732E2;
  dxBetaDistMinLog = -7.451332191019412076235E2;
  dxBetaDistMaxGamma = 171.624376956302725;

  dxIncompleteGammaEpsilon = 1E-15;
  dxIncompleteGammaBigNumber = 4503599627370496;
  dxIncompleteGammaBigNumberInv = 2.22044604925031308085E-16;

type
  TdxTokenAccess = class(TdxSpreadSheetFormulaToken);
  TdxReferenceAccess = class(TdxSpreadSheetFormulaReference);
  TdxFormulaAccess = class(TdxSpreadSheetCustomFormula);

  TdxAccumulationType = (atNone, atSum, atSumSQ, atSumProduct, atCount);


  TdxCalculateRatioItemsProcedure = procedure (const AArrayX, AArrayY: Variant;
    const AArraysDimension: TdxSpreadSheetFormulaTokenDimension;
    const AAvgX, AAvgY: Extended; var ANominator, ADenominator: Extended);
  TdxCustomVariance = function(ANumerics: TList<Double>; AAverage: Double): Double;
  TdxExtendedFunctionOfExtendedArgument = function(const X: Extended): Extended;
  TdxSetValue = procedure(const AValue: Variant; AData: TdxSpreadSheetEnumValues);

  { TdxOccurrence }

  TdxOccurrence = class
    Number: Double;
    Count: Integer;
    constructor Create(ANumber: Double; ACount: Integer);
  end;

constructor TdxOccurrence.Create(ANumber: Double; ACount: Integer);
begin
  Number := ANumber;
  Count := ACount;
end;

function dxIsIfConditionEqualValue(const AIfCondition, AValue: Variant): Boolean; inline;
var
  AIfConditionIsNull, AValueIsNull: Boolean;
begin
  AIfConditionIsNull := dxSpreadSheetIsNullValue(AIfCondition);
  AValueIsNull := dxSpreadSheetIsNullValue(AValue);
  Result := AIfConditionIsNull and AValueIsNull;
  if not Result then
  begin
    if AIfConditionIsNull or AValueIsNull then
      Result := False
    else
      Result :=(dxSpreadSheetVarCompare(AValue, AIfCondition) = 0) or (dxSearchValueInText(AIfCondition, AValue, 1, False) = 1);
  end;
end;

function dxGetBinomialProbabilityMass(X, N: Integer; P: Extended): Extended; inline;
begin
  Result := dxGetCombin(N, X) * Power(P, X) * Power(1 - P, N - X);
end;

function dxGetExponDist(X, L: Extended; ACumulative: Boolean): Extended;
begin
  Result := Exp(-L * X);
  if ACumulative then
    Result := 1 - Result
  else
    Result := L * Result;
end;

function dxGetPermut(N, K: Integer): Extended; inline;
var
  I: Integer;
begin
  Result := 1;
  for I := N - K + 1 to N do
    Result := Int(Result * I);
end;

function dxGetPoisson(X: Integer; L: Extended): Extended;
begin
  Result := Exp(-L) * Power(L, X) / dxGetFactorial(X);
end;

function dxGetCombin(N, K: Integer): Extended; inline;
var
  I: Integer;
begin
  Result := 1;
  if N < K then
    Result := 0
  else
    if N <> K then
      if K > N div 2 then
      begin
        for I := K + 1 to N do
          Result := Int(Result * I);
        Result := Result / dxGetFactorial(N - K);
      end
      else
        Result := dxGetPermut(N, K) / dxGetFactorial(K);
end;

function dxGetFactorial(AValue: Integer): Extended; inline;
var
  I: Integer;
begin
  Result := 1;
  for I := 2 to AValue do
    Result := Int(Result * I);
end;

function dxGetSumOfAbsoluteDeviations(ANumerics: TList<Double>; AAverage: Double): Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ANumerics.Count - 1 do
    Result := Result + Abs(ANumerics[I] - AAverage);
end;

function dxGetSumOfSquaresOfDeviations(ANumerics: TList<Double>; AAverage: Double): Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ANumerics.Count - 1 do
    Result := Result + Sqr(ANumerics[I] - AAverage);
end;

function dxGetIntegral(const XBeg, XEnd: Extended; AFunc: TdxExtendedFunctionOfExtendedArgument): Extended;

  function InternalGetResult(AIntervalCount: Integer): Extended;
  var
    AIntervalSize, AHalfInterval, X: Extended;
    I: Integer;
  begin
    AIntervalSize := (XEnd - XBeg) / AIntervalCount;
    AHalfInterval := AIntervalSize / 2;
    Result := 0;
    for I := 0 to AIntervalCount - 1 do
    begin
      X := XBeg + AIntervalSize * I;
      Result := Result + AIntervalSize * (AFunc(X) + 4 * AFunc(X + AHalfInterval) + AFunc(X + AIntervalSize)) / 6;
    end;
  end;

var
  AIntervalCount: Integer;
  AResult1: Extended;
begin
  AResult1 := InternalGetResult(1);
  AIntervalCount := 2;
  Result := InternalGetResult(AIntervalCount);
  while (Abs(Result - AResult1) > 1E-15) and ((Abs(XEnd - XBeg) / AIntervalCount) > 1E-7) do
  begin
    AResult1 := Result;
    AIntervalCount := AIntervalCount * 2;
    Result := InternalGetResult(AIntervalCount);
  end;
end;

function dxGetDensityStandartNormDist(X: Extended): Extended;
begin
  Result := 0.3989422804014326779 * Exp(-X * X / 2);
end;

function dxGetStandartNormDist(X: Extended): Extended;
const
  APointCount = 10;
  ALaplaceTable: array[0..APointCount - 1] of Extended =
    (0.5, 0.841344746068543, 0.977249868051821, 0.998650101968370, 0.999968328758167,
     0.999999713348428, 0.999999999013412, 0.999999999998720, 0.999999999999999, 1);
  AStep = 1;
var
  AAbsX: Extended;
  ANearestTableArgumentNumber: Integer;
begin
  AAbsX := Abs(X);
  if AAbsX >= (APointCount - 1) * AStep then
    Result := 1
  else
  begin
    ANearestTableArgumentNumber := Trunc(AAbsX / AStep + AStep / 2);
    Result := ALaplaceTable[ANearestTableArgumentNumber] +
      dxGetIntegral(ANearestTableArgumentNumber * AStep, AAbsX, @dxGetDensityStandartNormDist);
  end;
  if X < 0 then
    Result := 1 - Result;
end;

function dxGetNormDist(X, AMean, AStdDev: Extended; ACumulative: Boolean): Extended;
begin
  if not ACumulative then
    Result := dxGetDensityStandartNormDist((X - AMean)/AStdDev) / AStdDev
  else
    Result := dxGetStandartNormDist((X - AMean)/AStdDev);
end;

function dxGetStandartNormInv(X: Extended): Extended;

   function GetPolynomial(AArgument: Extended; AConsts: array of Extended): Extended;
   var
     I: Integer;
   begin
     Result := AConsts[0];
     for I := 1 to High(AConsts) do
       Result := AConsts[I] + AArgument * Result;
   end;

  function GetApproximation(AArgument, AHelper0, AHelper1: Extended; AType: Integer): Extended;
  const
    AConstsA0: array[0..4] of Extended = (-5.99633501014107895267E+1, 9.80010754185999661536E+1, -5.66762857469070293439E+1,
                                          1.39312609387279679503E+1, -1.23916583867381258016);

    AConstsA1: array[0..8] of Extended = (4.05544892305962419923, 3.15251094599893866154E+1, 5.71628192246421288162E+1,
                                      4.408050738932008347E+1, 1.46849561928858024014E+1, 2.18663306850790267539,
                                     -1.40256079171354495875E-1, -3.50424626827848203418E-2, -8.57456785154685413611E-4);

    AConstsA2: array[0..8] of Extended = (3.2377489177694603597, 6.91522889068984211695, 3.93881025292474443415,
                                        1.33303460815807542389, 2.01485389549179081538E-1, 1.23716634817820021358E-2,
                                        3.01581553508235416007E-4, 2.65806974686737550832E-6, 6.2397453918498329373E-9);

    AConstsB0: array[0..8] of Extended = (1, 1.95448858338141759834, 4.67627912898881538453, 8.63602421390890590575E+1,
                                       -2.25462687854119370527E+2, 2.00260212380060660359E+2, -8.20372256168333339912E+1,
                                        1.59056225126211695515E+1, -1.18331621121330003142);

    AConstsB1: array[0..8] of Extended = (1, 1.57799883256466749731E+1, 4.53907635128879210584E+1, 4.1317203825467203044E+1,
                                        1.50425385692907503408E+1, 2.50464946208309415979, -1.42182922854787788574E-1,
                                       -3.80806407691578277194E-2, -9.33259480895457427372E-4);

    AConstsB2: array[0..8] of Extended = (1, 6.02427039364742014255, 3.67983563856160859403, 1.37702099489081330271,
                                        2.1623699359449663589E-1, 1.34204006088543189037E-2, 3.28014464682127739104E-4,
                                        2.89247864745380683936E-6, 6.79019408009981274425E-9);
  begin
    case AType of
      0:
       Result := GetPolynomial(AArgument, AConstsA0) / GetPolynomial(AArgument, AConstsB0);
      1:
       Result := GetPolynomial(AArgument, AConstsA1) / GetPolynomial(AArgument, AConstsB1);
    else
       Result := GetPolynomial(AArgument, AConstsA2) / GetPolynomial(AArgument, AConstsB2);
    end;
    Result := AHelper0 + AHelper1 * AArgument * Result;
  end;

const
  AExpOfNegative2 = 0.13533528323661269189;
  ASqrtOf2Pi = 2.50662827463100050242;
var
  AAspect, AHelper0, AHelper1, Z: Extended;
  AType: Integer;
begin
  AAspect := -1;
  if X > (1 - AExpOfNegative2) then
  begin
    AAspect := 1;
    X := 1 - X;
  end;
  if X > AExpOfNegative2 then
  begin
    AAspect := ASqrtOf2Pi;
    X := X - 0.5;
    AHelper0 := X;
    AHelper1 := X;
    X := X * X;
    AType := 0;
  end
  else
  begin
    Z := Sqrt(-(2 * Ln(X)));
    X := 1 / Z;
    AHelper0 := Z - Ln(Z) / Z;
    AHelper1 := -1;
    AType := IfThen(Z < 8, 1, 2);
  end;
  Result := AAspect * GetApproximation(X, AHelper0, AHelper1, AType);
end;

function CanProcessingParameter(AProcessingInfo: TdxSpreadSheetEnumValuesProcessingInfo; ACellInfo: PdxSpreadSheetCellReference): Boolean;

  function CheckSubTotalIncluding(AToken: TdxSpreadSheetFormulaToken): Boolean;
  begin
    Result := False;
    while not Result and (AToken <> nil) do
    begin
      Result := (AToken is TdxSpreadSheetFormulaFunctionToken) and (TdxSpreadSheetFormulaFunctionToken(AToken).Information.Token = 344);
      if not Result and (AToken.ChildCount > 0) then
        Result := CheckSubTotalIncluding(AToken.FirstChild);
      AToken := AToken.Next;
    end;
  end;

  function IsCellFormulaIncludeSubTotalFunction(const AViewData: IdxSpreadSheetViewData): Boolean;
  var
    ACell: IdxSpreadSheetCellData;
    AFormula: TdxSpreadSheetCustomFormula;
  begin
    ACell := AViewData.GetCellData(ACellInfo^.RowIndex, ACellInfo^.ColumnIndex);
    if ACell <> nil then
      AFormula := TdxSpreadSheetCustomFormula(ACell.AsFormula)
    else
      AFormula := nil;

    if AFormula <> nil then
      Result := CheckSubTotalIncluding(AFormula.Tokens)
    else
      Result := False;
  end;

var
  AViewData: IdxSpreadSheetViewData;
begin
  Result := True;
  if (ACellInfo <> nil) and Supports(ACellInfo^.View, IdxSpreadSheetViewData, AViewData) then
  begin
    Result := not(
      (AProcessingInfo.IgnoreHiddenRows and not AViewData.IsRowVisible(ACellInfo.RowIndex)) or
      (AProcessingInfo.ForSubTotal and IsCellFormulaIncludeSubTotalFunction(AViewData)));
  end;
end;

function cbAccumulateResult(const AParameter: Variant; ACanConvertStrToNumber: Boolean;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode; AData: TdxSpreadSheetEnumValues; AType: TdxAccumulationType;
  AWithoutSimularNumericByReference: Boolean; AInfo: PdxSpreadSheetCellReference): Boolean;

  procedure InternalNonNumericProcessing;
  begin
    if ACanConvertStrToNumber and (AType <> atCount) then
      AData.SetErrorCode(ecValue)
    else
      if not AWithoutSimularNumericByReference and AData.ProcessingInfo.PopulateNumericListWhenAccumulateResult and
         not VarIsEmpty(AParameter) then
        AData.AddToNumericList(0, True);
  end;

var
  ACheckedValue: Variant;
  ANumeric: Double;
begin
  if AType = atCount then
    AErrorCode := ecNone;
  Result := AErrorCode = ecNone;
  if not CanProcessingParameter(AData.ProcessingInfo, AInfo) then
    Exit;
  if Result then
  begin
    if AData.CheckValueOnNumeric(AParameter, ACanConvertStrToNumber, AWithoutSimularNumericByReference, ACheckedValue) then
    begin
      if dxConvertXLSDateToNumeric(ACheckedValue, ANumeric) then
        ACheckedValue := ANumeric;
      if AData.ProcessingInfo.PopulateNumericListWhenAccumulateResult then
        AData.AddToNumericList(ACheckedValue, True);
      case AType of
        atSum, atCount:
          AData.ResultValue := AData.ResultValue + ACheckedValue;
        atSumSQ:
          AData.ResultValue := AData.ResultValue + ACheckedValue * ACheckedValue;
        atSumProduct:
          AData.ResultValue := AData.ResultValue * ACheckedValue;
      end
    end
    else
      InternalNonNumericProcessing;
  end
  else
    AData.SetErrorCode(AErrorCode);
end;

function cbCount(const AParameter: Variant; ACanConvertStrToNumber: Boolean; var AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AData: TdxSpreadSheetEnumValues; AInfo: PdxSpreadSheetCellReference = nil): Boolean;
begin
  Result := cbAccumulateResult(AParameter, ACanConvertStrToNumber, AErrorCode, AData, atCount, True, AInfo);
end;

function cbCountA(const AParameter: Variant; ACanConvertStrToNumber: Boolean; var AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AData: TdxSpreadSheetEnumValues; AInfo: PdxSpreadSheetCellReference = nil): Boolean;
begin
  Result := cbAccumulateResult(AParameter, ACanConvertStrToNumber, AErrorCode, AData, atCount, False, AInfo);
end;

function cbProduct(const AParameter: Variant; ACanConvertStrToNumber: Boolean; var AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AData: TdxSpreadSheetEnumValues; AInfo: PdxSpreadSheetCellReference = nil): Boolean;
begin
  Result := cbAccumulateResult(AParameter, ACanConvertStrToNumber, AErrorCode, AData, atSumProduct, True, AInfo);
end;

function cbSum(const AParameter: Variant; ACanConvertStrToNumber: Boolean; var AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AData: TdxSpreadSheetEnumValues; AInfo: PdxSpreadSheetCellReference = nil): Boolean;
begin
  Result := cbAccumulateResult(AParameter, ACanConvertStrToNumber, AErrorCode, AData, atSum, True, AInfo);
end;

function cbSumA(const AParameter: Variant; ACanConvertStrToNumber: Boolean; var AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AData: TdxSpreadSheetEnumValues; AInfo: PdxSpreadSheetCellReference = nil): Boolean;
begin
  Result := cbAccumulateResult(AParameter, ACanConvertStrToNumber, AErrorCode, AData, atSum, False, AInfo);
end;

function cbSumSQ(const AParameter: Variant; ACanConvertStrToNumber: Boolean; var AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AData: TdxSpreadSheetEnumValues; AInfo: PdxSpreadSheetCellReference = nil): Boolean;
begin
  Result := cbAccumulateResult(AParameter, ACanConvertStrToNumber, AErrorCode, AData, atSumSQ, True, AInfo);
end;

function dxCheckCondition(const ACheckedValue, AConditionValue: Variant; const AOperation: TdxSpreadSheetFormulaOperation): Boolean; inline;
var
  V: Variant;
  AIsEmptyChecked, AIsEmptyCondition: Boolean;
begin
  V := ACheckedValue;
  if VarIsStr(V) then
    V := dxSpreadSheetUpperCase(V);
  AIsEmptyChecked := VarIsEmpty(V) or VarIsNull(V);
  AIsEmptyCondition := VarIsEmpty(AConditionValue) or VarIsNull(AConditionValue);
  if AIsEmptyChecked or AIsEmptyCondition then
    Result := AIsEmptyChecked and AIsEmptyCondition and (AOperation in [opLE, opGE, opEQ]) or
      ((AOperation = opNE) and not(AIsEmptyChecked and AIsEmptyCondition))
  else
    case AOperation of
      opLT:
        Result := V < AConditionValue;
      opLE:
        Result := V <= AConditionValue;
      opGE:
        Result := V >= AConditionValue;
      opGT:
        Result := V > AConditionValue;
      opNE:
        Result := not dxIsIfConditionEqualValue(AConditionValue, V);
    else
      Result := dxIsIfConditionEqualValue(AConditionValue, V);
    end;
end;

function dxCheckAdditionalConditions(AData: TdxSpreadSheetEnumValuesWithCondition; ACellReference: TdxSpreadSheetCellReference): Boolean;
var
  ACondition: TdxSpreadSheetAdditionalCondition;
  AConditions: TList<TdxSpreadSheetAdditionalCondition>;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  ARange: TdxSpreadSheetFormulaReference;
  AValue: Variant;
  dRow, dColumn: Integer;
  I: Integer;
begin
  Result := True;
  AConditions := AData.AdditionalConditions;
  if AConditions.Count > 0 then
  begin
    dRow := ACellReference.RowIndex - TdxSpreadSheetFormulaReference(AData.ConditionRange).ActualRow;
    dColumn := ACellReference.ColumnIndex - TdxSpreadSheetFormulaReference(AData.ConditionRange).ActualColumn;
    for I := 0 to AConditions.Count - 1 do
    begin
      ACondition := AConditions[I];
      ARange := TdxSpreadSheetFormulaReference(ACondition.Range);
      TdxTokenAccess(ARange).GetCellValueAsOrdinal(ARange.View, ARange.ActualRow + dRow, ARange.ActualColumn + dColumn, AValue, AErrorCode);
      Result := dxCheckCondition(AValue, ACondition.Value, ACondition.Operation);
      if not Result then
        Break;
    end;
  end;
end;

function cbSumIF(const AValue: Variant; ACanConvertStrToNumber: Boolean;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode; AData, AInfo: Pointer): Boolean;

  function CanBeAdded(const ACheckedValue: Variant; out AOutputValue: Variant): Boolean;
  begin
    Result := False;
    AOutputValue := ACheckedValue;
    if dxSpreadSheetIsNullValue(ACheckedValue) or VarIsStr(ACheckedValue) then
      Exit;

    Result := dxIsNumberOrDateTime(ACheckedValue);
    if Result and VarIsDate(ACheckedValue) then
      AOutputValue := VarToDateTime(ACheckedValue);
  end;

  function GetActualRow(AReference: TdxSpreadSheetFormulaReference): Integer;
  begin
    Result := AReference.ActualRow;
    if TdxReferenceAccess(AReference).FRow.IsAllItems then
      Result := 0;
  end;

  function GetActualColumn(AReference: TdxSpreadSheetFormulaReference): Integer;
  begin
    Result := AReference.ActualColumn;
    if TdxReferenceAccess(AReference).FColumn.IsAllItems then
      Result := 0;
  end;

var
  ACellReference: TdxSpreadSheetCellReference;
  AConditionRange: TdxSpreadSheetFormulaReference;
  AEnumData: TdxSpreadSheetEnumValuesWithCondition;
  AResultRange: TdxSpreadSheetFormulaReference;
  AResultValue: Variant;
  V1: Variant;
begin
  Result := True;
  AErrorCode := ecNone;
  AEnumData := TdxSpreadSheetEnumValuesWithCondition(AData);
  AConditionRange := TdxSpreadSheetFormulaReference(AEnumData.ConditionRange);
  AResultRange := TdxSpreadSheetFormulaReference(AEnumData.ResultRange);
  ACellReference := PdxSpreadSheetCellReference(AInfo)^;
  try
    if dxCheckCondition(AValue, AEnumData.ConditionValue, AEnumData.Operation) and dxCheckAdditionalConditions(AEnumData, ACellReference) then
    begin
      if (AConditionRange = AResultRange) then
        AResultValue := AValue
      else
      begin
        AResultRange.GetCellValue(AResultRange.View,
          GetActualRow(AResultRange) + ACellReference.RowIndex - GetActualRow(AConditionRange),
          GetActualColumn(AResultRange) + ACellReference.ColumnIndex - GetActualColumn(AConditionRange),
          AResultValue, AErrorCode);
      end;
      if (AErrorCode = ecNone) and CanBeAdded(AResultValue, V1) then
      begin
        AEnumData.ResultValue := AEnumData.ResultValue + V1;
        Inc(AEnumData.CountValues);
      end;
    end;
  except
    on EVariantError do;
  end;
end;

function dxGetGammaLn(X: Extended): Extended;
const
  ACoefCount = 20;
  AStirlingCoef: array[0..ACoefCount - 1] of Extended =
   (1/12, -1/360, 1/1260, -1/1680, 1/1188, -691/360360, 1/156, -3617/122400, 43867/244188, -174611/125400,
    77683/5796, -236364091/1506960, 657931/300, -3392780147/93960, 1723168255201/2492028, -7709321041217/505920,
    151628697551/396, -26315271553053477373.0/2418179400, 154210205991661/444, -261082718496449122051.0/21106800);
  ALnSqrtOf2Pi = 0.9189385332046727417803297364;
var
  AApprox, ASum, APreSum, ADenominator, X2: Extended;
  I: Integer;
begin
  AApprox := 0;
  while X < 7 do
  begin
    AApprox := AApprox + Ln(X);
    X := X + 1;
  end;
  ADenominator := X;
  X2 := X * X;
  APreSum := (X - 0.5) * Ln(X) - X + ALnSqrtOf2Pi;
  for I := 0 to ACoefCount - 1 do
  begin
    ASum := APreSum + AStirlingCoef[I] / ADenominator;
    if IsZero(ASum - APreSum, 1E-15) then
      Break;
    ADenominator := ADenominator * X2;
    APreSum := ASum;
  end;
  Result := ASum - AApprox;
end;

{ BETA.DIST }

function dxGetLnBeta(AAlpha, ABeta: Extended): Extended;
begin
  Result := dxGetGammaLn(AAlpha + ABeta) - dxGetGammaLn(AAlpha) - dxGetGammaLn(ABeta);
end;

function dxGetBeta(AAlpha, ABeta: Extended): Extended;
begin
  Result := Exp(dxGetLnBeta(AAlpha, ABeta ));
end;

function dxGetPSeries(X, AAlpha, ABeta: Extended): Extended;
var
  N: Integer;
  ACurrent: Extended;
begin
  Result := 0;
  N := 2;
  ACurrent := (1 - ABeta) * X / (AAlpha + 1);
  while Abs(ACurrent) > dxBetaDistMachineEpsilon / AAlpha  do
  begin
    ACurrent := ACurrent * (AAlpha + N - 1) * (N - ABeta) * X / (N * (AAlpha + N));
    Result := Result + ACurrent;
    Inc(N);
  end;
  Result := Result + (1 - ABeta) * X / (AAlpha + 1) + 1 / AAlpha;
  if ((AAlpha + ABeta) < dxBetaDistMaxGamma) and (Abs(AAlpha * Ln(X)) < dxBetaDistMaxLog) then
    Result := Result * dxGetBeta(AAlpha, ABeta) * Power(X, AAlpha)
  else
  begin
    ACurrent := dxGetLnBeta(AAlpha, ABeta) + AAlpha * Ln(X) + Ln(Result);
    Result := IfThen(ACurrent < dxBetaDistMinLog, 0, Exp(ACurrent));
  end;
end;

function dxGetFractionExpansion(X, AAlpha, ABeta: Extended; AFlag: Boolean): Extended;
const
  ABig = 4.503599627370496e15;
  ABigInv = 2.22044604925031308085e-16;
var
  k1, k2, ACurrent, approxPk, approxQk, APrecision,
  approxCoefPk1, approxCoefPk2, approxCoefQk1, approxCoefQk2: Extended;
  N: Integer;
begin
  approxCoefPk1 := 1;
  approxCoefPk2 := 0;
  approxCoefQk1 := 1;
  approxCoefQk2 := 1;
  Result := 1;
  if AFlag then
  begin
    k1 := AAlpha + ABeta;
    k2 := ABeta - 1;
  end
  else
  begin
    k1 := ABeta - 1;
    k2 := AAlpha + ABeta;
  end;
  N := 0;
  while N < 300 do
  begin
    if AFlag then
      ACurrent := -(X * (AAlpha + N) * (k1 + N)) / ((AAlpha + 2 * N) * (AAlpha + 1 + 2 * N))
    else
      ACurrent := -(X / (1 - X) * (AAlpha + N) * (k1 - N)) / ((AAlpha + 2 * N) * (AAlpha + 1 + 2 * N));
    approxPk := approxCoefPk1 + approxCoefPk2 * ACurrent;
    approxQk := approxCoefQk1 + approxCoefQk2 * ACurrent;
    approxCoefPk2 := approxCoefPk1;
    approxCoefPk1 := approxPk;
    approxCoefQk2 := approxCoefQk1;
    approxCoefQk1 := approxQk;

    if AFlag then
      ACurrent := (X * (1 + N) * (k2 - N)) / ((AAlpha + 1 + 2 * N) * (AAlpha + 2 + 2 * N))
    else
      ACurrent := (X / (1 - X) * (1 + N) * (k2 + N)) / ((AAlpha + 1 + 2 * N) * (AAlpha + 2 + 2 * N));
    approxPk := approxCoefPk1 + approxCoefPk2 * ACurrent;
    approxQk := approxCoefQk1 + approxCoefQk2 * ACurrent;
    approxCoefPk2 := approxCoefPk1;
    approxCoefPk1 := approxPk;
    approxCoefQk2 := approxCoefQk1;
    approxCoefQk1 := approxQk;

    if (Abs(approxQk) > dxBetaDistMachineEpsilon) and (Abs(approxPk / approxQk) > dxBetaDistMachineEpsilon) then
    begin
      Result := approxPk / approxQk;
      APrecision := Abs(1 / Result - 1);
    end
    else
      APrecision := 1;
    if APrecision < 3.0 * dxBetaDistMachineEpsilon then
      Break;
    if (Abs(approxQk) + Abs(approxPk)) > ABig then
    begin
      approxCoefPk2 := approxCoefPk2 * ABigInv;
      approxCoefPk1 := approxCoefPk1 * ABigInv;
      approxCoefQk2 := approxCoefQk2 * ABigInv;
      approxCoefQk1 := approxCoefQk1 * ABigInv;
    end;
    if (Abs(approxQk) < ABigInv) or (Abs(approxPk) < ABigInv) then
    begin
      approxCoefPk2 := approxCoefPk2 * ABig;
      approxCoefPk1 := approxCoefPk1 * ABig;
      approxCoefQk2 := approxCoefQk2 * ABig;
      approxCoefQk1 := approxCoefQk1 * ABig;
    end;
    Inc(N);
  end;
end;

function dxGetBetaCore(XBeg, XEnd, AAlpha, ABeta: Extended; AFlag: Boolean): Extended;

  function GetFinalResult(APreResult: Extended): Extended;
  begin
    Result := APreResult;
    if AFlag and (APreResult <= dxBetaDistMachineEpsilon) then
      Result := 1 - dxBetaDistMachineEpsilon
    else
      if AFlag and (APreResult > dxBetaDistMachineEpsilon) then
        Result := 1 - APreResult
  end;

var
  AFraction: Extended;
begin
  if AFlag and ((ABeta * XEnd) <= 1) and (XEnd <= 0.95) then
    Result := GetFinalResult(dxGetPSeries(XEnd, AAlpha, ABeta))
  else
  begin
    Result := XEnd * (AAlpha + ABeta - 2) - (AAlpha - 1);
    AFraction := IfThen(Result < 0, dxGetFractionExpansion(XEnd, AAlpha, ABeta, True),
      dxGetFractionExpansion(XEnd, AAlpha, ABeta, False) / XBeg);
    Result := AAlpha * Ln(XEnd);
    if ((AAlpha + ABeta) < dxBetaDistMaxGamma) and (Abs(Result) < dxBetaDistMaxLog) and (Abs(ABeta * Ln(XBeg)) < dxBetaDistMaxLog) then
      Result := GetFinalResult(Power(XBeg, ABeta) * Power(XEnd, AAlpha) * AFraction * dxGetBeta(AAlpha, ABeta) / AAlpha)
    else
    begin
      Result := Result + ABeta * Ln(XBeg) + dxGetLnBeta(AAlpha, ABeta) + Ln(AFraction / AAlpha);
      if Result < dxBetaDistMinLog then
        Result := GetFinalResult(0)
      else
        Result := GetFinalResult(Exp(Result));
    end;
  end;
end;

function dxGetBetaDistCumulative(X, AAlpha, ABeta: Extended): Extended;
begin
  if (X * ABeta <= 1) and (X <= 0.95) then
    Result := dxGetPSeries(X, AAlpha, ABeta)
  else
    if X > (AAlpha / (AAlpha + ABeta)) then
      Result := dxGetBetaCore(X, 1 - X, ABeta, AAlpha, True)
    else
      Result := dxGetBetaCore(1 - X, X, AAlpha, ABeta, False);
end;

function dxGetBetaDist(X, AAlpha, ABeta, A, B: Extended; ACumulative: Boolean): Extended;
begin
  if ACumulative then
    if X = A then
      Result := 0
    else
      if X = B then
        Result := 1
      else
       Result := dxGetBetaDistCumulative((X - A) / (B - A), AAlpha, ABeta)
  else
    if (X = A) or (X = B) then
      Result := 0
    else
      Result := Exp(dxGetLnBeta(AAlpha, ABeta) + (AAlpha - 1) * Ln(X - A) + (ABeta - 1) * Ln(B - X) - (AAlpha + ABeta - 1) * Ln(B - A));
end;

{ BETA.INV }

type
  TdxBetaInvSolveState = (bissMain, bissIntervalHalvingInit, bissIntervalHalvingCycle, bissIntervalHalvingCycleBreak,
    bissNewtonIterationsInit, bissNewtonIterationsCycle, bissNewtonIterationsCycleBreak, bissReady);

  TdxBetaInvSolver = class
  private
    BetaDist: Extended;
    CurrentAlpha: Extended;
    CurrentBeta: Extended;
    GammaLn: Extended;
    PreResult: Extended;

    BeginY: Extended;
    CurrentY: Extended;
    EndY: Extended;

    BeginX: Extended;
    CurrentX: Extended;
    EndX: Extended;

    Dichotomy: Extended;
    DichotomyThreshhold: Extended;

    NewtonFlag: Boolean;
    ResultFlag: Boolean;

    DichotomyRegularity: Integer;
    IterationCount: Integer;

    State: TdxBetaInvSolveState;

    Alpha: Extended;
    Beta: Extended;
    Probability: Extended;

    procedure ChangeCurrentState;
    procedure DoChangeMain;
    procedure DoChangeIntervalHalvingInit;
    procedure DoChangeIntervalHalvingCycle;
    procedure DoChangeIntervalHalvingCycleBreak;
    procedure DoChangeNewtonIterationsInit;
    procedure DoChangeNewtonIterationsCycle;
    procedure DoChangeNewtonIterationsCycleBreak;
    procedure Initialize(AProbability, AAlpha, ABeta: Extended);
    procedure InitializeDichotomy;
    procedure SetCurrentAlphaBeta(AAlpha, ABeta: Extended);
  public
    function Solve(AProbability, AAlpha, ABeta: Extended): Extended;
  end;

procedure TdxBetaInvSolver.ChangeCurrentState;
begin
  case State of
    bissMain: DoChangeMain;
    bissIntervalHalvingInit: DoChangeIntervalHalvingInit;
    bissIntervalHalvingCycle: DoChangeIntervalHalvingCycle;
    bissIntervalHalvingCycleBreak: DoChangeIntervalHalvingCycleBreak;
    bissNewtonIterationsInit: DoChangeNewtonIterationsInit;
    bissNewtonIterationsCycle: DoChangeNewtonIterationsCycle;
    bissNewtonIterationsCycleBreak: DoChangeNewtonIterationsCycleBreak;
  end;
end;

procedure TdxBetaInvSolver.DoChangeMain;
var
  ANormsInv, ADelta: Extended;
begin
  if (Alpha <= 1) or (Beta <= 1) then
  begin
    DichotomyThreshhold := 1E-6;
    ResultFlag := False;
    SetCurrentAlphaBeta(Alpha, Beta);
    BeginY := Probability;
    PreResult := CurrentAlpha / (CurrentAlpha + CurrentBeta);
    BetaDist := dxGetBetaDistCumulative(PreResult, CurrentAlpha, CurrentBeta);
    State := bissIntervalHalvingInit;
  end
  else
  begin
    DichotomyThreshhold := 1E-4;
    ANormsInv := -dxGetStandartNormInv(Probability);
    if Probability > 0.5 then
    begin
      ResultFlag := True;
      SetCurrentAlphaBeta(Beta, Alpha);
      BeginY := 1 - Probability;
      ANormsInv := -ANormsInv;
    end
    else
    begin
      ResultFlag := False;
      SetCurrentAlphaBeta(Alpha, Beta);
      BeginY := Probability;
    end;
    GammaLn := (ANormsInv * ANormsInv - 3) / 6;
    PreResult := 2 / (1 / (2 * CurrentAlpha - 1) + 1 / (2 * CurrentBeta - 1));
    ADelta := 2 * ANormSInv * Sqrt(PreResult + GammaLn) / PreResult -
      (1 / (2 * CurrentBeta - 1) - 1 / (2 * CurrentAlpha - 1)) * (GammaLn + 5 / 6 - 2 / (3 * PreResult));
    if ADelta < dxBetaDistMinLog then
    begin
      PreResult := 0;
      State := bissReady;
    end
    else
    begin
      PreResult := CurrentAlpha / (CurrentAlpha + CurrentBeta * Exp(ADelta));
      BetaDist := dxGetBetaDistCumulative(PreResult, CurrentAlpha, CurrentBeta);
      if Abs((BetaDist - BeginY) / BeginY) < 0.2 then
        State := bissNewtonIterationsInit
      else
        State := bissIntervalHalvingInit;
    end;
  end;

end;

procedure TdxBetaInvSolver.DoChangeIntervalHalvingInit;
begin
  InitializeDichotomy;
  IterationCount := 0;
  State := bissIntervalHalvingCycle;
end;

procedure TdxBetaInvSolver.DoChangeIntervalHalvingCycle;
begin
  if IterationCount <= 99 then
  begin
    if IterationCount > 0 then
    begin
      PreResult := BeginX + Dichotomy * (EndX - BeginX);
      if IsZero(PreResult - 1, dxBetaDistMachineEpsilon) then
        PreResult := 1 - dxBetaDistMachineEpsilon;
      if IsZero(PreResult, dxBetaDistMachineEpsilon) then
      begin
        Dichotomy := 0.5;
        PreResult := BeginX + Dichotomy * (EndX - BeginX);
        if IsZero(PreResult, dxBetaDistMachineEpsilon) then
        begin
          State := bissReady;
          Exit;
        end;
      end;
      BetaDist := dxGetBetaDistCumulative(PreResult, CurrentAlpha, CurrentBeta);
      if (Abs((EndX - BeginX) / (EndX + BeginX)) < DichotomyThreshhold) or (Abs((BetaDist - BeginY) / BeginY) < DichotomyThreshhold) then
      begin
        State := bissNewtonIterationsInit;
        Exit;
      end;
    end;

    if BetaDist < BeginY then
    begin
      BeginX := PreResult;
      EndY := BetaDist;
      if DichotomyRegularity < 0 then
        InitializeDichotomy
      else
        if DichotomyRegularity > 3 then
          Dichotomy := 1 - (1 - Dichotomy) * (1 - Dichotomy)
         else
           if DichotomyRegularity > 1 then
             Dichotomy := 0.5 * Dichotomy + 0.5
            else
             Dichotomy := (BeginY - BetaDist) / (CurrentY - EndY);
      Inc(DichotomyRegularity);
      if beginX > 0.75 then
      begin
        if ResultFlag then
        begin
          SetCurrentAlphaBeta(Alpha, Beta);
          BeginY := Probability;
        end
        else
        begin
          SetCurrentAlphaBeta(Beta, Alpha);
          BeginY := 1 - Probability;
        end;
        ResultFlag := not ResultFlag;
        PreResult := 1 - PreResult;
        BetaDist := dxGetBetaDistCumulative(PreResult, CurrentAlpha, CurrentBeta);
        BeginX := 0;
        EndX := 1;
        EndY := 0;
        CurrentY := 1;
        State := bissIntervalHalvingInit;
        Exit;
      end;
    end
    else
    begin
      EndX := PreResult;
      if ResultFlag and (EndX < dxBetaDistMachineEpsilon) then
      begin
        PreResult := 0;
        State := bissReady;
        Exit;
      end;
      CurrentY := BetaDist;
      if DichotomyRegularity > 0 then
        InitializeDichotomy
      else
        if DichotomyRegularity < -3 then
          Dichotomy := Dichotomy * Dichotomy
        else
          Dichotomy := IfThen(DichotomyRegularity < -1, 0.5 * Dichotomy, (BetaDist - BeginY) / (CurrentY - EndY));
      Dec(DichotomyRegularity);
    end;
    Inc(IterationCount);
    State := bissIntervalHalvingCycle;
  end
  else
    State := bissIntervalHalvingCycleBreak;
 end;

procedure TdxBetaInvSolver.DoChangeIntervalHalvingCycleBreak;
begin
  if BeginX >= 1 then
  begin
    PreResult := 1 - dxBetaDistMachineEpsilon;
    State := bissReady;
  end
  else
    if PreResult <= dxBetaDistMachineEpsilon then
    begin
      PreResult := 0;
      State := bissReady;
    end
    else
      State := bissNewtonIterationsInit;
end;

procedure TdxBetaInvSolver.DoChangeNewtonIterationsInit;
begin
  if NewtonFlag then
    State := bissReady
  else
  begin
    NewtonFlag := True;
    GammaLn := dxGetLnBeta(CurrentAlpha, CurrentBeta);
    IterationCount := 0;
    State := bissNewtonIterationsCycle;
  end;
end;

procedure TdxBetaInvSolver.DoChangeNewtonIterationsCycle;
var
  ADelta: Extended;
begin
  if IterationCount <= 7 then
  begin
    if IterationCount > 0 then
      BetaDist := dxGetBetaDistCumulative(PreResult, CurrentAlpha, CurrentBeta);
    if BetaDist < EndY then
    begin
      PreResult := BeginX;
      BetaDist := EndY;
    end
    else
    begin
      if BetaDist > CurrentY then
      begin
        PreResult := EndX;
        BetaDist := CurrentY;
      end
      else
      begin
        if BetaDist < BeginY then
        begin
          BeginX := PreResult;
          EndY := BetaDist;
        end
        else
        begin
          EndX := PreResult;
          CurrentY := BetaDist;
        end;
      end;
    end;

    if IsZero(PreResult - 1, dxBetaDistMachineEpsilon) or IsZero(PreResult, dxBetaDistMachineEpsilon) then
    begin
      State := bissNewtonIterationsCycleBreak;
      Exit;
    end;

    ADelta := (CurrentAlpha - 1) * Ln(PreResult) + (CurrentBeta - 1) * Ln(1 - PreResult) + GammaLn;
    if ADelta < dxBetaDistMinLog then
    begin
      State := bissReady;
      Exit;
    end;
    if ADelta > dxBetaDistMaxLog then
    begin
      State := bissNewtonIterationsCycleBreak;
      Exit;
    end;

    ADelta := (BetaDist - BeginY) / Exp(ADelta);
    CurrentX := PreResult - ADelta;
    if (CurrentX <= BeginX) then
    begin
      BetaDist := (PreResult - BeginX) / (EndX - BeginX);
      CurrentX := BeginX + 0.5 * BetaDist * (PreResult - BeginX);
      if CurrentX <= dxBetaDistMachineEpsilon then
      begin
        State := bissNewtonIterationsCycleBreak;
        Exit;
      end;
    end;
    if (CurrentX >= EndX) then
    begin
      BetaDist := (EndX - PreResult) / (EndX - BeginX);
      CurrentX := EndX - 0.5 * BetaDist * (EndX - PreResult);
      if CurrentX >= 1 then
      begin
        State := bissNewtonIterationsCycleBreak;
        Exit;
      end;
    end;

    PreResult := CurrentX;
    if Abs(ADelta / PreResult) < 128 * dxBetaDistMachineEpsilon then
      State := bissReady
    else
    begin
      Inc(IterationCount);
      State := bissNewtonIterationsCycle;
    end;
  end
  else
    State := bissNewtonIterationsCycleBreak;
end;

procedure TdxBetaInvSolver.DoChangeNewtonIterationsCycleBreak;
begin
  DichotomyThreshhold := 256 * dxBetaDistMachineEpsilon;
  State := bissIntervalHalvingInit;
end;

procedure TdxBetaInvSolver.Initialize(AProbability, AAlpha, ABeta: Extended);
begin
  EndX := 1;
  CurrentY := 1;
  State := bissMain;
  Alpha := AAlpha;
  Beta := ABeta;
  Probability := AProbability;
end;

procedure TdxBetaInvSolver.InitializeDichotomy;
begin
  DichotomyRegularity := 0;
  Dichotomy := 0.5;
end;

procedure TdxBetaInvSolver.SetCurrentAlphaBeta(AAlpha, ABeta: Extended);
begin
  CurrentAlpha := AAlpha;
  CurrentBeta := ABeta;
end;

function TdxBetaInvSolver.Solve(AProbability, AAlpha, ABeta: Extended): Extended;
begin
  Initialize(AProbability, AAlpha, ABeta);
  while State <> bissReady do
    ChangeCurrentState;
  Result := PreResult;
  if ResultFlag then
    Result := IfThen(Result <= dxBetaDistMachineEpsilon, 1 - dxBetaDistMachineEpsilon, 1 - Result);
end;

function dxGetBetaInv(AProbability, AAlpha, ABeta, A, B: Extended): Extended;
var
  ASolver: TdxBetaInvSolver;
begin
  ASolver := TdxBetaInvSolver.Create;
  try
    Result := (B - A) * ASolver.Solve(AProbability, AAlpha, ABeta) + A;
  finally
    ASolver.Free;
  end;
end;

{ GAMMA.DIST }

function dxGetLowerIncompleteGamma(X, AAlpha: Extended): Extended;
var
  ADenominator, ACurrent: Extended;
begin
  ADenominator := AAlpha;
  ACurrent := 1;
  Result := 1;
  while (ACurrent / Result) > dxIncompleteGammaEpsilon do
  begin
    ADenominator := ADenominator + 1;
    ACurrent := ACurrent * X / ADenominator;
    Result := Result + ACurrent;
  end;
  Result := Result / AAlpha;
end;

function dxGetUpperIncompleteGamma(X, AAlpha: Extended): Extended;
var
  ACurrentY, ACurrentZ, ACurrent, APrecision,
  approxCoefPk1, approxCoefPk2, approxCoefQk1, approxCoefQk2, approxPk, approxQk: Extended;
  ACoef: Integer;
begin
  ACurrentY := 1 - AAlpha;
  ACurrentZ := X + ACurrentY + 1;
  ACoef := 0;
  approxCoefPk1 := X + 1;
  approxCoefPk2 := 1;
  approxCoefQk1 := ACurrentZ * X;
  approxCoefQk2 := X;
  Result := approxCoefPk1 / approxCoefQk1;
  APrecision := 1;
  while APrecision > dxIncompleteGammaEpsilon do
  begin
    Inc(ACoef);
    ACurrentY := ACurrentY + 1;
    ACurrentZ := ACurrentZ + 2;
    ACurrent := ACurrentY * ACoef;
    approxPk := approxCoefPk1 * ACurrentZ - approxCoefPk2 * ACurrent;
    approxQk := approxCoefQk1 * ACurrentZ - approxCoefQk2 * ACurrent;
    if Abs(approxQk) > dxIncompleteGammaEpsilon then
    begin
      APrecision := Abs((approxQk * Result - approxPk) / approxPk);
      Result := approxPk / approxQk;
    end;
    approxCoefPk2 := approxCoefPk1;
    approxCoefPk1 := approxPk;
    approxCoefQk2 := approxCoefQk1;
    approxCoefQk1 := approxQk;
    if Abs(approxPk) > dxIncompleteGammaBigNumber then
    begin
      approxCoefPk2 := approxCoefPk2 * dxIncompleteGammaBigNumberInv;
      approxCoefPk1 := approxCoefPk1 * dxIncompleteGammaBigNumberInv;
      approxCoefQk2 := approxCoefQk2 * dxIncompleteGammaBigNumberInv;
      approxCoefQk1 := approxCoefQk1 * dxIncompleteGammaBigNumberInv;
    end;
  end;
end;

function dxGetGammaDistCumulative(X, AAlpha: Extended): Extended;
var
  AValue: Extended;
begin
  AValue := Exp(AAlpha * Ln(X) - X - dxGetGammaLn(AAlpha));
  if (X > 1) and (X > AAlpha) then
    Result := 1 - AValue * dxGetUpperIncompleteGamma(X, AAlpha)
  else
    Result := AValue * dxGetLowerIncompleteGamma(X, AAlpha);
end;

function dxGetGammaDist(X, AAlpha, ABeta: Extended; ACumulative: Boolean): Extended;
begin
  if X = 0 then
    Result := 0
  else
    if ACumulative then
      Result := dxGetGammaDistCumulative(X / ABeta, AAlpha)
    else
      Result := Exp(-dxGetGammaLn(AAlpha) - AAlpha * Ln(ABeta) + (AAlpha - 1) * Ln(X) - X / ABeta);
end;

{ GAMMA.INV }

type
  TdxGammaInvSolveState =
    (gissNewtonIterationsFirstStage, gissBigNumberCheckStage, gissNewtonIterationsSecondStage, gissReady);

  TdxGammaInvSolver = class
  private
    Alpha: Extended;
    Probability: Extended;

    BeginX: Extended;
    BeginY: Extended;
    Dichotomy: Extended;
    DichotomyRegularity: Integer;
    EndX: Extended;
    EndY: Extended;
    IncompleteGamma: Extended;
    IterationCount: Integer;
    PreResult: Extended;
    NormSInv: Extended;
    State: TdxGammaInvSolveState;
    Threshhold: Extended;

    procedure ChangeCurrentState;
    procedure DoChangeNewtonIterationsFirstStage;
    procedure DoChangeBigNumberCheckStage();
    procedure DoChangeNewtonIterationsSecondStage;
    function GetUpperIncompleteGamma(X, AAlpha: Extended): Extended;
    procedure Initialize(AProbability, AAlpha: Extended);
    procedure InitializeDichotomy;
  public
    function Solve(AProbability, AAlpha: Extended): Extended;
  end;

procedure TdxGammaInvSolver.ChangeCurrentState;
begin
  case State of
    gissNewtonIterationsFirstStage: DoChangeNewtonIterationsFirstStage;
    gissBigNumberCheckStage: DoChangeBigNumberCheckStage;
    gissNewtonIterationsSecondStage: DoChangeNewtonIterationsSecondStage;
  end;
end;

procedure TdxGammaInvSolver.DoChangeNewtonIterationsFirstStage;
begin
  while IterationCount < 10 do
  begin
    if (PreResult > EndX) or (PreResult < BeginX) then
    begin
      Dichotomy := 0.0625;
      Break;
    end;

   IncompleteGamma := GetUpperIncompleteGamma(PreResult, Alpha);
   if (IncompleteGamma < BeginY) or (IncompleteGamma > EndY) then
   begin
     Dichotomy := 0.0625;
     Break;
   end;

   if IncompleteGamma < Probability then
   begin
     EndX := PreResult;
     BeginY := IncompleteGamma;
   end
   else
   begin
     BeginX := PreResult;
     EndY := IncompleteGamma;
   end;

   Dichotomy := (Alpha - 1) * Ln(PreResult) - PreResult - dxGetGammaLn(Alpha);
   if Dichotomy < -709.78271289338399 then
   begin
     Dichotomy := 0.0625;
     Break;
   end;

   Dichotomy := - (IncompleteGamma - Probability) / Exp(Dichotomy);
   if Abs(Dichotomy / PreResult) < dxIncompleteGammaEpsilon then
   begin
     State := gissReady;
     Exit;
   end;
   PreResult := PreResult - Dichotomy;
   Inc(IterationCount);
  end;
  State := gissBigNumberCheckStage;
end;

procedure TdxGammaInvSolver.DoChangeBigNumberCheckStage;
begin
  if IsZero(EndX - dxIncompleteGammaBigNumber, dxIncompleteGammaEpsilon) then
  begin
    if PreResult <= dxIncompleteGammaEpsilon then
      PreResult := 1;
    while IsZero(EndX - dxIncompleteGammaBigNumber, dxIncompleteGammaEpsilon) do
    begin
      PreResult := (1 + Dichotomy) * PreResult;
      IncompleteGamma := GetUpperIncompleteGamma(PreResult, Alpha);
      if IncompleteGamma < Probability then
      begin
        EndX := PreResult;
        BeginY := IncompleteGamma;
        Break;
      end;
      Dichotomy := Dichotomy * 2;
    end;
  end;
  State := gissNewtonIterationsSecondStage;
end;

procedure TdxGammaInvSolver.DoChangeNewtonIterationsSecondStage;
begin
  InitializeDichotomy;
  IterationCount := 0;
  while IterationCount < 400 do
  begin
    PreResult := BeginX + Dichotomy * (EndX - BeginX);
    IncompleteGamma := GetUpperIncompleteGamma(PreResult, Alpha);
    if (Abs((EndX - BeginX) / (BeginX + EndX)) < Threshhold) or
      (Abs((IncompleteGamma - Probability) / Probability) < Threshhold) or (PreResult <= dxIncompleteGammaEpsilon) then
      Break;
    if IncompleteGamma >= Probability then
    begin
      BeginX := PreResult;
      EndY := IncompleteGamma;
      if DichotomyRegularity < 0 then
        InitializeDichotomy
      else
        Dichotomy := IfThen(DichotomyRegularity > 1, 0.5 * Dichotomy + 0.5, (Probability - BeginY) / (EndY - BeginY));
      Inc(DichotomyRegularity);
    end
    else
    begin
      EndX := PreResult;
      BeginY := IncompleteGamma;
      if DichotomyRegularity > 0 then
        InitializeDichotomy
      else
        Dichotomy := IfThen(DichotomyRegularity < -1, 0.5 * Dichotomy, (Probability - BeginY) / (EndY - BeginY));
      Dec(DichotomyRegularity);
    end;
     Inc(IterationCount);
  end;
  State := gissReady;
end;

function TdxGammaInvSolver.GetUpperIncompleteGamma(X, AAlpha: Extended): Extended;
var
  AValue: Extended;
begin
  AValue := Exp(AAlpha * Ln(X) - X - dxGetGammaLn(AAlpha));
  if (X < 1) or (X < AAlpha) then
    Result := 1 - AValue * dxGetLowerIncompleteGamma(X, AAlpha)
  else
    Result := AValue * dxGetUpperIncompleteGamma(X, AAlpha);
end;

procedure TdxGammaInvSolver.Initialize(AProbability, AAlpha: Extended);
begin
  Threshhold := 5 * dxIncompleteGammaEpsilon;
  Alpha := AAlpha;
  Probability := AProbability;
  BeginX := 0;
  EndX := dxIncompleteGammaBigNumber;
  BeginY := 0;
  EndY := 1;
  Dichotomy := 1 / (9 * alpha);
  NormSInv := 1 - Dichotomy - dxGetStandartNormInv(Probability) * Sqrt(Dichotomy);
  PreResult := Alpha * NormSInv * NormSInv * NormSInv;
  IterationCount := 0;
  State := gissNewtonIterationsFirstStage;
end;

procedure TdxGammaInvSolver.InitializeDichotomy;
begin
  DichotomyRegularity := 0;
  Dichotomy := 0.5;
end;

function TdxGammaInvSolver.Solve(AProbability, AAlpha: Extended): Extended;
begin
  Initialize(AProbability, AAlpha);
  while State <> gissReady do
    ChangeCurrentState;
  Result := PreResult;
end;

function dxGetGammaInv(AProbability, AAlpha, ABeta: Extended): Extended;
var
  ASolver: TdxGammaInvSolver;
begin
  Result := 0;
  if AProbability = 0 then
    Exit;
  ASolver := TdxGammaInvSolver.Create;
  try
    Result := ABeta * ASolver.Solve(1 - AProbability, AAlpha);
  finally
    ASolver.Free;
  end;
end;

{ T.DIST }

function dxGetTDistCumulative(X: Extended; ADegFreedom: Integer): Extended;

  function GetDefiniteIntegral(ALowerLimit: Extended; ADegreeFreedom, ABeginDegreeFreedom: Integer): Extended;
  var
    ACurrent: Extended;
    ACurrentDegreeFreedom: Integer;
  begin
    Result := 1;
    ACurrent := Result;
    ACurrentDegreeFreedom := ABeginDegreeFreedom;
    while (ACurrentDegreeFreedom <= ADegreeFreedom - 2) and ((ACurrent / Result) > dxBetaDistMachineEpsilon) do
    begin
      ACurrent := ACurrent * (ACurrentDegreeFreedom - 1) / (ALowerLimit * ACurrentDegreeFreedom);
      Result := Result + ACurrent;
      Inc(ACurrentDegreeFreedom, 2);
    end;
  end;

var
  ALowerLimit, AValue: Extended;
begin
  Result := 0.5;
  if X = 0 then
      Exit;
  if X < -2 then
    Result := 0.5 * dxGetBetaDistCumulative(ADegFreedom / (ADegFreedom + X * X), 0.5 * ADegFreedom, 0.5)
  else
  begin
    ALowerLimit := 1 + X * X / ADegFreedom;
    if Odd(ADegFreedom) then
    begin
      AValue := Abs(X) / Sqrt(ADegFreedom);
      Result := ArcTan(AValue);
      if ADegFreedom > 1 then
        Result := Result + GetDefiniteIntegral(ALowerLimit, ADegFreedom, 3) * AValue / ALowerLimit;
      Result := Result * 2 / Pi;
    end
    else
      Result := GetDefiniteIntegral(ALowerLimit, ADegFreedom, 2) * Abs(X) / Sqrt(ALowerLimit * ADegFreedom);
    if X < 0 then
      Result := -Result;
    Result := 0.5 + 0.5 * Result;
  end;
end;

function dxGetT_Dist(X: Extended; ADegFreedom: Integer; ACumulative: Boolean): Extended;
begin
  if ACumulative then
    Result := dxGetTDistCumulative(X, ADegFreedom)
  else
    Result := Exp(dxGetLnBeta(0.5 * ADegFreedom, 0.5) -
      Ln(Sqrt(ADegFreedom)) + (-0.5 * (ADegFreedom + 1)) * Ln(1 + X * X / ADegFreedom));
end;

function dxGetT_Dist_RT(X: Extended; ADegFreedom: Integer): Extended;
begin
  Result := 1 - dxGetTDistCumulative(X, ADegFreedom);
end;

function dxGetT_Dist_2T(X: Extended; ADegFreedom: Integer): Extended;
begin
  Result := 2 * dxGetT_Dist_RT(X, ADegFreedom);
end;

function dxGetT_Inv(AProbability: Extended; ADegFreedom: Integer): Extended;
var
  ABetaInv1, ABetaInv2: Extended;
  ASign: Integer;
begin
  Result := 0;
  if AProbability = 0.5 then
    Exit;
  if (AProbability > 0.25) and (AProbability < 0.75) then
  begin
    ABetaInv1 := dxGetBetaInv(Abs(1 - 2 * AProbability), 0.5, 0.5 * ADegFreedom, 0, 1);
    Result := IfThen(AProbability < 0.5, -1, 1) * Sqrt(ADegFreedom * ABetaInv1 / (1 - ABetaInv1));
    Exit;
  end;
  ASign := -1;
  if AProbability >= 0.75 then
  begin
    AProbability := 1 - AProbability;
    ASign := 1;
  end;
  ABetaInv2 := dxGetBetaInv(2 * AProbability, 0.5 * ADegFreedom, 0.5, 0, 1);
  Result := ASign * IfThen(MaxDouble * ABetaInv2 < ADegFreedom, MaxDouble, Sqrt(ADegFreedom * (1 - ABetaInv2) / ABetaInv2));
end;

function dxGetT_Inv_2T(AProbability: Extended; ADegFreedom: Integer): Extended;
begin
  Result := -dxGetT_Inv(AProbability / 2, ADegFreedom);
end;

function IsAllowedAsNumeric(var AValue: Variant): Boolean;
begin
  if dxIsLogical(AValue) then
    AValue := Integer(AValue = True);
  Result := VarIsNumeric(AValue) or dxIsDateTime(AValue);
end;

procedure SetMaxDataValue(const AValue: Variant; AData: TdxSpreadSheetEnumValues);
begin
  AData.ResultValue := Max(AData.ResultValue, AValue);
end;

procedure SetMinDataValue(const AValue: Variant; AData: TdxSpreadSheetEnumValues);
begin
  AData.ResultValue := Min(AData.ResultValue, AValue);
end;

function cbMinMax(const AParameter: Variant; ACanConvertStrToNumber: Boolean; var AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AData: TdxSpreadSheetEnumValues; AWithoutSimularNumericByReference: Boolean; ASetValueProcedure: TdxSetValue;
  AInfo: PdxSpreadSheetCellReference): Boolean;
var
  ACheckedValue: Variant;
begin
  Result := AErrorCode = ecNone;
  if not CanProcessingParameter(AData.ProcessingInfo, AInfo) then
    Exit;
  if Result then
  begin
    if AData.CheckValueOnNumeric(AParameter, ACanConvertStrToNumber, AWithoutSimularNumericByReference, ACheckedValue) then
      ASetValueProcedure(ACheckedValue, AData)
    else
      if ACanConvertStrToNumber then
        AData.SetErrorCode(ecValue)
      else
        if not(AWithoutSimularNumericByReference or dxSpreadSheetIsNullValue(AParameter)) then
          ASetValueProcedure(0, AData);
  end
  else
    AData.SetErrorCode(AErrorCode);
end;

function cbMax(const AParameter: Variant; ACanConvertStrToNumber: Boolean; var AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AData: TdxSpreadSheetEnumValues; AInfo: PdxSpreadSheetCellReference = nil): Boolean;
begin
  Result := cbMinMax(AParameter, ACanConvertStrToNumber, AErrorCode, AData, True, @SetMaxDataValue, AInfo);
end;

function cbMaxA(const AParameter: Variant; ACanConvertStrToNumber: Boolean; var AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AData: TdxSpreadSheetEnumValues; AInfo: PdxSpreadSheetCellReference = nil): Boolean;
begin
  Result := cbMinMax(AParameter, ACanConvertStrToNumber, AErrorCode, AData, False, @SetMaxDataValue, AInfo);
end;

function cbMin(const AParameter: Variant; ACanConvertStrToNumber: Boolean; var AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AData: TdxSpreadSheetEnumValues; AInfo: PdxSpreadSheetCellReference = nil): Boolean;
begin
  Result := cbMinMax(AParameter, ACanConvertStrToNumber, AErrorCode, AData, True, @SetMinDataValue, AInfo);
end;

function cbMinA(const AParameter: Variant; ACanConvertStrToNumber: Boolean; var AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AData: TdxSpreadSheetEnumValues; AInfo: PdxSpreadSheetCellReference = nil): Boolean;
begin
  Result := cbMinMax(AParameter, ACanConvertStrToNumber, AErrorCode, AData, False, @SetMinDataValue, AInfo);
end;

procedure dxCalculateAverage(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AData: TdxSpreadSheetEnumValues; AWithText: Boolean; var AAverage: Extended; var ACount: Integer);
begin
  if AWithText then
    dxSpreadSheetCalculateForEachParam(Sender, AParams, AData, @cbSumA)
  else
    dxSpreadSheetCalculateForEachParam(Sender, AParams, AData, @cbSum);
  Sender.SetError(AData.ErrorCode);
  if Sender.Validate then
  begin
    if AWithText then
      ACount := AData.Count - AData.EmptyCount - AData.NullCount
    else
      ACount := AData.NumericCount;
    if ACount <> 0 then
      AAverage := AData.ResultValue / ACount
    else
      AAverage := 0;
  end;
end;

function dxCheckRangesSize(Sender: TdxSpreadSheetFormulaResult; ARange1, ARange2: TdxSpreadSheetFormulaToken): Boolean;
var
  ADimension1, ADimension2: TdxSpreadSheetFormulaTokenDimension;
  AError1, AError2: TdxSpreadSheetFormulaErrorCode;
begin
  ADimension1 := ARange1.GetDimension(AError1);
  ADimension2 := ARange2.GetDimension(AError2);
  Result := (AError1 = ecNone) and (AError2 = ecNone) and (ADimension1 = ADimension2);
  if not Result then
  begin
    if AError2 <> ecNone then
      Sender.SetError(AError2);
    if AError1 <> ecNone then
      Sender.SetError(AError1);
    if Sender.Validate then
      Sender.SetError(ecValue);
  end;
end;

procedure dxExtractCustomIfsAdditionalConditions(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AData: TdxSpreadSheetEnumValuesWithCondition; const APriorRequiredParamCount: Integer);
var
  AParamCount, AConditionIndex: Integer;
  I, J: Integer;
  AConditionRange: TdxSpreadSheetFormulaToken;
  ACondition: Variant;
  AOperation: TdxSpreadSheetFormulaOperation;
begin
  AParamCount := Sender.GetParamsCount(AParams);
  if not Odd(AParamCount + APriorRequiredParamCount) then
    for I := 1 to AParamCount div 2 - 1 do
    begin
      AConditionIndex := APriorRequiredParamCount + I * 2 - 1;
      Sender.ExtractCondition(AParams, 255, AConditionIndex, ACondition, AOperation);
      if Sender.Validate then
      begin
        AConditionRange := AParams;
        for J := 1 to AConditionIndex - 1 do
          AConditionRange := AConditionRange.Next;
        AConditionRange := AConditionRange.FirstChild;
        if not dxSpreadSheetIsReferenceToken(AConditionRange, Sender) then
          Sender.SetError(ecNA)
        else
        if dxCheckRangesSize(Sender, AData.ConditionRange, AConditionRange) then
          AData.AddAdditionalCondition(AConditionRange, ACondition, AOperation);
      end;
      if not Sender.Validate then
        Break;
    end
  else
    Sender.SetError(ecNA);
end;

procedure dxCustomSumIfCalculate(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AData: TdxSpreadSheetEnumValuesWithCondition; const AIFS: Boolean);

  function ExtractBaseRanges(var ARange, AResultRange: TdxSpreadSheetFormulaToken; var ARowShift, AColumnShift: Integer): Boolean;
  begin
    Result := True;
    if AIFS then
    begin
      AResultRange := AParams.FirstChild;
      if Sender.GetParamsCount(AParams) >= 3 then
        ARange := AParams.Next.FirstChild
      else
        Result := False;
    end
    else
    begin
      ARange := AParams.FirstChild;
      AResultRange := ARange;
      if Sender.GetParamsCount(AParams) = 3 then
        AResultRange := AParams.Next.Next.FirstChild;
    end;
    if Result then
      Result := dxSpreadSheetIsReferenceToken(ARange, Sender) and dxSpreadSheetIsReferenceToken(AResultRange, Sender)
        and Sender.Validate
    else
      Sender.SetError(ecNA);

    if Result and AIFS then
      Result := dxCheckRangesSize(Sender, ARange, AResultRange);
  end;

var
  ARowShift, AColumnShift: Integer;
begin
  AData.ResultValue := 0;
  AData.ErrorCode := ecNone;
  AData.CountValues := 0;
  ARowShift := 0;
  AColumnShift := 0;
  if AIFS then
  begin
    Sender.ExtractCondition(AParams, 255, 2, AData.ConditionValue, AData.Operation);
    if Sender.Validate then
    begin
      if ExtractBaseRanges(AData.ConditionRange, AData.ResultRange, ARowShift, AColumnShift) then
        dxExtractCustomIfsAdditionalConditions(Sender, AParams, AData, 3);
    end;
  end
  else
  begin
    Sender.ExtractCondition(AParams, 3, 1, AData.ConditionValue, AData.Operation);
    if Sender.Validate then
      ExtractBaseRanges(AData.ConditionRange, AData.ResultRange, ARowShift, AColumnShift);
  end;
  if Sender.Validate then
  begin
    AData.Value1 := ARowShift;
    AData.Value2 := AColumnShift;
    TdxTokenAccess(AData.ConditionRange).ForEach(@cbSumIF, AData, AData.ErrorCode);
  end
  else
    AData.ErrorCode := Sender.ErrorCode;
end;

procedure dxCustomAverage(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AWithText, AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal: Boolean);
var
  AAverage: Extended;
  ACount: Integer;
  AData: TdxSpreadSheetEnumValues;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, False, AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    dxCalculateAverage(Sender, AParams, AData, AWithText, AAverage, ACount);
    if Sender.Validate then
      if ACount > 0 then
        Sender.AddValue(AAverage)
      else
        Sender.SetError(ecDivByZero);
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnAveDev(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AData: TdxSpreadSheetEnumValues;
  AAverage: Extended;
  ACount: Integer;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, True, False, False);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    dxCalculateAverage(Sender, AParams, AData, False, AAverage, ACount);
    if Sender.Validate then
      if ACount > 0 then
        Sender.AddValue(dxGetSumOfAbsoluteDeviations(AData.NumericList, AAverage) / ACount)
      else
        Sender.SetError(ecNUM);
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnAverage(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomAverage(Sender, AParams, False, False, False, False);
end;

procedure fnAverageA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomAverage(Sender, AParams, True, False, False, False);
end;

procedure dxCustomAverageIF(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken; const AIFS: Boolean);
var
  AData: TdxSpreadSheetEnumValuesWithCondition;
begin
   AData := TdxSpreadSheetEnumValuesWithCondition.Create;
   try
     dxCustomSumIfCalculate(Sender, AParams, AData, AIFS);
     if Sender.Validate and (AData.CountValues = 0) then
       Sender.SetError(ecDivByZero);
     if Sender.Validate and (AData.CountValues = 0) then
       Sender.SetError(AData.ErrorCode);
     if Sender.Validate then
       Sender.AddValue(AData.ResultValue / AData.CountValues);
  finally
    AData.Free;
  end;
end;

procedure fnAverageIF(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomAverageIF(Sender, AParams, False);
end;

procedure fnAverageIFS(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomAverageIF(Sender, AParams, True);
end;

procedure fnBetaDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X, AAlpha, ABeta, A, B: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) and Sender.ExtractNumericParameter(AAlpha, AParams, 1) and
     Sender.ExtractNumericParameter(ABeta, AParams, 2) and
     Sender.ExtractNumericParameterDef(A, 0, 0, AParams, 3) and Sender.ExtractNumericParameterDef(B, 1, 1, AParams, 4) then
    if (AAlpha <= 0) or (ABeta <= 0) or (X < A) or (X > B) or (A = B) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetBetaDist(X, AAlpha, ABeta, A, B, True));
end;

procedure fnBeta_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X, AAlpha, ABeta, ACumulative, A, B: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) and Sender.ExtractNumericParameter(AAlpha, AParams, 1) and
     Sender.ExtractNumericParameter(ABeta, AParams, 2) and Sender.ExtractNumericParameter(ACumulative, AParams, 3) and
     Sender.ExtractNumericParameterDef(A, 0, 0, AParams, 4) and Sender.ExtractNumericParameterDef(B, 1, 1, AParams, 5) then
    if (AAlpha <= 0) or (ABeta <= 0) or (X < A) or (X > B) or (A = B) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetBetaDist(X, AAlpha, ABeta, A, B, ACumulative <> 0));
end;

procedure fnBeta_Inv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AProbability, AAlpha, ABeta, A, B: Variant;
begin
  if Sender.ExtractNumericParameter(AProbability, AParams) and
     Sender.ExtractNumericParameter(AAlpha, AParams, 1) and Sender.ExtractNumericParameter(ABeta, AParams, 2) and
     Sender.ExtractNumericParameterDef(A, 0, 0, AParams, 3) and Sender.ExtractNumericParameterDef(B, 1, 1, AParams, 4) then
    if (AAlpha <= 0) or (ABeta <= 0) or (AProbability <= 0) or (AProbability >= 1) or (A >= B) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetBetaInv(AProbability, AAlpha, ABeta, A, B));
end;

function dxGetBinomDist(ANumber, ATrials: Integer; AProbability: Extended; ACumulative: Boolean): Extended;
var
  I: Integer;
begin
  if not ACumulative then
    Result := dxGetBinomialProbabilityMass(ANumber, ATrials, AProbability)
  else
  begin
    Result := 0;
    for I := 0 to ANumber do
      Result := Result + dxGetBinomialProbabilityMass(I, ATrials, AProbability);
  end;
end;

procedure fnBinom_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ATrials, AProbability, ACumulative: Variant;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameter(ATrials, AParams, 1) and
     Sender.ExtractNumericParameter(AProbability, AParams, 2) and Sender.ExtractNumericParameter(ACumulative, AParams, 3) then
    if (ANumber < 0) or (ANumber > ATrials) or (AProbability < 0) or (AProbability > 1) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetBinomDist(Trunc(ANumber), Trunc(ATrials), AProbability, ACumulative <> 0));
end;

function dxGetBinom_Dist_Range(ATrials: Integer; AProbability: Extended; ANumber_s, ANumber_s2: Integer): Extended;
var
  I: Integer;
begin
  Result := 0;
  for I := ANumber_s to ANumber_s2 do
    Result := Result + dxGetBinomialProbabilityMass(I, ATrials, AProbability);
  Result := Min(1, Result);
end;

procedure fnBinom_Dist_Range(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ATrials, AProbability, ANumber_s, ANumber_s2: Variant;
begin
  if Sender.ExtractNumericParameter(ATrials, AParams) and Sender.ExtractNumericParameter(AProbability, AParams, 1) and
     Sender.ExtractNumericParameter(ANumber_s, AParams, 2) and
     Sender.ExtractNumericParameterDef(ANumber_s2, ANumber_s, ANumber_s, AParams, 3) then
    if (ATrials < 0) or (AProbability < 0) or (AProbability > 1) or (ANumber_s < 0) or (ANumber_s2 < 0) then
      Sender.SetError(ecNUM)
    else
    begin
      ATrials := Trunc(ATrials);
      ANumber_s := Trunc(ANumber_s);
      ANumber_s2 := Trunc(ANumber_s2);
      if (ANumber_s > ATrials) or (ANumber_s2 < ANumber_s) or (ANumber_s2 > ATrials) then
        Sender.SetError(ecNUM)
      else
        Sender.AddValue(dxGetBinom_Dist_Range(ATrials, AProbability, ANumber_s, ANumber_s2));
    end;
end;

function dxGetChiSQ_Dist(X: Extended; ADegFreedom: Integer; ACumulative: Boolean): Extended;
begin
  Result := dxGetGammaDist(X / 2, ADegFreedom / 2, 1, ACumulative);
  if not ACumulative then
    Result := Result / 2;
end;

procedure fnChiSQ_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X, ADegFreedom, ACumulative: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) and Sender.ExtractNumericParameter(ADegFreedom, AParams, 1) and
     Sender.ExtractNumericParameter(ACumulative, AParams, 2) then
    if (X < 0) or (ADegFreedom < 1) or (ADegFreedom > 1E10) or ((ACumulative = 0) and (X = 0) and (ADegFreedom < 2)) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetChiSQ_Dist(X, Trunc(ADegFreedom), ACumulative <> 0));
end;

procedure fnChiSQ_Dist_RT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X, ADegFreedom: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) and Sender.ExtractNumericParameter(ADegFreedom, AParams, 1) then
    if (X < 0) or (ADegFreedom < 1) or (ADegFreedom > 1E10) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(1 - dxGetChiSQ_Dist(X, Trunc(ADegFreedom), True));
end;

function dxGetChiSQ_Inv(AProbability: Extended; ADegFreedom: Integer): Extended;
begin
  Result := 2 * dxGetGammaInv(AProbability, ADegFreedom / 2, 1);
end;

procedure fnChiSQ_Inv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AProbability, ADegFreedom: Variant;
begin
  if Sender.ExtractNumericParameter(AProbability, AParams) and Sender.ExtractNumericParameter(ADegFreedom, AParams, 1) then
    if (AProbability < 0) or (AProbability >= 1) or (ADegFreedom < 1) or (ADegFreedom > 1E10) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetChiSQ_Inv(AProbability, Trunc(ADegFreedom)));
end;

procedure fnChiSQ_Inv_RT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AProbability, ADegFreedom: Variant;
begin
  if Sender.ExtractNumericParameter(AProbability, AParams) and Sender.ExtractNumericParameter(ADegFreedom, AParams, 1) then
    if (AProbability <= 0) or (AProbability > 1) or (ADegFreedom < 1) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetChiSQ_Inv(1 - AProbability, Trunc(ADegFreedom)));
end;

procedure fnCorrel(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnPearson(Sender, AParams);
end;

procedure dxRelationshipBetweenTwoDatasets(Sender: TdxSpreadSheetFormulaResult; AParamY, AParamX: TdxSpreadSheetFormulaToken;
  ACalculateRatioItemsProcedure: TdxCalculateRatioItemsProcedure; const ADivByZeroOccuring: Integer;
  var AAvgX, AAvgY, AResult: Extended);

  function CreateVariantArray(ADimension: TdxSpreadSheetFormulaTokenDimension): Variant;
  begin
    Result := VarArrayCreate([0, ADimension.RowCount - 1, 0, ADimension.ColumnCount - 1], varVariant);
  end;

  procedure PopulateVariantArray(var AArray: Variant; const ADimension: TdxSpreadSheetFormulaTokenDimension; const AParams: TdxSpreadSheetFormulaToken);

    function IsNumber(const AValue: Variant): Boolean;
    begin
      Result := ((AParams is TdxSpreadSheetFormulaAreaReference) and dxIsNumberOrDateTime(AValue)) or
        ((AParams is TdxSpreadSheetFormulaArrayToken) and VarIsNumeric(AValue) and not dxIsLogical(AValue));
    end;

  var
    AValue: Variant;
    ARow: TdxSpreadSheetVector;
    AErrorCode: TdxSpreadSheetFormulaErrorCode;
    ANumeric: Double;
    I, J: Integer;
  begin
    for I := 0 to ADimension.RowCount - 1 do
    begin
      ARow := Sender.ExtractRowFromRange(AParams, I, AErrorCode);
      try
        for J := 0 to ADimension.ColumnCount - 1 do
        begin
          AValue := ARow[J].Value;
          if ARow[J].IsError or not IsNumber(AValue) then
            AArray[I, J] := Null
          else
            if dxConvertXLSDateToNumeric(AValue, ANumeric) then
              AArray[I, J] := ANumeric
            else
              AArray[I, J] := AValue;
        end;
      finally
        ARow.Free;
      end;
    end;
  end;

  procedure SynchronizeArraysNullItems(var AKnownY, AKnownX: Variant; const ADimension: TdxSpreadSheetFormulaTokenDimension);
  var
    I, J: Integer;
  begin
    for I := 0 to ADimension.RowCount - 1 do
      for J := 0 to ADimension.ColumnCount - 1 do
        if VarIsNull(AKnownY[I, J]) or VarIsNull(AKnownX[I, J]) then
        begin
          AKnownY[I, J] := Null;
          AKnownX[I, J] := Null;
        end;
  end;

  function IsCalculatedAvg(const AArray: Variant; const ADimension: TdxSpreadSheetFormulaTokenDimension; var AAvg: Extended): Boolean;
  var
    I, J, ACount: Integer;
    ASumma: Extended;
  begin
    ASumma := 0;
    ACount := 0;
    for I := 0 to ADimension.RowCount - 1 do
      for J := 0 to ADimension.ColumnCount - 1 do
        if not VarIsNull(AArray[I, J]) then
        begin
          Inc(ACount);
          ASumma := ASumma + AArray[I, J];
        end;
    Result := ACount > ADivByZeroOccuring;
    if Result then
      AAvg := ASumma / ACount;
  end;

var
  AKnownY, AKnownX: Variant;
  ADimensionY, ADimensionX: TdxSpreadSheetFormulaTokenDimension;
  ANominator, ADenominator: Extended;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
begin
  dxSpreadSheetIsReferenceToken(AParamY, Sender);
  ADimensionY := AParamY.GetDimension(AErrorCode);
  if AErrorCode = ecNone then
  begin
    dxSpreadSheetIsReferenceToken(AParamX, Sender);
    ADimensionX := AParamX.GetDimension(AErrorCode);
  end;
  if (AErrorCode <> ecNone) or (ADimensionY <> ADimensionX) then
  begin
    Sender.SetError(ecNA);
    Exit;
  end;
  AKnownY := CreateVariantArray(ADimensionY);
  AKnownX := CreateVariantArray(ADimensionY);
  PopulateVariantArray(AKnownY, ADimensionY, AParamY);
  PopulateVariantArray(AKnownX, ADimensionY, AParamX);
  SynchronizeArraysNullItems(AKnownY, AKnownX, ADimensionY);
  if not(IsCalculatedAvg(AKnownX, ADimensionY, AAvgX) and IsCalculatedAvg(AKnownY, ADimensionY, AAvgY)) then
    Sender.SetError(ecDivByZero)
  else
  begin
    ACalculateRatioItemsProcedure(AKnownX, AKnownY, ADimensionY, AAvgX, AAvgY, ANominator, ADenominator);
    if ADenominator <> 0 then
      AResult := ANominator / ADenominator
    else
      Sender.SetError(ecDivByZero);
  end;
end;

procedure dxCalculateCovariance_P_RatioItems(const AArrayX, AArrayY: Variant; const AArraysDimension: TdxSpreadSheetFormulaTokenDimension;
  const AAvgX, AAvgY: Extended; var ANominator, ADenominator: Extended);
var
  I, J: Integer;
begin
  ANominator := 0;
  ADenominator := 0;
  for I := 0 to AArraysDimension.RowCount - 1 do
    for J := 0 to AArraysDimension.ColumnCount - 1 do
      if not VarIsNull(AArrayX[I, J]) then
      begin
        ANominator := ANominator + (AArrayX[I, J] - AAvgX) * (AArrayY[I, J] - AAvgY);
        ADenominator := ADenominator + 1;
      end;
end;

procedure fnCovariance_P(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AAvgX, AAvgY, ACovariance_P: Extended;
begin
  if (AParams = nil) or (AParams.Next = nil) then
    Sender.SetError(ecNA)
  else
    dxRelationshipBetweenTwoDatasets(Sender, AParams.FirstChild, AParams.Next.FirstChild, @dxCalculateCovariance_P_RatioItems,
    0, AAvgX, AAvgY, ACovariance_P);
  if Sender.Validate then
    Sender.AddValue(ACovariance_P);
end;

procedure dxCalculateCovariance_S_RatioItems(const AArrayX, AArrayY: Variant; const AArraysDimension: TdxSpreadSheetFormulaTokenDimension;
  const AAvgX, AAvgY: Extended; var ANominator, ADenominator: Extended);
begin
  dxCalculateCovariance_P_RatioItems(AArrayX, AArrayY, AArraysDimension, AAvgX, AAvgY, ANominator, ADenominator);
  ADenominator := ADenominator - 1;
end;

procedure fnCovariance_S(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AAvgX, AAvgY, ACovariance_S: Extended;
begin
  dxRelationshipBetweenTwoDatasets(Sender, AParams.FirstChild, AParams.Next.FirstChild, @dxCalculateCovariance_S_RatioItems,
    0, AAvgX, AAvgY, ACovariance_S);
  if Sender.Validate then
    Sender.AddValue(ACovariance_S);
end;

procedure dxCustomCount(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal: Boolean);
var
  AData: TdxSpreadSheetEnumValues;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, False, AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    dxSpreadSheetCalculateForEachParam(Sender, AParams, AData, @cbCount);
    if AData.Validate then
      Sender.AddValue(AData.NumericCount)
    else
      Sender.SetError(AData.ErrorCode);
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnCount(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomCount(Sender, AParams, False, False, False);
end;

procedure dxCustomCountA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal: Boolean);
var
  AData: TdxSpreadSheetEnumValues;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, False, AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    dxSpreadSheetCalculateForEachParam(Sender, AParams, AData, @cbCountA);
    if AData.Validate then
      Sender.AddValue(AData.Count - AData.EmptyCount - AData.NullCount)
    else
      Sender.SetError(AData.ErrorCode);
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnCountA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomCountA(Sender, AParams, False, False, False);
end;

procedure fnCountBlank(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AData: TdxSpreadSheetEnumValues;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, False, False, False);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    dxSpreadSheetCalculateForEachParam(Sender, AParams, AData, @cbCountA);
    if AData.Validate then
      Sender.AddValue(AData.EmptyCount + AData.NullCount)
    else
      Sender.SetError(AData.ErrorCode);
  finally
    FreeAndNil(AData);
  end;
end;

function cbCountIF(const AValue: Variant; ACanConvertStrToNumber: Boolean;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode; AData, AInfo: Pointer): Boolean;
var
  AResult: TdxSpreadSheetEnumValuesWithCondition;
begin
  Result := True;
  AResult := TdxSpreadSheetEnumValuesWithCondition(AData);
  try
    if dxCheckCondition(AValue, AResult.ConditionValue, AResult.Operation) and
       dxCheckAdditionalConditions(AResult, PdxSpreadSheetCellReference(AInfo)^) then
      AResult.ResultValue := AResult.ResultValue + 1;
  except
    on EVariantError do;
  end;
end;

procedure dxCustomCountIF(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken; const AIFS: Boolean);
const
  AMaxParamCount: array[Boolean] of Integer = (2, 254);
var
  AData: TdxSpreadSheetEnumValuesWithCondition;
begin
   AData := TdxSpreadSheetEnumValuesWithCondition.Create;
   try
     AData.ResultValue := 0;
     AData.ErrorCode := ecNone;
     AData.CountValues := 0;
     Sender.ExtractCondition(AParams, AMaxParamCount[AIFS], 1, AData.ConditionValue, AData.Operation);
     if Sender.Validate then
     begin
       AData.ConditionRange := AParams.FirstChild;
       if not dxSpreadSheetIsReferenceToken(AData.ConditionRange, Sender) then
         Sender.SetError(ecNA);
     end;
     if AIFS and Sender.Validate then
       dxExtractCustomIfsAdditionalConditions(Sender, AParams, AData, 2);

     if Sender.Validate then
     begin
       TdxTokenAccess(AData.ConditionRange).ForEach(@cbCountIF, AData, AData.ErrorCode);
       if Sender.Validate then
         Sender.SetError(AData.ErrorCode);
       if Sender.Validate then
         Sender.AddValue(AData.ResultValue);
     end;
  finally
    AData.Free;
  end;
end;

procedure fnCountIF(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomCountIF(Sender, AParams, False);
end;

procedure fnCountIFS(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomCountIF(Sender, AParams, True);
end;

procedure fnDevSQ(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AData: TdxSpreadSheetEnumValues;
  AAverage: Extended;
  ACount: Integer;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, True, False, False);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    dxCalculateAverage(Sender, AParams, AData, False, AAverage, ACount);
    if Sender.Validate then
      Sender.AddValue(dxGetSumOfSquaresOfDeviations(AData.NumericList, AAverage));
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnExpon_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X, ALambda, ACumulative: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) and Sender.ExtractNumericParameter(ALambda, AParams, 1) and
     Sender.ExtractNumericParameter(ACumulative, AParams, 2) then
    if (X < 0) or (ALambda <= 0) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetExponDist(X, ALambda, ACumulative <> 0));
end;

procedure dxCalculateSlopeRatioItems(const AArrayX, AArrayY: Variant; const AArraysDimension: TdxSpreadSheetFormulaTokenDimension;
  const AAvgX, AAvgY: Extended; var ANominator, ADenominator: Extended);
var
  I, J: Integer;
begin
  ANominator := 0;
  ADenominator := 0;
  for I := 0 to AArraysDimension.RowCount - 1 do
    for J := 0 to AArraysDimension.ColumnCount - 1 do
      if not VarIsNull(AArrayX[I, J]) then
      begin
        ANominator := ANominator + (AArrayX[I, J] - AAvgX) * (AArrayY[I, J] - AAvgY);
        ADenominator := ADenominator + Sqr((AArrayX[I, J] - AAvgX));
      end;
end;

procedure dxCalculateIntercept(Sender: TdxSpreadSheetFormulaResult; const AParamsY, AParamsX: TdxSpreadSheetFormulaToken;
  var AAvgX, AAvgY, ASlope, AIntercept: Extended);
begin
  dxRelationshipBetweenTwoDatasets(Sender, AParamsY, AParamsX, @dxCalculateSlopeRatioItems, 1, AAvgX, AAvgY, ASlope);
  if Sender.Validate then
    AIntercept := AAvgY - ASlope * AAvgX;
end;

procedure fnForecast(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X: Variant;
  AAvgX, AAvgY, ASlope, AIntercept: Extended;
begin
  if Sender.ExtractNumericParameter(X, AParams) then
  begin
    dxCalculateIntercept(Sender, AParams.Next.FirstChild, AParams.Next.Next.FirstChild, AAvgX, AAvgY, ASlope, AIntercept);
    if Sender.Validate then
      Sender.AddValue(AIntercept + ASlope * X);
  end;
end;

function dxGetGamma(X: Extended): Extended;
begin
  if X > 0 then
    Result := Exp(dxGetGammaLn(X))
  else
    Result := Pi / (Sin(Pi * X) * dxGetGamma(1 - X));
end;

procedure fnGamma(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) then
    if (X < 1) and (X = Trunc(X)) or (X > 171.6) or (X < {$IFDEF CPUX64}-170.6{$ELSE}-172.6{$ENDIF}) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetGamma(X));
end;

procedure fnGamma_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X, AAlpha, ABeta, ACumulative: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) and Sender.ExtractNumericParameter(AAlpha, AParams, 1) and
     Sender.ExtractNumericParameter(ABeta, AParams, 2) and Sender.ExtractNumericParameter(ACumulative, AParams, 3) then
    if (X < 0) or (AAlpha <= 0) or (ABeta <= 0) or ((ACumulative = 0) and (X = 0) and (AAlpha <= 1 )) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetGammaDist(X, AAlpha, ABeta, ACumulative <> 0));
end;

procedure fnGamma_Inv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AProbability, AAlpha, ABeta: Variant;
begin
  if Sender.ExtractNumericParameter(AProbability, AParams) and
     Sender.ExtractNumericParameter(AAlpha, AParams, 1) and Sender.ExtractNumericParameter(ABeta, AParams, 2) then
  if (AAlpha <= 0) or (ABeta <= 0) or (AProbability < 0) or (AProbability >= 1) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetGammaInv(AProbability, AAlpha, ABeta));
end;

procedure fnGammaLn(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) then
    if X <= 0 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetGammaLn(X));
end;

procedure fnGammaLn_Precise(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnGammaLn(Sender, AParams);
end;

function dxGetGauss(X: Extended): Extended;
begin
  if X >= 9 then
    Result := 0.5
  else
    if X <= -9 then
      Result := -0.5
    else
      Result := dxGetStandartNormDist(X) - 0.5;
end;

procedure fnGauss(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) then
    Sender.AddValue(dxGetGauss(X));
end;

procedure fnGeomean(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

const
  ecgmNum      = -1;
  ecgmOverflow = 0;

  function GetGeomean(const ANumbers: TList<Double>; ADirectCalculation: Boolean): Extended;
  var
    I, ACount: Integer;
  begin
    ACount := ANumbers.Count;
    Result := ecgmNum;
    if ACount = 0 then
      Exit;
    Result := 1;
    for I := 0 to ACount - 1 do
      if ANumbers[I] > 0 then
        try
          if ADirectCalculation then
            Result := Result * ANumbers[I]
          else
            Result := Result * Power(ANumbers[I], 1 / ACount);
        except
          on EOverflow do
          begin
            Result := ecgmOverflow;
            Break;
          end;
        end
      else
        begin
          Result := ecgmNum;
          Break;
        end;
    if ADirectCalculation and (Result > ecgmOverflow) then
      Result := Power(Result, 1/ACount);
  end;

var
  AData: TdxSpreadSheetEnumValues;
  AResult: Extended;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, False, False, False);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    if AData.PopulateNumericList(Sender, AParams, True) <> ecNone then
      Sender.SetError(AData.ErrorCode)
    else
    begin
      AResult := GetGeomean(AData.NumericList, True);
      if AResult = ecgmOverflow then
        AResult := GetGeomean(AData.NumericList, False);
      if AResult > 0 then
        Sender.AddValue(AResult)
      else
        Sender.SetError(ecNUM);
    end;
  finally
    FreeAndNil(AData);
  end;
end;

function dxGetHypgeomDist(ASamples, ASamplesSize, APopulation, APopulationSize: Integer): Extended; overload; inline;
begin
  Result := dxGetCombin(APopulation, ASamples) * dxGetCombin(APopulationSize - APopulation, ASamplesSize - ASamples) /
    dxGetCombin(APopulationSize, ASamplesSize);
end;

function dxGetHypgeomDist(ASamples, ASamplesSize, APopulation, APopulationSize: Integer; ACumulative: Boolean): Extended; overload;
var
  I: Integer;
begin
  Result := 0;
  if ACumulative then
  begin
    if ASamples >= APopulation then
      Result := 1
    else
    begin
      for I := 0 to ASamples do
        Result := Result + dxGetHypgeomDist(I, ASamplesSize, APopulation, APopulationSize);
      Result := Min(1, Result);
    end;
  end
  else
    if not((ASamples > Min(ASamplesSize, APopulation)) or
       (ASamples < Max(0, ASamplesSize - APopulationSize + APopulation)) or
       (ASamplesSize = 0) or (APopulation = 0)) then
      Result := dxGetHypgeomDist(ASamples, ASamplesSize, APopulation, APopulationSize);
end;

procedure fnCustomHypgeomDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  const AIsCompatibility: Boolean);
var
  ASamples, ASamplesSize, APopulation, APopulationSize, ACumulative: Variant;
begin
  if Sender.ExtractNumericParameter(ASamples, AParams) and Sender.ExtractNumericParameter(ASamplesSize, AParams, 1) and
     Sender.ExtractNumericParameter(APopulation, AParams, 2) and Sender.ExtractNumericParameter(APopulationSize, AParams, 3) then
    if (ASamples < 0) or (ASamplesSize < 0) or (APopulation < 0) or (APopulationSize <= 0) then
      Sender.SetError(ecNUM)
    else
    begin
      ACumulative := 0;
      if not AIsCompatibility then
        if not Sender.ExtractNumericParameterDef(ACumulative, 0, 0, AParams, 4) then
          Exit;
      ASamples := Trunc(ASamples);
      ASamplesSize := Trunc(ASamplesSize);
      APopulation := Trunc(APopulation);
      APopulationSize := Trunc(APopulationSize);
      if (APopulation > APopulationSize) or (ASamplesSize > APopulationSize) then
        Sender.SetError(ecNUM)
      else
        Sender.AddValue(dxGetHypgeomDist(ASamples, ASamplesSize, APopulation, APopulationSize, ACumulative <> 0));
    end;
end;

procedure fnHypgeomDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnCustomHypgeomDist(Sender, AParams, True);
end;

procedure fnHypgeom_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnCustomHypgeomDist(Sender, AParams, False);
end;

procedure fnIntercept(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AAvgX, AAvgY, ASlope, AIntercept: Extended;
begin
  dxCalculateIntercept(Sender, AParams.FirstChild, AParams.Next.FirstChild, AAvgX, AAvgY, ASlope, AIntercept);
  if Sender.Validate then
    Sender.AddValue(AIntercept);
end;

procedure dxGetSerialValue(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken; AIsAscending: Boolean);
var
  AData: TdxSpreadSheetEnumValues;
  ANumber: Variant;
  AIntNumber: Integer;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(AIsAscending, False, False, False);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    if (AData.PopulateNumericList(Sender, AParams) = ecNone) and Sender.ExtractNumericParameter(ANumber, AParams, 1)then
    begin
      AIntNumber := Trunc(ANumber);
      if (AIntNumber <= 0) or (AIntNumber > AData.NumericCount) then
        Sender.SetError(ecNUM)
      else
      begin
        AData.NumericList.Sort;
        Sender.AddValue(AData.NumericList[AIntNumber - 1]);
      end;
    end;
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnLarge(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxGetSerialValue(Sender, AParams, False);
end;

procedure dxCustomMinMax(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AProc: TdxSpreadSheetForEachCallBack; AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal: Boolean);
var
  AData: TdxSpreadSheetEnumValues;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, False, AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, MaxInt, ADataInfo);
  try
    if (@AProc = @cbMax) or (@AProc = @cbMaxA) then
      AData.ResultValue := MinInt;
    dxSpreadSheetCalculateForEachParam(Sender, AParams, AData, AProc);
    if not AData.Validate then
      Sender.SetError(AData.ErrorCode)
    else
      if AData.NumericCount > 0 then
        Sender.AddValue(AData.ResultValue)
      else
        Sender.AddValue(0);
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnMax(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomMinMax(Sender, AParams, @cbMax, False, False, False);
end;

procedure fnMaxA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomMinMax(Sender, AParams, @cbMaxA, False, False, False);
end;

procedure fnMedian(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AData: TdxSpreadSheetEnumValues;
  ACount, AIndex: Integer;
  ANumbers: TList<Double>;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, False, False, False);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    AData.PopulateNumericList(Sender, AParams, True);
    if not AData.Validate then
      Sender.SetError(AData.ErrorCode)
    else
    begin
      ACount := AData.NumericCount;
      if ACount = 0 then
        Sender.SetError(ecNUM)
      else
      begin
        ANumbers := AData.NumericList;
        ANumbers.Sort;
        AIndex := ACount div 2;
        if Odd(ACount) then
          Sender.AddValue(ANumbers[AIndex])
        else
          Sender.AddValue((ANumbers[AIndex - 1] + ANumbers[AIndex]) / 2);
      end;
    end;
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnMin(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomMinMax(Sender, AParams, @cbMin, False, False, False);
end;

procedure fnMinA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomMinMax(Sender, AParams, @cbMinA, False, False, False);
end;

procedure dxMode_Custom(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken; AAsArray: Boolean);
var
  AOccurrenceNumbers: TcxObjectList;

  function CheckOrdinaryParameters: TdxSpreadSheetFormulaErrorCode;
  var
    AParam: TdxSpreadSheetFormulaToken;
    ATempResult: TdxSpreadSheetFormulaResult;
    AValue: Variant;
    AErrorCode: TdxSpreadSheetFormulaErrorCode;
    ANeedExit: Boolean;
  begin
    Result := ecValue;
    AParam := AParams;
    while AParam <> nil do
    begin
      if AParam.HasChildren and not AParam.FirstChild.IsEnumeration then
      begin
        ATempResult := TdxFormulaAccess(Sender.Owner).Calculate(AParam.FirstChild);
        try
          ANeedExit := not ATempResult.Validate;
          if ANeedExit then
            Result := ATempResult.ErrorCode
          else
          begin
            if not ATempResult.LastItem.IsEnumeration then
            begin
              ATempResult.LastItem.GetValue(AValue, AErrorCode);
              ANeedExit := AErrorCode <> ecNone;
              if ANeedExit then
                Result := AErrorCode
              else
                ANeedExit := not dxIsNumberOrDateTime(AValue);
            end;
          end;
        finally
          ATempResult.Free;
        end;
        if ANeedExit then
          Exit;
      end;
      AParam := AParam.Next;
    end;
    Result := ecNone;
  end;

  function GetOccurrenceItem(const ANumber: Double): TdxOccurrence;
  var
    I: Integer;
    AItem: TdxOccurrence;
  begin
    Result := nil;
    for I := 0 to AOccurrenceNumbers.Count - 1 do
    begin
      AItem := TdxOccurrence(AOccurrenceNumbers[I]);
      if Abs((AItem.Number - ANumber)) < 1e-10 then
      begin
        Result := AItem;
        Break;
      end;
    end;
  end;

var
  AData: TdxSpreadSheetEnumValues;
  ANumbers: TList<Double>;
  AOccurrenceItem: TdxOccurrence;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
  AMaxCount, I: Integer;
  AResultArray, AParentToken: TdxSpreadSheetFormulaToken;
begin
  Sender.SetError(CheckOrdinaryParameters);
  if not Sender.Validate then
    Exit;
  ADataInfo.Init(False, False, False, False);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    AData.PopulateNumericList(Sender, AParams, True);
    if not AData.Validate then
      Sender.SetError(AData.ErrorCode)
    else
    begin
      ANumbers := AData.NumericList;
      if ANumbers.Count = 0 then
        Sender.SetError(ecNA)
      else
      begin
        AOccurrenceNumbers := TcxObjectList.Create;
        try
          AOccurrenceNumbers.Add(TdxOccurrence.Create(ANumbers[0], 1));
          AMaxCount := 1;
          for I := 1 to ANumbers.Count - 1 do
          begin
            AOccurrenceItem := GetOccurrenceItem(ANumbers[I]);
            if AOccurrenceItem <> nil then
            begin
              Inc(AOccurrenceItem.Count);
              AMaxCount := Max(AMaxCount, AOccurrenceItem.Count);
            end
            else
              AOccurrenceNumbers.Add(TdxOccurrence.Create(ANumbers[I], 1));
          end;

          if AMaxCount = 1 then
            Sender.SetError(ecNA)
          else
          begin
            AResultArray := TdxSpreadSheetFormulaArrayToken.Create;
            if AAsArray then
              Sender.AddTemporary(AResultArray);
            for I := 0 to AOccurrenceNumbers.Count - 1 do
            begin
              AOccurrenceItem := TdxOccurrence(AOccurrenceNumbers[I]);
              if AOccurrenceItem.Count = AMaxCount then
                if not AAsArray then
                begin
                  Sender.AddValue(AOccurrenceItem.Number);
                  Break;
                end
                else
                begin
                  if AResultArray.HasChildren then
                    TdxSpreadSheetFormulaToken.AddChild(AResultArray, TdxSpreadSheetFormulaArrayRowSeparator.Create());
                  AParentToken := TdxSpreadSheetFormulaToken.Create;
                  TdxSpreadSheetFormulaToken.AddChild(AParentToken, TdxSpreadSheetFormulaFloatValueToken.Create(AOccurrenceItem.Number));
                  TdxSpreadSheetFormulaToken.AddChild(AResultArray, AParentToken);
              end;
            end;
            if not AAsArray then
              AResultArray.Free;
          end;

        finally
          FreeAndNil(AOccurrenceNumbers);
        end;
      end;
    end;
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnMode_Mult(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxMode_Custom(Sender, AParams, True);
end;

procedure fnMode_Sngl(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxMode_Custom(Sender, AParams, False);
end;

procedure fnNorm_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X, AMean, AStdDev, ACumulative: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) and Sender.ExtractNumericParameter(AMean, AParams, 1) and
     Sender.ExtractNumericParameter(AStdDev, AParams, 2) and Sender.ExtractNumericParameter(ACumulative, AParams, 3) then
    if AStdDev <= 0 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetNormDist(X, AMean, AStdDev, ACumulative <> 0));
end;

procedure fnNorm_Inv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AProbability, AMean, AStdDev: Variant;
begin
  if Sender.ExtractNumericParameter(AProbability, AParams) and Sender.ExtractNumericParameter(AMean, AParams, 1) and
     Sender.ExtractNumericParameter(AStdDev, AParams, 2) then
    if not((AProbability > 0) and (AProbability < 1) and (AStdDev > 0)) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(AMean + AStdDev * dxGetStandartNormInv(AProbability));
end;

procedure fnNormSDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) then
    Sender.AddValue(dxGetStandartNormDist(X));
end;

procedure fnNorm_S_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X, ACumulative: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) and Sender.ExtractNumericParameter(ACumulative, AParams, 1) then
    Sender.AddValue(dxGetNormDist(X, 0, 1, ACumulative <> 0));
end;

procedure fnNorm_S_Inv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) then
    if not((X > 0) and (X < 1)) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetStandartNormInv(X));
end;

procedure dxCalculatePearsonRatioItems(const AArrayX, AArrayY: Variant; const AArraysDimension: TdxSpreadSheetFormulaTokenDimension;
  const AAvgX, AAvgY: Extended; var ANominator, ADenominator: Extended);
var
  I, J: Integer;
  ADenominatorX, ADenominatorY: Extended;
begin
  ANominator := 0;
  ADenominatorX := 0;
  ADenominatorY := 0;
  for I := 0 to AArraysDimension.RowCount - 1 do
    for J := 0 to AArraysDimension.ColumnCount - 1 do
      if not VarIsNull(AArrayX[I, J]) then
      begin
        ANominator := ANominator + (AArrayX[I, J] - AAvgX) * (AArrayY[I, J] - AAvgY);
        ADenominatorX := ADenominatorX + Sqr((AArrayX[I, J] - AAvgX));
        ADenominatorY := ADenominatorY + Sqr((AArrayY[I, J] - AAvgY));
      end;
  ADenominator := Sqrt(ADenominatorX * ADenominatorY);
end;

function dxIsPearsonCalculated(Sender: TdxSpreadSheetFormulaResult; const AParamsY, AParamsX: TdxSpreadSheetFormulaToken;
  var APearson: Extended): Boolean;
var
  AAvgX, AAvgY: Extended;
begin
  dxRelationshipBetweenTwoDatasets(Sender, AParamsY, AParamsX, @dxCalculatePearsonRatioItems,
    1, AAvgX, AAvgY, APearson);
  Result := Sender.Validate;
end;

procedure fnPearson(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  APearson: Extended;
begin
  if dxIsPearsonCalculated(Sender, AParams.FirstChild, AParams.Next.FirstChild, APearson) then
    Sender.AddValue(APearson);
end;

function dxGetPercentileCore(ANumerics: TList<Double>; ALevel: Extended): Extended;
var
  AAspect: Extended;
  AIndex: Integer;
begin
  AAspect := ALevel * (ANumerics.Count - 1);
  AIndex := Floor(AAspect);
  AAspect := AAspect - AIndex;
  Result := ANumerics[AIndex] * (1 - AAspect) + ANumerics[AIndex + 1] * AAspect;
end;

function dxGetPercentile_Exc(AData: TdxSpreadSheetEnumValues; ALevel: Extended; out AResult: Extended): Boolean;
var
  ALastIndex: Integer;
  ANumerics: TList<Double>;
begin
  Result := False;
  ANumerics := AData.NumericList;
  ANumerics.Sort;
  AData.AddToNumericList(ANumerics[ANumerics.Count - 1] + 1, False);
  AData.InsertIntoNumericList(0, ANumerics[0] - 1);
  ALastIndex := ANumerics.Count - 1;
  if (ALevel < 1 / ALastIndex) or (ALevel > ((ALastIndex - 1) / ALastIndex)) then
    Exit
  else
  begin
    AResult := dxGetPercentileCore(ANumerics, ALevel);
    Result := True;
  end;
end;

function dxGetPercentile_Inc(ANumerics: TList<Double>; ALevel: Extended): Extended;
var
  ACount: Integer;
begin
  ANumerics.Sort;
  ACount := ANumerics.Count;
  if (ALevel = 0) or (ACount = 1) then
    Result := ANumerics[0]
  else
    if ALevel = 1 then
      Result := ANumerics[ACount - 1]
    else
      Result := dxGetPercentileCore(ANumerics, ALevel);
end;

procedure fnPercentile_Exc(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AData: TdxSpreadSheetEnumValues;
  ALevel: Variant;
  AResult: Extended;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, False, False, False);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    if (AData.PopulateNumericList(Sender, AParams) = ecNone) and Sender.ExtractNumericParameter(ALevel, AParams, 1)then
    begin
      if (AData.NumericCount = 0) or (ALevel <= 0) or (ALevel >= 1) then
        Sender.SetError(ecNUM)
      else
        if dxGetPercentile_Exc(AData, ALevel, AResult) then
          Sender.AddValue(AResult)
        else
          Sender.SetError(ecNUM);
    end;
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnPercentile_Inc(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AData: TdxSpreadSheetEnumValues;
  ALevel: Variant;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, False, False, False);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    if (AData.PopulateNumericList(Sender, AParams) = ecNone) and Sender.ExtractNumericParameter(ALevel, AParams, 1)then
      if (AData.NumericCount = 0) or (ALevel < 0) or (ALevel > 1) then
        Sender.SetError(ecNUM)
      else
        Sender.AddValue(dxGetPercentile_Inc(AData.NumericList, ALevel));
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnPermut(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANumber, ANumberChosen: Variant;
begin
  if Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameter(ANumberChosen, AParams, 1) then
    if (ANumber <= 0) or (ANumberChosen < 0) or (ANumber < ANumberChosen) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetPermut(Trunc(ANumber), Trunc(ANumberChosen)));
end;

procedure fnPoisson_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X, ALambda, ACumulative: Variant;
  AResult: Extended;
  K: Word;
begin
  if Sender.ExtractNumericParameter(X, AParams) and Sender.ExtractNumericParameter(ALambda, AParams, 1) and
     Sender.ExtractNumericParameter(ACumulative, AParams, 2) then
    if (X < 0) or (ALambda < 0) then
      Sender.SetError(ecNUM)
    else
    begin
      X := Trunc(X);
      if ACumulative = 0 then
        Sender.AddValue(dxGetPoisson(X, ALambda))
      else
      begin
        AResult := 0;
        for K := 0 to X do
          AResult := AResult + dxGetPoisson(K, ALambda);
        Sender.AddValue(AResult);
      end;
    end;
end;

procedure dxCustomProduct(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal: Boolean);
var
  AData: TdxSpreadSheetEnumValues;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, False, AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, 1, ADataInfo);
  try
    dxSpreadSheetCalculateForEachParam(Sender, AParams, AData, @cbProduct);
    if not AData.Validate then
      Sender.SetError(AData.ErrorCode)
    else
      if AData.NumericCount > 0 then
        Sender.AddValue(AData.ResultValue)
      else
        Sender.AddValue(0);
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnProduct(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomProduct(Sender, AParams, False, False, False);
end;

procedure fnQuartile_Exc(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
const
  ALevels: array[1..3] of Real = (0.25, 0.5, 0.75);
var
  AData: TdxSpreadSheetEnumValues;
  AQuart: Variant;
  AIntQuart: Integer;
  AResult: Extended;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, False, False, False);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    if (AData.PopulateNumericList(Sender, AParams) = ecNone) and Sender.ExtractNumericParameter(AQuart, AParams, 1)then
    begin
      AIntQuart := Trunc(AQuart);
      if (AData.NumericCount = 0) or (AQuart < 1) or (AIntQuart > 4) then
        Sender.SetError(ecNUM)
      else
        if dxGetPercentile_Exc(AData, ALevels[AIntQuart], AResult) then
          Sender.AddValue(AResult)
        else
          Sender.SetError(ecNUM);
    end;
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnQuartile_Inc(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
const
  ALevels: array[0..4] of Real = (0, 0.25, 0.5, 0.75, 1);
var
  AData: TdxSpreadSheetEnumValues;
  AQuart: Variant;
  AIntQuart: Integer;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, False, False, False);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    if (AData.PopulateNumericList(Sender, AParams) = ecNone) and Sender.ExtractNumericParameter(AQuart, AParams, 1)then
    begin
      AIntQuart := Trunc(AQuart);
      if (AData.NumericCount = 0) or (AQuart < 0) or (AIntQuart > 4) then
        Sender.SetError(ecNUM)
      else
        Sender.AddValue(dxGetPercentile_Inc(AData.NumericList, ALevels[AIntQuart]));
    end;
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnCustomRank(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken; const AIsAvg: Boolean);

  function GetRangeAvg(const AOrderedList: TList<Double>; const ANumber: Variant; const AStartRange: Integer): Extended;
  var
    I, AEndRange: Integer;
  begin
    AEndRange := AStartRange;
    for I := AStartRange + 1 to AOrderedList.Count do
      if AOrderedList[I - 1] <> ANumber then
        Break
      else
        AEndRange := I;
    Result := (AStartRange + AEndRange) / 2;
  end;

var
  ANumber, AOrderAsc: Variant;
  AData: TdxSpreadSheetEnumValues;
  ARef: TList<Double>;
  AResult, I: Integer;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  if not (Sender.ExtractNumericParameter(ANumber, AParams) and Sender.ExtractNumericParameterDef(AOrderAsc, 0, 0, AParams, 2)) then
    Exit;
  ADataInfo.Init(AOrderAsc <> 0, False, False, False);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    if AData.PopulateNumericList(Sender, AParams.Next) = ecNone then
    begin
      ARef := AData.NumericList;
      if ARef.Count = 0 then
        Sender.SetError(ecNA)
      else
      begin
        ARef.Sort;
        AResult := -1;
        for I := 1 to ARef.Count do
          if ARef[I - 1] = ANumber then
          begin
            AResult := I;
            Break;
          end;
        if AResult = -1 then
          Sender.SetError(ecNA)
        else
          if AIsAvg then
            Sender.AddValue(GetRangeAvg(ARef, ANumber, AResult))
          else
            Sender.AddValue(AResult);
      end;
    end;
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnRank_AVG(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnCustomRank(Sender, AParams, True);
end;

procedure fnRank_EQ(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnCustomRank(Sender, AParams, False);
end;

procedure fnRSQ(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  APearson: Extended;
begin
  if dxIsPearsonCalculated(Sender, AParams.FirstChild, AParams.Next.FirstChild, APearson) then
    Sender.AddValue(Sqr(APearson));
end;

function dxCustomSkew(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken; AFunc: TdxCustomVariance): Double;
var
  AData: TdxSpreadSheetEnumValues;
  AAverage: Extended;
  ACount: Integer;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, True, False, True);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  Result := 0;
  try
    dxCalculateAverage(Sender, AParams, AData, False, AAverage, ACount);
    if Sender.Validate then
    begin
      if ACount > 2 then
        Result := AFunc(AData.NumericList, AAverage)
      else
        Sender.SetError(ecDivByZero);
    end;
  finally
    FreeAndNil(AData);
  end;
end;

function dxGetSkew(ANumerics: TList<Double>; AAverage: Double): Double;
var
  I, N: Integer;
  X, AStDev: Double;
begin
  Result := 0;
  N := ANumerics.Count;
  for I := 0 to N - 1 do
  begin
    X := ANumerics[I] - AAverage;
    Result := Result + X * X * X;
  end;
  AStDev := Sqrt(dxGetSumOfSquaresOfDeviations(ANumerics, AAverage) / (N - 1));
  Result := N * Result / (N - 1) / (N - 2) / AStDev / AStDev / AStDev;
end;


procedure fnSkew(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AResult: Extended;
begin
  AResult := dxCustomSkew(Sender, AParams, @dxGetSkew);
  if Sender.Validate then
    Sender.AddValue(AResult);
end;

function dxGetSkew_P(ANumerics: TList<Double>; AAverage: Double): Double;
var
  I, N: Integer;
  X, AStDevP: Double;
begin
  Result := 0;
  N := ANumerics.Count;
  for I := 0 to N - 1 do
  begin
    X := ANumerics[I] - AAverage;
    Result := Result + X * X * X;
  end;
  AStDevP := Sqrt(dxGetSumOfSquaresOfDeviations(ANumerics, AAverage) / N );
  Result := Result / N / AStDevP / AStDevP / AStDevP;
end;


procedure fnSkew_P(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AResult: Extended;
begin
  AResult := dxCustomSkew(Sender, AParams, @dxGetSkew_P);
  if Sender.Validate then
    Sender.AddValue(AResult);
end;

procedure fnSlope(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AAvgX, AAvgY, ASlope: Extended;
begin
  dxRelationshipBetweenTwoDatasets(Sender, AParams.FirstChild, AParams.Next.FirstChild, @dxCalculateSlopeRatioItems,
    1, AAvgX, AAvgY, ASlope);
  if Sender.Validate then
    Sender.AddValue(ASlope);
end;

procedure fnSmall(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxGetSerialValue(Sender, AParams, True);
end;

procedure fnStandardize(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X, AMean, AStDev: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) and Sender.ExtractNumericParameter(AMean, AParams, 1) and
     Sender.ExtractNumericParameter(AStDev, AParams, 2) then
    if AStDev <= 0 then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue((X - AMean) / AStDev);
end;

function dxGetCustomVar(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AIsStandart, AWithText, AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal: Boolean): Extended;
var
  AData: TdxSpreadSheetEnumValues;
  AAverage: Extended;
  ACount: Integer;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, True, AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  Result := 0;
  try
    dxCalculateAverage(Sender, AParams, AData, AWithText, AAverage, ACount);
    if Sender.Validate then
    begin
      Dec(ACount, Integer(AIsStandart));
      if ACount > 0 then
        Result := dxGetSumOfSquaresOfDeviations(AData.NumericList, AAverage) / ACount
      else
        Sender.SetError(ecDivByZero);
    end;
  finally
    FreeAndNil(AData);
  end;
end;

procedure dxCustomVar(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  const AIsStandart, AWithText, AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal: Boolean);
var
  AResult: Extended;
begin
  AResult := dxGetCustomVar(Sender, AParams, AIsStandart, AWithText, AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal);
  if Sender.Validate then
    Sender.AddValue(AResult);
end;

procedure dxCustomStDev(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  const AIsStandart, AWithText, AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal: Boolean);
var
  AResult: Extended;
begin
  AResult := dxGetCustomVar(Sender, AParams, AIsStandart, AWithText, AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal);
  if Sender.Validate then
    Sender.AddValue(Sqrt(AResult));
end;

procedure fnStDev_S(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomStDev(Sender, AParams, True, False, False, False, False);
end;

procedure fnStDev_P(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomStDev(Sender, AParams, False, False, False, False, False);
end;

procedure fnStDevA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomStDev(Sender, AParams, True, True, False, False, False);
end;

procedure fnStDevPA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomStDev(Sender, AParams, False, True, False, False, False);
end;

procedure dxCalculateSTEYXRatioItems(const AArrayX, AArrayY: Variant; const AArraysDimension: TdxSpreadSheetFormulaTokenDimension;
  const AAvgX, AAvgY: Extended; var ANominator, ADenominator: Extended);
var
  I, J, N: Integer;
  ANominator1, ANominator2: Extended;
begin
  ANominator1 := 0;
  ANominator2 := 0;
  ADenominator := 0;
  N := 0;
  for I := 0 to AArraysDimension.RowCount - 1 do
    for J := 0 to AArraysDimension.ColumnCount - 1 do
      if not VarIsNull(AArrayX[I, J]) then
      begin
        ANominator1 := ANominator1 + Sqr((AArrayY[I, J] - AAvgY));
        ANominator2 := ANominator2 + (AArrayX[I, J] - AAvgX) * (AArrayY[I, J] - AAvgY);
        Inc(N);
        ADenominator := ADenominator + Sqr((AArrayX[I, J] - AAvgX));
      end;
  ANominator := ANominator1 * ADenominator - Sqr(ANominator2);
  ADenominator := (Max(N, 2) - 2) * ADenominator;
end;

procedure fnSTEYX(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AAvgX, AAvgY, ASqrOf_STEYX: Extended;
begin
  dxRelationshipBetweenTwoDatasets(Sender, AParams.FirstChild, AParams.Next.FirstChild, @dxCalculateSTEYXRatioItems,
    2, AAvgX, AAvgY, ASqrOf_STEYX);
  if Sender.Validate then
    Sender.AddValue(Sqrt(ASqrOf_STEYX));
end;

procedure dxSumCore(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  AProc: TdxSpreadSheetForEachCallBack; AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal: Boolean);
var
  AData: TdxSpreadSheetEnumValues;
  ADataInfo: TdxSpreadSheetEnumValuesProcessingInfo;
begin
  ADataInfo.Init(True, False, AErrorIfNonReferenceToken, AIgnoreHiddenRows, AForSubTotal);
  AData := TdxSpreadSheetEnumValues.Create(Sender.FormatSettings, ADataInfo);
  try
    dxSpreadSheetCalculateForEachParam(Sender, AParams, AData, AProc);
    if AData.Validate then
      Sender.AddValue(AData.ResultValue)
    else
      Sender.SetError(AData.ErrorCode);
  finally
    FreeAndNil(AData);
  end;
end;

procedure fnSubTotal(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  ANum: Variant;
  AIntNum: Integer;
begin
  if not Sender.ExtractNumericParameter(ANum, AParams) then
    Exit;
  AIntNum := Trunc(ANum);
  if not (AIntNum in [1..11, 101..111]) then
    Sender.SetError(ecValue)
  else
    case AIntNum of
      1, 101: dxCustomAverage(Sender, AParams.Next, False, True, AIntNum > 100, True);
      2, 102: dxCustomCount(Sender, AParams.Next, True, AIntNum > 100, True);
      3, 103: dxCustomCountA(Sender, AParams.Next, True, AIntNum > 100, True);
      4, 104: dxCustomMinMax(Sender, AParams.Next, @cbMax, True, AIntNum > 100, True);
      5, 105: dxCustomMinMax(Sender, AParams.Next, @cbMin, True, AIntNum > 100, True);
      6, 106: dxCustomProduct(Sender, AParams.Next, True, AIntNum > 100, True);
      7, 107: dxCustomStDev(Sender, AParams.Next, True, False, True, AIntNum > 100, True);
      8, 108: dxCustomStDev(Sender, AParams.Next, False, False, True, AIntNum > 100, True);
      9, 109: dxSumCore(Sender, AParams.Next, @cbSum, True, AIntNum > 100, True);
      10, 110: dxCustomVar(Sender, AParams.Next, True, False, True, AIntNum > 100, True);
      11, 111: dxCustomVar(Sender, AParams.Next, False, False, True, AIntNum > 100, True);
    end;
end;

procedure fnSum(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSumCore(Sender, AParams, @cbSum, False, False, False);
end;

procedure dxCustomSumIF(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken; const AIFS: Boolean);
var
  AData: TdxSpreadSheetEnumValuesWithCondition;
begin
   AData := TdxSpreadSheetEnumValuesWithCondition.Create;
   try
     dxCustomSumIfCalculate(Sender, AParams, AData, AIFS);
     if Sender.Validate then
       Sender.SetError(AData.ErrorCode);
     if Sender.Validate then
       Sender.AddValue(AData.ResultValue);
  finally
    AData.Free;
  end;
end;

procedure fnSumIF(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomSumIF(Sender, AParams, False);
end;

procedure fnSumIFS(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomSumIF(Sender, AParams, True);
end;

procedure fnSumProduct(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

  function CheckParamsDimension(var ARowCount, AColumnCount: Integer): TdxSpreadSheetFormulaErrorCode;
  var
    ADimension, ANextDimension: TdxSpreadSheetFormulaTokenDimension;
    AParam: TdxSpreadSheetFormulaToken;
    AErrorCode: TdxSpreadSheetFormulaErrorCode;
  begin
    Result := ecNone;
    ARowCount := 0;
    AColumnCount := 0;
    AParam := AParams;
    ADimension := AParam.FirstChild.GetDimension(Result);
    while (AParam.Next <> nil) and (Result = ecNone) do
    begin
      AParam := AParam.Next;
      ANextDimension := AParam.FirstChild.GetDimension(AErrorCode);
      if AErrorCode <> ecNone then
        Result := AErrorCode
      else
        if ADimension <> ANextDimension then
          Result := ecValue;
    end;
    if Result = ecNone then
    begin
      ARowCount := ADimension.RowCount;
      AColumnCount := ADimension.ColumnCount;
    end;
  end;

var
  AParamsCount: Integer;
  AParam: TdxSpreadSheetFormulaToken;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  ARow, AColumn, ARowCount, AColumnCount: Integer;
  AValue: Variant;
  AResult, ASubResult: Extended;
  AIsValidSubResult, AIsPresentValidSubResult: Boolean;
begin
  AParamsCount := Sender.GetParamsCount(AParams);
  if AParamsCount = 0 then
    Sender.SetError(ecValue)
  else
  begin
    AErrorCode := CheckParamsDimension(ARowCount, AColumnCount);
    if AErrorCode = ecNone then
    begin
      AResult := 0;
      AIsPresentValidSubResult := False;
      for ARow := 0 to ARowCount - 1 do
      begin
        for AColumn := 0 to AColumnCount - 1 do
        begin
          ASubResult := 1;
          AIsValidSubResult := True;
          AParam := AParams;
          while (AParam <> nil) and (AErrorCode = ecNone) and (ASubResult <> 0) do
          begin
            TdxTokenAccess(AParam.FirstChild).GetValueAsArrayItem(ARow, AColumn, AValue, AErrorCode);
            if AErrorCode = ecNone then
              if dxIsNumberOrDateTime(AValue) then
                ASubResult := ASubResult * AValue
              else
              begin
                ASubResult := 0;
                if not((AParam.FirstChild is TdxSpreadSheetFormulaArrayToken) and dxIsLogical(AValue)) then
                  AIsValidSubResult := False;
              end;
            AParam := AParam.Next;
          end;
          if AErrorCode <> ecNone then
            Break;
          AResult := AResult + ASubResult;
          if not AIsPresentValidSubResult  then
            AIsPresentValidSubResult := AIsValidSubResult;
        end;
        if AErrorCode <> ecNone then
          Break;
      end;
      if not AIsPresentValidSubResult and (AErrorCode = ecNone) then
        AErrorCode := ecValue;
      if AErrorCode <> ecNone then
        Sender.SetError(AErrorCode)
      else
        Sender.AddValue(AResult);
    end;
  end;
end;

procedure fnSumSQ(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSumCore(Sender, AParams, @cbSumSQ, False, False, False);
end;

type
  TdxFunctionKindOfSumXY = (fkxyX2MY2, fkxyX2PY2, fkxyXMY2);

function GetCompositeIndex(const ADimension: TdxSpreadSheetFormulaTokenDimension;
  const ARow, AColumn: Integer): Integer; inline;
begin
  Result := ARow * ADimension.ColumnCount + AColumn;
end;

procedure ExtractIndexes(const ACompositeIndex: Integer;
  const ADimension: TdxSpreadSheetFormulaTokenDimension; var ARow, AColumn: Integer); inline;
begin
  ARow := ACompositeIndex div ADimension.ColumnCount;
  AColumn := ACompositeIndex - ARow * ADimension.ColumnCount;
end;

procedure dxSumXY(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken;
  const AFunctionKind: TdxFunctionKindOfSumXY);

  function CheckDimension(AArrayX, AArrayY: TdxSpreadSheetFormulaToken;
    var ADimensionX, ADimensionY: TdxSpreadSheetFormulaTokenDimension): TdxSpreadSheetFormulaErrorCode;
  begin
    ADimensionX := AArrayX.GetDimension(Result);
    if Result = ecNone then
      ADimensionY := AArrayY.GetDimension(Result);
    if (Result = ecNone) and (ADimensionX.Count <> ADimensionY.Count) then
      Result := ecNA;
  end;

  function CheckNonNumberValue(AArray: TdxSpreadSheetFormulaToken; const AValue: Variant): Boolean;
  begin
    Result := (AArray is TdxSpreadSheetFormulaArrayToken) and dxIsLogical(AValue);
  end;

  function CheckValue(const AValue: Variant): Boolean;
  begin
    Result := not VarIsEmpty(AValue) and dxIsNumberOrDateTime(AValue);
  end;

var
  AArrayX, AArrayY: TdxSpreadSheetFormulaToken;
  ADimensionX, ADimensionY: TdxSpreadSheetFormulaTokenDimension;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  ARow, AColumn, ARowY, AColumnY: Integer;
  AResult, ASubResult: Extended;
  X, Y: Variant;
  AIsValidSubResult, AIsPresentValidSubResult: Boolean;
begin
  if (AParams = nil) or (AParams.Next = nil) then
    Sender.SetError(ecNA)
  else
  begin
    AArrayX := AParams.FirstChild;
    AArrayY := AParams.Next.FirstChild;
    AErrorCode := CheckDimension(AArrayX, AArrayY, ADimensionX, ADimensionY);
    if AErrorCode <> ecNone then
      Sender.SetError(AErrorCode)
    else
    begin
      AResult := 0;
      AIsPresentValidSubResult := False;
      for ARow := 0 to ADimensionX.RowCount - 1 do
      begin
        for AColumn := 0 to ADimensionX.ColumnCount - 1 do
        begin
          ASubResult := 0;
          AIsValidSubResult := True;

          TdxTokenAccess(AArrayX).GetValueAsArrayItem(ARow, AColumn, X, AErrorCode);
          if AErrorCode = ecNone then
          begin
            if CheckValue(X) then
            begin
              ExtractIndexes(GetCompositeIndex(ADimensionX, ARow, AColumn), ADimensionY, ARowY, AColumnY);
              TdxTokenAccess(AArrayY).GetValueAsArrayItem(ARowY, AColumnY, Y, AErrorCode);
              if CheckValue(Y) then
                case AFunctionKind of
                  fkxyX2MY2:
                    ASubResult := X * X - Y * Y;
                  fkxyX2PY2:
                    ASubResult := X * X + Y * Y;
                  fkxyXMY2:
                    ASubResult := (X - Y) * (X - Y);
                end
              else
                AIsValidSubResult := CheckNonNumberValue(AArrayY, Y);
            end
            else
              AIsValidSubResult := CheckNonNumberValue(AArrayX, X);
          end;

          if AErrorCode <> ecNone then
            Break;
          AResult := AResult + ASubResult;
          if not AIsPresentValidSubResult  then
            AIsPresentValidSubResult := AIsValidSubResult;
        end;
        if AErrorCode <> ecNone then
          Break;
      end;

      if not AIsPresentValidSubResult and (AErrorCode = ecNone) then
        AErrorCode := ecValue;
      if AErrorCode <> ecNone then
        Sender.SetError(AErrorCode)
      else
        Sender.AddValue(AResult);
    end;
  end;
end;

procedure fnSumX2MY2(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSumXY(Sender, AParams, fkxyX2MY2);
end;

procedure fnSumX2PY2(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSumXY(Sender, AParams, fkxyX2PY2);
end;

procedure fnSumXMY2(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSumXY(Sender, AParams, fkxyXMY2);
end;

function dxGetTDist(X: Extended; ADegFreedom, AIntTails: Integer): Extended;
begin
  Result := AIntTails * (1 - dxGetT_Dist(X, ADegFreedom, True));
end;

procedure fnTDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X, ADegFreedom, ATails: Variant;
  AIntTails: Integer;
begin
  if Sender.ExtractNumericParameter(X, AParams) and Sender.ExtractNumericParameter(ADegFreedom, AParams, 1) and
     Sender.ExtractNumericParameter(ATails, AParams, 2) then
  begin
    AIntTails := Trunc(ATails);
    if (X < 0) or (ADegFreedom < 1) or (ADegFreedom > 10000000000) or not(AIntTails in [1..2]) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetTDist(X, Trunc(ADegFreedom), AIntTails));
  end;
end;

procedure fnT_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X, ADegFreedom, ACumulative: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) and Sender.ExtractNumericParameter(ADegFreedom, AParams, 1) and
     Sender.ExtractNumericParameter(ACumulative, AParams, 2) then
    if ADegFreedom > 10000000000 then
      Sender.SetError(ecNUM)
    else
      if ADegFreedom < 1 then
        if ACumulative <> 0 then
          Sender.SetError(ecNUM)
        else
          Sender.SetError(ecDivByZero)
      else
        Sender.AddValue(dxGetT_Dist(X, Trunc(ADegFreedom), ACumulative <> 0));
end;

procedure fnT_Dist_2T(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X, ADegFreedom: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) and Sender.ExtractNumericParameter(ADegFreedom, AParams, 1) then
    if (X < 0) or (ADegFreedom < 1) or (ADegFreedom > 10000000000) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetT_Dist_2T(X, Trunc(ADegFreedom)));
end;

procedure fnT_Dist_RT(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X, ADegFreedom: Variant;
begin
  if Sender.ExtractNumericParameter(X, AParams) and Sender.ExtractNumericParameter(ADegFreedom, AParams, 1) then
    if (ADegFreedom < 1) or (ADegFreedom > 10000000000) then
      Sender.SetError(ecNUM)
    else
      Sender.AddValue(dxGetT_Dist_RT(X, Trunc(ADegFreedom)));
end;

function dxGetTInv(AProbability: Extended; ADegFreedom: Integer): Extended;
begin
  Result := dxGetT_Inv(1 - AProbability / 2, ADegFreedom);
end;

procedure fnTInv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AProbability, ADegFreedom: Variant;
begin
  if Sender.ExtractNumericParameter(AProbability, AParams) and Sender.ExtractNumericParameter(ADegFreedom, AParams, 1) then
    if (AProbability = 0) and (ADegFreedom = 1) then
      Sender.SetError(ecDivByZero)
    else
      if (AProbability <= 0) or (AProbability >= 2) or (ADegFreedom < 1) or (ADegFreedom > 10000000000) then
        Sender.SetError(ecNUM)
      else
        Sender.AddValue(dxGetTInv(AProbability, Trunc(ADegFreedom)));
end;

procedure fnT_Inv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AProbability, ADegFreedom: Variant;
begin
  if Sender.ExtractNumericParameter(AProbability, AParams) and Sender.ExtractNumericParameter(ADegFreedom, AParams, 1) then
    if (AProbability = 0) and (ADegFreedom = 1) then
      Sender.SetError(ecDivByZero)
    else
      if (AProbability <= 0) or (AProbability >= 1) or (ADegFreedom < 1) or (ADegFreedom > 10000000000) then
        Sender.SetError(ecNUM)
      else
        Sender.AddValue(dxGetT_Inv(AProbability, Trunc(ADegFreedom)));
end;

procedure fnT_Inv_2T(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  AProbability, ADegFreedom: Variant;
begin
  if Sender.ExtractNumericParameter(AProbability, AParams) and Sender.ExtractNumericParameter(ADegFreedom, AParams, 1) then
    if (AProbability = 0) and (ADegFreedom = 1) then
      Sender.SetError(ecDivByZero)
    else
      if (AProbability <= 0) or (AProbability >= 2) or (ADegFreedom < 1) or (ADegFreedom > 10000000000) then
        Sender.SetError(ecNUM)
      else
        Sender.AddValue(dxGetT_Inv_2T(AProbability, Trunc(ADegFreedom)));
end;

procedure fnVar_P(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomVar(Sender, AParams, False, False, False, False, False);
end;

procedure fnVar_S(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomVar(Sender, AParams, True, False, False, False, False);
end;

procedure fnVarA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomVar(Sender, AParams, True, True, False, False, False);
end;

procedure fnVarPA(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxCustomVar(Sender, AParams, False, True, False, False, False);
end;

procedure fnWeibull_Dist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  X, AAlfa, ABeta, ACumulative: Variant;
  AResult: Extended;
begin
  if Sender.ExtractNumericParameter(X, AParams) and Sender.ExtractNumericParameter(AAlfa, AParams, 1) and
     Sender.ExtractNumericParameter(ABeta, AParams, 2) and Sender.ExtractNumericParameter(ACumulative, AParams, 3) then
    if (X < 0) or (AAlfa <= 0) or (ABeta <= 0) then
      Sender.SetError(ecNUM)
    else
      if AAlfa = 1 then
        Sender.AddValue(dxGetExponDist(X, 1/ABeta, ACumulative <> 0))
      else
      begin
        AResult := Exp(-Power(X / ABeta, AAlfa));
        if ACumulative <> 0 then
          Sender.AddValue(1 - AResult)
        else
          Sender.AddValue(AAlfa / Power(ABeta, AAlfa) * Power(X, AAlfa - 1) * AResult);
      end;
end;

procedure fpiAveDev(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiAverage(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiAverageA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiAverageIF(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkNonRequiredArray;
end;

procedure fpiAverageIFS(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkArray;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkNonRequiredUnlimited;
end;

procedure fpiBeta_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(6, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkValue;
  AParamKind[4] := fpkNonRequiredValue;
  AParamKind[5] := fpkNonRequiredValue;
end;

procedure fpiBeta_Inv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(5, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkNonRequiredValue;
  AParamKind[4] := fpkNonRequiredValue;
end;

procedure fpiBinom_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkValue;
end;

procedure fpiBinom_Dist_Range(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkNonRequiredValue;
end;

procedure fpiChiSQ_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
end;

procedure fpiChiSQ_Dist_RT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiChiSQ_Inv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiChiSQ_Inv_RT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiCorrel(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkArray;
end;

procedure fpiCount(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiCountA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiCountBlank(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiCountIF(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkValue;
end;

procedure fpiCountIFS(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkNonRequiredUnlimited;
end;

procedure fpiCovariance_P(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkArray;
end;

procedure fpiCovariance_S(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkArray;
end;

procedure fpiDevSQ(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiExpon_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
end;

procedure fpiForecast(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkArray;
  AParamKind[2] := fpkArray;
end;

procedure fpiGamma(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiGamma_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkValue;
end;

procedure fpiGamma_Inv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
end;

procedure fpiGammaLn(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiGammaLn_Precise(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiGauss(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiGeomean(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiHypgeom_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(5, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkValue;
  AParamKind[4] := fpkValue;
end;

procedure fpiIntercept(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkArray;
end;

procedure fpiLarge(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkValue;
end;

procedure fpiMax(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiMaxA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiMedian(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiMin(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiMinA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiMode_Mult(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiMode_Sngl(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiNorm_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkValue;
end;

procedure fpiNorm_Inv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
end;

procedure fpiNorm_S_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiNorm_S_Inv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiPearson(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkArray;
end;

procedure fpiPercentile_Exc(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkValue;
end;

procedure fpiPercentile_Inc(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkValue;
end;

procedure fpiPermut(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiPoisson_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
end;

procedure fpiQuartile_Exc(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkValue;
end;

procedure fpiQuartile_Inc(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkValue;
end;

procedure fpiRank_Avg(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkArray;
  AParamKind[2] := fpkNonRequiredValue;
end;

procedure fpiRank_Eq(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkArray;
  AParamKind[2] := fpkNonRequiredValue;
end;

procedure fpiRSQ(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkArray;
end;

procedure fpiSkew(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiSkew_P(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiSlope(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkArray;
end;

procedure fpiSmall(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkValue;
end;

procedure fpiStandardize(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
end;

procedure fpiStDev_S(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiStDev_P(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiStDevA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiStDevPA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiSTEYX(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkArray;
end;

procedure fpiSubTotal(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkUnlimited;
end;

procedure fpiT_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
end;

procedure fpiT_Dist_2T(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiT_Dist_RT(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiT_Inv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiT_Inv_2T(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiVar_P(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiVar_S(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiVarA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiVarPA(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiWeibull_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkValue;
end;

{ RegisterFunctions }

procedure RegisterFunctions(ARepository: TdxSpreadSheetFunctionsRepository);
begin
  ARepository.Add(@sfnAveDev, fnAveDev, fpiAveDev, frkNonArrayValue, 269, ftStatistical, @sfnAveDevDescription);
  ARepository.Add(@sfnAverage, fnAverage, fpiAverage, frkNonArrayValue, 5, ftStatistical, @sfnAverageDescription);
  ARepository.Add(@sfnAverageA, fnAverageA, fpiAverageA, frkNonArrayValue, 361, ftStatistical, @sfnAverageADescription);
  ARepository.Add(@sfnAverageIF, fnAverageIF, fpiAverageIF, frkNonArrayValue, 255, ftStatistical, @sfnAverageIFDescription);
  ARepository.Add(@sfnAverageIFS, fnAverageIFS, fpiAverageIFS, frkNonArrayValue, 255, ftStatistical, @sfnAverageIFSDescription);
  ARepository.Add(@sfnBeta_Dist, fnBeta_Dist, fpiBeta_Dist, frkValue, 255, ftStatistical, @sfnBeta_DistDescription, 1);
  ARepository.Add(@sfnBeta_Inv, fnBeta_Inv, fpiBeta_Inv, frkValue, 255, ftStatistical, @sfnBeta_InvDescription, 1);
  ARepository.Add(@sfnBinom_Dist, fnBinom_Dist, fpiBinom_Dist, frkValue, 255, ftStatistical, @sfnBinom_DistDescription, 1);
  ARepository.Add(@sfnBinom_Dist_Range, fnBinom_Dist_Range, fpiBinom_Dist_Range, frkValue, 255, ftStatistical, @sfnBinom_Dist_RangeDescription, 1);
  ARepository.Add(@sfnBinom_Inv, nil, nil, frkValue, 255, ftStatistical, @sfnBinom_InvDescription, 1);
  ARepository.Add(@sfnChiSQ_Dist, fnChiSQ_Dist, fpiChiSQ_Dist, frkValue, 255, ftStatistical, @sfnChiSQ_DistDescription, 1);
  ARepository.Add(@sfnChiSQ_Dist_RT, fnChiSQ_Dist_RT, fpiChiSQ_Dist_RT, frkValue, 255, ftStatistical, @sfnChiSQ_Dist_RTDescription, 1);
  ARepository.Add(@sfnChiSQ_Inv, fnChiSQ_Inv, fpiChiSQ_Inv, frkValue, 255, ftStatistical, @sfnChiSQ_InvDescription, 1);
  ARepository.Add(@sfnChiSQ_Inv_RT, fnChiSQ_Inv_RT, fpiChiSQ_Inv_RT, frkValue, 255, ftStatistical, @sfnChiSQ_Inv_RTDescription, 1);
  ARepository.Add(@sfnChiSQ_Test, nil, nil, frkValue, 255, ftStatistical, @sfnChiSQ_TestDescription, 1);
  ARepository.Add(@sfnConfidence_Norm, nil, nil, frkValue, 255, ftStatistical, @sfnConfidence_NormDescription, 1);
  ARepository.Add(@sfnConfidence_T, nil, nil, frkValue, 255, ftStatistical, @sfnConfidence_TDescription, 1);
  ARepository.Add(@sfnCorrel, fnCorrel, fpiCorrel, frkNonArrayValue, 307, ftStatistical, @sfnCorrelDescription);
  ARepository.Add(@sfnCovariance_P, fnCovariance_P, fpiCovariance_P, frkNonArrayValue, 255, ftStatistical, @sfnCovariance_PDescription, 1);
  ARepository.Add(@sfnCovariance_S, fnCovariance_S, fpiCovariance_S, frkNonArrayValue, 255, ftStatistical, @sfnCovariance_SDescription, 1);
  ARepository.Add(@sfnCount, fnCount, fpiCount, frkNonArrayValue, 0, ftStatistical, @sfnCountDescription);
  ARepository.Add(@sfnCountA, fnCountA, fpiCountA, frkNonArrayValue, 169, ftStatistical, @sfnCountADescription);
  ARepository.Add(@sfnCountBlank, fnCountBlank, fpiCountBlank, frkNonArrayValue, 347, ftStatistical, @sfnCountBlankDescription);
  ARepository.Add(@sfnCountIF, fnCountIF, fpiCountIF, frkNonArrayValue, 346, ftStatistical, @sfnCountIFDescription);
  ARepository.Add(@sfnCountIFS, fnCountIFS, fpiCountIFS, frkNonArrayValue, 255, ftStatistical, @sfnCountIFSDescription);
  ARepository.Add(@sfnDevSQ, fnDevSQ, fpiDevSQ, frkNonArrayValue, 318, ftStatistical, @sfnDevSQDescription);
  ARepository.Add(@sfnExpon_Dist, fnExpon_Dist, fpiExpon_Dist, frkValue, 255, ftStatistical, @sfnExpon_DistDescription, 1);
  ARepository.Add(@sfnF_Dist, nil, nil, frkValue, 255, ftStatistical, @sfnF_DistDescription, 1);
  ARepository.Add(@sfnF_Dist_RT, nil, nil, frkValue, 255, ftStatistical, @sfnF_Dist_RTDescription, 1);
  ARepository.Add(@sfnF_Inv, nil, nil, frkValue, 255, ftStatistical, @sfnF_InvDescription, 1);
  ARepository.Add(@sfnF_Inv_RT, nil, nil, frkValue, 255, ftStatistical, @sfnF_Inv_RTDescription, 1);
  ARepository.Add(@sfnF_Test, nil, nil, frkValue, 255, ftStatistical, @sfnF_TestDescription, 1);
  ARepository.Add(@sfnFisher, nil, nil, frkValue, 283, ftStatistical, @sfnFisherDescription);
  ARepository.Add(@sfnFisherInv, nil, nil, frkValue, 284, ftStatistical, @sfnFisherInvDescription);
  ARepository.Add(@sfnForecast, fnForecast, fpiForecast, frkValue, 309, ftStatistical, @sfnForecastDescription);
  ARepository.Add(@sfnFrequency, nil, nil, frkValue, 252, ftStatistical, @sfnFrequencyDescription);
  ARepository.Add(@sfnGamma, fnGamma, fpiGamma, frkValue, 255, ftStatistical, @sfnGammaDescription, 1);
  ARepository.Add(@sfnGamma_Dist, fnGamma_Dist, fpiGamma_Dist, frkValue, 255, ftStatistical, @sfnGamma_DistDescription, 1);
  ARepository.Add(@sfnGamma_Inv, fnGamma_Inv, fpiGamma_Inv, frkValue, 255, ftStatistical, @sfnGamma_InvDescription, 1);
  ARepository.Add(@sfnGammaLn, fnGammaLn, fpiGammaLn, frkValue, 271, ftStatistical, @sfnGammaLnDescription);
  ARepository.Add(@sfnGammaLn_Precise, fnGammaLn_Precise, fpiGammaLn_Precise, frkValue, 255, ftStatistical, @sfnGammaLn_PreciseDescription, 1);
  ARepository.Add(@sfnGauss, fnGauss, fpiGauss, frkValue, 255, ftStatistical, @sfnGaussDescription, 1);
  ARepository.Add(@sfnGeomean, fnGeomean, fpiGeomean, frkNonArrayValue, 319, ftStatistical, @sfnGeomeanDescription);
  ARepository.Add(@sfnGrowth, nil, nil, frkValue, 52, ftStatistical, @sfnGrowthDescription);
  ARepository.Add(@sfnHarmean, nil, nil, frkValue, 320, ftStatistical, @sfnHarmeanDescription);
  ARepository.Add(@sfnHypgeom_Dist, fnHypgeom_Dist, fpiHypgeom_Dist, frkValue, 255, ftStatistical, @sfnHypgeom_DistDescription, 1);
  ARepository.Add(@sfnIntercept, fnIntercept, fpiIntercept, frkNonArrayValue, 311, ftStatistical, @sfnInterceptDescription);
  ARepository.Add(@sfnKurt, nil, nil, frkValue, 322, ftStatistical, @sfnKurtDescription);
  ARepository.Add(@sfnLarge, fnLarge, fpiLarge, frkValue, 325, ftStatistical, @sfnLargeDescription);
  ARepository.Add(@sfnLinest, nil, nil, frkValue, 49, ftStatistical, @sfnLinestDescription);
  ARepository.Add(@sfnLogest, nil, nil, frkValue, 51, ftStatistical, @sfnLogestDescription);
  ARepository.Add(@sfnLogNorm_Dist, nil, nil, frkValue, 255, ftStatistical, @sfnLogNorm_DistDescription, 1);
  ARepository.Add(@sfnLogNorm_Inv, nil, nil, frkValue, 255, ftStatistical, @sfnLogNorm_InvDescription, 1);
  ARepository.Add(@sfnMax, fnMax, fpiMax, frkNonArrayValue, 7, ftStatistical, @sfnMaxDescription);
  ARepository.Add(@sfnMaxA, fnMaxA, fpiMaxA, frkNonArrayValue, 362, ftStatistical, @sfnMaxADescription);
  ARepository.Add(@sfnMedian, fnMedian, fpiMedian, frkNonArrayValue, 227, ftStatistical, @sfnMedianDescription);
  ARepository.Add(@sfnMin, fnMin, fpiMin, frkNonArrayValue, 6, ftStatistical, @sfnMinDescription);
  ARepository.Add(@sfnMinA, fnMinA, fpiMinA, frkNonArrayValue, 363, ftStatistical, @sfnMinADescription);
  ARepository.Add(@sfnMode_Mult, fnMode_Mult, fpiMode_Mult, frkArray, 255, ftStatistical, @sfnMode_MultDescription, 1);
  ARepository.Add(@sfnMode_SNGL, fnMode_Sngl, fpiMode_Sngl, frkNonArrayValue, 255, ftStatistical, @sfnMode_SNGLDescription, 1);
  ARepository.Add(@sfnNegBinom_Dist, nil, nil, frkValue, 255, ftStatistical, @sfnNegBinom_DistDescription, 1);
  ARepository.Add(@sfnNorm_Dist, fnNorm_Dist, fpiNorm_Dist, frkValue, 255, ftStatistical, @sfnNorm_DistDescription, 1);
  ARepository.Add(@sfnNorm_Inv, fnNorm_Inv, fpiNorm_Inv, frkValue, 255, ftStatistical, @sfnNorm_InvDescription, 1);
  ARepository.Add(@sfnNorm_S_Dist, fnNorm_S_Dist, fpiNorm_S_Dist, frkValue, 255, ftStatistical, @sfnNorm_S_DistDescription, 1);
  ARepository.Add(@sfnNorm_S_Inv, fnNorm_S_Inv, fpiNorm_S_Inv, frkValue, 255, ftStatistical, @sfnNorm_S_InvDescription, 1);
  ARepository.Add(@sfnPearson, fnPearson, fpiPearson, frkNonArrayValue, 312, ftStatistical, @sfnPearsonDescription);
  ARepository.Add(@sfnPercentile_Exc, fnPercentile_Exc, fpiPercentile_Exc, frkValue, 255, ftStatistical, @sfnPercentile_ExcDescription, 1);
  ARepository.Add(@sfnPercentile_Inc, fnPercentile_Inc, fpiPercentile_Inc, frkValue, 255, ftStatistical, @sfnPercentile_IncDescription, 1);
  ARepository.Add(@sfnPercentRank_Exc, nil, nil, frkValue, 255, ftStatistical, @sfnPercentRank_ExcDescription, 1);
  ARepository.Add(@sfnPercentRank_Inc, nil, nil, frkValue, 255, ftStatistical, @sfnPercentRank_IncDescription, 1);
  ARepository.Add(@sfnPermut, fnPermut, fpiPermut, frkValue, 299, ftStatistical, @sfnPermutDescription);
  ARepository.Add(@sfnPermutationA, nil, nil, frkValue, 255, ftStatistical, @sfnPermutationADescription, 1);
  ARepository.Add(@sfnPHI, nil, nil, frkValue, 255, ftStatistical, @sfnPHIDescription, 1);
  ARepository.Add(@sfnPoisson_Dist, fnPoisson_Dist, fpiPoisson_Dist, frkValue, 255, ftStatistical, @sfnPoisson_DistDescription, 1);
  ARepository.Add(@sfnProb, nil, nil, frkValue, 317, ftStatistical, @sfnProbDescription);
  ARepository.Add(@sfnQuartile_Exc, fnQuartile_Exc, fpiQuartile_Exc, frkValue, 255, ftStatistical, @sfnQuartile_ExcDescription, 1);
  ARepository.Add(@sfnQuartile_Inc, fnQuartile_Inc, fpiQuartile_Inc, frkValue, 255, ftStatistical, @sfnQuartile_IncDescription, 1);
  ARepository.Add(@sfnRank_Avg, fnRank_Avg, fpiRank_Avg, frkValue, 255, ftStatistical, @sfnRank_AvgDescription, 1);
  ARepository.Add(@sfnRank_Eq, fnRank_Eq, fpiRank_Eq, frkValue, 255, ftStatistical, @sfnRank_EqDescription, 1);
  ARepository.Add(@sfnRSQ, fnRSQ, fpiRSQ, frkNonArrayValue, 313, ftStatistical, @sfnRSQDescription);
  ARepository.Add(@sfnSkew, fnSkew, fpiSkew, frkNonArrayValue, 323, ftStatistical, @sfnSkewDescription);
  ARepository.Add(@sfnSkew_P, fnSkew_P, fpiSkew_P, frkNonArrayValue, 255, ftStatistical, @sfnSkew_PDescription, 1);
  ARepository.Add(@sfnSlope, fnSlope, fpiSlope, frkNonArrayValue, 315, ftStatistical, @sfnSlopeDescription);
  ARepository.Add(@sfnSmall, fnSmall, fpiSmall, frkValue, 326, ftStatistical, @sfnSmallDescription);
  ARepository.Add(@sfnStandardize, fnStandardize, fpiStandardize, frkValue, 297, ftStatistical, @sfnStandardizeDescription);
  ARepository.Add(@sfnStDev_P, fnStDev_P, fpiStDev_P, frkNonArrayValue, 255, ftStatistical, @sfnStDev_PDescription, 1);
  ARepository.Add(@sfnStDev_S, fnStDev_S, fpiStDev_S, frkNonArrayValue, 255, ftStatistical, @sfnStDev_SDescription, 1);
  ARepository.Add(@sfnStDevA, fnStDevA, fpiStDevA, frkNonArrayValue, 366, ftStatistical, @sfnStDevADescription);
  ARepository.Add(@sfnStDevPA, fnStDevPA, fpiStDevPA, frkNonArrayValue, 364, ftStatistical, @sfnStDevPADescription);
  ARepository.Add(@sfnSTEYX, fnSTEYX, fpiSTEYX, frkNonArrayValue, 314, ftStatistical, @sfnSTEYXDescription);
  ARepository.Add(@sfnT_Dist, fnT_Dist, fpiT_Dist, frkValue, 255, ftStatistical, @sfnT_DistDescription, 1);
  ARepository.Add(@sfnT_Dist_2T, fnT_Dist_2T, fpiT_Dist_2T, frkValue, 255, ftStatistical, @sfnT_Dist_2TDescription, 1);
  ARepository.Add(@sfnT_Dist_RT, fnT_Dist_RT, fpiT_Dist_RT, frkValue, 255, ftStatistical, @sfnT_Dist_RTDescription, 1);
  ARepository.Add(@sfnT_Inv, fnT_Inv, fpiT_Inv, frkValue, 255, ftStatistical, @sfnT_InvDescription, 1);
  ARepository.Add(@sfnT_Inv_2T, fnT_Inv_2T, fpiT_Inv_2T, frkValue, 255, ftStatistical, @sfnT_Inv_2TDescription, 1);
  ARepository.Add(@sfnT_Test, nil, nil, frkValue, 255, ftStatistical, @sfnT_TestDescription, 1);
  ARepository.Add(@sfnTrend, nil, nil, frkValue, 50, ftStatistical, @sfnTrendDescription);
  ARepository.Add(@sfnTrimMean, nil, nil, frkValue, 331, ftStatistical, @sfnTrimMeanDescription);
  ARepository.Add(@sfnVar_P, fnVar_P, fpiVar_P, frkNonArrayValue, 255, ftStatistical, @sfnVar_PDescription, 1);
  ARepository.Add(@sfnVar_S, fnVar_S, fpiVar_S, frkNonArrayValue, 255, ftStatistical, @sfnVar_SDescription, 1);
  ARepository.Add(@sfnVarA, fnVarA, fpiVarA, frkNonArrayValue, 367, ftStatistical, @sfnVarADescription);
  ARepository.Add(@sfnVarPA, fnVarPA, fpiVarPA, frkNonArrayValue, 365, ftStatistical, @sfnVarPADescription);
  ARepository.Add(@sfnWeibull_Dist, fnWeibull_Dist, fpiWeibull_Dist, frkValue, 255, ftStatistical, @sfnWeibull_DistDescription, 1);
  ARepository.Add(@sfnZ_Test, nil, nil, frkValue, 255, ftStatistical, @sfnZ_TestDescription, 1);
end;

initialization
  RegisterFunctions(dxSpreadSheetFunctionsRepository);
end.
