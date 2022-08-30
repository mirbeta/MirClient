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

unit dxSpreadSheetFunctionsCompatibility;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, Variants, Math, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  //
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetFunctionsMath,
  dxSpreadSheetTypes,
  dxSpreadSheetUtils;

procedure fnBetaDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnBetaInv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnBinomDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnChiDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnChiInv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnExponDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnHypgeomDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnNormDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnNormInv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnNormSDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnNormSInv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnPoisson(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnRank(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnStDev(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnStDevP(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnTDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnTInv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnVar(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnVarP(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
procedure fnWeibull(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

procedure fpiBetaDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiBetaInv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiBinomDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiChiDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiChiInv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiCovariance_P(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiExponDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiGamma_Dist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiGamma_Inv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiHypgeomDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiNormDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiNormInv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiNormSDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiNormSInv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiPercentile_Inc(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiPoisson(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiQuartile_Inc(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiRank(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiStDev(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiStDevP(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiTDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiTInv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiVar(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiVarP(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fpiWeibull(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
implementation

uses
  dxSpreadSheetFunctions,
  dxSpreadSheetFunctionsStatistical,
  dxSpreadSheetFunctionsStrs;

procedure fnBetaDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSpreadSheetFunctionsStatistical.fnBetaDist(Sender, AParams);
end;

procedure fnBetaInv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnBeta_Inv(Sender, AParams);
end;

procedure fnBinomDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnBinom_Dist(Sender, AParams);
end;

procedure fnChiDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnChiSQ_Dist_RT(Sender, AParams);
end;

procedure fnChiInv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnChiSQ_Inv_RT(Sender, AParams);
end;

procedure fnExponDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnExpon_Dist(Sender, AParams);
end;

procedure fnHypgeomDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSpreadSheetFunctionsStatistical.fnHypgeomDist(Sender, AParams);
end;

procedure fnNormDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnNorm_Dist(Sender, AParams);
end;

procedure fnNormInv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnNorm_Inv(Sender, AParams);
end;

procedure fnNormSDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSpreadSheetFunctionsStatistical.fnNormSDist(Sender, AParams);
end;

procedure fnNormSInv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnNorm_S_Inv(Sender, AParams);
end;

procedure fnPoisson(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnPoisson_Dist(Sender, AParams);
end;

procedure fnRank(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnRank_EQ(Sender, AParams);
end;

procedure fnStDev(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnStDev_S(Sender, AParams);
end;

procedure fnStDevP(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnStDev_P(Sender, AParams);
end;

procedure fnTDist(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSpreadSheetFunctionsStatistical.fnTDist(Sender, AParams);
end;

procedure fnTInv(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  dxSpreadSheetFunctionsStatistical.fnTInv(Sender, AParams);
end;

procedure fnVar(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnVar_S(Sender, AParams);
end;

procedure fnVarP(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnVar_P(Sender, AParams);
end;

procedure fnWeibull(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
begin
  fnWeibull_Dist(Sender, AParams);
end;

procedure fpiBetaDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(5, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkNonRequiredValue;
  AParamKind[4] := fpkNonRequiredValue;
end;

procedure fpiBetaInv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(5, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkNonRequiredValue;
  AParamKind[4] := fpkNonRequiredValue;
end;

procedure fpiBinomDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkValue;
end;

procedure fpiChiDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiChiInv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiCovariance_P(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkArray;
end;

procedure fpiExponDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
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

procedure fpiHypgeomDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkValue;
end;

procedure fpiNormDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(4, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
  AParamKind[3] := fpkValue;
end;

procedure fpiNormInv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
end;

procedure fpiNormSDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiNormSInv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
end;

procedure fpiPercentile_Inc(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkValue;
end;

procedure fpiPoisson(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
end;

procedure fpiQuartile_Inc(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkArray;
  AParamKind[1] := fpkValue;
end;

procedure fpiRank(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkNonRequiredValue;
end;

procedure fpiStDev(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiStDevP(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiTDist(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(3, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
end;

procedure fpiTInv(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(2, AParamCount, AParamKind);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
end;

procedure fpiVar(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiVarP(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  dxSpreadSheetInitializeParamInfo(1, AParamCount, AParamKind);
  AParamKind[0] := fpkUnlimited;
end;

procedure fpiWeibull(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
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
  ARepository.Add(@sfnBetaDist, fnBetaDist, fpiBetaDist, frkValue, 270, ftCompatibility, @sfnBetaDistDescription);
  ARepository.Add(@sfnBetaInv, fnBetaInv, fpiBetaInv, frkValue, 272, ftCompatibility, @sfnBetaInvDescription);
  ARepository.Add(@sfnBinomDist, fnBinomDist, fpiBinomDist, frkValue, 273, ftCompatibility, @sfnBinomDistDescription);
  ARepository.Add(@sfnChiDist, fnChiDist, fpiChiDist, frkValue, 274, ftCompatibility, @sfnChiDistDescription);
  ARepository.Add(@sfnChiInv, fnChiInv, fpiChiInv, frkValue, 275, ftCompatibility, @sfnChiInvDescription);
  ARepository.Add(@sfnChiTest, nil, nil, frkValue, 306, ftCompatibility, @sfnChiTestDescription);
  ARepository.Add(@sfnCovar, fnCovariance_P, fpiCovariance_P, frkNonArrayValue, 308, ftCompatibility, @sfnCovarDescription);
  ARepository.Add(@sfnCritBinom, nil, nil, frkValue, 278, ftCompatibility, @sfnCritBinomDescription);
  ARepository.Add(@sfnExponDist, fnExponDist, fpiExponDist, frkValue, 280, ftCompatibility, @sfnExponDistDescription);
  ARepository.Add(@sfnFDist, nil, nil, frkValue, 281, ftCompatibility, @sfnFDistDescription);
  ARepository.Add(@sfnFInv, nil, nil, frkValue, 282, ftCompatibility, @sfnFInvDescription);
  ARepository.Add(@sfnFTest, nil, nil, frkValue, 310, ftCompatibility, @sfnFTestDescription);
  ARepository.Add(@sfnGammaDist, fnGamma_Dist, fpiGamma_Dist, frkValue, 286, ftCompatibility, @sfnGammaDistDescription);
  ARepository.Add(@sfnGammaInv, fnGamma_Inv, fpiGamma_Inv, frkValue, 287, ftCompatibility, @sfnGammaInvDescription);
  ARepository.Add(@sfnHypgeomDist, fnHypgeomDist, fpiHypgeomDist, frkValue, 289, ftCompatibility, @sfnHypgeomDistDescription);
  ARepository.Add(@sfnLogInv, nil, nil, frkValue, 291, ftCompatibility, @sfnLogInvDescription);
  ARepository.Add(@sfnLogNormDist, nil, nil, frkValue, 290, ftCompatibility, @sfnLogNormDistDescription);
  ARepository.Add(@sfnMode, nil, nil, frkValue, 330, ftCompatibility, @sfnModeDescription);
  ARepository.Add(@sfnNegBinomDist, nil, nil, frkValue, 292, ftCompatibility, @sfnNegBinomDistDescription);
  ARepository.Add(@sfnNormDist, fnNormDist, fpiNormDist, frkValue, 293, ftCompatibility, @sfnNormDistDescription);
  ARepository.Add(@sfnNormInv, fnNormInv, fpiNormInv, frkValue, 295, ftCompatibility, @sfnNormInvDescription);
  ARepository.Add(@sfnNormSDist, fnNormSDist, fpiNormSDist, frkValue, 294, ftCompatibility, @sfnNormSDistDescription);
  ARepository.Add(@sfnNormSInv, fnNormSInv, fpiNormSInv, frkValue, 296, ftCompatibility, @sfnNormSInvDescription);
  ARepository.Add(@sfnPercentile, fnPercentile_Inc, fpiPercentile_Inc, frkValue, 328, ftCompatibility, @sfnPercentileDescription);
  ARepository.Add(@sfnPercentRank, nil, nil, frkValue, 329, ftCompatibility, @sfnPercentRankDescription);
  ARepository.Add(@sfnPoisson, fnPoisson, fpiPoisson, frkValue, 300, ftCompatibility, @sfnPoissonDescription);
  ARepository.Add(@sfnQuartile, fnQuartile_Inc, fpiQuartile_Inc, frkValue, 327, ftCompatibility, @sfnQuartileDescription);
  ARepository.Add(@sfnRank, fnRank, fpiRank, frkValue, 216, ftCompatibility, @sfnRankDescription);
  ARepository.Add(@sfnStDev, fnStDev, fpiStDev, frkNonArrayValue, 12, ftCompatibility, @sfnStDevDescription);
  ARepository.Add(@sfnStDevP, fnStDevP, fpiStDevP, frkNonArrayValue, 193, ftCompatibility, @sfnStDevPDescription);
  ARepository.Add(@sfnTDist, fnTDist, fpiTDist, frkValue, 301, ftCompatibility, @sfnTDistDescription);
  ARepository.Add(@sfnTInv, fnTInv, fpiTInv, frkValue, 332, ftCompatibility, @sfnTInvDescription);
  ARepository.Add(@sfnTTest, nil, nil, frkValue, 316, ftCompatibility, @sfnTTestDescription);
  ARepository.Add(@sfnVar, fnVar, fpiVar, frkNonArrayValue, 46, ftCompatibility, @sfnVarDescription);
  ARepository.Add(@sfnVarP, fnVarP, fpiVarP, frkNonArrayValue, 194, ftCompatibility, @sfnVarPDescription);
  ARepository.Add(@sfnWeibull, fnWeibull, fpiWeibull, frkValue, 302, ftCompatibility, @sfnWeibullDescription);
  ARepository.Add(@sfnZTest, nil, nil, frkValue, 324, ftCompatibility, @sfnZTestDescription);
end;

initialization
  RegisterFunctions(dxSpreadSheetFunctionsRepository);
end.
