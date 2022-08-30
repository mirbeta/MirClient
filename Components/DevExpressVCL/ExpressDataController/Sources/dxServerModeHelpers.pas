{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDataController                                    }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSDATACONTROLLER AND ALL         }
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

unit dxServerModeHelpers;

{$I cxVer.inc}

interface

uses
  SysUtils, Variants, Classes, dxCoreClasses;

type
  TdxLinearLeastSquaresArgs = class;
  TdxPackSample = array of Double;
  TdxPackSampleArray = array of TdxPackSample;
  TdxSample = class;
  TdxSampleList = class;
  TdxServerModeOptimalFetchResult = class;
  TdxServerModeServerAndChannelModel = class;
  TdxSolution = class;

  { TdxServerModeServerAndChannelModelBuilder }

  TdxServerModeServerAndChannelModelBuilder = class
  private
    FSamples: TdxSampleList;
  protected
    function IsInvalid(ASolution: TdxServerModeServerAndChannelModel): Boolean;
    function PackSample(ASample: TdxSample; ACombination: string): TdxPackSample;
    function UnpackSolution(ALlsResults: array of Double; ACombination: string): TdxServerModeServerAndChannelModel;
    function Weight(ASamples: TdxFastList; ASolution: TdxServerModeServerAndChannelModel): Double;

    property Samples: TdxSampleList read FSamples;
  public
    constructor Create;
    destructor Destroy; override;

    function GetMaxObservableTake: Integer;
    function Linear(AUnknowns: Integer; AData: TdxPackSampleArray): TdxPackSample;
    function LinearLeastSquares(AArgs: TdxLinearLeastSquaresArgs): TdxPackSample;
    function PackSamples(ASamples: TdxFastList; const ACombination: string): TdxPackSampleArray;
    procedure RegisterSample(ATake, AScan: Integer; time: Double);
    function Resolve(ASamples: TdxFastList; const ACombination: string): TdxServerModeServerAndChannelModel; overload;
    function Resolve: TdxServerModeServerAndChannelModel; overload;
  end;

  { TdxServerModeServerAndChannelModel }

  TdxServerModeServerAndChannelModel = class
  private
    FConstantPart: Double;
    FScanCoeff: Double;
    FTakeCoeff: Double;
  public
    constructor Create(AConstPart, ATakeCoeff, AScanCoeff: Double);

    function Clone: TdxServerModeServerAndChannelModel;
    function ToString: string; override;

    property ConstantPart: Double read FConstantPart;
    property ScanCoeff: Double read FScanCoeff;
    property TakeCoeff: Double read FTakeCoeff;
  end;

  { TdxServerModeOptimalFetchParam }

  TdxServerModeOptimalFetchParam = class
  private
    FBaseCountTakeData: Integer;
    FEdgeTimeMultiplier: Double;
    FFillTimeMultiplier: Double;
    FIndex: Integer;
    FMaxAllowedTake: Integer;
    FMaxIndex: Integer;
    FMiddleTimeMultiplier: Double;
    FMinIndex: Integer;
    FModel: TdxServerModeServerAndChannelModel;
    FTotalCount: Integer;
  public
    constructor Create(AModel: TdxServerModeServerAndChannelModel; AIndex, AMinIndex, AMaxIndex,
      ATotalCount, ABaseCountTakeData, AMaxAllowedTake: Integer; AFillTimeMultiplier, AEdgeTimeMultiplier,
      AMiddleTimeMultiplier: Double);
    function ScanFromBottom(ALastIndexToFetch: Integer): Integer;
    function ScanFromTop(ALastIndexToFetch: Integer): Integer;
    function SkipFromBottom(AFirstIndexToFetch: Integer): Integer;
    function SkipFromTop(AFirstIndexToFetch: Integer): Integer;
    function ToString: string; override;

    property BaseCountTakeData: Integer read FBaseCountTakeData;
    property EdgeTimeMultiplier: Double read FEdgeTimeMultiplier;
    property FillTimeMultiplier: Double read FFillTimeMultiplier;
    property Index: Integer read FIndex;
    property MaxAllowedTake: Integer read FMaxAllowedTake;
    property MaxIndex: Integer read FMaxIndex;
    property MiddleTimeMultiplier: Double read FMiddleTimeMultiplier;
    property MinIndex: Integer read FMinIndex;
    property Model: TdxServerModeServerAndChannelModel read FModel;
    property TotalCount: Integer read FTotalCount;
  end;

  { TdxServerModeOptimalFetchHelper }

  TdxServerModeOptimalFetchHelper = class
  public
    class function CalculateOptimalFetchResult(AParam: TdxServerModeOptimalFetchParam): TdxServerModeOptimalFetchResult;
    class function CalculateTakeFromFixedTimeAndScan(AModel: TdxServerModeServerAndChannelModel; ATargetTime: Double; AScan: Integer; out AValue: Integer): Boolean;
    class function CalculateTakeFromFixedTimeAndSkip(AModel: TdxServerModeServerAndChannelModel; ATargetTime: Double; ASkip: Integer; out AValue: Integer): Boolean;
    class function OptimalResultCore(AParam: TdxServerModeOptimalFetchParam): TdxSolution;
  end;

  { TdxLinearLeastSquaresArgs }

  TdxLinearLeastSquaresArgs = class
  private
    FData: TdxPackSampleArray;
    FUnknowns: Integer;
  public
    constructor Create(AUnknowns: Integer; AData: TdxPackSampleArray);

    property Data: TdxPackSampleArray read FData;
    property Unknowns: Integer read FUnknowns;
  end;

  { TdxServerModeOptimalFetchResult }

  TdxServerModeOptimalFetchResult = class
  private
    FIsFromEnd: Boolean;
    FSkip: Integer;
    FTake: Integer;
  public
    constructor Create(AIsFromEnd: Boolean; ASkip, ATake: Integer);

    property IsFromEnd: Boolean read FIsFromEnd;
    property Skip: Integer read FSkip;
    property Take: Integer read FTake;
  end;

  { TdxSolution }

  TdxSolution = class
  private
    FIsFromEnd: Boolean;
    FParams: TdxServerModeOptimalFetchParam;
    FSkip: Integer;
    FTake: Integer;
    function GetScan: Integer;
    function GetTime: Double;
  public
    constructor Create(ASkip, ATake: Integer; AIsFromEnd: Boolean; AParams: TdxServerModeOptimalFetchParam);
    function FromI1I2(AParams: TdxServerModeOptimalFetchParam; AIsFromEnd: Boolean; I1, I2: Integer): TdxSolution;
    class function FromSkipTake(AParams: TdxServerModeOptimalFetchParam; AIsFromEnd: Boolean; ASkip, ATake: Integer): TdxSolution;

    property IsFromEnd: Boolean read FIsFromEnd;
    property Scan: Integer read GetScan;
    property Skip: Integer read FSkip;
    property Take: Integer read FTake;
    property Time: Double read GetTime;
  end;

  { TdxListSolution }

  TdxSolutionList = class(TdxFastObjectList)
  private
    function GetItem(Index: Integer): TdxSolution;
  public
    property Items[Index: Integer]: TdxSolution read GetItem; default;
  end;

  { TdxSample }

  TdxSample = class
  private
    FScan: Integer;
    FTake: Integer;
    FTime: Double;
  public
    constructor Create(ATake, AScan: Integer; ATime: Double);

    property Scan: Integer read FScan;
    property Take: Integer read FTake;
    property Time: Double read FTime;
  end;

  { TdxListSample }

  TdxSampleList = class(TdxFastObjectList)
  private
    function GetItem(Index: Integer): TdxSample;
  public
    property Items[Index: Integer]: TdxSample read GetItem; default;
  end;

implementation

uses
  RTLConsts, Math, cxVariants;

const
  Combinations: array[0..6] of string = ('C', 'S', 'T', 'CS', 'CT', 'TS', 'CTS');
  TraceWriteLines = False;
  MaxSamples = 1024;

{ TdxServerModeServerAndChannelModelBuilder }

constructor TdxServerModeServerAndChannelModelBuilder.Create;
begin
  inherited Create;
  FSamples := TdxSampleList.Create;
end;

destructor TdxServerModeServerAndChannelModelBuilder.Destroy;
begin
  FreeAndNil(FSamples);
  inherited Destroy;
end;

function TdxServerModeServerAndChannelModelBuilder.GetMaxObservableTake: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Samples.Count - 1 do
    if Samples[I].Take > Result then
      Result := Samples[I].Take;
  if Result <= 0 then
    Result := Low(Integer);
end;

function TdxServerModeServerAndChannelModelBuilder.Linear(AUnknowns: Integer; AData: TdxPackSampleArray): TdxPackSample;
var
  I, J, ABestI, K: Integer;
  AEps, ABestV, ACurrentV, ABuf, ADiv, AKDiv: Double;
begin
  for I := 0 to AUnknowns - 1 do
  begin
    AEps := MinSingle;
    if Abs(AData[I, I]) < AEps then
    begin
      ABestV := Abs(AData[I, I]);
      ABestI := -1;
      for J := I + 1 to AUnknowns - 1 do
      begin
        ACurrentV := Abs(AData[J, I]);
        if ABestV < ACurrentV then
        begin
          ABestV := ACurrentV;
          ABestI := J;
        end;
      end;
      if ABestI < 0 then
      begin
        Result := nil;
        Exit;
      end;
      for J := 0 to AUnknowns do
      begin
        ABuf := AData[I, J];
        AData[I, J] := AData[ABestI, J];
        AData[ABestI, J] := ABuf;
      end;
    end;
    ADiv := AData[I, I];
    AData[I, I] := 1.0;
    for J := I + 1 to AUnknowns do
      AData[I, J] := AData[I, J] / ADiv;
    for K := 0 to AUnknowns - 1 do
    begin
      if K = I then
        Continue;
      AKDiv := AData[K, I];
      AData[K, I] := 0.0;
      for J := I + 1 to AUnknowns do
        AData[K, J] := AData[K, J] - (AData[I, J] * AKDiv);
    end;
  end;
  SetLength(Result, AUnknowns);
  for I := 0 to AUnknowns - 1 do
    Result[I] := AData[I, AUnknowns];
end;

function TdxServerModeServerAndChannelModelBuilder.LinearLeastSquares(AArgs: TdxLinearLeastSquaresArgs): TdxPackSample;
var
  ARow: TdxPackSample;
  I, J, K, AUnknowns: Integer;
  AData, ANormal: TdxPackSampleArray;
begin
  AUnknowns := AArgs.Unknowns;
  AData := AArgs.Data;
  SetLength(ANormal, AUnknowns);
  for I := 0 to Length(ANormal) - 1 do
    SetLength(ANormal[I], AUnknowns + 1);

  for K := 0 to Length(AData) - 1 do
  begin
    ARow := AData[K];
    for I := 0 to AUnknowns - 1 do
      for J := 0 to AUnknowns do
        ANormal[I, J] :=  ANormal[I, J] + (ARow[I] * ARow[J]);
  end;
  Result := Linear(AUnknowns, ANormal);
end;

function TdxServerModeServerAndChannelModelBuilder.PackSamples(ASamples: TdxFastList; const ACombination: string): TdxPackSampleArray;
var
  I: Integer;
begin
  SetLength(Result, ASamples.Count);
  for I := 0 to ASamples.Count - 1 do
    Result[I] := PackSample(ASamples[I], ACombination);
end;

procedure TdxServerModeServerAndChannelModelBuilder.RegisterSample(ATake, AScan: Integer; time: Double);
begin
  Samples.Add(TdxSample.Create(ATake, AScan, time));
  if Samples.Count > MaxSamples then
    Samples.Delete(0);
end;

function TdxServerModeServerAndChannelModelBuilder.Resolve(ASamples: TdxFastList;
  const ACombination: string): TdxServerModeServerAndChannelModel;
var
  ASolution: TdxPackSample;
  AArgs: TdxLinearLeastSquaresArgs;
begin
  AArgs := TdxLinearLeastSquaresArgs.Create(Length(ACombination), PackSamples(ASamples, ACombination));
  try
    ASolution := LinearLeastSquares(AArgs);
  finally
    AArgs.Free;
  end;
  Result := UnpackSolution(ASolution, ACombination);
end;

function TdxServerModeServerAndChannelModelBuilder.Resolve: TdxServerModeServerAndChannelModel;

  function Get2OfEvery3(AList: TdxFastList): TdxFastList;
  var
    I, J: Integer;
  begin
    Result := TdxFastList.Create;
    I := 0;
    for J := 0 to AList.Count - 1 do
    begin
      if I = 1 then
        Result.Add(AList[J]);
      I := (I + 1) mod 3;
    end;
  end;

  function Get1And3OfEvery3(AList: TdxFastList): TdxFastList;
  var
    I, J: Integer;
  begin
    Result := TdxFastList.Create;
    I := 0;
    for J := 0 to AList.Count - 1 do
    begin
      if I <> 1 then
        Result.Add(AList[J]);
      I := (I + 1) mod 3;
    end;
  end;

  function CalculateCombinationIndex(ABaseSequence, AValidateSequence: TdxFastList): Integer;
  var
    I: Integer;
    ACombination: string;
    AWeight, AFinalSolutionWeight: Double;
    ASolution: TdxServerModeServerAndChannelModel;
  begin
    Result := -1;
    AFinalSolutionWeight := Infinity;
    for I := 0 to Length(Combinations) - 1 do
    begin
      ACombination := Combinations[I];
      if Length(ACombination) >= ABaseSequence.Count then
        Continue;
      ASolution := Resolve(ABaseSequence, ACombination);
      try
        if (ASolution = nil) or IsInvalid(ASolution) then
          Continue;
        AWeight := Weight(AValidateSequence, ASolution);
        if AWeight >= AFinalSolutionWeight then
          Continue;
        AFinalSolutionWeight := AWeight;
      finally
        ASolution.Free;
      end;
      Result := I;
    end;
  end;

var
  ACombinationIndex: Integer;
  ANeedResolveOnFullSequence: Boolean;
  ABaseSequence, AValidateSequence: TdxFastList;
begin
  Result := nil;
  if Samples.Count < 2 then
    Exit;
  ANeedResolveOnFullSequence := Samples.Count >= 9;
  if not ANeedResolveOnFullSequence then
  begin
    ABaseSequence := Samples;
    AValidateSequence := Samples;
  end
  else
  begin
    ABaseSequence := Get1And3OfEvery3(Samples);
    AValidateSequence := Get2OfEvery3(Samples);
  end;
  try
    ACombinationIndex := CalculateCombinationIndex(ABaseSequence, AValidateSequence);
    if ACombinationIndex >= 0 then
      Result := Resolve(Samples, Combinations[ACombinationIndex]);
  finally
    if ANeedResolveOnFullSequence then
    begin
      AValidateSequence.Free;
      ABaseSequence.Free;
    end;
  end;
end;

function TdxServerModeServerAndChannelModelBuilder.IsInvalid(ASolution: TdxServerModeServerAndChannelModel): Boolean;
begin
  Result := (ASolution.ConstantPart < 0) or (ASolution.ScanCoeff < 0) or (ASolution.TakeCoeff < 0);
end;

function TdxServerModeServerAndChannelModelBuilder.PackSample(ASample: TdxSample; ACombination: string): TdxPackSample;
var
  I: Integer;
  AField: Char;
  AValue: Double;
begin
  SetLength(Result, Length(ACombination) + 1);
  for I := 1 to Length(ACombination) do
  begin
    AField := ACombination[I];
    case AField of
      'T': AValue := ASample.Take;
      'S': AValue := ASample.Scan;
    else //'C':
      AValue := 1.0;
    end;
    Result[I - 1] := AValue;
  end;
  Result[Length(ACombination)] := ASample.Time;
end;

function TdxServerModeServerAndChannelModelBuilder.UnpackSolution(ALlsResults: array of Double;
  ACombination: string): TdxServerModeServerAndChannelModel;
var
  I: Integer;
  AField: Char;
  C, T, S, AValue: Double;
begin
  if Length(ALlsResults) = 0 then
  begin
    Result := nil;
    Exit;
  end;
  C := 0.0;
  T := 0.0;
  S := 0.0;
  for I := 1 to Length(ACombination) do
  begin
    AField := ACombination[I];
    AValue := ALlsResults[I - 1];
    if IsInfinite(AValue) or IsNan(AValue) then
    begin
      Result := nil;
      Exit;
    end;
    if IsInfinite(AValue) or IsNan(AValue) then
    begin
      Result := nil;
      Exit;
    end;
    case AField of
      'C': C := AValue;
      'T': T := AValue;
      'S': S := AValue;
    end;
  end;
  Result := TdxServerModeServerAndChannelModel.Create(C, T, S);
end;

function TdxServerModeServerAndChannelModelBuilder.Weight(ASamples: TdxFastList;
  ASolution: TdxServerModeServerAndChannelModel): Double;
var
  I: Integer;
  ASample: TdxSample;
  AWeight, ASquare, ADelta, APrediction: Double;
begin
  AWeight := 0;
  for I := 0 to ASamples.Count - 1 do
  begin
    ASample := ASamples[I];
    APrediction := ASolution.ConstantPart + ASolution.TakeCoeff * ASample.Take + ASolution.ScanCoeff * ASample.Scan;
    ADelta := APrediction - ASample.Time;
    ASquare := ADelta * ADelta;
    AWeight := AWeight + ASquare;
  end;
  Result := AWeight;
end;

{ TdxServerModeServerAndChannelModel }

constructor TdxServerModeServerAndChannelModel.Create(AConstPart, ATakeCoeff, AScanCoeff: Double);
begin
  inherited Create;
  FConstantPart := AConstPart;
  FTakeCoeff := ATakeCoeff;
  FScanCoeff := AScanCoeff;
end;

function TdxServerModeServerAndChannelModel.Clone: TdxServerModeServerAndChannelModel;
begin
  Result := TdxServerModeServerAndChannelModel.Create(ConstantPart, TakeCoeff, ScanCoeff);
end;

function TdxServerModeServerAndChannelModel.ToString: string;
begin
  Result := Format('Take*%g + Scan*%g + %g', [ConstantPart, TakeCoeff, ScanCoeff])
end;

{ TdxServerModeOptimalFetchParam }

constructor TdxServerModeOptimalFetchParam.Create(AModel: TdxServerModeServerAndChannelModel; AIndex,
  AMinIndex, AMaxIndex, ATotalCount, ABaseCountTakeData, AMaxAllowedTake: Integer; AFillTimeMultiplier,
  AEdgeTimeMultiplier, AMiddleTimeMultiplier: Double);
begin
  inherited Create;
  Assert((AIndex >= 0) and (AMinIndex >= 0) and (AMaxIndex >= 0) and (AIndex >= AMinIndex)
    and (AIndex <= AMaxIndex) and (AMinIndex <= AMaxIndex) and (ATotalCount > 0)
    and (ABaseCountTakeData >= 0) and (ATotalCount >= 0) and (AMiddleTimeMultiplier >= 1.0)
    and (AEdgeTimeMultiplier >= AMiddleTimeMultiplier) and (AFillTimeMultiplier >= AEdgeTimeMultiplier),
    'Incorrect input data!');
  FModel := AModel;
  FIndex := AIndex;
  FMinIndex := AMinIndex;
  FMaxIndex := AMaxIndex;
  FTotalCount := ATotalCount;
  FBaseCountTakeData := ABaseCountTakeData;
  FMaxAllowedTake := AMaxAllowedTake;
  FFillTimeMultiplier := AFillTimeMultiplier;
  FEdgeTimeMultiplier := AEdgeTimeMultiplier;
  FMiddleTimeMultiplier := AMiddleTimeMultiplier;
end;

function TdxServerModeOptimalFetchParam.ScanFromBottom(ALastIndexToFetch: Integer): Integer;
begin
  Result := TotalCount - ALastIndexToFetch;
end;

function TdxServerModeOptimalFetchParam.ScanFromTop(ALastIndexToFetch: Integer): Integer;
begin
  Result := ALastIndexToFetch + 1;
end;

function TdxServerModeOptimalFetchParam.SkipFromBottom(AFirstIndexToFetch: Integer): Integer;
begin
  Result := TotalCount - AFirstIndexToFetch - 1;
end;

function TdxServerModeOptimalFetchParam.SkipFromTop(AFirstIndexToFetch: Integer): Integer;
begin
  Result := AFirstIndexToFetch;
end;

function TdxServerModeOptimalFetchParam.ToString: string;
begin
  Result := Format('new ServerModeOptimalFetchParam(new ServerModeServerAndChannelModel(%g, %g, %g), %d, %d, %d, %d, %d, %d, %g, %g, %g)',
    [Model.ConstantPart, Model.TakeCoeff, Model.ScanCoeff, Index, MinIndex, MaxIndex, TotalCount, BaseCountTakeData, MaxAllowedTake,
      FillTimeMultiplier, EdgeTimeMultiplier, MiddleTimeMultiplier]);
end;

{ TdxServerModeOptimalFetchHelper }

class function TdxServerModeOptimalFetchHelper.CalculateOptimalFetchResult(
  AParam: TdxServerModeOptimalFetchParam): TdxServerModeOptimalFetchResult;
var
  AInternalResult: TdxSolution;
begin
  AInternalResult := OptimalResultCore(AParam);
  try
    Result := TdxServerModeOptimalFetchResult.Create(AInternalResult.IsFromEnd, AInternalResult.Skip, AInternalResult.Take);
  finally
    AInternalResult.Free;
  end;
end;

class function TdxServerModeOptimalFetchHelper.CalculateTakeFromFixedTimeAndScan(
  AModel: TdxServerModeServerAndChannelModel; ATargetTime: Double; AScan: Integer; out AValue: Integer): Boolean;
var
  ADoubleResult: Double;
begin
  Result := False;
  try
    if IsZero(AModel.TakeCoeff) then
      AValue := High(Integer)
    else
    begin
      ADoubleResult := (ATargetTime - AModel.ScanCoeff * AScan - AModel.ConstantPart) / AModel.TakeCoeff;
      if IsNan(ADoubleResult) and IsInfinite(ADoubleResult) and (High(Integer) < ADoubleResult) then
        AValue := High(Integer)
      else
        AValue := Trunc(ADoubleResult);
      if ADoubleResult < 1 then
        Exit;
    end;
  except
    AValue := High(Integer);
  end;
  Result := True;
end;

class function TdxServerModeOptimalFetchHelper.CalculateTakeFromFixedTimeAndSkip(
  AModel: TdxServerModeServerAndChannelModel; ATargetTime: Double; ASkip: Integer; out AValue: Integer): Boolean;
var
  ADoubleResult, ACoeff: Double;
begin
  Result := False;
  try
    ACoeff := AModel.ScanCoeff + AModel.TakeCoeff;
    if IsZero(ACoeff) then
      AValue := High(Integer)
    else
    begin
      ADoubleResult := (ATargetTime - AModel.ScanCoeff * ASkip - AModel.ConstantPart) / ACoeff;
      if IsNan(ADoubleResult) and IsInfinite(ADoubleResult) and (High(Integer) < ADoubleResult) then
        AValue := High(Integer)
      else
        AValue := Trunc(ADoubleResult);
      if ADoubleResult < 1 then
        Exit;
    end;
  except
    AValue := High(Integer);
  end;
  Result := True;
end;

class function TdxServerModeOptimalFetchHelper.OptimalResultCore(AParam: TdxServerModeOptimalFetchParam): TdxSolution;
var
  AIsFromBottom: Boolean;
  ASolutions: TdxSolutionList;
  ASolution, ABestSolution: TdxSolution;
  ABaseTime, ATargetFillTime, AEdgeTime, ATargetTime, ACalculatedTake: Double;
  I, ABaseSkip, AFillTake, ASkipFromTop, ASkipFromBottom, AMaxTake, AScanToMiddleFromTop, AScanToMiddleFromBottom,
  AScanToMiddle, ATake, ASkip, AScan, ALastIndex, AFirstIndex: Integer;
begin
  ABaseSkip := Trunc(Math.Min(Math.Max(0, AParam.Index - AParam.BaseCountTakeData / 2), Math.Max(0, AParam.TotalCount - AParam.Index - 1 - AParam.BaseCountTakeData / 2)));
  ABaseTime := AParam.Model.ConstantPart + AParam.Model.TakeCoeff * AParam.BaseCountTakeData + AParam.Model.ScanCoeff * (ABaseSkip + AParam.BaseCountTakeData);
  AFillTake := AParam.MaxIndex - AParam.MinIndex + 1;
  if AFillTake <= AParam.MaxAllowedTake then
  begin
    ATargetFillTime := ABaseTime * AParam.FillTimeMultiplier;
    ASkipFromTop := AParam.SkipFromTop(AParam.MinIndex);
    ASkipFromBottom := AParam.SkipFromBottom(AParam.MaxIndex);
    if ASkipFromTop <= ASkipFromBottom then
    begin
      ASolution := TdxSolution.FromSkipTake(AParam, False, ASkipFromTop, AFillTake);
      if ATargetFillTime >= ASolution.Time then
      begin
        Result := ASolution;
        Exit;
      end
      else
        ASolution.Free;
    end
    else
    begin
      ASolution := TdxSolution.FromSkipTake(AParam, True, ASkipFromBottom, AFillTake);
      if ATargetFillTime >= ASolution.Time then
      begin
        Result := ASolution;
        Exit;
      end
      else
        ASolution.Free;
    end;
  end;
  AMaxTake := Math.Min(AFillTake, AParam.MaxAllowedTake);
  AEdgeTime := ABaseTime * AParam.EdgeTimeMultiplier;

  ASolutions := TdxSolutionList.Create;
  try

    ASkip := AParam.SkipFromTop(AParam.MinIndex);
    if CalculateTakeFromFixedTimeAndSkip(AParam.Model, AEdgeTime, ASkip, ATake) then
    begin
      if ATake > AMaxTake then
        ATake := AMaxTake;
      ALastIndex := ASkip + ATake - 1;
      if AParam.Index <= ALastIndex then
        ASolutions.Add(TdxSolution.FromSkipTake(AParam, False, ASkip, ATake));
    end;

    ASkip := AParam.SkipFromBottom(AParam.MaxIndex);
    if CalculateTakeFromFixedTimeAndSkip(AParam.Model, AEdgeTime, ASkip, ATake) then
    begin
      if ATake > AMaxTake then
        ATake := AMaxTake;
      ALastIndex := AParam.TotalCount - ASkip - ATake;
      if ALastIndex <= AParam.Index then
        ASolutions.Add(TdxSolution.FromSkipTake(AParam, True, ASkip, ATake));
    end;

    AScan := AParam.MaxIndex + 1;
    if CalculateTakeFromFixedTimeAndScan(AParam.Model, AEdgeTime, AScan, ATake) then
    begin
      if ATake > AMaxTake then
        ATake := AMaxTake;
      Assert(ATake <= AScan);
      ASkip := AScan - ATake;
      AFirstIndex := ASkip;
      if AParam.Index >= AFirstIndex then
        ASolutions.Add(TdxSolution.FromSkipTake(AParam, False, ASkip, ATake));
    end;

    AScan := AParam.TotalCount - AParam.MinIndex;
    if CalculateTakeFromFixedTimeAndScan(AParam.Model, AEdgeTime, AScan, ATake) then
    begin
      if ATake > AMaxTake then
        ATake := AMaxTake;
      Assert(ATake <= AScan);
      ASkip := AScan - ATake;
      AFirstIndex := AParam.TotalCount - ASkip - 1;
      if AParam.Index <= AFirstIndex then
        ASolutions.Add(TdxSolution.FromSkipTake(AParam, True, ASkip, ATake));
    end;

    ABestSolution := nil;
    for I := 0 to ASolutions.Count - 1 do
    begin
      ASolution := ASolutions[I];
      if (ABestSolution = nil) or (ASolution.Take > ABestSolution.Take) or ((ASolution.Take = ABestSolution.Take) and (ASolution.Scan < ABestSolution.Scan)) then
        ABestSolution := ASolution;
    end;
    if ABestSolution <> nil then
    begin
      ASolutions.Extract(ABestSolution);
      Result := ABestSolution;
      Exit;
    end;
  finally
    ASolutions.Free;
  end;
  ATargetTime := ABaseTime * AParam.MiddleTimeMultiplier;
  AScanToMiddleFromTop := AParam.Index + 1;
  AScanToMiddleFromBottom := AParam.TotalCount - AParam.Index;
  if AScanToMiddleFromTop < AScanToMiddleFromBottom  then
  begin
    AIsFromBottom := False;
    AScanToMiddle := AScanToMiddleFromTop;
  end
  else
  begin
    AIsFromBottom := True;
    AScanToMiddle := AScanToMiddleFromBottom;
  end;
  if not IsZero(AParam.Model.TakeCoeff + AParam.Model.ScanCoeff / 2) then
    ACalculatedTake := (ATargetTime - AParam.Model.ScanCoeff * AScanToMiddle - AParam.Model.ConstantPart) / (AParam.Model.TakeCoeff + AParam.Model.ScanCoeff / 2)
  else
    ACalculatedTake := Infinity;
  if ACalculatedTake >= AMaxTake then
    ATake := AMaxTake
  else
    ATake := Trunc(ACalculatedTake);
  ASkip := Trunc(AScanToMiddle - 1 - ATake / 2);
  Result := TdxSolution.FromSkipTake(AParam, AIsFromBottom, ASkip, ATake);
end;

{ TdxLinearLeastSquaresArgs }

constructor TdxLinearLeastSquaresArgs.Create(AUnknowns: Integer; AData: TdxPackSampleArray);
begin
  inherited Create;
  FUnknowns := AUnknowns;
  FData := AData;
end;

{ TdxServerModeOptimalFetchResult }

constructor TdxServerModeOptimalFetchResult.Create(AIsFromEnd: Boolean; ASkip, ATake: Integer);
begin
  inherited Create;
  FIsFromEnd := AIsFromEnd;
  FSkip := ASkip;
  FTake := ATake;
end;

{ TdxSolution }

constructor TdxSolution.Create(ASkip, ATake: Integer; AIsFromEnd: Boolean; AParams: TdxServerModeOptimalFetchParam);
{$IFOPT C+}
var
  I1, I2: Integer;
{$ENDIF}
begin
  inherited Create;
  Assert(ATake > 0, 'ATake: ' + IntToStr(ATake));
  Assert(ASkip >= 0, 'ASkip: ' + IntToStr(ASkip));
  Assert(ASkip + ATake <= AParams.TotalCount, Format('skip(%d) + take(%d) > TotalCount(%d)', [ASkip, ATake, AParams.TotalCount]));
  FSkip := ASkip;
  FTake := ATake;
  FIsFromEnd := AIsFromEnd;
  FParams := AParams;
{$IFOPT C+}
  if IsFromEnd then
  begin
    I1 := FParams.TotalCount - Skip - 1;
    I2 := FParams.TotalCount - Scan;
  end
  else
  begin
    I1 := Skip;
    I2 := Scan - 1;
  end;
  Assert((FParams.Index >= Min(I1, I2)) and (FParams.Index <= Max(I1, I2)),
    Format('Index (%d) is outside of range (%d, %d)', [FParams.Index, I1, I2]));
{$ENDIF}
end;

function TdxSolution.FromI1I2(AParams: TdxServerModeOptimalFetchParam; AIsFromEnd: Boolean; I1,
  I2: Integer): TdxSolution;
begin
  if IsFromEnd then
    Result := FromSkipTake(AParams, AIsFromEnd, AParams.TotalCount - I1 - 1, I1 - I2 + 1)
  else
    Result := FromSkipTake(AParams, AIsFromEnd, I1, I2 - I1 + 1);
end;

class function TdxSolution.FromSkipTake(AParams: TdxServerModeOptimalFetchParam; AIsFromEnd: Boolean; ASkip,
  ATake: Integer): TdxSolution;
begin
  Result := TdxSolution.Create(ASkip, ATake, AIsFromEnd, AParams);
end;

function TdxSolution.GetScan: Integer;
begin
  Result := Skip + Take;
end;

function TdxSolution.GetTime: Double;
begin
  Result := FParams.Model.ScanCoeff * Scan + FParams.Model.TakeCoeff * Take + FParams.Model.ConstantPart;
end;

{ TdxSolutionList }

function TdxSolutionList.GetItem(Index: Integer): TdxSolution;
begin
  Result := TdxSolution(inherited Items[Index]);
end;

{ TdxSample }

constructor TdxSample.Create(ATake, AScan: Integer; ATime: Double);
begin
  inherited Create;
  FTake := ATake;
  FScan := AScan;
  FTime := ATime;
end;

{ TdxSampleList }

function TdxSampleList.GetItem(Index: Integer): TdxSample;
begin
  Result := TdxSample(inherited Items[Index]);
end;

end.

