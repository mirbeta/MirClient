{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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
unit dxRichEdit.Import.Doc.PositionConverter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxGenerics,
  dxRichEdit.Utils.Types;

type
  TdxPositionList = class(TdxIntegerList)
  public
    procedure UnionWith(const AValues: TArray<Integer>);
    function CreateList(ASorted: Boolean = False): TdxIntegerList;
  end;

  { TdxPositionConverter }

  TdxPositionConverter = class
  strict protected type

    TPositionConverterState = class
    strict private
      FStart: Integer;
      FEnd: Integer;
      FPositionsMapping: TDictionary<Integer, TdxDocumentLogPosition>;
      FSkippedPositionsMapping: TDictionary<Integer, TdxDocumentLogPosition>;
      FCurrentPositionIndex: Integer;
    public
      constructor Create(AStart: Integer; AEnd: Integer);
      destructor Destroy; override;

      property Start: Integer read FStart;
      property &End: Integer read FEnd;
      property PositionsMapping: TDictionary<Integer, TdxDocumentLogPosition> read FPositionsMapping;
      property SkippedPositionsMapping: TDictionary<Integer, TdxDocumentLogPosition> read FSkippedPositionsMapping;
      property PositionIndex: Integer read FCurrentPositionIndex write FCurrentPositionIndex;
    end;

  strict private
    FEditEnabled: Boolean;
    FOriginalPositions: TdxIntegerList;
    FUniquePositions: TdxPositionList;
    FCurrentState: TPositionConverterState;
    FStates: TStack<TPositionConverterState>;
    FStateList: TdxFastObjectList;
    function CreateState(AStart, AEnd: Integer): TPositionConverterState;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginInit;
    procedure EndInit;
    procedure BeginEmbeddedContent(AStart: Integer; AEnd: Integer);
    procedure EndEmbeddedContent;
    procedure AppendPositions(APositions: TdxIntegerList); overload;
    procedure AppendPositions(APositions: TArray<Integer>); overload;
    function CalculatePositionIndex(ACharacterPosition: Integer): Integer;
    procedure AdvanceNext(ALogPosition: TdxDocumentLogPosition; AOriginalPosition: Integer; ALength: Integer);
    function ContainsPosition(APosition: Integer): Boolean;
    function TryConvert(APosition: Integer; ASkipDeletedPosition: Boolean; out ALogPosition: TdxDocumentLogPosition): Boolean; overload;
    function TryConvert(APosition: Integer; out ALogPosition: TdxDocumentLogPosition): Boolean; overload;
    function GetNextObtainablePosition(APosition: Integer): TdxDocumentLogPosition;
    function GetPrevObtainablePosition(APosition: Integer): TdxDocumentLogPosition;
  end;

implementation

{ TdxPositionConverter.TPositionConverterState }

constructor TdxPositionConverter.TPositionConverterState.Create(AStart: Integer; AEnd: Integer);
begin
  FStart := AStart;
  FEnd := AEnd;
  FPositionsMapping := TDictionary<Integer, TdxDocumentLogPosition>.Create;
  FSkippedPositionsMapping := TDictionary<Integer, TdxDocumentLogPosition>.Create;
end;

destructor TdxPositionConverter.TPositionConverterState.Destroy;
begin
  FPositionsMapping.Free;
  FSkippedPositionsMapping.Free;
  inherited Destroy;
end;

{ TdxPositionConverter }

constructor TdxPositionConverter.Create;
begin
  inherited Create;
  FStates := TStack<TPositionConverterState>.Create;
  FStateList := TdxFastObjectList.Create;
end;

destructor TdxPositionConverter.Destroy;
begin
  FStates.Free;
  FStateList.Free;
  FOriginalPositions.Free;
  FUniquePositions.Free;
  inherited Destroy;
end;

procedure TdxPositionConverter.BeginInit;
begin
  FEditEnabled := True;
  FUniquePositions.Free;
  FUniquePositions := TdxPositionList.Create;
end;

procedure TdxPositionConverter.EndInit;
begin
  FEditEnabled := False;

  FOriginalPositions.Free;
  FOriginalPositions := FUniquePositions.CreateList(True);
end;

procedure TdxPositionConverter.BeginEmbeddedContent(AStart: Integer; AEnd: Integer);
var
  AStartIndex: Integer;
begin
  if FCurrentState <> nil then
    FStates.Push(FCurrentState);
  FCurrentState := CreateState(AStart, AEnd);
  FOriginalPositions.BinarySearch(AStart, AStartIndex);
  if (AStartIndex < FOriginalPositions.Count) and (FOriginalPositions[AStartIndex] <= AEnd) then
    FCurrentState.PositionIndex := AStartIndex
  else
    FCurrentState.PositionIndex := -1;
end;

procedure TdxPositionConverter.EndEmbeddedContent;
begin
  if FStates.Count > 0 then
    FCurrentState := FStates.Pop
  else
    FCurrentState := nil;
end;

procedure TdxPositionConverter.AppendPositions(APositions: TdxIntegerList);
begin
  AppendPositions(APositions.ToArray);
end;

procedure TdxPositionConverter.AppendPositions(APositions: TArray<Integer>);
begin
  if FEditEnabled then
    FUniquePositions.UnionWith(APositions);
end;

function TdxPositionConverter.CalculatePositionIndex(ACharacterPosition: Integer): Integer;
begin
  if not FOriginalPositions.BinarySearch(ACharacterPosition, Result) then
    Result := -1;
end;

procedure TdxPositionConverter.AdvanceNext(ALogPosition: TdxDocumentLogPosition; AOriginalPosition: Integer; ALength: Integer);
var
  AIndex, ACurrentPosition: Integer;
begin
  Assert(FCurrentState <> nil);
  AIndex := FCurrentState.PositionIndex;
  if AIndex < 0 then
    Exit;
  while (AIndex < FOriginalPositions.Count) and (FOriginalPositions[AIndex] < AOriginalPosition) do
  begin
    ACurrentPosition := FOriginalPositions[AIndex];
    FCurrentState.SkippedPositionsMapping.Add(ACurrentPosition, ALogPosition);
    Inc(AIndex);
  end;
  while (AIndex < FOriginalPositions.Count) and (FOriginalPositions[AIndex] < AOriginalPosition + ALength) do
  begin
    ACurrentPosition := FOriginalPositions[AIndex];
    FCurrentState.PositionsMapping.Add(ACurrentPosition, ALogPosition + ACurrentPosition - AOriginalPosition);
    Inc(AIndex);
  end;
  FCurrentState.PositionIndex := AIndex;
end;

function TdxPositionConverter.ContainsPosition(APosition: Integer): Boolean;
begin
  Result := (APosition >= FCurrentState.Start) and (APosition <= FCurrentState.&End);
end;

function TdxPositionConverter.TryConvert(APosition: Integer; ASkipDeletedPosition: Boolean; out ALogPosition: TdxDocumentLogPosition): Boolean;
begin
  if not FCurrentState.PositionsMapping.TryGetValue(APosition, ALogPosition) then
  begin
    if not ASkipDeletedPosition then
      Result := FCurrentState.SkippedPositionsMapping.TryGetValue(APosition, ALogPosition)
    else
      Result := False;
  end
  else
    Result := True;
end;

function TdxPositionConverter.TryConvert(APosition: Integer; out ALogPosition: TdxDocumentLogPosition): Boolean;
begin
  Result := TryConvert(APosition, False, ALogPosition);
end;

function TdxPositionConverter.GetNextObtainablePosition(APosition: Integer): TdxDocumentLogPosition;
var
  APositionsMapping: TDictionary<Integer, TdxDocumentLogPosition>;
  AIndex, I: Integer;
begin
  APositionsMapping := FCurrentState.PositionsMapping;
  AIndex := CalculatePositionIndex(APosition);
  Assert(AIndex >= 0);
  for I := AIndex + 1 to FOriginalPositions.Count - 1 do
    if APositionsMapping.TryGetValue(FOriginalPositions[I], Result) then
      Exit;
  Result := 0;
end;

function TdxPositionConverter.GetPrevObtainablePosition(APosition: Integer): TdxDocumentLogPosition;
var
  APositionsMapping: TDictionary<Integer, TdxDocumentLogPosition>;
  AIndex, I: Integer;
begin
  APositionsMapping := FCurrentState.PositionsMapping;
  AIndex := CalculatePositionIndex(APosition);
  Assert(AIndex >= 0);
  for I := AIndex - 1 downto 0 do
    if APositionsMapping.TryGetValue(FOriginalPositions[I], Result) then
      Exit(Result);
  Result := 0;
end;

function TdxPositionConverter.CreateState(AStart, AEnd: Integer): TPositionConverterState;
begin
  Result := TPositionConverterState.Create(AStart, AEnd);
  FStateList.Add(Result);
end;

{ TdxPositionList }

function TdxPositionList.CreateList(ASorted: Boolean = False): TdxIntegerList;
var
  AIndex, I: Integer;
begin
  Result := TdxIntegerList.Create;
  if Count > 0 then
  begin
    Sort;
    AIndex := 0;
    for I := 1 to Count - 1 do
    begin
      if Items[AIndex] <> Items[I] then
        Inc(AIndex);
      Items[AIndex] := Items[I];
    end;
    if Count > 0 then
      Count := AIndex + 1;
    Result.AddRange(Self);
  end;
end;

procedure TdxPositionList.UnionWith(const AValues: TArray<Integer>);
begin
  AddRange(AValues);
end;

end.
