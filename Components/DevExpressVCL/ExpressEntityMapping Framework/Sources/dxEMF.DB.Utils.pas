{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEntityMapping Framework                           }
{                                                                    }
{           Copyright (c) 2016-2019 Developer Express Inc.           }
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
{   (DCU, OBJ, DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY  }
{   TRADE SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER }
{   IS LICENSED TO DISTRIBUTE THE EXPRESSENTITYMAPPING FRAMEWORK     }
{   AS PART OF AN EXECUTABLE PROGRAM ONLY.                           }
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

unit dxEMF.DB.Utils;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  SysUtils, Classes, Generics.Collections, Rtti, DB,
  dxCoreClasses,
  dxEMF.Types,
  dxEMF.Metadata,
  dxEMF.DB.Criteria;

type

 { TdxBaseFormatterHelper }

  TdxBaseFormatterHelper = class
  public
    class function DefaultFormatFunction(AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<string>): string; static;
    class function DefaultFormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand, ARightOperand: string): string; static;
    class function DefaultFormatUnary(AOperatorType: TdxUnaryOperatorType; const AOperand: string): string; static;
  end;

  { TdxSimpleSQLParser }

  TdxSimpleSQLParser = class
  strict private const
    AsDelimiterString = ' as ';
  strict private
    FSQLText: string;
    FResult: TList<string>;
    FCurColumn: TStringBuilder;
    FInBrackets: Integer;
    FInQuotes: Boolean;
    FInDoubleQuotes: Boolean;
    FFirstQuote: Boolean;
    FQuoteEscaped: Boolean;
    FChar: Char;
    FNextChr: TdxNullableValue<Char>;
    FPrevChr: TdxNullableValue<Char>;
    procedure Append(AColumnOnly: Boolean);
    function ClosingBracket: Boolean;
    function Comma: Boolean;
    function Default: Boolean;
    function DoubleQuotes: Boolean;
    function Quote: Boolean;
    function OpeningBracket: Boolean;
  public
    constructor Create(const ASQLText: string);
    destructor Destroy; override;

    class function GetExpandedProperties(const AProperties: TArray<string>; const AExpandingAlias: string): string; static;
    class function GetColumns(const ASQLText: string): TArray<string>; overload; static;
    function GetColumns: TArray<string>; overload;
  end;

  { TdxFieldValueConverter }

  TdxFieldValueConverter = class
  public
    class function ToValue(AField: TField; AMemberInfo: TdxMappingMemberInfo): TValue;
    class function ToValues(const AFields: TArray<TField>; AMemberInfo: TdxMappingMemberInfo): TArray<TValue>;
    class procedure SetValue(AObject: TObject; const AFields: TArray<TField>; AMemberInfo: TdxMappingMemberInfo); overload;
    class procedure SetValue(AObject: TObject; const AValues: TArray<Variant>; AMemberInfo: TdxMappingMemberInfo); overload;
  end;

implementation

uses
  StrUtils, TypInfo, FMTBcd, SqlTimSt, DateUtils, Variants,
  dxCore, dxStringHelper,
  dxEMF.Serializer,
  dxEMF.Strs,
  dxEMF.Utils,
  dxEMF.Utils.Exceptions;

function dxStringToGUID(const S: string): TGUID;
begin
  if Length(S) = 36 then
    Result := StringToGUID('{' + S + '}')
  else
    Result := StringToGUID(S);
end;

{ TdxBaseFormatterHelper }

class function TdxBaseFormatterHelper.DefaultFormatFunction(AOperatorType: TdxFunctionOperatorType;
  const AOperands: TArray<string>): string;
var
  AArg, ALen: string;
  ASb: TStringBuilder;
  AIndex: Integer;
begin
  case AOperatorType of
    TdxFunctionOperatorType.Iif:
      begin
        if (Length(AOperands) < 3) or ((Length(AOperands) mod 2) = 0) then
          raise EArgumentException.Create(sdxFilteringTheIifFunctionOperatorRequiresThree);
        if Length(AOperands) = 3 then
          Exit(Format('case when %s then %s else %s end', [AOperands[0], AOperands[1], AOperands[2]]));
        ASb := TStringBuilder.Create;
        try
          AIndex := -2;
          ASb.Append('case ');
          repeat
            Inc(AIndex, 2);
            ASb.AppendFormat('when %s then %s ', [AOperands[AIndex], AOperands[AIndex + 1]]);
          until not ((AIndex + 3) < Length(AOperands));;
          ASb.AppendFormat('else %s end', [AOperands[AIndex + 2]]);
          Result := ASb.ToString;
        finally
          ASb.Free;
        end;
      end;
    TdxFunctionOperatorType.IsNullOrEmpty:
      Result := Format('((%s) is null or (Len(%0:s) = 0))', [AOperands[0]]);
    TdxFunctionOperatorType.Trim:
      Result := Format('ltrim(rtrim(%s))', [AOperands[0]]);
    TdxFunctionOperatorType.Concat:
      begin
        ASb := TStringBuilder.Create;
        try
          for AArg in AOperands do
          begin
            if ASb.Length > 0 then
              ASb.Append(' || ');
            ASb.Append(AArg);
          end;
          Result := ASb.ToString;
        finally
          ASb.Free;
        end;
      end;
    TdxFunctionOperatorType.Substring:
      begin
        if Length(AOperands) < 3 then
          ALen := 'Len(' + AOperands[0] + ')' + ' - ' + AOperands[1]
        else
          ALen := AOperands[2];
        Result := 'Substring(' + AOperands[0] + ', (' + AOperands[1] + ') + 1, ' + ALen + ')';
      end;
    TdxFunctionOperatorType.IsNull,
    TdxFunctionOperatorType.Len,
    TdxFunctionOperatorType.UpperCase,
    TdxFunctionOperatorType.LowerCase:
      begin
        ASb := TStringBuilder.Create;
        try
          case AOperatorType of
            TdxFunctionOperatorType.IsNull:
              ASb.Append('IsNull');
            TdxFunctionOperatorType.Len:
              ASb.Append('Len');
            TdxFunctionOperatorType.UpperCase:
              ASb.Append('Upper');
            TdxFunctionOperatorType.LowerCase:
              ASb.Append('Lower');
          end;
          ASb.Append('(');
          for AIndex := 0 to Length(AOperands) - 1 do
          begin
            if AIndex > 0 then
              ASb.Append(', ');
            ASb.Append(AOperands[AIndex]);
          end;
          ASb.Append(')');
          Result := ASb.ToString;
        finally
          ASb.Free;
        end;
      end;
    TdxFunctionOperatorType.Abs:
      Result := Format('Abs(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Sqr:
      Result := Format('Sqr(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Log:
      Result := Format('Log(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Random:
      Result := 'Rnd()';
    TdxFunctionOperatorType.Sin:
      Result := Format('Sin(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Tan:
      Result := Format('Tan(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcTan:
      Result := Format('Atn(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Cos:
      Result := Format('Cos(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Exp:
      Result := Format('Exp(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Power:
      Result := Format('Power(%s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MillisecondOf:
      Result := Format('DATEPART(Millisecond, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.SecondOf:
      Result := Format('DATEPART(Second, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.MinuteOf:
      Result := Format('DATEPART(Minute, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.HourOf:
      Result := Format('DATEPART(Hour, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOf:
      Result := Format('DATEPART(Day, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.MonthOf:
      Result := Format('DATEPART(Month, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.YearOf:
      Result := Format('DATEPART(Year, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.GetTimeOfDay:
      Result := Format('(CONVERT(BigInt,((CONVERT(BigInt,DATEPART(HOUR, %0:s))) * 36000000000) + ' +
        '((CONVERT(BigInt,DATEPART(MINUTE, %0:s))) * 600000000) + ' +
        '((CONVERT(BigInt,DATEPART(SECOND, %0:s))) * 10000000) + ' +
        '((CONVERT(BigInt,DATEPART(MILLISECOND, %0:s))) * 10000)))', [AOperands[0]]);
    TdxFunctionOperatorType.DayOfTheWeek:
      Result := Format('CONVERT(Int,(DATEPART(dw, %s) + (@@DATEFIRST) + 6) %% 7)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOfTheYear:
      Result := Format('DATEPART(DayOfYear, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.DateOf:
      Result := Format('CONVERT(DATE, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.Ascii:
      Result := Format('Ascii(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Char:
      Result := Format('Char(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ToString:
      Result := Format('Str(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Replace:
      Result := Format('Replace(%s, %s, %s)', [AOperands[0], AOperands[1], AOperands[2]]);
    TdxFunctionOperatorType.Reverse:
      Result := Format('Reverse(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Remove:
      Result := Format('Stuff(%s, %s + 1, %s, '#$27#$27')', [AOperands[0], AOperands[1], AOperands[2]]);
    TdxFunctionOperatorType.Insert:
      Result := Format('Stuff(%s, %s + 1, 0, %s)', [AOperands[0], AOperands[1], AOperands[2]]);
    TdxFunctionOperatorType.CharIndex:
      Result := Format('(CharIndex(%s, %s) - 1)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Sign:
      Result := Format('Sign(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Round:
      Result := Format('Round(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Floor:
      Result := Format('Floor(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Ceil:
      Result := Format('Ceiling(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.IncDay:
      Result := Format('AddDays(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.UTCNow:
      Result := 'UTCNOW()';
    TdxFunctionOperatorType.Now:
      Result := 'NOW()';
    TdxFunctionOperatorType.Today:
      Result := 'DATE()';
    else
      raise ENotImplemented.Create('DefaultFormatFunction for ' + TdxFormatterHelper.GetName(AOperatorType));
  end;
end;

class function TdxBaseFormatterHelper.DefaultFormatBinary(AOperatorType: TdxBinaryOperatorType;
  const ALeftOperand, ARightOperand: string): string;
const
  ASQLOperator: array[TdxBinaryOperatorType] of string =
    ('=', '<>', '>', '<', '<=', '>=', '&', '|', '^', '/', '%', '*', '+', '-');
begin
  Result :=  '(' + ALeftOperand + ' ' + ASQLOperator[AOperatorType] + ' ' + ARightOperand + ')';
end;

class function TdxBaseFormatterHelper.DefaultFormatUnary(AOperatorType: TdxUnaryOperatorType; const AOperand: string): string;
var
  AFormat: string;
begin
  case AOperatorType of
    TdxUnaryOperatorType.IsNull:
      AFormat := '%s is null';
    TdxUnaryOperatorType.Not:
      AFormat := 'not (%s)';
    TdxUnaryOperatorType.Minus:
      AFormat := '-%s';
    TdxUnaryOperatorType.Plus:
      AFormat := '+%s';
    TdxUnaryOperatorType.BitwiseNot:
      AFormat := '~(%s)';
    else
      raise ENotImplemented.Create('DefaultFormatUnary('#$27 + TdxFormatterHelper.GetName(AOperatorType) + #$27', '#$27 + AOperand + #$27')');
  end;
  Result := Format(AFormat, [AOperand]);
end;

{ TdxSimpleSQLParser }

constructor TdxSimpleSQLParser.Create(const ASQLText: string);
begin
  inherited Create;
  FResult := TList<string>.Create;
  FCurColumn := TStringBuilder.Create;
  FInBrackets := 0;
  FInQuotes := False;
  FInDoubleQuotes := False;
  FFirstQuote := False;
  FQuoteEscaped := False;
  FSQLText := ASQLText;
end;

destructor TdxSimpleSQLParser.Destroy;
begin
  FreeAndNil(FResult);
  FreeAndNil(FCurColumn);
  inherited Destroy;
end;

class function TdxSimpleSQLParser.GetExpandedProperties(const AProperties: TArray<string>;
  const AExpandingAlias: string): string;
var
  AExpandedSelectedProperties: TStringBuilder;
  I, ALastAsIndex: Integer;
  AField, AExistingAlias, AAlias: string;
begin
  AExpandedSelectedProperties := TStringBuilder.Create;
  try
    for I := 0 to Length(AProperties) - 1 do
    begin
      if Trim(AProperties[I]) = '' then
        Continue;
      AField := AProperties[I];
      AExistingAlias := '';
      {$IFDEF DELPHIXE3}
      ALastAsIndex := AField.LastIndexOf(AsDelimiterString);
      {$ELSE}
      ALastAsIndex := TdxStringHelper.LastIndexOf(AField, AsDelimiterString);
      {$ENDIF}
      if ALastAsIndex > 0 then
      begin
        {$IFDEF DELPHIXE3}
        AAlias := AField.Substring(ALastAsIndex + AsDelimiterString.Length).Trim;
        if not AAlias.Contains(' ') then
        begin
          AField := AField.Remove(ALastAsIndex);
          AExistingAlias := AAlias;
        end;
        {$ELSE}
        AAlias := Trim(TdxStringHelper.Substring(AField, ALastAsIndex + Length(AsDelimiterString)));
        if not TdxStringHelper.Contains(AAlias, ' ') then
        begin
          AField := TdxStringHelper.Remove(AField, ALastAsIndex);
          AExistingAlias := AAlias;
        end;
        {$ENDIF}
      end;
      if AExistingAlias = '' then
      begin
        AProperties[I] := Format('%s as F%d', [AField, I]);
        if I > 0 then
          AExpandedSelectedProperties.Append(', ');
        AExpandedSelectedProperties.Append(AExpandingAlias);
        AExpandedSelectedProperties.Append('.F');
        AExpandedSelectedProperties.Append(I);
      end
      else
      begin
        if I > 0 then
          AExpandedSelectedProperties.Append(', ');
        AExpandedSelectedProperties.Append(AExpandingAlias);
        AExpandedSelectedProperties.Append('.');
        AExpandedSelectedProperties.Append(AExistingAlias);
      end;
    end;
    Result := AExpandedSelectedProperties.ToString;
  finally
    AExpandedSelectedProperties.Free;
  end;
end;

class function TdxSimpleSQLParser.GetColumns(const ASQLText: string): TArray<string>;
var
  AParser: TdxSimpleSQLParser;
begin
  AParser := TdxSimpleSQLParser.Create(ASQLText);
  try
    Result := AParser.GetColumns;
  finally
    AParser.Free;
  end;
end;

function TdxSimpleSQLParser.GetColumns: TArray<string>;
var
  I: Integer;
  AAppend: Boolean;
begin
  for I := 0 to Length(FSQLText) - 1 do
  begin
    FChar := FSQLText[I + 1];
    FNextChr.Reset;
    FPrevChr.Reset;
    if I + 1 < Length(FSQLText) then
      FNextChr := FSQLText[I + 1 + 1];
    if I - 1 > -1 then
      FPrevChr := FSQLText[I + 1 - 1];
    case FChar of
      #$27:
        AAppend := Quote;
      '"':
        AAppend := DoubleQuotes;
      '(':
        AAppend := OpeningBracket;
      ')':
        AAppend := ClosingBracket;
      ',':
        AAppend := Comma;
      else
        AAppend := Default;
    end;
    Append(AAppend);
  end;
  Result := FResult.ToArray;
end;

function TdxSimpleSQLParser.Comma: Boolean;
begin
  if not FNextChr.HasValue then
    raise EdxFormatException.Create(sdxStatementNotFinished);
  Result := (FInBrackets <> 0) or FInQuotes or FInDoubleQuotes;
end;

procedure TdxSimpleSQLParser.Append(AColumnOnly: Boolean);
begin
  if AColumnOnly then
  begin
    FCurColumn.Append(FChar);
    Exit;
  end;
  if FChar <> ',' then
    FCurColumn.Append(FChar);
  FResult.Add(Trim(FCurColumn.ToString));
  FCurColumn.Remove(0, FCurColumn.Length);
end;

function TdxSimpleSQLParser.OpeningBracket: Boolean;
begin
  if not FNextChr.HasValue then
    raise EdxFormatException.Create(sdxStatementNotFinished);
  if FInDoubleQuotes then
  begin
    if not FNextChr.HasValue then
      raise EdxFormatException.Create(sdxStatementNotFinished);
    Exit(True);
  end;
  if FInQuotes then
  begin
    if not FNextChr.HasValue then
      raise EdxFormatException.Create(sdxStatementNotFinished);
    Exit(True);
  end;
  Inc(FInBrackets);
  Result := True;
end;

function TdxSimpleSQLParser.ClosingBracket: Boolean;
begin
  if FInDoubleQuotes then
  begin
    if not FNextChr.HasValue then
      raise EdxFormatException.Create(sdxStatementNotFinished);
    Exit(True);
  end;
  if FInQuotes then
  begin
    if not FNextChr.HasValue then
      raise EdxFormatException.Create(sdxStatementNotFinished);
    Exit(True);
  end;
  if FInBrackets < 1 then
    raise EdxFormatException.Create(sdxStatementNotFinished);
  Dec(FInBrackets);
  if FNextChr.HasValue then
    Exit(True);
  if FInBrackets <> 0 then
    raise EdxFormatException.Create(sdxStatementNotFinished);
  Result := False;
end;

function TdxSimpleSQLParser.Default: Boolean;
begin
  if FFirstQuote then
    FFirstQuote := False;
  Result := FNextChr.HasValue;
end;

function TdxSimpleSQLParser.Quote: Boolean;
begin
  if FInDoubleQuotes then
  begin
    if not FNextChr.HasValue then
      raise EdxFormatException.Create(sdxStatementNotFinished);
    Exit(True);
  end;
  if not FInQuotes then
  begin
    FInQuotes := True;
    FFirstQuote := True;
  end
  else
  begin
    FQuoteEscaped := not FQuoteEscaped;
    if FFirstQuote then
    begin
      FFirstQuote := False;
      if (not FNextChr.HasValue) or (FNextChr.Value <> #$27) then
      begin
        FInQuotes := False;
        FQuoteEscaped := False;
      end;
    end
    else
    begin
      if FQuoteEscaped then
        if FNextChr <> #$27 then
        begin
          FInQuotes := False;
          FQuoteEscaped := False;
        end;
    end;
  end;
  if FInQuotes and not FNextChr.HasValue then
    raise EdxFormatException.Create(sdxStatementNotFinished);
  Result := FNextChr.HasValue;
end;

function TdxSimpleSQLParser.DoubleQuotes: Boolean;
begin
  if FInQuotes then
  begin
    if not FNextChr.HasValue then
      raise EdxFormatException.Create(sdxStatementNotFinished);
    Exit(True);
  end;
  if not FInDoubleQuotes then
  begin
    FInDoubleQuotes := True;
    FFirstQuote := True;
  end
  else
  begin
    FQuoteEscaped := not FQuoteEscaped;
    if FFirstQuote then
    begin
      FFirstQuote := False;
      if (not FNextChr.HasValue) or (FNextChr.Value <> '"') then
      begin
        FInDoubleQuotes := False;
        FQuoteEscaped := False;
      end;
    end
    else
      if FQuoteEscaped then
        if FNextChr <> '"' then
        begin
          FInDoubleQuotes := False;
          FQuoteEscaped := False;
        end;
  end;
  if FInDoubleQuotes and not FNextChr.HasValue then
    raise EdxFormatException.Create(sdxStatementNotFinished);
  Result := FNextChr.HasValue;
end;

type

  TDataStream = class(TMemoryStream)
  public
    constructor Create(const ABytes: Pointer; ALength: Integer);
  end;

constructor TDataStream.Create(const ABytes: Pointer; ALength: Integer);
begin
  inherited Create;
  SetPointer(ABytes, ALength);
end;

{ TdxFieldValueConverter }

class procedure TdxFieldValueConverter.SetValue(AObject: TObject; const AFields: TArray<TField>; AMemberInfo: TdxMappingMemberInfo);
var
  AStream: TStream;
begin
  if Length(AFields) > 1 then
    NotImplemented;
  if AMemberInfo.IsSerialize then
  begin
    if (not AFields[0].IsNull) and (AFields[0].DataSize >= 0) then
    begin
      if AFields[0] is TBytesField then
        AStream := TBytesStream.Create(AFields[0].AsBytes)
      else
        AStream := AFields[0].DataSet.CreateBlobStream(AFields[0], bmRead);
      try
        AMemberInfo.Serializer.LoadFromStream(AMemberInfo.GetValue(AObject).AsObject, AStream);
      finally
        AStream.Free;
      end;
    end;
  end
  else
    AMemberInfo.SetValue(AObject, ToValue(AFields[0], AMemberInfo));
end;

class procedure TdxFieldValueConverter.SetValue(AObject: TObject; const AValues: TArray<Variant>;
  AMemberInfo: TdxMappingMemberInfo);
var
  ADataPtr: Pointer;
  ALength: Integer;
  AStream: TStream;
  ASerializedObject: TObject;
begin
  if Length(AValues) > 1 then
    NotImplemented;
  if AMemberInfo.IsSerialize then
  begin
    ASerializedObject := AMemberInfo.GetValue(AObject).AsObject;
    if ASerializedObject <> nil then
    begin
      ALength := VarArrayHighBound(AValues[0], 0);
      ADataPtr := VarArrayLock(AValues[0]);
      try
        AStream := TDataStream.Create(ADataPtr, ALength);
        try
          AMemberInfo.Serializer.LoadFromStream(ASerializedObject, AStream);
        finally
          AStream.Free;
        end;
      finally
        VarArrayUnlock(AValues[0]);
      end;
    end;
  end
  else
    AMemberInfo.SetValue(AObject, TValue.FromVariant(AValues[0]));
end;

function StrToBoolean(const AValue: string): Boolean; overload;
begin
  Result := (Length(AValue) > 0) and
    (CharInSet(AValue[1], ['T', 't', 'Y', 'y']) or (AValue <> '0'));
end;

function StrToBoolean(const AValue: AnsiString): Boolean; overload;
begin
  Result := (Length(AValue) > 0) and
    (CharInSet(AValue[1], ['T', 't', 'Y', 'y']) or (AValue <> '0'));
end;

class function TdxFieldValueConverter.ToValue(AField: TField; AMemberInfo: TdxMappingMemberInfo): TValue;
var
  ACurrency: Currency;
  ADateTime: TDateTime;
  AMemoryStream: TdxMemoryStream;
  ABlobStream: TStream;
  ABytes: TBytes;
  S: string;
begin
  if AField.IsNull then
    Exit(TValue.Empty);
  if AMemberInfo.IsType(TypeInfo(TGUID)) and (AField.DataType <> ftGuid) then
  begin
    if AField.AsString = '' then
      Exit(TValue.From<TGUID>(TGUID.Empty))
    else
      Exit(TValue.From<TGUID>(dxStringToGUID(AField.AsString)));
  end;
  if AMemberInfo.FieldType <> AField.DataType then
  begin
    case AMemberInfo.FieldType of
      ftBoolean:
        begin
          case AField.DataType of
            ftString, ftFixedChar, ftWideString:
              Exit(StrToBoolean(AField.AsString));
            else
              Exit(TValue.From<Boolean>(AField.Value));
          end;
        end;
      ftCurrency:
        Exit(TValue.From<Currency>(AField.AsCurrency));
      ftDate, ftDateTime:
        case AField.DataType of
          ftWideString, ftFixedWideChar:
            begin
              ADateTime := VarToDateTime(AField.AsString);
              Exit(TValue.From<TDateTime>(ADateTime));
            end;
        end;
      ftBlob:
        begin
          if AMemberInfo.IsDynamicArrayType then
          begin
            if (AField is TBytesField) then
            begin
              if AField.IsNull then
                Exit(TValue.Empty)
              else
                Exit(TValue.From<TBytes>(AField.AsBytes));
            end
            else
            begin
              AMemoryStream := TdxMemoryStream.Create;
              try
                ABlobStream := AField.DataSet.CreateBlobStream(AField, bmRead);
                try
                  AMemoryStream.CopyFrom(ABlobStream, 0);
                  Exit(TValue.From<TBytes>(AMemoryStream.ToArray));
                finally
                  ABlobStream.Free;
                end;
              finally
                AMemoryStream.Free;
              end;
            end;
          end;
        end;
      ftFixedWideChar:
        if AMemberInfo.IsType(TypeInfo(Char)) then
          begin
            S := AField.AsString;
            if S = '' then
              Exit(TValue.From<Char>(#0))
            else
              Exit(TValue.From<Char>(S[1]));
          end;
    end;
  end;
  case AField.DataType of
    ftString:
      Result := AField.AsString;
    ftSmallint:
      Result := TValue.From<Smallint>(AField.AsInteger);
    ftInteger:
      Result := AField.AsInteger;
    ftWord:
      Result := TValue.From<Word>(AField.AsInteger);
    ftBoolean:
      Result := AField.AsBoolean;
    ftFloat:
      case AMemberInfo.TypeKind of
        tkInteger:
          Result := AField.AsInteger;
        tkFloat:
          Result := AField.AsFloat;
        tkInt64:
          Result := AField.AsLargeInt;
        else
          Result := TValue.FromVariant(AField.AsVariant);
      end;
    ftCurrency:
      Result := TValue.From<Currency>(AField.AsCurrency);
    ftBCD:
      case AMemberInfo.TypeKind of
        tkInteger:
          Result := AField.AsInteger;
        tkFloat:
          begin
            case AMemberInfo.MemberType.Handle.TypeData.FloatType of
              ftCurr:
                begin
                  BCDToCurr(AField.AsBCD, ACurrency);
                  Result := TValue.From<Currency>(ACurrency);
                end;
              else
                Result := AField.AsExtended;
            end;
          end;
        tkEnumeration, tkInt64:
          Result := AField.AsLargeInt;
        else
          Result := TValue.From<TBCD>(AField.AsBCD);
      end;
    ftDate:
      Result := TValue.From<TDate>(AField.AsDateTime);
    ftTime:
      Result := TValue.From<TTime>(AField.AsDateTime);
    ftDateTime:
      Result := TValue.From<TDateTime>(AField.AsDateTime);
    ftFixedChar, ftWideString:
      begin
        if AMemberInfo.IsType(TypeInfo(Boolean)) then
          Result := StrToBoolean(AField.AsString)
        else
          Result := AField.AsString;
      end;
    ftLargeint, ftAutoInc:
      Result := AField.AsLargeInt;
    ftVariant:
      Result := TValue.FromVariant(AField.AsVariant);
    ftGuid:
      if AField is TGuidField then
        Result := TValue.From<TGUID>(TGuidField(AField).AsGuid)
      else
        Result := TValue.From<TGUID>(dxStringToGUID(AField.AsString));
    ftTimeStamp:
      begin
        ADateTime := SQLTimeStampToDateTime(AField.AsSQLTimeStamp);
        if AMemberInfo.IsType(TypeInfo(TTime)) then
          Result := TValue.From<TTime>(TimeOf(ADateTime))
        else
          Result := TValue.From<TDateTime>(ADateTime);
      end;
    ftFMTBcd:
      case AMemberInfo.TypeKind of
        tkInt64:
          Result := AField.AsLargeInt;
        tkInteger:
          Result := AField.AsInteger;
      else
        Result := TValue.From<TBCD>(AField.AsBCD);
      end;
    ftFixedWideChar, ftWideMemo:
      Result := AField.AsString;
    ftLongWord:
      Result := {$IFDEF DELPHIXE3}AField.AsLongWord{$ELSE}LongWord(AField.AsLargeInt){$ENDIF};
    ftShortint:
      Result := TValue.From<Shortint>(AField.AsInteger);
    ftByte:
      Result := TValue.From<Byte>(AField.AsInteger);
    TFieldType.ftExtended:
      Result := AField.AsFloat;
    ftTimeStampOffset:
      Result := TValue.From<TDateTime>(SQLTimeStampOffsetToDateTime(AField.AsSQLTimeStampOffset));
    TFieldType.ftSingle:
      Result := TValue.From<Single>(AField.AsFloat);
    TFieldType.ftBytes, TFieldType.ftBlob:
      begin
        if AMemberInfo.IsDynamicArrayType then
        begin
          if AField is TBinaryField then
            ABytes := TBinaryField(AField).AsBytes
          else
          if AField is TBlobField then
            ABytes := TBlobField(AField).Value
          else
            NotImplemented;
          Result := AMemberInfo.ConvertArray(ABytes);
        end
        else
          Result := TValue.FromVariant(AField.AsVariant);
      end
    else
      Result := TValue.FromVariant(AField.AsVariant);
  end;
end;

class function TdxFieldValueConverter.ToValues(const AFields: TArray<TField>;
  AMemberInfo: TdxMappingMemberInfo): TArray<TValue>;
var
  I: Integer;
begin
  SetLength(Result, Length(AFields));
  for I := 0 to Length(AFields) - 1 do
    Result[I] := ToValue(AFields[I], AMemberInfo);
end;

end.
