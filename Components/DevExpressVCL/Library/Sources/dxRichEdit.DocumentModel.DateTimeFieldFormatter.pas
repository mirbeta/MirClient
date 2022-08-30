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

unit dxRichEdit.DocumentModel.DateTimeFieldFormatter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, cxDateUtils, dxCultureInfo,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.FieldFormatter;

type
  { TdxDateTimeFormattingItem }

  TdxDateTimeFormattingItem = class abstract
  strict private
    FCulture: TdxCultureInfo;
    function GetFormatInfo: TFormatSettings;
    function GetCalendar: TcxCustomCalendarTable;
  protected
    function GetPatternsLength: TArray<Integer>; virtual; abstract;

    property Culture: TdxCultureInfo read FCulture;
    property FormatInfo: TFormatSettings read GetFormatInfo;
    property Calendar: TcxCustomCalendarTable read GetCalendar;
  public
    constructor Create(const ACulture: TdxCultureInfo);
    function Format(ADateTime: TDateTime; APatternLength: Integer): string; virtual; abstract;
    function GetAvailablePatternLength(APatternLength: Integer): Integer;

    property PatternsLength: TArray<Integer> read GetPatternsLength;
  end;

  { TdxDateTimeFieldFormatter }

  TdxDateTimeFieldFormatter = class(TdxSpecificFieldFormatter<TDateTime>)
  strict private
    FAMPMKeyword: string;
    FDefaultFormat: string;
    FDateTime: TDateTime;
    FFormatString: string;
    FUseCurrentCultureDateTimeFormat: Boolean;
  protected
    function GetFormatInfo: TFormatSettings; virtual;
    function InternalFormat(const ADateTime: TDateTime; const AFormatString: string): string; override;
    function FormatCore: string; virtual;
    function FormatByDefault(const AValue: TDateTime): string; override;
    function ProcessNextChar(AIndex: Integer; AResultString: TStringBuilder): Integer; virtual;
    function TryCreateItem(AFormattingChar: Char; out AResult: TdxDateTimeFormattingItem): Boolean;
    function ProcessAsFormattingItem(AIndex: Integer; AFormattingItem: TdxDateTimeFormattingItem; AResultString: TStringBuilder): Integer; virtual;
    function ProcessAsAMPMKeyword(AResultString: TStringBuilder): Integer; virtual;
    function ProcessAsEmbedText(AIndex: Integer; AResultString: TStringBuilder): Integer; virtual;
    function ProcessAsSingleCharacter(AIndex: Integer; AResultString: TStringBuilder): Integer; virtual;
    function IsKeyword(const AKeyword: string; AIndex: Integer): Boolean; virtual;
    function GetCharacterSequenceLength(ACh: Char; AIndex: Integer;
      const APredicate: TEqualityComparison<Char>): Integer;
  public
    constructor Create;

    property UseCurrentCultureDateTimeFormat: Boolean read FUseCurrentCultureDateTimeFormat write FUseCurrentCultureDateTimeFormat;
    property FormatInfo: TFormatSettings read GetFormatInfo;
  end;

  { TdxNumericFormattingItem }

  TdxNumericFormattingItem = class abstract(TdxDateTimeFormattingItem)
  strict private
    class var
      FPatternsLength: TArray<Integer>;
    class constructor Initialize;
  protected
    function GetPatternsLength: TArray<Integer>; override;
    function FormatInteger(AValue: Integer; APatternLength: Integer): string; virtual;
  end;

  { TdxCombinedFormattingItem }

  TdxCombinedFormattingItem = class abstract(TdxNumericFormattingItem)
  strict private
    class var
      FPatternsLength: TArray<Integer>;
    class constructor Initialize;
  protected
    function GetPatternsLength: TArray<Integer>; override;
    function GetNumericValue(ADateTime: TDateTime): Integer; virtual; abstract;
    function GetAbbreviatedName(ADateTime: TDateTime): string; virtual; abstract;
    function GetFullName(ADateTime: TDateTime): string; virtual; abstract;
  public
    function Format(ADateTime: TDateTime; APatternLength: Integer): string; override;
  end;

  { TdxHour24FormattingItem }

  TdxHour24FormattingItem = class(TdxNumericFormattingItem)
  public
    function Format(ADateTime: TDateTime; APatternLength: Integer): string; override;
  end;

  { TdxHour12FormattingItem }

  TdxHour12FormattingItem = class(TdxNumericFormattingItem)
  public
    function Format(ADateTime: TDateTime; APatternLength: Integer): string; override;
  end;

  { TdxMinuteFormattingItem }

  TdxMinuteFormattingItem = class(TdxNumericFormattingItem)
  public
    function Format(ADateTime: TDateTime; APatternLength: Integer): string; override;
  end;

  { TdxSecondFormattingItem }

  TdxSecondFormattingItem = class(TdxNumericFormattingItem)
  public
    function Format(ADateTime: TDateTime; APatternLength: Integer): string; override;
  end;

  { TdxDayFormattingItem }

  TdxDayFormattingItem = class(TdxCombinedFormattingItem)
  private
    function GetDayOfWeek(ADateTime: TDateTime): TdxDayOfWeekVCL;
  protected
    function GetNumericValue(ADateTime: TDateTime): Integer; override;
    function GetAbbreviatedName(ADateTime: TDateTime): string; override;
    function GetFullName(ADateTime: TDateTime): string; override;
  end;

  { TdxMonthFormattingItem }

  TdxMonthFormattingItem = class(TdxCombinedFormattingItem)
  protected
    function GetNumericValue(ADateTime: TDateTime): Integer; override;
    function GetAbbreviatedName(ADateTime: TDateTime): string; override;
    function GetFullName(ADateTime: TDateTime): string; override;
  end;

  { TdxYearFormattingItem }

  TdxYearFormattingItem = class(TdxDateTimeFormattingItem)
  strict private
    class var
      FPatternsLength: TArray<Integer>;
    class constructor Initialize;
  protected
    function GetPatternsLength: TArray<Integer>; override;
  public
    function Format(ADateTime: TDateTime; APatternLength: Integer): string; override;
  end;

implementation

uses
  Math;

{ TdxDateTimeFormattingItem }

constructor TdxDateTimeFormattingItem.Create(const ACulture: TdxCultureInfo);
begin
  inherited Create;
  FCulture := ACulture;
end;

function TdxDateTimeFormattingItem.GetFormatInfo: TFormatSettings;
begin
  Result := Culture.FormatSettings;
end;

function TdxDateTimeFormattingItem.GetCalendar: TcxCustomCalendarTable;
begin
  Result := Culture.Calendar;
end;

function TdxDateTimeFormattingItem.GetAvailablePatternLength(APatternLength: Integer): Integer;
var
  ACount, I: Integer;
  APatternsLength: TArray<Integer>;
begin
  APatternsLength := PatternsLength;
  ACount := Length(APatternsLength);
  Result := APatternsLength[ACount - 1];
  for I := 0 to ACount - 1 do
  begin
    if APatternsLength[I] >= APatternLength then
    begin
      Result := APatternsLength[I];
      Break;
    end;
  end;
end;

{ TdxDateTimeFieldFormatter }

constructor TdxDateTimeFieldFormatter.Create;
begin
  inherited Create;
  FAMPMKeyword := 'am/pm';
  FDefaultFormat := 'M/d/yyyy';
end;

function TdxDateTimeFieldFormatter.GetFormatInfo: TFormatSettings;
begin
  Result := Culture.FormatSettings;
end;

function TdxDateTimeFieldFormatter.InternalFormat(const ADateTime: TDateTime; const AFormatString: string): string;
begin
  FDateTime := ADateTime;
  FFormatString := AFormatString;
  Result := FormatCore;
end;

function TdxDateTimeFieldFormatter.FormatCore: string;
var
  AIndex, AFormatLength: Integer;
  AResultString: TStringBuilder;
begin
  AIndex := 1;
  AFormatLength := Length(FFormatString);
  AResultString := TStringBuilder.Create;
  try
    while AIndex <= AFormatLength do
      AIndex := AIndex + ProcessNextChar(AIndex, AResultString);
    Result := AResultString.ToString;
  finally
    AResultString.Free;
  end;
end;

function TdxDateTimeFieldFormatter.FormatByDefault(const AValue: TDateTime): string;
var
  AFormatString: string;
begin
  if FUseCurrentCultureDateTimeFormat then
    AFormatString := Culture.FormatSettings.ShortDateFormat
  else
    AFormatString := FDefaultFormat;
  DateTimeToString(Result, AFormatString, AValue, FormatInfo);
end;

function TdxDateTimeFieldFormatter.ProcessNextChar(AIndex: Integer; AResultString: TStringBuilder): Integer;
var
  ACh: Char;
  AFormattingItem: TdxDateTimeFormattingItem;
begin
  ACh := FFormatString[AIndex];
  if TryCreateItem(ACh, AFormattingItem) then
  try
    Exit(ProcessAsFormattingItem(AIndex, AFormattingItem, AResultString));
  finally
    AFormattingItem.Free;
  end;
  if IsKeyword(FAMPMKeyword, AIndex) then
    Exit(ProcessAsAMPMKeyword(AResultString));
  if ACh = '''' then
    Exit(ProcessAsEmbedText(AIndex, AResultString));
  Result := ProcessAsSingleCharacter(AIndex, AResultString);
end;

function TdxDateTimeFieldFormatter.TryCreateItem(AFormattingChar: Char;
  out AResult: TdxDateTimeFormattingItem): Boolean;
begin
  case AFormattingChar of
    'h':
      AResult := TdxHour12FormattingItem.Create(Culture);
    'H':
      AResult := TdxHour24FormattingItem.Create(Culture);
    'm':
      AResult := TdxMinuteFormattingItem.Create(Culture);
    'S',
    's':
      AResult := TdxSecondFormattingItem.Create(Culture);
    'Y',
    'y':
      AResult := TdxYearFormattingItem.Create(Culture);
    'M':
      AResult := TdxMonthFormattingItem.Create(Culture);
    'D',
    'd':
      AResult := TdxDayFormattingItem.Create(Culture);
    else
      AResult := nil;
      Exit(False);
  end;
  Result := True;
end;

function TdxDateTimeFieldFormatter.ProcessAsFormattingItem(AIndex: Integer;
  AFormattingItem: TdxDateTimeFormattingItem; AResultString: TStringBuilder): Integer;
var
  ASequenceLength, APatternLength: Integer;
  AResult: string;
begin
  ASequenceLength := GetCharacterSequenceLength(FFormatString[AIndex], AIndex,
    function(const Left, Right: Char): Boolean
    begin
      Result := Left = Right;
    end);
  APatternLength := AFormattingItem.GetAvailablePatternLength(ASequenceLength);
  AResult := AFormattingItem.Format(FDateTime, APatternLength);
  AResultString.Append(AResult);
  Result := Min(ASequenceLength, APatternLength);
end;

function TdxDateTimeFieldFormatter.ProcessAsAMPMKeyword(AResultString: TStringBuilder): Integer;
var
  AResult: string;
begin
  if (Culture.Calendar.FromDateTime(FDateTime).Hours - 12) >= 0 then
    AResult := 'PM'
  else
    AResult := 'AM';
  AResultString.Append(AResult);
  Result := Length(FAMPMKeyword);
end;

function TdxDateTimeFieldFormatter.ProcessAsEmbedText(AIndex: Integer; AResultString: TStringBuilder): Integer;
var
  AStartTextIndex, ATextLength: Integer;
begin
  AStartTextIndex := AIndex + 1;
  if AStartTextIndex >= Length(FFormatString) then
    Exit(1);
  ATextLength := GetCharacterSequenceLength(FFormatString[AIndex], AStartTextIndex,
    function(const Left, Right: Char): Boolean
    begin
      Result := Left <> Right;
    end);
  if (ATextLength + AStartTextIndex) > Length(FFormatString) then
    ThrowUnmatchedQuotesError;
  AResultString.Append(FFormatString, AStartTextIndex - 1, ATextLength);
  Result := ATextLength + 2;
end;

function TdxDateTimeFieldFormatter.ProcessAsSingleCharacter(AIndex: Integer; AResultString: TStringBuilder): Integer;
begin
  AResultString.Append(FFormatString[AIndex]);
  Result := 1;
end;

function TdxDateTimeFieldFormatter.IsKeyword(const AKeyword: string; AIndex: Integer): Boolean;
var
  ASubstring: string;
begin
  if Length(AKeyword) > (Length(FFormatString) - AIndex + 1) then
    Exit(False);
  ASubstring := Copy(FFormatString, AIndex, Length(AKeyword));
  Result := CompareText(AKeyword, ASubstring) = 0;
end;

function TdxDateTimeFieldFormatter.GetCharacterSequenceLength(ACh: Char; AIndex: Integer;
  const APredicate: TEqualityComparison<Char>): Integer;
var
  ALength, ANextCharIndex: Integer;
begin
  ALength := Length(FFormatString);
  ANextCharIndex := AIndex + 1;
  while (ANextCharIndex <= ALength) and APredicate(ACh, FFormatString[ANextCharIndex]) do
    Inc(ANextCharIndex);
  Result := ANextCharIndex - AIndex;
end;

{ TdxNumericFormattingItem }

class constructor TdxNumericFormattingItem.Initialize;
begin
  FPatternsLength := TArray<Integer>.Create(1, 2);
end;

function TdxNumericFormattingItem.GetPatternsLength: TArray<Integer>;
begin
  Result := FPatternsLength;
end;

function TdxNumericFormattingItem.FormatInteger(AValue: Integer; APatternLength: Integer): string;
begin
  Result := IntToStr(AValue);
  if (APatternLength = 2) and (Length(Result) = 1) then
    Exit('0' + Result);
end;

{ TdxCombinedFormattingItem }

class constructor TdxCombinedFormattingItem.Initialize;
begin
  FPatternsLength := TArray<Integer>.Create(1, 2, 3, 4);
end;

function TdxCombinedFormattingItem.GetPatternsLength: TArray<Integer>;
begin
  Result := FPatternsLength;
end;

function TdxCombinedFormattingItem.Format(ADateTime: TDateTime; APatternLength: Integer): string;
begin
  if APatternLength <= 2 then
    Exit(FormatInteger(GetNumericValue(ADateTime), APatternLength));
  if APatternLength = 3 then
    Exit(GetAbbreviatedName(ADateTime));
  Result := GetFullName(ADateTime);
end;

{ TdxHour24FormattingItem }

function TdxHour24FormattingItem.Format(ADateTime: TDateTime;
  APatternLength: Integer): string;
begin
  Result := FormatInteger(Calendar.FromDateTime(ADateTime).Hours, APatternLength);
end;

{ TdxHour12FormattingItem }

function TdxHour12FormattingItem.Format(ADateTime: TDateTime;
  APatternLength: Integer): string;
var
  AHour: Integer;
begin
  AHour := Calendar.FromDateTime(ADateTime).Hours mod 12;
  if AHour = 0 then
    AHour := 12;
  Result := FormatInteger(AHour, APatternLength);
end;

{ TdxMinuteFormattingItem }

function TdxMinuteFormattingItem.Format(ADateTime: TDateTime;
  APatternLength: Integer): string;
begin
  Result := FormatInteger(Calendar.FromDateTime(ADateTime).Minutes, APatternLength);
end;

{ TdxSecondFormattingItem }

function TdxSecondFormattingItem.Format(ADateTime: TDateTime;
  APatternLength: Integer): string;
begin
  Result := FormatInteger(Calendar.FromDateTime(ADateTime).Seconds, APatternLength);
end;

{ TdxDayFormattingItem }

function TdxDayFormattingItem.GetAbbreviatedName(ADateTime: TDateTime): string;
begin
  Result := FormatInfo.ShortDayNames[GetDayOfWeek(ADateTime)];
end;

function TdxDayFormattingItem.GetDayOfWeek(ADateTime: TDateTime): TdxDayOfWeekVCL;
begin
  Result := dxDayOfWeekToVCL(dxDayOfWeek(ADateTime));
end;

function TdxDayFormattingItem.GetFullName(ADateTime: TDateTime): string;
begin
  Result := FormatInfo.LongDayNames[GetDayOfWeek(ADateTime)];
end;

function TdxDayFormattingItem.GetNumericValue(ADateTime: TDateTime): Integer;
begin
  Result := Calendar.FromDateTime(ADateTime).Day;
end;

{ TdxMonthFormattingItem }

function TdxMonthFormattingItem.GetAbbreviatedName(
  ADateTime: TDateTime): string;
begin
  Result := FormatInfo.ShortMonthNames[GetNumericValue(ADateTime)];
end;

function TdxMonthFormattingItem.GetFullName(ADateTime: TDateTime): string;
begin
  Result := FormatInfo.LongMonthNames[GetNumericValue(ADateTime)];
end;

function TdxMonthFormattingItem.GetNumericValue(ADateTime: TDateTime): Integer;
begin
  Result := Calendar.FromDateTime(ADateTime).Month;
end;

{ TdxYearFormattingItem }

function TdxYearFormattingItem.Format(ADateTime: TDateTime;
  APatternLength: Integer): string;
var
  AYear, AShortYear: Integer;
begin
  AYear := Calendar.FromDateTime(ADateTime).Year;
  if (APatternLength = 2) and (AYear > 99) then
  begin
    AShortYear := AYear mod 100;
    Result := IntToStr(AShortYear);
    if Length(Result) = 1 then
      Result := '0' + Result;
  end
  else
    Result := IntToStr(AYear);
end;

function TdxYearFormattingItem.GetPatternsLength: TArray<Integer>;
begin
  Result := FPatternsLength;
end;

class constructor TdxYearFormattingItem.Initialize;
begin
  FPatternsLength := TArray<Integer>.Create(2, 4);
end;

end.
