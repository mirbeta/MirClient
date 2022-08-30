{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
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

unit cxFormats;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages, SysUtils,
  Classes, cxDateUtils, cxClasses, cxControls, dxCore;

type
  IcxFormatControllerListener = interface
    ['{A7F2F6D3-1A7D-4295-A6E6-9297BD83D0DE}']
    procedure FormatChanged;
  end;

  IcxFormatControllerListener2 = interface
  ['{5E33A2A7-0C77-415F-A359-112103E54937}']
    procedure TimeChanged;
  end;

  TcxDateTimeEditMaskKind = (dtmkDate, dtmkTime, dtmkDateTime);

  TcxDateTimeFormatItemKind = (
    dtikString,
    dtikYear,          // YY YYYY
    dtikMonth,         // M MM MMM MMMM
    dtikDay,           // D DD DDD DDDD
    dtikHour,          // 12H 12HH 24H 24HH
    dtikMin,           // N NN
    dtikSec,           // S SS
    dtikMSec,          // Z ZZ ZZZ
    dtikTimeSuffix,    // A/P AM/PM AMPM (lower, upper, or mixed case)
    dtikDateSeparator,
    dtikTimeSeparator
  );

  TcxTimeSuffixKind = (tskAP, tskAMPM, tskAMPMString);

  TcxDateTimeFormatItem = class
  public
    Data: string;
    Kind: TcxDateTimeFormatItemKind;
  end;

  TcxDateTimeFormatItemKindArray = array[TcxDateTimeFormatItemKind] of Boolean;

  TcxDateTimeFormatInfo = class
  private
    FDefinedItems: TcxDateTimeFormatItemKindArray;
    FItems: TcxObjectList;

    function GetItemCount: Integer;
    function GetDefinedItems(AKind: TcxDateTimeFormatItemKind): Boolean;
    function GetItems(AIndex: Integer): TcxDateTimeFormatItem;
    procedure SetDefinedItems(AKind: TcxDateTimeFormatItemKind; AValue: Boolean);

    procedure ClearItems;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddItem(AKind: TcxDateTimeFormatItemKind; AData: string);
    procedure RemoveItem(AIndex: Integer);

    procedure ClearFormatInfo;

    property DefinedItems[AKind: TcxDateTimeFormatItemKind]: Boolean read GetDefinedItems write SetDefinedItems;
    property Items[AIndex: Integer]: TcxDateTimeFormatItem read GetItems;
    property ItemCount: Integer read GetItemCount;
  end;

  TcxDateTimeFormatItemInfo = record
    Kind: TcxDateTimeFormatItemKind;
    ItemZoneStart, ItemZoneLength: Integer;
    TimeSuffixKind: TcxTimeSuffixKind;
  end;

  { TcxFormatController }

  TcxFormatController = class(TcxMessageWindow, IUnknown, IdxLocalizerListener)
  private
    FAssignedCurrencyFormat: Boolean;
    FAssignedRegExprDateEditMask: Boolean;
    FAssignedRegExprDateTimeEditMask: Boolean;
    FAssignedStandardDateEditMask: Boolean;
    FAssignedStandardDateTimeEditMask: Boolean;
    FCurrencyFormat: string;
    FDateEditFormat: string;
    FDateEditMask: string;
    FFirstWeekOfYear: TcxFirstWeekOfYear;
    FList: TList;
    FLocalFormatSettings: TFormatSettings;
    FLocalIsGregorianCalendar: Boolean;
    FLockCount: Integer;
    FMaskedDateEditFormat: string;
    FMaskedDateTimeEditFormat: string;
    FRegExprDateEditMask: string;
    FRegExprDateTimeEditMask: string;
    FStandardDateEditMask: string;
    FStandardDateTimeEditMask: string;
    FUseDelphiDateTimeFormats: Boolean;

    FDateFormatInfo: TcxDateTimeFormatInfo;
    FDateTimeFormatInfo: TcxDateTimeFormatInfo;
    FTimeFormatInfo: TcxDateTimeFormatInfo;

    procedure CalculateDateEditMasks(AUseSmartInputWhenRegExpr: Boolean);
    function GetAssignedStartOfWeek: Boolean;
    function GetCurrencyFormat: string;
    function GetDateEditFormat(AIsMasked: Boolean): string;
    function GetDateTimeDisplayFormat(AMaskKind: TcxDateTimeEditMaskKind): string;
    class function GetDateTimeFormatItemStandardMaskZoneLength(
      const AItem: TcxDateTimeFormatItem): Integer;
    function GetStartOfWeek: TDay;
    function InternalGetDateTimeEditRegExprMask(
      AFormatInfo: TcxDateTimeFormatInfo;
      AMaskKind: TcxDateTimeEditMaskKind): string;
    function InternalGetDateTimeEditStandardMask(
      AFormatInfo: TcxDateTimeFormatInfo;
      AMaskKind: TcxDateTimeEditMaskKind): string;
    function InternalGetMaskedDateEditFormat(
      AFormatInfo: TcxDateTimeFormatInfo): string;
    procedure SetAssignedCurrencyFormat(Value: Boolean);
    procedure SetAssignedRegExprDateEditMask(Value: Boolean);
    procedure SetAssignedRegExprDateTimeEditMask(Value: Boolean);
    procedure SetAssignedStandardDateEditMask(Value: Boolean);
    procedure SetAssignedStandardDateTimeEditMask(Value: Boolean);
    procedure SetAssignedStartOfWeek(Value: Boolean);
    procedure SetCurrencyFormat(const Value: string);
    procedure SetFirstWeekOfYear(Value: TcxFirstWeekOfYear);
    procedure SetRegExprDateEditMask(const Value: string);
    procedure SetRegExprDateTimeEditMask(const Value: string);
    procedure SetStandardDateEditMask(const Value: string);
    procedure SetStandardDateTimeEditMask(const Value: string);
    procedure SetStartOfWeek(Value: TDay);
    procedure SetUseDelphiDateTimeFormats(Value: Boolean);
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure FormatChanged;
    procedure TimeChanged;
    procedure UpdateLocalSettings;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddListener(AListener: IcxFormatControllerListener); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure GetFormats;
    class function GetDateTimeFormatItemStandardMaskInfo(
      const AFormatInfo: TcxDateTimeFormatInfo; APos: Integer;
      out AItemInfo: TcxDateTimeFormatItemInfo): Boolean;
    function GetDateTimeStandardMaskStringLength(
      const AFormatInfo: TcxDateTimeFormatInfo): Integer;
    procedure NotifyListeners;
    procedure RemoveListener(AListener: IcxFormatControllerListener); virtual;
    function RegExprCustomDateEditMask(const AFormatString: string): string;

    //  IdxLocalizerListener
    procedure TranslationChanged;

    property AssignedCurrencyFormat: Boolean read FAssignedCurrencyFormat write SetAssignedCurrencyFormat;
    property AssignedRegExprDateEditMask: Boolean read FAssignedRegExprDateEditMask write SetAssignedRegExprDateEditMask;
    property AssignedRegExprDateTimeEditMask: Boolean read FAssignedRegExprDateTimeEditMask write SetAssignedRegExprDateTimeEditMask;
    property AssignedStandardDateEditMask: Boolean read FAssignedStandardDateEditMask write SetAssignedStandardDateEditMask;
    property AssignedStandardDateTimeEditMask: Boolean read FAssignedStandardDateTimeEditMask write SetAssignedStandardDateTimeEditMask;
    property AssignedStartOfWeek: Boolean read GetAssignedStartOfWeek write SetAssignedStartOfWeek;
    property CurrencyFormat: string read FCurrencyFormat write SetCurrencyFormat;
    property DateEditFormat: string read FDateEditFormat;
    property DateEditMask: string read FDateEditMask;
    property DateFormatInfo: TcxDateTimeFormatInfo read FDateFormatInfo;
    property DateTimeFormatInfo: TcxDateTimeFormatInfo read FDateTimeFormatInfo;
    property FirstWeekOfYear: TcxFirstWeekOfYear read FFirstWeekOfYear write SetFirstWeekOfYear;
    property MaskedDateEditFormat: string read FMaskedDateEditFormat;
    property MaskedDateTimeEditFormat: string read FMaskedDateTimeEditFormat;
    property RegExprDateEditMask: string read FRegExprDateEditMask write SetRegExprDateEditMask;
    property RegExprDateTimeEditMask: string read FRegExprDateTimeEditMask write SetRegExprDateTimeEditMask;
    property StandardDateEditMask: string read FStandardDateEditMask write SetStandardDateEditMask;
    property StandardDateTimeEditMask: string read FStandardDateTimeEditMask write SetStandardDateTimeEditMask;
    property StartOfWeek: TDay read GetStartOfWeek write SetStartOfWeek;
    property TimeFormatInfo: TcxDateTimeFormatInfo read FTimeFormatInfo;
    property UseDelphiDateTimeFormats: Boolean read FUseDelphiDateTimeFormats write SetUseDelphiDateTimeFormats;
    // actual local settings
    property LocalFormatSettings: TFormatSettings read FLocalFormatSettings;
    property LocalIsGregorianCalendar: Boolean read FLocalIsGregorianCalendar;
  end;

  { TdxFormatSettings }

  TdxFormatSettings = class
  private
    function GetCurrencyDecimals: Byte; inline;
    function GetCurrencyFormat: Byte; inline;
    function GetCurrencyString: string; inline;
    function GetDateSeparator: Char; inline;
    function GetDecimalSeparator: Char; inline;
    function GetListSeparator: Char; inline;
    function GetLongDateFormat: string; inline;
    function GetLongDayNames(AIndex: Integer): string; inline;
    function GetLongMonthNames(AIndex: Integer): string; inline;
    function GetLongTimeFormat: string; inline;
    function GetNegCurrFormat: Byte; inline;
    function GetShortDateFormat: string; inline;
    function GetShortDayNames(AIndex: Integer): string; inline;
    function GetShortMonthNames(AIndex: Integer): string; inline;
    function GetShortTimeFormat: string; inline;
    function GetThousandSeparator: Char; inline;
    function GetTimeAMString: string; inline;
    function GetTimePMString: string; inline;
    function GetTimeSeparator: Char; inline;
    procedure SetCurrencyDecimals(AValue: Byte); inline;
    procedure SetCurrencyString(const AValue: string); inline;
    procedure SetDateSeparator(AValue: Char); inline;
    procedure SetDecimalSeparator(AValue: Char); inline;
    procedure SetLongDayNames(AIndex: Integer; const Value: string); inline;
    procedure SetLongMonthNames(AIndex: Integer; const Value: string); inline;
    procedure SetShortDateFormat(const AValue: string); inline;
    procedure SetShortDayNames(AIndex: Integer; const AValue: string); inline;
    procedure SetShortMonthNames(AIndex: Integer; const AValue: string); inline;
    procedure SetThousandSeparator(AValue: Char); inline;
  public
    property CurrencyDecimals: Byte read GetCurrencyDecimals write SetCurrencyDecimals;
    property CurrencyFormat: Byte read GetCurrencyFormat;
    property CurrencyString: string read GetCurrencyString write SetCurrencyString;
    property DateSeparator: Char read GetDateSeparator write SetDateSeparator;
    property DecimalSeparator: Char read GetDecimalSeparator write SetDecimalSeparator;
    property ListSeparator: Char read GetListSeparator;
    property LongDateFormat: string read GetLongDateFormat;
    property LongDayNames[AIndex: Integer]: string read GetLongDayNames write SetLongDayNames;
    property LongMonthNames[AIndex: Integer]: string read GetLongMonthNames write SetLongMonthNames;
    property LongTimeFormat: string read GetLongTimeFormat;
    property NegCurrFormat: Byte read GetNegCurrFormat;
    property ShortDateFormat: string read GetShortDateFormat write SetShortDateFormat;
    property ShortDayNames[AIndex: Integer]: string read GetShortDayNames write SetShortDayNames;
    property ShortMonthNames[AIndex: Integer]: string read GetShortMonthNames write SetShortMonthNames;
    property ShortTimeFormat: string read GetShortTimeFormat;
    property ThousandSeparator: Char read GetThousandSeparator write SetThousandSeparator;
    property TimeAMString: string read GetTimeAMString;
    property TimePMString: string read GetTimePMString;
    property TimeSeparator: Char read GetTimeSeparator;
  end;

function cxFormatController: TcxFormatController;
function GetCharString(C: Char; ACount: Integer): string;
procedure GetDateTimeFormatInfo(const AFormat: string; var AFormatInfo: TcxDateTimeFormatInfo);
function DefaultCurrencyDisplayFormat: string;

var
// FormatSettings
  dxFormatSettings: TdxFormatSettings;

implementation

uses
  Forms, DateUtils, dxCalendarUtils;

var
  FcxFormatController: TcxFormatController;

function GetLongTimeFormat: string;

  function GetCount(const S: string; var AStartIndex: Integer; AStartSymbol: Char): Integer;
  begin
    Result := 1;
    while (AStartIndex <= Length(S)) and (S[AStartIndex] = AStartSymbol) do
    begin
      Inc(AStartIndex);
      Inc(Result);
    end;
  end;

  function ConvertLocalFormatToDelphi(const S: string): string;
  var
    I: Integer;
  begin
    Result := '';
    I := 1;
    while (I <= Length(S)) do
    begin
      if dxCharInSet(S[I], ['t', 'T']) then
      begin
        if GetCount(S, I, S[I]) > 1 then
          Result := Result + 'AMPM'
        else
          Result := Result + 'A/P';
      end
      else
        Result := Result + S[I];
      Inc(I);
    end;
  end;

begin
  if cxFormatController.UseDelphiDateTimeFormats then
    Result := dxFormatSettings.LongTimeFormat
  else
    Result := ConvertLocalFormatToDelphi(cxGetLocalLongTimeFormat);
end;

function GetCharString(C: Char; ACount: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to ACount do
    Result := Result + C;
end;

function CharLength(const S: string; Index: Integer): Integer;
begin
  Result := 1;
  assert((Index > 0) and (Index <= Length(S)));
  if SysLocale.FarEast and dxCharInSet(S[Index], LeadBytes) then
    Result := cxStrCharLength(S, Index);
end;

procedure GetDateTimeFormatInfo(const AFormat: string;
  var AFormatInfo: TcxDateTimeFormatInfo);
var
  A12HourFormat: Boolean;

  procedure AddFormatItem(AItemKind: TcxDateTimeFormatItemKind;
    const AItemData: string);
  begin
    if not(AItemKind in [dtikString, dtikDateSeparator, dtikTimeSeparator]) and
      AFormatInfo.DefinedItems[AItemKind] then
        Exit;
    AFormatInfo.AddItem(AItemKind, AItemData);
    AFormatInfo.DefinedItems[AItemKind] := True;
  end;

  procedure AppendChars(const S: string; AStartIndex, ACount: Integer);
  var
    AFormatItem: TcxDateTimeFormatItem;
  begin
    if (AFormatInfo.ItemCount = 0) or
      (AFormatInfo.Items[AFormatInfo.ItemCount - 1].Kind <> dtikString) then
        AddFormatItem(dtikString, '');
    AFormatItem := AFormatInfo.Items[AFormatInfo.ItemCount - 1];
    AFormatItem.Data := AFormatItem.Data + Copy(S, AStartIndex, ACount);
  end;

  function GetCount(const AFormat: string; var AStartIndex: Integer;
    AStartSymbol: Char): Integer;
  begin
    Result := 1;
    while (AStartIndex <= Length(AFormat)) and (AFormat[AStartIndex] = AStartSymbol) do
    begin
      Inc(AStartIndex);
      Inc(Result);
    end;
  end;

  function ParseFormat(const AFormat: string; ARecursionDepth: Integer): Boolean;
  var
    ACount, APrevI, I: Integer;
    AFormatItemData: string;
    ALastToken, AStartSymbol, AToken: Char;
    AThereIsHourItem, AThereIsTimeSuffixItem: Boolean;
  begin
    Result := True;
    if (AFormat = '') or (ARecursionDepth = 2) then
      Exit;
    Inc(ARecursionDepth);
    ALastToken := ' ';
    AThereIsHourItem := False;
    AThereIsTimeSuffixItem := False;
    I := 1;
    while I <= Length(AFormat) do
    begin
      AStartSymbol := AFormat[I];
      if dxCharInSet(AStartSymbol, LeadBytes) then
      begin
        AppendChars(AFormat, I, CharLength(AFormat, I));
        Inc(I, CharLength(AFormat, I));
        ALastToken := ' ';
        Continue;
      end;
      Inc(I, CharLength(AFormat, I));
      AToken := AStartSymbol;
      if dxCharInSet(AToken, ['a'..'z']) then
        Dec(AToken, 32);
      if dxCharInSet(AToken, ['A'..'Z']) then
      begin
        if (AToken = 'M') and (ALastToken = 'H') then
          AToken := 'N';
        ALastToken := AToken;
      end;
      case AToken of
        'E', 'Y':
          begin
            ACount := GetCount(AFormat, I, AStartSymbol);
            if ACount <= 2 then
              AFormatItemData := 'YY'
            else
              AFormatItemData := 'YYYY';
            AddFormatItem(dtikYear, AFormatItemData);
          end;
        'G':
          begin
            Result := False;
            Break;
          end;
        'M':
          begin
            ACount := GetCount(AFormat, I, AStartSymbol);
            if ACount > 4 then
              ACount := 4;
            AddFormatItem(dtikMonth, GetCharString('M', ACount));
          end;
        'D':
          begin
            ACount := GetCount(AFormat, I, AStartSymbol);
            case ACount of
              1..4:
                AddFormatItem(dtikDay, GetCharString('D', ACount));
              5:
                Result := ParseFormat(dxFormatSettings.ShortDateFormat, ARecursionDepth);
              else
                Result := ParseFormat(dxFormatSettings.LongDateFormat, ARecursionDepth);
            end;
            if not Result then
              Break;
          end;
        'H':
          begin
            ACount := GetCount(AFormat, I, AStartSymbol);
            if ACount > 2 then
              ACount := 2;
            AddFormatItem(dtikHour, GetCharString('H', ACount));
            AThereIsHourItem := True;
          end;
        'N':
          begin
            ACount := GetCount(AFormat, I, AStartSymbol);
            if ACount > 2 then
              ACount := 2;
            AddFormatItem(dtikMin, GetCharString('N', ACount));
          end;
        'S':
          begin
            ACount := GetCount(AFormat, I, AStartSymbol);
            if ACount > 2 then
              ACount := 2;
            AddFormatItem(dtikSec, GetCharString('S', ACount));
          end;
        'T':
          begin
            ACount := GetCount(AFormat, I, AStartSymbol);
            if ACount = 1 then
              Result := ParseFormat(dxFormatSettings.ShortTimeFormat, ARecursionDepth)
            else
              Result := ParseFormat(GetLongTimeFormat, ARecursionDepth);
            if not Result then
              Break;
          end;
        'Z':
          begin
            ACount := GetCount(AFormat, I, AStartSymbol);
            if ACount > 3 then
              ACount := 3;
            AddFormatItem(dtikMSec, GetCharString('Z', ACount));
          end;
        'A':
          begin
            if SameText(Copy(AFormat, I - 1, 5), 'AM/PM') then
            begin
              AddFormatItem(dtikTimeSuffix, Copy(AFormat, I - 1, 5));
              Inc(I, 4);
              AThereIsTimeSuffixItem := True;
            end
            else if SameText(Copy(AFormat, I - 1, 3), 'A/P') then
            begin
              AddFormatItem(dtikTimeSuffix, Copy(AFormat, I - 1, 3));
              Inc(I, 2);
              AThereIsTimeSuffixItem := True;
            end
            else if SameText(Copy(AFormat, I - 1, 4), 'AMPM') then
            begin
              AddFormatItem(dtikTimeSuffix, 'AMPM');
              Inc(I, 3);
              AThereIsTimeSuffixItem := True;
            end
            else if SameText(Copy(AFormat, I - 1, 3), 'AAA') then
            begin
              if SameText(Copy(AFormat, I - 1, 4), 'AAAA') then
                ACount := 4
              else
                ACount := 3;
              AddFormatItem(dtikDay, GetCharString('D', ACount));
              Inc(I, ACount - 1);
            end
            else
              AppendChars(AStartSymbol, 1, 1);
          end;
        'C':
          begin
            GetCount(AFormat, I, AStartSymbol);
            Result := ParseFormat(dxFormatSettings.ShortDateFormat, ARecursionDepth);
            if not Result then
              Break;
            AppendChars(' ', 1, 1);
            Result := ParseFormat(GetLongTimeFormat, ARecursionDepth);
            if not Result then
              Break;
          end;
        '/':
          AddFormatItem(dtikDateSeparator, '');
        ':':
          AddFormatItem(dtikTimeSeparator, '');
        '''', '"':
          begin
            APrevI := I;
            while (I <= Length(AFormat)) and (AFormat[I] <> AStartSymbol) do
              if dxCharInSet(AFormat[I], LeadBytes) then
                Inc(I, CharLength(AFormat, I))
              else
                Inc(I);
            AppendChars(AFormat, APrevI, I - APrevI);
            if I <= Length(AFormat) then
              Inc(I);
          end;
      else
        AppendChars(AStartSymbol, 1, 1);
      end;
    end;
    if AThereIsHourItem then
      A12HourFormat := AThereIsTimeSuffixItem;
  end;

  procedure ProcessHourItem;
  var
    I: Integer;
    AFormatItem: TcxDateTimeFormatItem;
  begin
    if AFormatInfo.DefinedItems[dtikHour] then
    begin
      for I := 0 to AFormatInfo.ItemCount - 1 do
        if AFormatInfo.Items[I].Kind = dtikHour then
        begin
          AFormatItem := AFormatInfo.Items[I];
          if A12HourFormat then
            AFormatItem.Data := '12' + AFormatItem.Data
          else
            AFormatItem.Data := '24' + AFormatItem.Data;
          Break;
        end;
    end
    else
      if AFormatInfo.DefinedItems[dtikTimeSuffix] then
        for I := 0 to AFormatInfo.ItemCount - 1 do
          if AFormatInfo.Items[I].Kind = dtikTimeSuffix then
          begin
            AFormatInfo.DefinedItems[dtikTimeSuffix] := False;
            AFormatInfo.RemoveItem(I);
            Break;
          end;
  end;

var
  ARes: Boolean;
begin
  AFormatInfo.ClearFormatInfo;
  if AFormat <> '' then
    ARes := ParseFormat(AFormat, 0)
  else
    ARes := ParseFormat('C', 0);
  if not ARes then
    AFormatInfo.ClearFormatInfo
  else
    ProcessHourItem;
end;

function DefaultCurrencyDisplayFormat: string;
begin
  Result := cxFormatController.CurrencyFormat;
end;

{ TcxDateTimeFormatInfo }

constructor TcxDateTimeFormatInfo.Create;
begin
  inherited;
  FItems := TcxObjectList.Create;
end;

destructor TcxDateTimeFormatInfo.Destroy;
begin
  ClearFormatInfo;
  FreeAndNil(FItems);
  inherited;
end;

procedure TcxDateTimeFormatInfo.AddItem(AKind: TcxDateTimeFormatItemKind; AData: string);
var
  AItem: TcxDateTimeFormatItem;
begin
  AItem := TcxDateTimeFormatItem.Create;
  AItem.Kind := AKind;
  AItem.Data := AData;
  FItems.Add(AItem);
end;

procedure TcxDateTimeFormatInfo.ClearItems;
begin
  FItems.Clear;
end;

procedure TcxDateTimeFormatInfo.RemoveItem(AIndex: Integer);
begin
  FItems[AIndex].Free;
  FItems.Delete(AIndex);
end;

procedure TcxDateTimeFormatInfo.ClearFormatInfo;
var
  AFormatItemKind: TcxDateTimeFormatItemKind;
begin
  ClearItems;
  for AFormatItemKind := Low(TcxDateTimeFormatItemKind) to High(TcxDateTimeFormatItemKind) do
    DefinedItems[AFormatItemKind] := False;
end;

function TcxDateTimeFormatInfo.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxDateTimeFormatInfo.GetDefinedItems(AKind: TcxDateTimeFormatItemKind): Boolean;
begin
  Result := FDefinedItems[AKind];
end;

function TcxDateTimeFormatInfo.GetItems(AIndex: Integer): TcxDateTimeFormatItem;
begin
  Result := TcxDateTimeFormatItem(FItems[AIndex]);
end;

procedure TcxDateTimeFormatInfo.SetDefinedItems(AKind: TcxDateTimeFormatItemKind; AValue: Boolean);
begin
  FDefinedItems[AKind] := AValue;
end;

{ TcxFormatController }

constructor TcxFormatController.Create;
begin
  inherited Create;
  FList := TList.Create;
  FFirstWeekOfYear := fwySystem;
  FUseDelphiDateTimeFormats := False;

  FDateFormatInfo := TcxDateTimeFormatInfo.Create;
  FDateTimeFormatInfo := TcxDateTimeFormatInfo.Create;
  FTimeFormatInfo := TcxDateTimeFormatInfo.Create;

  GetFormats;
  dxResourceStringsRepository.AddListener(Self);
end;

destructor TcxFormatController.Destroy;
begin
  dxResourceStringsRepository.RemoveListener(Self);
  FreeAndNil(FDateFormatInfo);
  FreeAndNil(FDateTimeFormatInfo);
  FreeAndNil(FTimeFormatInfo);
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TcxFormatController.CalculateDateEditMasks(
  AUseSmartInputWhenRegExpr: Boolean);
begin
  GetDateTimeFormatInfo(GetDateTimeDisplayFormat(dtmkDate), FDateFormatInfo);
  GetDateTimeFormatInfo(GetDateTimeDisplayFormat(dtmkTime), FTimeFormatInfo);
  GetDateTimeFormatInfo(GetDateTimeDisplayFormat(dtmkDateTime),
    FDateTimeFormatInfo);

  FDateEditMask := InternalGetDateTimeEditStandardMask(FDateFormatInfo,
    dtmkDate);
  if not FAssignedStandardDateEditMask then
    FStandardDateEditMask := FDateEditMask;

  if not FAssignedRegExprDateEditMask then
  begin
    FRegExprDateEditMask := InternalGetDateTimeEditRegExprMask(FDateFormatInfo, dtmkDate);
    if AUseSmartInputWhenRegExpr then
      AddDateRegExprMaskSmartInput(FRegExprDateEditMask, False);
  end;

  if not FAssignedRegExprDateTimeEditMask then
  begin
    FRegExprDateTimeEditMask := InternalGetDateTimeEditRegExprMask(
      FDateFormatInfo, dtmkDate);
    FRegExprDateTimeEditMask := FRegExprDateTimeEditMask + ' '' ''(' +
      InternalGetDateTimeEditRegExprMask(FTimeFormatInfo, dtmkTime) + ')?';
    if AUseSmartInputWhenRegExpr then
      AddDateRegExprMaskSmartInput(FRegExprDateTimeEditMask, True);
  end;

  if not FAssignedStandardDateTimeEditMask then
    FStandardDateTimeEditMask := InternalGetDateTimeEditStandardMask(
      FDateTimeFormatInfo, dtmkDateTime);

  FMaskedDateEditFormat := InternalGetMaskedDateEditFormat(FDateFormatInfo);
  FMaskedDateTimeEditFormat := InternalGetMaskedDateEditFormat(FDateTimeFormatInfo);
end;

function TcxFormatController.GetAssignedStartOfWeek: Boolean;
begin
  Result := dxGetAssignedStartOfWeek;
end;

function TcxFormatController.GetCurrencyFormat: string;

  function GetPositiveCurrencyFormat(const AFormat, ACurrStr: string): string;
  begin
    case dxFormatSettings.CurrencyFormat of
      0: Result := ACurrStr + AFormat; { '$1' }
      1: Result := AFormat + ACurrStr; { '1$' }
      2: Result := ACurrStr + ' ' + AFormat; { '$ 1' }
      3: Result := AFormat + ' ' + ACurrStr; { '1 $' }
    end;
  end;

  function GetNegativeCurrencyFormat(const AFormat, ACurrStr: string): string;
  begin
    case dxFormatSettings.NegCurrFormat of
      0: Result := '(' + ACurrStr + AFormat + ')';
      1: Result := '-' + ACurrStr + AFormat;
      2: Result := ACurrStr + '-' + AFormat;
      3: Result := ACurrStr + AFormat + '-';
      4: Result := '(' + AFormat + ACurrStr + ')';
      5: Result := '-' + AFormat + ACurrStr;
      6: Result := AFormat + '-' + ACurrStr;
      7: Result := AFormat + ACurrStr + '-';
      8: Result := '-' + AFormat + ' ' + ACurrStr;
      9: Result := '-' + ACurrStr + ' ' + AFormat;
      10: Result := AFormat + ' ' + ACurrStr + '-';
      11: Result := ACurrStr + ' ' + AFormat + '-';
      12: Result := ACurrStr + ' ' + '-' + AFormat;
      13: Result := AFormat + '-' + ' ' + ACurrStr;
      14: Result := '(' + ACurrStr + ' ' + AFormat + ')';
      15: Result := '(' + AFormat + ' ' + ACurrStr + ')';
    end;
  end;

var
  ACurrStr: string;
  I: Integer;
  C: Char;
begin
  if dxFormatSettings.CurrencyDecimals > 0 then
    Result := GetCharString('0', dxFormatSettings.CurrencyDecimals)
  else
    Result := '';
  Result := ',0.' + Result;
  ACurrStr := '';
  for I := 1 to Length(dxFormatSettings.CurrencyString) do
  begin
    C := dxFormatSettings.CurrencyString[I];
    if (C = ',') or (C = '.') then
      ACurrStr := ACurrStr + '''' + C + ''''
    else
      ACurrStr := ACurrStr + C;
  end;
  Result := GetPositiveCurrencyFormat(Result, ACurrStr) + ';' +
    GetNegativeCurrencyFormat(Result, ACurrStr);
end;

function TcxFormatController.GetDateEditFormat(AIsMasked: Boolean): string;

  procedure CorrectForMaskEdit(var S: string);
  var
    APos, AStartPos: Integer;
  begin
    APos := Pos('M', S);
    if APos <> 0 then
    begin
      AStartPos := APos;
      while APos <= Length(S) do
        if S[APos] = 'M' then
          Inc(APos)
        else
          Break;
      if APos - AStartPos > 3 then
        Delete(S, AStartPos + 3, APos - AStartPos - 3);
    end;
  end;

var
  Format: string;
  I: Integer;
  ExistFirst: Boolean;
begin
  Format := dxFormatSettings.ShortDateFormat;
  Result := '';
  for I := 1 to Length(Format) do
  begin
    if (Format[I] = 'd') then
    begin
      ExistFirst := True;
      if (1 < I) and (Format[I - 1] = 'd') then ExistFirst := False;
      if (I < Length(Format)) and (Format[I + 1] = 'd') then ExistFirst := False;
      if ExistFirst then Result := Result + 'd';
    end;
    if (Format[I] = 'M') then
    begin
      ExistFirst := True;
      if (1 < I) and (Format[I - 1] = 'M') then ExistFirst := False;
      if (I < Length(Format)) and (Format[I + 1] = 'M') then ExistFirst := False;
      if ExistFirst then Result := Result + 'M';
    end;
    Result := Result + Format[I];
  end;
  if AIsMasked then CorrectForMaskEdit(Result);
end;

function TcxFormatController.GetDateTimeDisplayFormat(
  AMaskKind: TcxDateTimeEditMaskKind): string;
begin
  case AMaskKind of
    dtmkDate:
      Result := dxFormatSettings.ShortDateFormat;
    dtmkTime:
      Result := GetLongTimeFormat;
    dtmkDateTime:
      Result := dxFormatSettings.ShortDateFormat + ' ' + GetLongTimeFormat;
  end;
end;

class function TcxFormatController.GetDateTimeFormatItemStandardMaskZoneLength(
  const AItem: TcxDateTimeFormatItem): Integer;
begin
  case AItem.Kind of
    dtikString:
      Result := Length(AItem.Data);
    dtikYear:
      if Length(AItem.Data) = 2 then
        Result := 2
      else
        Result := 4;
    dtikMonth, dtikDay:
      if Length(AItem.Data) < 3 then
        Result := 2
      else
        Result := 3;
    dtikHour, dtikMin, dtikSec:
      Result := 2;
//        dtikMSec:
    dtikTimeSuffix:
      begin
        if UpperCase(AItem.Data) = 'A/P' then
          Result := 1
        else if UpperCase(AItem.Data) = 'AM/PM' then
          Result := 2
        else
        begin
          Result := Length(dxFormatSettings.TimeAMString);
          if Length(dxFormatSettings.TimePMString) > Result then
            Result := Length(dxFormatSettings.TimePMString);
        end;
      end;
    dtikDateSeparator, dtikTimeSeparator:
      Result := 1;
    else
      Result := 0;
  end;
end;

function TcxFormatController.GetStartOfWeek: TDay;
begin
  Result := dxGetStartOfWeek;
end;

function TcxFormatController.InternalGetDateTimeEditRegExprMask(
  AFormatInfo: TcxDateTimeFormatInfo;
  AMaskKind: TcxDateTimeEditMaskKind): string;

  procedure AddChar(var S: string; C: Char);
  begin
    if C = ' ' then
      S := S + ''' '''
    else
      S := S + '\' + C;
  end;

  procedure AddString(var ADst: string; const ASrc: string);
  begin
    ADst := ADst + '''' + ASrc + '''';
  end;

  function GetGenitiveForm(AMonthIndex: Integer): string;
  var
    ASystemTime: TSystemTime;
    ABuf: array[0..100] of Char;
  begin
    ASystemTime.wYear := YearOf(Now);
    ASystemTime.wMonth := AMonthIndex;
    ASystemTime.wDay := 1;
    if GetDateFormat(GetThreadLocale, 0, @ASystemTime, 'ddMMMM', ABuf, 100) <> 0 then
    begin
      Result := ABuf;
      Delete(Result, 1, 2);
    end
    else
      Result := dxFormatSettings.LongMonthNames[AMonthIndex];
  end;

  procedure AddLongMonthName(var ADst: string; AIndex: Integer);
  var
    AGenitiveForm: string;
  begin
    AddString(ADst, dxFormatSettings.LongMonthNames[AIndex]);
    AGenitiveForm := GetGenitiveForm(AIndex);
    if CompareText(AGenitiveForm, dxFormatSettings.LongMonthNames[AIndex]) <> 0 then
    begin
      ADst := ADst + '|';
      AddString(ADst, AGenitiveForm);
    end;
  end;

  procedure ProcessDateItem(var S: string;
    const AFormatItem: TcxDateTimeFormatItem);
  const
    reTwoDigitYearMask = '\d\d';
    reFourDigitYearMask = '\d\d\d\d';
    reMonthMask = '(0?[1-9]|1[012])';
    reDayMask = '([012]?[1-9]|[123]0|31)';
  var
    AUseLongMonthNames: Boolean;
    I: Integer;
  begin
    with AFormatItem do
      case Kind of
        dtikString:
          AddString(S, Data);
        dtikYear:
          if Length(Data) = 2 then
            Result := S + reTwoDigitYearMask
          else
            Result := S + reFourDigitYearMask;
        dtikMonth:
          begin
            S := S + '(' + reMonthMask + '|(';
            AUseLongMonthNames := Length(Data) = 4;
            if AUseLongMonthNames then
              AddLongMonthName(S, 1)
            else
              AddString(S, dxFormatSettings.ShortMonthNames[1]);
            for I := 2 to 12 do
            begin
              S := S + '|';
              if AUseLongMonthNames then
                AddLongMonthName(S, I)
              else
                AddString(S, dxFormatSettings.ShortMonthNames[I]);
            end;
            S := S + '))';
          end;
        dtikDay:
          S := S + reDayMask;
        dtikDateSeparator:
          if dxFormatSettings.DateSeparator <> #0 then
            AddChar(S, dxFormatSettings.DateSeparator);
      end;
  end;

  procedure ProcessTimeItem(var S: string;
    const AFormatItem: TcxDateTimeFormatItem);
  begin
    with AFormatItem do
      case Kind of
        dtikString:
          AddString(S, Data);
        dtikHour:
          begin
            if Copy(Data, 1, 2) = '12' then
              S := S + '(0?[1-9]|1[012])'
            else
              S := S + '([01]?\d|2[0-3])';
          end;
        dtikMin:
          S := S + '[0-5]?\d';
        dtikSec:
          S := S + '[0-5]?\d';
//        dtikMSec:
        dtikTimeSuffix:
          begin
            if UpperCase(Data) = 'A/P' then
              S := S + '(A|P)?'
            else if UpperCase(Data) = 'AM/PM' then
              S := S + '(AM|PM)?'
            else
              if (dxFormatSettings.TimeAMString <> '') or
                 (dxFormatSettings.TimePMString <> '') then
              begin
                S := S + '(''';
                if (dxFormatSettings.TimeAMString <> '') and (dxFormatSettings.TimePMString <> '') then
                  S := S + dxFormatSettings.TimeAMString + '''|''' + dxFormatSettings.TimePMString
                else
                  if dxFormatSettings.TimeAMString <> '' then
                    S := S + dxFormatSettings.TimeAMString
                  else
                    S := S + dxFormatSettings.TimePMString;
                S := S + ''')?';
              end;
          end;
        dtikTimeSeparator:
          if dxFormatSettings.TimeSeparator <> #0 then
            AddChar(S, dxFormatSettings.TimeSeparator);
      end;
  end;

var
  I: Integer;
begin
  Result := '';
  if AFormatInfo.ItemCount = 0 then
    Exit;
  for I := 0 to AFormatInfo.ItemCount - 1 do
    if (AFormatInfo.Items[I].Kind in [dtikString, dtikYear, dtikMonth, dtikDay, dtikDateSeparator]) and
      (AMaskKind in [dtmkDate, dtmkDateTime]) then
        ProcessDateItem(Result, AFormatInfo.Items[I])
    else
      if (AFormatInfo.Items[I].Kind in [dtikString, dtikHour, dtikMin, dtikSec, dtikMSec, dtikTimeSuffix, dtikTimeSeparator]) and
        (AMaskKind in [dtmkTime, dtmkDateTime]) then
          ProcessTimeItem(Result, AFormatInfo.Items[I]);
end;

function TcxFormatController.InternalGetDateTimeEditStandardMask(
  AFormatInfo: TcxDateTimeFormatInfo;
  AMaskKind: TcxDateTimeEditMaskKind): string;

  procedure AddChar(var S: string; C: Char);
  begin
    S := S + '\' + C;
  end;

var
  I, J: Integer;
begin
  Result := '';
  if AFormatInfo.ItemCount = 0 then
    Exit;
  if AMaskKind <> dtmkTime then
    Result := '!';
  for I := 0 to AFormatInfo.ItemCount - 1 do
    with AFormatInfo.Items[I] do
      case Kind of
        dtikString:
          for J := 1 to Length(Data) do
            AddChar(Result, Data[J]);
        dtikYear:
          if Length(Data) = 2 then
            Result := Result + '99'
          else
            Result := Result + '9999';
        dtikMonth:
          if Length(Data) < 3 then
            Result := Result + '99'
          else
            Result := Result + 'lll';
        dtikDay:
          if Length(Data) < 3 then
            Result := Result + '99'
          else
            Result := Result + 'lll';
        dtikHour, dtikMin, dtikSec:
          if AMaskKind = dtmkTime then
            Result := Result + '00'
          else
            Result := Result + '99';
//        dtikMSec:
        dtikTimeSuffix:
          begin
            if UpperCase(Data) = 'A/P' then
              Result := Result + 'c'
            else if UpperCase(Data) = 'AM/PM' then
              Result := Result + 'cc'
            else
            begin
              J := Length(dxFormatSettings.TimeAMString);
              if Length(dxFormatSettings.TimePMString) > J then
                J := Length(dxFormatSettings.TimePMString);
              Result := Result + GetCharString('c', J);
            end;
          end;
        dtikDateSeparator:
          Result := Result + '/';
        dtikTimeSeparator:
          Result := Result + ':';
      end;
  if AMaskKind = dtmkTime then
    Result := Result + ';1;0'
  else
    Result := Result + ';1; ';
end;

function TcxFormatController.InternalGetMaskedDateEditFormat(
  AFormatInfo: TcxDateTimeFormatInfo): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to AFormatInfo.ItemCount - 1 do
    with AFormatInfo.Items[I] do
      case Kind of
        dtikString:
          Result := Result + '''' + Data + '''';
        dtikYear:
          Result := Result + LowerCase(Data);
        dtikMonth:
          if Length(Data) < 3 then
            Result := Result + 'mm'
          else
            Result := Result + 'mmm';
        dtikDay:
          if Length(Data) < 3 then
            Result := Result + 'dd'
          else
            Result := Result + 'ddd';
        dtikHour:
          Result := Result + 'hh';
        dtikMin:
          Result := Result + 'nn';
        dtikSec:
          Result := Result + 'ss';
//        dtikMSec:
        dtikTimeSuffix:
          Result := Result + LowerCase(Data);
        dtikDateSeparator:
          Result := Result + '/';
        dtikTimeSeparator:
          Result := Result + ':';
      end;
end;

procedure TcxFormatController.AddListener(
  AListener: IcxFormatControllerListener);
begin
  with FList do
    if IndexOf(Pointer(AListener)) = -1 then
      Add(Pointer(AListener));
end;

procedure TcxFormatController.RemoveListener(
  AListener: IcxFormatControllerListener);
begin
  FList.Remove(Pointer(AListener));
end;

function TcxFormatController.RegExprCustomDateEditMask(const AFormatString: string): string;
const
  AMaskKindMap: array [Boolean] of TcxDateTimeEditMaskKind = (dtmkDate, dtmkDateTime);
var
  AFormatInfo: TcxDateTimeFormatInfo;
begin
  AFormatInfo := TcxDateTimeFormatInfo.Create;
  try
    GetDateTimeFormatInfo(AFormatString, AFormatInfo);
    Result := InternalGetDateTimeEditRegExprMask(AFormatInfo, AMaskKindMap[AFormatInfo.DefinedItems[dtikHour]]);
  finally
    FreeAndNil(AFormatInfo);
  end;
end;

procedure TcxFormatController.TranslationChanged;
begin
  FormatChanged;
end;

procedure TcxFormatController.GetFormats;
begin
  if FcxFormatController = nil then // to avoid stack overflow
    FcxFormatController := Self;
  if not FAssignedCurrencyFormat then
    FCurrencyFormat := GetCurrencyFormat;
  FDateEditFormat := GetDateEditFormat(False);
  UpdateLocalSettings;
  CalculateDateEditMasks(True);
end;

class function TcxFormatController.GetDateTimeFormatItemStandardMaskInfo(
  const AFormatInfo: TcxDateTimeFormatInfo; APos: Integer;
  out AItemInfo: TcxDateTimeFormatItemInfo): Boolean;

  function GetTimeSuffixKind(const AFormatItemData: string): TcxTimeSuffixKind;
  begin
    if UpperCase(AFormatItemData) = 'A/P' then
      Result := tskAP
    else if UpperCase(AFormatItemData) = 'AM/PM' then
      Result := tskAMPM
    else
      Result := tskAMPMString;
  end;

var
  AItemZoneStart, I: Integer;
  AItemZoneStarts: array of Integer;
begin
  Result := False;
  if (APos < 1) or (AFormatInfo.ItemCount = 0) then
    Exit;
  SetLength(AItemZoneStarts, AFormatInfo.ItemCount);
  AItemZoneStart := 1;
  for I := 0 to AFormatInfo.ItemCount - 1 do
  begin
    AItemZoneStarts[I] := AItemZoneStart;
    Inc(AItemZoneStart, GetDateTimeFormatItemStandardMaskZoneLength(AFormatInfo.Items[I]));
    if APos < AItemZoneStart then
    begin
      AItemInfo.Kind := AFormatInfo.Items[I].Kind;
      AItemInfo.ItemZoneStart := AItemZoneStarts[I];
      AItemInfo.ItemZoneLength := AItemZoneStart - AItemZoneStarts[I];
      if AItemInfo.Kind = dtikTimeSuffix then
        AItemInfo.TimeSuffixKind := GetTimeSuffixKind(AFormatInfo.Items[I].Data);
      Result := True;
      Break;
    end;
  end;
end;

function TcxFormatController.GetDateTimeStandardMaskStringLength(
  const AFormatInfo: TcxDateTimeFormatInfo): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AFormatInfo.ItemCount - 1 do
    Inc(Result, GetDateTimeFormatItemStandardMaskZoneLength(AFormatInfo.Items[I]));
end;

procedure TcxFormatController.NotifyListeners;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    IcxFormatControllerListener(FList[I]).FormatChanged;
end;

procedure TcxFormatController.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_SETTINGCHANGE) and (Message.WParam = 0) and
    (PChar(Message.LParam) = 'intl') then
  begin
    if Application.UpdateFormatSettings then
    begin
      SetThreadLocale(LOCALE_USER_DEFAULT);
      Sysutils.GetFormatSettings;
      GetFormats;
      NotifyListeners;
    end
    else
      UpdateLocalSettings;
    Message.Result := 0;
    Exit;
  end;
  if Message.Msg = WM_TIMECHANGE then
  begin
    TimeChanged;
    Message.Result := 0;
    Exit;
  end;
  inherited;
end;

procedure TcxFormatController.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TcxFormatController.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    NotifyListeners;
end;

procedure TcxFormatController.FormatChanged;
begin
  if FLockCount = 0 then
  begin
    GetFormats;
    NotifyListeners;
  end;
end;

procedure TcxFormatController.TimeChanged;
var
  I: Integer;
  AIntf: IcxFormatControllerListener2;
begin
  for I := 0 to FList.Count - 1 do
    if Supports(IcxFormatControllerListener(FList[I]),
      IcxFormatControllerListener2, AIntf) then
        AIntf.TimeChanged;
end;

procedure TcxFormatController.UpdateLocalSettings;
begin
  FLocalFormatSettings := cxGetLocalFormatSettings;
  FLocalIsGregorianCalendar := cxIsGregorianCalendar;
end;

function cxFormatController: TcxFormatController;
begin
  if FcxFormatController = nil then
    FcxFormatController := TcxFormatController.Create;
  Result := FcxFormatController;
end;

procedure TcxFormatController.SetAssignedCurrencyFormat(Value: Boolean);
begin
  if FAssignedCurrencyFormat <> Value then
  begin
    FAssignedCurrencyFormat := Value;
    FormatChanged;
  end;
end;

procedure TcxFormatController.SetAssignedRegExprDateEditMask(Value: Boolean);
begin
  if FAssignedRegExprDateEditMask <> Value then
  begin
    FAssignedRegExprDateEditMask := Value;
    FormatChanged;
  end;
end;

procedure TcxFormatController.SetAssignedRegExprDateTimeEditMask(Value: Boolean);
begin
  if FAssignedRegExprDateTimeEditMask <> Value then
  begin
    FAssignedRegExprDateTimeEditMask := Value;
    FormatChanged;
  end;
end;

procedure TcxFormatController.SetAssignedStandardDateEditMask(Value: Boolean);
begin
  if FAssignedStandardDateEditMask <> Value then
  begin
    FAssignedStandardDateEditMask := Value;
    FormatChanged;
  end;
end;

procedure TcxFormatController.SetAssignedStandardDateTimeEditMask(Value: Boolean);
begin
  if FAssignedStandardDateTimeEditMask <> Value then
  begin
    FAssignedStandardDateTimeEditMask := Value;
    FormatChanged;
  end;
end;

procedure TcxFormatController.SetAssignedStartOfWeek(Value: Boolean);
begin
  if AssignedStartOfWeek <> Value then
  begin
    if Value then
      dxSetStartOfWeek(dxGetLocalStartOfWeek)
    else
      dxResetAssignedStartOfWeek;
  end;
end;

procedure TcxFormatController.SetCurrencyFormat(const Value: string);
begin
  FAssignedCurrencyFormat := True;
  if FCurrencyFormat <> Value then
  begin
    FCurrencyFormat := Value;
    FormatChanged;
  end;
end;

procedure TcxFormatController.SetFirstWeekOfYear(Value: TcxFirstWeekOfYear);
begin
  if Value <> FFirstWeekOfYear then
  begin
    FFirstWeekOfYear := Value;
    FormatChanged;
  end;
end;

procedure TcxFormatController.SetRegExprDateEditMask(const Value: string);
begin
  FAssignedRegExprDateEditMask := True;
  if FRegExprDateEditMask <> Value then
  begin
    FRegExprDateEditMask := Value;
    FormatChanged;
  end;
end;

procedure TcxFormatController.SetRegExprDateTimeEditMask(const Value: string);
begin
  FAssignedRegExprDateTimeEditMask := True;
  if FRegExprDateTimeEditMask <> Value then
  begin
    FRegExprDateTimeEditMask := Value;
    FormatChanged;
  end;
end;

procedure TcxFormatController.SetStandardDateEditMask(const Value: string);
begin
  FAssignedStandardDateEditMask := True;
  if FStandardDateEditMask <> Value then
  begin
    FStandardDateEditMask := Value;
    FormatChanged;
  end;
end;

procedure TcxFormatController.SetStandardDateTimeEditMask(const Value: string);
begin
  FAssignedStandardDateTimeEditMask := True;
  if FStandardDateTimeEditMask <> Value then
  begin
    FStandardDateTimeEditMask := Value;
    FormatChanged;
  end;
end;

procedure TcxFormatController.SetStartOfWeek(Value: TDay);
begin
  if dxGetStartOfWeek <> Value then
  begin
    dxSetStartOfWeek(Value);
    FormatChanged;
  end;
end;

procedure TcxFormatController.SetUseDelphiDateTimeFormats(Value: Boolean);
begin
  if FUseDelphiDateTimeFormats <> Value then
  begin
    FUseDelphiDateTimeFormats := Value;
    FormatChanged;
  end;
end;

{ TdxFormatSettings }

function TdxFormatSettings.GetCurrencyDecimals: Byte;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.CurrencyDecimals;
{$ELSE}
  Result := SysUtils.CurrencyDecimals;
{$ENDIF}
end;

function TdxFormatSettings.GetCurrencyFormat: Byte;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.CurrencyFormat;
{$ELSE}
  Result := SysUtils.CurrencyFormat;
{$ENDIF}
end;

function TdxFormatSettings.GetCurrencyString: string;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.CurrencyString;
{$ELSE}
  Result := SysUtils.CurrencyString;
{$ENDIF}
end;

function TdxFormatSettings.GetDateSeparator: Char;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.DateSeparator;
{$ELSE}
  Result := SysUtils.DateSeparator;
{$ENDIF}
end;

function TdxFormatSettings.GetDecimalSeparator: Char;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.DecimalSeparator;
{$ELSE}
  Result := SysUtils.DecimalSeparator;
{$ENDIF}
end;

function TdxFormatSettings.GetListSeparator: Char;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.ListSeparator;
{$ELSE}
  Result := SysUtils.ListSeparator;
{$ENDIF}
end;

function TdxFormatSettings.GetLongDateFormat: string;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.LongDateFormat;
{$ELSE}
  Result := SysUtils.LongDateFormat;
{$ENDIF}
end;

function TdxFormatSettings.GetLongDayNames(AIndex: Integer): string;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.LongDayNames[AIndex];
{$ELSE}
  Result := SysUtils.LongDayNames[AIndex];
{$ENDIF}
end;

function TdxFormatSettings.GetLongMonthNames(AIndex: Integer): string;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.LongMonthNames[AIndex];
{$ELSE}
  Result := SysUtils.LongMonthNames[AIndex];
{$ENDIF}
end;

function TdxFormatSettings.GetLongTimeFormat: string;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.LongTimeFormat;
{$ELSE}
  Result := SysUtils.LongTimeFormat;
{$ENDIF}
end;

function TdxFormatSettings.GetNegCurrFormat: Byte;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.NegCurrFormat;
{$ELSE}
  Result := SysUtils.NegCurrFormat;
{$ENDIF}
end;

function TdxFormatSettings.GetShortDateFormat: string;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.ShortDateFormat;
{$ELSE}
  Result := SysUtils.ShortDateFormat;
{$ENDIF}
end;

function TdxFormatSettings.GetShortDayNames(AIndex: Integer): string;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.ShortDayNames[AIndex];
{$ELSE}
  Result := SysUtils.ShortDayNames[AIndex];
{$ENDIF}
end;

function TdxFormatSettings.GetShortMonthNames(AIndex: Integer): string;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.ShortMonthNames[AIndex];
{$ELSE}
  Result := SysUtils.ShortMonthNames[AIndex];
{$ENDIF}
end;

function TdxFormatSettings.GetShortTimeFormat: string;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.ShortTimeFormat;
{$ELSE}
  Result := SysUtils.ShortTimeFormat;
{$ENDIF}
end;

function TdxFormatSettings.GetThousandSeparator: Char;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.ThousandSeparator;
{$ELSE}
  Result := SysUtils.ThousandSeparator;
{$ENDIF}
end;

function TdxFormatSettings.GetTimeAMString: string;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.TimeAMString;
{$ELSE}
  Result := SysUtils.TimeAMString;
{$ENDIF}
end;

function TdxFormatSettings.GetTimePMString: string;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.TimePMString;
{$ELSE}
  Result := SysUtils.TimePMString;
{$ENDIF}
end;

function TdxFormatSettings.GetTimeSeparator: Char;
begin
{$IFDEF DELPHI15}
  Result := FormatSettings.TimeSeparator;
{$ELSE}
  Result := SysUtils.TimeSeparator;
{$ENDIF}
end;

procedure TdxFormatSettings.SetCurrencyDecimals(AValue: Byte);
begin
{$IFDEF DELPHI15}
  FormatSettings.CurrencyDecimals := AValue;
{$ELSE}
  SysUtils.CurrencyDecimals := AValue;
{$ENDIF}
end;

procedure TdxFormatSettings.SetCurrencyString(const AValue: string);
begin
{$IFDEF DELPHI15}
  FormatSettings.CurrencyString := AValue;
{$ELSE}
  SysUtils.CurrencyString := AValue;
{$ENDIF}
end;

procedure TdxFormatSettings.SetDateSeparator(AValue: Char);
begin
{$IFDEF DELPHI15}
  FormatSettings.DateSeparator := AValue;
{$ELSE}
  SysUtils.DateSeparator := AValue;
{$ENDIF}
end;

procedure TdxFormatSettings.SetDecimalSeparator(AValue: Char);
begin
{$IFDEF DELPHI15}
  FormatSettings.DecimalSeparator := AValue;
{$ELSE}
  SysUtils.DecimalSeparator := AValue;
{$ENDIF}
end;

procedure TdxFormatSettings.SetLongDayNames(AIndex: Integer; const Value: string);
begin
{$IFDEF DELPHI15}
  FormatSettings.LongDayNames[AIndex] := Value;
{$ELSE}
  SysUtils.LongDayNames[AIndex] := Value;
{$ENDIF}
end;

procedure TdxFormatSettings.SetLongMonthNames(AIndex: Integer; const Value: string);
begin
{$IFDEF DELPHI15}
  FormatSettings.LongMonthNames[AIndex] := Value;
{$ELSE}
  SysUtils.LongMonthNames[AIndex] := Value;
{$ENDIF}
end;

procedure TdxFormatSettings.SetShortDateFormat(const AValue: string);
begin
{$IFDEF DELPHI15}
  FormatSettings.ShortDateFormat := AValue;
{$ELSE}
  SysUtils.ShortDateFormat := AValue;
{$ENDIF}
end;

procedure TdxFormatSettings.SetShortDayNames(AIndex: Integer; const AValue: string);
begin
{$IFDEF DELPHI15}
  FormatSettings.ShortDayNames[AIndex] := AValue;
{$ELSE}
  SysUtils.ShortDayNames[AIndex] := AValue;
{$ENDIF}
end;

procedure TdxFormatSettings.SetShortMonthNames(AIndex: Integer; const AValue: string);
begin
{$IFDEF DELPHI15}
  FormatSettings.ShortMonthNames[AIndex] := AValue;
{$ELSE}
  SysUtils.ShortMonthNames[AIndex] := AValue;
{$ENDIF}
end;

procedure TdxFormatSettings.SetThousandSeparator(AValue: Char);
begin
{$IFDEF DELPHI15}
  FormatSettings.ThousandSeparator := AValue;
{$ELSE}
  SysUtils.ThousandSeparator := AValue;
{$ENDIF}
end;

initialization

finalization
  FreeAndNil(FcxFormatController);

end.
