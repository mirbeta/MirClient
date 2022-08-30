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

unit dxSpreadSheetFormatCSV;

{$I cxVer.Inc}

interface

uses
  Types, Variants, SysUtils, Classes, dxSpreadSheetCore, dxSpreadSheetClasses, dxCore, dxSpreadSheetTextFileFormatCore;

type
  TdxSpreadSheetCSVFormatExportSettings = class;

  { TdxSpreadSheetCSVFormat }

  TdxSpreadSheetCSVFormat = class(TdxSpreadSheetCustomFormat)
  public
    class function CanCheckByContent: Boolean; override;
    class function CreateFormatSettings: TdxSpreadSheetFormatSettings; override;
    class function GetDescription: string; override;
    class function GetExt: string; override;
    class function GetReader: TdxSpreadSheetCustomReaderClass; override;
    class function GetWriter: TdxSpreadSheetCustomWriterClass; override;
  end;

  { TdxSpreadSheetCSVFormatSettings }

  TdxSpreadSheetCSVFormatSettings = class(TPersistent)
  strict private
    FEncoding: TEncoding;
    FExport: TdxSpreadSheetCSVFormatExportSettings;
    FQuote: WideChar;
    FValueSeparator: WideChar;

    procedure SetExport(AValue: TdxSpreadSheetCSVFormatExportSettings);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset;
    //
    property Encoding: TEncoding read FEncoding write FEncoding;
    property Export: TdxSpreadSheetCSVFormatExportSettings read FExport write SetExport;
    property Quote: WideChar read FQuote write FQuote;
    property ValueSeparator: WideChar read FValueSeparator write FValueSeparator;
  end;

  { TdxSpreadSheetCSVFormatExportSettings }

  TdxSpreadSheetCSVFormatExportSettings = class(TPersistent)
  strict private
    FQuoteStringValues: Boolean;
    FUseDisplayValues: Boolean;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure Reset;
    //
    property QuoteStringValues: Boolean read FQuoteStringValues write FQuoteStringValues;
    property UseDisplayValues: Boolean read FUseDisplayValues write FUseDisplayValues;
  end;

  { TdxSpreadSheetCSVCustomParser }

  TdxSpreadSheetCSVCustomParser = class
  strict private
    FChars: PWideChar;
    FCount: Integer;
    FSettings: TdxSpreadSheetCSVFormatSettings;
    FValueContainsQuote: Boolean;
    FValueCursor: PWideChar;

    procedure GoToNext; inline;
    function LookInNext: WideChar; inline;
    procedure ProcessQuotedValue;
  protected
    procedure DoRowBegin; virtual;
    procedure DoRowEnd; virtual;
    procedure DoValue(const AValue: string; AIsQuotedValue: Boolean); virtual;
    procedure DoValueBegin; inline;
    procedure DoValueEnd; inline;
  public
    constructor Create(AChars: PWideChar; ACount: Integer);
    destructor Destroy; override;
    procedure Parse;
    //
    property Chars: PWideChar read FChars;
    property Count: Integer read FCount;
    property Settings: TdxSpreadSheetCSVFormatSettings read FSettings;
  end;

  { TdxSpreadSheetCSVReader }

  TdxSpreadSheetCSVReader = class(TdxSpreadSheetCustomReader)
  protected
    function CreateParser(const AData: string): TdxSpreadSheetCSVCustomParser; virtual;
    function CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper; override;
    procedure Parse(const AData: string);
  public
    procedure ReadData; override;
  end;

  { TdxSpreadSheetCSVReaderCustomParser }

  TdxSpreadSheetCSVReaderCustomParser = class(TdxSpreadSheetCSVCustomParser)
  strict private
    FKeepSourceNumberFormat: Boolean;
    FView: TdxSpreadSheetTableView;
  protected
    FColumnIndex: Integer;
    FFormatSettings: TdxSpreadSheetFormatSettings;
    FRow: TdxSpreadSheetTableRow;
    FRowIndex: Integer;

    function CreateFormatSettings: TdxSpreadSheetFormatSettings; virtual;
    procedure DoRowBegin; override;
    procedure DoRowEnd; override;
    procedure DoValue(const AValue: string; AIsQuotedValue: Boolean); override;
  public
    constructor Create(AView: TdxSpreadSheetTableView; AChars: PWideChar; ACount: Integer);
    destructor Destroy; override;
    //
    property KeepSourceNumberFormat: Boolean read FKeepSourceNumberFormat write FKeepSourceNumberFormat;
    property View: TdxSpreadSheetTableView read FView;
  end;

  { TdxSpreadSheetCSVReaderParser }

  TdxSpreadSheetCSVReaderParser = class(TdxSpreadSheetCSVReaderCustomParser)
  strict private
    FCharsCount: Integer;
    FProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
  protected
    procedure DoRowBegin; override;
    procedure DoRowEnd; override;
  public
    constructor Create(AProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
      AView: TdxSpreadSheetTableView; AChars: PWideChar; ACount: Integer);
    destructor Destroy; override;
  end;

  { TdxSpreadSheetCSVWriter }

  TdxSpreadSheetCSVWriter = class(TdxSpreadSheetCustomTextFormatWriter)
  strict private
    FSettings: TdxSpreadSheetCSVFormatSettings;
  protected
    function CreateTableViewWriter(AView: TdxSpreadSheetTableView; const AArea: TRect): TdxSpreadSheetCustomFilerSubTask; override;
    function GetEncoding: TEncoding; override;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream); override;
    destructor Destroy; override;
    //
    property Settings: TdxSpreadSheetCSVFormatSettings read FSettings;
  end;

  { TdxSpreadSheetCSVTableViewWriter }

  TdxSpreadSheetCSVTableViewWriter = class(TdxSpreadSheetCustomTextFormatTableViewWriter)
  strict private
    function GetSettings: TdxSpreadSheetCSVFormatSettings;
  protected
    function EncodeValue(ACell: TdxSpreadSheetCell): string; virtual;
    procedure WriteCell(ARowIndex, AColumnIndex: Integer; ACell: TdxSpreadSheetCell); override;
  public
    property Settings: TdxSpreadSheetCSVFormatSettings read GetSettings;
  end;

function dxSpreadSheetCSVFormatSettings: TdxSpreadSheetCSVFormatSettings;
implementation

uses
  dxHashUtils, Math, dxSpreadSheetTypes, dxSpreadSheetUtils, cxClasses, dxSpreadSheetStrs, cxDateUtils,
  dxSpreadSheetStyles;

type
  TdxCustomSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetCellAccess = class(TdxSpreadSheetCell);
  TdxSpreadSheetTableRowAccess = class(TdxSpreadSheetTableRow);
  TdxSpreadSheetTableRowCellsAccess = class(TdxSpreadSheetTableRowCells);
  TdxSpreadSheetTableRowsAccess = class(TdxSpreadSheetTableRows);

var
  FFormatSettings: TdxSpreadSheetCSVFormatSettings;

function dxSpreadSheetCSVFormatSettings: TdxSpreadSheetCSVFormatSettings;
begin
  if FFormatSettings = nil then
    FFormatSettings := TdxSpreadSheetCSVFormatSettings.Create;
  Result := FFormatSettings;
end;

{ TdxSpreadSheetCSVFormat }

class function TdxSpreadSheetCSVFormat.CanCheckByContent: Boolean;
begin
  Result := False;
end;

class function TdxSpreadSheetCSVFormat.CreateFormatSettings: TdxSpreadSheetFormatSettings;
begin
  Result := TdxSpreadSheetFormatSettings.Create;
end;

class function TdxSpreadSheetCSVFormat.GetDescription: string;
begin
  Result := 'Comma delimited';
end;

class function TdxSpreadSheetCSVFormat.GetExt: string;
begin
  Result := '.csv';
end;

class function TdxSpreadSheetCSVFormat.GetReader: TdxSpreadSheetCustomReaderClass;
begin
  Result := TdxSpreadSheetCSVReader;
end;

class function TdxSpreadSheetCSVFormat.GetWriter: TdxSpreadSheetCustomWriterClass;
begin
  Result := TdxSpreadSheetCSVWriter;
end;

{ TdxSpreadSheetCSVFormatSettings }

constructor TdxSpreadSheetCSVFormatSettings.Create;
begin
  inherited Create;
  FExport := TdxSpreadSheetCSVFormatExportSettings.Create;
  Reset;
end;

destructor TdxSpreadSheetCSVFormatSettings.Destroy;
begin
  FreeAndNil(FExport);
  inherited Destroy;
end;

procedure TdxSpreadSheetCSVFormatSettings.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetCSVFormatSettings then
  begin
    Encoding := TdxSpreadSheetCSVFormatSettings(Source).Encoding;
    Export := TdxSpreadSheetCSVFormatSettings(Source).Export;
    Quote := TdxSpreadSheetCSVFormatSettings(Source).Quote;
    ValueSeparator := TdxSpreadSheetCSVFormatSettings(Source).ValueSeparator;
  end;
end;

procedure TdxSpreadSheetCSVFormatSettings.Reset;
begin
  Export.Reset;
  FEncoding := TEncoding.Default;
  FValueSeparator := ',';
  FQuote := '"';
end;

procedure TdxSpreadSheetCSVFormatSettings.SetExport(AValue: TdxSpreadSheetCSVFormatExportSettings);
begin
  FExport.Assign(AValue);
end;

{ TdxSpreadSheetCSVFormatExportSettings }

constructor TdxSpreadSheetCSVFormatExportSettings.Create;
begin
  inherited Create;
  Reset;
end;

procedure TdxSpreadSheetCSVFormatExportSettings.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetCSVFormatExportSettings then
  begin
    QuoteStringValues := TdxSpreadSheetCSVFormatExportSettings(Source).QuoteStringValues;
    UseDisplayValues := TdxSpreadSheetCSVFormatExportSettings(Source).UseDisplayValues;
  end;
end;

procedure TdxSpreadSheetCSVFormatExportSettings.Reset;
begin
  QuoteStringValues := True;
  UseDisplayValues := True;
end;

{ TdxSpreadSheetCSVCustomParser }

constructor TdxSpreadSheetCSVCustomParser.Create(AChars: PWideChar; ACount: Integer);
begin
  inherited Create;
  FChars := AChars;
  FCount := ACount;
  FSettings := TdxSpreadSheetCSVFormatSettings.Create;
  FSettings.Assign(dxSpreadSheetCSVFormatSettings);
end;

destructor TdxSpreadSheetCSVCustomParser.Destroy;
begin
  FreeAndNil(FSettings);
  inherited Destroy;
end;

procedure TdxSpreadSheetCSVCustomParser.Parse;
var
  AChar: WideChar;
begin
  if FCount = 0 then
    Exit;

  DoRowBegin;
  while FCount > 0 do
  begin
    AChar := FChars^;
    if AChar = Settings.Quote then
      ProcessQuotedValue
    else

    if AChar = Settings.ValueSeparator then
    begin
      DoValueEnd;
      GoToNext;
      while (FCount > 0) and CharInSet(FChars^, [#9, ' ']) and (FChars^ <> Settings.ValueSeparator) do
        GoToNext;
      DoValueBegin;
    end
    else

    if AChar = #10 then
    begin
      DoRowEnd;
      GoToNext;
      DoRowBegin;
    end
    else

    if AChar = #13 then
    begin
      DoRowEnd;
      if LookInNext = #10 then
        GoToNext;
      GoToNext;
      DoRowBegin;
    end
    else
      GoToNext;
  end;
  DoRowEnd;
end;

procedure TdxSpreadSheetCSVCustomParser.DoRowBegin;
begin
  DoValueBegin;
end;

procedure TdxSpreadSheetCSVCustomParser.DoRowEnd;
begin
  DoValueEnd;
end;

procedure TdxSpreadSheetCSVCustomParser.DoValue(const AValue: string; AIsQuotedValue: Boolean);
begin
  // do nothing
end;

procedure TdxSpreadSheetCSVCustomParser.DoValueBegin;
begin
  FValueCursor := FChars;
  FValueContainsQuote := False;
end;

procedure TdxSpreadSheetCSVCustomParser.DoValueEnd;
var
  ACount: Integer;
  AIsQuotedValue: Boolean;
  AValue: string;
begin
  ACount := (TdxNativeUInt(FChars) - TdxNativeUInt(FValueCursor)) div SizeOf(WideChar);
  if ACount > 0 then
  begin
    AIsQuotedValue := (FValueCursor^ = Settings.Quote) and ((FValueCursor + ACount - 1)^ = Settings.Quote);
    if AIsQuotedValue then
    begin
      Inc(FValueCursor);
      Dec(ACount, 2);
    end
    else
      if (FCount > 0) and (FChars^ = Settings.ValueSeparator) then
      begin
        while (ACount > 0) and CharInSet((FValueCursor + ACount - 1)^, [' ', #9]) do
          Dec(ACount);
      end;

    SetString(AValue, FValueCursor, ACount);
    if FValueContainsQuote then
      AValue := StringReplace(AValue, Settings.Quote + Settings.Quote, Settings.Quote, [rfReplaceAll]);
    DoValue(AValue, AIsQuotedValue);
  end
  else
    DoValue('', False);
end;

procedure TdxSpreadSheetCSVCustomParser.GoToNext;
begin
  Inc(FChars);
  Dec(FCount);
end;

function TdxSpreadSheetCSVCustomParser.LookInNext: WideChar;
begin
  if FCount > 1 then
    Result := (FChars + 1)^
  else
    Result := #0;
end;

procedure TdxSpreadSheetCSVCustomParser.ProcessQuotedValue;
begin
  GoToNext;
  if (FCount > 0) and (FChars^ = Settings.Quote) then
  begin
    FValueContainsQuote := True;
    GoToNext;
    Exit;
  end;

  while FCount > 0 do
  begin
    if FChars^ = Settings.Quote then
    begin
      if LookInNext <> Settings.Quote then
      begin
        GoToNext;
        Break;
      end;
      FValueContainsQuote := True;
      GoToNext;
    end;
    GoToNext;
  end;
end;

{ TdxSpreadSheetCSVReader }

procedure TdxSpreadSheetCSVReader.ReadData;
var
  ABuffer: TBytes;
  AEncoding: TEncoding;
  ASize: Integer;
begin
  ASize := Stream.Size - Stream.Position;
  if ASize > 0 then
  begin
    SetLength(ABuffer, ASize);
    Stream.Read(ABuffer[0], ASize);

    AEncoding := nil;
    ASize := TEncoding.GetBufferEncoding(ABuffer, AEncoding{$IFDEF DELPHIXE}, dxSpreadSheetCSVFormatSettings.Encoding{$ENDIF});
    Parse(AEncoding.GetString(ABuffer, ASize, Length(ABuffer) - ASize));
  end;
end;

function TdxSpreadSheetCSVReader.CreateParser(const AData: string): TdxSpreadSheetCSVCustomParser;
begin
  Result := TdxSpreadSheetCSVReaderParser.Create(ProgressHelper, AddTableView(''), PWideChar(AData), Length(AData));
end;

function TdxSpreadSheetCSVReader.CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
begin
  Result := TdxSpreadSheetCustomFilerProgressHelper.Create(Self, 1);
end;

procedure TdxSpreadSheetCSVReader.Parse(const AData: string);
begin
  with CreateParser(AData) do
  try
    Parse;
  finally
    Free;
  end;
end;

{ TdxSpreadSheetCSVReaderParser }

constructor TdxSpreadSheetCSVReaderCustomParser.Create(AView: TdxSpreadSheetTableView; AChars: PWideChar; ACount: Integer);
begin
  inherited Create(AChars, ACount);
  FView := AView;
  FFormatSettings := CreateFormatSettings;
  FRowIndex := -1;
end;

destructor TdxSpreadSheetCSVReaderCustomParser.Destroy;
begin
  FreeAndNil(FFormatSettings);
  inherited Destroy;
end;

function TdxSpreadSheetCSVReaderCustomParser.CreateFormatSettings: TdxSpreadSheetFormatSettings;
begin
  Result := TdxSpreadSheetControlFormatSettings.Create(View.SpreadSheet);
end;

procedure TdxSpreadSheetCSVReaderCustomParser.DoRowBegin;
begin
  inherited;
  Inc(FRowIndex);
  FColumnIndex := -1;
end;

procedure TdxSpreadSheetCSVReaderCustomParser.DoRowEnd;
begin
  inherited DoRowEnd;
  FRow := nil;
end;

procedure TdxSpreadSheetCSVReaderCustomParser.DoValue(const AValue: string; AIsQuotedValue: Boolean);
var
  ACell: TdxSpreadSheetCell;
  AOrdinalValue: Variant;
begin
  Inc(FColumnIndex);
  if AValue <> '' then
  begin
    if FRow = nil then
      FRow := View.Rows.CreateItem(FRowIndex);

    ACell := FRow.CreateCell(FColumnIndex);
    if KeepSourceNumberFormat then
      AIsQuotedValue := AIsQuotedValue or ACell.StyleHandle.DataFormat.IsText;
    if not AIsQuotedValue and dxTryStrToOrdinal(AValue, AOrdinalValue, FFormatSettings) then
      ACell.AsVariant := AOrdinalValue
    else
      ACell.AsString := AValue;
  end;
end;

{ TdxSpreadSheetCSVReaderParser }

constructor TdxSpreadSheetCSVReaderParser.Create(
  AProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
  AView: TdxSpreadSheetTableView; AChars: PWideChar; ACount: Integer);
begin
  inherited Create(AView, AChars, ACount);
  FProgressHelper := AProgressHelper;
  FProgressHelper.BeginStage(ACount);
end;

destructor TdxSpreadSheetCSVReaderParser.Destroy;
begin
  if FProgressHelper <> nil then
    FProgressHelper.EndStage;
  inherited Destroy;
end;

procedure TdxSpreadSheetCSVReaderParser.DoRowBegin;
begin
  inherited DoRowBegin;
  FCharsCount := Count;
end;

procedure TdxSpreadSheetCSVReaderParser.DoRowEnd;
begin
  inherited DoRowEnd;
  if FProgressHelper <> nil then
    FProgressHelper.NextTask(FCharsCount - Count);
end;

{ TdxSpreadSheetCSVWriter }

constructor TdxSpreadSheetCSVWriter.Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
begin
  inherited Create(AOwner, AStream);
  FSettings := TdxSpreadSheetCSVFormatSettings.Create;
  FSettings.Assign(dxSpreadSheetCSVFormatSettings);
end;

destructor TdxSpreadSheetCSVWriter.Destroy;
begin
  FreeAndNil(FSettings);
  inherited Destroy;
end;

function TdxSpreadSheetCSVWriter.CreateTableViewWriter(
  AView: TdxSpreadSheetTableView; const AArea: TRect): TdxSpreadSheetCustomFilerSubTask;
begin
  Result := TdxSpreadSheetCSVTableViewWriter.Create(Self, AView, AArea);
end;

function TdxSpreadSheetCSVWriter.GetEncoding: TEncoding;
begin
  Result := Settings.Encoding;
end;

{ TdxSpreadSheetCSVTableViewWriter }

function TdxSpreadSheetCSVTableViewWriter.EncodeValue(ACell: TdxSpreadSheetCell): string;

  function GetCellValue(ACell: TdxSpreadSheetCell): string;
  begin
    if Settings.Export.UseDisplayValues then
      Result := ACell.DisplayText
    else
      Result := ACell.AsString;
  end;

  function HasProhibitedSymbols(const S: string): Boolean;
  begin
    Result := LastDelimiter(#13#10 + Settings.ValueSeparator, S) > 0;
  end;

  function IsStringValue(ACell: TdxSpreadSheetCell): Boolean;
  begin
    if Settings.Export.UseDisplayValues then
      Result := TdxSpreadSheetCellAccess(ACell).ActualDataType = cdtString
    else
      Result := ACell.DataType = cdtString;
  end;

begin
  Result := StringReplace(GetCellValue(ACell), Settings.Quote, Settings.Quote + Settings.Quote, [rfReplaceAll]);
  if Settings.Export.QuoteStringValues and IsStringValue(ACell) or HasProhibitedSymbols(Result) then
    Result := Settings.Quote + Result + Settings.Quote;
end;

procedure TdxSpreadSheetCSVTableViewWriter.WriteCell(ARowIndex, AColumnIndex: Integer; ACell: TdxSpreadSheetCell);
begin
  if AColumnIndex > Area.Left then
    StreamWriter.Write(Settings.ValueSeparator);
  if ACell <> nil then
    StreamWriter.Write(EncodeValue(ACell));
end;

function TdxSpreadSheetCSVTableViewWriter.GetSettings: TdxSpreadSheetCSVFormatSettings;
begin
  Result := TdxSpreadSheetCSVWriter(Owner).Settings;
end;

initialization
  TdxSpreadSheetCSVFormat.Register;

finalization
  TdxSpreadSheetCSVFormat.Unregister;
  FreeAndNil(FFormatSettings);
end.
