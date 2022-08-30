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

unit dxSpreadSheetFormatHTML;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxHashUtils, cxGraphics, dxGDIPlusClasses,
  dxSpreadSheetCore, dxSpreadSheetClasses, dxSpreadSheetUtils, dxSpreadSheetTextFileFormatCore, dxSpreadSheetGraphics,
  dxSpreadSheetTypes, dxSpreadSheetHyperlinks, dxSpreadSheetCoreStyles, dxSpreadSheetStyles;

type

  { TdxSpreadSheetHTMLFormat }

  TdxSpreadSheetHTMLFormat = class(TdxSpreadSheetCustomFormat)
  strict private
    class var FCellAutoHeight: Boolean;
  public
    class function CanReadFromStream(AStream: TStream): Boolean; override;
    class function GetDescription: string; override;
    class function GetExt: string; override;
    class function GetReader: TdxSpreadSheetCustomReaderClass; override;
    class function GetWriter: TdxSpreadSheetCustomWriterClass; override;
    //
    class property CellAutoHeight: Boolean read FCellAutoHeight write FCellAutoHeight;
  end;

  { TdxSpreadSheetHTMFormat }

  TdxSpreadSheetHTMFormat = class(TdxSpreadSheetHTMLFormat)
  public
    class function GetExt: string; override;
  end;

  { TdxSpreadSheetHTMLFontStyleMap }

  TdxSpreadSheetHTMLFontStyleMap = class(TDictionary<TdxSpreadSheetFontHandle, string>)
  protected
    procedure KeyNotify(const Key: TdxSpreadSheetFontHandle; Action: TCollectionNotification); override;
  end;

  { TdxSpreadSheetHTMLStyleMap }

  TdxSpreadSheetHTMLStyleMap = class(TDictionary<TdxSpreadSheetCellStyleHandle, string>)
  protected
    procedure KeyNotify(const Key: TdxSpreadSheetCellStyleHandle; Action: TCollectionNotification); override;
  end;

  { TdxSpreadSheetHTMLFormatWriter }

  TdxSpreadSheetHTMLFormatWriter = class(TdxSpreadSheetCustomTextFormatWriter)
  strict private
    FFontMap: TdxSpreadSheetHTMLFontStyleMap;
    FGraphicCount: Integer;
    FStyleMap: TdxSpreadSheetHTMLStyleMap;
    FStylesStream: TMemoryStream;
    FStylesWriter: TStreamWriter;
  protected
    FStylesPositionInStream: Int64;

    function CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper; override;
    function CreateTableViewWriter(AView: TdxSpreadSheetTableView; const AArea: TRect): TdxSpreadSheetCustomFilerSubTask; override;
    function GetEncoding: TEncoding; override;
    function RegisterFont(AFont: TdxSpreadSheetFontHandle): string; virtual;
    function RegisterStyle(AStyle: TdxSpreadSheetCellStyle): string; virtual;

    procedure WriteDocumentFooter; override;
    procedure WriteDocumentHeader; override;
    procedure WriteStandardStyles; virtual;

    property FontMap: TdxSpreadSheetHTMLFontStyleMap read FFontMap;
    property StyleMap: TdxSpreadSheetHTMLStyleMap read FStyleMap;
    property StylesWriter: TStreamWriter read FStylesWriter;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream); override;
    destructor Destroy; override;
    function GetImageFileName(out ARootPath, ASubFileName: string): Boolean;
    procedure WriteData; override;
  end;

  { TdxSpreadSheetHTMLFormatCustomWriter }

  TdxSpreadSheetHTMLFormatCustomWriter = class(TdxSpreadSheetCustomFilerSubTask)
  strict private
    FStreamWriter: TStreamWriter;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomFiler; AStreamWriter: TStreamWriter);
    //
    property StreamWriter: TStreamWriter read FStreamWriter;
  end;

  { TdxSpreadSheetHTMLFormatDisplayStyleCacheItem }

  TdxSpreadSheetHTMLFormatDisplayStyleCacheItem = class
  public
    ColumnIndex: Integer;
    RowIndex: Integer;
    Style: TdxSpreadSheetTableViewCellDisplayStyle;

    constructor Create(ARowIndex, AColumnIndex: Integer; AStyle: TdxSpreadSheetTableViewCellDisplayStyle);
    destructor Destroy; override;
  end;

  { TdxSpreadSheetHTMLFormatDisplayStyleCache }

  TdxSpreadSheetHTMLFormatDisplayStyleCache = class(TObjectList<TdxSpreadSheetHTMLFormatDisplayStyleCacheItem>)
  strict private
    FMaxCapacity: Integer;

    procedure CheckCapacity;
  public
    constructor Create;
    procedure Add(AItem: TdxSpreadSheetHTMLFormatDisplayStyleCacheItem);
    function Find(ARowIndex, AColumnIndex: Integer; out AStyle: TdxSpreadSheetTableViewCellDisplayStyle): Boolean;
  end;

  { TdxSpreadSheetHTMLFormatTableViewWriter }

  TdxSpreadSheetHTMLFormatTableViewWriter = class(TdxSpreadSheetCustomTextFormatTableViewWriter)
  strict private
    FDisplayStyleCache: TdxSpreadSheetHTMLFormatDisplayStyleCache;
    FFloatContainers: TList<TPair<TdxSpreadSheetContainer, TRect>>;
    FHyperlinkCells: TDictionary<TdxSpreadSheetCell, TdxSpreadSheetHyperlink>;
    FInscribedContainers: TDictionary<TdxSpreadSheetCell, TdxSpreadSheetContainer>;

    function GetOwner: TdxSpreadSheetHTMLFormatWriter; inline;
    function IsInscribedContainer(AContainer: TdxSpreadSheetContainer): Boolean;
  protected
    FCurrentRowSize: Integer;

    procedure ExtendDimensionsByContainers(var R: TRect);
    function GetCellStyle(ARowIndex, AColumnIndex: Integer; ACell: TdxSpreadSheetCell; ACellIsAssigned: Boolean): TdxSpreadSheetTableViewCellDisplayStyle;
    function GetCellStyleName(ARowIndex: Integer; AColumnIndex: Integer; ACell: TdxSpreadSheetCell): string;
    function GetHyperlinkAsHTMLText(AHyperlink: TdxSpreadSheetHyperlink; const AValue: string = ''): string;
    procedure PrepareContainerImage(const AFileName: string; var AContainerBounds: TRect; AContainer: TdxSpreadSheetContainer);
    procedure PrepareContainers; virtual;
    procedure PrepareHyperlinks; virtual;

    procedure WriteCell(ARowIndex: Integer; AColumnIndex: Integer; ACell: TdxSpreadSheetCell); override;
    procedure WriteCellContent(ACell: TdxSpreadSheetCell);
    procedure WriteContainer(AContainer: TdxSpreadSheetContainer; AContainerBounds: TRect); virtual;
    procedure WriteContainers; virtual;
    procedure WriteFormattedText(AText: TdxSpreadSheetFormattedSharedString);
    procedure WriteRow(ARowIndex: Integer; ARow: TdxSpreadSheetTableRow); override;
    procedure WriteRowFooter(ARowIndex: Integer; ARow: TdxSpreadSheetTableRow); virtual;
    procedure WriteRowHeader(ARowIndex: Integer; ARow: TdxSpreadSheetTableRow); virtual;
    procedure WriteTableFooter; virtual;
    procedure WriteTableHeader; virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomTextFormatWriter; AView: TdxSpreadSheetTableView; const AArea: TRect); override;
    destructor Destroy; override;
    procedure Execute; override;
    //
    property FloatContainers: TList<TPair<TdxSpreadSheetContainer, TRect>> read FFloatContainers;
    property HyperlinkCells: TDictionary<TdxSpreadSheetCell, TdxSpreadSheetHyperlink> read FHyperlinkCells;
    property InscribedContainers: TDictionary<TdxSpreadSheetCell, TdxSpreadSheetContainer> read FInscribedContainers;
    property Owner: TdxSpreadSheetHTMLFormatWriter read GetOwner;
  end;

  { TdxSpreadSheetHTMLFormatFontStyleWriter }

  TdxSpreadSheetHTMLFormatFontStyleWriter = class(TdxSpreadSheetHTMLFormatCustomWriter)
  strict private
    FFontStyle: TdxSpreadSheetFontHandle;

    function GetContentStyle: TcxViewParams;
  protected
    procedure WriteFont; virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomFiler; AWriter: TStreamWriter; AFontStyle: TdxSpreadSheetFontHandle);
    procedure Execute; override;
    //
    property ContentStyle: TcxViewParams read GetContentStyle;
  end;

  { TdxSpreadSheetHTMLFormatCellStyleWriter }

  TdxSpreadSheetHTMLFormatCellStyleWriter = class(TdxSpreadSheetHTMLFormatFontStyleWriter)
  strict private
    FStyle: TdxSpreadSheetCellStyleHandle;
  protected
    procedure WriteBorders; virtual;
    procedure WriteBrush; virtual;
    procedure WriteTextAlignment; virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomFiler; AWriter: TStreamWriter; AStyle: TdxSpreadSheetCellStyleHandle);
    procedure Execute; override;
    //
    property Style: TdxSpreadSheetCellStyleHandle read FStyle;
  end;

implementation

uses
  Math, StrUtils, RTLConsts, dxColorPicker, dxCoreGraphics, dxCoreClasses, cxGeometry, dxSpreadSheetCoreHelpers,
  dxSpreadSheetStrs, dxStringHelper;

const
  dxBodyMargin = 8;

type
  TdxSpreadSheetCellStylesAccess = class(TdxSpreadSheetCellStyles);
  TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);
  TdxSpreadSheetTableColumnsAccess = class(TdxSpreadSheetTableColumns);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TdxSpreadSheetTableViewInfoAccess = class(TdxSpreadSheetTableViewInfo);

function ConvertSpecialCharacters(AString: string): string;
var
  ABuilder: TStringBuilder;
  AStartIndex, AFinishIndex: Integer;
begin
  AString := StringReplace(AString, #13#10, #13, [rfReplaceAll]);
  AString := StringReplace(AString, #10, #13, [rfReplaceAll]);

  ABuilder := TdxStringBuilderManager.Get(MulDiv(Length(AString), 5, 4));
  try
    AStartIndex := 1;
    AFinishIndex := Length(AString);

    while (AStartIndex <= AFinishIndex) and (AString[AStartIndex] = ' ') do
    begin
      ABuilder.Append('&nbsp;');
      Inc(AStartIndex);
    end;

    while (AStartIndex <= AFinishIndex) and (AString[AFinishIndex] = ' ') do
      Dec(AFinishIndex);

    while AStartIndex <= AFinishIndex do
    begin
      case AString[AStartIndex] of
        #13:
          ABuilder.Append('<br/>');
        '<':
          ABuilder.Append('&lt;');
        '>':
          ABuilder.Append('&gt;');
        '&':
          ABuilder.Append('&amp;');
        '"':
          ABuilder.Append('&quot;');
      else
        ABuilder.Append(AString[AStartIndex]);
      end;
      Inc(AStartIndex);
    end;

    Inc(AFinishIndex);
    while AFinishIndex <= Length(AString) do
    begin
      ABuilder.Append('&nbsp;');
      Inc(AFinishIndex);
    end;

    Result := ABuilder.ToString;
  finally
    TdxStringBuilderManager.Release(ABuilder);
  end;
end;

function GetHTMLColor(AColor: TColor): string;
begin
  if AColor = clNone then
    Result := 'transparent'
  else
    Result := '#' + TdxColorHelper.AlphaColorToHexCode(dxColorToAlphaColor(AColor), False, False);
end;

procedure StreamInsertData(AStream: TStream; AData: TMemoryStream;
  const AInsertPosition: Int64; AProgressHelper: TdxSpreadSheetCustomFilerProgressHelper);
const
  TempBufferSize = 1048576; // 1 MB
var
  ABytesToRead: Integer;
  ADataSize: Integer;
  ALimitPos, ATempPos, ANewTempPos: Int64;
  AStreamSize: Int64;
  ATempBuffer: PByte;
begin
  AProgressHelper.BeginStage(100);
  try
    ADataSize := AData.Size;
    ATempBuffer := AllocMem(TempBufferSize);
    try
      AStreamSize := AStream.Size;
      ATempPos := AStreamSize;
      ALimitPos := AInsertPosition;
      repeat
        ANewTempPos := Max(ALimitPos, ATempPos - TempBufferSize);
        ABytesToRead := ATempPos - ANewTempPos;
        if ABytesToRead > 0 then
        begin
          AStream.Position := ANewTempPos;
          AStream.ReadBuffer(ATempBuffer^, ABytesToRead);
          AStream.Position := ANewTempPos + ADataSize;
          AStream.WriteBuffer(ATempBuffer^, ABytesToRead);
          AProgressHelper.SetTaskNumber(Trunc(100 * (AStreamSize - ANewTempPos) / AStreamSize));
          ATempPos := ANewTempPos;
        end;
      until ATempPos = ALimitPos;
    finally
      FreeMem(ATempBuffer, TempBufferSize);
    end;
    AStream.Position := AInsertPosition;
    AStream.WriteBuffer(AData.Memory^, ADataSize);
  finally
    AProgressHelper.EndStage;
  end;
end;

{ TdxSpreadSheetHTMLFormat }

class function TdxSpreadSheetHTMLFormat.CanReadFromStream(AStream: TStream): Boolean;
begin
  Result := False;
end;

class function TdxSpreadSheetHTMLFormat.GetDescription: string;
begin
  Result := 'Web Page';
end;

class function TdxSpreadSheetHTMLFormat.GetExt: string;
begin
  Result := '.html';
end;

class function TdxSpreadSheetHTMLFormat.GetReader: TdxSpreadSheetCustomReaderClass;
begin
  Result := nil;
end;

class function TdxSpreadSheetHTMLFormat.GetWriter: TdxSpreadSheetCustomWriterClass;
begin
  Result := TdxSpreadSheetHTMLFormatWriter;
end;

{ TdxSpreadSheetHTMFormat }

class function TdxSpreadSheetHTMFormat.GetExt: string;
begin
  Result := '.htm';
end;

{ TdxSpreadSheetHTMLFontStyleMap }

procedure TdxSpreadSheetHTMLFontStyleMap.KeyNotify(const Key: TdxSpreadSheetFontHandle; Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      Key.AddRef;
    cnRemoved, cnExtracted:
      Key.Release;
  end;
end;

{ TdxSpreadSheetHTMLStyleMap }

procedure TdxSpreadSheetHTMLStyleMap.KeyNotify(const Key: TdxSpreadSheetCellStyleHandle; Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      Key.AddRef;
    cnRemoved, cnExtracted:
      Key.Release;
  end;
end;

{ TdxSpreadSheetHTMLFormatWriter }

constructor TdxSpreadSheetHTMLFormatWriter.Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
begin
  inherited Create(AOwner, AStream);
  WriteEncodingPreamble := False;
  FStyleMap := TdxSpreadSheetHTMLStyleMap.Create;
  FFontMap := TdxSpreadSheetHTMLFontStyleMap.Create;
end;

destructor TdxSpreadSheetHTMLFormatWriter.Destroy;
begin
  FreeAndNil(FStyleMap);
  FreeAndNil(FFontMap);
  inherited Destroy;
end;

function TdxSpreadSheetHTMLFormatWriter.GetImageFileName(out ARootPath, ASubFileName: string): Boolean;
begin
  Result := Stream is TFileStream;
  if Result then
  begin
    ARootPath := ExtractFilePath(TFileStream(Stream).FileName);
    if ARootPath = '' then
      ARootPath := IncludeTrailingPathDelimiter(GetCurrentDir);
    ASubFileName := ChangeFileExt(ExtractFileName(TFileStream(Stream).FileName), '.images') + PathDelim + IntToStr(FGraphicCount) + '.png';
    Inc(FGraphicCount);
  end;
end;

procedure TdxSpreadSheetHTMLFormatWriter.WriteData;
begin
  FStylesStream := TMemoryStream.Create;
  FStylesWriter := TdxSpreadSheetStreamWriter.Create(FStylesStream, Encoding, False);
  try
    inherited WriteData;
    StreamInsertData(Stream, FStylesStream, FStylesPositionInStream, ProgressHelper);
  finally
    FreeAndNil(FStylesStream);
    FreeAndNil(FStylesWriter);
  end;
end;

function TdxSpreadSheetHTMLFormatWriter.CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
begin
  Result := TdxSpreadSheetCustomFilerProgressHelper.Create(Self, 3);
end;

function TdxSpreadSheetHTMLFormatWriter.CreateTableViewWriter(
  AView: TdxSpreadSheetTableView; const AArea: TRect): TdxSpreadSheetCustomFilerSubTask;
begin
  Result := TdxSpreadSheetHTMLFormatTableViewWriter.Create(Self, AView, AArea);
end;

function TdxSpreadSheetHTMLFormatWriter.GetEncoding: TEncoding;
begin
  Result := TEncoding.UTF8;
end;

function TdxSpreadSheetHTMLFormatWriter.RegisterFont(AFont: TdxSpreadSheetFontHandle): string;
begin
  if not FontMap.TryGetValue(AFont, Result) then
  begin
    Result := 'Font_' + IntToStr(FontMap.Count);

    StylesWriter.WriteLine('.' + Result);
    StylesWriter.WriteLine('{');
    ExecuteSubTask(TdxSpreadSheetHTMLFormatFontStyleWriter.Create(Self, StylesWriter, AFont));
    StylesWriter.WriteLine('}');

    FontMap.Add(AFont, Result);
  end;
end;

function TdxSpreadSheetHTMLFormatWriter.RegisterStyle(AStyle: TdxSpreadSheetCellStyle): string;
begin
  if not StyleMap.TryGetValue(AStyle.Handle, Result) then
  begin
    Result := 'CellStyle_' + IntToStr(StyleMap.Count);

    StylesWriter.WriteLine('.' + Result);
    StylesWriter.WriteLine('{');
    ExecuteSubTask(TdxSpreadSheetHTMLFormatCellStyleWriter.Create(Self, StylesWriter, AStyle.Handle));
    StylesWriter.WriteLine('}');

    StyleMap.Add(AStyle.Handle, Result);
  end;
end;

procedure TdxSpreadSheetHTMLFormatWriter.WriteDocumentFooter;
begin
  StreamWriter.WriteLine('</body>');
  StreamWriter.WriteLine('</html>');
end;

procedure TdxSpreadSheetHTMLFormatWriter.WriteDocumentHeader;
begin
  StreamWriter.WriteLine('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">');
  StreamWriter.WriteLine('<html>');
  StreamWriter.WriteLine('<head>');
  StreamWriter.WriteLine('<meta http-equiv="content-type" content="text/html" charset="utf-8">');

  StreamWriter.Write('<title>');
  StreamWriter.Write(SpreadSheet.ActiveSheetAsTable.Caption);
  StreamWriter.WriteLine('</title>');

  StreamWriter.WriteLine('<style type="text/css"><!--');
  WriteStandardStyles;
  FStylesPositionInStream := StreamWriter.BaseStream.Position;
  StreamWriter.WriteLine('--></style>');

  StreamWriter.WriteLine('</head>');
  StreamWriter.WriteLine('<body>');
end;

procedure TdxSpreadSheetHTMLFormatWriter.WriteStandardStyles;
begin
  StreamWriter.WriteLine('div.cell { width: 100%; overflow: hidden; position: relative;}');
  StreamWriter.WriteLine('table { border-collapse: collapse; table-layout: fixed; }');
  StreamWriter.WriteLine('table td { overflow: hidden; padding: 0px;}');
  StreamWriter.WriteLine('body { margin: %dpx; }', [dxBodyMargin]);
end;

{ TdxSpreadSheetHTMLFormatCustomWriter }

constructor TdxSpreadSheetHTMLFormatCustomWriter.Create(AOwner: TdxSpreadSheetCustomFiler; AStreamWriter: TStreamWriter);
begin
  inherited Create(AOwner);
  FStreamWriter := AStreamWriter;
end;

{ TdxSpreadSheetHTMLFormatDisplayStyleCacheItem }

constructor TdxSpreadSheetHTMLFormatDisplayStyleCacheItem.Create(
  ARowIndex, AColumnIndex: Integer; AStyle: TdxSpreadSheetTableViewCellDisplayStyle);
begin
  inherited Create;
  ColumnIndex := AColumnIndex;
  RowIndex := ARowIndex;
  Style := AStyle;
end;

destructor TdxSpreadSheetHTMLFormatDisplayStyleCacheItem.Destroy;
begin
  FreeAndNil(Style);
  inherited Destroy;
end;

{ TdxSpreadSheetHTMLFormatDisplayStyleCache }

constructor TdxSpreadSheetHTMLFormatDisplayStyleCache.Create;
begin
  inherited Create;
  FMaxCapacity := dxSpreadSheetMaxColumnCount * 3;
  Capacity := FMaxCapacity + 1;
end;

procedure TdxSpreadSheetHTMLFormatDisplayStyleCache.Add(AItem: TdxSpreadSheetHTMLFormatDisplayStyleCacheItem);
begin
  inherited Add(AItem);
  CheckCapacity;
end;

function TdxSpreadSheetHTMLFormatDisplayStyleCache.Find(
  ARowIndex, AColumnIndex: Integer; out AStyle: TdxSpreadSheetTableViewCellDisplayStyle): Boolean;
var
  AItem: TdxSpreadSheetHTMLFormatDisplayStyleCacheItem;
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    AItem := Items[I];
    if (AItem.RowIndex = ARowIndex) and (AItem.ColumnIndex = AColumnIndex) then
    begin
      AStyle := AItem.Style;
      Exit(True);
    end;
  end;
  Result := False;
end;

procedure TdxSpreadSheetHTMLFormatDisplayStyleCache.CheckCapacity;
begin
  if Count > FMaxCapacity then
    DeleteRange(0, Count - FMaxCapacity);
end;

{ TdxSpreadSheetHTMLFormatTableViewWriter }

constructor TdxSpreadSheetHTMLFormatTableViewWriter.Create(
  AOwner: TdxSpreadSheetCustomTextFormatWriter; AView: TdxSpreadSheetTableView; const AArea: TRect);
begin
  inherited Create(AOwner, AView, AArea);
  FEnumBlankCellsInTheEndOfLine := True;
  FFloatContainers := TList<TPair<TdxSpreadSheetContainer, TRect>>.Create;
  FInscribedContainers := TDictionary<TdxSpreadSheetCell, TdxSpreadSheetContainer>.Create;
  FDisplayStyleCache := TdxSpreadSheetHTMLFormatDisplayStyleCache.Create;
  FHyperlinkCells := TDictionary<TdxSpreadSheetCell, TdxSpreadSheetHyperlink>.Create;
end;

destructor TdxSpreadSheetHTMLFormatTableViewWriter.Destroy;
begin
  FreeAndNil(FInscribedContainers);
  FreeAndNil(FDisplayStyleCache);
  FreeAndNil(FFloatContainers);
  FreeAndNil(FHyperlinkCells);
  inherited Destroy;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.Execute;
begin
  PrepareContainers;
  PrepareHyperlinks;
  WriteTableHeader;
  inherited Execute;
  WriteTableFooter;
  WriteContainers;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.ExtendDimensionsByContainers(var R: TRect);
var
  AColumnIndex: Integer;
  ARowIndex: Integer;
  ATableView: IdxSpreadSheetTableView;
  I: Integer;
begin
  if Supports(View, IdxSpreadSheetTableView, ATableView) then
    for I := 0 to FloatContainers.Count - 1 do
      if ATableView.GetCellAtAbsolutePoint(FloatContainers[I].Value.BottomRight, ARowIndex, AColumnIndex) then
      begin
        R.Right := Max(R.Right, AColumnIndex);
        R.Bottom := Max(R.Bottom, ARowIndex);
      end;
end;

function TdxSpreadSheetHTMLFormatTableViewWriter.GetCellStyle(ARowIndex, AColumnIndex: Integer;
  ACell: TdxSpreadSheetCell; ACellIsAssigned: Boolean): TdxSpreadSheetTableViewCellDisplayStyle;
begin
  if not FDisplayStyleCache.Find(ARowIndex, AColumnIndex, Result) then
  begin
    if not ACellIsAssigned then
      ACell := View.Cells[ARowIndex, AColumnIndex];
    Result := TdxSpreadSheetTableViewCellDisplayStyle.Create(View,
      TdxSpreadSheetTableViewAccess(View).GetCellStyleHandle(ARowIndex, AColumnIndex, ACell));
    View.ConditionalFormatting.CalculateStyle(Result, ARowIndex, AColumnIndex, ACell);
    FDisplayStyleCache.Add(TdxSpreadSheetHTMLFormatDisplayStyleCacheItem.Create(ARowIndex, AColumnIndex, Result));
  end;
end;

function TdxSpreadSheetHTMLFormatTableViewWriter.GetCellStyleName(
  ARowIndex: Integer; AColumnIndex: Integer; ACell: TdxSpreadSheetCell): string;

  function GetNeighborCellStyle(ARow, AColumn: Integer; ASide: TcxBorder): TdxSpreadSheetCellStyle;
  begin
    case ASide of
      bLeft:
        Dec(AColumn);
      bTop:
        Dec(ARow);
      bRight:
        Inc(AColumn);
    else
      Inc(ARow);
    end;
    Result := GetCellStyle(ARow, AColumn, nil, False);
  end;

var
  ABorder: TcxBorder;
  ABorderColor: TColor;
  ABorderStyle: TdxSpreadSheetCellBorderStyle;
  AStyle: TdxSpreadSheetTableViewCellDisplayStyle;
begin
  AStyle := TdxSpreadSheetTableViewCellDisplayStyle.Create(View);
  try
    AStyle.Assign(GetCellStyle(ARowIndex, AColumnIndex, ACell, True));
    AStyle.BeginUpdate;
    try
      for ABorder := bRight to bBottom do
      begin
        dxSpreadSheetMergeBorderStyle(ABorder, AStyle.Handle,
          GetNeighborCellStyle(ARowIndex, AColumnIndex, ABorder).Handle,
          ABorderColor, ABorderStyle);
        AStyle.Borders[ABorder].Color := ABorderColor;
        AStyle.Borders[ABorder].Style := ABorderStyle;
      end;
    finally
      AStyle.EndUpdate;
    end;
    Result := Owner.RegisterStyle(AStyle);
  finally
    AStyle.Free;
  end;
end;

function TdxSpreadSheetHTMLFormatTableViewWriter.GetHyperlinkAsHTMLText(
  AHyperlink: TdxSpreadSheetHyperlink; const AValue: string = ''): string;
var
  AScreenTip: string;
begin
  Result := AValue;
  if (AHyperlink = nil) or (AHyperlink.ValueType = hvtReference) then
    Exit;
  AScreenTip := AHyperlink.ScreenTip;
  if AValue = '' then
    Result := ConvertSpecialCharacters(AHyperlink.DisplayText);
  if not (havScreenTip in AHyperlink.AssignedValues) then
    AScreenTip := Format(cxGetResourceString(@sdxDefaultHyperlinkShortScreenTip), [AHyperlink.Value]);
  Result := Format('<a href="%s" target="_blank" title="%s">%s</a>',
    [AHyperlink.Value, ConvertSpecialCharacters(AScreenTip), Result]);
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.PrepareContainerImage(
  const AFileName: string; var AContainerBounds: TRect; AContainer: TdxSpreadSheetContainer);

  function CreateContainerImage(var AContainerBounds: TRect): TdxPNGImage;
  var
    ABitmap: TcxBitmap32;
    ACanDrawSelection: Boolean;
    AViewInfo: TdxSpreadSheetContainerViewInfo;
  begin
    AViewInfo := TdxSpreadSheetContainerAccess(AContainer).CreateViewInfo;
    try
      AViewInfo.SetBounds(AContainerBounds, AContainerBounds);
      AViewInfo.Calculate;

      ABitmap := TcxBitmap32.CreateSize(AViewInfo.RealDrawingBounds, True);
      try
        ACanDrawSelection := AViewInfo.CanDrawSelection;
        try
          AViewInfo.CanDrawSelection := False;
          ABitmap.cxCanvas.WindowOrg := AViewInfo.RealDrawingBounds.TopLeft;
          AViewInfo.Draw(ABitmap.cxCanvas, dsFirst);
          AViewInfo.Draw(ABitmap.cxCanvas, dsSecond);
        finally
          AViewInfo.CanDrawSelection := ACanDrawSelection;
        end;

        Result := TdxPNGImage.CreateFromBitmap(ABitmap);
      finally
        ABitmap.Free;
      end;

      AContainerBounds := AViewInfo.RealDrawingBounds;
    finally
      AViewInfo.Free;
    end;
  end;

var
  AImage: TdxPNGImage;
begin
  AImage := CreateContainerImage(AContainerBounds);
  try
    ForceDirectories(ExtractFilePath(AFileName));
    AImage.SaveToFile(AFileName);
  finally
    AImage.Free;
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.PrepareContainers;
var
  AContainer: TdxSpreadSheetContainerAccess;
  I: Integer;
begin
  if StreamWriter.BaseStream is TFileStream then
  begin
    for I := 0 to View.Containers.Count - 1 do
    begin
      AContainer := TdxSpreadSheetContainerAccess(View.Containers[I]);
      if AContainer.Visible then
      begin
        if IsInscribedContainer(AContainer) then
          InscribedContainers.AddOrSetValue(AContainer.AnchorPoint1.Cell, AContainer)
        else
          FloatContainers.Add(TPair<TdxSpreadSheetContainer, TRect>.Create(AContainer, AContainer.Calculator.CalculateBounds));
      end;
    end;
    ExtendDimensionsByContainers(FArea);
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.PrepareHyperlinks;
var
  ACell: TdxSpreadSheetCell;
  AHyperlink: TdxSpreadSheetHyperlink;
  I: Integer;
begin
  for I := 0 to View.Hyperlinks.Count - 1 do
  begin
    AHyperlink := View.Hyperlinks[I];
    if AHyperlink.IsAreaCorrect and (AHyperlink.ValueType <> hvtReference) then
    begin
      ACell := View.CreateCell(AHyperlink.Area.Top, AHyperlink.Area.Left);
      if not HyperlinkCells.ContainsKey(ACell) then
        HyperlinkCells.Add(ACell, AHyperlink);
    end;
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteCell(ARowIndex, AColumnIndex: Integer; ACell: TdxSpreadSheetCell);

  function IsSingeLineCell(AMergedCell: TdxSpreadSheetMergedCell): Boolean;
  begin
    Result := (AMergedCell = nil) or (dxSpreadSheetAreaHeight(AMergedCell.Area) = 1);
  end;

  procedure WriteDivStyle(ACell: TdxSpreadSheetCell; AMergedCell: TdxSpreadSheetMergedCell; AInscribedContainer: TdxSpreadSheetContainer);
  begin
    if (ACell <> nil) and (ACell.Style.AlignHorzIndent > 0) or (AInscribedContainer <> nil) or
      not TdxSpreadSheetHTMLFormat.CellAutoHeight and IsSingeLineCell(AMergedCell) then
    begin
      StreamWriter.Write(' style="');
      if ACell.Style.AlignHorzIndent > 0 then
      begin
        if ACell.Style.AlignHorz in [ssahLeft, ssahDistributed] then
          StreamWriter.Write('padding-left: %dpx; ', [ACell.Style.AlignHorzIndent]);
        if ACell.Style.AlignHorz in [ssahRight, ssahDistributed] then
          StreamWriter.Write('padding-right: %dpx; ', [ACell.Style.AlignHorzIndent]);
      end;
      if AInscribedContainer <> nil then
        StreamWriter.Write('min-height: %dpx; ', [FCurrentRowSize]);
      if not TdxSpreadSheetHTMLFormat.CellAutoHeight and IsSingeLineCell(AMergedCell) then
        StreamWriter.Write('max-height: %dpx; ', [FCurrentRowSize]);
      StreamWriter.Write('"');
    end;
  end;

  function GetInscribedContainer: TdxSpreadSheetContainer;
  begin
    if not InscribedContainers.TryGetValue(ACell, Result) then
      Result := nil;
  end;

var
  AContainer: TdxSpreadSheetContainerAccess;
  AMergedCell: TdxSpreadSheetMergedCell;
begin
  AMergedCell := View.MergedCells.FindCell(ARowIndex, AColumnIndex);
  if (AMergedCell <> nil) and ((AMergedCell.Area.Left <> AColumnIndex) or (AMergedCell.Area.Top <> ARowIndex)) then
    Exit;

  StreamWriter.Write('<td class="%s"', [GetCellStyleName(ARowIndex, AColumnIndex, ACell)]);

  if AMergedCell <> nil then
  begin
    if dxSpreadSheetAreaWidth(AMergedCell.Area) > 1 then
      StreamWriter.Write(' colspan="%d"', [dxSpreadSheetAreaWidth(AMergedCell.Area)]);
    if dxSpreadSheetAreaHeight(AMergedCell.Area) > 1 then
      StreamWriter.Write(' rowspan="%d"', [dxSpreadSheetAreaHeight(AMergedCell.Area)]);
  end;

  if (ACell <> nil) and (ACell.Style.AlignHorz = ssahGeneral) and ACell.IsNumericValue then
    StreamWriter.Write(' style="text-align: right;"');

  StreamWriter.Write('>');

  AContainer := TdxSpreadSheetContainerAccess(GetInscribedContainer);
  if (ACell <> nil) and (not ACell.IsEmpty or (AContainer <> nil)) then
  begin
    StreamWriter.Write('<div class="cell"');
    WriteDivStyle(ACell, AMergedCell, AContainer);
    StreamWriter.Write('>');

    if ACell.Style.WordWrap then
      WriteCellContent(ACell)
    else
    begin
      StreamWriter.Write('<nobr>');
      WriteCellContent(ACell);
      StreamWriter.Write('</nobr>');
    end;

    if AContainer <> nil then
      WriteContainer(AContainer, cxRectSetOrigin(AContainer.Calculator.CalculateBounds, AContainer.AnchorPoint1.Offset));

    StreamWriter.Write('</div>');
  end;
  StreamWriter.WriteLine('</td>');
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteCellContent(ACell: TdxSpreadSheetCell);
var
  AHyperlink: TdxSpreadSheetHyperlink;
begin
  if HyperlinkCells.TryGetValue(ACell, AHyperlink) then
    StreamWriter.Write(GetHyperlinkAsHTMLText(AHyperlink))
  else
    if ACell.AsSharedString is TdxSpreadSheetFormattedSharedString then
      WriteFormattedText(TdxSpreadSheetFormattedSharedString(ACell.AsSharedString))
    else
      StreamWriter.Write(ConvertSpecialCharacters(ACell.DisplayText));
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteContainer(AContainer: TdxSpreadSheetContainer; AContainerBounds: TRect);
var
  AFileName, ARootPath, AValue: string;
begin
  if Owner.GetImageFileName(ARootPath, AFileName) then
  begin
    PrepareContainerImage(ARootPath + AFileName, AContainerBounds, AContainer);
    AValue := Format('<img src="%s" style="position: absolute; left: %dpx; top: %dpx">',
      [dxReplacePathDelimiter(AFileName, PathDelim, '/'), AContainerBounds.Left, AContainerBounds.Top]);
    AValue := GetHyperlinkAsHTMLText(AContainer.Hyperlink, AValue);
    StreamWriter.WriteLine(AValue);
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteContainers;
var
  I: Integer;
begin
  ProgressHelper.BeginStage(FloatContainers.Count);
  try
    for I := 0 to FloatContainers.Count - 1 do
    begin
      with FloatContainers[I] do
        WriteContainer(Key, cxRectOffset(Value, dxBodyMargin, dxBodyMargin));
      ProgressHelper.NextTask;
    end;
  finally
    ProgressHelper.EndStage;
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteTableFooter;
begin
  StreamWriter.WriteLine('</table>');
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteTableHeader;

  function GetBackgroundColor: TColor;
  begin
    Result := cxGetActualColor(View.CellStyles.DefaultStyle.Brush.BackgroundColor, SpreadSheet.Styles.GetContentStyle(nil).Color);
  end;

  function GetTotalWidth(const AColumnWidths: array of Integer): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to Length(AColumnWidths) - 1 do
      Inc(Result, AColumnWidths[I]);
  end;

  procedure DoWriteTableHeader(const AColumnWidths: array of Integer);
  var
    I: Integer;
  begin
    StreamWriter.WriteLine('<table border="0" cellspacing="0" width="%dpx" bgcolor="%s">',
      [GetTotalWidth(AColumnWidths), GetHTMLColor(GetBackgroundColor)]);
    for I := 0 to Length(AColumnWidths) - 1 do
      StreamWriter.WriteLine('<col width="%dpx"/>', [AColumnWidths[I]]);
  end;

var
  AColumnWidths: array of Integer;
  I: Integer;
begin
  SetLength(AColumnWidths, Area.Right + 1);
  for I := 0 to Area.Right do
    AColumnWidths[I] := TdxSpreadSheetTableColumnsAccess(View.Columns).GetItemSize(I);
  DoWriteTableHeader(AColumnWidths);
  SetLength(AColumnWidths, 0);
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteFormattedText(AText: TdxSpreadSheetFormattedSharedString);

  function GetRunLength(AStartIndex, ARunIndex: Integer): Integer;
  begin
    if ARunIndex + 1 < AText.Runs.Count then
      Result := AText.Runs[ARunIndex + 1].StartIndex - AStartIndex
    else
      Result := Length(AText.Value) - AStartIndex + 1;
  end;

var
  ALength: Integer;
  ARun: TdxSpreadSheetFormattedSharedStringRun;
  I: Integer;
begin
  ALength := GetRunLength(1, -1);
  if ALength > 0 then
    StreamWriter.Write(ConvertSpecialCharacters(Copy(AText.Value, 1, ALength)));

  for I := 0 to AText.Runs.Count - 1 do
  begin
    ARun := AText.Runs[I];
    ALength := GetRunLength(ARun.StartIndex, I);
    if ALength > 0 then
    begin
      StreamWriter.Write('<span class="%s">', [Owner.RegisterFont(ARun.FontHandle)]);
      StreamWriter.Write(ConvertSpecialCharacters(Copy(AText.Value, ARun.StartIndex, ALength)));
      StreamWriter.Write('</span>');
    end;
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteRow(ARowIndex: Integer; ARow: TdxSpreadSheetTableRow);
begin
  if ARow <> nil then
    FCurrentRowSize := ARow.Size
  else
    FCurrentRowSize := View.Rows.DefaultSize;

  WriteRowHeader(ARowIndex, ARow);
  inherited WriteRow(ARowIndex, ARow);
  WriteRowFooter(ARowIndex, ARow);
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteRowFooter(ARowIndex: Integer; ARow: TdxSpreadSheetTableRow);
begin
  StreamWriter.Write('</tr>');
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteRowHeader(ARowIndex: Integer; ARow: TdxSpreadSheetTableRow);
begin
  StreamWriter.WriteLine('<tr height="%dpx">', [FCurrentRowSize])
end;

function TdxSpreadSheetHTMLFormatTableViewWriter.GetOwner: TdxSpreadSheetHTMLFormatWriter;
begin
  Result := TdxSpreadSheetHTMLFormatWriter(inherited Owner);
end;

function TdxSpreadSheetHTMLFormatTableViewWriter.IsInscribedContainer(AContainer: TdxSpreadSheetContainer): Boolean;

  function Check(AItem1, AItem2: TdxSpreadSheetTableItem; AOffset: Integer): Boolean;
  begin
    Result := (AItem1 = AItem2) or (AOffset = 0) and (AItem1.Index + 1 = AItem2.Index);
  end;

begin
  Result := (AContainer.AnchorPoint1.Cell <> nil) and (AContainer.AnchorPoint2.Cell <> nil) and
    Check(AContainer.AnchorPoint1.Cell.Column, AContainer.AnchorPoint2.Cell.Column, AContainer.AnchorPoint2.Offset.X) and
    Check(AContainer.AnchorPoint1.Cell.Row, AContainer.AnchorPoint2.Cell.Row, AContainer.AnchorPoint2.Offset.Y);
end;

{ TdxSpreadSheetHTMLFormatFontStyleWriter }

constructor TdxSpreadSheetHTMLFormatFontStyleWriter.Create(
  AOwner: TdxSpreadSheetCustomFiler; AWriter: TStreamWriter; AFontStyle: TdxSpreadSheetFontHandle);
begin
  inherited Create(AOwner, AWriter);
  FFontStyle := AFontStyle;
end;

procedure TdxSpreadSheetHTMLFormatFontStyleWriter.Execute;
begin
  WriteFont;
end;

procedure TdxSpreadSheetHTMLFormatFontStyleWriter.WriteFont;
var
  ASize: Integer;
  ATextColor: TColor;
const
  AVertAlign: array[TdxSpreadSheetFontScript] of string = ('baseline', 'super', 'sub');
begin
  ATextColor := FFontStyle.Color;
  if not cxColorIsValid(ATextColor) then
    ATextColor := ContentStyle.TextColor;
  ASize := FFontStyle.Size;
  if FFontStyle.Script <> fsNone then
  begin
    StreamWriter.WriteLine('vertical-align: %s;', [AVertAlign[FFontStyle.Script]]);
    ASize := MulDiv(ASize, 50, 100);
  end;
  StreamWriter.WriteLine('color: %s;', [GetHTMLColor(ATextColor)]);
  StreamWriter.WriteLine('font-family: %s;', [FFontStyle.Name]);
  StreamWriter.WriteLine('font-size: %dpt;', [ASize]);
  StreamWriter.WriteLine('mso-font-charset: %d;', [FFontStyle.Charset]);

  if fsBold in FFontStyle.Style then
    StreamWriter.WriteLine('font-weight: bold;');
  if fsItalic in FFontStyle.Style then
    StreamWriter.WriteLine('font-style: italic;');
  if fsUnderline in FFontStyle.Style then
    StreamWriter.WriteLine('text-decoration: underline;')
  else
    if fsStrikeOut in FFontStyle.Style then
      StreamWriter.WriteLine('text-decoration: line-through;');
end;

function TdxSpreadSheetHTMLFormatFontStyleWriter.GetContentStyle: TcxViewParams;
begin
  Result := SpreadSheet.Styles.GetContentStyle(nil);
end;

{ TdxSpreadSheetHTMLFormatCellStyleWriter }

constructor TdxSpreadSheetHTMLFormatCellStyleWriter.Create(
  AOwner: TdxSpreadSheetCustomFiler; AWriter: TStreamWriter; AStyle: TdxSpreadSheetCellStyleHandle);
begin
  inherited Create(AOwner, AWriter, AStyle.Font);
  FStyle := AStyle;
end;

procedure TdxSpreadSheetHTMLFormatCellStyleWriter.Execute;
begin
  inherited;
  WriteBorders;
  WriteBrush;
  WriteTextAlignment;
end;

procedure TdxSpreadSheetHTMLFormatCellStyleWriter.WriteBorders;
const
  BorderStyle: array[TdxSpreadSheetCellBorderStyle] of string = (
    'solid', 'dotted', 'dotted', 'dashed', 'dashed', 'dashed', 'solid',
    'dashed', 'solid', 'dashed', 'dashed', 'solid', 'solid', 'double', 'solid'
  );
  SideName: array[TcxBorder] of string = (
    'left', 'top', 'right', 'bottom'
  );
var
  ABorderColor: TColor;
  ADefaultBorderColor: TColor;
  ASide: TcxBorder;
begin
  if SpreadSheet.ActiveSheetAsTable.Options.ActualGridLines then
    ADefaultBorderColor := cxGetActualColor(SpreadSheet.OptionsView.GridLineColor, clBtnShadow)
  else
    ADefaultBorderColor := clNone;

  for ASide := Low(TcxBorder) to High(TcxBorder) do
  begin
    ABorderColor := Style.Borders.BorderColor[ASide];
    if Style.Borders.BorderStyle[ASide] = sscbsDefault then
      ABorderColor := cxGetActualColor(ABorderColor, ADefaultBorderColor)
    else
      ABorderColor := cxGetActualColor(ABorderColor, clBlack);

    StreamWriter.WriteLine('border-%s: %dpx %s %s;', [SideName[ASide],
      IfThen(ABorderColor <> clNone, dxSpreadSheetBorderStyleThickness[Style.Borders.BorderStyle[ASide]]),
      BorderStyle[Style.Borders.BorderStyle[ASide]], GetHTMLColor(ABorderColor)]);
  end;
end;

procedure TdxSpreadSheetHTMLFormatCellStyleWriter.WriteBrush;
begin
  if Style.Brush.Style = sscfsSolid then
    StreamWriter.Write('background-color: %s;', [GetHTMLColor(cxGetActualColor(Style.Brush.BackgroundColor, ContentStyle.Color))]);
end;

procedure TdxSpreadSheetHTMLFormatCellStyleWriter.WriteTextAlignment;
const
  AlignHorzMap: array[TdxSpreadSheetDataAlignHorz] of string = (
    'left', 'left', 'center', 'right', 'justify', 'justify', 'justify'
  );
  AlignVertMap: array[TdxSpreadSheetDataAlignVert] of string = (
    'top', 'middle', 'bottom', 'middle', 'middle'
  );
begin
  StreamWriter.WriteLine('text-align: %s;', [AlignHorzMap[Style.AlignHorz]]);
  if Style.AlignHorzIndent > 0 then
  begin
    if Style.AlignHorz in [ssahLeft, ssahDistributed] then
      StreamWriter.WriteLine('padding-left: %dpx;', [Style.AlignHorzIndent]);
    if Style.AlignHorz in [ssahRight, ssahDistributed] then
      StreamWriter.WriteLine('padding-right: %dpx;', [Style.AlignHorzIndent]);
  end;
  StreamWriter.WriteLine('vertical-align: %s;', [AlignVertMap[Style.AlignVert]]);
end;

initialization
  TdxSpreadSheetHTMFormat.Register;
  TdxSpreadSheetHTMLFormat.Register;

finalization
  TdxSpreadSheetHTMLFormat.Unregister;
  TdxSpreadSheetHTMFormat.Unregister;
end.
