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

unit dxSpreadSheetFormattedTextUtils;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, Classes, Types, SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  // CX
  cxClasses, dxCore, cxGraphics, dxCoreClasses, cxFormats, cxControls, cxGeometry, cxLookAndFeelPainters,
  dxGDIPlusClasses, cxDrawTextUtils, dxStringHelper,
  // SpreadSheet
  dxSpreadSheetCore, dxSpreadSheetCoreStyles, dxSpreadSheetTypes, dxSpreadSheetClasses, dxSpreadSheetUtils,
  dxSpreadSheetStrs, dxSpreadSheetGraphics, dxSpreadSheetNumberFormat, dxSpreadSheetPrinting, dxSpreadSheetHyperlinks,
  dxSpreadSheetProtection;

{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "wininet.lib"'}
{$ENDIF}

type
  { TdxSpreadSheetFormattedTextService }

  TdxSpreadSheetFormattedTextService = class(TdxSpreadSheetTextService)
  strict private
    class function CalculateValueSize(ACell: TdxSpreadSheetCell; AWidth: Integer): TSize;
    class function GetCache(ACell: TdxSpreadSheetCell): TdxSpreadSheetFormattedSharedStringCache;
    class function GetRender(ACell: TdxSpreadSheetCell): TObject; overload;
    class function GetRender(ACell: TdxSpreadSheetCell; const ABounds: TRect): TObject; overload;
  protected
    class function HasDefaultCharacterProperties(const AEditValue: string): Boolean;
    class procedure ApplyDefaultStyle(ACell: TdxSpreadSheetCell; const AEditValue: string); override;
    class procedure CalculateTextBounds(ACanvas: TcxCanvas; ACell: TdxSpreadSheetTableViewCustomDataCellViewInfo; var ATextBounds: TRect); override;
    class procedure DrawValue(ACanvas: TcxCanvas; ACell: TdxSpreadSheetTableViewCustomDataCellViewInfo; ABounds: TRect); override;
    class function IsRTFText(const S: string): Boolean; inline;
    class function SetAsRTFValue(ACell: TdxSpreadSheetCell; const AEditValue: string; AForce: Boolean): Boolean;
  public
    class procedure CalculateSize(ACell: TdxSpreadSheetCell; ACanvas: TcxCanvas;
      const ABounds: TRect; AIsMerged: Boolean; AWidth, AHeight: PInteger); override;
    class function IsFormattedEditValue(const AEditValue: string): Boolean; override;
    class function IsFormattedTextValue(ACell: TdxSpreadSheetCell): Boolean; override;
    class function IsRTFSupported: Boolean; override;
    class function ForceSetAsRTF(ACell: TdxSpreadSheetCell; const AEditValue: string): Boolean; override;
    class function GetAsRTF(ACell: TdxSpreadSheetCell; var AValue: string): Boolean; override;
    class function SetAsRTF(ACell: TdxSpreadSheetCell; const AEditValue: string): Boolean; override;
  end;

implementation

uses
  StrUtils, dxGenerics, dxCoreGraphics, dxGDIPlusAPI, dxTypeHelpers,
  dxCharacters,
  dxRichEdit.Utils.Types,
  dxRichEdit.NativeApi,
  dxRichEdit.Options.Simple,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.PieceTable.InternalAPI,
  dxRichEdit.Export.Rtf.Keywords,
  dxRichEdit.Inplace.Render;

type
  TdxSpreadSheetCellAccess = class(TdxSpreadSheetCell);
  TdxCustomSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetCustomFontAccess = class(TdxSpreadSheetCustomFont);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TdxSpreadSheetTableViewCellViewInfoAccess = class(TdxSpreadSheetTableViewCellViewInfo);
  TdxTextRunBaseAccess = class(TdxTextRunBase);

  { TdxRichEditSpreadSheetFormattedSharedStringImporter }

  TdxRichEditSpreadSheetFormattedSharedStringImporter = class
  strict private
    FWordWrap: Boolean;
    FDefaultTextColor: TColor;
    FDocumentModel: TdxDocumentModel;
    FPosition: TdxInputPosition;
    FRuns: TdxSpreadSheetFormattedSharedStringRuns;
    function GetPieceTable: TdxPieceTable;
  protected
    function AdjustTextLines(const S: string): string;
    class procedure ApplyDefaultFormatting(ADocumentModel: TdxDocumentModel; ACell: TdxSpreadSheetCell); overload; static;
    procedure ApplyDefaultFormatting(ACell: TdxSpreadSheetCell); overload;
    procedure ApplyFormatting(AFontHandle: TdxSpreadSheetFontHandle); overload;
    class procedure ApplyFormatting(const ACharacterProperties: IdxCharacterProperties;
      AFontHandle: TdxSpreadSheetFontHandle; ADefaultTextColor: TColor); overload; static;
    procedure ImportCore(const AString: string); virtual;
    function PrepareRuns(AString: TdxSpreadSheetFormattedSharedString): TdxSpreadSheetFormattedSharedStringRuns;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);

    procedure Import(ACell: TdxSpreadSheetCell);

    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  { TdxRichEditSpreadSheetFormattedSharedStringExporter }

  TdxRichEditSpreadSheetFormattedSharedStringExporter = class
  strict private
    FDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
  protected
    function AdjustTextLines(const S: string): string;
    procedure ApplyFormattingToCell(ACell: TdxSpreadSheetCell; AProperties: TdxCharacterProperties);
    procedure ApplyDefaultFormatting(ACell: TdxSpreadSheetCell);
    procedure ApplyFirstRunFormatting(ACell: TdxSpreadSheetCell);
    procedure ApplyFormatting(AMergedProperties: TdxMergedCharacterProperties; AProperties: TdxCharacterProperties; AFontHandle: TdxSpreadSheetFontHandle); overload;
    procedure ApplyFormatting(ARun: TdxTextRunBase; AFontHandle: TdxSpreadSheetFontHandle); overload;
    function GetText: string;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);

    procedure Export(ACell: TdxSpreadSheetCell);

    property PieceTable: TdxPieceTable read GetPieceTable;
    property DocumentModel: TdxDocumentModel read FDocumentModel;
  end;

function IsRTFContent(APieceTable: TdxPieceTable): Boolean;
begin
  Result := APieceTable.Runs.Count - APieceTable.Paragraphs.Count > 1;
end;

{ TdxRichEditSpreadSheetFormattedSharedStringImporter }

constructor TdxRichEditSpreadSheetFormattedSharedStringImporter.Create(
  ADocumentModel: TdxDocumentModel);
begin
  Assert(ADocumentModel <> nil);
  inherited Create;
  FDocumentModel := ADocumentModel;
end;

procedure TdxRichEditSpreadSheetFormattedSharedStringImporter.Import(
  ACell: TdxSpreadSheetCell);
var
  AUpdateOptions: TdxFieldUpdateOnLoadOptions;
  AString: TdxSpreadSheetFormattedSharedString;
begin
  Assert(ACell <> nil);
  if not (ACell.AsSharedString is TdxSpreadSheetFormattedSharedString) then
    Exit;
  AString := ACell.AsSharedString as TdxSpreadSheetFormattedSharedString;
  DocumentModel.BeginSetContent;
  try
    FRuns := PrepareRuns(AString);
    try
      FPosition := TdxInputPosition.Create(PieceTable);
      try
        ApplyDefaultFormatting(ACell);
        ImportCore(AString.Value);
      finally
        FreeAndNil(FPosition);
      end;
    finally
      FreeAndNil(FRuns);
    end;
    PieceTable.FixLastParagraph;
  finally
    AUpdateOptions := TdxFieldUpdateOnLoadOptions.Create(False, False);
    try
      DocumentModel.EndSetContent(TdxDocumentModelChangeType.LoadNewDocument, False, False, AUpdateOptions);
    finally
      AUpdateOptions.Free;
    end;
  end;
end;

procedure TdxRichEditSpreadSheetFormattedSharedStringImporter.ApplyFormatting(AFontHandle: TdxSpreadSheetFontHandle);
begin
  ApplyFormatting(FPosition.CharacterFormatting, AFontHandle, FDefaultTextColor);
end;

class procedure TdxRichEditSpreadSheetFormattedSharedStringImporter.ApplyFormatting(
  const ACharacterProperties: IdxCharacterProperties; AFontHandle: TdxSpreadSheetFontHandle;
  ADefaultTextColor: TColor);
const
  AFontUnderlineTypeMap: array[Boolean] of TdxUnderlineType = (TdxUnderlineType.None, TdxUnderlineType.Single);
  AFontStrikeoutTypeMap: array[Boolean] of TdxStrikeoutType = (TdxStrikeoutType.None, TdxStrikeoutType.Single);
  AFontScriptTypeMap: array[TdxSpreadSheetFontScript] of TdxCharacterFormattingScript = (TdxCharacterFormattingScript.Normal,
    TdxCharacterFormattingScript.Superscript, TdxCharacterFormattingScript.Subscript);
begin
  ACharacterProperties.FontName := AFontHandle.Name;
  ACharacterProperties.ForeColor := TdxAlphaColors.FromColor(cxGetActualColor(AFontHandle.Color, ADefaultTextColor));
  ACharacterProperties.DoubleFontSize := Abs(AFontHandle.Size * 2);
  ACharacterProperties.FontBold := fsBold in AFontHandle.Style;
  ACharacterProperties.FontItalic := fsItalic in AFontHandle.Style;
  ACharacterProperties.FontUnderlineType := AFontUnderlineTypeMap[fsUnderline in AFontHandle.Style];
  ACharacterProperties.FontStrikeoutType := AFontStrikeoutTypeMap[fsStrikeOut in AFontHandle.Style];
  ACharacterProperties.Script := AFontScriptTypeMap[AFontHandle.Script];
end;

class procedure TdxRichEditSpreadSheetFormattedSharedStringImporter.ApplyDefaultFormatting(
  ADocumentModel: TdxDocumentModel; ACell: TdxSpreadSheetCell);
var
  AFontHandle: TdxSpreadSheetFontHandle;
  ADefaultTextColor: TColor;
begin
  AFontHandle := TdxSpreadSheetCustomFontAccess(ACell.Style.Font).Handle;
  ADefaultTextColor := TdxSpreadSheetTableViewAccess(ACell.View).ViewInfo.ContentParams.TextColor;
  ApplyFormatting(ADocumentModel.DefaultCharacterProperties, AFontHandle, ADefaultTextColor);
end;

procedure TdxRichEditSpreadSheetFormattedSharedStringImporter.ApplyDefaultFormatting(ACell: TdxSpreadSheetCell);
const
  AlignmentMap: array[TdxSpreadSheetDataAlignHorz] of TdxRichEditParagraphAlignment =
    (TdxRichEditParagraphAlignment.Left, TdxRichEditParagraphAlignment.Left,
    TdxRichEditParagraphAlignment.Center, TdxRichEditParagraphAlignment.Right,
    TdxRichEditParagraphAlignment.Justify, TdxRichEditParagraphAlignment.Justify,
    TdxRichEditParagraphAlignment.Justify);
var
  AAlign: TdxSpreadSheetDataAlignHorz;
begin
  FDefaultTextColor := TdxSpreadSheetTableViewAccess(ACell.View).ViewInfo.ContentParams.TextColor;
  FWordWrap := ACell.Style.WordWrap;
  AAlign := ACell.Style.AlignHorz;
  ApplyDefaultFormatting(DocumentModel, ACell);
  DocumentModel.DefaultParagraphProperties.Alignment := AlignmentMap[AAlign];
end;

procedure TdxRichEditSpreadSheetFormattedSharedStringImporter.ImportCore(
  const AString: string);
var
  I: Integer;
  APos: Integer;
  AStr: string;
  ALength, AMaxLength: Integer;
  ACount: Integer;
begin
  I := 0;
  APos := 1;
  ACount := FRuns.Count;
  AMaxLength := Length(AString);
  repeat
    if I >= ACount then
      ALength := AMaxLength
    else
      ALength := FRuns[I].StartIndex - APos;
    if ALength > 0 then
    begin
      AStr := Copy(AString, APos, ALength);
      Inc(APos, ALength);
      AStr := AdjustTextLines(AStr);
      if Length(AStr) > 0 then
        PieceTable.AppendText(FPosition, AStr);
    end;
    if I < ACount then
      ApplyFormatting(FRuns[I].FontHandle);
    Inc(I);
  until (I > ACount);
end;

function TdxRichEditSpreadSheetFormattedSharedStringImporter.PrepareRuns(AString: TdxSpreadSheetFormattedSharedString): TdxSpreadSheetFormattedSharedStringRuns;
begin
  Result := TdxSpreadSheetFormattedSharedStringRuns.Create;
  Result.Assign(AString.Runs);
  Result.SortList(
    function (Item1, Item2: Pointer): Integer
    begin
      Result := TdxSpreadSheetFormattedSharedStringRun(Item1).StartIndex -
        TdxSpreadSheetFormattedSharedStringRun(Item2).StartIndex;
    end);
end;

function TdxRichEditSpreadSheetFormattedSharedStringImporter.AdjustTextLines(const S: string): string;
begin
  if FWordWrap then
    Result := TdxStringHelper.ReplaceParagraphMarksWithLineBreaks(S)
  else
  begin
    Result := TdxStringHelper.Replace(S, TdxCharacters.ParagraphMark, '');
    Result := TdxStringHelper.Replace(Result, #$0A, '');
  end;
end;

function TdxRichEditSpreadSheetFormattedSharedStringImporter.GetPieceTable: TdxPieceTable;
begin
  Result := DocumentModel.MainPieceTable;
end;

{ TdxRichEditSpreadSheetFormattedSharedStringExporter }

constructor TdxRichEditSpreadSheetFormattedSharedStringExporter.Create(
  ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
end;

procedure TdxRichEditSpreadSheetFormattedSharedStringExporter.Export(ACell: TdxSpreadSheetCell);
const
  AlignHorzMap: array[TdxRichEditParagraphAlignment] of TdxSpreadSheetDataAlignHorz =
    (ssahLeft, ssahRight, ssahCenter, ssahJustify);
var
  AResult: TdxSpreadSheetFormattedSharedString;
  ARun: TdxSpreadSheetFormattedSharedStringRun;
  AFontHandle: TdxSpreadSheetFontHandle;
  I: Integer;
  AValue: string;
  AParagraphRun: TdxParagraphRun;
  AAlign: TdxSpreadSheetDataAlignHorz;
  AWordWrap: Boolean;
begin
  Assert(ACell <> nil);
  AValue := AdjustTextLines(GetText);
  AWordWrap := ACell.Style.WordWrap or (TdxStringHelper.IndexOf(AValue, #$0A) >= 0);
  AResult := TdxSpreadSheetFormattedSharedString.CreateObject(AValue);
  AAlign := ACell.Style.AlignHorz;
  AValue := '';
  ApplyDefaultFormatting(ACell);
  for I := 0 to PieceTable.Runs.Count - 1 do
  begin
    if PieceTable.Runs[I].InheritsFrom(TdxParagraphRun) then
    begin
      AParagraphRun := TdxParagraphRun(PieceTable.Runs[I]);
      AAlign := AlignHorzMap[AParagraphRun.Paragraph.Alignment];
      AValue := AValue + #$0A;
    end;
    if not PieceTable.Runs[I].InheritsFrom(TdxTextRun) then
      Continue;
    ARun := AResult.Runs.Add;
    AFontHandle := ACell.SpreadSheet.CellStyles.Fonts.CreateFont;
    AFontHandle.Assign(TdxSpreadSheetCustomFontAccess(ACell.Style.Font).Handle);
    ApplyFormatting(PieceTable.Runs[I], AFontHandle);
    ARun.FontHandle := ACell.SpreadSheet.CellStyles.Fonts.AddFont(AFontHandle);
    ARun.StartIndex := Length(AValue) + 1;
    AValue := AValue + AdjustTextLines(TdxTextRunBaseAccess(PieceTable.Runs[I]).GetText);
  end;
  ACell.Style.WordWrap := AWordWrap;
  ACell.Style.AlignHorz := AAlign;

  AResult := TdxCustomSpreadSheetAccess(ACell.SpreadSheet).StringTable.Add(AResult) as TdxSpreadSheetFormattedSharedString;
  TdxSpreadSheetCellAccess(ACell).SetAsSharedString(AResult);
end;

function TdxRichEditSpreadSheetFormattedSharedStringExporter.GetPieceTable: TdxPieceTable;
begin
  Result := DocumentModel.MainPieceTable;
end;

function TdxRichEditSpreadSheetFormattedSharedStringExporter.AdjustTextLines(const S: string): string;
begin
  Result := TdxStringHelper.Replace(S, #$0D#$0A, #$0A);
  Result := TdxStringHelper.Replace(Result, TdxCharacters.ParagraphMark, #$0A);
  Result := TdxStringHelper.Replace(Result, TdxCharacters.LineBreak, #$0A);
end;

procedure TdxRichEditSpreadSheetFormattedSharedStringExporter.ApplyFormattingToCell(
  ACell: TdxSpreadSheetCell; AProperties: TdxCharacterProperties);
var
  AFontHandle: TdxSpreadSheetFontHandle;
  AMergedProperties: TdxMergedCharacterProperties;
begin
  ACell.Style.BeginUpdate;
  try
    AFontHandle := TdxSpreadSheetCustomFontAccess(ACell.Style.Font).Handle.Clone;
    try
      AMergedProperties := TdxMergedCharacterProperties.Create(AProperties);
      try
        ApplyFormatting(AMergedProperties, AProperties, AFontHandle);
      finally
        AMergedProperties.Free;
      end;
      TdxSpreadSheetCustomFontAccess(ACell.Style.Font).Handle :=
        ACell.SpreadSheet.CellStyles.Fonts.AddFont(AFontHandle);
    except
      AFontHandle.Free;
    end;
  finally
    ACell.Style.EndUpdate;
  end;
end;

procedure TdxRichEditSpreadSheetFormattedSharedStringExporter.ApplyDefaultFormatting(ACell: TdxSpreadSheetCell);
var
  AProperties: TdxCharacterProperties;
begin
  AProperties := DocumentModel.DefaultCharacterProperties;
  ApplyFormattingToCell(ACell, AProperties);
end;

procedure TdxRichEditSpreadSheetFormattedSharedStringExporter.ApplyFirstRunFormatting(ACell: TdxSpreadSheetCell);
var
  AProperties: TdxCharacterProperties;
begin
  if DocumentModel.MainPieceTable.Runs.Count > 0 then
  begin
    AProperties := DocumentModel.MainPieceTable.Runs[0].CharacterProperties;
    ApplyFormattingToCell(ACell, AProperties);
  end;
end;

procedure TdxRichEditSpreadSheetFormattedSharedStringExporter.ApplyFormatting(
  AMergedProperties: TdxMergedCharacterProperties;
  AProperties: TdxCharacterProperties; AFontHandle: TdxSpreadSheetFontHandle);
const
  AFontScriptTypeMap: array[TdxCharacterFormattingScript] of TdxSpreadSheetFontScript =
    (fsNone, fsSubscript, fsSuperscript);
var
  AStyle: TFontStyles;
begin
  if AProperties.UseFontName then
    AFontHandle.Name := AMergedProperties.Info.FontName;
  if AProperties.UseForeColor then
    AFontHandle.Color := TdxAlphaColors.ToColor(AMergedProperties.Info.ForeColor);
  if AProperties.UseDoubleFontSize then
    AFontHandle.Size := AMergedProperties.Info.DoubleFontSize div 2;
  if AProperties.UseScript then
    AFontHandle.Script := AFontScriptTypeMap[AMergedProperties.Info.Script];
  AStyle := [];
  if AProperties.UseFontBold and AMergedProperties.Info.FontBold then
    Include(AStyle, fsBold);
  if AProperties.UseFontItalic and AMergedProperties.Info.FontItalic then
    Include(AStyle, fsItalic);
  if AProperties.UseFontUnderlineType and (AMergedProperties.Info.FontUnderlineType <> TdxUnderlineType.None) then
    Include(AStyle, fsUnderline);
  if AProperties.UseFontStrikeoutType and (AMergedProperties.Info.FontStrikeoutType <> TdxStrikeoutType.None) then
    Include(AStyle, fsStrikeOut);
  AFontHandle.Style := AStyle;
end;

procedure TdxRichEditSpreadSheetFormattedSharedStringExporter.ApplyFormatting(
  ARun: TdxTextRunBase; AFontHandle: TdxSpreadSheetFontHandle);
var
  AMergedProperties: TdxMergedCharacterProperties;
  AProperties: TdxCharacterProperties;
begin
  AMergedProperties := ARun.GetMergedCharacterProperties;
  try
    AProperties := ARun.CharacterProperties;
    ApplyFormatting(AMergedProperties, AProperties, AFontHandle);
  finally
    AMergedProperties.Free;
  end;
end;

function TdxRichEditSpreadSheetFormattedSharedStringExporter.GetText: string;
var
  AInternalApi: TdxInternalAPI;
begin
  AInternalApi := TdxInternalAPI.Create(DocumentModel);
  try
    Result := AInternalApi.Text;
  finally
    AInternalApi.Free;
  end;
end;

{ TdxSpreadSheetFormattedTextService }

class function TdxSpreadSheetFormattedTextService.IsRTFSupported: Boolean;
begin
  Result := True;
end;

class function TdxSpreadSheetFormattedTextService.GetAsRTF(ACell: TdxSpreadSheetCell; var AValue: string): Boolean;
var
  ARender: TdxRichEditRender;
begin
  Result := IsFormattedTextValue(ACell);
  if Result then
  begin
    ARender := TdxRichEditRender(GetRender(ACell));
    Result := IsRTFContent(ARender.DocumentModel.MainPieceTable);
    if Result then
      AValue := ARender.Document.RtfText;
  end;
end;

class function TdxSpreadSheetFormattedTextService.SetAsRTF(ACell: TdxSpreadSheetCell; const AEditValue: string): Boolean;
begin
  Result := SetAsRTFValue(ACell, AEditValue, False);
end;

class function TdxSpreadSheetFormattedTextService.HasDefaultCharacterProperties(const AEditValue: string): Boolean;
begin
  Result := TdxStringHelper.StartsWith(AEditValue, '{\rtf') and TdxStringHelper.Contains(AEditValue, TdxRtfExportSR.DefaultCharacterProperties);
end;

class procedure TdxSpreadSheetFormattedTextService.ApplyDefaultStyle(ACell: TdxSpreadSheetCell; const AEditValue: string);
var
  ARender: TdxRichEditRender;
  AExporter: TdxRichEditSpreadSheetFormattedSharedStringExporter;
begin
  if not IsRTFText(AEditValue) then
    Exit;
  ARender := TdxRichEditRender.Create;
  try
    ARender.Document.RtfText := AEditValue;
    AExporter := TdxRichEditSpreadSheetFormattedSharedStringExporter.Create(ARender.DocumentModel);
    try
      if HasDefaultCharacterProperties(AEditValue) then
        AExporter.ApplyDefaultFormatting(ACell)
      else
        AExporter.ApplyFirstRunFormatting(ACell);
    finally
      AExporter.Free;
    end;
  finally
    ARender.Free;
  end;
end;

class procedure TdxSpreadSheetFormattedTextService.CalculateSize(ACell: TdxSpreadSheetCell;
  ACanvas: TcxCanvas; const ABounds: TRect; AIsMerged: Boolean; AWidth, AHeight: PInteger);
var
  AResult: TSize;
  AValueWidth: Integer;
begin
  if IsFormattedTextValue(ACell) then
  begin
    if (AHeight <> nil) and (AWidth = nil) then
      AValueWidth := ABounds.Width
    else
      AValueWidth := -1;

    AResult := CalculateValueSize(ACell, AValueWidth);
    if AWidth <> nil then
      AWidth^ := AResult.cx;
    if AHeight <> nil then
      AHeight^ := AResult.cy;
  end
  else
    inherited CalculateSize(ACell, ACanvas, ABounds, AIsMerged, AWidth, AHeight);
end;

class procedure TdxSpreadSheetFormattedTextService.CalculateTextBounds(
  ACanvas: TcxCanvas; ACell: TdxSpreadSheetTableViewCustomDataCellViewInfo; var ATextBounds: TRect);
var
  ASize: TSize;
begin
  if IsFormattedTextValue(ACell.Cell) then
  begin
    ASize := CalculateValueSize(ACell.Cell, ATextBounds.Width);
    if ACell.AlignVert <> taTop then
    begin
      if ACell.AlignVert = taBottom then
        ATextBounds.Offset(0, ATextBounds.Height - ASize.cy);
      if ACell.AlignVert = taCenterY then
        ATextBounds.Offset(0, (ATextBounds.Height - ASize.cy) div 2);
    end;
    ATextBounds.Size := ASize;
  end
  else
    inherited CalculateTextBounds(ACanvas, ACell, ATextBounds);
end;

class procedure TdxSpreadSheetFormattedTextService.DrawValue(
  ACanvas: TcxCanvas; ACell: TdxSpreadSheetTableViewCustomDataCellViewInfo; ABounds: TRect);
const
  ACorrection: array[TcxTextAlignX] of Integer = (-cxTextOffset, 0, 1, 0, 0);
begin
  if IsFormattedTextValue(ACell.Cell) then
  begin
    ACanvas.SaveClipRegion;
    try
      TdxRichEditRender(GetRender(ACell.Cell, ABounds)).Draw(ACanvas, cxPointOffset(ABounds.TopLeft, ACorrection[ACell.AlignHorz], 0))
    finally
      ACanvas.RestoreClipRegion;
    end
  end
  else
    inherited DrawValue(ACanvas, ACell, ABounds);
end;

class function TdxSpreadSheetFormattedTextService.ForceSetAsRTF(ACell: TdxSpreadSheetCell; const AEditValue: string): Boolean;
begin
  Result := SetAsRTFValue(ACell, AEditValue, True);
end;

class function TdxSpreadSheetFormattedTextService.IsFormattedEditValue(const AEditValue: string): Boolean;
var
  ARender: TdxRichEditRender;
begin
  Result := IsRTFText(AEditValue);
  if Result then
  begin
    ARender := TdxRichEditRender.Create;
    try
      ARender.Document.RtfText := AEditValue;
      Result := IsRTFContent(ARender.DocumentModel.MainPieceTable);
    finally
      ARender.Free;
    end;
  end;
end;

class function TdxSpreadSheetFormattedTextService.IsFormattedTextValue(ACell: TdxSpreadSheetCell): Boolean;
begin
  Result := (ACell <> nil) and (ACell.DataType = cdtString) and (ACell.AsSharedString is TdxSpreadSheetFormattedSharedString);
end;

class function TdxSpreadSheetFormattedTextService.IsRTFText(const S: string): Boolean;
begin
  Result := TdxStringHelper.StartsWith(S, '{\rtf');
end;

class function TdxSpreadSheetFormattedTextService.SetAsRTFValue(ACell: TdxSpreadSheetCell; const AEditValue: string; AForce: Boolean): Boolean;
var
  ARender: TdxRichEditRender;
  AExporter: TdxRichEditSpreadSheetFormattedSharedStringExporter;
  ARange: IdxRichEditDocumentRange;
  APosition: IdxRichEditDocumentPosition;
begin
  Result := AForce and IsRTFText(AEditValue) or IsFormattedEditValue(AEditValue);
  if Result then
  begin
    GetCache(ACell).RemoveItems(TdxSpreadSheetFormattedSharedString(ACell.AsSharedString));
    ARender := TdxRichEditRender.Create;
    try
      if not HasDefaultCharacterProperties(AEditValue) then
      begin
        ARender.DocumentModel.BeginSetContentForExport;
        try
          APosition := ARender.Document.CreatePosition(0);
          try
            TdxRichEditSpreadSheetFormattedSharedStringImporter.ApplyDefaultFormatting(ARender.DocumentModel, ACell);
            ARange := ARender.Document.InsertRtfText(APosition, AEditValue, TdxRichEditInsertOptions.KeepSourceFormatting);
            ARender.DocumentModel.MainPieceTable.FixLastParagraph;
          finally
            APosition := nil;
            ARange := nil;
          end;
        finally
          ARender.DocumentModel.EndSetContentForExport(TdxDocumentModelChangeType.LoadNewDocument, False);
        end;
      end
      else
        ARender.Document.RtfText := AEditValue;
      AExporter := TdxRichEditSpreadSheetFormattedSharedStringExporter.Create(ARender.DocumentModel);
      try
        AExporter.Export(ACell);
      finally
        AExporter.Free;
      end;
    finally
      ARender.Free;
    end;
  end;
end;

class function TdxSpreadSheetFormattedTextService.GetCache(ACell: TdxSpreadSheetCell): TdxSpreadSheetFormattedSharedStringCache;
begin
  Result := TdxCustomSpreadSheetAccess(ACell.View.SpreadSheet).FormattedSharedStringCache;
end;

class function TdxSpreadSheetFormattedTextService.GetRender(ACell: TdxSpreadSheetCell): TObject;
begin
  Result := GetRender(ACell, cxRect(0, 0, ACell.Column.Size, ACell.Row.Size));
end;

class function TdxSpreadSheetFormattedTextService.GetRender(ACell: TdxSpreadSheetCell; const ABounds: TRect): TObject;
var
  AString: TdxSpreadSheetFormattedSharedString;
  ACache: TdxSpreadSheetFormattedSharedStringCache;
  ACachedRender: TObject;
  AImporter: TdxRichEditSpreadSheetFormattedSharedStringImporter;
begin
  AString := ACell.AsSharedString as TdxSpreadSheetFormattedSharedString;
  ACache := GetCache(ACell);
  if ACache.TryGetRender(AString, ACell.StyleHandle, ABounds.Size, ACachedRender) then
    Result := TdxRichEditRender(ACachedRender)
  else
  begin
    Result := TdxRichEditRender.Create;
    TdxRichEditRender(Result).Bounds := cxRectOffset(ABounds, -ABounds.Left, -ABounds.Top);
    ACache.AddRender(AString, ACell.StyleHandle, ABounds.Size, Result);
    AImporter := TdxRichEditSpreadSheetFormattedSharedStringImporter.Create(TdxRichEditRender(Result).DocumentModel);
    try
      AImporter.Import(ACell);
    finally
      AImporter.Free;
    end;
  end;
end;

class function TdxSpreadSheetFormattedTextService.CalculateValueSize(ACell: TdxSpreadSheetCell; AWidth: Integer): TSize;
var
  AString: TdxSpreadSheetFormattedSharedString;
begin
  Assert(ACell <> nil);
  if not ACell.Style.WordWrap then
    AWidth := -1;

  Assert(ACell.AsSharedString is TdxSpreadSheetFormattedSharedString);
  AString := ACell.AsSharedString as TdxSpreadSheetFormattedSharedString;
  if GetCache(ACell).TryGetSize(AString, ACell.StyleHandle, AWidth, Result) then
    Exit;

  Result := TdxRichEditRender.CalculateSize(
    procedure (ADocumentModel: TdxDocumentModel)
    var
      AImporter: TdxRichEditSpreadSheetFormattedSharedStringImporter;
    begin
      AImporter := TdxRichEditSpreadSheetFormattedSharedStringImporter.Create(ADocumentModel);
      try
        AImporter.Import(ACell);
      finally
        AImporter.Free;
      end;
    end, AWidth);
  GetCache(ACell).AddSize(AString, ACell.StyleHandle, AWidth, Result);
end;

initialization
  TdxSpreadSheetFormattedTextService.Register;

finalization
  TdxSpreadSheetFormattedTextService.UnRegister;
end.
