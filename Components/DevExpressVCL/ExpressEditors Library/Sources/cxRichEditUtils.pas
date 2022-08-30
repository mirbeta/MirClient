{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit cxRichEditUtils;

{$I cxVer.inc}

{$HPPEMIT '#include <RichEdit.h>'}

interface

uses
  Windows, Messages, Classes, Controls, RichEdit, Graphics, ComCtrls, Contnrs,
  cxEdit, dxDrawRichTextUtils;

const
  {$EXTERNALSYM PFM_TABLEROWDELIMITER}
  PFM_TABLEROWDELIMITER	            = $10000000;
  {$EXTERNALSYM PFE_TABLEROWDELIMITER}
  PFE_TABLEROWDELIMITER             = $1000;

  {$EXTERNALSYM EM_SETTYPOGRAPHYOPTIONS}
  EM_SETTYPOGRAPHYOPTIONS           = WM_USER + 202;
  {$EXTERNALSYM EM_GETTYPOGRAPHYOPTIONS}
  EM_GETTYPOGRAPHYOPTIONS           = WM_USER + 203;
  {$EXTERNALSYM TO_ADVANCEDTYPOGRAPHY}
  TO_ADVANCEDTYPOGRAPHY             = $0001;
  {$EXTERNALSYM TO_SIMPLELINEBREAK}
  TO_SIMPLELINEBREAK                = $0002;
  {$EXTERNALSYM TO_DISABLECUSTOMTEXTOUT}
  TO_DISABLECUSTOMTEXTOUT           = $0004;
  {$EXTERNALSYM TO_ADVANCEDLAYOUT}
  TO_ADVANCEDLAYOUT                 = $0008;

  {$EXTERNALSYM EM_INSERTTABLE}
  EM_INSERTTABLE                    = WM_USER + 232;

type
  { TcxRichCustomAttributes }

  TcxRichCustomAttributes = class(TPersistent)
  private
    FRichEdit: TcxCustomEdit;
    function GetInnerRich: TWinControl;
  protected
    property InnerRich: TWinControl read GetInnerRich;
  public
    constructor Create(ARichEdit: TcxCustomEdit); virtual;

    property RichEdit: TcxCustomEdit read FRichEdit;
  end;

  TcxRichEditColor = (recBlack, recBlue, recCyan, recGreen, recMagenta,
    recRed, recYellow, recWhite, recDarkBlue, recDarkCyan, recDarkGreen, recDarkMagenta,
    recDarkRed, recDarkYellow, recDarkGray, recLightGray);

  { TcxParaAttributes2 }

  TcxParaFormat2Alignment = (pfaLeft, pfaRight, pfaCenter, pfaJustify);
  TcxParaFormat2NumberingStyle = (pfnsRightParenthesis, pfnsParenthesis = $100, pfnsPeriod = $200, pfnsNumberOnly = $300,
    pfnsContinuesList = $400, pfnsStartNew = $8000);
  TcxParaFormat2NumberingType = (pfnNone, pfnSymbols, pfnNumber, pfnLCLetter, pfnUCLetter, pfnLCRoman, pfnUCRoman, pfnSequence);
  TcxParaFormat2LineSpacingRule = (pfsrSingle, pfsrOneAndHalf, pfsrDouble, pfsrRule3, pfsrRule4, pfsrRule5);
  TcxParaFormat2ShadingStyle = (pfsNone, pfsDarkHorizontal, pfsDarkVertical, pfsDarkDownDiagonal, pfsDarkUpDiagonal,
    pfsDarkGrid, pfsDarkTrellis, pfsLightHorizontal, pfsLightVertical, pfsLightDownDiagonal, pfsLightUpDiagonal,
    pfsLightGrid, pfsLightTrellis);
  TcxParaFormat2BorderLocation = (pfblLeft, pfblRight, pfblTop, pfblBottom, pfblInside, pfblOutside, pfblAutocolor);
  TcxParaFormat2BorderLocations = set of TcxParaFormat2BorderLocation;
  TcxParaFormat2BorderStyle = (pfbsNone, pfbs3Per4, pfbs11Per2, pfbs21Per4, pfbs3,
     pfbs41Per2, pfbs6, pfbs3Per4Double, pfbs11Per2Double, pfbs21Per4Double,
     pfbs3Per4Gray, pfbs3Per4GrayDashed);
  TcxParaFormat2TableStyle = (pftsNone, pftsCell, pftsRowDelimiter);
  TcxParaFormat2TabAlignment = (pftaLeft, pftaCenter, pftaRight, pftaDecimal, pftaWordBar);
  TcxParaFormat2TabLeader = (pftlNone, pftlDotted, pftlDashed, pftlUnderlined, pftlThick, pftlDouble);

  TcxParaAttributes2 = class(TcxRichCustomAttributes)
  private
    function GetAlignment: TcxParaFormat2Alignment;
    function GetIndent(const Index: Integer): Longint;
    function GetLineSpacingRule: TcxParaFormat2LineSpacingRule;
    function GetNumberingStart: Word;
    function GetNumberingStyle: TcxParaFormat2NumberingStyle;
    function GetNumberingTab: Word;
    function GetNumberingType: TcxParaFormat2NumberingType;
    function GetPageBreakBefore: Boolean;
    function GetStyle: Smallint;
    function GetTab(Index: Byte): Longint;
    function GetTabAlignment(Index: Integer): TcxParaFormat2TabAlignment;
    function GetTabCount: Integer;
    function GetTabLeader(Index: Integer): TcxParaFormat2TabLeader;
    function GetTableStyle: TcxParaFormat2TableStyle;
    function GetYSpace(const Index: Integer): Longint;
    procedure SetAlignment(Value: TcxParaFormat2Alignment);
    procedure SetIndent(const Index: Integer; const Value: Longint);
    procedure SetLineSpacingRule(Value: TcxParaFormat2LineSpacingRule);
    procedure SetNumberingStart(Value: Word);
    procedure SetNumberingStyle(Value: TcxParaFormat2NumberingStyle);
    procedure SetNumberingTab(Value: Word);
    procedure SetNumberingType(Value: TcxParaFormat2NumberingType);
    procedure SetPageBreakBefore(Value: Boolean);
    procedure SetStyle(Value: Smallint);
    procedure SetTab(Index: Byte; Value: Longint);
    procedure SetTabAlignment(Index: Integer; const Value: TcxParaFormat2TabAlignment);
    procedure SetTabCount(Value: Integer);
    procedure SetTabLeader(Index: Integer; const Value: TcxParaFormat2TabLeader);
    procedure SetTabValues(Index, AWidth: Integer; AAlignment: TcxParaFormat2TabAlignment; ALeader: TcxParaFormat2TabLeader);
    procedure SetYSpace(const Index: Integer; const Value: Longint);
  protected
    procedure GetAttributes(var AParagraph: PARAFORMAT2); virtual;
    procedure InitParagraph(var AParagraph: PARAFORMAT2); virtual;
    procedure SetAttributes(var AParagraph: PARAFORMAT2); virtual;
  public
    procedure Assign(Source: TPersistent); override;

    property Alignment: TcxParaFormat2Alignment read GetAlignment write SetAlignment;

    property NumberingStart: Word read GetNumberingStart write SetNumberingStart;
    property NumberingStyle: TcxParaFormat2NumberingStyle read GetNumberingStyle write SetNumberingStyle;
    property NumberingTab: Word read GetNumberingTab write SetNumberingTab;
    property NumberingType: TcxParaFormat2NumberingType read GetNumberingType write SetNumberingType;

    property AbsoluteIndent: Longint index 3 read GetIndent write SetIndent;
    property FirstIndent: Longint index 0 read GetIndent write SetIndent;
    property OffsetIndent: Longint index 2 read GetIndent write SetIndent;
    property RightIndent: Longint index 1 read GetIndent write SetIndent;

    property Tab[Index: Byte]: Longint read GetTab write SetTab;
    property TabAlignment[Index: Integer]: TcxParaFormat2TabAlignment read GetTabAlignment write SetTabAlignment;
    property TabCount: Integer read GetTabCount write SetTabCount;
    property TabLeader[Index: Integer]: TcxParaFormat2TabLeader read GetTabLeader write SetTabLeader;

    property LineSpacing: Longint index 2 read GetYSpace write SetYSpace;
    property LineSpacingRule: TcxParaFormat2LineSpacingRule read GetLineSpacingRule write SetLineSpacingRule;
    property SpaceAfter: Longint index 1 read GetYSpace write SetYSpace;
    property SpaceBefore: Longint index 0 read GetYSpace write SetYSpace;

    property Style: Smallint read GetStyle write SetStyle;
    property TableStyle: TcxParaFormat2TableStyle read GetTableStyle;

    property PageBreakBefore: Boolean read GetPageBreakBefore write SetPageBreakBefore;
  end;

  { TcxCharAttributes2 }

  TcxCharFormat2UnderlineType = (cfutNone, cfutSolid, cfutDotted, cfutDouble, cfutWordOnly);
  TcxCharFormat2Weight = (cfwDontCare, cfwThin, cfwUltraLight, cfwLight,
    cfwNormal, cfwMedium, cfwSemiBold, cfwBold, cfwUltraBold, cfwHeavy);
  TcxCharFormat2FontStyleEx = (cffsAllCaps, cffsDisabled, cffsEmboss, cffsHidden, cffsImprint,
    cffsOutline, cffsRevised, cffsShadow, cffsSmallCaps, cffsSubscript, cffsSuperscript);
  TcxCharFormat2FontStylesEx = set of TcxCharFormat2FontStyleEx;

  TcxTextAttributes2 = class(TcxRichCustomAttributes)
  private
    FType: TAttributeType;

    function GetEffects: DWORD;
    procedure SetEffects(AMask, AEffects: DWORD);

    function GetCharset: TFontCharset;
    function GetColor(Index: Integer): TColor;
    function GetConsistentAttributes: TConsistentAttributes;
    function GetHeight: Integer;
    function GetName: TFontName;
    function GetOffset: Integer;
    function GetPitch: TFontPitch;
    function GetProtected: Boolean;
    function GetSize: Integer;
    function GetStyle: TFontStyles;
    function GetStyleEx: TcxCharFormat2FontStylesEx;
    function GetUnderlineType: TcxCharFormat2UnderlineType;
    function GetWeight: TcxCharFormat2Weight;
    procedure SetCharset(const Value: TFontCharset);
    procedure SetColor(Index: Integer; const Value: TColor);
    procedure SetHeight(Value: Integer);
    procedure SetName(const Value: TFontName);
    procedure SetOffset(Value: Integer);
    procedure SetPitch(const Value: TFontPitch);
    procedure SetProtected(Value: Boolean);
    procedure SetSize(Value: Integer);
    procedure SetStyle(const Value: TFontStyles);
    procedure SetStyleEx(const Value: TcxCharFormat2FontStylesEx);
    procedure SetUnderlineType(const Value: TcxCharFormat2UnderlineType);
    procedure SetWeight(const Value: TcxCharFormat2Weight);
  protected
    procedure GetAttributes(var AFormat: CHARFORMAT2); virtual;
    procedure InitFormat(var AFormat: CHARFORMAT2); virtual;
    procedure SetAttributes(var AFormat: CHARFORMAT2); virtual;

    function GetFlag: WParam;
  public
    constructor Create(ARichEdit: TcxCustomEdit; AType: TAttributeType); reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;

    property BackgroundColor: TColor index 0 read GetColor write SetColor;
    property Charset: TFontCharset read GetCharset write SetCharset;
    property Color: TColor index 1 read GetColor write SetColor;
    property ConsistentAttributes: TConsistentAttributes read GetConsistentAttributes;
    property Height: Integer read GetHeight write SetHeight;
    property Name: TFontName read GetName write SetName;
    property Offset: Integer read GetOffset write SetOffset;
    property Pitch: TFontPitch read GetPitch write SetPitch;
    property Protected: Boolean read GetProtected write SetProtected;
    property Size: Integer read GetSize write SetSize;
    property Style: TFontStyles read GetStyle write SetStyle;
    property StyleEx: TcxCharFormat2FontStylesEx read GetStyleEx write SetStyleEx;
    property UnderlineType: TcxCharFormat2UnderlineType read GetUnderlineType write SetUnderlineType;
    property Weight: TcxCharFormat2Weight read GetWeight write SetWeight;
  end;

  { Rich Edit Tables }

  TcxRichEditTableRowParams = record
    cbSize: Byte;             // Count of bytes in this structure
    cbCellSize: Byte;         // Count of bytes in TABLECELLPARMS
    cCell: Byte;              // Count of cells
    cRow: Byte;               // Count of rows
    dxCellMargin: Longint;    // Cell left/right margin (\trgaph)
    dxIndent: Longint;        // Row left (right if fRTL indent (similar to \trleft)
    dyHeight: Longint;        // Row height (\trrh)
    nParams: DWORD;           // 0 - 2 bits - Row alignment (like PARAFORMAT::bAlignment, 1/2/3
                              //               \trql, trqr, \trqc)
                              // 3 bit - Display cells in RTL order (\rtlrow)
                              // 4 bit - Keep row together (\trkeep}
                              // 5 bit - Keep row on same page as following row (\trkeepfollow)
                              // 6 bit - Wrap text to right/left (depending on bAlignment)
                              //          (see \tdfrmtxtLeftN, \tdfrmtxtRightN)
                              // 7 bit - lparam points at single struct valid for all cells
  end;
  PcxRichEditTableRowParams = ^TcxRichEditTableRowParams;

  TcxRichEditTableRowWinSevenParams = record
    cbSize: Byte;             // Count of bytes in this structure
    cbCellSize: Byte;         // Count of bytes in TABLECELLPARMS
    cCell: Byte;              // Count of cells
    cRow: Byte;               // Count of rows
    dxCellMargin: Longint;    // Cell left/right margin (\trgaph)
    dxIndent: Longint;        // Row left (right if fRTL indent (similar to \trleft)
    dyHeight: Longint;        // Row height (\trrh)
    nParams: DWORD;           // 0 - 2 bits - Row alignment (like PARAFORMAT::bAlignment, 1/2/3
                              //               \trql, trqr, \trqc)
                              // 3 bit - Display cells in RTL order (\rtlrow)
                              // 4 bit - Keep row together (\trkeep}
                              // 5 bit - Keep row on same page as following row (\trkeepfollow)
                              // 6 bit - Wrap text to right/left (depending on bAlignment)
                              //          (see \tdfrmtxtLeftN, \tdfrmtxtRightN)
                              // 7 bit - lparam points at single struct valid for all cells
    cpStartRow: Longint;
    bTableLevel: Byte;
    iCell: Byte;
  end;

  TcxRichEditTableCellParams = record
    dxWidth: Cardinal;        // Cell width (\cellx)
    nParams: Word;            // 0 - 1 bits - Vertical alignment (0/1/2 = top/center/bottom
                              //              \clvertalt (def), \clvertalc, \clvertalb)
                              // 2 bit - Top cell for vertical merge (\clvmgf)
                              // 3 bit - Merge with cell above (\clvmrg)
                              // 4 bit - Display text top to bottom, right to left (\cltxtbrlv)
    wShading: Word;           // Shading in .01% (\clshdng) e.g., 10000 flips fore/back
    dxBrdrLeft: Smallint;     // Left border width (\clbrdrl\brdrwN) (in twips)
    dyBrdrTop: Smallint;      // Top border width  (\clbrdrt\brdrwN)
    dxBrdrRight: Smallint;    // Right border width (\clbrdrr\brdrwN)
    dyBrdrBottom: Smallint;   // Bottom border width (\clbrdrb\brdrwN)
    crBrdrLeft: COLORREF;     // Left border color (\clbrdrl\brdrcf)
    crBrdrTop: COLORREF;      // Top border color (\clbrdrt\brdrcf)
    crBrdrRight: COLORREF;    // Right border color (\clbrdrr\brdrcf)
    crBrdrBottom: COLORREF;   // Bottom border color (\clbrdrb\brdrcf)
    crBackPat: COLORREF;      // Background color (\clcbpat)
    crForePat: COLORREF;      // Foreground color (\clcfpat)
  end;

  TcxRichEditTableBorderColor = record
    Left: TcxRichEditColor;
    Top: TcxRichEditColor;
    Right: TcxRichEditColor;
    Bottom: TcxRichEditColor;
  end;

  TcxRichEditTableColumnParams = record
    BackgroundColor: TcxRichEditColor;
    BorderColor: TcxRichEditTableBorderColor;
    BorderWidth: TRect;
    ForegroundColor: TcxRichEditColor;
    VerticalAlignment: TcxEditVertAlignment;
    Width: Cardinal;
  end;

  TcxRichEditTableColumnParamsList = array of TcxRichEditTableColumnParams;

  TcxRichEditTableParams = class(TPersistent)
  private
    FAlignment: TAlignment;
    FCellMargins: Cardinal;
    FRowHeight: Cardinal;
    FRowIndent: Cardinal;

    FColumnParams: TcxRichEditTableColumnParamsList;
    function GetColumnParamsCount: Integer;
  protected
    function GetDefaultColumnParams: TcxRichEditTableColumnParams; virtual;
  public
    destructor Destroy; override;

    function AddColumnParams: Integer; overload;
    function AddColumnParams(const AParams: TcxRichEditTableColumnParams): Integer; overload;
    procedure ClearColumnParams;

    procedure InsertTable(ARichEdit: TcxCustomEdit; AColumnCount, ARowCount: Integer);

    property ColumnParams: TcxRichEditTableColumnParamsList read FColumnParams;
    property ColumnParamsCount: Integer read GetColumnParamsCount;

    property Alignment: TAlignment read FAlignment write FAlignment;
    property CellMargins: Cardinal read FCellMargins write FCellMargins;
    property RowHeight: Cardinal read FRowHeight write FRowHeight;
    property RowIndent: Cardinal read FRowIndent write FRowIndent;
  end;

function cxIntegerToParaFormat2BorderLocations(AValue: Integer): TcxParaFormat2BorderLocations;
function cxParaFormat2BorderLocationsToInteger(AValue: TcxParaFormat2BorderLocations): Integer;
function cxRichEditColorToColor(AValue: TcxRichEditColor): TColor;
function cxRichEditGetCharPosition(ARichHandle: THandle; ARichVersion: Integer; ACharIndex: Integer): TPoint;
implementation

uses
  Math, SysUtils, dxCore, cxGeometry, cxGraphics, cxClasses, cxRichEdit;

const
  cxAutoColorMaskMap: array[0..1] of DWORD = (CFE_AUTOBACKCOLOR, CFE_AUTOCOLOR);
  cxAutoColorMap: array[0..1] of TColor = (clWindow, clWindowText);

function cxRichEditGetCharPosition(ARichHandle: THandle; ARichVersion: Integer; ACharIndex: Integer): TPoint;
var
  APosition: LRESULT;
begin
  if InRange(ARichVersion, 200, 299) and not IsWinSevenOrLater then
  begin
    APosition := SendMessage(ARichHandle, EM_POSFROMCHAR, ACharIndex, 0);
    Result.X := SmallInt(LoWord(APosition));
    Result.Y := SmallInt(HiWord(APosition));
  end
  else
    SendMessage(ARichHandle, EM_POSFROMCHAR, WParam(@Result), ACharIndex);
end;

function cxIntegerToParaFormat2BorderLocations(AValue: Integer): TcxParaFormat2BorderLocations;
var
  ALocation: TcxParaFormat2BorderLocation;
  ABit: Word;
begin
  Result := [];
  for ALocation := Low(TcxParaFormat2BorderLocation) to High(TcxParaFormat2BorderLocation) do
  begin
    ABit := 1 shl Ord(ALocation);
    if (ABit and AValue) <> 0 then
      Include(Result, ALocation);
  end;
end;

function cxParaFormat2BorderLocationsToInteger(AValue: TcxParaFormat2BorderLocations): Integer;
var
  ALocation: TcxParaFormat2BorderLocation;
begin
  Result := 0;
  for ALocation := Low(TcxParaFormat2BorderLocation) to High(TcxParaFormat2BorderLocation) do
    if ALocation in AValue then
      Result := Result or 1 shl Ord(ALocation);
end;

function cxRichEditColorToColor(AValue: TcxRichEditColor): TColor;
const
  ResultMap: array[TcxRichEditColor] of TColor = (clBlack, clBlue, clAqua,
    clLime, clFuchsia, clRed, clYellow, clWhite, clNavy, clTeal, clGreen, clPurple,
    clMaroon, clOlive, clDkGray, clLtGray);
begin
  Result := ResultMap[AValue];
end;

function TwipsToPixels(AValue: Integer): Integer;
begin
  Result:= MulDiv(AValue, cxGetPixelsPerInch.cy, 1440);
end;

{ TcxRichCustomAttributes }

constructor TcxRichCustomAttributes.Create(ARichEdit: TcxCustomEdit);
begin
  inherited Create;
  FRichEdit := ARichEdit;
end;

function TcxRichCustomAttributes.GetInnerRich: TWinControl;
begin
  Result := RichEdit.InnerControl;
end;

{ TcxParaAttributes2 }

procedure TcxParaAttributes2.Assign(Source: TPersistent);
var
  AParaAttributes: TcxParaAttributes2;
  I: Integer;
begin
  if Source is TcxParaAttributes2 then
  begin
    AParaAttributes := TcxParaAttributes2(Source);
    Alignment := AParaAttributes.Alignment;
    NumberingStart := AParaAttributes.NumberingStart;
    NumberingStyle := AParaAttributes.NumberingStyle;
    NumberingTab := AParaAttributes.NumberingTab;
    NumberingType := AParaAttributes.NumberingType;
    AbsoluteIndent := AParaAttributes.AbsoluteIndent;
    FirstIndent := AParaAttributes.FirstIndent;
    OffsetIndent := AParaAttributes.OffsetIndent;
    RightIndent := AParaAttributes.RightIndent;
    TabCount := AParaAttributes.TabCount;
    for I := 0 to TabCount - 1 do
      Tab[I] := AParaAttributes.Tab[I];
    LineSpacing := AParaAttributes.LineSpacing;
    LineSpacingRule := AParaAttributes.LineSpacingRule;
    SpaceAfter := AParaAttributes.SpaceAfter;
    SpaceBefore := AParaAttributes.SpaceBefore;
    Style := AParaAttributes.Style;
  end
  else
    inherited;
end;

procedure TcxParaAttributes2.GetAttributes(var AParagraph: PARAFORMAT2);
begin
  InitParagraph(AParagraph);
  if InnerRich.HandleAllocated then
    SendMessage(InnerRich.Handle, EM_GETPARAFORMAT, 0, LPARAM(@AParagraph));
end;

procedure TcxParaAttributes2.InitParagraph(var AParagraph: PARAFORMAT2);
begin
  FillChar(AParagraph, SizeOf(PARAFORMAT2), 0);
  AParagraph.cbSize := SizeOf(PARAFORMAT2);
end;

procedure TcxParaAttributes2.SetAttributes(var AParagraph: PARAFORMAT2);
begin
  InnerRich.HandleNeeded;
  if InnerRich.HandleAllocated then
  begin
    if InnerRich.UseRightToLeftAlignment then
      if AParagraph.wAlignment = PFA_LEFT then
        AParagraph.wAlignment := PFA_RIGHT
      else
        if AParagraph.wAlignment = PFA_RIGHT then
          AParagraph.wAlignment := PFA_LEFT;
    cxSendStructMessage(InnerRich.Handle, EM_SETPARAFORMAT, 0, AParagraph);
  end;
end;

function TcxParaAttributes2.GetAlignment: TcxParaFormat2Alignment;
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  Result := TcxParaFormat2Alignment(AParagraph.wAlignment - 1);
end;

function TcxParaAttributes2.GetIndent(const Index: Integer): Longint;
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  case Index of
    1: Result := TwipsToPixels(AParagraph.dxRightIndent);
    2: Result := TwipsToPixels(AParagraph.dxOffset + AParagraph.dxStartIndent);
  else
    Result := TwipsToPixels(AParagraph.dxStartIndent);
  end;
end;

function TcxParaAttributes2.GetLineSpacingRule: TcxParaFormat2LineSpacingRule;
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  Result := TcxParaFormat2LineSpacingRule(AParagraph.bLineSpacingRule);
end;

function TcxParaAttributes2.GetNumberingStart: Word;
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  Result := AParagraph.wNumberingStart;
end;

function TcxParaAttributes2.GetNumberingStyle: TcxParaFormat2NumberingStyle;
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  Result := TcxParaFormat2NumberingStyle(AParagraph.wNumberingStyle);
end;

function TcxParaAttributes2.GetNumberingTab: Word;
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  Result := TwipsToPixels(AParagraph.wNumberingTab);
end;

function TcxParaAttributes2.GetNumberingType: TcxParaFormat2NumberingType;
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  Result := TcxParaFormat2NumberingType(AParagraph.wNumbering);
end;

function TcxParaAttributes2.GetPageBreakBefore: Boolean;
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  Result := (AParagraph.wReserved and PFE_PAGEBREAKBEFORE) <> 0;
end;

function TcxParaAttributes2.GetStyle: Smallint;
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  Result := AParagraph.sStyle;
end;

function TcxParaAttributes2.GetTab(Index: Byte): Longint;
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  Result := TwipsToPixels(AParagraph.rgxTabs[Index] and $00FFFFFF);
end;

function TcxParaAttributes2.GetTabAlignment(Index: Integer): TcxParaFormat2TabAlignment;
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  Result := TcxParaFormat2TabAlignment((AParagraph.rgxTabs[Index] shr 24) and $0F);
end;

function TcxParaAttributes2.GetTabCount: Integer;
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  Result := AParagraph.cTabCount;
end;

function TcxParaAttributes2.GetTabLeader(Index: Integer): TcxParaFormat2TabLeader;
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  Result := TcxParaFormat2TabLeader(AParagraph.rgxTabs[Index] shr 28);
end;

function TcxParaAttributes2.GetTableStyle: TcxParaFormat2TableStyle;
var
  AParagraph: PARAFORMAT2;
begin
  Result := pftsNone;
  GetAttributes(AParagraph);
  with AParagraph do
  begin
    if (wReserved and PFE_TABLEROWDELIMITER) <> 0 then
      Result := pftsRowDelimiter
    else
      if (wReserved and PFE_TABLECELL) <> 0 then
        Result := pftsCell;
  end;
end;

function TcxParaAttributes2.GetYSpace(const Index: Integer): Longint;
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  case Index of
    0: Result := TwipsToPixels(AParagraph.dySpaceBefore);
    1: Result := TwipsToPixels(AParagraph.dySpaceAfter);
  else
    Result := TwipsToPixels(AParagraph.dyLineSpacing);
  end;
end;

procedure TcxParaAttributes2.SetAlignment(Value: TcxParaFormat2Alignment);
var
  AParagraph: PARAFORMAT2;
begin
  InitParagraph(AParagraph);
  with AParagraph do
  begin
    dwMask := PFM_ALIGNMENT;
    wAlignment := Ord(Value) + 1;
  end;
  SetAttributes(AParagraph);
end;

procedure TcxParaAttributes2.SetIndent(const Index: Integer; const Value: Longint);

  function GetMask: DWORD;
  begin
    case Index of
      0: Result := PFM_STARTINDENT or PFM_OFFSET;
      1: Result := PFM_RIGHTINDENT;
      2: Result := PFM_OFFSET;
    else
      Result := PFM_OFFSETINDENT;
    end;
  end;

var
  AParagraph: PARAFORMAT2;
  ANewValue: Longint;
begin
  GetAttributes(AParagraph);
  with AParagraph do
  begin
    dwMask := GetMask;
    ANewValue := dxPixelsToTwips(Value);
    case Index of
      0:
        begin
          dxOffset := dxOffset - ANewValue + dxStartIndent;
          dxStartIndent := ANewValue;
        end;
      1: dxRightIndent := ANewValue;
      2: dxOffset := ANewValue - dxStartIndent;
    else
      dxStartIndent := ANewValue - dxStartIndent;
    end;
  end;
  SetAttributes(AParagraph);
end;

procedure TcxParaAttributes2.SetLineSpacingRule(Value: TcxParaFormat2LineSpacingRule);
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  with AParagraph do
  begin
    dwMask := PFM_LINESPACING;
    bLineSpacingRule := Ord(Value);
  end;
  SetAttributes(AParagraph);
end;

procedure TcxParaAttributes2.SetNumberingStart(Value: Word);
var
  AParagraph: PARAFORMAT2;
begin
  InitParagraph(AParagraph);
  with AParagraph do
  begin
    dwMask := dwMask or PFM_NUMBERINGSTART;
    wNumberingStart := Value;
  end;
  SetAttributes(AParagraph);
end;

procedure TcxParaAttributes2.SetNumberingStyle(Value: TcxParaFormat2NumberingStyle);
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  with AParagraph do
  begin
    dwMask := PFM_NUMBERINGSTYLE;
    wNumberingStyle := Ord(Value);
  end;
  SetAttributes(AParagraph);
end;

procedure TcxParaAttributes2.SetNumberingTab(Value: Word);
var
  AParagraph: PARAFORMAT2;
begin
  InitParagraph(AParagraph);
  with AParagraph do
  begin
    dwMask := dwMask or PFM_NUMBERINGTAB;
    wNumberingTab := dxPixelsToTwips(Value);
  end;
  SetAttributes(AParagraph);
end;

procedure TcxParaAttributes2.SetNumberingType(Value: TcxParaFormat2NumberingType);
var
  AParagraph: PARAFORMAT2;
begin
  InitParagraph(AParagraph);
  with AParagraph do
  begin
    dwMask := dwMask or PFM_NUMBERING;
    wNumbering := Ord(Value);
  end;
  SetAttributes(AParagraph);
end;

procedure TcxParaAttributes2.SetPageBreakBefore(Value: Boolean);
var
  AParagraph: PARAFORMAT2;
begin
  InitParagraph(AParagraph);
  with AParagraph do
  begin
    dwMask := dwMask or PFM_PAGEBREAKBEFORE;
    wReserved := PFE_PAGEBREAKBEFORE;
  end;
  SetAttributes(AParagraph);
end;

procedure TcxParaAttributes2.SetStyle(Value: Smallint);
var
  AParagraph: PARAFORMAT2;
begin
  InitParagraph(AParagraph);
  with AParagraph do
  begin
    dwMask := PFM_STYLE;
    sStyle := Value;
  end;
  SetAttributes(AParagraph);
end;

procedure TcxParaAttributes2.SetTab(Index: Byte; Value: Longint);
begin
  SetTabValues(Index, Value, TabAlignment[Index], TabLeader[Index]);
end;

procedure TcxParaAttributes2.SetTabAlignment(Index: Integer; const Value: TcxParaFormat2TabAlignment);
begin
  SetTabValues(Index, Tab[Index], Value, TabLeader[Index]);
end;

procedure TcxParaAttributes2.SetTabCount(Value: Integer);
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  with AParagraph do
  begin
    dwMask := PFM_TABSTOPS;
    cTabCount := Value;
  end;
  SetAttributes(AParagraph);
end;

procedure TcxParaAttributes2.SetTabLeader(Index: Integer; const Value: TcxParaFormat2TabLeader);
begin
  SetTabValues(Index, Tab[Index], TabAlignment[Index], Value);
end;

procedure TcxParaAttributes2.SetTabValues(Index, AWidth: Integer; AAlignment: TcxParaFormat2TabAlignment; ALeader: TcxParaFormat2TabLeader);
var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  with AParagraph do
  begin
    rgxTabs[Index] := dxPixelsToTwips(AWidth) or Ord(AAlignment) shl 24 or Ord(ALeader) shr 28;
    dwMask := PFM_TABSTOPS;
    if cTabCount < Index then cTabCount := Index;
  end;
  SetAttributes(AParagraph);
end;

procedure TcxParaAttributes2.SetYSpace(const Index: Integer;
  const Value: Longint);

  function GetMask: DWORD;
  begin
    case Index of
      0: Result := PFM_SPACEBEFORE;
      1: Result := PFM_SPACEAFTER;
    else
      Result := PFM_LINESPACING;
    end;
  end;

var
  AParagraph: PARAFORMAT2;
begin
  GetAttributes(AParagraph);
  with AParagraph do
  begin
    dwMask := GetMask;
    case Index of
      0: dySpaceBefore := dxPixelsToTwips(Value);
      1: dySpaceAfter := dxPixelsToTwips(Value);
    else
      dyLineSpacing := dxPixelsToTwips(Value);
    end;
  end;
  SetAttributes(AParagraph);
end;

{ TcxTextAttributes2 }

constructor TcxTextAttributes2.Create(ARichEdit: TcxCustomEdit; AType: TAttributeType);
begin
  inherited Create(ARichEdit);
  FType := AType;
end;

procedure TcxTextAttributes2.Assign(Source: TPersistent);
var
  ATextAttributes: TcxTextAttributes2;
begin
  if Source is TcxTextAttributes2 then
  begin
    ATextAttributes := TcxTextAttributes2(Source);
    BackgroundColor := ATextAttributes.BackgroundColor;
    Charset := ATextAttributes.Charset;
    Color := ATextAttributes.Color;
    Height := ATextAttributes.Height;
    Name := ATextAttributes.Name;
    Offset := ATextAttributes.Offset;
    Pitch := ATextAttributes.Pitch;
    Protected := ATextAttributes.Protected;
    Size := ATextAttributes.Size;
    Style := ATextAttributes.Style;
    StyleEx := ATextAttributes.StyleEx;
    UnderlineType := ATextAttributes.UnderlineType;
    Weight := ATextAttributes.Weight;
  end
  else
    inherited;
end;

procedure TcxTextAttributes2.GetAttributes(var AFormat: CHARFORMAT2);
begin
  InitFormat(AFormat);
  if InnerRich.HandleAllocated then
    SendMessage(InnerRich.Handle, EM_GETCHARFORMAT, GetFlag, LPARAM(@AFormat));
end;

procedure TcxTextAttributes2.InitFormat(var AFormat: CHARFORMAT2);
begin
  FillChar(AFormat, SizeOf(CHARFORMAT2), 0);
  AFormat.cbSize := SizeOf(CHARFORMAT2);
end;

procedure TcxTextAttributes2.SetAttributes(var AFormat: CHARFORMAT2);
begin
  InnerRich.HandleNeeded;
  if InnerRich.HandleAllocated then
    cxSendStructMessage(InnerRich.Handle, EM_SETCHARFORMAT, GetFlag, AFormat);
end;

function TcxTextAttributes2.GetFlag: WParam;
const
  AResultMap: array[Boolean] of WPARAM = (SCF_DEFAULT, SCF_SELECTION);
begin
  Result := AResultMap[FType = atSelected];
end;

function TcxTextAttributes2.GetEffects: DWORD;
var
  AFormat: CHARFORMAT2;
begin
  GetAttributes(AFormat);
  Result := AFormat.dwEffects;
end;

procedure TcxTextAttributes2.SetEffects(AMask, AEffects: DWORD);
var
  AFormat: CHARFORMAT2;
begin
  InitFormat(AFormat);
  AFormat.dwMask := AMask;
  AFormat.dwEffects := AEffects;
  SetAttributes(AFormat);
end;

function TcxTextAttributes2.GetCharset: TFontCharset;
var
  AFormat: CHARFORMAT2;
begin
  GetAttributes(AFormat);
  Result := AFormat.bCharSet;
end;

function TcxTextAttributes2.GetColor(Index: Integer): TColor;

  procedure GetColorParams(out ATextColor, ABackgrondColor: TColor);
  var
    AFormat: CHARFORMAT2;
  begin
    GetAttributes(AFormat);
    ATextColor := AFormat.crTextColor;
    ABackgrondColor := AFormat.crBackColor;
  end;

var
  AEffects: DWORD;
  ABackgrondColor: TColor;
  ATextColor: TColor;
begin
  AEffects := GetEffects;
  GetColorParams(ATextColor, ABackgrondColor);
  if (AEffects and cxAutoColorMaskMap[Index]) <> 0 then
    Result := cxAutoColorMap[Index]
  else
    if Index = 0 then
      Result := ABackgrondColor
    else
      Result := ATextColor;
end;

function TcxTextAttributes2.GetConsistentAttributes: TConsistentAttributes;

  function GetMask: DWORD;
  var
    AFormat: CHARFORMAT2;
  begin
    GetAttributes(AFormat);
    Result := AFormat.dwMask;
  end;

var
  AMask: DWORD;
begin
  Result := [];
  if InnerRich.HandleAllocated and (FType = atSelected) then
  begin
    AMask := GetMask;
    if (AMask and CFM_BOLD) <> 0 then Include(Result, caBold);
    if (AMask and CFM_COLOR) <> 0 then Include(Result, caColor);
    if (AMask and CFM_FACE) <> 0 then Include(Result, caFace);
    if (AMask and CFM_ITALIC) <> 0 then Include(Result, caItalic);
    if (AMask and CFM_SIZE) <> 0 then Include(Result, caSize);
    if (AMask and CFM_STRIKEOUT) <> 0 then Include(Result, caStrikeOut);
    if (AMask and CFM_UNDERLINE) <> 0 then Include(Result, caUnderline);
    if (AMask and CFM_PROTECTED) <> 0 then Include(Result, caProtected);
  end;
end;

function TcxTextAttributes2.GetHeight: Integer;
begin
  Result := MulDiv(Size, cxGetPixelsPerInch.cy, 72);
end;

function TcxTextAttributes2.GetName: TFontName;
var
  AFormat: CHARFORMAT2;
begin
  GetAttributes(AFormat);
  Result := AFormat.szFaceName;
end;

function TcxTextAttributes2.GetOffset: Integer;
var
  AFormat: CHARFORMAT2;
begin
  GetAttributes(AFormat);
  Result := TwipsToPixels(AFormat.yOffset);
end;

function TcxTextAttributes2.GetPitch: TFontPitch;

  function GetPitchAndFamily: Byte;
  var
    AFormat: CHARFORMAT2;
  begin
    GetAttributes(AFormat);
    Result := AFormat.bPitchAndFamily;
  end;

var
  APitchAndFamily: Byte;
begin
  APitchAndFamily := GetPitchAndFamily;
  case (APitchAndFamily and $03) of
    VARIABLE_PITCH: Result := fpVariable;
    FIXED_PITCH: Result := fpFixed;
  else
    Result := fpDefault;
  end;
end;

function TcxTextAttributes2.GetProtected: Boolean;
begin
  Result := GetEffects and CFE_PROTECTED <> 0;
end;

function TcxTextAttributes2.GetSize: Integer;
var
  AFormat: CHARFORMAT2;
begin
  GetAttributes(AFormat);
  Result := TwipsToPixels(AFormat.yHeight);
end;

function TcxTextAttributes2.GetStyle: TFontStyles;
var
  AEffects: DWORD;
begin
  Result := [];
  AEffects := GetEffects;
  if (AEffects and CFE_BOLD) <> 0 then Include(Result, fsBold);
  if (AEffects and CFE_ITALIC) <> 0 then Include(Result, fsItalic);
  if (AEffects and CFE_UNDERLINE) <> 0 then Include(Result, fsUnderline);
  if (AEffects and CFE_STRIKEOUT) <> 0 then Include(Result, fsStrikeOut);
end;

function TcxTextAttributes2.GetStyleEx: TcxCharFormat2FontStylesEx;
var
  AEffects: DWORD;
begin
  Result := [];
  AEffects := GetEffects;
  if (AEffects and CFE_ALLCAPS) <> 0 then Include(Result, cffsAllCaps);
  if (AEffects and CFE_DISABLED) <> 0 then Include(Result, cffsDisabled);
  if (AEffects and CFE_EMBOSS) <> 0 then Include(Result, cffsEmboss);
  if (AEffects and CFE_HIDDEN) <> 0 then Include(Result, cffsHidden);
  if (AEffects and CFE_IMPRINT) <> 0 then Include(Result, cffsImprint);
  if (AEffects and CFE_OUTLINE) <> 0 then Include(Result, cffsOutline);
  if (AEffects and CFE_REVISED) <> 0 then Include(Result, cffsRevised);
  if (AEffects and CFE_SHADOW) <> 0 then Include(Result, cffsShadow);
  if (AEffects and CFE_SMALLCAPS) <> 0 then Include(Result, cffsSmallCaps);
  if (AEffects and CFE_SUBSCRIPT) <> 0 then Include(Result, cffsSubscript);
  if (AEffects and CFE_SUPERSCRIPT) <> 0 then Include(Result, cffsSuperscript);
end;

function TcxTextAttributes2.GetUnderlineType: TcxCharFormat2UnderlineType;

  function InternalGetUnderlineType: Byte;
  var
    AFormat: CHARFORMAT2;
  begin
    GetAttributes(AFormat);
    Result := AFormat.bUnderlineType;
  end;

begin
  case InternalGetUnderlineType of
    CFU_UNDERLINEDOTTED: Result := cfutDotted;
    CFU_UNDERLINEDOUBLE: Result := cfutDouble;
    CFU_UNDERLINEWORD: Result := cfutWordOnly;
    CFU_UNDERLINE: Result := cfutSolid;
  else
    Result := cfutNone;
  end;
end;

function TcxTextAttributes2.GetWeight: TcxCharFormat2Weight;

  function GetWeightValue: Word;
  var
    AFormat: CHARFORMAT2;
  begin
    GetAttributes(AFormat);
    Result := AFormat.wWeight;
  end;

begin
  case GetWeightValue of
    FW_THIN: Result := cfwThin;
    FW_ULTRALIGHT: Result := cfwUltraLight;
    FW_LIGHT: Result := cfwLight;
    FW_NORMAL: Result := cfwNormal;
    FW_MEDIUM: Result := cfwMedium;
    FW_SEMIBOLD: Result := cfwSemiBold;
    FW_BOLD: Result := cfwBold;
    FW_ULTRABOLD: Result := cfwUltraBold;
    FW_HEAVY: Result := cfwHeavy;
  else
    Result := cfwDontCare;
  end;
end;

procedure TcxTextAttributes2.SetCharset(const Value: TFontCharset);
var
  AFormat: CHARFORMAT2;
begin
  InitFormat(AFormat);
  AFormat.dwMask := CFM_CHARSET;
  AFormat.bCharSet := Value;
  SetAttributes(AFormat);
end;

procedure TcxTextAttributes2.SetColor(Index: Integer; const Value: TColor);
const
  AMaskMap: array[0..1] of DWORD = (CFM_BACKCOLOR, CFM_COLOR);
var
  AFormat: CHARFORMAT2;
begin
  InitFormat(AFormat);
  with AFormat do
  begin
    dwMask := AMaskMap[Index];
    if Value = cxAutoColorMap[Index] then
      dwEffects := cxAutoColorMaskMap[Index]
    else
      if Index = 0 then
        crBackColor := Value
      else
        crTextColor := Value;
  end;
  SetAttributes(AFormat);
end;

procedure TcxTextAttributes2.SetHeight(Value: Integer);
begin
  Size := MulDiv(Value, 72, cxGetPixelsPerInch.cy);
end;

procedure cxWideStrPCopy(ADest: PWideChar; const ASource: PWideChar);
var
  ALen: Cardinal;
begin
  ALen := Length(ASource);
  Move(ASource^, ADest^, ALen * SizeOf(WideChar));
  ADest[ALen] := #0;
end;

procedure TcxTextAttributes2.SetName(const Value: TFontName);
var
  AFormat: CHARFORMAT2;
begin
  GetAttributes(AFormat);
  StrPCopy(AFormat.szFaceName, Value);
  SetAttributes(AFormat);
end;

procedure TcxTextAttributes2.SetOffset(Value: Integer);
var
  AFormat: CHARFORMAT2;
begin
  InitFormat(AFormat);
  AFormat.dwMask := CFM_OFFSET;
  AFormat.yOffset := dxPixelsToTwips(Value);
  SetAttributes(AFormat);
end;

procedure TcxTextAttributes2.SetPitch(const Value: TFontPitch);
const
  APitchMap: array[TFontPitch] of Byte = (DEFAULT_PITCH, VARIABLE_PITCH, FIXED_PITCH);
var
  AFormat: CHARFORMAT2;
begin
  InitFormat(AFormat);
  AFormat.bPitchAndFamily := APitchMap[Value];
  SetAttributes(AFormat);
end;

procedure TcxTextAttributes2.SetProtected(Value: Boolean);
const
  AProtectedMap: array[Boolean] of DWORD = (0, CFE_PROTECTED);
begin
  SetEffects(CFM_PROTECTED, AProtectedMap[Value]);
end;

procedure TcxTextAttributes2.SetSize(Value: Integer);
var
  AFormat: CHARFORMAT2;
begin
  InitFormat(AFormat);
  AFormat.dwMask := CFM_SIZE;
  AFormat.yHeight := dxPixelsToTwips(Value);
  SetAttributes(AFormat);
end;

procedure TcxTextAttributes2.SetStyle(const Value: TFontStyles);

  function InternalGetEffects: DWORD;
  begin
    Result := 0;
    if fsBold in Value then Result := Result or CFE_BOLD;
    if fsItalic in Value then Result := Result or CFE_ITALIC;
    if fsUnderline in Value then Result := Result or CFE_UNDERLINE;
    if fsStrikeOut in Value then Result := Result or CFE_STRIKEOUT;
  end;

begin
  SetEffects(CFM_BOLD or CFM_ITALIC or CFM_UNDERLINE or CFM_STRIKEOUT,
    InternalGetEffects);
end;

procedure TcxTextAttributes2.SetStyleEx(const Value: TcxCharFormat2FontStylesEx);

  function InternalGetEffects: DWORD;
  begin
    Result := 0;
    if cffsAllCaps in Value then Result := Result or CFE_ALLCAPS;
    if cffsDisabled in Value then Result := Result or CFE_DISABLED;
    if cffsEmboss in Value then Result := Result or CFE_EMBOSS;
    if cffsHidden in Value then Result := Result or CFE_HIDDEN;
    if cffsImprint in Value then Result := Result or CFE_IMPRINT;
    if cffsOutline in Value then Result := Result or CFE_OUTLINE;
    if cffsRevised in Value then Result := Result or CFE_REVISED;
    if cffsShadow in Value then Result := Result or CFE_SHADOW;
    if cffsSmallCaps in Value then Result := Result or CFE_SMALLCAPS;
    if cffsSubscript in Value then Result := Result or CFE_SUBSCRIPT;
    if cffsSuperscript in Value then Result := Result or CFE_SUPERSCRIPT;
  end;

var
  AMask: DWORD;
begin
  AMask := CFM_ALLCAPS or CFM_DISABLED or CFM_EMBOSS or CFM_HIDDEN or CFM_IMPRINT or CFM_OUTLINE or
    CFM_REVISED or CFM_SHADOW or CFM_SMALLCAPS or CFM_SUBSCRIPT or CFM_SUPERSCRIPT;
  SetEffects(AMask, InternalGetEffects);
end;

procedure TcxTextAttributes2.SetUnderlineType(const Value: TcxCharFormat2UnderlineType);
const
  AUnderlineTypeMap: array[TcxCharFormat2UnderlineType] of Byte = (CFU_UNDERLINENONE, CFU_UNDERLINE,
    CFU_UNDERLINEDOTTED, CFU_UNDERLINEDOUBLE, CFU_UNDERLINEWORD);
var
  AFormat: CHARFORMAT2;
begin
  InitFormat(AFormat);
  AFormat.dwMask := CFM_UNDERLINETYPE;
  AFormat.bUnderlineType := AUnderlineTypeMap[Value];
  SetAttributes(AFormat);
end;

procedure TcxTextAttributes2.SetWeight(const Value: TcxCharFormat2Weight);
const
  AWeightMap: array[TcxCharFormat2Weight] of Word = (FW_DONTCARE,
    FW_THIN, FW_ULTRALIGHT, FW_LIGHT, FW_NORMAL, FW_MEDIUM, FW_SEMIBOLD,
    FW_BOLD, FW_ULTRABOLD, FW_HEAVY);

var
  AFormat: CHARFORMAT2;
begin
  GetAttributes(AFormat);
  AFormat.dwMask := AFormat.dwMask or CFM_BOLD or CFM_WEIGHT;
  if Value = cfwDontCare then
    AFormat.dwEffects := AFormat.dwEffects and not CFE_BOLD
  else
    AFormat.dwEffects := AFormat.dwEffects or CFE_BOLD;
  AFormat.wWeight := AWeightMap[Value];
  SetAttributes(AFormat);
end;

{ TcxRichEditTableParams }

destructor TcxRichEditTableParams.Destroy;
begin
  SetLength(FColumnParams, 0);
  inherited Destroy;
end;

function TcxRichEditTableParams.AddColumnParams: Integer;
begin
  Result := ColumnParamsCount;
  SetLength(FColumnParams, ColumnParamsCount + 1);
  ColumnParams[Result] := GetDefaultColumnParams;
end;

function TcxRichEditTableParams.AddColumnParams(const AParams: TcxRichEditTableColumnParams): Integer;
begin
  Result := AddColumnParams;
  ColumnParams[Result] := AParams;
end;

procedure TcxRichEditTableParams.ClearColumnParams;
begin
  SetLength(FColumnParams, 0);
end;

procedure TcxRichEditTableParams.InsertTable(ARichEdit: TcxCustomEdit; AColumnCount, ARowCount: Integer);

  procedure InitializeCellParams(var ACell: TcxRichEditTableCellParams; AParams: TcxRichEditTableColumnParams);
  const
    AVerticalAlignmentMap: array[TcxEditVertAlignment] of Word = (0, 2, 1);
  begin
    FillChar(ACell, SizeOf(TcxRichEditTableCellParams), 0);
    ACell.nParams := AVerticalAlignmentMap[AParams.VerticalAlignment];
    ACell.dxWidth := dxPixelsToTwips(AParams.Width);
    ACell.dxBrdrLeft := dxPixelsToTwips(AParams.BorderWidth.Left);
    ACell.dyBrdrTop := dxPixelsToTwips(AParams.BorderWidth.Top);
    ACell.dxBrdrRight := dxPixelsToTwips(AParams.BorderWidth.Right);
    ACell.dyBrdrBottom := dxPixelsToTwips(AParams.BorderWidth.Bottom);
    ACell.crBrdrLeft := cxRichEditColorToColor(AParams.BorderColor.Left);
    ACell.crBrdrTop := cxRichEditColorToColor(AParams.BorderColor.Top);
    ACell.crBrdrRight := cxRichEditColorToColor(AParams.BorderColor.Right);
    ACell.crBrdrBottom := cxRichEditColorToColor(AParams.BorderColor.Bottom);
    ACell.crBackPat := cxRichEditColorToColor(AParams.BackgroundColor);
    ACell.crForePat := cxRichEditColorToColor(AParams.ForegroundColor);
  end;

var
  ACells: array of TcxRichEditTableCellParams;
  ARows: Pointer;
  ACount: Integer;
  AFirstColumnParamsForAllCells: Boolean;
  I: Integer;
  ASize: Integer;
begin
  if not ARichEdit.InnerControl.HandleAllocated then
    Exit;

  if IsWinSeven then
    ASize := SizeOf(TcxRichEditTableRowWinSevenParams)
  else
    ASize := SizeOf(TcxRichEditTableRowParams);
  ARows := AllocMem(ASize);
  try
    PcxRichEditTableRowParams(ARows).cbSize := ASize;
    PcxRichEditTableRowParams(ARows).cbCellSize := SizeOf(TcxRichEditTableCellParams);
    PcxRichEditTableRowParams(ARows).cCell := AColumnCount;
    PcxRichEditTableRowParams(ARows).cRow := ARowCount;
    PcxRichEditTableRowParams(ARows).dxCellMargin := dxPixelsToTwips(CellMargins);
    PcxRichEditTableRowParams(ARows).dyHeight := dxPixelsToTwips(RowHeight);
    PcxRichEditTableRowParams(ARows).dxIndent := dxPixelsToTwips(RowIndent);
    PcxRichEditTableRowParams(ARows).nParams := Ord(Alignment) + 1;

    AFirstColumnParamsForAllCells := AColumnCount > ColumnParamsCount;
    if AFirstColumnParamsForAllCells then
      PcxRichEditTableRowParams(ARows).nParams := PcxRichEditTableRowParams(ARows).nParams or $80;

    ACount := IfThen(AFirstColumnParamsForAllCells, 1, AColumnCount);

    SetLength(ACells, ACount);
    try
      if (ACount = 1) and (ColumnParamsCount = 0) then
        InitializeCellParams(ACells[0], GetDefaultColumnParams)
      else
        for I := 0 to ACount - 1 do
          InitializeCellParams(ACells[I], ColumnParams[I]);
      SendMessage(ARichEdit.InnerControl.Handle, EM_INSERTTABLE, WPARAM(ARows), LPARAM(@ACells[0]));
    finally
      SetLength(ACells, 0);
    end;
  finally
    FreeMem(ARows);
  end;
end;

function TcxRichEditTableParams.GetDefaultColumnParams: TcxRichEditTableColumnParams;
begin
  cxZeroMemory(@Result, SizeOf(Result));
  Result.BackgroundColor := recWhite;
  Result.BorderWidth := cxSimpleRect;
  Result.Width := 100;
end;

function TcxRichEditTableParams.GetColumnParamsCount: Integer;
begin
  Result := Length(FColumnParams);
end;

end.
