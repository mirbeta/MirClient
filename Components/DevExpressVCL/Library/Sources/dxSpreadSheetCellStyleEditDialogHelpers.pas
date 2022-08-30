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

unit dxSpreadSheetCellStyleEditDialogHelpers;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, Generics.Defaults, Generics.Collections, cxDropDownEdit, cxCheckListBox, cxListBox,
  cxTextEdit, cxLookAndFeelPainters, cxControls, cxGraphics, cxImageComboBox, cxColorComboBox, dxCore, dxCoreClasses,
  cxGeometry, dxSpreadSheetGraphics, dxSpreadSheetUtils, dxSpreadSheetTypes, dxSpreadSheetNumberFormat,
  dxSpreadSheetClasses, dxSpreadSheetCoreStyles, dxSpreadSheetStyles, dxSpreadSheetCellStyleEditDialogController;

type

  { TdxSpreadSheetCellStyleEditDialogBorderInfo }

  TdxSpreadSheetCellStyleEditDialogBorder = (fcdbLeft, fcdbTop, fcdbRight, fcdbBottom, fcdbInsideVertical, fcdbInsideHorizontal);
  TdxSpreadSheetCellStyleEditDialogBorders = set of TdxSpreadSheetCellStyleEditDialogBorder;
  TdxSpreadSheetCellStyleEditDialogBordersPreview = class;

  TdxSpreadSheetCellStyleEditDialogBorderInfo = class
  strict private
    FAssigned: Boolean;
    FColor: TColor;
    FOwner: TdxSpreadSheetCellStyleEditDialogBordersPreview;
    FStyle: TdxSpreadSheetCellBorderStyle;
    FUndefined: Boolean;
    FVisible: Boolean;

    function GetEmpty: Boolean;
    procedure SetColor(const AValue: TColor);
    procedure SetStyle(const AValue: TdxSpreadSheetCellBorderStyle);
    procedure SetUndefined(const AValue: Boolean);
    procedure SetVisible(const AValue: Boolean);
  protected
    Bounds: TRect;
    procedure Changed;
    function HitTestArea: TRect; inline;
    function Size: Integer; inline;
  public
    constructor Create(AOwner: TdxSpreadSheetCellStyleEditDialogBordersPreview);
    //
    property Assigned: Boolean read FAssigned write FAssigned;
    property Color: TColor read FColor write SetColor;
    property Empty: Boolean read GetEmpty;
    property Style: TdxSpreadSheetCellBorderStyle read FStyle write SetStyle;
    property Undefined: Boolean read FUndefined write SetUndefined;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  { TdxSpreadSheetCellStyleEditDialogBordersPreview }

  TdxSpreadSheetCellStyleEditDialogBordersPreview = class
  strict private
    FBordersInfo: array[TdxSpreadSheetCellStyleEditDialogBorder] of TdxSpreadSheetCellStyleEditDialogBorderInfo;
    FBounds: TRect;
    FPreviewText: string;
    FTextBounds: TdxRectList;

    FOnChanged: TNotifyEvent;

    function GetBackgroundColor: TColor;
    function GetBorderInfo(AIndex: TdxSpreadSheetCellStyleEditDialogBorder): TdxSpreadSheetCellStyleEditDialogBorderInfo;
    function GetHasHorzInsideBorder: Boolean;
    function GetHasVertInsideBorder: Boolean;
    function GetPainter: TcxCustomLookAndFeelPainter;
    procedure SetBounds(const AValue: TRect);
    procedure SetHasHorzInsideBorder(const AValue: Boolean);
    procedure SetHasVertInsideBorder(const AValue: Boolean);
  protected
    procedure CalculateTextBounds(const AContentBounds: TRect);
    procedure Changed;
    procedure DrawBorder(ACanvas: TcxCanvas; ABorder: TdxSpreadSheetCellStyleEditDialogBorder;
      AInfo: TdxSpreadSheetCellStyleEditDialogBorderInfo);
    procedure DrawPreviewText(ACanvas: TcxCanvas; R: TRect);
    //
    property BackgroundColor: TColor read GetBackgroundColor;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property TextBounds: TdxRectList read FTextBounds;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Calculate;
    procedure Draw(ACanvas: TcxCanvas);
    function HitTest(const P: TPoint; out ABorder: TdxSpreadSheetCellStyleEditDialogBorder): Boolean;
    //
    property BordersInfo[AIndex: TdxSpreadSheetCellStyleEditDialogBorder]: TdxSpreadSheetCellStyleEditDialogBorderInfo read GetBorderInfo;
    property Bounds: TRect read FBounds write SetBounds;
    property HasHorzInsideBorder: Boolean read GetHasHorzInsideBorder write SetHasHorzInsideBorder;
    property HasVertInsideBorder: Boolean read GetHasVertInsideBorder write SetHasVertInsideBorder;
    property PreviewText: string read FPreviewText write FPreviewText;
    //
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  { TdxSpreadSheetCellStyleEditDialogColorComboBoxCache }

  TdxSpreadSheetCellStyleEditDialogColorComboBoxCache = class(TDictionary<TcxColorComboBox, TColor>)
  public
    function GetValue(AColorComboBox: TcxColorComboBox): TColor;
  end;

  { TdxSpreadSheetCellStyleEditDialogListBoxIndexCache }

  TdxSpreadSheetCellStyleEditDialogListBoxIndexCache = class(TDictionary<TcxListBox, Integer>)
  public
    function GetValue(AListBox: TcxListBox): Integer;
  end;

  { TdxSpreadSheetCellStyleEditDialogHelper }

  TdxSpreadSheetCellStyleEditDialogHelper = class
  strict private const
    FontStyles: array[0..3] of TFontStyles = ([], [fsItalic], [fsBold], [fsBold, fsItalic]);
  strict private
    FColorComboCache: TdxSpreadSheetCellStyleEditDialogColorComboBoxCache;
    FController: TdxSpreadSheetCellStyleEditDialogCustomController;
    FListBoxCache: TdxSpreadSheetCellStyleEditDialogListBoxIndexCache;

    function GetPredefinedFormats: TdxSpreadSheetPredefinedFormats;
  public
    constructor Create(AController: TdxSpreadSheetCellStyleEditDialogCustomController);
    destructor Destroy; override;
    procedure ClearCache;

    procedure DrawBorderStylePattern(AControl: TcxListBox; ACanvas: TcxCanvas;
      ABorderColor: TColor; R: TRect; AStyle: TdxSpreadSheetCellBorderStyle);

    function DisplayFormatCodeToFormatCode(AListBox: TcxListBox): string; overload;
    function DisplayFormatCodeToFormatCode(const AFormatCode: string; AFormatCodeId: Integer = -1): string; overload;
    function FormatCodeToDisplayFormatCode(const ADataFormat: TdxSpreadSheetCellDataFormat): string; overload;
    function FormatCodeToDisplayFormatCode(const AHandle: TdxSpreadSheetFormatHandle): string; overload;

    function GetActualBorderType(ASide: TcxBorder; ARow, AColumn: Integer; const AArea: TRect): TdxSpreadSheetCellStyleEditDialogBorder;
    function GetActualColor(AColorComboBox: TcxColorComboBox): TColor;
    function GetFontStyle(AListBoxIndex: Integer): TFontStyles;
    function GetFontStyleName(AStyle: TFontStyles): string;
    function GetListBoxValue(AListBox: TcxListBox): string;

    procedure NavigateOnListbox(AKey: Word; AListBox: TcxListBox);
    procedure SyncSearchEdit(AListBox: TcxListBox; AEdit: TcxCustomTextEdit);

    procedure PopulateFillPatterns(AImageList: TcxImageList; AComboBox: TcxImageComboBox);
    procedure PopulateFontSizes(AListBox: TcxListBox);
    procedure PopulateFontStyles(AListBox: TcxListBox);
    procedure PopulateFontUnderlineStyles(AComboBox: TcxComboBox);
    procedure PopulateLocale(AComboBox: TcxComboBox);
    procedure PopulateNumberFormatCategories(AListBox: TcxListBox);
    procedure PopulateNumberFormatTemplates(AListBox: TcxListBox; ACategory: TdxSpreadSheetNumberFormatCategory);
    procedure PopulateTextAlignHorz(AComboBox: TcxComboBox);
    procedure PopulateTextAlignVert(AComboBox: TcxComboBox);
    //
    property ColorComboCache: TdxSpreadSheetCellStyleEditDialogColorComboBoxCache read FColorComboCache;
    property ListBoxCache: TdxSpreadSheetCellStyleEditDialogListBoxIndexCache read FListBoxCache;
    property PredefinedFormats: TdxSpreadSheetPredefinedFormats read GetPredefinedFormats;
  end;

  { TdxSpreadSheetCellStyleEditDialogNumberFormatHelper }

  TdxSpreadSheetCellStyleEditDialogNumberFormatHelper = class
  strict private
    class function CheckCode(const ACode, ARegExpr: string): Boolean;
  public
    class function BuildCodeForCurrency(ADecimalPlaces: Integer; const ASymbol: string; ALocale: Integer): string;
    class function BuildCodeForNumber(ADecimalPlaces: Integer; AUseThousandSeparator: Boolean): string;
    class function BuildCodeForPercentage(ADecimalPlaces: Integer): string;
    class function BuildCodeForScientific(ADecimalPlaces: Integer): string;

    class function IsCurrencyCode(ACode: string;
      out ADecimalPlaces: Integer; out ASymbol: string; out ALocale: Integer): Boolean;
    class function IsGeneralCode(const ACode: string): Boolean;
    class function IsNumberCode(const ACode: string;
      out ADecimalPlaces: Integer; out AUseThousandSeparator: Boolean): Boolean;
    class function IsPercentageCode(const ACode: string; out ADecimalPlaces: Integer): Boolean;
    class function IsScientificCode(const ACode: string; out ADecimalPlaces: Integer): Boolean;
  end;

implementation

uses
  Math, SysUtils, dxHashUtils, cxClasses, dxSpreadSheetCoreStrs, dxSpreadSheetCoreDialogsStrs, dxTypeHelpers,
  cxDrawTextUtils, cxLookAndFeels, cxVariants, Variants, StrUtils, cxFormats, cxRegExpr;

type
  TcxListBoxAccess = class(TcxListBox);
  TdxSpreadSheetCellDataFormatAccess = class(TdxSpreadSheetCellDataFormat);

{ TdxSpreadSheetCellStyleEditDialogBorderInfo }

constructor TdxSpreadSheetCellStyleEditDialogBorderInfo.Create(AOwner: TdxSpreadSheetCellStyleEditDialogBordersPreview);
begin
  inherited Create;
  FOwner := AOwner;
  FColor := clDefault;
  FVisible := True;
end;

procedure TdxSpreadSheetCellStyleEditDialogBorderInfo.Changed;
begin
  FOwner.Changed;
end;

function TdxSpreadSheetCellStyleEditDialogBorderInfo.HitTestArea: TRect;
begin
  Result := cxRectInflate(Bounds, 8);
end;

function TdxSpreadSheetCellStyleEditDialogBorderInfo.Size: Integer;
begin
  if Undefined then
    Result := 3
  else
    Result := dxSpreadSheetBorderStyleThickness[Style];
end;

function TdxSpreadSheetCellStyleEditDialogBorderInfo.GetEmpty: Boolean;
begin
  Result := (Color = clNone) or (Style in [sscbsDefault, sscbsNone]);
end;

procedure TdxSpreadSheetCellStyleEditDialogBorderInfo.SetColor(const AValue: TColor);
begin
  if Color <> AValue then
  begin
    FColor := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogBorderInfo.SetStyle(const AValue: TdxSpreadSheetCellBorderStyle);
begin
  if Style <> AValue then
  begin
    FStyle := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogBorderInfo.SetUndefined(const AValue: Boolean);
begin
  if Undefined <> AValue then
  begin
    FUndefined := AValue;
    Changed;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogBorderInfo.SetVisible(const AValue: Boolean);
begin
  if Visible <> AValue then
  begin
    FVisible := AValue;
    Changed;
  end;
end;

{ TdxSpreadSheetCellStyleEditDialogBordersPreview }

constructor TdxSpreadSheetCellStyleEditDialogBordersPreview.Create;
var
  AIndex: TdxSpreadSheetCellStyleEditDialogBorder;
begin
  inherited Create;
  FTextBounds := TdxRectList.Create;
  for AIndex := Low(AIndex) to High(AIndex) do
    FBordersInfo[AIndex] := TdxSpreadSheetCellStyleEditDialogBorderInfo.Create(Self);
end;

destructor TdxSpreadSheetCellStyleEditDialogBordersPreview.Destroy;
var
  AIndex: TdxSpreadSheetCellStyleEditDialogBorder;
begin
  for AIndex := Low(AIndex) to High(AIndex) do
    FreeAndNil(FBordersInfo[AIndex]);
  FreeAndNil(FTextBounds);
  inherited Destroy;
end;

procedure TdxSpreadSheetCellStyleEditDialogBordersPreview.Calculate;
var
  R: TRect;
begin
  R := cxRectInflate(Bounds, -9);
  BordersInfo[fcdbBottom].Bounds := cxRectSetBottom(R, R.Bottom, BordersInfo[fcdbBottom].Size);
  BordersInfo[fcdbLeft].Bounds := cxRectSetWidth(R, BordersInfo[fcdbLeft].Size);
  BordersInfo[fcdbRight].Bounds := cxRectSetRight(R, R.Right, BordersInfo[fcdbRight].Size);
  BordersInfo[fcdbTop].Bounds := cxRectSetHeight(R, BordersInfo[fcdbTop].Size);
  BordersInfo[fcdbInsideHorizontal].Bounds := cxRectCenterVertically(R, BordersInfo[fcdbInsideHorizontal].Size);
  BordersInfo[fcdbInsideVertical].Bounds := cxRectCenterHorizontally(R, BordersInfo[fcdbInsideVertical].Size);
  CalculateTextBounds(R);
end;

procedure TdxSpreadSheetCellStyleEditDialogBordersPreview.Draw(ACanvas: TcxCanvas);
var
  AIndex: TdxSpreadSheetCellStyleEditDialogBorder;
  I: Integer;
begin
  ACanvas.FillRect(Bounds, BackgroundColor);
  ACanvas.FrameRect(Bounds, cxGetActualColor(Painter.GetContainerBorderColor(False), clWindowFrame));
  for AIndex := Low(AIndex) to High(AIndex) do
    DrawBorder(ACanvas, AIndex, BordersInfo[AIndex]);
  for I := 0 to TextBounds.Count - 1 do
    DrawPreviewText(ACanvas, TextBounds[I]);
end;

function TdxSpreadSheetCellStyleEditDialogBordersPreview.HitTest(
  const P: TPoint; out ABorder: TdxSpreadSheetCellStyleEditDialogBorder): Boolean;
var
  AIndex: TdxSpreadSheetCellStyleEditDialogBorder;
begin
  Result := False;
  for AIndex := Low(AIndex) to High(AIndex) do
    if BordersInfo[AIndex].Visible and PtInRect(BordersInfo[AIndex].HitTestArea, P) then
    begin
      ABorder := AIndex;
      Result := True;
      Break;
    end;
end;

procedure TdxSpreadSheetCellStyleEditDialogBordersPreview.CalculateTextBounds(const AContentBounds: TRect);
var
  R1, R2: TRect;
begin
  FTextBounds.Clear;
  R2 := AContentBounds;
  R2.Bottom := R2.Top + Round(R2.Height / (1 + Ord(HasHorzInsideBorder)));
  R2.Right := R2.Left + Round(R2.Width / (1 + Ord(HasVertInsideBorder)));
  while R2.Top < AContentBounds.Bottom do
  begin
    R1 := R2;
    while R1.Left < AContentBounds.Right do
    begin
      FTextBounds.Add(R1);
      OffsetRect(R1, R1.Width, 0);
    end;
    OffsetRect(R2, 0, R2.Height);
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogBordersPreview.Changed;
begin
  Calculate;
  CallNotify(OnChanged, Self);
end;

procedure TdxSpreadSheetCellStyleEditDialogBordersPreview.DrawBorder(ACanvas: TcxCanvas;
  ABorder: TdxSpreadSheetCellStyleEditDialogBorder; AInfo: TdxSpreadSheetCellStyleEditDialogBorderInfo);
begin
  if AInfo.Visible then
  begin
    if AInfo.Undefined then
      cxFillHalfToneRect(ACanvas.Canvas, AInfo.Bounds, BackgroundColor,
        cxGetActualColor(Painter.DefaultEditorTextColor(True), clGray))
    else
      if AInfo.Style <> sscbsDefault then
        dxSpreadSheetDrawBorder(ACanvas, AInfo.Bounds, cxGetActualColor(AInfo.Color, clBlack),
          BackgroundColor, AInfo.Style, ABorder in [fcdbTop, fcdbBottom, fcdbInsideHorizontal]);
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogBordersPreview.DrawPreviewText(ACanvas: TcxCanvas; R: TRect);
begin
  if ACanvas.RectVisible(R) then
    cxTextOut(ACanvas.Handle, PreviewText, R, CXTO_CENTER_HORIZONTALLY or CXTO_CENTER_VERTICALLY,
      nil, 0, 0, 0, Painter.DefaultContentTextColor);
end;

function TdxSpreadSheetCellStyleEditDialogBordersPreview.GetBackgroundColor: TColor;
begin
  Result := cxGetActualColor(Painter.DefaultEditorBackgroundColor(False), clWindow);
end;

function TdxSpreadSheetCellStyleEditDialogBordersPreview.GetBorderInfo(
  AIndex: TdxSpreadSheetCellStyleEditDialogBorder): TdxSpreadSheetCellStyleEditDialogBorderInfo;
begin
  Result := FBordersInfo[AIndex];
end;

function TdxSpreadSheetCellStyleEditDialogBordersPreview.GetHasHorzInsideBorder: Boolean;
begin
  Result := BordersInfo[fcdbInsideHorizontal].Visible;
end;

function TdxSpreadSheetCellStyleEditDialogBordersPreview.GetHasVertInsideBorder: Boolean;
begin
  Result := BordersInfo[fcdbInsideVertical].Visible;
end;

function TdxSpreadSheetCellStyleEditDialogBordersPreview.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := RootLookAndFeel.Painter;
end;

procedure TdxSpreadSheetCellStyleEditDialogBordersPreview.SetBounds(const AValue: TRect);
begin
  if not cxRectIsEqual(Bounds, AValue) then
  begin
    FBounds := AValue;
    Calculate;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogBordersPreview.SetHasHorzInsideBorder(const AValue: Boolean);
begin
  BordersInfo[fcdbInsideHorizontal].Visible := AValue;
end;

procedure TdxSpreadSheetCellStyleEditDialogBordersPreview.SetHasVertInsideBorder(const AValue: Boolean);
begin
  BordersInfo[fcdbInsideVertical].Visible := AValue;
end;

{ TdxSpreadSheetCellStyleEditDialogColorComboBoxCache }

function TdxSpreadSheetCellStyleEditDialogColorComboBoxCache.GetValue(AColorComboBox: TcxColorComboBox): TColor;
begin
  if not TryGetValue(AColorComboBox, Result) then
  begin
    Result := AColorComboBox.ColorValue;
    Add(AColorComboBox, Result);
  end;
end;

{ TdxSpreadSheetCellStyleEditDialogListBoxIndexCache }

function TdxSpreadSheetCellStyleEditDialogListBoxIndexCache.GetValue(AListBox: TcxListBox): Integer;
begin
  if not TryGetValue(AListBox, Result) then
  begin
    Result := AListBox.ItemIndex;
    Add(AListBox, Result);
  end;
end;

{ TdxSpreadSheetCellStyleEditDialogHelper }

constructor TdxSpreadSheetCellStyleEditDialogHelper.Create(
  AController: TdxSpreadSheetCellStyleEditDialogCustomController);
begin
  inherited Create;
  FController := AController;
  FColorComboCache := TdxSpreadSheetCellStyleEditDialogColorComboBoxCache.Create;
  FListBoxCache := TdxSpreadSheetCellStyleEditDialogListBoxIndexCache.Create;
end;

destructor TdxSpreadSheetCellStyleEditDialogHelper.Destroy;
begin
  FreeAndNil(FColorComboCache);
  FreeAndNil(FListBoxCache);
  inherited Destroy;
end;

procedure TdxSpreadSheetCellStyleEditDialogHelper.ClearCache;
begin
  FColorComboCache.Clear;
  FListBoxCache.Clear;
end;

procedure TdxSpreadSheetCellStyleEditDialogHelper.DrawBorderStylePattern(
  AControl: TcxListBox; ACanvas: TcxCanvas; ABorderColor: TColor; R: TRect; AStyle: TdxSpreadSheetCellBorderStyle);
var
  ABackgroundColor: TColor;
begin
  ABackgroundColor := TcxListBoxAccess(AControl).ViewInfo.BackgroundColor;
  ACanvas.FillRect(R, ABackgroundColor);
  ACanvas.Font := AControl.Style.Font;
  if AStyle <> sscbsDefault then
  begin
    R := cxRectCenterVertically(R, dxSpreadSheetBorderStyleThickness[AStyle]);
    R := cxRectInflate(R, -4, 0);
    dxSpreadSheetDrawBorder(ACanvas, R, ABorderColor, ABackgroundColor, AStyle, True);
  end
  else
    cxTextOut(ACanvas.Handle, cxGetResourceString(@sdxFormatCellsDialogNone), R, CXTO_CENTER_HORIZONTALLY or CXTO_CENTER_VERTICALLY);
end;

function TdxSpreadSheetCellStyleEditDialogHelper.DisplayFormatCodeToFormatCode(AListBox: TcxListBox): string;
begin
  if AListBox.ItemIndex >= 0 then
    Result := DisplayFormatCodeToFormatCode(AListBox.Items[AListBox.ItemIndex], Integer(AListBox.ItemObject))
  else
    Result := '';
end;

function TdxSpreadSheetCellStyleEditDialogHelper.DisplayFormatCodeToFormatCode(
  const AFormatCode: string; AFormatCodeId: Integer = -1): string;
var
  AHandle: TdxSpreadSheetFormatHandle;
begin
  Result := AFormatCode;
  if (Result <> '') and (AFormatCodeId >= 0) then
  begin
    AHandle := PredefinedFormats.GetFormatHandleByID(AFormatCodeId);
    if (AHandle <> nil) and AHandle.IsDependedFromRegionalSettings and (Result[1] = '*') then
      Delete(Result, 1, 1);
  end;
end;

function TdxSpreadSheetCellStyleEditDialogHelper.FormatCodeToDisplayFormatCode(
  const ADataFormat: TdxSpreadSheetCellDataFormat): string;
begin
  Result := FormatCodeToDisplayFormatCode(TdxSpreadSheetCellDataFormatAccess(ADataFormat).Handle);
end;

function TdxSpreadSheetCellStyleEditDialogHelper.FormatCodeToDisplayFormatCode(
  const AHandle: TdxSpreadSheetFormatHandle): string;
begin
  if AHandle.IsDependedFromRegionalSettings then
    Result := '*' + AHandle.FormatCode
  else
    Result := AHandle.FormatCode;
end;

function TdxSpreadSheetCellStyleEditDialogHelper.GetActualBorderType(
  ASide: TcxBorder; ARow, AColumn: Integer; const AArea: TRect): TdxSpreadSheetCellStyleEditDialogBorder;
begin
  case ASide of
    bTop:
      if AArea.Top = ARow then
        Result := fcdbTop
      else
        Result := fcdbInsideHorizontal;

    bRight:
      if AArea.Right = AColumn then
        Result := fcdbRight
      else
        Result := fcdbInsideVertical;

    bBottom:
      if AArea.Bottom = ARow then
        Result := fcdbBottom
      else
        Result := fcdbInsideHorizontal;

  else
    if AArea.Left = AColumn then
      Result := fcdbLeft
    else
      Result := fcdbInsideVertical;
  end;
end;

function TdxSpreadSheetCellStyleEditDialogHelper.GetActualColor(AColorComboBox: TcxColorComboBox): TColor;
begin
  if VarIsNull(AColorComboBox.EditingValue) then
    Result := clDefault
  else
    Result := AColorComboBox.ColorValue;

  if not cxColorIsValid(Result) then
    Result := clDefault;
  Result := cxGetActualColor(Result, clWindowText);
end;

function TdxSpreadSheetCellStyleEditDialogHelper.GetFontStyle(AListBoxIndex: Integer): TFontStyles;
begin
  Result := FontStyles[Max(0, AListBoxIndex)];
end;

function TdxSpreadSheetCellStyleEditDialogHelper.GetFontStyleName(AStyle: TFontStyles): string;
const
  NamesMap: array[0..3] of Pointer = (
    @sdxFontStyleRegular, @sdxFontStyleItalic, @sdxFontStyleBold, @sdxFontStyleBoldItalic
  );
var
  I: Integer;
begin
  Result := '';
  AStyle := AStyle - [fsUnderline, fsStrikeOut];
  for I := High(FontStyles) downto Low(FontStyles) do
    if AStyle = FontStyles[I] then
    begin
      Result := cxGetResourceString(NamesMap[I]);
      Break;
    end;
end;

function TdxSpreadSheetCellStyleEditDialogHelper.GetListBoxValue(AListBox: TcxListBox): string;
begin
  if AListBox.ItemIndex >= 0 then
    Result := AListBox.Items[AListBox.ItemIndex]
  else
    Result := '';
end;

procedure TdxSpreadSheetCellStyleEditDialogHelper.NavigateOnListbox(AKey: Word; AListBox: TcxListBox);
begin
  case AKey of
    VK_UP:
      AListBox.ItemIndex := Max(AListBox.ItemIndex - 1, 0);
    VK_DOWN:
      AListBox.ItemIndex := Min(AListBox.ItemIndex + 1, AListBox.Count - 1);
  end;
  CallNotify(AListBox.OnClick, AListBox);
end;

procedure TdxSpreadSheetCellStyleEditDialogHelper.SyncSearchEdit(AListBox: TcxListBox; AEdit: TcxCustomTextEdit);
begin
  if AListBox.ItemIndex >= 0 then
    AEdit.EditValue := AListBox.Items[AListBox.ItemIndex];
end;

procedure TdxSpreadSheetCellStyleEditDialogHelper.PopulateFillPatterns(AImageList: TcxImageList; AComboBox: TcxImageComboBox);
var
  ABitmap: TcxBitmap;
  AFillStyle: TdxSpreadSheetCellFillStyle;
  AItem: TcxImageComboBoxItem;
begin
  AComboBox.Properties.Items.BeginUpdate;
  try
    AComboBox.Properties.Items.Clear;
    ABitmap := TcxBitmap.CreateSize(AImageList.Width, AImageList.Height);
    try
      for AFillStyle := Low(AFillStyle) to High(AFillStyle) do
      begin
        cxPaintCanvas.BeginPaint(ABitmap.Canvas);
        dxSpreadSheetDrawBackground(cxPaintCanvas, ABitmap.ClientRect, clWhite, clBlack, AFillStyle);
        cxPaintCanvas.EndPaint;
        AItem := AComboBox.Properties.Items.Add;
        AItem.ImageIndex := AImageList.Add(ABitmap, nil);
        AItem.Tag := Ord(AFillStyle);
        AItem.Value := AItem.Tag;
      end;
    finally
      ABitmap.Free;
    end;
  finally
    AComboBox.Properties.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogHelper.PopulateFontSizes(AListBox: TcxListBox);
var
  ASavedItemIndex: Integer;
  I: Integer;
begin
  AListBox.Items.BeginUpdate;
  try
    ASavedItemIndex := AListBox.ItemIndex;
    try
      AListBox.Items.Clear;
      for I := 0 to dxDefaultFontSizeCount - 1 do
        AListBox.Items.Add(IntToStr(dxDefaultFontSizes[I]));
    finally
      AListBox.ItemIndex := ASavedItemIndex;
    end;
  finally
    AListBox.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogHelper.PopulateFontStyles(AListBox: TcxListBox);
var
  ASavedItemIndex: Integer;
  I: Integer;
begin
  AListBox.Items.BeginUpdate;
  try
    ASavedItemIndex := AListBox.ItemIndex;
    try
      AListBox.Items.Clear;
      for I := Low(FontStyles) to High(FontStyles) do
        AListBox.Items.Add(GetFontStyleName(FontStyles[I]));
    finally
      AListBox.ItemIndex := ASavedItemIndex;
    end;
  finally
    AListBox.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogHelper.PopulateFontUnderlineStyles(AComboBox: TcxComboBox);
var
  ASavedItemIndex: Integer;
begin
  AComboBox.Properties.Items.BeginUpdate;
  try
    ASavedItemIndex := AComboBox.ItemIndex;
    try
      AComboBox.Properties.Items.Clear;
      AComboBox.Properties.Items.Add(cxGetResourceString(@sdxFormatCellsDialogUnderlineNone));
      AComboBox.Properties.Items.Add(cxGetResourceString(@sdxFormatCellsDialogUnderlineSingle));
    finally
      AComboBox.ItemIndex := ASavedItemIndex;
    end;
  finally
    AComboBox.Properties.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogHelper.PopulateLocale(AComboBox: TcxComboBox);
var
  I: Integer;
begin
  AComboBox.Properties.Items.BeginUpdate;
  try
    AComboBox.Properties.Items.Clear;
    AComboBox.Properties.Items.Capacity := Languages.Count;
    for I := 0 to Languages.Count - 1 do
      AComboBox.Properties.Items.AddObject(Languages.Name[I], TObject(Languages.LocaleID[I]));
    AComboBox.Properties.Sorted := True;
  finally
    AComboBox.Properties.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogHelper.PopulateNumberFormatCategories(AListBox: TcxListBox);
var
  AIndex: TdxSpreadSheetNumberFormatCategory;
  ASavedItemIndex: Integer;
begin
  AListBox.Items.BeginUpdate;
  try
    ASavedItemIndex := AListBox.ItemIndex;
    try
      AListBox.Items.Clear;
      for AIndex := Low(AIndex) to High(AIndex) do
        AListBox.Items.AddObject(cxGetResourceString(dxSpreadSheetNumberFormatCategoryNames[AIndex]), TObject(AIndex));
    finally
      AListBox.ItemIndex := ASavedItemIndex;
    end;
  finally
    AListBox.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogHelper.PopulateNumberFormatTemplates(
  AListBox: TcxListBox; ACategory: TdxSpreadSheetNumberFormatCategory);

  procedure DoAddPredefinedFormat(AFormatID: Integer);
  var
    AHandle: TdxSpreadSheetFormatHandle;
  begin
    AHandle := PredefinedFormats.GetFormatHandleByID(AFormatID);
    if AHandle <> nil then
      AListBox.AddItem(FormatCodeToDisplayFormatCode(AHandle), TObject(AHandle.FormatCodeID));
  end;

  procedure DoAddPredefinedFormats(AFormatIDList: array of Integer);
  var
    I: Integer;
  begin
    for I := Low(AFormatIDList) to High(AFormatIDList) do
      DoAddPredefinedFormat(AFormatIDList[I]);
  end;

var
  I: Integer;
begin
  AListBox.Items.BeginUpdate;
  try
    AListBox.Items.Clear;
    case ACategory of
      nfcCurrency:
        DoAddPredefinedFormats([$05, $06, $07, $08]);
      nfcAccounting:
        DoAddPredefinedFormats([$29, $2A, $2B, $2C]);
      nfcDate:
        DoAddPredefinedFormats([$0E, $0F, $10, $11, $16]);
      nfcTime:
        DoAddPredefinedFormats([$12, $13, $14, $15, $2D, $2E, $2F]);
      nfcFraction:
        DoAddPredefinedFormats([$0C, $0D]);
      nfcCustom:
        for I := 00 to $31 do
          DoAddPredefinedFormat(I);
    end;
  finally
    AListBox.Items.EndUpdate;
  end;

  AListBox.ItemIndex := 0;
  CallNotify(AListBox.OnClick, AListBox);
end;

procedure TdxSpreadSheetCellStyleEditDialogHelper.PopulateTextAlignHorz(AComboBox: TcxComboBox);
var
  ASavedItemIndex: Integer;
  H: TdxSpreadSheetDataAlignHorz;
begin
  AComboBox.Properties.Items.BeginUpdate;
  try
    ASavedItemIndex := AComboBox.ItemIndex;
    try
      AComboBox.Properties.Items.Clear;
      for H := Low(H) to High(H) do
        AComboBox.Properties.Items.AddObject(cxGetResourceString(dxSpreadSheetDataHorzAlignNames[H]), TObject(H));
    finally
      AComboBox.ItemIndex := ASavedItemIndex;
    end;
  finally
    AComboBox.Properties.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetCellStyleEditDialogHelper.PopulateTextAlignVert(AComboBox: TcxComboBox);
var
  ASavedItemIndex: Integer;
  V: TdxSpreadSheetDataAlignVert;
begin
  AComboBox.Properties.Items.BeginUpdate;
  try
    ASavedItemIndex := AComboBox.ItemIndex;
    try
      AComboBox.Properties.Items.Clear;
      for V := Low(V) to High(V) do
        AComboBox.Properties.Items.AddObject(cxGetResourceString(dxSpreadSheetDataVertAlignNames[V]), TObject(V));
    finally
      AComboBox.ItemIndex := ASavedItemIndex;
    end;
  finally
    AComboBox.Properties.Items.EndUpdate;
  end;
end;

function TdxSpreadSheetCellStyleEditDialogHelper.GetPredefinedFormats: TdxSpreadSheetPredefinedFormats;
begin
  Result := FController.PredefinedFormats;
end;

{ TdxSpreadSheetCellStyleEditDialogNumberFormatHelper }

class function TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.BuildCodeForCurrency(
  ADecimalPlaces: Integer; const ASymbol: string; ALocale: Integer): string;
begin
  Result := '[$' + ASymbol + '-' + IntToHex(ALocale, 0) + ']' + BuildCodeForNumber(ADecimalPlaces, True);
end;

class function TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.BuildCodeForNumber(
  ADecimalPlaces: Integer; AUseThousandSeparator: Boolean): string;
begin
  Result := IfThen(AUseThousandSeparator, '#,##') + '0' + IfThen(ADecimalPlaces > 0, '.') + DupeString('0', ADecimalPlaces);
end;

class function TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.BuildCodeForPercentage(ADecimalPlaces: Integer): string;
begin
  Result := BuildCodeForNumber(ADecimalPlaces, False) + '%';
end;

class function TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.BuildCodeForScientific(ADecimalPlaces: Integer): string;
begin
  Result := BuildCodeForNumber(ADecimalPlaces, False) + 'E+00';
  IsScientificCode(Result, ADecimalPlaces);
end;

class function TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.IsCurrencyCode(ACode: string;
  out ADecimalPlaces: Integer; out ASymbol: string; out ALocale: Integer): Boolean;
var
  APos1: Integer;
  APos2: Integer;
  AUnused: Boolean;
begin
  Result := False;
  if ACode <> '' then
  begin
    if ACode[1] = '$' then
    begin
      ASymbol := '$';
      ALocale := dxMakeLCID(dxMakeLangID(LANG_ENGLISH, SUBLANG_ENGLISH_US), SORT_DEFAULT);
      Result := IsNumberCode(Copy(ACode, 2, MaxInt), ADecimalPlaces, AUnused);
    end
    else
      if ACode[1] = '[' then
      begin
        APos1 := Pos('-', ACode);
        APos2 := Pos(']', ACode);
        if (APos1 > 0) and (APos2 > 0) and (APos1 < APos2) then
        begin
          ASymbol := Copy(ACode, 2, APos1 - 2);
          ALocale := StrToIntDef('$' + Copy(ACode, APos1 + 1, APos2 - APos1 - 1), -1);
          Delete(ACode, 1, APos2);
          Result := (ALocale >= 0) and IsNumberCode(ACode, ADecimalPlaces, AUnused);
        end;
      end;
  end;
end;

class function TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.IsGeneralCode(const ACode: string): Boolean;
begin
  Result := (ACode = '') or SameText(ACode, dxSpreadSheetGeneralNumberFormat);
end;

class function TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.IsNumberCode(
  const ACode: string; out ADecimalPlaces: Integer; out AUseThousandSeparator: Boolean): Boolean;
var
  AIndex: Integer;
begin
  Result := CheckCode(ACode, '(#,##){0,1}0(\.0+){0,1}');
  if Result then
  begin
    AUseThousandSeparator := ACode[1] = '#';

    AIndex := LastDelimiter('.', ACode);
    if AIndex > 0 then
      ADecimalPlaces := Length(ACode) - AIndex
    else
      ADecimalPlaces := 0;
  end;
end;

class function TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.IsPercentageCode(
  const ACode: string; out ADecimalPlaces: Integer): Boolean;
var
  AIndex: Integer;
begin
  Result := CheckCode(ACode, '0(\.0+){0,1}\%');
  if Result then
  begin
    AIndex := LastDelimiter('.', ACode);
    if AIndex > 0 then
      ADecimalPlaces := Length(ACode) - AIndex - 1
    else
      ADecimalPlaces := 0;
  end;
end;

class function TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.IsScientificCode(
  const ACode: string; out ADecimalPlaces: Integer): Boolean;
var
  AIndex: Integer;
begin
  Result := CheckCode(ACode, '0(\.0+){0,1}E\+00');
  if Result then
  begin
    AIndex := LastDelimiter('.', ACode);
    if AIndex > 0 then
      ADecimalPlaces := Length(ACode) - AIndex - 4
    else
      ADecimalPlaces := 0;
  end;
end;

class function TdxSpreadSheetCellStyleEditDialogNumberFormatHelper.CheckCode(const ACode, ARegExpr: string): Boolean;
var
  AChar: Char;
  ARegExp: TcxRegExpr;
  I: Integer;
begin
  ARegExp := TcxRegExpr.Create;
  try
    Result := True;
    ARegExp.Compile(ARegExpr);
    for I := 1 to Length(ACode) do
    begin
      AChar := ACode[I];
      Result := ARegExp.Next(AChar);
      if not Result then
        Break;
    end;
  finally
    ARegExp.Free;
  end;
end;

end.
