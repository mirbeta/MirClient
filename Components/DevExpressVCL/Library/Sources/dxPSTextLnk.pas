{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSTextLnk;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, Controls, StdCtrls, ExtCtrls, ComCtrls, dxCore,
  dxPSCore, dxPrnPg, cxDrawTextUtils, dxPSGlbl, dxExtCtrls, ImgList, cxPC, cxControls,
  cxContainer, cxEdit, cxLabel, cxTextEdit, cxMaskEdit, cxSpinEdit, cxGraphics,
  cxDropDownEdit, Menus, cxLookAndFeelPainters, cxButtons, dxPSReportRenderCanvas, cxLookAndFeels,
  dxPSReportLinkDesignWindow, dxLayoutControlAdapters, dxLayoutLookAndFeels,
  dxLayoutContainer, cxClasses, dxLayoutControl, dxLayoutcxEditAdapters, cxImageList;

type
  TdxCustomTextReportLink = class;
  TdxfmTextReportLinkDesignWindow = class;

  TdxMemoLineItem = class(TdxReportCellString)
  public
    function GetDTFormat: DWORD; override;
  end;

  TdxTextReportLinkCustomDrawRowEvent = procedure(Sender: TdxCustomTextReportLink;
    ACanvas: TCanvas; AnItem: TdxReportCellString; var ADone: Boolean) of object;

  TdxTextReportLinkInitializeRowEvent = procedure(Sender: TdxCustomTextReportLink;
    AnItem: TdxReportCellString) of object;

  TdxCustomTextReportLink = class(TBasedxReportLink)
  private
    FAlignment: TcxTextAlignX;
    FDelimiters: TList;
    FFontIndex: Integer;
    FScreenCanvas: TdxPSReportRenderCustomCanvas;
    FSupportedCustomDraw: Boolean;
    FTextRowHeight: Integer;
    FTextRowSpacing: Double;
    FOnCustomDrawRow: TdxTextReportLinkCustomDrawRowEvent;
    FOnInitializeRow: TdxTextReportLinkInitializeRowEvent;
    function GetDelimiter(Index: Integer): Integer;
    function GetDelimiterCount: Integer;
    function GetDesignWindow: TdxfmTextReportLinkDesignWindow;
    function GetTextBounds: TRect;
    function IsTextRowSpacingStored: Boolean;
    procedure SetAlignment(Value: TcxTextAlignX);
    procedure SetOnCustomDrawRow(Value: TdxTextReportLinkCustomDrawRowEvent);
    procedure SetSupportedCustomDraw(Value: Boolean);
    procedure SetTextRowSpacing(Value: Double);
  protected
    procedure ConstructReport(AReportCells: TdxReportCells); override;
    procedure ConvertCoords; override;
    procedure CustomDraw(AItem: TAbstractdxReportCellData; ACanvas: TCanvas;
      ABoundsRect, AClientRect: TRect; var ADone: Boolean); override;
    function GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean; override;
    procedure InternalRestoreDefaults; override;
    function IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean; override;
    procedure MakeDelimiters(AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList); override;

    procedure AddDelimiter(Value: Integer); overload;
    procedure AddDelimiter(AItem: TdxReportVisualItem); overload;

    procedure AdjustHostBounds(AHost: TdxReportCell);
    procedure CalculateTextRowHeight; virtual;
    function CreateHost(AReportCells: TdxReportCells): TdxReportCell; virtual;
    function CreateItem(AParent: TdxReportCell; const ATextRow: TcxTextRow): TdxReportCellString; virtual;
    procedure CreateItems(AHost: TdxReportCell);
    function GetTextRowBoundsWidth: Integer; virtual;
    procedure InitializeHost(AHost: TdxReportCell); virtual;
    procedure InitializeItem(AItem: TdxReportCellString; const AText: string); virtual;

    procedure DoConstructReport(AReportCells: TdxReportCells); virtual;
    procedure PrepareConstruct(AReportCells: TdxReportCells); virtual;
    procedure UnprepareConstruct(AReportCells: TdxReportCells); virtual;

    function GetText: string; virtual;
    function GetTextFormat: DWORD; virtual;
    function GetTextItemClass: TdxReportCellStringClass; virtual;

    procedure DoCustomDrawRow(ACanvas: TCanvas; AnItem: TdxReportCellString; var ADone: Boolean); dynamic;
    procedure DoInitializeRow(AnItem: TdxReportCellString); dynamic;

    property DelimiterCount: Integer read GetDelimiterCount;
    property DelimiterList: TList read FDelimiters;
    property Delimiters[Index: Integer]: Integer read GetDelimiter;
    property FontIndex: Integer read FFontIndex;
    property ScreenCanvas: TdxPSReportRenderCustomCanvas read FScreenCanvas;
    property TextBounds: TRect read GetTextBounds;
    property TextFormat: DWORD read GetTextFormat;
    property TextRowBoundsWidth: Integer read GetTextRowBoundsWidth;
    property TextRowHeight: Integer read FTextRowHeight write FTextRowHeight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function Aggregable: Boolean; override;
    function DataProviderPresent: Boolean; override;

    property Alignment: TcxTextAlignX read FAlignment write SetAlignment default taLeft;
    property Color;
    property DesignWindow: TdxfmTextReportLinkDesignWindow read GetDesignWindow;
    property Font;
    property ScaleFonts;
    property SupportedCustomDraw: Boolean read FSupportedCustomDraw write SetSupportedCustomDraw default False;
    property Text: string read GetText;
    property TextRowSpacing: Double read FTextRowSpacing write SetTextRowSpacing stored IsTextRowSpacingStored;
    property Transparent;
    property UseHorzDelimiters;

    property OnCustomDrawRow: TdxTextReportLinkCustomDrawRowEvent read FOnCustomDrawRow write SetOnCustomDrawRow;
    property OnInitializeRow: TdxTextReportLinkInitializeRowEvent read FOnInitializeRow write FOnInitializeRow;
  end;

  TdxCustomStringsReportLink = class(TdxCustomTextReportLink)
  protected
    function GetStrings: TStrings; virtual; abstract;
    function GetText: string; override;
    function GetTextFormat: DWORD; override;
  public
    property Strings: TStrings read GetStrings;
  end;

  TdxTextReportLink = class(TdxCustomStringsReportLink)
  private
    FStrings: TStrings;
    procedure StringsChanged(Sender: TObject);
  protected
    function GetStrings: TStrings; override;
    procedure SetStrings(Value: TStrings); virtual;
    procedure SetText(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Text write SetText;
  published
    property Alignment;
    property Color;
    property Font;
    property ScaleFonts;
    property Strings write SetStrings;
    property TextRowSpacing;
    property SupportedCustomDraw;
    property UseHorzDelimiters;
    property Transparent;

    property OnCustomDrawRow;
    property OnInitializeRow;
  end;

  TdxCustomMemoReportLink = class(TdxCustomStringsReportLink)
  protected
    function GetTextRowBoundsWidth: Integer; override;
    procedure InternalRestoreFromOriginal; override;

    function GetCustomMemo: TCustomMemo; virtual;
    function GetStrings: TStrings; override;
    property CustomMemo: TCustomMemo read GetCustomMemo;
  published
    property Alignment;
    property Color;
    property Font;
    property ScaleFonts;
    property SupportedCustomDraw;
    property TextRowSpacing;
    property UseHorzDelimiters;
    property Transparent;

    property OnCustomDrawRow;
    property OnInitializeRow;
  end;

  TdxMemoReportLink = class(TdxCustomMemoReportLink)
  private
    function GetMemo: TMemo;
  public
    property Memo: TMemo read GetMemo;
  end;

  TdxfmTextReportLinkDesignWindow = class(TStandarddxReportLinkDesignWindow)
    btnFont: TcxButton;
    cbxAlignment: TcxComboBox;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    edFont: TcxTextEdit;
    ilAlignments: TcxImageList;
    lblAlignment: TdxLayoutItem;
    lblLineSpacing: TdxLayoutItem;
    lblPreview: TdxLayoutItem;
    pbxPreview: TPaintBox;
    pnlPreview: TPanel;
    seLineSpacing: TcxSpinEdit;
    tshOptions: TdxLayoutGroup;

    procedure btnFontClick(Sender: TObject);
    procedure cbxAlignmentClick(Sender: TObject);
    procedure cbxAlignmentPropertiesDrawItem(AControl: TcxCustomComboBox;
      ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
    procedure LineSpacingChanged(Sender: TObject);
    procedure pbxPreviewPaint(Sender: TObject);
  private
    function GetReportLink: TdxCustomTextReportLink;
  protected
    procedure DoInitialize; override;
    procedure LoadStrings; override;
    procedure UpdatePreview; override;
  public
    constructor Create(AOwner: TComponent); override;
    //
    property ReportLink: TdxCustomTextReportLink read GetReportLink;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, dxPSUtl, dxPSRes, cxGeometry, dxDPIAwareUtils;

type
  TCustomMemoAccess = class(TCustomMemo);


{ TdxMemoLineItem }

function TdxMemoLineItem.GetDTFormat: DWORD;
begin
  if Renderer.ReportLink is TdxCustomTextReportLink then
    Result := TdxCustomTextReportLink(Renderer.ReportLink).GetTextFormat
  else
    Result := inherited GetDTFormat and not CXTO_AUTOINDENTS;
end;

{ TdxCustomTextReportLink }

constructor TdxCustomTextReportLink.Create(AOwner: TComponent);
begin
  inherited;
  FDelimiters := TList.Create;
  InternalRestoreDefaults;
  LinkModified(False);
end;

destructor TdxCustomTextReportLink.Destroy;
begin
  FreeAndNil(FDelimiters);
  inherited;
end;

procedure TdxCustomTextReportLink.Assign(Source: TPersistent);
begin
  if Source is TdxCustomTextReportLink then
    with TdxCustomTextReportLink(Source) do
    begin
      Self.Alignment := Alignment;
      Self.SupportedCustomDraw := SupportedCustomDraw;
      Self.TextRowSpacing := TextRowSpacing;
    end;
  inherited;
end;

class function TdxCustomTextReportLink.Aggregable: Boolean;
begin
  Result := False;
end;

function TdxCustomTextReportLink.DataProviderPresent: Boolean;
begin
  if DataSource = rldsComponent then
    Result := Text <> ''
  else
    Result := inherited DataProviderPresent;
end;

procedure TdxCustomTextReportLink.ConstructReport(AReportCells: TdxReportCells);
begin
  PrepareConstruct(AReportCells);
  try
    DoConstructReport(AReportCells);
  finally
    UnprepareConstruct(AReportCells)
  end;
end;

procedure TdxCustomTextReportLink.ConvertCoords;

  procedure ConvertDelimiters(ADelimiters: TList);
  var
    I, Value: Integer;
  begin
    for I := 0 to ADelimiters.Count - 1 do
    begin
      Value := Integer(ADelimiters[I]);
      Value := MulDiv(Value, PixelsNumerator, PixelsDenominator);
      ADelimiters[I] := Pointer(Value);
    end;
  end;

begin
  inherited;
  ConvertDelimiters(DelimiterList);
end;

procedure TdxCustomTextReportLink.CustomDraw(AItem: TAbstractdxReportCellData;
  ACanvas: TCanvas; ABoundsRect, AClientRect: TRect; var ADone: Boolean);
begin
  DoCustomDrawRow(ACanvas, TdxReportCellString(AItem), ADone);
end;

function TdxCustomTextReportLink.GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean;
begin
  Result := AUpdateCodes * uaMarginsVert  <> [];
end;

procedure TdxCustomTextReportLink.InternalRestoreDefaults;
begin
  inherited;
  Alignment := taLeft;
  TextRowSpacing := 1;
  SupportedCustomDraw := False;
end;

function TdxCustomTextReportLink.IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean;
begin
  Result := SupportedCustomDraw and (Item <> nil);
end;

procedure TdxCustomTextReportLink.MakeDelimiters(AReportCells: TdxReportCells;
  AHorzDelimiters, AVertDelimiters: TList);
begin
  inherited;
  dxCopyList(DelimiterList, AVertDelimiters);
end;

procedure TdxCustomTextReportLink.AddDelimiter(Value: Integer);
begin
  FDelimiters.Add(TObject(Value));
end;

procedure TdxCustomTextReportLink.AddDelimiter(AItem: TdxReportVisualItem);
begin
  with AItem.AbsoluteRect do
  begin
    AddDelimiter(Top);
    AddDelimiter(Bottom);
  end;
end;

procedure TdxCustomTextReportLink.AdjustHostBounds(AHost: TdxReportCell);
begin
  with AHost do
    if DataItemCount <> 0 then
    begin
      Left := 0;
      Top := 0;
      Width := DataItems[0].Width;
      Height := DataItems[DataItemCount - 1].BoundsRect.Bottom;
    end;
end;

procedure TdxCustomTextReportLink.CalculateTextRowHeight;
begin
  FTextRowHeight := Round(FTextRowSpacing * cxTextHeight(Font));
end;

function TdxCustomTextReportLink.CreateHost(AReportCells: TdxReportCells): TdxReportCell;
begin
  Result := TdxReportCell.Create(AReportCells.Cells);
  InitializeHost(Result);
end;

function TdxCustomTextReportLink.CreateItem(AParent: TdxReportCell;
  const ATextRow: TcxTextRow): TdxReportCellString;

  function ExtractString(ATextRow: TcxTextRow): string;
  begin
    SetString(Result, ATextRow.Text, ATextRow.TextLength);
  end;

  procedure PlaceItem(AItem: TdxReportCellString);
  begin
    with Result do
    begin
      Left := 0;
      Top := Index * TextRowHeight;
      Width := TextRowBoundsWidth;
      Height := TextRowHeight;
    end;
 end;

begin
  Result := GetTextItemClass.Create(AParent);
  PlaceItem(Result);
  InitializeItem(Result, ExtractString(ATextRow));
  DoInitializeRow(Result);
end;

procedure TdxCustomTextReportLink.CreateItems(AHost: TdxReportCell);
var
  TextParams: TcxTextParams;
  TextRows: TcxTextRows;
  I, RowCount: Integer;
  Item: TdxReportCellString;
  PercentsDone: Double;
begin
  ScreenCanvas.SaveState;
  try
    ScreenCanvas.Font := Font;
    TextParams := ScreenCanvas.CalculateTextParams(TextFormat);
    ScreenCanvas.MakeTextRows(PChar(Text), Length(Text), TextBounds, TextParams,
      TextRows, RowCount);
    try
      for I := 0 to cxGetTextRowCount(TextRows) - 1 do
      begin
        Item := CreateItem(AHost, cxGetTextRow(TextRows, I)^);
        AddDelimiter(Item);

        PercentsDone := (I + 1) * 100 / cxGetTextRowCount(TextRows);
        DoProgress(PercentsDone);
        if AbortBuilding then Break;
      end;
    finally
      cxResetTextRows(TextRows);
    end;
  finally
    ScreenCanvas.RestoreState;
  end;
end;

function TdxCustomTextReportLink.GetTextRowBoundsWidth: Integer;
begin
  with RealPrinterPage.PaintRectPixels do
    Result := Right - Left - 1;
end;

procedure TdxCustomTextReportLink.InitializeHost(AHost: TdxReportCell);
begin
  AHost.CellSides := [];
  AHost.Transparent := True;
end;

procedure TdxCustomTextReportLink.InitializeItem(AItem: TdxReportCellString;
  const AText: string);
begin
  AItem.CellSides := [];
  AItem.FontIndex := FontIndex;
  AItem.Text := AText;
  AItem.TextAlignX := Alignment;
  AItem.Transparent := Transparent;
end;

procedure TdxCustomTextReportLink.DoConstructReport(AReportCells: TdxReportCells);
var
  Host: TdxReportCell;
  R: TRect;
begin
  Host := CreateHost(AReportCells);
  CreateItems(Host);
  if not AbortBuilding then
  begin
    AdjustHostBounds(Host);

    R := Host.BoundsRect;
    OffsetRect(R, -R.Left, -R.Top);
    AReportCells.Cells.BoundsRect := R;
  end;
end;

procedure TdxCustomTextReportLink.PrepareConstruct(AReportCells: TdxReportCells);
begin
  FScreenCanvas := TdxPSReportRenderScreenCanvas.Create;
  FDelimiters.Clear;
  FFontIndex := AddFontToPool(Font);
  CalculateTextRowHeight;
end;

procedure TdxCustomTextReportLink.UnprepareConstruct(AReportCells: TdxReportCells);
begin
  FreeAndNil(FScreenCanvas);
end;

function TdxCustomTextReportLink.GetText: string;
begin
  Result := '';
end;

function TdxCustomTextReportLink.GetTextFormat: DWORD;
begin
  Result := CXTO_WORDBREAK or CXTO_EXPANDTABS or cxDrawTextUtils.cxMakeFormat(Alignment, taTop);
end;

function TdxCustomTextReportLink.GetTextItemClass: TdxReportCellStringClass;
begin
  Result := TdxMemoLineItem;
end;

procedure TdxCustomTextReportLink.DoCustomDrawRow(ACanvas: TCanvas;
  AnItem: TdxReportCellString; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawRow) then
    FOnCustomDrawRow(Self, ACanvas, AnItem, ADone);
end;

procedure TdxCustomTextReportLink.DoInitializeRow(AnItem: TdxReportCellString);
begin
  if Assigned(FOnInitializeRow) then FOnInitializeRow(Self, AnItem);
end;

function TdxCustomTextReportLink.GetDelimiter(Index: Integer): Integer;
begin
  Result := Integer(FDelimiters[Index]);
end;

function TdxCustomTextReportLink.GetDelimiterCount: Integer;
begin
  Result := FDelimiters.Count;
end;

function TdxCustomTextReportLink.GetDesignWindow: TdxfmTextReportLinkDesignWindow;
begin
  Result := inherited DesignWindow as TdxfmTextReportLinkDesignWindow;
end;

function TdxCustomTextReportLink.GetTextBounds: TRect;
begin
  Result := RealPrinterPage.PaintRectPixels;
  Dec(Result.Right);
  InflateRect(Result, -1, -1);
  Result.Bottom := MaxInt; // no limit
end;

function TdxCustomTextReportLink.IsTextRowSpacingStored: Boolean;
begin
  Result := Abs(TextRowSpacing - 1) > 0.001;
end;

procedure TdxCustomTextReportLink.SetAlignment(Value: TcxTextAlignX);
begin
  if Value = taJustifyX then
    Value := taDistributeX;
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    LinkModified(True);
  end;
end;

procedure TdxCustomTextReportLink.SetOnCustomDrawRow(Value: TdxTextReportLinkCustomDrawRowEvent);
begin
  if @FOnCustomDrawRow <> @Value then
  begin
    FOnCustomDrawRow := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TdxCustomTextReportLink.SetSupportedCustomDraw(Value: Boolean);
begin
  if FSupportedCustomDraw <> Value then
  begin
    FSupportedCustomDraw := Value;
    if Assigned(FOnCustomDrawRow) then LinkModified(True);
  end;
end;

procedure TdxCustomTextReportLink.SetTextRowSpacing(Value: Double);
begin
  if Value < 1 then Value := 1;
  if FTextRowSpacing <> Value then
  begin
    FTextRowSpacing := Value;
    LinkModified(True);
  end;
end;

{ TdxCustomStringsReportLink }

function TdxCustomStringsReportLink.GetText: string;
begin
  if Strings <> nil then
    Result := Strings.Text
  else
    Result := '';
end;

function TdxCustomStringsReportLink.GetTextFormat: DWORD;
begin
  Result := inherited GetTextFormat or CXTO_CHARBREAK;
end;

{ TdxTextReportLink }

constructor TdxTextReportLink.Create(AOwner: TComponent);
begin
  inherited;
  FStrings := TStringList.Create;
  TStringList(FStrings).OnChange := StringsChanged;
end;

destructor TdxTextReportLink.Destroy;
begin
  FreeAndNil(FStrings);
  inherited;
end;

procedure TdxTextReportLink.Assign(Source: TPersistent);
begin
  if Source is TdxTextReportLink then
    Text := TdxTextReportLink(Source).Text;
  inherited;
end;

function TdxTextReportLink.GetStrings: TStrings;
begin
  Result := FStrings;
end;

procedure TdxTextReportLink.SetStrings(Value: TStrings);
begin
  Strings.Assign(Value);
end;

procedure TdxTextReportLink.SetText(const Value: string);
begin
  Strings.Text := Value;
end;

procedure TdxTextReportLink.StringsChanged(Sender: TObject);
begin
  LinkModified(True);
end;

{ TdxCustomMemoReportLink }

function TdxCustomMemoReportLink.GetTextRowBoundsWidth: Integer;
var
  ControllerIntf: IdxReportLinkController;
begin
  if IsAggregated and SysUtils.Supports(TObject(Controller), IdxReportLinkController, ControllerIntf) then
    with ControllerIntf.GetControlSiteBounds(CustomMemo) do
      Result := Right - Left - 1
  else
    Result := inherited GetTextRowBoundsWidth;
end;

procedure TdxCustomMemoReportLink.InternalRestoreFromOriginal;
begin
  inherited;
  Alignment := dxPSCore.dxTextAlignX[TCustomMemoAccess(CustomMemo).Alignment];
end;

function TdxCustomMemoReportLink.GetCustomMemo: TCustomMemo;
begin
  Result := inherited Component as TCustomMemo;
end;

function TdxCustomMemoReportLink.GetStrings: TStrings;
begin
  if CustomMemo <> nil then
    Result := CustomMemo.Lines
  else
    Result := nil;
end;

{ TdxMemoReportLink }

function TdxMemoReportLink.GetMemo: TMemo;
begin
  Result := inherited Component as TMemo;
end;

{ TdxfmTextReportLinkDesignWindow }

constructor TdxfmTextReportLinkDesignWindow.Create(AOwner: TComponent);
begin
  HelpContext := dxPSGlbl.dxhcTextReportLinkDesigner;
  inherited Create(AOwner);
end;

procedure TdxfmTextReportLinkDesignWindow.DoInitialize;
begin
  inherited;
  cbxAlignment.ItemIndex := cbxAlignment.Properties.Items.IndexOfObject(
    TObject(ReportLink.Alignment));
  seLineSpacing.Value := ReportLink.TextRowSpacing;
  FontInfoToText(ReportLink.Font, edFont);
end;

procedure TdxfmTextReportLinkDesignWindow.LoadStrings;
begin
  inherited;
  tshOptions.Caption := cxGetResourceString(@sdxOptions);
  lblPreview.Caption := DropAmpersand(cxGetResourceString(@sdxPreview));

  lblAlignment.Caption := AddColon(cxGetResourceString(@sdxAlignment));
  lblLineSpacing.Caption := cxGetResourceString(@sdxLineSpacing);
  with cbxAlignment.Properties do
  begin
    Items.BeginUpdate;
    try
      Items.AddObject(cxGetResourceString(@sdxTextAlignLeft), TObject(taLeft));
      Items.AddObject(cxGetResourceString(@sdxTextAlignCenter), TObject(taCenterX));
      Items.AddObject(cxGetResourceString(@sdxTextAlignRight), TObject(taRight));
      Items.AddObject(cxGetResourceString(@sdxTextAlignJustified), TObject(taDistributeX));
    finally
      Items.EndUpdate;
    end;
  end;
  btnFont.Caption := cxGetResourceString(@sdxBtnFont);
end;

procedure TdxfmTextReportLinkDesignWindow.UpdatePreview;
begin
  pbxPreview.Invalidate;
end;

function TdxfmTextReportLinkDesignWindow.GetReportLink: TdxCustomTextReportLink;
begin
  Result := inherited ReportLink as TdxCustomTextReportLink;
end;

procedure TdxfmTextReportLinkDesignWindow.LineSpacingChanged(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.TextRowSpacing := TcxSpinEdit(Sender).Value;
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmTextReportLinkDesignWindow.cbxAlignmentClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;

  with TcxComboBox(Sender) do
    ReportLink.Alignment := TcxTextAlignX(Properties.Items.Objects[ItemIndex]);
  Modified := True;
  UpdatePreview;
end;

procedure TdxfmTextReportLinkDesignWindow.btnFontClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;

  dxPSGlbl.FontDialog.Font := ReportLink.Font;
  if dxPSGlbl.FontDialog.Execute then
  begin
    ReportLink.Font := dxPSGlbl.FontDialog.Font;
    FontInfoToText(ReportLink.Font, edFont);
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TdxfmTextReportLinkDesignWindow.pbxPreviewPaint(Sender: TObject);

  function GetText: string;
  const
    RowCount: Integer = 10;
  var
    Line: string;
    I: Integer;
  begin
    Line := cxGetResourceString(@sdxSampleText);
    Result := '';
    for I := 0 to RowCount - 1 do
      Result := Result + Line + #13#10;
  end;

var
  Format: DWORD;
  R: TRect;
begin
  Format := CXTO_PATTERNEDTEXT + CXTO_WORDBREAK + CXTO_CHARBREAK + cxMakeFormat(ReportLink.Alignment, taTop);
  with TPaintBox(Sender) do
  begin
    dxAssignFont(Canvas.Font, ReportLink.Font, Self.ScaleFactor, ReportLink.ScaleFactor);
    R := ClientRect;
    cxTextOut(Canvas.Handle, GetText, R, Format, Canvas.Font, 0, 0, 0, clDefault, ReportLink.TextRowSpacing);
  end;
end;

procedure TdxfmTextReportLinkDesignWindow.cbxAlignmentPropertiesDrawItem(
  AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
var
  AImageRect: TRect;
  R: TRect;
begin
  R := ARect;
  ACanvas.FillRect(R, clDefault);
  R := cxRectInflate(R, -ScaleFactor.Apply(1));
  AImageRect := cxRectSetSize(R, dxGetImageSize(ilAlignments, ScaleFactor));
  cxDrawImage(ACanvas, AImageRect, nil, ilAlignments, AIndex, True, nil, ScaleFactor);
  R.Left := AImageRect.Right + ScaleFactor.Apply(4);
  ACanvas.DrawTexT(AControl.Properties.Items[AIndex], R, cxAlignVCenter);
end;

initialization
  dxPSRegisterReportLink(TdxTextReportLink, nil, TdxfmTextReportLinkDesignWindow);
  dxPSRegisterReportLink(TdxMemoReportLink, TMemo, TdxfmTextReportLinkDesignWindow);

finalization
  dxPSUnregisterReportLink(TdxMemoReportLink, TMemo, TdxfmTextReportLinkDesignWindow);
  dxPSUnregisterReportLink(TdxTextReportLink, nil, TdxfmTextReportLinkDesignWindow);

end.
