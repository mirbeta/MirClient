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

unit dxPSChLbxLnk;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, CheckLst, Menus,
  Types, dxCore, dxPSCore, dxPSGrLnks, dxPSGlbl, dxPSBaseGridLnk, cxPC, cxControls, cxGraphics, cxTextEdit, cxMaskEdit,
  cxDropDownEdit, cxColorComboBox, cxCheckBox, cxContainer, cxEdit, cxLabel, cxLookAndFeelPainters, cxButtons,
  cxLookAndFeels, cxImageComboBox, dxPSReportLinkDesignWindow, dxLayoutControlAdapters, dxLayoutLookAndFeels, cxGeometry,
  dxLayoutContainer, cxClasses, dxLayoutControl, dxLayoutcxEditAdapters, cxImage;

type
  TdxPSCheckGridCellDataMap = class(TdxPSTextGridCellDataMap)
  protected
    class procedure InitializeCellData(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData;
      AReportLink: TAbstractdxGridReportLink); override;
    class function DataClass: TdxReportCellDataClass; override;
  end;

  TdxCheckListBoxPaintOption = (chlbxpoBorder, chlbxpoHorzLines, chlbxpoFlatCheckMarks);
  TdxCheckListBoxPaintOptions = set of TdxCheckListBoxPaintOption;

  TdxCheckListBoxReportLink = class(TdxCustomListBoxReportLink)
  private
    FOptions: TdxCheckListBoxPaintOptions;
    function GetOptions: TdxCheckListBoxPaintOptions;
    function GetCheckListBox: TCheckListBox;
    procedure SetOptions(Value: TdxCheckListBoxPaintOptions);
    function IsFlatCheckMarks: Boolean;
  protected
    function GetDataItemClass(ACol: Integer; ARow: Integer = 0): TdxReportCellDataClass; override;
    procedure InternalRestoreDefaults; override;
    procedure InternalRestoreFromOriginal; override;

    function IsDrawBorder: Boolean; override;
    function IsDrawHorzLines: Boolean; override;
    procedure SetDrawMode(Value: TdxGridDrawMode); override;
  public
    procedure Assign(Source: TPersistent); override;
    property CheckListBox: TCheckListBox read GetCheckListBox;
  published
    property AutoWidth;
    property Color;
    property DrawMode;
    property EndEllipsis;
    property EvenColor;
    property EvenFont;
    property Font;
    property OddColor;
    property OddFont;
    property Options: TdxCheckListBoxPaintOptions read GetOptions write SetOptions
      default [chlbxpoBorder..chlbxpoFlatCheckMarks];
    property Multiline;
    property RowAutoHeight;
    property ScaleFonts;
    property SupportedCustomDraw;
    property Transparent;
    property UseCustomPageBreaks;
    property UseHorzDelimiters;
    property UseVertDelimiters;
    property Width;

    property OnCustomDrawItem;
    property OnGetCustomPageBreaks;
    property OnInitializeItem;
  end;

  TdxChlbxReportLinkDesignWindow = class(TStandarddxReportLinkDesignWindow)
    btnEvenFont: TcxButton;
    btnFont: TcxButton;
    cbxDrawMode: TcxImageComboBox;
    ccbxColor: TcxColorComboBox;
    ccbxEvenColor: TcxColorComboBox;
    ccbxGridLineColor: TcxColorComboBox;
    chbxAutoWidth: TcxCheckBox;
    chbxFlatCheckMarks: TcxCheckBox;
    chbxRowAutoHeight: TcxCheckBox;
    chbxShowBorders: TcxCheckBox;
    chbxShowHorzLines: TcxCheckBox;
    chbxTransparent: TcxCheckBox;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    dxLayoutItem14: TdxLayoutItem;
    dxLayoutItem15: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    edEvenFont: TcxTextEdit;
    edFont: TcxTextEdit;
    Image1: TcxImage;
    imgGrid: TcxImage;
    lblColor: TdxLayoutItem;
    lblDrawMode: TdxLayoutItem;
    lblEvenColor: TdxLayoutItem;
    lblGridLinesColor: TdxLayoutItem;
    lblMiscellaneous: TcxLabel;
    lblPreview: TdxLayoutItem;
    lblShow: TcxLabel;
    pcMain: TdxLayoutGroup;
    pnlPreview: TPanel;
    stTransparent: TcxLabel;
    tshColor: TdxLayoutGroup;
    tshFont: TdxLayoutGroup;
    tshOptions: TdxLayoutGroup;

    procedure ccbxColorChange(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure pbxPreviewPaint(Sender: TObject);
    procedure chbxTransparentClick(Sender: TObject);
    procedure cbxDrawModeClick(Sender: TObject);
    procedure chbxShowBordersClick(Sender: TObject);
    procedure chbxRowAutoHeightClick(Sender: TObject);
    procedure chbxAutoWidthClick(Sender: TObject);
    procedure stTransparentClick(Sender: TObject);
    procedure pnlPreviewResize(Sender: TObject);
  private
    FItemCount: Integer;
    FPaintWidth: Integer;
    FPaintHeight: Integer;
    FPreviewBox: TCustomPanel;
    FPreviewFont: TFont;
    FRectWidth: Integer;
    FRectHeight: Integer;
    procedure CreateControls;
    function GetReportLink: TdxCheckListBoxReportLink;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure DoInitialize; override;
    function GetPreviewHost: TCustomPanel; override;
    procedure LoadGroupsIcons; override;
    procedure LoadStrings; override;
    procedure PaintPreview(ACanvas: TCanvas; R: TRect); override;
    procedure UpdateControlsState; override;
    procedure UpdatePreview; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ReportLink: TdxCheckListBoxReportLink read GetReportLink;
  end;

const
  dxDefaultCheckListBoxPaintOptions: TdxCheckListBoxPaintOptions =
    [chlbxpoBorder, chlbxpoHorzLines, chlbxpoFlatCheckMarks];

  DesignerStringCount = 6;
  dxCheckListBoxStrings: array[0..DesignerStringCount - 1] of string =
    ('Sample Text Row #1', 'Sample Text Row #2', 'Sample Text Row #3',
     'Sample Text Row #4', 'Sample Text Row #5', 'Sample Text Row #6');

implementation

uses
  dxExtCtrls, dxPSRes, dxPrnDev, dxPSUtl, dxPSImgs, dxDPIAwareUtils;

{$R *.DFM}

{ TdxPSCheckGridCellDataMap }

class function TdxPSCheckGridCellDataMap.DataClass: TdxReportCellDataClass;
begin
  Result := TdxReportCellCheck;
end;

class procedure TdxPSCheckGridCellDataMap.InitializeCellData(ACol, ARow: Integer;
  ADataItem: TAbstractdxReportCellData; AReportLink: TAbstractdxGridReportLink);
begin
  inherited;
  with TdxReportCellCheck(ADataItem) do
  begin
    CheckPos := ccpLeft;
    Checked := TdxCheckListBoxReportLink(AReportLink).CheckListBox.State[ARow] > cbUnchecked;
    Enabled := TdxCheckListBoxReportLink(AReportLink).CheckListBox.State[ARow] < cbGrayed;
    Enabled := Enabled and TdxCheckListBoxReportLink(AReportLink).CheckListBox.ItemEnabled[ARow];
    FlatBorder := TdxCheckListBoxReportLink(AReportLink).IsFlatCheckMarks;
  end;
end;

{ TdxCheckListBoxReportLink }

procedure TdxCheckListBoxReportLink.Assign(Source: TPersistent);
begin
  if Source is TdxCheckListBoxReportLink then
    Options := TdxCheckListBoxReportLink(Source).Options;
  inherited;
end;

procedure TdxCheckListBoxReportLink.InternalRestoreDefaults;
begin
  inherited;
  Options := dxDefaultCheckListBoxPaintOptions;
end;

procedure TdxCheckListBoxReportLink.InternalRestoreFromOriginal;
begin
  inherited;
  if CheckListBox.Flat then
    Options := Options + [chlbxpoFlatCheckMarks]
  else
    Options := Options - [chlbxpoFlatCheckMarks];
end;

function TdxCheckListBoxReportLink.IsDrawBorder: Boolean;
begin
  Result := chlbxpoBorder in Options;
end;

function TdxCheckListBoxReportLink.IsDrawHorzLines: Boolean;
begin
  Result := chlbxpoHorzLines in Options;
end;

procedure TdxCheckListBoxReportLink.SetDrawMode(Value: TdxGridDrawMode);
begin
  if Value > gdmOddEven then Value := gdmOddEven;
  inherited SetDrawMode(Value);
end;

function TdxCheckListBoxReportLink.GetCheckListBox: TCheckListBox;
begin
  Result := TCheckListBox(Component);
end;

function TdxCheckListBoxReportLink.GetDataItemClass(ACol: Integer; ARow: Integer = 0): TdxReportCellDataClass;
begin
  Result := TdxReportCellCheck;
end;

function TdxCheckListBoxReportLink.IsFlatCheckMarks: Boolean;
begin
  Result := chlbxpoFlatCheckMarks in Options;
end;

function TdxCheckListBoxReportLink.GetOptions: TdxCheckListBoxPaintOptions;
begin
  Result := FOptions;
end;

procedure TdxCheckListBoxReportLink.SetOptions(Value: TdxCheckListBoxPaintOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    LinkModified(True);
  end;
end;

{ TdxChlbxReportLinkDesignWindow }

constructor TdxChlbxReportLinkDesignWindow.Create(AOwner: TComponent);
begin
  HelpContext := dxhcCheckListBoxReportLinkDesigner;
  inherited;
  CreateControls;
  FPreviewFont := TFont.Create;
end;

destructor TdxChlbxReportLinkDesignWindow.Destroy;
begin
  FreeAndNil(FPreviewFont);
  inherited;
end;

procedure TdxChlbxReportLinkDesignWindow.CreateControls;
begin
  FPreviewBox := TdxPSPaintPanel.Create(Self);
  FPreviewBox.Parent := pnlPreview;
  FPreviewBox.Align := alClient;
  TdxPSPaintPanel(FPreviewBox).EdgeInner := esNone;
  TdxPSPaintPanel(FPreviewBox).EdgeOuter := esNone;
  TdxPSPaintPanel(FPreviewBox).OnPaint := pbxPreviewPaint;
end;

function TdxChlbxReportLinkDesignWindow.GetReportLink: TdxCheckListBoxReportLink;
begin
  Result := inherited ReportLink as TdxCheckListBoxReportLink;
end;

procedure TdxChlbxReportLinkDesignWindow.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  for I := 0 to pcMain.Count - 1 do
    if IsAccel(Message.CharCode, pcMain.Items[I].Caption) then
    begin
      Message.Result := 1;
      pcMain.ItemIndex := I;
      Exit;
    end;
end;

procedure TdxChlbxReportLinkDesignWindow.LoadGroupsIcons;
begin
  inherited LoadGroupsIcons;
  dxLoadIconFromResourceEx(imgGrid, IDB_DXPSGROUPICON_SHOW);
  dxLoadIconFromResourceEx(Image1, IDB_DXPSGROUPICON_MISCELLANEOUS);
end;

procedure TdxChlbxReportLinkDesignWindow.LoadStrings;
begin
  inherited;
  tshOptions.Caption := cxGetResourceString(@sdxOptions);
  tshFont.Caption := cxGetResourceString(@sdxFonts);
  tshColor.Caption := cxGetResourceString(@sdxColors);
  lblPreview.Caption := DropAmpersand(cxGetResourceString(@sdxPreview));

  lblShow.Caption := cxGetResourceString(@sdxShow);
  chbxShowBorders.Caption := cxGetResourceString(@sdxBorderLines);
  chbxShowHorzLines.Caption := cxGetResourceString(@sdxHorzLines);

  lblMiscellaneous.Caption := cxGetResourceString(@sdxMiscellaneous);
  chbxFlatCheckMarks.Caption := cxGetResourceString(@sdxFlatCheckMarks);
  chbxAutoWidth.Caption := cxGetResourceString(@sdxAutoWidth);
  chbxRowAutoHeight.Caption := cxGetResourceString(@sdxRowAutoHeight);

  lblDrawMode.Caption := cxGetResourceString(@sdxDrawMode);
  dxPSInitalizeDrawModeCombo(cbxDrawMode, [gdmStrict, gdmOddEven]);

  stTransparent.Caption := ' ' + cxGetResourceString(@sdxTransparent) + ' ';
  lblColor.Caption := cxGetResourceString(@sdxColor);
  lblEvenColor.Caption := cxGetResourceString(@sdxEvenColor);
  lblGridLinesColor.Caption := cxGetResourceString(@sdxGridLinesColor);

  btnFont.Caption := cxGetResourceString(@sdxBtnFont);
  btnEvenFont.Caption := cxGetResourceString(@sdxBtnEvenFont);
end;

procedure TdxChlbxReportLinkDesignWindow.UpdateControlsState;
begin
  inherited UpdateControlsState;
  ccbxColor.Enabled := not chbxTransparent.Checked;
  lblColor.Enabled := ccbxColor.Enabled;
  ccbxEvenColor.Enabled := not chbxTransparent.Checked and
    (ReportLink.DrawMode in [gdmOddEven, gdmChess]);
  lblEvenColor.Enabled := ccbxEvenColor.Enabled;

  btnEvenFont.Enabled := ReportLink.DrawMode in [gdmOddEven, gdmChess];
  if ReportLink.DrawMode in [gdmOddEven, gdmChess] then
  begin
    lblColor.Caption := cxGetResourceString(@sdxOddColor);
    btnFont.Caption := cxGetResourceString(@sdxBtnOddFont);
  end
  else
  begin
    lblColor.Caption := cxGetResourceString(@sdxColor);
    btnFont.Caption := cxGetResourceString(@sdxBtnFont);
  end;
end;

procedure TdxChlbxReportLinkDesignWindow.DoInitialize;
begin
  inherited DoInitialize;

  dxPSSyncDrawModeComboItemIndex(cbxDrawMode, ReportLink.DrawMode);
  chbxShowBorders.Checked := chlbxpoBorder in ReportLink.Options;
  chbxShowHorzLines.Checked := chlbxpoHorzLines in ReportLink.Options;
  chbxFlatCheckMarks.Checked := chlbxpoFlatCheckMarks in ReportLink.Options;
  chbxAutoWidth.Checked := ReportLink.AutoWidth;
  chbxRowAutoHeight.Checked := ReportLink.RowAutoHeight;

  chbxTransparent.Checked := ReportLink.Transparent;
  ccbxColor.ColorValue := ReportLink.Color;
  ccbxEvenColor.ColorValue := ReportLink.EvenColor;
  ccbxGridLineColor.ColorValue := ReportLink.GridLineColor;

  FontInfoToText(ReportLink.Font, edFont);
  FontInfoToText(ReportLink.EvenFont, edEvenFont);
end;

function TdxChlbxReportLinkDesignWindow.GetPreviewHost: TCustomPanel;
begin
  Result := pnlPreview;
end;

procedure TdxChlbxReportLinkDesignWindow.PaintPreview(ACanvas: TCanvas; R: TRect);
const
  CheckBorderMap: array[Boolean] of UINT = (0, DFCS_FLAT);
  ControlStateMap: array[TCheckBoxState] of UINT =
    (DFCS_BUTTONCHECK, DFCS_BUTTONCHECK or DFCS_CHECKED, DFCS_BUTTON3STATE or DFCS_CHECKED);
  CheckStateMap: array[0..DesignerStringCount - 1] of TCheckBoxState =
    (cbUnchecked, cbChecked, cbGrayed, cbUnchecked, cbChecked, cbChecked);
var
  DC: HDC;
  Brush: HBRUSH;
  I, dY: Integer;
  R2: TRect;
  PrevBkMode: Integer;
  PrevFont: HFONT;
  PrevFontColor: COLORREF;
  uState: UINT;
  S: string;
begin
  inherited;
  DC := ACanvas.Handle;
  //FillRect(DC, R, HBRUSH(COLOR_WINDOW + 1));
  InflateRect(R, -ScaleFactor.Apply(4), -ScaleFactor.Apply(4));
  R2 := R;
  dY := (R.Bottom - R.Top) div DesignerStringCount;

  Brush := SelectObject(DC, CreateSolidBrush(ColorToRGB(ReportLink.GridLineColor)));
  for I := 0 to DesignerStringCount do
    if (((I = 0) or (I = DesignerStringCount)) and ReportLink.IsDrawBorder) or
      ((I > 0) and (I < DesignerStringCount) and ReportLink.IsDrawHorzLines) then
    begin
      R := Rect(R2.Left + 1, R2.Top + I * dY, R2.Right - 1, R2.Top + I * dY + 1);
      PatBlt(DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, PATCOPY);
    end;

  if ReportLink.IsDrawBorder then
  begin
    R := Rect(R2.Left, R2.Top, R2.Left + 1, R2.Top + dY * DesignerStringCount + 1);
    PatBlt(DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, PATCOPY);
    R := Rect(R2.Right - 1, R2.Top, R2.Right, R2.Top + dY * DesignerStringCount + 1);
    PatBlt(DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, PATCOPY);
  end;
  DeleteObject(SelectObject(DC, Brush));

  PrevBkMode := SetBkMode(DC, Windows.TRANSPARENT);
  PrevFont := GetCurrentObject(DC, OBJ_FONT);
  PrevFontColor := GetTextColor(DC);
  for I := 0 to DesignerStringCount - 1 do
  begin
    R := Rect(R2.Left + 1, R2.Top + I * dY + 1, R2.Right - 1, R2.Top + (I + 1) * dY +
      Byte(not ReportLink.IsDrawHorzLines and (I < DesignerStringCount - 1)));
    if not ReportLink.Transparent then
    begin
      Brush := CreateSolidBrush(ColorToRGB(ReportLink.GetCellColor(0, I)));
      FillRect(DC, R, Brush);
      DeleteObject(Brush);
    end;
    InflateRect(R, -ScaleFactor.Apply(2), -ScaleFactor.Apply(2));
    Inc(R.Left, ScaleFactor.Apply(CheckWidth + 2));

    dxAssignFont(FPreviewFont, ReportLink.GetCellFont(0, I), ScaleFactor, ReportLink.ScaleFactor);
    SelectObject(DC, FPreviewFont.Handle);
    SetTextColor(DC, ColorToRGB(FPreviewFont.Color));
    S := dxCheckListBoxStrings[I];
    Windows.DrawText(DC, PChar(S), Length(S), R, DT_NOPREFIX or DT_SINGLELINE or
      dxDrawTextTextAlignX[ReportLink.TextAlignX] or dxDrawTextTextAlignY[ReportLink.TextAlignY]);

    R := Bounds(R2.Left + ScaleFactor.Apply(2), R2.Top + I * dY + 1 + (dY - ScaleFactor.Apply(CheckHeight)) div 2,
      ScaleFactor.Apply(CheckWidth), ScaleFactor.Apply(CheckHeight));
    uState := DFCS_TRANSPARENT or ControlStateMap[CheckStateMap[I]] or CheckBorderMap[ReportLink.IsFlatCheckMarks];
    DrawFrameControl(DC, R, DFC_BUTTON, uState);
  end;
  SetTextColor(DC, PrevFontColor);
  SelectObject(DC, PrevFont);
  SetBkMode(DC, PrevBkMode);
end;

procedure TdxChlbxReportLinkDesignWindow.chbxShowBordersClick(Sender: TObject);
var
  Option: TdxCheckListBoxPaintOption;
begin
  if LockControlsUpdate then Exit;

  Option := TdxCheckListBoxPaintOption(TTagToInt(TcxCheckBox(Sender).Tag));
  with ReportLink do
    if TcxCheckBox(Sender).Checked then
      Options := Options + [Option]
    else
      Options := Options - [Option];

  Modified := True;
  UpdatePreview;
end;

procedure TdxChlbxReportLinkDesignWindow.chbxAutoWidthClick(
  Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.AutoWidth := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TdxChlbxReportLinkDesignWindow.chbxRowAutoHeightClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.RowAutoHeight := TcxCheckBox(Sender).Checked;
  Modified := True;
  UpdatePreview;
end;

procedure TdxChlbxReportLinkDesignWindow.cbxDrawModeClick(Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    ReportLink.DrawMode := dxPSGetSelectedDrawMode(cbxDrawMode);
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TdxChlbxReportLinkDesignWindow.chbxTransparentClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.Transparent := TcxCheckBox(Sender).Checked;
  Modified := True;
  UpdatePreview;
end;

procedure TdxChlbxReportLinkDesignWindow.stTransparentClick(Sender: TObject);
begin
  if chbxTransparent.CanFocus then
    ActiveControl := chbxTransparent;
  chbxTransparent.Checked := not chbxTransparent.Checked;
end;

procedure TdxChlbxReportLinkDesignWindow.UpdatePreview;
begin
  FPreviewBox.Invalidate;
end;

procedure TdxChlbxReportLinkDesignWindow.ccbxColorChange(Sender: TObject);
var
  AColor: TColor;
begin
  if LockControlsUpdate then Exit;

  AColor := TcxColorComboBox(Sender).ColorValue;
  case TTagToInt(TcxColorComboBox(Sender).Tag) of
    0: ReportLink.Color := AColor;
    1: ReportLink.EvenColor := AColor;
    2: ReportLink.GridLineColor := AColor;
  end;
  Modified := True;
  UpdatePreview;
end;

procedure TdxChlbxReportLinkDesignWindow.btnFontClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;

  with dxPSGlbl.FontDialog do
  begin
    case TTagToInt(TComponent(Sender).Tag) of
      0: Font := ReportLink.Font;
      1: Font := ReportLink.EvenFont;
    end;
    if Execute then
    begin
      case TTagToInt(TComponent(Sender).Tag) of
        0:
          begin
            ReportLink.Font := Font;
            FontInfoToText(ReportLink.Font, edFont);
          end;
        1:
          begin
            ReportLink.EvenFont := Font;
            FontInfoToText(ReportLink.EvenFont, edEvenFont);
          end;
      end;
      Modified := True;
      UpdatePreview;
    end;
  end;
end;

procedure TdxChlbxReportLinkDesignWindow.pbxPreviewPaint(Sender: TObject);
begin
  with TdxPSPaintPanel(Sender) do
    PaintPreview(Canvas, ClientRect);
end;

procedure TdxChlbxReportLinkDesignWindow.pnlPreviewResize(Sender: TObject);
begin
  if FPreviewBox = nil then
    Exit;

  FItemCount := 5;
  FRectWidth := FPreviewBox.Width - ScaleFactor.Apply(15);
  FRectHeight := (FPreviewBox.Height - ScaleFactor.Apply(15)) div FItemCount;
  FPaintWidth := FRectWidth + 1;
  FPaintHeight := FItemCount * (FRectHeight + 1);
end;

initialization
  TdxPSCheckGridCellDataMap.Register;
  dxPSRegisterReportLink(TdxCheckListBoxReportLink, TCheckListBox, TdxChlbxReportLinkDesignWindow);

finalization
  dxPSUnregisterReportLink(TdxCheckListBoxReportLink, TCheckListBox, TdxChlbxReportLinkDesignWindow);
  TdxPSCheckGridCellDataMap.Unregister;

end.

