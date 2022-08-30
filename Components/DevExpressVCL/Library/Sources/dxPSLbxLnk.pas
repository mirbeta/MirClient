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

unit dxPSLbxLnk;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Types,
  ComCtrls, ExtCtrls, checklst, ImgList, dxCore, dxPSGlbl, dxPSCore, dxPSBaseGridLnk,
  dxPSGrLnks, cxControls, cxContainer, cxEdit, cxCheckBox, Menus, cxLookAndFeelPainters,
  cxButtons, cxTextEdit, cxLabel, cxPC, cxGraphics, cxMaskEdit, cxDropDownEdit,
  cxColorComboBox, cxLookAndFeels, cxImageComboBox, dxPSReportLinkDesignWindow,
  dxLayoutControlAdapters, dxLayoutLookAndFeels, dxLayoutContainer, cxClasses, dxLayoutControl,
  dxLayoutcxEditAdapters, dxExtCtrls, cxImage;

type
  TdxListBoxPaintOption = (lbxpoBorder, lbxpoHorzLines);
  TdxListBoxPaintOptions = set of TdxListBoxPaintOption;

  TCustomdxListBoxReportLinkControl = class(TdxCustomListBoxReportLink)
  private
    FOptions: TdxListBoxPaintOptions;
    FPaintItemsGraphics: Boolean;
    FTransparentGraphics: Boolean;
    function GetOptions: TdxListBoxPaintOptions;
    procedure SetOptions(Value: TdxListBoxPaintOptions);
    procedure SetPaintItemsGraphics(Value: Boolean);
    procedure SetTransparentGraphics(Value: Boolean);
  protected
    procedure AssignData(ACol, ARow: Integer; ADataItem: TAbstractdxReportCellData); override;
    function GetDataItemClass(ACol: Integer; ARow: Integer = 0): TdxReportCellDataClass; override;
    procedure InternalRestoreDefaults; override;
    function IsDrawBorder: Boolean; override;
    function IsDrawHorzLines: Boolean; override;
    procedure SetDrawMode(Value: TdxGridDrawMode); override;

    function GetCellHasImage(ACol, ARow: Integer): Boolean; override;
    function GetCellImage(ACol, ARow: Integer): TGraphic; override;
    function GetCellImageTransparent(ACol, ARow: Integer): Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;

    property Options: TdxListBoxPaintOptions read GetOptions write SetOptions default [lbxpoBorder..lbxpoHorzLines];
    property PaintItemsGraphics: Boolean read FPaintItemsGraphics write SetPaintItemsGraphics default False;
    property TransparentGraphics: Boolean read FTransparentGraphics write SetTransparentGraphics default False;
  end;

  TdxListBoxReportLink = class(TCustomdxListBoxReportLinkControl)
  private
    function GetListBox: TListBox;
  public
    property ListBox: TListBox read GetListBox;
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
    property Options;
    property Multiline;
    property PaintItemsGraphics;
    property RowAutoHeight;
    property ScaleFonts;
    property SupportedCustomDraw;
    property Transparent;
    property TransparentGraphics;
    property UseCustomPageBreaks;
    property UseHorzDelimiters;
    property UseVertDelimiters;
    property Width;

    property OnCustomDrawItem;
    property OnGetCustomPageBreaks;
    property OnInitializeItem;
  end;

  TdxLBxReportLinkDesignWindow = class(TStandarddxReportLinkDesignWindow)
    ilPreview: TcxImageList;
    lblPreview: TdxLayoutItem;
    pnlPreview: TPanel;
    dxLayoutItem1: TdxLayoutItem;
    lblShow: TcxLabel;
    dxLayoutItem2: TdxLayoutItem;
    chbxShowBorders: TcxCheckBox;
    dxLayoutItem3: TdxLayoutItem;
    chbxShowHorzLines: TcxCheckBox;
    dxLayoutItem4: TdxLayoutItem;
    chbxPaintItemGraphics: TcxCheckBox;
    dxLayoutItem5: TdxLayoutItem;
    chbxTransparentGraphics: TcxCheckBox;
    pcMain: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    tshOptions: TdxLayoutGroup;
    dxLayoutItem6: TdxLayoutItem;
    imgGrid: TcxImage;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutItem7: TdxLayoutItem;
    PaintBox1: TPaintBox;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    tshColor: TdxLayoutGroup;
    lblDrawMode: TdxLayoutItem;
    cbxDrawMode: TcxImageComboBox;
    dxLayoutItem8: TdxLayoutItem;
    chbxTransparent: TcxCheckBox;
    dxLayoutItem9: TdxLayoutItem;
    stTransparent: TcxLabel;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    lblColor: TdxLayoutItem;
    ccbxColor: TcxColorComboBox;
    lblEvenColor: TdxLayoutItem;
    ccbxEvenColor: TcxColorComboBox;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    lblGridLinesColor: TdxLayoutItem;
    ccbxGridLineColor: TcxColorComboBox;
    tshFont: TdxLayoutGroup;
    dxLayoutItem10: TdxLayoutItem;
    btnFont: TcxButton;
    dxLayoutItem11: TdxLayoutItem;
    edFont: TcxTextEdit;
    dxLayoutItem12: TdxLayoutItem;
    btnEvenFont: TcxButton;
    dxLayoutItem13: TdxLayoutItem;
    edEvenFont: TcxTextEdit;
    tshBehaviors: TdxLayoutGroup;
    dxLayoutItem14: TdxLayoutItem;
    lblSelection: TcxLabel;
    dxLayoutItem15: TdxLayoutItem;
    Image1: TcxImage;
    dxLayoutItem16: TdxLayoutItem;
    chbxOnlySelected: TcxCheckBox;
    dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup;
    dxLayoutItem17: TdxLayoutItem;
    lblMiscellaneous: TcxLabel;
    dxLayoutItem18: TdxLayoutItem;
    imgMiscellaneous: TcxImage;
    dxLayoutItem19: TdxLayoutItem;
    chbxAutoWidth: TcxCheckBox;
    dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup;
    dxLayoutItem20: TdxLayoutItem;
    chbxRowAutoHeight: TcxCheckBox;
    dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup;
    procedure chbxOnlySelectedClick(Sender: TObject);
    procedure ccbxColorChange(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure pbxPreviewPaint(Sender: TObject);
    procedure chbxPaintItemGraphicsClick(Sender: TObject);
    procedure chbxTransparentClick(Sender: TObject);
    procedure lblComboClick(Sender: TObject);
    procedure chbxShowBordersClick(Sender: TObject);
    procedure cbxDrawModeClick(Sender: TObject);
    procedure chbxRowAutoHeightClick(Sender: TObject);
    procedure chbxTransparentGraphicsClick(Sender: TObject);
    procedure chbxAutoWidthClick(Sender: TObject);
    procedure stTransparentClick(Sender: TObject);
    procedure pnlPreviewResize(Sender: TObject);
  private
    FItemCount: Integer;
    FPaintHeight: Integer;
    FPaintWidth: Integer;
    FPreviewBox: TdxPSPaintPanel;
    FPreviewFont: TFont;
    FRectHeight: Integer;
    FRectWidth: Integer;

    procedure CreateControls;
    function GetReportLink: TCustomdxListBoxReportLinkControl;
    procedure CMDialogChar(var message: TCMDialogChar); message CM_DIALOGCHAR;
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

    property ReportLink: TCustomdxListBoxReportLinkControl read GetReportLink;
  end;

const
  dxDefaultListBoxPaintOptions: TdxListBoxPaintOptions =
    [Low(TdxListBoxPaintOption)..High(TdxListBoxPaintOption)];

  dxListBoxStrings: array[0..5] of string =
    ('Sample Text Row #1', 'Sample Text Row #2', 'Sample Text Row #3',
     'Sample Text Row #4', 'Sample Text Row #5', 'Sample Text Row #6');

implementation

{$R *.DFM}

uses
  CommCtrl, dxPSRes, dxPrnDev, dxPSUtl, dxPSImgs, cxGeometry, dxDPIAwareUtils;

{ CustomListBox Helpers }

function ListBoxGetMultiSelect(ACustomListBox: TCustomListBox): Boolean;
begin
  Result := ACustomListBox.MultiSelect;
end;

 { TCustomdxListBoxReportLinkControl }

procedure TCustomdxListBoxReportLinkControl.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCustomdxListBoxReportLinkControl then
  begin
    Options := TCustomdxListBoxReportLinkControl(Source).Options;
    PaintItemsGraphics := TCustomdxListBoxReportLinkControl(Source).PaintItemsGraphics;
  end;
end;

procedure TCustomdxListBoxReportLinkControl.AssignData(ACol, ARow: Integer;
  ADataItem: TAbstractdxReportCellData);
var
  Obj: TObject;
begin
  inherited;
  if ADataItem is TdxReportCellImage then
    with TdxReportCellImage(ADataItem) do
    begin
      ImageTransparent := TransparentGraphics;
      MakeSpaceForEmptyImage := True;
      Obj := CustomListBox.Items.Objects[ARow];
      if Obj is TGraphic then
        Image := TGraphic(Obj);
    end;
end;

function TCustomdxListBoxReportLinkControl.GetDataItemClass(ACol: Integer;
  ARow: Integer = 0): TdxReportCellDataClass;
begin
  if PaintItemsGraphics then
    Result := TdxReportCellImage
  else
    Result := inherited GetDataItemClass(ACol, ARow);
end;

procedure TCustomdxListBoxReportLinkControl.InternalRestoreDefaults;
begin
  inherited;
  Options := dxDefaultListBoxPaintOptions; {[Low(TdxListBoxPaintOptions)..High(TdxListBoxPaintOptions)]}
  PaintItemsGraphics := False;
  TransparentGraphics := False;
end;

function TCustomdxListBoxReportLinkControl.IsDrawBorder: Boolean;
begin
  Result := lbxpoBorder in Options;
end;

function TCustomdxListBoxReportLinkControl.IsDrawHorzLines: Boolean;
begin
  Result := lbxpoHorzLines in Options;
end;

procedure TCustomdxListBoxReportLinkControl.SetDrawMode(Value: TdxGridDrawMode);
begin
  if Value > gdmOddEven then Value := gdmOddEven;
  inherited SetDrawMode(Value);
end;

function TCustomdxListBoxReportLinkControl.GetCellHasImage(ACol, ARow: Integer): Boolean;
begin
  if PaintItemsGraphics then
    try
      Result := CustomListBox.Items.Objects[ARow] is TGraphic;
    except
      Result := False;
    end
  else
    Result := False;
end;

function TCustomdxListBoxReportLinkControl.GetCellImage(ACol, ARow: Integer): TGraphic;
begin
  Result := TGraphic(CustomListBox.Items.Objects[ARow]);
end;

function TCustomdxListBoxReportLinkControl.GetCellImageTransparent(ACol, ARow: Integer): Boolean;
begin
  Result := TransparentGraphics;
end;

function TCustomdxListBoxReportLinkControl.GetOptions: TdxListBoxPaintOptions;
begin
  Result := FOptions;
end;

procedure TCustomdxListBoxReportLinkControl.SetOptions(Value: TdxListBoxPaintOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    LinkModified(True);
  end;
end;

procedure TCustomdxListBoxReportLinkControl.SetPaintItemsGraphics(Value: Boolean);
begin
  if FPaintItemsGraphics <> Value then
  begin
    FPaintItemsGraphics := Value;
    LinkModified(True);
  end;
end;

procedure TCustomdxListBoxReportLinkControl.SetTransparentGraphics(Value: Boolean);
begin
  if FTransparentGraphics <> Value then
  begin
    FTransparentGraphics := Value;
    LinkModified(True);
  end;
end;

{ TdxListBoxReportLink }

function TdxListBoxReportLink.GetListBox: TListBox;
begin
  Result := inherited Component as TListBox;
end;

{ TdxLBxReportLinkDesignWindow }

constructor TdxLBxReportLinkDesignWindow.Create(AOwner: TComponent);
begin
  HelpContext := dxhcListBoxReportLinkDesigner;
  inherited;
  CreateControls;
  FPreviewFont := TFont.Create;
end;

destructor TdxLBxReportLinkDesignWindow.Destroy;
begin
  FreeAndNil(FPreviewFont);
  inherited;
end;

procedure TdxLBxReportLinkDesignWindow.CreateControls;
begin
  FPreviewBox := TdxPSPaintPanel.Create(Self);
  FPreviewBox.Parent := pnlPreview;
  FPreviewBox.Align := alClient;
  FPreviewBox.EdgeInner := esNone;
  FPreviewBox.EdgeOuter := esNone;
  FPreviewBox.OnPaint := pbxPreviewPaint;
end;

function TdxLBxReportLinkDesignWindow.GetPreviewHost: TCustomPanel;
begin
  Result := pnlPreview;
end;

procedure TdxLBxReportLinkDesignWindow.LoadGroupsIcons;
begin
  inherited LoadGroupsIcons;
  dxLoadIconFromResourceEx(imgGrid, IDB_DXPSGROUPICON_SHOW);
  dxLoadIconFromResourceEx(Image1, IDB_DXPSGROUPICON_SELECTION);
  dxLoadIconFromResourceEx(imgMiscellaneous, IDB_DXPSGROUPICON_SIZE);
end;

procedure TdxLBxReportLinkDesignWindow.LoadStrings;
begin
  inherited;
  tshOptions.Caption := cxGetResourceString(@sdxOptions);
  tshFont.Caption := cxGetResourceString(@sdxFonts);
  tshColor.Caption := cxGetResourceString(@sdxColors);
  tshBehaviors.Caption := cxGetResourceString(@sdxBehaviors);
  lblPreview.Caption := DropAmpersand(cxGetResourceString(@sdxPreview));

  lblShow.Caption := cxGetResourceString(@sdxShow);
  chbxShowBorders.Caption := cxGetResourceString(@sdxBorderLines);
  chbxShowHorzLines.Caption := cxGetResourceString(@sdxHorzLines);
  chbxPaintItemGraphics.Caption := cxGetResourceString(@sdxPaintItemsGraphics);
  chbxTransparentGraphics.Caption := cxGetResourceString(@sdxTransparentGraphics);

  lblDrawMode.Caption := cxGetResourceString(@sdxDrawMode);
  dxPSInitalizeDrawModeCombo(cbxDrawMode, [gdmStrict, gdmOddEven]);

  stTransparent.Caption := ' ' + cxGetResourceString(@sdxTransparent) + ' ';
  lblColor.Caption := cxGetResourceString(@sdxColor);
  lblEvenColor.Caption := cxGetResourceString(@sdxEvenColor);
  lblGridLinesColor.Caption := cxGetResourceString(@sdxGridLinesColor);

  btnFont.Caption := cxGetResourceString(@sdxBtnFont);
  btnEvenFont.Caption := cxGetResourceString(@sdxBtnEvenFont);

  lblSelection.Caption := cxGetResourceString(@sdxSelection);
  chbxOnlySelected.Caption := cxGetResourceString(@sdxOnlySelected);
  lblMiscellaneous.Caption := cxGetResourceString(@sdxMiscellaneous);
  chbxAutoWidth.Caption := cxGetResourceString(@sdxAutoWidth);
  chbxRowAutoHeight.Caption := cxGetResourceString(@sdxRowAutoHeight);
end;

function TdxLBxReportLinkDesignWindow.GetReportLink: TCustomdxListBoxReportLinkControl;
begin
  Result := inherited ReportLink as TCustomdxListBoxReportLinkControl;
end;

procedure TdxLBxReportLinkDesignWindow.CMDialogChar(var message: TCMDialogChar);
var
  I: Integer;
begin
  inherited;
  for I := 0 to pcMain.Count - 1 do
    if IsAccel(message.CharCode, pcMain.Items[I].Caption) then
    begin
      message.Result := 1;
      pcMain.ItemIndex := I;
      Exit;
    end;
end;

procedure TdxLBxReportLinkDesignWindow.UpdateControlsState;

  function IsPaintItemsGraphics: Boolean;
  var
    I: Integer;
    Obj: TObject;
  begin
    Result := True;
    with ReportLink.CustomListBox do
      for I := 0 to Items.Count - 1 do
      begin
        Obj := Items.Objects[I];
        try
          if Obj is TGraphic then Exit;
        except
        end;
      end;
    Result := False;
  end;

begin
  inherited UpdateControlsState;

  ccbxColor.Enabled := not chbxTransparent.Checked;
  lblColor.Enabled := ccbxColor.Enabled;
  ccbxEvenColor.Enabled := not chbxTransparent.Checked and
    (ReportLink.DrawMode in [gdmOddEven, gdmChess]);
  lblEvenColor.Enabled := ccbxEvenColor.Enabled;

  chbxOnlySelected.Enabled :=
    (ReportLink.CustomListBox = nil) or ListBoxGetMultiSelect(ReportLink.CustomListBox);
  chbxPaintItemGraphics.Enabled :=
    (ReportLink.CustomListBox = nil) or IsPaintItemsGraphics;
  chbxTransparentGraphics.Enabled := chbxPaintItemGraphics.Checked;
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
  btnEvenFont.Enabled := ReportLink.DrawMode in [gdmOddEven, gdmChess];
//    (ReportLink.ListBox.Style in [lbOwnerDrawVariable, lbOwnerDrawFixed]);
end;

procedure TdxLBxReportLinkDesignWindow.DoInitialize;
begin
  inherited DoInitialize;
  dxPSSyncDrawModeComboItemIndex(cbxDrawMode, ReportLink.DrawMode);

  chbxShowBorders.Checked := lbxpoBorder in ReportLink.Options;
  chbxShowHorzLines.Checked := lbxpoHorzLines in ReportLink.Options;
  chbxPaintItemGraphics.Checked := ReportLink.PaintItemsGraphics;
  chbxTransparentGraphics.Checked := ReportLink.TransparentGraphics;

  chbxTransparent.Checked := ReportLink.Transparent;
  ccbxColor.ColorValue := ReportLink.Color;
  ccbxEvenColor.ColorValue := ReportLink.EvenColor;
  ccbxGridLineColor.ColorValue := ReportLink.GridLineColor;

  FontInfoToText(ReportLink.Font, edFont);
  FontInfoToText(ReportLink.EvenFont, edEvenFont);

  chbxOnlySelected.Checked := ReportLink.OnlySelected;
  chbxAutoWidth.Checked := ReportLink.AutoWidth;
  chbxRowAutoHeight.Checked := ReportLink.RowAutoHeight;
end;

procedure TdxLBxReportLinkDesignWindow.PaintPreview(ACanvas: TCanvas; R: TRect);
const
  C = 6;
var
  DC: hDC;
  Brush: HBRUSH;
  I, dY, Offset: Integer;
  R2: TRect;
  PrevBkMode: Integer;
  PrevFont: HFONT;
  PrevFontColor: COLORREF;
  S: string;
begin
  inherited;
  DC := ACanvas.Handle;
  //FillRect(DC, R, HBRUSH(COLOR_WINDOW + 1));
  InflateRect(R, -ScaleFactor.Apply(4), -ScaleFactor.Apply(4));
  R2 := R;
  dY := (R.Bottom - R.Top) div C;
  Brush := SelectObject(DC, CreateSolidBrush(ColorToRGB(ReportLink.GridLineColor)));
  for I := 0 to C do
    if (((I = 0) or (I = C)) and ReportLink.IsDrawBorder) or ((I > 0) and (I < C) and ReportLink.IsDrawHorzLines) then
    begin
      R := Rect(R2.Left + 1, R2.Top + I * dY, R2.Right - 1, R2.Top + I * dY + 1);
      PatBlt(DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, PATCOPY);
    end;
  if ReportLink.IsDrawBorder then
  begin
    R := Rect(R2.Left, R2.Top, R2.Left + 1, R2.Top + dY * C + 1);
    PatBlt(DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, PATCOPY);
    R := Rect(R2.Right - 1, R2.Top, R2.Right, R2.Top + dY * C + 1);
    PatBlt(DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, PATCOPY);
  end;
  DeleteObject(SelectObject(DC, Brush));
  PrevBkMode := SetBkMode(DC, Windows.TRANSPARENT);
  PrevFont := GetCurrentObject(DC, OBJ_FONT);
  PrevFontColor := GetTextColor(DC);
  for I := 0 to C - 1 do
  begin
    R := Rect(R2.Left + 1, R2.Top + I * dY + 1, R2.Right - 1, R2.Top + (I + 1) * dY +
      Ord(not ReportLink.IsDrawHorzLines and (I < C - 1)));
    if not ReportLink.Transparent then
    begin
      Brush := CreateSolidBrush(ColorToRGB(ReportLink.GetCellColor(0, I)));
      FillRect(DC, R, Brush);
      DeleteObject(Brush);
    end;
    Offset := ScaleFactor.Apply(1);
    if ReportLink.PaintItemsGraphics then
    begin
      Inc(R.Left, Offset);
      cxPaintCanvas.BeginPaint(ACanvas);
      try
        cxDrawImage(cxPaintCanvas, cxRectCenterVertically(cxRectSetWidth(R, ScaleFactor.Apply(ilPreview.Width)),
          ScaleFactor.Apply(ilPreview.Height)), nil, ilPreview, 0, Enabled, nil, ScaleFactor);
      finally
        cxPaintCanvas.EndPaint;
      end;
      Inc(Offset, ScaleFactor.Apply(ilPreview.Width + 1));
    end;
    Inc(R.Left, Offset);
    InflateRect(R, -ScaleFactor.Apply(2), -ScaleFactor.Apply(2));
    dxAssignFont(FPreviewFont, ReportLink.GetCellFont(0, I), ScaleFactor, ReportLink.ScaleFactor);
    SelectObject(DC, FPreviewFont.Handle);
    SetTextColor(DC, ColorToRGB(FPreviewFont.Color));
    S := dxListBoxStrings[I];
    Windows.DrawText(DC, PChar(S), Length(S), R, DT_NOPREFIX or DT_SINGLELINE or
      dxDrawTextTextAlignX[ReportLink.TextAlignX] or dxDrawTextTextAlignY[ReportLink.TextAlignY]);
  end;
  SetTextColor(DC, PrevFontColor);
  SelectObject(DC, PrevFont);
  SetBkMode(DC, PrevBkMode);
end;

procedure TdxLBxReportLinkDesignWindow.chbxOnlySelectedClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.OnlySelected := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TdxLBxReportLinkDesignWindow.chbxPaintItemGraphicsClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.PaintItemsGraphics := TcxCheckBox(Sender).Checked;
  Modified := True;
  FPreviewBox.Invalidate;
end;

procedure TdxLBxReportLinkDesignWindow.chbxTransparentGraphicsClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.TransparentGraphics := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TdxLBxReportLinkDesignWindow.chbxAutoWidthClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.AutoWidth := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TdxLBxReportLinkDesignWindow.chbxRowAutoHeightClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.RowAutoHeight := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TdxLBxReportLinkDesignWindow.cbxDrawModeClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.DrawMode := dxPSGetSelectedDrawMode(cbxDrawMode);
  Modified := True;
  UpdatePreview;
end;

procedure TdxLBxReportLinkDesignWindow.chbxShowBordersClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  with TcxCheckBox(Sender) do
    if Checked then
      ReportLink.Options := ReportLink.Options + [TdxListBoxPaintOption(TTagToInt(Tag))]
    else
      ReportLink.Options := ReportLink.Options - [TdxListBoxPaintOption(TTagToInt(Tag))];
  Modified := True;
  UpdatePreview;
end;

procedure TdxLBxReportLinkDesignWindow.chbxTransparentClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.Transparent := TcxCheckBox(Sender).Checked;
  Modified := True;
  UpdatePreview;
end;

procedure TdxLBxReportLinkDesignWindow.stTransparentClick(Sender: TObject);
begin
  if chbxTransparent.CanFocus then
    ActiveControl := chbxTransparent;
  chbxTransparent.Checked := not chbxTransparent.Checked;
end;

procedure TdxLBxReportLinkDesignWindow.UpdatePreview;
begin
  FPreviewBox.Invalidate;
end;

procedure TdxLBxReportLinkDesignWindow.ccbxColorChange(Sender: TObject);
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

procedure TdxLBxReportLinkDesignWindow.btnFontClick(Sender: TObject);
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

procedure TdxLBxReportLinkDesignWindow.pbxPreviewPaint(Sender: TObject);
begin
  with TdxPSPaintPanel(Sender) do
    PaintPreview(Canvas, ClientRect);
end;

procedure TdxLBxReportLinkDesignWindow.pnlPreviewResize(Sender: TObject);
begin
  if FPreviewBox <> nil then
  begin
    FItemCount := 5;
    FRectWidth := FPreviewBox.Width - ScaleFactor.Apply(15);
    FRectHeight := (FPreviewBox.Height - ScaleFactor.Apply(15)) div FItemCount;
    FPaintWidth := FRectWidth + 1;
    FPaintHeight := FItemCount * (FRectHeight + 1);
  end;
end;

procedure TdxLBxReportLinkDesignWindow.lblComboClick(Sender: TObject);
begin
  ActivateComboBoxControl(Self, TcxLabel(Sender).FocusControl);
end;

initialization
  dxPSRegisterReportLink(TdxListBoxReportLink, TListBox, TdxLBxReportLinkDesignWindow);

finalization
  dxPSUnregisterReportLink(TdxListBoxReportLink, TListBox, TdxLBxReportLinkDesignWindow);

end.
