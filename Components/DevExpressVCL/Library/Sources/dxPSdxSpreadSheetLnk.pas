{*******************************************************************}
{                                                                   }
{       Developer Express Visual Component Library                  }
{       ExpressPrinting System COMPONENT SUITE                      }
{                                                                   }
{       Copyright (C) 1998-2019 Developer Express Inc.              }
{       ALL RIGHTS RESERVED                                         }
{                                                                   }
{   The entire contents of this file is protected by U.S. and       }
{   International Copyright Laws. Unauthorized reproduction,        }
{   reverse-engineering, and distribution of all or any portion of  }
{   the code contained in this file is strictly prohibited and may  }
{   result in severe civil and criminal penalties and will be       }
{   prosecuted to the maximum extent possible under the law.        }
{                                                                   }
{   RESTRICTIONS                                                    }
{                                                                   }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES           }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE    }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS   }
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTINGSYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                     }
{   EXECUTABLE PROGRAM ONLY.                                        }
{                                                                   }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED      }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE        }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE       }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT  }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                      }
{                                                                   }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON       }
{   ADDITIONAL RESTRICTIONS.                                        }
{                                                                   }
{*******************************************************************}

unit dxPSdxSpreadSheetLnk;

interface

{$I cxVer.inc}

uses
  Types,
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Classes, Graphics, Controls, StdCtrls, ExtCtrls, ComCtrls, Dialogs, dxSpreadSheetCore, dxHashUtils,
  dxSpreadSheet, dxSpreadSheetTypes, dxSpreadSheetGraphics, dxPSCore, dxPSForm, dxPSBaseGridLnk, dxPSExcelEdgePatterns,
  dxPSExcelFillPatterns,  dxPSEdgePatterns, dxPSFillPatterns, cxDrawTextUtils, cxPC, dxPSGlbl, cxControls, cxContainer,
  cxEdit, cxLabel, cxCheckBox, Menus, cxLookAndFeelPainters, cxTextEdit, cxButtons, cxGraphics, cxDropDownEdit, dxCore,
  cxMaskEdit, cxColorComboBox, dxPSReportRenderCanvas, cxLookAndFeels, dxSpreadSheetUtils, cxClasses,
  dxPSReportLinkDesignWindow,dxLayoutControlAdapters, dxLayoutLookAndFeels, dxLayoutContainer, dxLayoutControl,
  dxLayoutcxEditAdapters, dxSpreadSheetStyles, cxImage, dxPScxCommon, dxSpreadSheetConditionalFormattingRules,
  dxPSdxSpreadSheetLnkCore;

type

  { TdxSpreadSheetCustomReportLink }

  TdxPSSpreadSheetReportLinkOptionView = (ssovRowAndColumnHeadings, ssovGridLines, ssovSuppressSourceFormats);
  TdxPSSpreadSheetReportLinkOptionsView = set of TdxPSSpreadSheetReportLinkOptionView;

  TdxSpreadSheetCustomReportLink = class(TdxSpreadSheetAbstractReportLink)
  private
    FOptionsView: TdxPSSpreadSheetReportLinkOptionsView;
    FPrintArea: TRect;

    procedure SetOptionsView(Value: TdxPSSpreadSheetReportLinkOptionsView);
    procedure SetPrintArea(Value: TRect);
  protected
    procedure DoChangeComponent; override;
    procedure InternalRestoreDefaults; override;
    procedure InternalRestoreFromOriginal; override;

    function GetSelectionRect: TRect; override;
    function HasSelection: Boolean; override;

    function CanPrintContainer(AContainer: TdxSpreadSheetContainer): Boolean; override;
    procedure CheckPrintAreaBounds(var R: TRect);
    function IsShowGridLines: Boolean; override;
    function IsShowRowAndColumnHeadings: Boolean; override;
    function IsSuppressSourceFormats: Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;

    procedure ClearPrintArea;
    function DataProviderPresent: Boolean; override;
    function DataToPrintExist: Boolean; virtual;
    function PrintAreaExists: Boolean; virtual;

    property PrintArea: TRect read FPrintArea write SetPrintArea;
  published
    property Color;
    property Effects3D;
    property FixedColor;
    property FixedFont;
    property FixedTransparent;
    property Font;
    property GridLineColor;
    property HeadersOnEveryPage;
    property OnlySelected;
    property OptionsView: TdxPSSpreadSheetReportLinkOptionsView read FOptionsView write SetOptionsView default [ssovRowAndColumnHeadings];
    property RowAutoHeight;
    property ScaleFonts;
    property Soft3D;
    property Transparent;
    property UseCustomPageBreaks;
    property UseHorzDelimiters;
    property UseVertDelimiters;

    property OnGetCustomPageBreaks;
    property OnInitializeItem;
  end;

  TdxSpreadSheetReportLnk = class(TdxSpreadSheetCustomReportLink);

  TfmdxSpreadSheet2DesignWindow = class(TStandarddxReportLinkDesignWindow)
    btnFixedFont: TcxButton;
    btnFont: TcxButton;
    ccbxColor: TcxColorComboBox;
    ccbxFixedColor: TcxColorComboBox;
    ccbxLineColor: TcxColorComboBox;
    chbxFixedRowsOnEveryPage: TcxCheckBox;
    chbxFixedTransparent: TcxCheckBox;
    chbxOnlySelected: TcxCheckBox;
    chbxRowAutoHeight: TcxCheckBox;
    chbxShowGridlines: TcxCheckBox;
    chbxShowRowAndColumnHeadings: TcxCheckBox;
    chbxSuppressSourceFormats: TcxCheckBox;
    chbxTransparent: TcxCheckBox;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem14: TdxLayoutItem;
    dxLayoutItem15: TdxLayoutItem;
    dxLayoutItem16: TdxLayoutItem;
    dxLayoutItem17: TdxLayoutItem;
    dxLayoutItem18: TdxLayoutItem;
    dxLayoutItem19: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem20: TdxLayoutItem;
    dxLayoutItem21: TdxLayoutItem;
    dxLayoutItem22: TdxLayoutItem;
    dxLayoutItem23: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    edFixedFont: TcxTextEdit;
    edFont: TcxTextEdit;
    Image1: TcxImage;
    Image2: TcxImage;
    Image3: TcxImage;
    imgGrid: TcxImage;
    lblColor: TdxLayoutItem;
    lblFixedColor: TdxLayoutItem;
    lblGridLinesColor: TdxLayoutItem;
    lblMiscellaneous: TcxLabel;
    lblOnEveryPage: TcxLabel;
    lblPreview: TdxLayoutItem;
    lblSelection: TcxLabel;
    lblShow: TcxLabel;
    pcMain: TdxLayoutGroup;
    pnlPreview: TPanel;
    stFixedTransparent: TcxLabel;
    stTransparent: TcxLabel;
    tshBehaviors: TdxLayoutGroup;
    tshColor: TdxLayoutGroup;
    tshFont: TdxLayoutGroup;
    tshOptions: TdxLayoutGroup;

    procedure btnFixedFontClick(Sender: TObject);
    procedure ccbxColorChange(Sender: TObject);
    procedure chbxFixedRowsOnEveryPageClick(Sender: TObject);
    procedure chbxFixedTransparentClick(Sender: TObject);
    procedure chbxOnlySelectedClick(Sender: TObject);
    procedure chbxOptionsViewChanged(Sender: TObject);
    procedure chbxRowAutoHeightClick(Sender: TObject);
    procedure pbxPreviewPaint(Sender: TObject);
    procedure stFixedTransparentClick(Sender: TObject);
    procedure stTransparentClick(Sender: TObject);
  private
    FPreviewBox: TCustomControl;
    function GetReportLink: TdxSpreadSheetCustomReportLink;
    procedure CreateControls;
    procedure SetReportLink(Value: TdxSpreadSheetCustomReportLink);
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
    property ReportLink: TdxSpreadSheetCustomReportLink read GetReportLink write SetReportLink;
  end;

const
  dxPSEmptySSPrintArea: TRect = (Left: -1; Top: -1; Right: -1; Bottom: -1);

  BorderMap: array[TdxCellSide] of TcxBorder = (bLeft, bTop, bRight, bBottom);

implementation

{$R *.DFM}

uses
  SysUtils, Forms, Math,
  dxPSRes, dxPSUtl, dxExtCtrls, dxPrnDev, dxPSImgs, cxGeometry, RTLConsts, dxCoreClasses, dxSpreadSheetClasses,
  dxSpreadSheetContainers, dxSpreadSheetConditionalFormattingIconSet, dxSpreadSheetCoreStyles,
  dxPSdxSpreadSheetDocumentBasedLnk;

{ TdxSpreadSheetCustomReportLink }

procedure TdxSpreadSheetCustomReportLink.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxSpreadSheetCustomReportLink then
    with TdxSpreadSheetCustomReportLink(Source) do
    begin
      Self.OptionsView := OptionsView;
      Self.PrintArea := PrintArea;
    end;
end;

procedure TdxSpreadSheetCustomReportLink.ClearPrintArea;
begin
  PrintArea := dxPSEmptySSPrintArea;
end;

function TdxSpreadSheetCustomReportLink.DataProviderPresent: Boolean;
begin
  Result := inherited DataProviderPresent and DataToPrintExist;
end;

function TdxSpreadSheetCustomReportLink.DataToPrintExist: Boolean;
begin
  Result := (MeaningColCount > 0) and (MeaningRowCount > 0);
end;

function TdxSpreadSheetCustomReportLink.PrintAreaExists: Boolean;
begin
  Result := not EqualRect(PrintArea, dxPSEmptySSPrintArea);
end;

procedure TdxSpreadSheetCustomReportLink.DoChangeComponent;
begin
  ClearPrintArea;
  inherited DoChangeComponent;
end;

procedure TdxSpreadSheetCustomReportLink.InternalRestoreDefaults;
begin
  inherited InternalRestoreDefaults;
  EndEllipsis := False;
  Soft3D := True;
  FPrintArea := dxPSEmptySSPrintArea;
  OptionsView := [ssovRowAndColumnHeadings];
end;

procedure TdxSpreadSheetCustomReportLink.InternalRestoreFromOriginal;
begin
  inherited InternalRestoreFromOriginal;
  FixedFont := SpreadSheet.Styles.GetHeaderStyle(Sheet).Font;
end;

function TdxSpreadSheetCustomReportLink.GetSelectionRect: TRect;
begin
  if OnlySelected then
    Result := Sheet.Selection.Area
  else
    Result := PrintArea;

  Result := SpreadSheetAreaToReportArea(Result);
end;

function TdxSpreadSheetCustomReportLink.HasSelection: Boolean;
begin
  Result := inherited HasSelection or PrintAreaExists;
end;

function TdxSpreadSheetCustomReportLink.CanPrintContainer(AContainer: TdxSpreadSheetContainer): Boolean;
begin
  Result := inherited CanPrintContainer(AContainer) and not (AContainer is TdxSpreadSheetCommentContainer);
end;

procedure TdxSpreadSheetCustomReportLink.CheckPrintAreaBounds(var R: TRect);
begin
  if not EqualRect(R, dxPSEmptySSPrintArea) then
    with R do
    begin
      if Left < 0 then Left :=0;
      if Top < 0 then Top := 0;
      if Right < Left then Right := Left;
      if Bottom < Top then Bottom := Top;
    end;
end;

function TdxSpreadSheetCustomReportLink.IsShowGridLines: Boolean;
begin
  Result := ssovGridLines in OptionsView;
end;

function TdxSpreadSheetCustomReportLink.IsShowRowAndColumnHeadings: Boolean;
begin
  Result := ssovRowAndColumnHeadings in OptionsView;
end;

function TdxSpreadSheetCustomReportLink.IsSuppressSourceFormats: Boolean;
begin
  Result := ssovSuppressSourceFormats in OptionsView;
end;

procedure TdxSpreadSheetCustomReportLink.SetOptionsView(Value: TdxPSSpreadSheetReportLinkOptionsView);
begin
  if FOptionsView <> Value then
  begin
    FOptionsView := Value;
    if ssovSuppressSourceFormats in FOptionsView then
      DrawMode := gdmStrict
    else
      DrawMode := gdmBorrowSource;
    LinkModified(True);
  end;
end;

procedure TdxSpreadSheetCustomReportLink.SetPrintArea(Value: TRect);
begin
  CheckPrintAreaBounds(Value);
  if not EqualRect(Value, FPrintArea) then
  begin
    FPrintArea := Value;
    LinkModified(True);
  end;
end;

{ TfmdxSpreadSheet2DesignWindow }

constructor TfmdxSpreadSheet2DesignWindow.Create(AOwner: TComponent);
begin
  HelpContext := dxhccxSpreadSheetReportLinkDesigner;
  inherited;
  CreateControls;
end;

procedure TfmdxSpreadSheet2DesignWindow.DoInitialize;
begin
  inherited;
  chbxShowRowAndColumnHeadings.Checked := ssovRowAndColumnHeadings in ReportLink.OptionsView;
  chbxShowGridLines.Checked := ssovGridLines in ReportLink.OptionsView;

//  chbxAutoWidth.Checked := ReportLink.AutoWidth;
  chbxRowAutoHeight.Checked := ReportLink.RowAutoHeight;
  chbxSuppressSourceFormats.Checked := ssovSuppressSourceFormats in ReportLink.OptionsView;

  chbxTransparent.Checked := ReportLink.Transparent;
  ccbxColor.ColorValue := ReportLink.Color;
  chbxFixedTransparent.Checked := ReportLink.FixedTransparent;
  ccbxFixedColor.ColorValue := ReportLink.FixedColor;
  ccbxLineColor.ColorValue := ReportLink.GridLineColor;

  FontInfoToText(ReportLink.Font, edFont);
  FontInfoToText(ReportLink.FixedFont, edFixedFont);

  chbxFixedRowsOnEveryPage.Checked := ReportLink.HeadersOnEveryPage;
  chbxOnlySelected.Checked := ReportLink.OnlySelected;
end;

function TfmdxSpreadSheet2DesignWindow.GetPreviewHost: TCustomPanel;
begin
  Result := pnlPreview;
end;

procedure TfmdxSpreadSheet2DesignWindow.LoadGroupsIcons;
begin
  inherited LoadGroupsIcons;
  dxLoadIconFromResourceEx(imgGrid, IDB_DXPSGROUPICON_SHOW);
  dxLoadIconFromResourceEx(Image2, IDB_DXPSGROUPICON_FRAMING);
  dxLoadIconFromResourceEx(Image3, IDB_DXPSGROUPICON_ONEVERYPAGE);
  dxLoadIconFromResourceEx(Image1, IDB_DXPSGROUPICON_SELECTION);
end;

procedure TfmdxSpreadSheet2DesignWindow.LoadStrings;
begin
  inherited;

  tshOptions.Caption := cxGetResourceString(@sdxOptions);
  tshFont.Caption := cxGetResourceString(@sdxFonts);
  tshColor.Caption := cxGetResourceString(@sdxColors);
  tshBehaviors.Caption := cxGetResourceString(@sdxBehaviors);
  lblPreview.Caption := DropAmpersand(cxGetResourceString(@sdxPreview));

  lblShow.Caption := cxGetResourceString(@sdxShow);
  chbxShowRowAndColumnHeadings.Caption := cxGetResourceString(@sdxShowRowAndColumnHeadings);
  chbxShowGridLines.Caption := cxGetResourceString(@sdxShowGridLines);

  lblMiscellaneous.Caption := cxGetResourceString(@sdxMiscellaneous);
  //chbxAutoWidth.Caption := cxGetResourceString(@sdxAutoWidth);
  chbxRowAutoHeight.Caption := cxGetResourceString(@sdxRowAutoHeight);
  chbxSuppressSourceFormats.Caption := cxGetResourceString(@sdxSuppressSourceFormats);

  stTransparent.Caption := ' ' + cxGetResourceString(@sdxTransparent) + ' ';
  lblColor.Caption := cxGetResourceString(@sdxColor);
  stFixedTransparent.Caption := ' ' + cxGetResourceString(@sdxFixedTransparent) + ' ';
  lblFixedColor.Caption := cxGetResourceString(@sdxFixedColor);
  lblGridLinesColor.Caption := cxGetResourceString(@sdxGridLinesColor);

  btnFont.Caption := cxGetResourceString(@sdxBtnFont);
  btnFixedFont.Caption := cxGetResourceString(@sdxBtnFixedFont);

  lblOnEveryPage.Caption := cxGetResourceString(@sdxOnEveryPage);
  chbxFixedRowsOnEveryPage.Caption := cxGetResourceString(@sdxRepeatHeaderRowAtTop);

  lblSelection.Caption := cxGetResourceString(@sdxSelection);
  chbxOnlySelected.Caption := cxGetResourceString(@sdxOnlySelected);
end;

procedure TfmdxSpreadSheet2DesignWindow.PaintPreview(ACanvas: TCanvas; R: TRect);
begin
  inherited;
  dxPSBaseGridLnk.dxPSDrawGridPreview(ACanvas, R, ReportLink,
    ReportLink.IsShowRowAndColumnHeadings, ReportLink.IsShowRowAndColumnHeadings, ScaleFactor);
end;

procedure TfmdxSpreadSheet2DesignWindow.UpdateControlsState;
begin
  inherited;
  ccbxColor.Enabled := not chbxTransparent.Checked;
  lblColor.Enabled := ccbxColor.Enabled;
  ccbxFixedColor.Enabled := not chbxFixedTransparent.Checked;
  lblFixedColor.Enabled := ccbxFixedColor.Enabled;
  chbxFixedRowsOnEveryPage.Enabled := not ReportLink.IsAggregated;
end;

procedure TfmdxSpreadSheet2DesignWindow.UpdatePreview;
begin
  FPreviewBox.Invalidate;
end;

function TfmdxSpreadSheet2DesignWindow.GetReportLink: TdxSpreadSheetCustomReportLink;
begin
  Result := inherited ReportLink as TdxSpreadSheetCustomReportLink;
end;

procedure TfmdxSpreadSheet2DesignWindow.SetReportLink(Value: TdxSpreadSheetCustomReportLink);
begin
  inherited ReportLink := Value;
end;

procedure TfmdxSpreadSheet2DesignWindow.CreateControls;
begin
  FPreviewBox := TdxPSPaintPanel.Create(pnlPreview);
  with TdxPSPaintPanel(FPreviewBox) do
  begin
    Parent := pnlPreview;
    Align := alClient;
    EdgeInner := esNone;
    EdgeOuter := esNone;
    OnPaint := pbxPreviewPaint;
  end;
end;

procedure TfmdxSpreadSheet2DesignWindow.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  inherited;
  for I := 0 to pcMain.Count - 1 do
    if IsAccel(Message.CharCode, pcMain.Items[I].Caption) then
    begin
      Message.Result := 1;
      pcMain.ItemIndex := I;
      Exit;
    end;
end;

procedure TfmdxSpreadSheet2DesignWindow.pbxPreviewPaint(Sender: TObject);
begin
  with TdxPSPaintPanel(Sender) do
    PaintPreview(Canvas, ClientRect);
end;

procedure TfmdxSpreadSheet2DesignWindow.chbxFixedRowsOnEveryPageClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.HeadersOnEveryPage := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TfmdxSpreadSheet2DesignWindow.chbxOnlySelectedClick(Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.OnlySelected := TcxCheckBox(Sender).checked;
  Modified := True;
end;

procedure TfmdxSpreadSheet2DesignWindow.ccbxColorChange(Sender: TObject);
var
  Color: TColor;
begin
  if LockControlsUpdate then Exit;
  Color := TcxColorComboBox(Sender).ColorValue;
  case TcxColorComboBox(Sender).Tag of
    0: ReportLink.Color := Color;
    1: ReportLink.FixedColor := Color;
    2: ReportLink.GridLineColor := Color;
  end;
  Modified := True;
  UpdatePreview;
end;

procedure TfmdxSpreadSheet2DesignWindow.btnFixedFontClick(Sender: TObject);

  function GetEdit: TcxTextEdit;
  begin
    if TcxButton(Sender).Tag = 0 then
      Result := edFont
    else
      Result := edFixedFont;
  end;

begin
  if LockControlsUpdate then Exit;

  with dxPSGlbl.FontDialog do
  begin
    case TcxButton(Sender).Tag of
      0: Font := ReportLink.Font;
      1: Font := ReportLink.FixedFont;
    end;

    if Execute then
    begin
      case TcxButton(Sender).Tag of
        0: ReportLink.Font := Font;
        1: ReportLink.FixedFont := Font;
      end;
      FontInfoToText(Font, GetEdit);
      Modified := True;
      UpdatePreview;
    end;
  end;
end;

procedure TfmdxSpreadSheet2DesignWindow.chbxRowAutoHeightClick(
  Sender: TObject);
begin
  if LockControlsUpdate then Exit;
  ReportLink.RowAutoHeight := TcxCheckBox(Sender).Checked;
  Modified := True;
end;

procedure TfmdxSpreadSheet2DesignWindow.chbxOptionsViewChanged(Sender: TObject);
var
  AOption: TdxPSSpreadSheetReportLinkOptionView;
begin
  if not LockControlsUpdate then
  begin
    case TcxCheckBox(Sender).Tag of
      0: AOption := ssovRowAndColumnHeadings;
      1: AOption := ssovGridLines;
      2: AOption := ssovSuppressSourceFormats;
      else
        Exit;
    end;

    if TcxCheckBox(Sender).Checked then
      ReportLink.OptionsView := ReportLink.OptionsView + [AOption]
    else
      ReportLink.OptionsView := ReportLink.OptionsView - [AOption];

    Modified := True;
    UpdatePreview;
  end;
end;

procedure TfmdxSpreadSheet2DesignWindow.chbxFixedTransparentClick(Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    case TcxCheckBox(Sender).Tag of
      0: ReportLink.Transparent := TcxCheckBox(Sender).checked;
      1: ReportLink.FixedTransparent := TcxCheckBox(Sender).checked;
    end;
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TfmdxSpreadSheet2DesignWindow.stTransparentClick(Sender: TObject);
begin
  if chbxTransparent.CanFocus then
    ActiveControl := chbxTransparent;
  chbxTransparent.Checked := not chbxTransparent.Checked;
end;

procedure TfmdxSpreadSheet2DesignWindow.stFixedTransparentClick(Sender: TObject);
begin
  if chbxFixedTransparent.CanFocus then
    ActiveControl := chbxFixedTransparent;
  chbxFixedTransparent.Checked := not chbxFixedTransparent.Checked;
end;

initialization
  dxPSRegisterReportLink(TdxSpreadSheetDocumentBasedReportLink, TdxCustomSpreadSheet, nil);
  dxPSRegisterReportLink(TdxSpreadSheetReportLnk, TdxCustomSpreadSheet, TfmdxSpreadSheet2DesignWindow);

finalization
  dxPSUnregisterReportLink(TdxSpreadSheetDocumentBasedReportLink, TdxCustomSpreadSheet, nil);
  dxPSUnregisterReportLink(TdxSpreadSheetReportLnk, TdxCustomSpreadSheet, TfmdxSpreadSheet2DesignWindow);
end.
