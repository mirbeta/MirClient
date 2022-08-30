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

unit dxPSdxPDFViewerPrintDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  SysUtils, Forms, Controls, Classes, Menus, ImgList, ExtCtrls, StdCtrls, Types,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxButtons, cxLabel, cxGroupBox, cxContainer, cxEdit,
  cxDropDownEdit, cxTextEdit, cxRadioGroup, cxCheckBox, cxMaskEdit, cxListBox, cxSpinEdit, dxPrnDev, dxPrnDlg, dxPgsDlg,
  dxPSPrVw, dxPSCore, dxLayoutControlAdapters, dxLayoutContainer, dxLayoutcxEditAdapters, dxLayoutLookAndFeels,
  cxClasses, dxLayoutControl, cxImageList;

resourcestring
  sdxPDFViewerPagePlacementAndScaling = 'Placement and &Scaling';
  sdxPDFViewerPrintAsImage = 'Print as Image';

type
  TCustomdxComponentPrinterAccess = class(TCustomdxComponentPrinter);
  TdxPDFOnGetPrintingResult = procedure(APrinted: Boolean) of object;

  { TdxfmPSPDFPrintDialog }

  TdxPDFPrintDialog = class(TdxfmPrintDialog, IdxPSPreviewWindowDialog)
    cbCenterHorz: TcxCheckBox;
    cbCenterVert: TcxCheckBox;
    cbPrintAsImage: TcxCheckBox;
    dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup13: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup15: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    gbxPlacementAndScaling: TdxLayoutGroup;
    gbxPreview: TdxLayoutGroup;
    lblCenterOnPage: TcxLabel;
    lblPercentOfNormalSize: TdxLayoutItem;
    Orientation: TdxLayoutGroup;
    Preview: TdxPSPreviewWindow;
    rbtnAdjustTo: TcxRadioButton;
    rbtnAutoOrientation: TcxRadioButton;
    rbtnFitToWidth: TcxRadioButton;
    rbtnLandscape: TcxRadioButton;
    rbtnPortrait: TcxRadioButton;
    seScaleFactor: TcxSpinEdit;

    procedure btnCancelClick(Sender: TObject);
    procedure cbCenterHorzClick(Sender: TObject);
    procedure cbCenterVertClick(Sender: TObject);
    procedure cbPrintAsImageClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PreviewInitContent(Sender: TObject);
    procedure rbtnAdjustToClick(Sender: TObject);
    procedure rbtnAutoOrientationClick(Sender: TObject);
    procedure rbtnFitToWidthClick(Sender: TObject);
    procedure rbtnLandscapeClick(Sender: TObject);
    procedure rbtnPortraitClick(Sender: TObject);
    procedure seScaleFactorExit(Sender: TObject);
    procedure seScaleFactorPropertiesEditValueChanged(Sender: TObject);
    procedure edPageRangesExit(Sender: TObject);
    procedure rbtnPagesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  strict private
    FIsPageRangesValid: Boolean;
    FUseOwnPrintDlgData: Boolean;
    FOnGetPrintingResult: TdxPDFOnGetPrintingResult;

    function GetComponentPrinter: TCustomdxComponentPrinterAccess;
    procedure DoShrinkToPageWidth;
    procedure UpdateOrientation(AValue: TdxPrinterOrientation);
    procedure UpdatePagesToPrint(const AValue: TIntegerDynArray);
    procedure UpdateScaleFactor(AValue: Integer);
    procedure SyncParameters(AScaleFactor: Integer);
    procedure OnPrintDeviceChangedHandler(ASender: TObject);
  protected
    PrintDlgData: PdxPrintDlgData;
    function DoShowStylePageSetup(AStyle: TBasedxPrintStyle; APageIndex: Integer;
      AShowPreviewBtn, AShowPrintBtn: Boolean; out APreviewBtnClicked, APrintBtnClicked: Boolean): Boolean; override;
    procedure CallOnPageSetup(Sender: TObject; var ADone: Boolean; APreviewBtnClicked, APrintBtnClicked: PBoolean); override;
    procedure DefinePrintStyleDlg(out APreviewBtnClicked: Boolean); override;
    procedure LoadStrings; override;
    procedure Initialize;

    property OnGetPrintingResult: TdxPDFOnGetPrintingResult read FOnGetPrintingResult write FOnGetPrintingResult;
  public
    destructor Destroy; override;
  end;

implementation

uses
  cxGeometry, dxCore, dxCustomPreview, dxPrnPg, dxPSUtl, dxPSGlbl, dxPSdxPDFViewerLnk, dxPSRes, dxPreVw, dxPDFViewer;

{$R *.dfm}

type
  TdxfmPrintDialogAccess = class(TdxfmPrintDialog);
  TdxPSCustomStatusBarAccess = class(TdxPSCustomStatusBar);
  TdxPreviewAccess = class(TdxPreview);
  TdxPrinterPageAccess = class(TdxPrinterPage);
  TdxPSPreviewWindowAccess = class(TdxPSPreviewWindow);
  TdxPDFViewerReportLinkAccess = class(TdxPDFViewerReportLink);
  TdxPDFViewerReportLinkRenderInfoAccess = class(TdxPDFViewerReportLinkRenderInfo);

{ TdxfmPSPDFPrintDialog }

destructor TdxPDFPrintDialog.Destroy;
begin
  if FUseOwnPrintDlgData and (DialogData.FileList <> nil) then
    DialogData.FileList.Free;
  inherited Destroy;
end;

function TdxPDFPrintDialog.DoShowStylePageSetup(AStyle: TBasedxPrintStyle; APageIndex: Integer;
  AShowPreviewBtn, AShowPrintBtn: Boolean; out APreviewBtnClicked, APrintBtnClicked: Boolean): Boolean;
begin
  Result := inherited DoShowStylePageSetup(AStyle, APageIndex, False, False, APreviewBtnClicked, APrintBtnClicked);
end;

procedure TdxPDFPrintDialog.btnCancelClick(Sender: TObject);
begin
  if ModalResult = mrCancel then
    Close;
end;

procedure TdxPDFPrintDialog.CallOnPageSetup(Sender: TObject; var ADone: Boolean;
  APreviewBtnClicked, APrintBtnClicked: PBoolean);
begin
  OnPrintDeviceChangedHandler(Self);
  inherited CallOnPageSetup(Sender, ADone, nil, nil);
  SyncParameters(Preview.ReportLink.PrinterPage.ScaleFactor);
end;

procedure TdxPDFPrintDialog.DefinePrintStyleDlg(out APreviewBtnClicked: Boolean);
begin
  inherited DefinePrintStyleDlg(APreviewBtnClicked);
  APreviewBtnClicked := False;
end;

procedure TdxPDFPrintDialog.LoadStrings;
begin
  inherited LoadStrings;
  gbxPlacementAndScaling.Caption := cxGetResourceString(@sdxPDFViewerPagePlacementAndScaling);
  rbtnAdjustTo.Caption := cxGetResourceString(@sdxAdjustTo);
  rbtnFitToWidth.Caption := cxGetResourceString(@sdxMenuFormatShrinkToPage);
  lblPercentOfNormalSize.Caption := cxGetResourceString(@sdxPercentOfNormalSize);
  lblCenterOnPage.Caption := cxGetResourceString(@sdxCenterOnPage);
  cbCenterHorz.Caption := cxGetResourceString(@sdxHorizontally);
  cbCenterVert.Caption := cxGetResourceString(@sdxVertically);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  gbxPreview.Caption := DropAmpersand(cxGetResourceString(@sdxPreview));
  cbPrintAsImage.Caption := cxGetResourceString(@sdxPDFViewerPrintAsImage);

  rbtnAutoOrientation.Caption := cxGetResourceString(@sdxAutoOrientation);
  rbtnPortrait.Caption := cxGetResourceString(@sdxPortrait);
  rbtnLandscape.Caption := cxGetResourceString(@sdxLandscape);
  Orientation.Caption := cxGetResourceString(@sdxOrientation);
end;

procedure TdxPDFPrintDialog.Initialize;
var
  APrintDlgData: TdxPrintDlgData;
begin
  Position := poOwnerFormCenter;
  Preview.PrinterPage.ApplyToPrintDevice;
  FUseOwnPrintDlgData := Pointer(PrintDlgData) = nil;
  if FUseOwnPrintDlgData then
    GetComponentPrinter.InitializeDefaultPrintDlgData(Preview.ReportLink, APrintDlgData)
  else
    APrintDlgData := PrintDlgData^;
  APrintDlgData.ButtonsVisible := APrintDlgData.ButtonsVisible - [pdbPreview];
  SetupDialog(APrintDlgData);
  FIsPageRangesValid := True;
  PrepareSettings;
end;

function TdxPDFPrintDialog.GetComponentPrinter: TCustomdxComponentPrinterAccess;
begin
  Result := TCustomdxComponentPrinterAccess(Preview.ComponentPrinter);
end;

procedure TdxPDFPrintDialog.cbCenterHorzClick(Sender: TObject);
begin
  if Preview.ReportLink.PrinterPage.CenterOnPageH <> cbCenterHorz.Checked then
  begin
    Preview.ReportLink.PrinterPage.CenterOnPageH := cbCenterHorz.Checked;
    TdxPrinterPageAccess(Preview.ReportLink.PrinterPage).PageParamsChanged([ucScale]);
  end;
end;

procedure TdxPDFPrintDialog.cbCenterVertClick(Sender: TObject);
begin
  if Preview.ReportLink.PrinterPage.CenterOnPageV <> cbCenterVert.Checked then
  begin
    Preview.ReportLink.PrinterPage.CenterOnPageV := cbCenterVert.Checked;
    TdxPrinterPageAccess(Preview.ReportLink.PrinterPage).PageParamsChanged([ucScale]);
  end;
end;

procedure TdxPDFPrintDialog.cbPrintAsImageClick(Sender: TObject);
begin
  (Preview.ReportLink.PrinterPage as TdxPDFViewerPrinterPage).PrintAsImage := cbPrintAsImage.Checked;
end;

procedure TdxPDFPrintDialog.DoShrinkToPageWidth;
begin
  Preview.ReportLink.ShrinkToPageWidth := True;
end;

procedure TdxPDFPrintDialog.edPageRangesExit(Sender: TObject);
var
  APages: TIntegers;
begin
  inherited;
  if FIsPageRangesValid and not FwpPageRanges.State then
  begin
    DecodePageIndexes(edPageRanges.Text, APages);
    UpdatePagesToPrint(TIntegerDynArray(APages));
    FIsPageRangesValid := True;
  end;
end;

procedure TdxPDFPrintDialog.UpdateOrientation(AValue: TdxPrinterOrientation);
var
  ACurrentPageIndex: Integer;
begin
  ACurrentPageIndex := TdxPreviewAccess(Preview.Preview).SelPageIndex;
  Preview.ReportLink.PrinterPage.Orientation := AValue;
  Preview.ReportLink.RebuildReport;
  TdxPreviewAccess(Preview.Preview).SelPageIndex := ACurrentPageIndex;
end;

procedure TdxPDFPrintDialog.UpdatePagesToPrint(const AValue: TIntegerDynArray);
var
  I: Integer;
  ARebuildReport: Boolean;
begin
  ARebuildReport := Length(TdxPDFViewerReportLinkAccess(Preview.ReportLink).PagesToPrint) <> Length(AValue);
  if not ARebuildReport then
    for I := 0 to Length(AValue) - 1 do
    begin
      ARebuildReport := AValue[I] <> TdxPDFViewerReportLinkAccess(Preview.ReportLink).PagesToPrint[I];
      if ARebuildReport then
        Break;
    end;
  if ARebuildReport then
  begin
    TdxPDFViewerReportLinkAccess(Preview.ReportLink).PagesToPrint := AValue;
    Preview.ReportLink.RebuildReport;
  end;
end;

procedure TdxPDFPrintDialog.UpdateScaleFactor(AValue: Integer);
begin
  if Assigned(Preview.ReportLink) then
  begin
    Preview.ReportLink.PrinterPage.BeginUpdate;
    try
      Preview.ReportLink.PrinterPage.ScaleFactor := 10;
      Preview.ReportLink.PrinterPage.ScaleFactor := AValue;
    finally
      Preview.ReportLink.PrinterPage.EndUpdate;
    end;
  end;
end;

procedure TdxPDFPrintDialog.SyncParameters(AScaleFactor: Integer);
begin
  rbtnAdjustTo.Checked := Preview.ReportLink.PrinterPage.ScaleMode = smAdjust;
  rbtnFitToWidth.Checked := Preview.ReportLink.PrinterPage.ScaleMode = smFit;
  rbtnAutoOrientation.Checked := Preview.ReportLink.PrinterPage.Orientation = poAuto;
  rbtnLandscape.Checked := Preview.ReportLink.PrinterPage.Orientation = poLandscape;
  rbtnPortrait.Checked := Preview.ReportLink.PrinterPage.Orientation = poPortrait;
  cbPrintAsImage.Checked := (Preview.ReportLink.PrinterPage as TdxPDFViewerPrinterPage).PrintAsImage;
  seScaleFactor.Value := AScaleFactor;
end;

procedure TdxPDFPrintDialog.OnPrintDeviceChangedHandler(ASender: TObject);
var
  AMode: TdxScaleMode;
begin
  AMode := Preview.PrinterPage.ScaleMode;
  RereadDefaultPrinterPage;
  Preview.PrinterPage.BeginUpdate;
  try
    Preview.PrinterPage.DMPaper := dxPrintDevice.DefaultDMPaper;
    Preview.PrinterPage.ScaleFactor := DefaultPrinterPage.ScaleFactor;
    Preview.PrinterPage.ScaleMode := AMode;
    Preview.PrinterPage.Orientation := dxPrintDevice.Orientation;
    Preview.PrinterPage.MinMargins.Rect := cxNullRect;
    Preview.PrinterPage.FixMarginsOutside;
  finally
    Preview.PrinterPage.EndUpdate;
  end;
end;

procedure TdxPDFPrintDialog.rbtnAdjustToClick(Sender: TObject);
begin
  rbtnFitToWidth.Checked := not rbtnAdjustTo.Checked;
  seScaleFactor.Enabled := rbtnAdjustTo.Checked;
  UpdateScaleFactor(seScaleFactor.Value);
end;

procedure TdxPDFPrintDialog.rbtnAutoOrientationClick(Sender: TObject);
begin
  UpdateOrientation(poAuto);
  rbtnPortrait.Checked := False;
  rbtnLandscape.Checked := False;
end;

procedure TdxPDFPrintDialog.rbtnFitToWidthClick(Sender: TObject);
begin
  rbtnAdjustTo.Checked := not rbtnFitToWidth.Checked;
  seScaleFactor.Enabled := rbtnAdjustTo.Checked;
  if Assigned(Preview.ReportLink) then
    DoShrinkToPageWidth
end;

procedure TdxPDFPrintDialog.rbtnLandscapeClick(Sender: TObject);
begin
  UpdateOrientation(poLandscape);
  rbtnAutoOrientation.Checked := False;
  rbtnPortrait.Checked := False;
end;

procedure TdxPDFPrintDialog.rbtnPagesClick(Sender: TObject);
var
  APages: TIntegerDynArray;
begin
  inherited;
  if rbtnCurrentPage.Checked then
  begin
    SetLength(APages, 1);
    APages[0] := (Preview.ReportLink.Component as TdxPDFCustomViewer).CurrentPageIndex + 1;
  end
  else
    SetLength(APages, 0);
  UpdatePagesToPrint(APages);
  FIsPageRangesValid := True;
end;

procedure TdxPDFPrintDialog.rbtnPortraitClick(Sender: TObject);
begin
  UpdateOrientation(poPortrait);
  rbtnAutoOrientation.Checked := False;
  rbtnLandscape.Checked := False;
end;

procedure TdxPDFPrintDialog.PreviewInitContent(Sender: TObject);
var
  AMode: TdxScaleMode;
begin
  OnPrintersChanged := OnPrintDeviceChangedHandler;
  OnPrintDeviceChanged := OnPrintDeviceChangedHandler;

  Preview.Preview.OptionsHint := [pohShowForMargins, pohShowOnDrag, pohShowOnScroll];
  Preview.StatusBar.Hint := '';
  TdxPSCustomStatusBarAccess(Preview.StatusBar).OnDblClick := nil;
  TdxPSCustomStatusBarAccess(Preview.StatusBar).OnMouseMove := nil;
  Preview.ComponentPrinter.PreviewOptions.Rect := BoundsRect;
  Preview.ThumbnailsSplitter.Visible := False;
  Preview.ThumbnailsPane.Visible := False;
  Preview.Options.ShowMarginBar := False;
  Preview.Options.ShowExplorer := False;
  Preview.Preview.ZoomMode := pzmPages;
  AMode := Preview.ReportLink.PrinterPage.ScaleMode;
  SyncParameters(Preview.ReportLink.PrinterPage.ScaleFactor);
  if AMode = smFit then
    DoShrinkToPageWidth;

  seScaleFactor.Properties.MaxValue := dxPDFViewerMaxZoomFactor;
  seScaleFactor.Properties.MinValue := dxPDFViewerMinZoomFactor;

  cbCenterHorz.Checked := Preview.PrinterPage.CenterOnPageH;
  cbCenterVert.Checked := Preview.PrinterPage.CenterOnPageV;

  seCopies.Value := dxPrintDevice.Copies;
  chbxCollate.Checked := dxPrintDevice.Collate;

  if rbtnPageRanges.Checked then
    edPageRanges.OnExit(nil);
end;

procedure TdxPDFPrintDialog.seScaleFactorExit(Sender: TObject);
begin
  if Assigned(Preview.ReportLink) then
    UpdateScaleFactor(seScaleFactor.Value);
end;

procedure TdxPDFPrintDialog.seScaleFactorPropertiesEditValueChanged(Sender: TObject);
begin
  if Assigned(Preview.ReportLink) then
    UpdateScaleFactor(seScaleFactor.Value);
end;

procedure TdxPDFPrintDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  APages: string;
  APrintingResult: Boolean;
  ARenderInfo: TdxPDFViewerReportLinkRenderInfoAccess;
begin
  inherited;
  FIsPageRangesValid := CanClose;
  if FIsPageRangesValid then
  begin
    APrintingResult := ModalResult = mrOk;
    if APrintingResult then
    begin
      if not FUseOwnPrintDlgData then
        TdxPDFViewerReportLinkAccess(Preview.ReportLink).PrintDlgData.DialogData := DialogData;
      if DialogData.PrintToFile then
        dxPrintDevice.FileName := DialogData.FileName
      else
        dxPrintDevice.FileName := '';
      if DialogData.PageRanges in [prCurrent, prRange] then
      begin
        if DialogData.PageRanges = prCurrent then
          APages := IntToStr(Preview.ReportLink.CurrentPage)
        else
          APages := DialogData.Pages;
        ARenderInfo := TdxPDFViewerReportLinkRenderInfoAccess(
          TdxPDFViewerReportLinkAccess(Preview.ReportLink).RenderInfo as TdxPDFViewerReportLinkRenderInfo);
        GetComponentPrinter.PrintPagesEx(TIntegers(ARenderInfo.PageIndexesToPrint), DialogData.PageNums,
          DialogData.Copies, DialogData.Collate, Preview.ReportLink);
      end
      else
        GetComponentPrinter.PrintEx(DialogData.PageNums, DialogData.Copies, DialogData.Collate, Preview.ReportLink);
      if Assigned(OnGetPrintingResult) then
        FOnGetPrintingResult(APrintingResult);
    end;
  end;
end;

procedure TdxPDFPrintDialog.FormShow(Sender: TObject);
begin
  inherited;
  dxCheckAvailablePrinters;
end;

procedure AddPDFViewerPrintingResourceStrings(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxPDFViewerPagePlacementAndScaling', @sdxPDFViewerPagePlacementAndScaling);
  AProduct.Add('sdxPDFViewerPrintAsImage', @sdxPDFViewerPrintAsImage);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressPrinting System', @AddPDFViewerPrintingResourceStrings);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressPrinting System', @AddPDFViewerPrintingResourceStrings);

end.
