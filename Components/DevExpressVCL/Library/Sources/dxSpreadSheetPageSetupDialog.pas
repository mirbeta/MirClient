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

unit dxSpreadSheetPageSetupDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
{$IFDEF DELPHI101BERLIN}
  System.ImageList,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Menus,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxClasses, dxLayoutLookAndFeels, dxLayoutContainer,
  dxLayoutControl, dxLayoutControlAdapters, cxButtons, dxForms, dxSpreadSheetCore, dxLayoutcxEditAdapters, cxContainer,
  cxEdit, cxTextEdit, cxEditRepositoryItems, dxSpreadSheetPrinting, cxLabel, cxCheckBox, cxMaskEdit, cxDropDownEdit,
  ExtCtrls, cxRadioGroup, ImgList, cxImageList, cxSpinEdit, cxGroupBox, dxCustomPreview, dxMeasurementUnits, cxGeometry,
  cxMemo, cxButtonEdit, dxCoreClasses;

type

  { TdxSpreadSheetHeaderFooterMacroPreview }

  TdxSpreadSheetHeaderFooterMacroPreview = class(TdxSpreadSheetAbstractHeaderFooterMacroExpander)
  protected
    class var FSheet: TdxSpreadSheetTableView;

    class function EvalFuncDate(const S: string; var AIndex: Integer; ALength: Integer): string; override;
    class function EvalFuncPageNumber(const S: string; var AIndex: Integer; ALength: Integer): string; override;
    class function EvalFuncPages(const S: string; var AIndex: Integer; ALength: Integer): string; override;
    class function EvalFuncSheetName(const S: string; var AIndex: Integer; ALength: Integer): string; override;
    class function EvalFuncTime(const S: string; var AIndex: Integer; ALength: Integer): string; override;
  public
    class function Evaluate(const S: string; ASheet: TdxSpreadSheetTableView): string;
  end;

  TdxSpreadSheetPageSetupDialogViewMode = (psdvmAuto, psdvmCompact);

  { TdxSpreadSheetPageSetupDialogForm }

  TdxSpreadSheetPageSetupDialogFormClass = class of TdxSpreadSheetPageSetupDialogForm;
  TdxSpreadSheetPageSetupDialogForm = class(TdxForm,
    IdxSpreadSheetListener,
    IdxSpreadSheetSelectionListener,
    IdxSpreadSheetTableViewSelectionModeListener
  )
    beAreaSelector: TcxButtonEdit;
    btnCancel: TcxButton;
    btnCustomHeaderFooter: TcxButton;
    btnOK: TcxButton;
    btnPrint: TcxButton;
    btnPrintPreview: TcxButton;
    cbAlignWithPageMargins: TcxCheckBox;
    cbBlackAndWhite: TcxCheckBox;
    cbCenterHorizontally: TcxCheckBox;
    cbCenterVertically: TcxCheckBox;
    cbDraftQuality: TcxCheckBox;
    cbFooterTemplates: TcxComboBox;
    cbHeaderTemplates: TcxComboBox;
    cbPaperSize: TcxComboBox;
    cbPrintCellErrorsMode: TcxComboBox;
    cbPrintCommentsMode: TcxComboBox;
    cbPrintGridLines: TcxCheckBox;
    cbPrintHeaders: TcxCheckBox;
    cbScaleWithDocument: TcxCheckBox;
    dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup13: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
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
    dxLayoutItem24: TdxLayoutItem;
    dxLayoutItem25: TdxLayoutItem;
    dxLayoutItem26: TdxLayoutItem;
    dxLayoutItem29: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem30: TdxLayoutItem;
    dxLayoutItem31: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    EditRepository: TcxEditRepository;
    ersiMargins: TcxEditRepositorySpinItem;
    ertiArea: TcxEditRepositoryButtonItem;
    gbPagePreviewHolder: TcxGroupBox;
    ilLarge: TcxImageList;
    ilPrintOrders: TcxImageList;
    LayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    lbCenterOnPage: TcxLabel;
    lbOrientation: TcxLabel;
    lbPageOrder: TcxLabel;
    lbPrint: TcxLabel;
    lbPrintTitles: TcxLabel;
    lbScaling: TcxLabel;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lgButtons: TdxLayoutAutoCreatedGroup;
    lgHeaderFooter: TdxLayoutGroup;
    lgMargins: TdxLayoutGroup;
    lgPage: TdxLayoutGroup;
    lgPrintButtons: TdxLayoutGroup;
    lgSheet: TdxLayoutGroup;
    lgTabs: TdxLayoutGroup;
    liAdjustTo: TdxLayoutItem;
    liAreaSelector: TdxLayoutItem;
    liBlackAndWhite: TdxLayoutItem;
    liColumnsToRepeat: TdxLayoutItem;
    liFirstPageNumber: TdxLayoutItem;
    liFooter: TdxLayoutItem;
    liHeader: TdxLayoutItem;
    liMarginBottom: TdxLayoutItem;
    liMarginFooter: TdxLayoutItem;
    liMarginHeader: TdxLayoutItem;
    liMarginLeft: TdxLayoutItem;
    liMarginRight: TdxLayoutItem;
    liMarginTop: TdxLayoutItem;
    liPagesTall: TdxLayoutItem;
    liPagesWide: TdxLayoutItem;
    liPaperSize: TdxLayoutItem;
    liPrintArea: TdxLayoutItem;
    liPrintCellErrorsMode: TdxLayoutItem;
    liPrintCommentsMode: TdxLayoutItem;
    liRowsToRepeat: TdxLayoutItem;
    meFirstPageNumber: TcxMaskEdit;
    pbFooterPreview: TPaintBox;
    pbHeaderPreview: TPaintBox;
    pbxPageOrder: TPaintBox;
    pbxPageOrientation: TPaintBox;
    rbAdjustTo: TcxRadioButton;
    rbFitTo: TcxRadioButton;
    rbPageOrientationLandscape: TcxRadioButton;
    rbPageOrientationPortrait: TcxRadioButton;
    rbtnDownThenOver: TcxRadioButton;
    rbtnOverThenDown: TcxRadioButton;
    seAdjustTo: TcxSpinEdit;
    seMarginBottom: TcxSpinEdit;
    seMarginFooter: TcxSpinEdit;
    seMarginHeader: TcxSpinEdit;
    seMarginLeft: TcxSpinEdit;
    seMarginRight: TcxSpinEdit;
    seMarginTop: TcxSpinEdit;
    sePagesTall: TcxSpinEdit;
    sePagesWide: TcxSpinEdit;
    teColumnsToRepeat: TcxButtonEdit;
    tePrintArea: TcxButtonEdit;
    teRowsToRepeat: TcxButtonEdit;

    procedure beAreaSelectorPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure btnCancelClick(Sender: TObject);
    procedure btnCustomHeaderFooterClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure btnPrintPreviewClick(Sender: TObject);
    procedure cbFooterTemplatesPropertiesChange(Sender: TObject);
    procedure cbHeaderTemplatesPropertiesChange(Sender: TObject);
    procedure ertiAreaPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure ertiAreaPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lgTabsTabChanged(Sender: TObject);
    procedure PageSettingsChanged(Sender: TObject);
    procedure pbFooterPreviewPaint(Sender: TObject);
    procedure pbHeaderPreviewPaint(Sender: TObject);
    procedure pbxPageOrderPaint(Sender: TObject);
    procedure pbxPageOrientationPaint(Sender: TObject);
    procedure rbAdjustToClick(Sender: TObject);
    procedure rbFitToClick(Sender: TObject);
    procedure rbPageOrientationPortraitClick(Sender: TObject);
    procedure rbtnOverThenDownClick(Sender: TObject);
    procedure seAdjustToPropertiesChange(Sender: TObject);
    procedure sePagesWidePropertiesChange(Sender: TObject);
    procedure tePrintAreaEnter(Sender: TObject);
    procedure tePrintAreaExit(Sender: TObject);
  strict private
    FIsCompactView: Boolean;
    FViewMode: TdxSpreadSheetPageSetupDialogViewMode;

    function GetDialogsLookAndFeel: TcxLookAndFeel;
    function GetOptions: TdxSpreadSheetTableViewOptionsPrint;
    procedure SetIsCompactView(AValue: Boolean);
    procedure SetViewMode(AValue: TdxSpreadSheetPageSetupDialogViewMode);
  protected const
    SectionsDelimiter = ';';
  protected
    FDefaultPageMargins: TdxSpreadSheetTableViewOptionsPrintPageMargins;
    FDefaultPageSize: Integer;
    FLoading: Boolean;
    FPageFooter: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText;
    FPageHeader: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText;
    FPagePreview: TdxCustomPreview;
    FSavedSelection: TMemoryStream;
    FSelectionMode: TdxSpreadSheetTableViewSelectionMode;
    FSheet: TdxSpreadSheetTableView;
    FSpreadSheet: TdxCustomSpreadSheet;
    FUnitsConverter: TdxUnitsConverterClass;

    procedure ApplyChanges; virtual;
    procedure ApplyLocalizations; virtual;
    procedure CheckViewMode;
    procedure Finalize; virtual;
    procedure Initialize(ASheet: TdxSpreadSheetTableView); virtual;
    procedure InitializeAreaEditors; virtual;
    procedure InitializeCore; virtual;
    procedure InitializeMeasurementUnits; virtual;
    procedure InitializePageDefaults; virtual;
    procedure InitializeScalingDefaults; virtual;
    procedure PopulateHeaderFooterTemplates(AStrings: TStrings); virtual;
    procedure PopulatePaperSizes; virtual;
    procedure PopulatePrintCellErrorsModes; virtual;
    procedure PopulatePrintCommentsModes; virtual;
    procedure SetHeaderFooterText(const ATemplate: string; AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText); overload;
    procedure SetHeaderFooterText(const L, C, R: string; AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText); overload; virtual;
    procedure UpdateHeaderFooterPreview; virtual;
    procedure UpdatePagePreview; virtual;
    procedure UpdatePrintButtons; virtual;
    procedure UpdateTemplateSelection(ATemplate: TcxComboBox; AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);

    procedure RestoreSelection;
    procedure SaveSelection;

    procedure DrawHeaderFooterPreview(ACanvas: TCanvas; ARect: TRect;
      AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText; AIsHeader: Boolean); virtual;
    procedure DrawPageContent(Sender: TObject; ACanvas: TCanvas; ARect: TRect; APageIndex: Integer); virtual;

    procedure Load; virtual;
    procedure LoadCore; virtual;
    procedure LoadHeaderFooterPage; virtual;
    procedure LoadMarginsPage; virtual;
    procedure LoadPagePage; virtual;
    procedure LoadSheetPage; virtual;
    function CanSaveChanges: Boolean; virtual;
    procedure Save; virtual;
    procedure SaveHeaderFooterPage; virtual;
    procedure SaveMarginsPage; virtual;
    procedure SavePagePage; virtual;
    procedure SaveSheetPage; virtual;

    function AreaToString(const AArea: TdxSpreadSheetTableViewOptionsPrintRect): string; overload;
    function AreaToString(const AArea: TRect): string; overload;
    function TryStringToArea(const S: string; AArea: TdxSpreadSheetTableViewOptionsPrintRect): Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // IdxSpreadSheetListener
    procedure DataChanged(Sender: TdxCustomSpreadSheet);
    // IdxSpreadSheetSelectionListener
    procedure SelectionChanged(Sender: TdxSpreadSheetCustomView);
    // IdxSpreadSheetTableViewSelectionModeListener
    procedure SelectionModeChanged(Sender: TdxSpreadSheetTableView; AMode: TdxSpreadSheetTableViewSelectionMode);

    property DefaultPageMargins: TdxSpreadSheetTableViewOptionsPrintPageMargins read FDefaultPageMargins;
    property DialogsLookAndFeel: TcxLookAndFeel read GetDialogsLookAndFeel;
    property ViewMode: TdxSpreadSheetPageSetupDialogViewMode read FViewMode write SetViewMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Options: TdxSpreadSheetTableViewOptionsPrint read GetOptions;
    property Sheet: TdxSpreadSheetTableView read FSheet;
    property SpreadSheet: TdxCustomSpreadSheet read FSpreadSheet;
  end;

var
  dxSpreadSheetPageSetupDialogClass: TdxSpreadSheetPageSetupDialogFormClass = TdxSpreadSheetPageSetupDialogForm;

procedure HidePageSetupDialog;
procedure ShowPageSetupDialog(ASheet: TdxSpreadSheetTableView; const AActivePage: Integer = -1);
implementation

uses
  dxSpreadSheetDialogStrs, dxCore, dxSpreadSheetUtils, dxSpreadSheetCoreFormulasParser, StrUtils, Math,
  dxPrintUtils, dxTypeHelpers, cxFormats, dxPrinting, dxSpreadSheetPageSetupHeaderFooterDialog, cxVariants,
  dxSpreadSheetCoreHistory;

{$R *.dfm}

type
  TdxCustomPreviewAccess = class(TdxCustomPreview);
  TdxCustomSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TdxSpreadSheetTableViewSelectionAccess = class(TdxSpreadSheetTableViewSelection);

var
  FDialog: TdxSpreadSheetPageSetupDialogForm = nil;

procedure HidePageSetupDialog;
begin
  FreeAndNil(FDialog);
end;

procedure ShowPageSetupDialog(ASheet: TdxSpreadSheetTableView; const AActivePage: Integer = -1);
begin
  if FDialog = nil then
  begin
    FDialog := dxSpreadSheetPageSetupDialogClass.Create(GetParentForm(ASheet.SpreadSheet));
    FDialog.Initialize(ASheet);
    FDialog.Load;
  end;
  if AActivePage >= 0 then
    FDialog.lgTabs.ItemIndex := AActivePage;
  FDialog.Show;
end;


{ TdxSpreadSheetHeaderFooterMacroPreview }

class function TdxSpreadSheetHeaderFooterMacroPreview.Evaluate(const S: string; ASheet: TdxSpreadSheetTableView): string;
begin
  FSheet := ASheet;
  try
    Result := EvaluateCore(S);
  finally
    FSheet := nil;
  end;
end;

class function TdxSpreadSheetHeaderFooterMacroPreview.EvalFuncDate(
  const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  Result := FormatDateTime(dxFormatSettings.ShortDateFormat, Now);
end;

class function TdxSpreadSheetHeaderFooterMacroPreview.EvalFuncPageNumber(
  const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  Result := '1';
end;

class function TdxSpreadSheetHeaderFooterMacroPreview.EvalFuncPages(
  const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  Result := '1';
end;

class function TdxSpreadSheetHeaderFooterMacroPreview.EvalFuncSheetName(
  const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  Result := FSheet.Caption;
end;

class function TdxSpreadSheetHeaderFooterMacroPreview.EvalFuncTime(
  const S: string; var AIndex: Integer; ALength: Integer): string;
begin
  Result := FormatDateTime(dxFormatSettings.ShortTimeFormat, Now);
end;

{ TdxSpreadSheetPageSetupDialogForm }

constructor TdxSpreadSheetPageSetupDialogForm.Create(AOwner: TComponent);
begin
  inherited;
  FSavedSelection := TMemoryStream.Create;
  FPageHeader := TdxSpreadSheetTableViewOptionsPrintHeaderFooterText.Create(nil);
  FPageFooter := TdxSpreadSheetTableViewOptionsPrintHeaderFooterText.Create(nil);
end;

destructor TdxSpreadSheetPageSetupDialogForm.Destroy;
begin
  FreeAndNil(FPageFooter);
  FreeAndNil(FPageHeader);
  FreeAndNil(FDefaultPageMargins);
  FreeAndNil(FSavedSelection);
  inherited;
end;

procedure TdxSpreadSheetPageSetupDialogForm.ApplyChanges;
begin
  if CanSaveChanges then
  begin
    Sheet.History.BeginAction(TdxSpreadSheetHistoryChangePrintingOptionsAction);
    try
      Sheet.History.AddCommand(TdxSpreadSheetHistoryChangePrintingOptionsCommand.Create);
      Options.BeginUpdate;
      try
        Save;
      finally
        Options.EndUpdate;
      end;
    finally
      Sheet.History.EndAction;
    end;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.ApplyLocalizations;
begin
  Caption := cxGetResourceString(@sdxPageSetupDialogCaption);
  btnOK.Caption := cxGetResourceString(@sdxPageSetupDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxPageSetupDialogButtonCancel);
  btnPrint.Caption := cxGetResourceString(@sdxPageSetupDialogButtonPrint);
  btnPrintPreview.Caption := cxGetResourceString(@sdxPageSetupDialogButtonPrintPreview);

  // Page
  lbOrientation.Caption := cxGetResourceString(@sdxPageSetupDialogPageOrientation);
  lbScaling.Caption := cxGetResourceString(@sdxPageSetupDialogScaling);
  lgPage.Caption := cxGetResourceString(@sdxPageSetupDialogPage);
  liAdjustTo.Caption := cxGetResourceString(@sdxPageSetupDialogScalingAdjustToSuffix);
  liFirstPageNumber.Caption := cxGetResourceString(@sdxPageSetupDialogPageFirstPageNumber);
  liPagesTall.Caption := cxGetResourceString(@sdxPageSetupDialogScalingFitToPageTall);
  liPagesWide.Caption := cxGetResourceString(@sdxPageSetupDialogScalingFitToPageWide);
  liPaperSize.Caption := cxGetResourceString(@sdxPageSetupDialogPaperSize);
  rbAdjustTo.Caption := cxGetResourceString(@sdxPageSetupDialogScalingAdjustTo);
  rbFitTo.Caption := cxGetResourceString(@sdxPageSetupDialogScalingFitTo);
  rbPageOrientationPortrait.Caption := cxGetResourceString(@sdxPageSetupDialogPageOrientationPortrait);
  rbPageOrientationLandscape.Caption := cxGetResourceString(@sdxPageSetupDialogPageOrientationLandscape);

  // Margins
  lgMargins.Caption := cxGetResourceString(@sdxPageSetupDialogMargins);
  liMarginBottom.Caption := cxGetResourceString(@sdxPageSetupDialogMarginBottom);
  liMarginFooter.Caption := cxGetResourceString(@sdxPageSetupDialogMarginFooter);
  liMarginHeader.Caption := cxGetResourceString(@sdxPageSetupDialogMarginHeader);
  liMarginLeft.Caption := cxGetResourceString(@sdxPageSetupDialogMarginLeft);
  liMarginRight.Caption := cxGetResourceString(@sdxPageSetupDialogMarginRight);
  liMarginTop.Caption := cxGetResourceString(@sdxPageSetupDialogMarginTop);
  lbCenterOnPage.Caption := cxGetResourceString(@sdxPageSetupDialogCenterOnPage);
  cbCenterHorizontally.Caption := cxGetResourceString(@sdxPageSetupDialogCenterHorizontally);
  cbCenterVertically.Caption := cxGetResourceString(@sdxPageSetupDialogCenterVertically);

  // Header/Footer
  lgHeaderFooter.Caption := cxGetResourceString(@sdxPageSetupDialogHeaderFooter);
  btnCustomHeaderFooter.Caption := cxGetResourceString(@sdxPageSetupDialogButtonCustomHeaderFooter);
  cbAlignWithPageMargins.Caption := cxGetResourceString(@sdxPageSetupDialogAlignWithMargins);
  cbScaleWithDocument.Caption := cxGetResourceString(@sdxPageSetupDialogScaleWithDocument);
  liFooter.Caption := cxGetResourceString(@sdxPageSetupDialogFooter);
  liHeader.Caption := cxGetResourceString(@sdxPageSetupDialogHeader);

  // Sheet
  cbBlackAndWhite.Caption := cxGetResourceString(@sdxPageSetupDialogBlackAndWhite);
  cbDraftQuality.Caption := cxGetResourceString(@sdxPageSetupDialogPrintDraftQuality);
  cbPrintGridLines.Caption := cxGetResourceString(@sdxPageSetupDialogPrintGridLines);
  cbPrintHeaders.Caption := cxGetResourceString(@sdxPageSetupDialogPrintRowAndColumnHeadings);
  lbPrint.Caption := cxGetResourceString(@sdxPageSetupDialogPrint);
  lgSheet.Caption := cxGetResourceString(@sdxPageSetupDialogTabSheetCaption);
  lbPrintTitles.Caption := cxGetResourceString(@sdxPageSetupDialogPrintTitles);
  liPrintArea.Caption := cxGetResourceString(@sdxPageSetupDialogPrintArea);
  liPrintCellErrorsMode.Caption := cxGetResourceString(@sdxPageSetupDialogPrintCellErrorsMode);
  liPrintCommentsMode.Caption := cxGetResourceString(@sdxPageSetupDialogPrintCommentsMode);
  liRowsToRepeat.Caption := cxGetResourceString(@sdxPageSetupDialogPrintTitlesRowsToRepeat);
  liColumnsToRepeat.Caption := cxGetResourceString(@sdxPageSetupDialogPrintTitlesColumnsToRepeat);
  lbPageOrder.Caption := cxGetResourceString(@sdxPageSetupDialogPageOrder);
  rbtnDownThenOver.Caption := cxGetResourceString(@sdxPageSetupDialogDownThenOver);
  rbtnOverThenDown.Caption := cxGetResourceString(@sdxPageSetupDialogOverThenDown);
end;

procedure TdxSpreadSheetPageSetupDialogForm.CheckViewMode;
begin
  SetIsCompactView((beAreaSelector.Tag <> 0) and (
    (ViewMode = psdvmCompact) or (ViewMode = psdvmAuto) and (FSelectionMode <> smNone)));
end;

procedure TdxSpreadSheetPageSetupDialogForm.Finalize;
begin
  SpreadSheet.RemoveListener(Self);
  SpreadSheet.RemoveFreeNotification(Self);
  if (Sheet <> nil) and not TdxSpreadSheetTableViewAccess(Sheet).IsDestroying then
  begin
    Sheet.Controller.ForcedSelectionMode := smNone;
    RestoreSelection;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.Initialize(ASheet: TdxSpreadSheetTableView);
begin
  FSheet := ASheet;
  FSpreadSheet := FSheet.SpreadSheet;
  SaveSelection;
  SetControlLookAndFeel(Self, DialogsLookAndFeel);
  SpreadSheet.FreeNotification(Self);
  SpreadSheet.AddListener(Self);

  FLoading := True;
  try
    InitializeCore;
    ApplyLocalizations;
  finally
    FLoading := False;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.InitializeAreaEditors;
begin
  tePrintArea.Tag := Ord(smCells);
  teRowsToRepeat.Tag := Ord(smRows);
  teColumnsToRepeat.Tag := Ord(smColumns);
end;

procedure TdxSpreadSheetPageSetupDialogForm.InitializeCore;
begin
  InitializeAreaEditors;
  InitializeMeasurementUnits;
  InitializePageDefaults;
  InitializeScalingDefaults;
  PopulateHeaderFooterTemplates(cbHeaderTemplates.Properties.Items);
  PopulateHeaderFooterTemplates(cbFooterTemplates.Properties.Items);
  PopulatePrintCellErrorsModes;
  PopulatePrintCommentsModes;
  PopulatePaperSizes;
  UpdatePrintButtons;
end;

procedure TdxSpreadSheetPageSetupDialogForm.InitializeMeasurementUnits;
var
  AUnitsName: Pointer;
begin
  if dxGetDefaultMeasurementUnits = muInches then
  begin
    AUnitsName := @sdxPageSetupDialogUnitsInches;
    FUnitsConverter := TdxInchesUnits;
    ersiMargins.Properties.Increment := 0.25;
  end
  else
  begin
    AUnitsName := @sdxPageSetupDialogUnitsMillimeters;
    FUnitsConverter := TdxMillimetersUnits;
    ersiMargins.Properties.Increment := 0.5;
  end;
  ersiMargins.Properties.DisplayFormat := Format('0.0 %s', [cxGetResourceString(AUnitsName)]);
end;

procedure TdxSpreadSheetPageSetupDialogForm.InitializePageDefaults;
var
  AFooter: Integer;
  AHeader: Integer;
  AMargins: TRect;
  AOrientation: TdxPrinterOrientation;
  APageSize: TPoint;
begin
  TdxPrintingDefaults.GetDefaultPageInfo(AMargins, AHeader, AFooter, APageSize, FDefaultPageSize, AOrientation, muInches);

  FDefaultPageMargins := TdxSpreadSheetTableViewOptionsPrintPageMargins.Create(nil);
  FDefaultPageMargins.Bottom := AMargins.Bottom / 1000;
  FDefaultPageMargins.Footer := AFooter / 1000;
  FDefaultPageMargins.Header := AHeader / 1000;
  FDefaultPageMargins.Left := AMargins.Left / 1000;
  FDefaultPageMargins.Right := AMargins.Right / 1000;
  FDefaultPageMargins.Top := AMargins.Top / 1000;
end;

procedure TdxSpreadSheetPageSetupDialogForm.InitializeScalingDefaults;
begin
  seAdjustTo.Properties.MaxValue := Options.Page.ScaleMax;
  seAdjustTo.Properties.MinValue := Options.Page.ScaleMin;
  sePagesTall.Properties.MinValue := 1;
  sePagesWide.Properties.MinValue := 1;
end;

procedure TdxSpreadSheetPageSetupDialogForm.PopulatePrintCommentsModes;
var
  AIndex: TdxSpreadSheetTableViewOptionsPrintSourceCellComments;
begin
  for AIndex := Low(AIndex) to High(AIndex) do
    cbPrintCommentsMode.Properties.Items.AddObject(cxGetResourceString(dxPrintCommentsModesNames[AIndex]), TObject(AIndex));
end;

procedure TdxSpreadSheetPageSetupDialogForm.SetHeaderFooterText(
  const ATemplate: string; AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
var
  AParts: TStringDynArray;
begin
  AParts := SplitString(ATemplate, SectionsDelimiter);
  if Length(AParts) >= 3 then
    SetHeaderFooterText(AParts[0], AParts[1], AParts[2], AText)
  else
    SetHeaderFooterText('', '', '', AText);
end;

procedure TdxSpreadSheetPageSetupDialogForm.SetHeaderFooterText(
  const L, C, R: string; AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
begin
  AText.LeftSection := Trim(L);
  AText.RightSection := Trim(R);
  AText.CenterSection := Trim(C);
  AText.Assigned := (L <> '') and (C <> '') and (R <> '');
  UpdateHeaderFooterPreview;
end;

procedure TdxSpreadSheetPageSetupDialogForm.SetIsCompactView(AValue: Boolean);

  procedure Prepare(ATarget, ASource: TcxButtonEdit);
  begin
    ATarget.EditValue := ASource.EditValue;
    if ATarget.CanFocusEx then
      ATarget.SetFocus;
  end;

var
  AEdit: TcxButtonEdit;
begin
  if AValue <> FIsCompactView then
  begin
    lcMain.BeginUpdate;
    try
      FIsCompactView := AValue;

      liAreaSelector.Visible := FIsCompactView;
      lgButtons.Visible := not FIsCompactView;
      lgTabs.Visible := not FIsCompactView;

      AEdit := TObject(beAreaSelector.Tag) as TcxButtonEdit;
      if FIsCompactView then
        Prepare(beAreaSelector, AEdit)
      else
        Prepare(AEdit, beAreaSelector);
    finally
      lcMain.EndUpdate(False);
    end;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.SetViewMode(AValue: TdxSpreadSheetPageSetupDialogViewMode);
begin
  if AValue <> FViewMode then
  begin
    FViewMode := AValue;
    CheckViewMode;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.UpdateHeaderFooterPreview;
begin
  pbHeaderPreview.Invalidate;
  pbFooterPreview.Invalidate;
end;

procedure TdxSpreadSheetPageSetupDialogForm.UpdatePagePreview;

  function ConvertValue(const S: Single): Integer;
  begin
    Result := MillimetersToLoMetric(FUnitsConverter.ToMM(S));
  end;

const
  Map: array[Boolean] of TdxPreviewPaperOrientation = (ppoLandscape, ppoPortrait);
begin
  if FLoading then Exit;

  TdxCustomPreviewAccess(FPagePreview).PageSize.Orientation := Map[rbPageOrientationPortrait.Checked];
  TdxCustomPreviewAccess(FPagePreview).PageSize.Size := TdxPrintingDefaults.Papers[cbPaperSize.ItemIndex].Size;
  TdxCustomPreviewAccess(FPagePreview).PageSize.Header := ConvertValue(seMarginHeader.Value);
  TdxCustomPreviewAccess(FPagePreview).PageSize.Footer := ConvertValue(seMarginFooter.Value);
  TdxCustomPreviewAccess(FPagePreview).PageSize.Margins := Rect(
    ConvertValue(seMarginLeft.Value), ConvertValue(seMarginTop.Value),
    ConvertValue(seMarginRight.Value), ConvertValue(seMarginBottom.Value));
  TdxCustomPreviewAccess(FPagePreview).PagesContentCache.InvalidateAll;
  TdxCustomPreviewAccess(FPagePreview).LayoutChanged;
end;

procedure TdxSpreadSheetPageSetupDialogForm.UpdatePrintButtons;
begin
  lgPrintButtons.Visible := dxPrintingRepository.CanBuildReport(SpreadSheet);
end;

procedure TdxSpreadSheetPageSetupDialogForm.DrawHeaderFooterPreview(ACanvas: TCanvas;
  ARect: TRect; AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText; AIsHeader: Boolean);

  function GetContentRect(const R: TRect): TRect;
  begin
    Result := cxRectContent(R, DialogsLookAndFeel.Painter.PrintPreviewPageBordersScaledWidth(ScaleFactor));
  end;

  function GetSectionWidth(const S: string): Integer;
  begin
    Result := cxTextWidth(ACanvas.Font, TdxSpreadSheetHeaderFooterMacroPreview.Evaluate(S, Sheet));
  end;

  procedure DrawBackground(ACanvas: TcxCanvas; R: TRect);
  begin
    if AIsHeader then
      Inc(R.Bottom, cxRectHeight(R))
    else
      Dec(R.Top, cxRectHeight(R));

    DialogsLookAndFeel.Painter.DrawPrintPreviewPageScaledBackground(ACanvas, R, GetContentRect(R), False, True, ScaleFactor);
  end;

  procedure DrawSection(ACanvas: TcxCanvas; const R: TRect; const S: string; AAlignment: TAlignment);
  begin
    ACanvas.Font.Color := DialogsLookAndFeel.Painter.PrintPreviewBackgroundTextColor;
    ACanvas.DrawTexT(TdxSpreadSheetHeaderFooterMacroPreview.Evaluate(S, Sheet),
      cxRectInflate(R, -ScaleFactor.Apply(cxTextOffset)), AAlignment, vaCenter, False, True);
  end;

var
  ACalculator: TcxAutoWidthObject;
begin
  cxPaintCanvas.BeginPaint(ACanvas);
  try
    DrawBackground(cxPaintCanvas, ARect);

    ARect := GetContentRect(ARect);
    ACalculator := TcxAutoWidthObject.Create(3);
    try
      ACalculator.AvailableWidth := cxRectWidth(ARect);
      ACalculator.AddItem.Width := Max(GetSectionWidth(AText.LeftSection), GetSectionWidth(AText.RightSection));
      ACalculator.AddItem.Width := GetSectionWidth(AText.CenterSection);
      ACalculator.AddItem.Width := ACalculator.Items[0].Width;
      ACalculator.Calculate;

      ARect := cxRectSetWidth(ARect, ACalculator.Items[0].AutoWidth);
      DrawSection(cxPaintCanvas, ARect, AText.LeftSection, taLeftJustify);
      ARect := cxRectSetWidth(ARect, ARect.Right, ACalculator.Items[1].AutoWidth);
      DrawSection(cxPaintCanvas, ARect, AText.CenterSection, taCenter);
      ARect := cxRectSetWidth(ARect, ARect.Right, ACalculator.Items[2].AutoWidth);
      DrawSection(cxPaintCanvas, ARect, AText.RightSection, taRightJustify);
    finally
      ACalculator.Free;
    end;
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.DrawPageContent(
  Sender: TObject; ACanvas: TCanvas; ARect: TRect; APageIndex: Integer);
begin
  dxDrawDefaultPagePreview(ACanvas,
    TdxCustomPreviewAccess(FPagePreview).PageGetContentBounds(ARect),
    cbCenterHorizontally.Checked, cbCenterVertically.Checked, ScaleFactor);
end;

procedure TdxSpreadSheetPageSetupDialogForm.UpdateTemplateSelection(
  ATemplate: TcxComboBox; AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);

  function EscapeString(const S: string): string;
  begin
    Result := StringReplace(Trim(S), #13#10, '', [rfReplaceAll]);
  end;

  function BuildTemplate(const L, C, R: string): string;
  begin
    if (L <> '') or (C <> '') or (R <> '') then
      Result := EscapeString(L) + SectionsDelimiter + EscapeString(C) + SectionsDelimiter + EscapeString(R)
    else
      Result := '';
  end;

var
  ATemplateIndex: Integer;
  ATemplateString: string;
begin
  ATemplateString := BuildTemplate(AText.LeftSection, AText.CenterSection, AText.RightSection);
  ATemplateIndex := ATemplate.Properties.Items.IndexOf(ATemplateString);
  if ATemplateIndex < 0 then
    ATemplateIndex := ATemplate.Properties.Items.Add(ATemplateString);
  ATemplate.ItemIndex := ATemplateIndex;
end;

procedure TdxSpreadSheetPageSetupDialogForm.RestoreSelection;
var
  AReader: TcxReader;
begin
  AReader := TcxReader.Create(FSavedSelection);
  try
    FSavedSelection.Position := 0;
    TdxSpreadSheetTableViewSelectionAccess(Sheet.Selection).LoadFromStream(AReader);
  finally
    AReader.Free;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.SaveSelection;
var
  AWriter: TcxWriter;
begin
  AWriter := TcxWriter.Create(FSavedSelection);
  try
    FSavedSelection.Size := 0;
    TdxSpreadSheetTableViewSelectionAccess(Sheet.Selection).SaveToStream(AWriter);
  finally
    AWriter.Free;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.PopulateHeaderFooterTemplates(AStrings: TStrings);
const
  Templates: array[0..8] of string = (
    '%0:sPage &P%0:s',
    '%0:sPage &P of &N%0:s',
    '%0:s&A%0:s',
    'Confidential%0:s&D%0:sPage &P',
    '%0:s&A%0:sPage &P',
    '&A%0:sConfidential%0:sPage &P',
    '%0:sPage &P%0:s&A',
    '%1:s%0:sPage &P%0:s&D',
    '%0:sPrepared by %1:s &D%0:sPage &P'
  );
var
  AUserName: string;
  I: Integer;
begin
  AUserName := dxGetUserNameEx(dxUserNameDisplay);
  AStrings.BeginUpdate;
  try
    AStrings.Add('');
    for I := Low(Templates) to High(Templates) do
      AStrings.Add(Format(Templates[I], [SectionsDelimiter, AUserName]));
  finally
    AStrings.EndUpdate;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.PopulatePaperSizes;
var
  AItems: TStrings;
  I: Integer;
begin
  AItems := cbPaperSize.Properties.Items;
  AItems.BeginUpdate;
  try
    AItems.Clear;
    for I := 0 to TdxPrintingDefaults.Papers.Count - 1 do
      AItems.Add(TdxPrintingDefaults.Papers[I].Name);
  finally
    AItems.EndUpdate;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.PopulatePrintCellErrorsModes;
var
  AIndex: TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication;
begin
  for AIndex := pseiBlank to High(AIndex) do
    cbPrintCellErrorsMode.Properties.Items.AddObject(cxGetResourceString(dxPrintCellErrorsModesNames[AIndex]), TObject(AIndex));
end;

procedure TdxSpreadSheetPageSetupDialogForm.Load;
begin
  FLoading := True;
  try
    LoadCore;
  finally
    FLoading := False;
    UpdateHeaderFooterPreview;
    UpdatePagePreview;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.LoadCore;
begin
  LoadPagePage;
  LoadMarginsPage;
  LoadHeaderFooterPage;
  LoadSheetPage;
end;

procedure TdxSpreadSheetPageSetupDialogForm.LoadHeaderFooterPage;
begin
  FPageFooter.Assign(Options.HeaderFooter.CommonFooter);
  FPageHeader.Assign(Options.HeaderFooter.CommonHeader);
  cbAlignWithPageMargins.Checked := Options.HeaderFooter.ActualAlignWithMargins;
  cbScaleWithDocument.Checked := Options.HeaderFooter.ActualScaleWithDocument;
  UpdateTemplateSelection(cbFooterTemplates, FPageFooter);
  UpdateTemplateSelection(cbHeaderTemplates, FPageHeader);
end;

procedure TdxSpreadSheetPageSetupDialogForm.LoadMarginsPage;

  function ConvertValue(const S: Single): Single;
  begin
    Result := RoundTo(FUnitsConverter.FromInch(S), -1);
  end;

var
  AMargins: TdxSpreadSheetTableViewOptionsPrintPageMargins;
begin
  if Options.Page.Margins.Assigned then
    AMargins := Options.Page.Margins
  else
    AMargins := FDefaultPageMargins;

  seMarginBottom.Value := ConvertValue(AMargins.Bottom);
  seMarginFooter.Value := ConvertValue(AMargins.Footer);
  seMarginHeader.Value := ConvertValue(AMargins.Header);
  seMarginLeft.Value := ConvertValue(AMargins.Left);
  seMarginRight.Value := ConvertValue(AMargins.Right);
  seMarginTop.Value := ConvertValue(AMargins.Top);

  cbCenterHorizontally.Checked := Options.Printing.ActualHorizontalCentered;
  cbCenterVertically.Checked := Options.Printing.ActualVerticalCentered;
end;

procedure TdxSpreadSheetPageSetupDialogForm.LoadPagePage;

  function GetActualDMPaper: Integer;
  begin
    if Options.Page.Paper.Assigned then
    begin
      if Options.Page.Paper.SizeID > 0 then
        Result := Options.Page.Paper.SizeID
      else
        Result := DMPAPER_USER;
    end
    else
      Result := FDefaultPageSize;
  end;

begin
  case Options.Page.ScaleMode of
    oppsmDefault:
      begin
        rbAdjustTo.Checked := True;
        seAdjustTo.Value := 100;
      end;

    oppsmAdjustToScale:
      begin
        rbAdjustTo.Checked := True;
        seAdjustTo.Value := Options.Page.Scale;
      end;

    oppsmFitToPage:
      begin
        rbFitTo.Checked := True;
        sePagesTall.Value := Options.Page.FitToHeight;
        sePagesWide.Value := Options.Page.FitToWidth;
      end;
  end;

  rbPageOrientationPortrait.Checked := Options.Page.Orientation <> oppoLandscape;
  rbPageOrientationLandscape.Checked := Options.Page.Orientation = oppoLandscape;
  meFirstPageNumber.Text := IfThen(Options.Page.FirstPageNumber > 0, IntToStr(Options.Page.FirstPageNumber));
  cbPaperSize.ItemIndex := TdxPrintingDefaults.Papers.FindByDMPaper(GetActualDMPaper);
end;

procedure TdxSpreadSheetPageSetupDialogForm.LoadSheetPage;
begin
  tePrintArea.Text := AreaToString(Options.Source.Area);
  teRowsToRepeat.Text := AreaToString(Options.Source.RowsToRepeat);
  teColumnsToRepeat.Text := AreaToString(Options.Source.ColumnsToRepeat);
  cbPrintGridLines.Checked := Options.Source.ActualGridLines;
  cbPrintHeaders.Checked := Options.Source.ActualHeaders;
  cbPrintCellErrorsMode.ItemIndex := cbPrintCellErrorsMode.Properties.Items.IndexOfObject(TObject(Options.Source.ActualErrorIndication));
  cbPrintCommentsMode.ItemIndex := cbPrintCommentsMode.Properties.Items.IndexOfObject(TObject(Options.Source.CellComments));
  cbBlackAndWhite.Checked := Options.Printing.ActualBlackAndWhite;
  cbDraftQuality.Checked := Options.Printing.ActualDraft;
  rbtnDownThenOver.Checked := Options.Printing.ActualPageOrder = opppDownThenOver;
  rbtnOverThenDown.Checked := Options.Printing.ActualPageOrder = opppOverThenDown;
end;

function TdxSpreadSheetPageSetupDialogForm.CanSaveChanges: Boolean;
begin
  Result := tePrintArea.ValidateEdit(False);
end;

procedure TdxSpreadSheetPageSetupDialogForm.Save;
begin
  SavePagePage;
  SaveMarginsPage;
  SaveHeaderFooterPage;
  SaveSheetPage;
end;

procedure TdxSpreadSheetPageSetupDialogForm.SaveHeaderFooterPage;
begin
  Options.HeaderFooter.CommonFooter.Assign(FPageFooter);
  Options.HeaderFooter.CommonHeader.Assign(FPageHeader);
  Options.HeaderFooter.AlignWithMargins := dxBooleanToDefaultBoolean(cbAlignWithPageMargins.Checked);
  Options.HeaderFooter.ScaleWithDocument := dxBooleanToDefaultBoolean(cbScaleWithDocument.Checked);
end;

procedure TdxSpreadSheetPageSetupDialogForm.SaveMarginsPage;
begin
  Options.Page.Margins.Bottom := FUnitsConverter.ToInch(seMarginBottom.Value);
  Options.Page.Margins.Footer := FUnitsConverter.ToInch(seMarginFooter.Value);
  Options.Page.Margins.Header := FUnitsConverter.ToInch(seMarginHeader.Value);
  Options.Page.Margins.Left := FUnitsConverter.ToInch(seMarginLeft.Value);
  Options.Page.Margins.Right := FUnitsConverter.ToInch(seMarginRight.Value);
  Options.Page.Margins.Top := FUnitsConverter.ToInch(seMarginTop.Value);
  Options.Printing.HorizontalCentered := dxBooleanToDefaultBoolean(cbCenterHorizontally.Checked);
  Options.Printing.VerticalCentered := dxBooleanToDefaultBoolean(cbCenterVertically.Checked);
end;

procedure TdxSpreadSheetPageSetupDialogForm.SavePagePage;
var
  ASizeID: Integer;
begin
  Options.Page.FirstPageNumber := StrToIntDef(meFirstPageNumber.Text, 0);
  if rbFitTo.Checked then
  begin
    Options.Page.FitToWidth := sePagesWide.Value;
    Options.Page.FitToHeight := sePagesTall.Value;
  end
  else
    Options.Page.Scale := seAdjustTo.Value;

  ASizeID := TdxPrintingDefaults.Papers[cbPaperSize.ItemIndex].DMPaper;
  if ASizeID <> DMPAPER_USER then
    Options.Page.Paper.SizeID := ASizeID;

  if rbPageOrientationLandscape.Checked then
    Options.Page.Orientation := oppoLandscape
  else
    Options.Page.Orientation := oppoDefault;
end;

procedure TdxSpreadSheetPageSetupDialogForm.SaveSheetPage;
const
  PageOrderMap: array[Boolean] of TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder = (opppOverThenDown, opppDownThenOver);
begin
  TryStringToArea(tePrintArea.Text, Options.Source.Area);
  TryStringToArea(teRowsToRepeat.Text, Options.Source.RowsToRepeat);
  TryStringToArea(teColumnsToRepeat.Text, Options.Source.ColumnsToRepeat);
  Options.Source.GridLines := dxBooleanToDefaultBoolean(cbPrintGridLines.Checked);
  Options.Source.Headers := dxBooleanToDefaultBoolean(cbPrintHeaders.Checked);
  Options.Source.CellComments := TdxSpreadSheetTableViewOptionsPrintSourceCellComments(cbPrintCommentsMode.ItemObject);
  Options.Source.ErrorIndication := TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication(cbPrintCellErrorsMode.ItemObject);
  Options.Printing.BlackAndWhite := dxBooleanToDefaultBoolean(cbBlackAndWhite.Checked);
  Options.Printing.Draft := dxBooleanToDefaultBoolean(cbDraftQuality.Checked);
  Options.Printing.PageOrder := PageOrderMap[rbtnDownThenOver.Checked]
end;

function TdxSpreadSheetPageSetupDialogForm.AreaToString(const AArea: TRect): string;
begin
  Result := dxReferenceToString(AArea, SpreadSheet.OptionsView.R1C1Reference)
end;

function TdxSpreadSheetPageSetupDialogForm.AreaToString(const AArea: TdxSpreadSheetTableViewOptionsPrintRect): string;
begin
  if AArea.Assigned then
    Result := AreaToString(AArea.Rect)
  else
    Result := '';
end;

function TdxSpreadSheetPageSetupDialogForm.TryStringToArea(const S: string; AArea: TdxSpreadSheetTableViewOptionsPrintRect): Boolean;
var
  R: TRect;
begin
  if S = '' then
  begin
    AArea.Reset;
    Exit(True);
  end;

  Result := dxTryStringToReferenceArea(S, SpreadSheet.OptionsView.R1C1Reference, R);
  if Result then
  begin
    AArea.Rect := R;
    AArea.Assigned := True;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = SpreadSheet then
      Release;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.DataChanged(Sender: TdxCustomSpreadSheet);
begin
  if TdxCustomSpreadSheetAccess(Sender).FSheets.IndexOf(FSheet) < 0 then
  begin
    FSheet := nil;
    Release;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.SelectionChanged(Sender: TdxSpreadSheetCustomView);
begin
  if Sender = Sheet then
    beAreaSelector.Text := AreaToString(Sheet.Selection.Area);
end;

procedure TdxSpreadSheetPageSetupDialogForm.SelectionModeChanged(
  Sender: TdxSpreadSheetTableView; AMode: TdxSpreadSheetTableViewSelectionMode);
begin
  FSelectionMode := AMode;
  CheckViewMode;
end;

function TdxSpreadSheetPageSetupDialogForm.GetDialogsLookAndFeel: TcxLookAndFeel;
begin
  Result := SpreadSheet.DialogsLookAndFeel;
end;

function TdxSpreadSheetPageSetupDialogForm.GetOptions: TdxSpreadSheetTableViewOptionsPrint;
begin
  Result := Sheet.OptionsPrint;
end;

procedure TdxSpreadSheetPageSetupDialogForm.beAreaSelectorPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  ViewMode := psdvmAuto;
end;

procedure TdxSpreadSheetPageSetupDialogForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TdxSpreadSheetPageSetupDialogForm.btnCustomHeaderFooterClick(Sender: TObject);
var
  AOptions: TdxSpreadSheetTableViewOptionsPrintHeaderFooter;
begin
  AOptions := TdxSpreadSheetTableViewOptionsPrintHeaderFooter.Create(nil);
  try
    AOptions.CommonFooter := FPageFooter;
    AOptions.CommonHeader := FPageHeader;
    if ShowPageHeaderFooterSetupDialog(Sheet, AOptions) then
    begin
      FPageFooter.Assign(AOptions.CommonFooter);
      FPageHeader.Assign(AOptions.CommonHeader);
      UpdateTemplateSelection(cbFooterTemplates, FPageFooter);
      UpdateTemplateSelection(cbHeaderTemplates, FPageHeader);
    end;
  finally
    AOptions.Free;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.btnOKClick(Sender: TObject);
begin
  if CanSaveChanges then
  begin
    ApplyChanges;
    Close;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.btnPrintClick(Sender: TObject);
begin
  ApplyChanges;
  dxPrintingRepository.PrintReport(SpreadSheet);
end;

procedure TdxSpreadSheetPageSetupDialogForm.btnPrintPreviewClick(Sender: TObject);
begin
  ApplyChanges;
  dxPrintingRepository.PreviewReport(SpreadSheet);
end;

procedure TdxSpreadSheetPageSetupDialogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TdxSpreadSheetPageSetupDialogForm.FormCreate(Sender: TObject);
begin
  FPagePreview := TdxCustomPreview.Create(Self);
  FPagePreview.Parent := gbPagePreviewHolder;
  FPagePreview.Align := alClient;
  FPagePreview.PageCount := 1;
  TdxCustomPreviewAccess(FPagePreview).OnDrawPageContent := DrawPageContent;
  TdxCustomPreviewAccess(FPagePreview).ZoomMode := pzmPages;
  TdxCustomPreviewAccess(FPagePreview).Enabled := False;
end;

procedure TdxSpreadSheetPageSetupDialogForm.FormDestroy(Sender: TObject);
begin
  Finalize;
  FDialog := nil;
end;

procedure TdxSpreadSheetPageSetupDialogForm.PageSettingsChanged(Sender: TObject);
begin
  UpdatePagePreview;
end;

procedure TdxSpreadSheetPageSetupDialogForm.cbFooterTemplatesPropertiesChange(Sender: TObject);
begin
  SetHeaderFooterText(cbFooterTemplates.Text, FPageFooter);
end;

procedure TdxSpreadSheetPageSetupDialogForm.cbHeaderTemplatesPropertiesChange(Sender: TObject);
begin
  SetHeaderFooterText(cbHeaderTemplates.Text, FPageHeader);
end;

procedure TdxSpreadSheetPageSetupDialogForm.ertiAreaPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  ViewMode := psdvmCompact;
end;

procedure TdxSpreadSheetPageSetupDialogForm.ertiAreaPropertiesValidate(Sender: TObject;
  var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
var
  AArea: TdxSpreadSheetTableViewOptionsPrintRect;
begin
  AArea := TdxSpreadSheetTableViewOptionsPrintRect.Create(nil);
  try
    Error := not TryStringToArea(DisplayValue, AArea);
  finally
    AArea.Free;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.pbFooterPreviewPaint(Sender: TObject);
begin
  DrawHeaderFooterPreview(pbFooterPreview.Canvas, pbFooterPreview.ClientRect, FPageFooter, False);
end;

procedure TdxSpreadSheetPageSetupDialogForm.pbHeaderPreviewPaint(Sender: TObject);
begin
  DrawHeaderFooterPreview(pbHeaderPreview.Canvas, pbHeaderPreview.ClientRect, FPageHeader, True);
end;

procedure TdxSpreadSheetPageSetupDialogForm.pbxPageOrderPaint(Sender: TObject);
begin
  cxPaintCanvas.BeginPaint(TPaintBox(Sender).Canvas);
  try
    cxDrawImage(cxPaintCanvas, TPaintBox(Sender).ClientRect, nil,
      ilPrintOrders, Ord(rbtnOverThenDown.Checked), True, nil, ScaleFactor);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.pbxPageOrientationPaint(Sender: TObject);
begin
  cxPaintCanvas.BeginPaint(TPaintBox(Sender).Canvas);
  try
    cxDrawImage(cxPaintCanvas, TPaintBox(Sender).ClientRect, nil,
      ilLarge, Ord(rbPageOrientationLandscape.Checked), True, nil, ScaleFactor);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.rbAdjustToClick(Sender: TObject);
begin
  if rbAdjustTo.Focused then
    seAdjustTo.SetFocus;
end;

procedure TdxSpreadSheetPageSetupDialogForm.rbFitToClick(Sender: TObject);
begin
  if rbFitTo.Focused then
    sePagesWide.SetFocus;
end;

procedure TdxSpreadSheetPageSetupDialogForm.rbPageOrientationPortraitClick(Sender: TObject);
begin
  pbxPageOrientation.Invalidate;
  UpdatePagePreview;
end;

procedure TdxSpreadSheetPageSetupDialogForm.rbtnOverThenDownClick(Sender: TObject);
begin
  pbxPageOrder.Invalidate;
end;

procedure TdxSpreadSheetPageSetupDialogForm.seAdjustToPropertiesChange(Sender: TObject);
begin
  if TcxSpinEdit(Sender).Focused then
    rbAdjustTo.Checked := True;
end;

procedure TdxSpreadSheetPageSetupDialogForm.sePagesWidePropertiesChange(Sender: TObject);
begin
  if TcxSpinEdit(Sender).Focused then
    rbFitTo.Checked := True;
end;

procedure TdxSpreadSheetPageSetupDialogForm.lgTabsTabChanged(Sender: TObject);
begin
  lgPrintButtons.Parent := lgTabs.Items[lgTabs.ItemIndex] as TdxCustomLayoutGroup;
end;

procedure TdxSpreadSheetPageSetupDialogForm.tePrintAreaEnter(Sender: TObject);
var
  AArea: TdxSpreadSheetTableViewOptionsPrintRect;
  AEdit: TcxButtonEdit;
begin
  AEdit := Sender as TcxButtonEdit;
  beAreaSelector.Tag := TdxNativeUInt(AEdit);
  Sheet.Controller.ForcedSelectionMode := TdxSpreadSheetTableViewSelectionMode(AEdit.Tag);

  AArea := TdxSpreadSheetTableViewOptionsPrintRect.Create(nil);
  try
    if TryStringToArea(AEdit.EditingValue, AArea) and AArea.Assigned then
      Sheet.Selection.Add(AArea.Rect)
    else
      Sheet.Selection.Clear;
  finally
    AArea.Free;
  end;
end;

procedure TdxSpreadSheetPageSetupDialogForm.tePrintAreaExit(Sender: TObject);
begin
  if not FIsCompactView then
  begin
    beAreaSelector.Tag := 0;
    Sheet.Selection.Clear;
    Sheet.Controller.ForcedSelectionMode := smNone;
  end;
end;

end.
