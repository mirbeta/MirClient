unit RibbonRichEditForm;

{$I cxVer.inc}

interface

uses
  Windows, Forms, RichEditControlBase, RecentDocumentController, RibbonRichEditDemoOptions,
  IniFiles, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxRibbonCustomizationForm, dxRibbonSkins, Menus, dxCore, dxCoreClasses, dxGDIPlusAPI,
  dxGDIPlusClasses, dxRichEdit.Types, dxRichEdit.Options, dxRichEdit.Control,
  cxFontNameComboBox, cxDropDownEdit, dxRibbon, dxBar, dxBarApplicationMenu,
  dxScreenTip, Dialogs, dxRichEdit.Actions, dxActions, Classes, ActnList,
  dxBarExtItems, dxRibbonGallery, dxSkinChooserGallery, cxBarEditItem, ImgList,
  Controls, dxRichEdit.Platform.Win.Control, Graphics, ExtCtrls, cxTextEdit,
  cxMemo, StdCtrls, cxButtons, cxScrollBox, dxGallery, dxGalleryControl,
  dxRibbonBackstageViewGalleryControl, dxBevel, cxLabel, cxGroupBox, dxBarBuiltInMenu,
  dxRibbonBackstageView, dxStatusBar, dxRibbonStatusBar, cxClasses, cxTrackBar,
  dxZoomTrackBar, dxHttpIndyRequest, RibbonRichEditMainForm, dxPSGlbl, dxPSUtl,
  dxPSEngn, dxPrnPg, dxBkgnd, dxWrap, dxPrnDev, dxPSCompsProvider,
  dxPSFillPatterns, dxPSEdgePatterns, dxPSPDFExportCore, dxPSPDFExport,
  cxDrawTextUtils, dxPSPrVwStd, dxPSPrVwAdv, dxPSPrVwRibbon,
  dxPScxPageControlProducer, dxPSRichEditControlLnk, dxPScxEditorProducers,
  dxPScxExtEditorProducers, dxPSCore, dxRichEdit.NativeApi, dxRichEdit.DOC;

type
  { TfrmRibbonRichEditMain }

  TfrmRibbonRichEditForm = class(TfrmRibbonRichEditMain)
    rbvBackstageView: TdxRibbonBackstageView;
    bvtsOpen: TdxRibbonBackstageViewTabSheet;
    bvlSpacer1: TBevel;
    bvSpacer2: TBevel;
    bvSpacer7: TBevel;
    gbBackstageViewTabCaption: TcxGroupBox;
    bvSpacer4: TBevel;
    bvSpacer3: TBevel;
    lbbvTabCaption2010: TcxLabel;
    lbbvTabCaption2013: TcxLabel;
    gbLocationsMain: TcxGroupBox;
    gbLocationsPane: TcxGroupBox;
    bvSpacer5: TBevel;
    dxBevel1: TdxBevel;
    bvgcLocations: TdxRibbonBackstageViewGalleryControl;
    bvgcLocationsRecentDocumentsGroup: TdxRibbonBackstageViewGalleryGroup;
    bvgcLocationsRecentDocumentsItem: TdxRibbonBackstageViewGalleryItem;
    bvgcLocationsGroup1: TdxRibbonBackstageViewGalleryGroup;
    bvgcLocationsComputerItem: TdxRibbonBackstageViewGalleryItem;
    gbRecentPathsPane: TcxScrollBox;
    bvSpacer6: TBevel;
    bvgcRecentPaths: TdxRibbonBackstageViewGalleryControl;
    bvgcRecentPathsGroup: TdxRibbonBackstageViewGalleryGroup;
    gbRecentPathsPaneBottom: TcxGroupBox;
    btnBrowsePath: TcxButton;
    gbRecentPathsPaneCurrentFolder: TcxGroupBox;
    lbCurrentFolder: TcxLabel;
    bvgcCurrentFolder: TdxRibbonBackstageViewGalleryControl;
    lbComputer: TcxLabel;
    lbRecentFolders: TcxLabel;
    gbRecentDocumentsPane: TcxScrollBox;
    bvSpacer8: TBevel;
    lbRecentDocuments: TcxLabel;
    bvgcRecentDocuments: TdxRibbonBackstageViewGalleryControl;
    bvtsSaveAs: TdxRibbonBackstageViewTabSheet;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    bvtsAbout: TdxRibbonBackstageViewTabSheet;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    meAbout: TcxMemo;
    bvtsHelp: TdxRibbonBackstageViewTabSheet;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    gbHelpContent: TcxGroupBox;
    Image1: TImage;
    imDot1: TImage;
    imDot2: TImage;
    imDot3: TImage;
    imDot4: TImage;
    imDot5: TImage;
    imDot6: TImage;
    imLogo: TImage;
    lblSupport: TcxLabel;
    cxLabel1: TcxLabel;
    lblClientCenter: TcxLabel;
    lblDownloads: TcxLabel;
    lblDXonWeb: TcxLabel;
    lblHelpBars: TcxLabel;
    lblHelpDocking: TcxLabel;
    lblProducts: TcxLabel;
    lblSupportCenter: TcxLabel;
    procedure rbvBackstageViewPopup(Sender: TObject);
    procedure rbvBackstageViewTabChanged(Sender: TObject);
    procedure btnBrowsePathClick(Sender: TObject);
    procedure bvgcLocationsItemClick(Sender: TObject; AItem: TdxRibbonBackstageViewGalleryItem);
    procedure FormCreate(Sender: TObject);
    procedure bvgcRecentDocumentsItemClick(Sender: TObject; AItem: TdxRibbonBackstageViewGalleryItem);
    procedure bvgcRecentPathsItemClick(Sender: TObject; AItem: TdxRibbonBackstageViewGalleryItem);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RichEditControlDocumentLoaded(Sender: TObject);
  private
    FRecentDocumentsController: TdxRibbonRecentDocumentsController;

    function CreateRecentDocumentsController: TdxRibbonRecentDocumentsController;
    procedure DoBackstageViewTabChanged;
    procedure UpdateColorSchemeRelatedControls;
  protected
    procedure SetColorScheme(const AName: string); override;
    procedure SetRibbonDemoStyle(const AStyle: TRibbonDemoStyle); override;

    property RecentDocumentsController: TdxRibbonRecentDocumentsController read FRecentDocumentsController;
  public
    destructor Destroy; override;
  end;

var
  frmRibbonRichEditForm: TfrmRibbonRichEditForm;

implementation

uses
  SysUtils, Variants, Types, cxGeometry, dxRichEdit.Commands.ChangeProperties,
  dxBarSkinConsts, EBarsUtils, dxCoreGraphics, ShellAPI;

{$R *.dfm}

function GetInitDocumentFileName: string;
begin
  Result := ExtractFilePath(Application.ExeName) + 'InitDocument.ini';
end;

function GetRecentDocumentsFileName: string;
begin
  Result := ExtractFilePath(Application.ExeName) + 'RecentDocuments.ini';
end;

{ TfrmRibbonRichEditMain }

function TfrmRibbonRichEditForm.CreateRecentDocumentsController: TdxRibbonRecentDocumentsController;
begin
  Result := TdxRibbonRecentDocumentsController.Create(bvgcRecentDocuments.Gallery.Groups, 
    bvgcRecentPaths.Gallery.Groups);
end;

destructor TfrmRibbonRichEditForm.Destroy;
begin
//
  inherited;
end;

procedure TfrmRibbonRichEditForm.DoBackstageViewTabChanged;
begin
  if not (csReading in ComponentState) then
  begin
    gbBackstageViewTabCaption.Parent := rbvBackstageView.ActiveTab;
    lbbvTabCaption2010.Caption := rbvBackstageView.ActiveTab.Caption;
    lbbvTabCaption2013.Caption := rbvBackstageView.ActiveTab.Caption;
    if bvtsOpen.Active or bvtsSaveAs.Active then
    begin
      bvgcLocationsRecentDocumentsGroup.Visible := rbvBackstageView.ActiveTab = bvtsOpen;
      if bvgcLocationsRecentDocumentsGroup.Visible and not bvgcLocationsComputerItem.Checked then
        bvgcLocationsRecentDocumentsItem.Checked := True
      else
        bvgcLocationsComputerItem.Checked := True;

      gbLocationsMain.Parent := rbvBackstageView.ActiveTab;
    end;
  end;
end;

procedure TfrmRibbonRichEditForm.SetColorScheme(const AName: string);
begin
  inherited SetColorScheme(AName);
  UpdateColorSchemeRelatedControls;
end;

procedure TfrmRibbonRichEditForm.SetRibbonDemoStyle(const AStyle: TRibbonDemoStyle);
begin
  if AStyle in [rdsOffice2007, rdsScenic] then
    rtHelp.Visible := True
  else
  begin
    Ribbon.ApplicationButton.Menu := rbvBackstageView;
    rtHelp.Visible := False;
  end;
  lbbvTabCaption2010.Visible := AStyle = rdsOffice2010;
  lbbvTabCaption2013.Visible := AStyle in [rdsOffice2013, rdsOffice2016, rdsOffice2016Tablet, rdsOffice2019];
  if AStyle in [rdsOffice2013, rdsOffice2016, rdsOffice2016Tablet, rdsOffice2019] then
    gbBackstageViewTabCaption.Height := cxTextHeight(lbbvTabCaption2013.Style.Font) + bvSpacer7.Height
  else
    gbBackstageViewTabCaption.Height := cxTextHeight(lbbvTabCaption2010.Style.Font) + bvSpacer7.Height;
  inherited SetRibbonDemoStyle(AStyle);
end;

procedure TfrmRibbonRichEditForm.UpdateColorSchemeRelatedControls;
begin
  lbComputer.Style.TextColor := Ribbon.ColorScheme.GetPartColor(rspTabHeaderText);
  lbRecentDocuments.Style.TextColor := lbComputer.Style.TextColor;
  lbbvTabCaption2010.Style.TextColor := Ribbon.ColorScheme.GetPartColor(DXBAR_BACKSTAGEVIEW_TEXTCOLOR);
  lbbvTabCaption2013.Style.TextColor := lbbvTabCaption2010.Style.TextColor;
  lbRecentFolders.Style.TextColor := lbbvTabCaption2010.Style.TextColor;
  lbCurrentFolder.Style.TextColor := lbbvTabCaption2010.Style.TextColor;
end;

procedure TfrmRibbonRichEditForm.rbvBackstageViewPopup(Sender: TObject);
begin
  gbRecentPathsPaneCurrentFolder.Visible := bvgcCurrentFolder.Gallery.Groups.Count > 0;
  DoBackstageViewTabChanged;
end;

procedure TfrmRibbonRichEditForm.rbvBackstageViewTabChanged(Sender: TObject);
begin
  DoBackstageViewTabChanged;
end;

procedure TfrmRibbonRichEditForm.RichEditControlDocumentLoaded(Sender: TObject);
var
  AName: string;
begin
  inherited;
  AName := RichEditControl.Options.DocumentSaveOptions.CurrentFileName;
  RecentDocumentsController.Add(AName);
end;

procedure TfrmRibbonRichEditForm.btnBrowsePathClick(Sender: TObject);
var
  AHandled: Boolean;
begin
  if bvtsSaveAs.Active then
    AHandled := acSaveAs.Execute
  else
    AHandled := acOpenDocument.Execute;
  if AHandled then
    rbvBackstageView.Hide;
end;

procedure TfrmRibbonRichEditForm.bvgcLocationsItemClick(Sender: TObject; AItem: TdxRibbonBackstageViewGalleryItem);
begin
  gbRecentDocumentsPane.Visible := bvgcLocationsRecentDocumentsItem.Checked;
  gbRecentPathsPane.Visible := bvgcLocationsComputerItem.Checked;
end;

procedure TfrmRibbonRichEditForm.bvgcRecentDocumentsItemClick(Sender: TObject;
  AItem: TdxRibbonBackstageViewGalleryItem);
begin
  RichEditControl.Document.LoadDocument(AItem.Hint, TdxRichEditDocumentFormat.Undefined);
  rbvBackstageView.Hide;
end;

procedure TfrmRibbonRichEditForm.bvgcRecentPathsItemClick(Sender: TObject;
  AItem: TdxRibbonBackstageViewGalleryItem);
var
  AHandled: Boolean;
begin
  if bvtsSaveAs.Active then
    AHandled := acSaveAs.Execute
  else
    AHandled := acOpenDocument.Execute;
  if AHandled then
    rbvBackstageView.Hide;
end;

procedure TfrmRibbonRichEditForm.FormCreate(Sender: TObject);
var
  AAboutFileName: string;
begin
  inherited FormCreate(Sender);

  bvgcRecentDocuments.Tag := 0;
  bvgcRecentPaths.Tag := 1;
  FRecentDocumentsController := CreateRecentDocumentsController;
  FRecentDocumentsController.MaxCount := 19;
  FRecentDocumentsController.LoadFromIniFile(GetRecentDocumentsFileName);

  AAboutFileName := ExtractFilePath(Application.ExeName) + 'About.txt';
  if FileExists(AAboutFileName) then
    meAbout.Lines.LoadFromFile(AAboutFileName);

  lblSupport.OnClick := dmCommonData.actSupport.OnExecute;
  lblHelpBars.OnClick := dmCommonData.actBarsHelp.OnExecute;
  lblHelpDocking.OnClick := dmCommonData.actDockingHelp.OnExecute;
  lblProducts.OnClick := dmCommonData.actProducts.OnExecute;
  lblClientCenter.OnClick := dmCommonData.actMyDX.OnExecute;
  lblDXonWeb.OnClick := dmCommonData.actDXOnTheWeb.OnExecute;
  lblDownloads.OnClick := dmCommonData.actDownloads.OnExecute;
end;

procedure TfrmRibbonRichEditForm.FormDestroy(Sender: TObject);
begin
  FRecentDocumentsController.SaveToIniFile(GetRecentDocumentsFileName);
  FreeAndNil(FRecentDocumentsController);
  inherited FormDestroy(Sender);
end;

procedure TfrmRibbonRichEditForm.FormShow(Sender: TObject);
var
  AIniFile: TIniFile;
  ADocumentName: string;
begin
  AIniFile := TIniFile.Create(GetInitDocumentFileName);
  try
    ADocumentName := AIniFile.ReadString('DocumentName', 'DocumentName', '');
    if FileExists(ADocumentName) then
      RichEditControl.LoadDocument(ADocumentName);
  finally
    AIniFile.Free;
  end;
end;

end.
