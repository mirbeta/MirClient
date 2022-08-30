unit RichEditMasterDetailMailMerge;

{$I cxVer.inc}

interface

uses
  Forms, RichEditControlBase, SysUtils, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxRibbonCustomizationForm, dxRibbonSkins, dxCore,
  dxCoreClasses, dxGDIPlusAPI, dxGDIPlusClasses, dxRichEdit.Types,
  dxRichEdit.Options, dxRichEdit.Control, dxBarBuiltInMenu, dxBar,
  dxBarApplicationMenu, dxRibbon, dxScreenTip, DB, DBClient, ImgList, Controls,
  dxRichEdit.Actions, Classes, ActnList, dxActions, dxBarExtItems,
  dxRibbonGallery, dxSkinChooserGallery, cxNavigator, cxDBNavigator,
  dxStatusBar, dxRibbonStatusBar, dxRichEdit.Platform.Win.Control, cxClasses,
  cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxDBData,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGridLevel,
  cxGridCustomView, cxGrid, cxSplitter, dxLayoutContainer, dxLayoutControl,
  dxHttpIndyRequest, DBUriStreamProvider, dxLayoutLookAndFeels, cxBarEditItem,
  cxFontNameComboBox, cxDropDownEdit, dxRibbonColorGallery, MidasLib, 
  dxRichEdit.OpenXML, dxRichEdit.HTML, dxRichEdit.Doc, dxRichEdit.NativeApi;

type
  TfrmRichEditMasterDetailMailMerge = class(TfrmRichEditControlBase)
    rtMailMerge: TdxRibbonTab;
    bmbMailMerge: TdxBar;
    acShowAllFieldResults: TdxRichEditControlShowAllFieldResults;
    ShowAllFieldCodes: TdxRichEditControlShowAllFieldCodes;
    bbShowAllFieldCodes: TdxBarLargeButton;
    bbShowAllFieldResults: TdxBarLargeButton;
    stShowAllFieldCodes: TdxScreenTip;
    stShowAllFieldResults: TdxScreenTip;
    LayoutControlGroup_Root: TdxLayoutGroup;
    LayoutControl: TdxLayoutControl;
    liTemplate: TdxLayoutItem;
    lgTemplate: TdxLayoutGroup;
    recTemplate: TdxRichEditControl;
    lgResultingDocument: TdxLayoutGroup;
    lgDetail: TdxLayoutGroup;
    lgMaster: TdxLayoutGroup;
    recMaster: TdxRichEditControl;
    liMaster: TdxLayoutItem;
    recDetail: TdxRichEditControl;
    liDetail: TdxLayoutItem;
    recResultingDocument: TdxRichEditControl;
    liResultingDocument: TdxLayoutItem;
    dsTemplate: TDataSource;
    cdsTemplate: TClientDataSet;
    cdsTemplatefake: TIntegerField;
    cdsMaster: TClientDataSet;
    cdsDetail: TClientDataSet;
    cdsCategories: TClientDataSet;
    dsMaster: TDataSource;
    dsDetail: TDataSource;
    LayoutLookAndFeels: TdxLayoutLookAndFeelList;
    LayoutCxLookAndFeel: TdxLayoutCxLookAndFeel;
    bmbMergeToNewDocument: TdxBar;
    bMergeToNewDocument: TdxBarLargeButton;
    dxRichEditControlNewDocument: TdxRichEditControlNewDocument;
    dxRibbonTabFile: TdxRibbonTab;
    dxBarCommon: TdxBar;
    dxBarLargeButtonNew: TdxBarLargeButton;
    dxRichEditControlLoadDocument: TdxRichEditControlLoadDocument;
    dxBarLargeButtonOpen: TdxBarLargeButton;
    dxRichEditControlSaveDocument: TdxRichEditControlSaveDocument;
    dxBarLargeButtonSave: TdxBarLargeButton;
    dxRichEditControlSaveDocumentAs: TdxRichEditControlSaveDocumentAs;
    dxBarLargeButtonSaveAs: TdxBarLargeButton;
    dxRichEditControlPasteSelection: TdxRichEditControlPasteSelection;
    dxRibbonTabHome: TdxRibbonTab;
    dxBarClipboard: TdxBar;
    dxBarLargeButtonPaste: TdxBarLargeButton;
    dxRichEditControlCutSelection: TdxRichEditControlCutSelection;
    dxBarButtonCut: TdxBarButton;
    dxRichEditControlCopySelection: TdxRichEditControlCopySelection;
    dxBarButtonCopy: TdxBarButton;
    dxRichEditControlSelectAll: TdxRichEditControlSelectAll;
    dxBarButtonSelectAll: TdxBarButton;
    dxRichEditControlChangeFontName: TdxRichEditControlChangeFontName;
    dxBarFont: TdxBar;
    cxBarEditItemFont: TcxBarEditItem;
    dxRichEditControlChangeFontSize: TdxRichEditControlChangeFontSize;
    cxBarEditItemFontSize: TcxBarEditItem;
    dxRichEditControlIncreaseFontSize: TdxRichEditControlIncreaseFontSize;
    dxBarButtonGrowFont: TdxBarButton;
    dxRichEditControlDecreaseFontSize: TdxRichEditControlDecreaseFontSize;
    dxBarButtonShrinkFont: TdxBarButton;
    dxBarSubItem1: TdxBarSubItem;
    dxRichEditControlTextUpperCase: TdxRichEditControlTextUpperCase;
    dxBarLargeButtonUPPERCASE: TdxBarLargeButton;
    dxRichEditControlTextLowerCase: TdxRichEditControlTextLowerCase;
    dxBarLargeButtonlowercase: TdxBarLargeButton;
    dxRichEditControlToggleTextCase: TdxRichEditControlToggleTextCase;
    dxBarLargeButtontOGGLEcASE: TdxBarLargeButton;
    dxRichEditControlToggleFontBold: TdxRichEditControlToggleFontBold;
    dxBarButtonBold: TdxBarButton;
    dxRichEditControlToggleFontItalic: TdxRichEditControlToggleFontItalic;
    dxBarButtonItalic: TdxBarButton;
    dxRichEditControlToggleFontUnderline: TdxRichEditControlToggleFontUnderline;
    dxBarButtonUnderline: TdxBarButton;
    dxRichEditControlToggleFontDoubleUnderline: TdxRichEditControlToggleFontDoubleUnderline;
    dxBarButtonDoubleUnderline: TdxBarButton;
    dxRichEditControlToggleFontStrikeout: TdxRichEditControlToggleFontStrikeout;
    dxBarButtonStrikethrough: TdxBarButton;
    dxRichEditControlToggleFontDoubleStrikeout: TdxRichEditControlToggleFontDoubleStrikeout;
    dxBarButtonDoubleStrikethrough: TdxBarButton;
    dxRichEditControlToggleFontSubscript: TdxRichEditControlToggleFontSubscript;
    dxBarButtonSubscript: TdxBarButton;
    dxRichEditControlToggleFontSuperscript: TdxRichEditControlToggleFontSuperscript;
    dxBarButtonSuperscript: TdxBarButton;
    dxRichEditControlTextHighlight: TdxRichEditControlTextHighlight;
    dxRibbonColorGalleryItemTextHighlightColor: TdxRibbonColorGalleryItem;
    dxRichEditControlChangeFontColor: TdxRichEditControlChangeFontColor;
    dxRibbonColorGalleryItemFontColor: TdxRibbonColorGalleryItem;
    dxRichEditControlToggleBulletedList: TdxRichEditControlToggleBulletedList;
    dxBarParagraph: TdxBar;
    dxBarButtonBullets: TdxBarButton;
    dxRichEditControlToggleSimpleNumberingList: TdxRichEditControlToggleSimpleNumberingList;
    dxBarButtonNumbering: TdxBarButton;
    dxRichEditControlToggleMultiLevelList: TdxRichEditControlToggleMultiLevelList;
    dxBarButtonMultilevellist: TdxBarButton;
    dxRichEditControlDecrementIndent: TdxRichEditControlDecrementIndent;
    dxBarButtonDecreaseIndent: TdxBarButton;
    dxRichEditControlIncrementIndent: TdxRichEditControlIncrementIndent;
    dxBarButtonIncreaseIndent: TdxBarButton;
    dxRichEditControlToggleShowWhitespace: TdxRichEditControlToggleShowWhitespace;
    dxBarButtonShowHide: TdxBarButton;
    dxRichEditControlToggleParagraphAlignmentLeft: TdxRichEditControlToggleParagraphAlignmentLeft;
    dxBarButtonAlignTextLeft: TdxBarButton;
    dxRichEditControlToggleParagraphAlignmentCenter: TdxRichEditControlToggleParagraphAlignmentCenter;
    dxBarButtonCenter: TdxBarButton;
    dxRichEditControlToggleParagraphAlignmentRight: TdxRichEditControlToggleParagraphAlignmentRight;
    dxBarButtonAlignTextRight: TdxBarButton;
    dxRichEditControlToggleParagraphAlignmentJustify: TdxRichEditControlToggleParagraphAlignmentJustify;
    dxBarButtonJustify: TdxBarButton;
    dxBarSubItem2: TdxBarSubItem;
    dxRichEditControlSetSingleParagraphSpacing: TdxRichEditControlSetSingleParagraphSpacing;
    dxBarLargeButton1: TdxBarLargeButton;
    dxRichEditControlSetSesquialteralParagraphSpacing: TdxRichEditControlSetSesquialteralParagraphSpacing;
    dxBarLargeButton2: TdxBarLargeButton;
    dxRichEditControlSetDoubleParagraphSpacing: TdxRichEditControlSetDoubleParagraphSpacing;
    dxBarLargeButton3: TdxBarLargeButton;
    dxRichEditControlShowParagraphForm: TdxRichEditControlShowParagraphForm;
    dxBarLargeButtonParagraph: TdxBarLargeButton;
    dxRichEditControlSearchFind: TdxRichEditControlSearchFind;
    dxBarEditing: TdxBar;
    dxBarButtonFind: TdxBarButton;
    dxRichEditControlSearchReplace: TdxRichEditControlSearchReplace;
    dxBarButtonReplace: TdxBarButton;
    dxRichEditControlUndo: TdxRichEditControlUndo;
    dxBarLargeButtonUndo: TdxBarLargeButton;
    dxRichEditControlRedo: TdxRichEditControlRedo;
    dxBarLargeButtonRedo: TdxBarLargeButton;
    procedure FormShow(Sender: TObject);
    procedure ResultingDocumentCalculateDocumentVariable(Sender: TObject;
      E: TdxCalculateDocumentVariableEventArgs);
    procedure TemplateMailMergeStarted(Sender: TObject;
      const Args: TdxMailMergeStartedEventArgs);
    procedure bMergeToNewDocumentClick(Sender: TObject);
    procedure LayoutControlGroup_RootTabChanged(Sender: TObject);
    procedure ModifiedChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  protected
    procedure CalculateMaxAndMin(out AMax, AMin: Double);
    function GetID(const AValue: string): Integer;
    procedure DetailDocumentServerCalculateDocumentVariable(Sender: TObject;
      E: TdxCalculateDocumentVariableEventArgs);
    procedure FillTemplate;
    procedure InitDataBases;
    procedure InitDocuments;
    function LookUp(ASource: TClientDataSet; AKeyField, AField: string; AKeyValue: Variant): Variant;
    procedure MasterDocumentServerCalculateDocumentVariable(Sender: TObject;
      E: TdxCalculateDocumentVariableEventArgs);
    procedure MergeToNewDocument;
  end;

var
  frmRichEditMasterDetailMailMerge: TfrmRichEditMasterDetailMailMerge;

implementation

uses
  Math, Variants, RTTI, dxRichEdit.Utils.UriStreamService,
  dxRichEdit.DocumentModel.RichEditDocumentServer;

const
  CategoriesDatabaseName = '..\..\Data\Categories.cds';
  DetailDocumentName = '..\..\Data\MasterDetailMailMergeDetail.rtf';
  ProductsDatabaseName = '..\..\Data\Products.cds';
  MasterDatabaseName = '..\..\Data\Master.cds';
  MasterDocumentName = '..\..\Data\MasterDetailMailMergeMaster.rtf';
  TemplateDocumentName = '..\..\Data\MasterDetailMailMergeTemplate.rtf';

{$R *.dfm}

{ TfrmRichEditSimpleMailMerge }

procedure TfrmRichEditMasterDetailMailMerge.CalculateMaxAndMin(out AMax, AMin: Double);
var
  ADataSet: TClientDataSet;
  AValue: Variant;
begin
  ADataSet := TClientDataSet.Create(nil);
  try
    ADataSet.CloneCursor(cdsDetail, False);
    ADataSet.First;
    AValue := ADataSet.FieldValues['UnitPrice'];
    AMax := AValue;
    AMin := AValue;
    while not ADataSet.Eof do
    begin
      ADataSet.Next;
      AValue := ADataSet.FieldValues['UnitPrice'];
      AMax := Max(AMax, AValue);
      AMin := Min(AMin, AValue);
    end;
  finally
    ADataSet.Free;
  end;
end;

function TfrmRichEditMasterDetailMailMerge.GetID(const AValue: string): Integer;
begin
  if TryStrToInt(AValue, Result) then
    Exit(Result);
  Result := -1;
end;

procedure TfrmRichEditMasterDetailMailMerge.bMergeToNewDocumentClick(
  Sender: TObject);
begin
  LayoutControlGroup_Root.ItemIndex := lgResultingDocument.Index;
end;

procedure TfrmRichEditMasterDetailMailMerge.DetailDocumentServerCalculateDocumentVariable(
  Sender: TObject; E: TdxCalculateDocumentVariableEventArgs);
var
  AProductId: Integer;
  AValue: Variant;
begin
  if E.Arguments.Count = 0 then
    Exit;
  AProductId := GetID(E.Arguments[0].Value);
  if AProductId = -1 then
    Exit;
  if E.VariableName = 'UnitPrice' then
  begin
    AValue := LookUp(cdsDetail, 'ProductID', 'UnitPrice', AProductId);
    E.Value := CurrToStrF(AValue, ffCurrency, 2);
    E.Handled := True;
  end;
end;

procedure TfrmRichEditMasterDetailMailMerge.FillTemplate;
begin
  cdsTemplate.Append;
  cdsTemplate.Fields[0].Value := 0;
  cdsTemplate.Post;
end;

procedure TfrmRichEditMasterDetailMailMerge.InitDataBases;
begin
  if FileExists(CategoriesDatabaseName) then
    cdsCategories.LoadFromFile(CategoriesDatabaseName);
  if FileExists(MasterDatabaseName) then
    cdsMaster.LoadFromFile(MasterDatabaseName);
  if FileExists(ProductsDatabaseName) then
    cdsDetail.LoadFromFile(ProductsDatabaseName);
  cdsTemplate.DisableControls;
  cdsMaster.DisableControls;
  cdsDetail.DisableControls;
  cdsCategories.DisableControls;
end;

procedure TfrmRichEditMasterDetailMailMerge.InitDocuments;
begin
  if FileExists(TemplateDocumentName) then
    recTemplate.Document.LoadDocument(TemplateDocumentName, TdxRichEditDocumentFormat.Undefined);
  if FileExists(MasterDocumentName) then
    recMaster.Document.LoadDocument(MasterDocumentName, TdxRichEditDocumentFormat.Undefined);
  if FileExists(DetailDocumentName) then
    recDetail.Document.LoadDocument(DetailDocumentName, TdxRichEditDocumentFormat.Undefined);
  recTemplate.DocumentModelModified := False;
  recMaster.DocumentModelModified := False;
  recDetail.DocumentModelModified := False;
end;

function TfrmRichEditMasterDetailMailMerge.LookUp(ASource: TClientDataSet;
  AKeyField, AField: string; AKeyValue: Variant): Variant;
var
  ADataSet: TClientDataSet;
begin
  ADataSet := TClientDataSet.Create(nil);
  try
    ADataSet.CloneCursor(ASource, True);
    Result := ADataSet.Lookup(AKeyField, AKeyValue, AField);
  finally
    ADataSet.Free;
  end;
end;

procedure TfrmRichEditMasterDetailMailMerge.LayoutControlGroup_RootTabChanged(Sender: TObject);
var
  AIsResultDocument: Boolean;
begin
  AIsResultDocument := LayoutControlGroup_Root.ItemIndex = lgResultingDocument.Index;
  bmbMergeToNewDocument.Visible := not AIsResultDocument;
  if AIsResultDocument then
    MergeToNewDocument;
end;

procedure TfrmRichEditMasterDetailMailMerge.MasterDocumentServerCalculateDocumentVariable(
  Sender: TObject; E: TdxCalculateDocumentVariableEventArgs);
var
  ACurrentCategoryID: Integer;
  AOptions: IdxRichEditMailMergeOptions;
  ADocumentServer: IdxRichEditDocumentServer;
  AMax, AMin: Double;
  AValue: Variant;
begin
  if E.Arguments.Count = 0 then
    Exit;
  ACurrentCategoryID := GetID(E.Arguments[0].Value);
  if ACurrentCategoryID = -1 then
    Exit;
  cdsDetail.Filter := 'CategoryID = ' + IntToStr(ACurrentCategoryID);
  cdsDetail.Filtered := True;
  if E.VariableName = 'Products' then
  begin
    ADocumentServer := recDetail.CreateDocumentServer;
    AOptions := recDetail.CreateMailMergeOptions;
    AOptions.MergeMode := TdxRichEditMergeMode.JoinTables;
    ADocumentServer.AddCalculateDocumentVariableHandler(DetailDocumentServerCalculateDocumentVariable);
    recDetail.MailMerge(AOptions, ADocumentServer.Document);
    ADocumentServer.RemoveCalculateDocumentVariableHandler(DetailDocumentServerCalculateDocumentVariable);
    E.Value := TValue.From(ADocumentServer);
    E.Handled := True;
  end;
  CalculateMaxAndMin(AMax, AMin);
  if E.VariableName = 'ItemCount' then
  begin
    E.Value := cdsDetail.RecordCount;
    E.Handled := True;
  end;
  if E.VariableName = 'LowestPrice' then
  begin
    E.Value := CurrToStrF(AMin, ffCurrency, 2);
    E.Handled := True;
  end;
  if E.VariableName = 'HighestPrice' then
  begin
    E.Value := CurrToStrF(AMax, ffCurrency, 2);
    E.Handled := True;
  end;
  if E.VariableName = 'TotalSales' then
  begin
    AValue := LookUp(cdsMaster, 'CategoryID', 'TotalSales', ACurrentCategoryID);
    E.Value := CurrToStrF(AValue, ffCurrency, 2);
    E.Handled := True;
  end;
end;

procedure TfrmRichEditMasterDetailMailMerge.MergeToNewDocument;
var
  ANeedMerge: Boolean;
begin
  ANeedMerge := recTemplate.DocumentModelModified or recMaster.DocumentModelModified or
    recDetail.DocumentModelModified;

  if not ANeedMerge then
    Exit;
  recTemplate.MailMerge(recResultingDocument.Document);
  recTemplate.DocumentModelModified := False;
  recMaster.DocumentModelModified := False;
  recDetail.DocumentModelModified := False;
end;

procedure TfrmRichEditMasterDetailMailMerge.ModifiedChanged(
  Sender: TObject);
begin
// do nothing
end;

procedure TfrmRichEditMasterDetailMailMerge.FormCreate(Sender: TObject);
begin
  inherited;
  TcxFontNameComboBoxProperties(cxBarEditItemFont.Properties).DropDownListStyle := lsEditList;
end;

procedure TfrmRichEditMasterDetailMailMerge.FormShow(Sender: TObject);
begin
  InitDocuments;
  InitDataBases;
  FillTemplate;
  MergeToNewDocument;
end;

procedure TfrmRichEditMasterDetailMailMerge.ResultingDocumentCalculateDocumentVariable(
  Sender: TObject; E: TdxCalculateDocumentVariableEventArgs);
var
  ADocumentServer: IdxRichEditDocumentServer;
begin
  if E.VariableName = 'Categories' then
  begin
    ADocumentServer := recMaster.CreateDocumentServer;
    ADocumentServer.AddCalculateDocumentVariableHandler(MasterDocumentServerCalculateDocumentVariable);
    recMaster.MailMerge(ADocumentServer.Document);
    ADocumentServer.RemoveCalculateDocumentVariableHandler(MasterDocumentServerCalculateDocumentVariable);
    E.Value := TValue.From(ADocumentServer);
    E.Handled := True;
  end;
end;

procedure TfrmRichEditMasterDetailMailMerge.TemplateMailMergeStarted(
  Sender: TObject; const Args: TdxMailMergeStartedEventArgs);
var
  AService: IdxUriStreamService;
  AProvider: TdxDBUriStreamProvider;
begin
  AProvider := TdxDBUriStreamProvider.Create(cdsCategories, 'CategoryID', 'Picture', 'dbimg://');
  AService := recMaster.InnerControl.GetService<IdxUriStreamService>;
  AService.RegisterProvider(AProvider);
end;

end.
