unit RichEditSimpleMailMerge;

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
  cxGridCustomView, cxGrid, cxSplitter, dxHttpIndyRequest, DBUriStreamProvider, RibbonRichEditMainForm,
  cxFontNameComboBox, cxDropDownEdit, dxRibbonColorGallery, cxBarEditItem, MidasLib, 
  dxRichEdit.OpenXML, dxRichEdit.HTML, dxRichEdit.Doc , dxRichEdit.NativeApi;

type
  TfrmRichEditSimpleMailMerge = class(TfrmRichEditControlBase)
    rtMailMerge: TdxRibbonTab;
    dsMails: TDataSource;
    bmbMailMerge: TdxBar;
    acShowInsertMergeFieldForm: TdxRichEditControlShowInsertMergeFieldForm;
    acShowAllFieldResults: TdxRichEditControlShowAllFieldResults;
    ShowAllFieldCodes: TdxRichEditControlShowAllFieldCodes;
    bbInsertMergeField: TdxBarLargeButton;
    bbShowAllFieldCodes: TdxBarLargeButton;
    bbShowAllFieldResults: TdxBarLargeButton;
    cdsMail: TClientDataSet;
    acViewMergedData: TdxRichEditControlToggleViewMergedData;
    bbViewMergedData: TdxBarLargeButton;
    stInsertMergeField: TdxScreenTip;
    stShowAllFieldCodes: TdxScreenTip;
    stShowAllFieldResults: TdxScreenTip;
    stViewMergedData: TdxScreenTip;
    tvEmployees: TcxGridDBTableView;
    GridLevel: TcxGridLevel;
    Grid: TcxGrid;
    tvEmployeesLastName: TcxGridDBColumn;
    tvEmployeesContactName: TcxGridDBColumn;
    GridAndRichSplitter: TcxSplitter;
    RichEditControl: TdxRichEditControl;
    acShowMergeDatabaseRecordsForm: TdxRichEditControlShowMergeDatabaseRecordsForm;
    bmbMergeTo: TdxBar;
    bMergeTo: TdxBarLargeButton;
    cdsPhotos: TClientDataSet;
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
    procedure RichEditControlMailMergeGetTargetDocument(Sender: TObject;
      const Args: TdxMailMergeGetTargetDocumentEventArgs);
    procedure FormCreate(Sender: TObject);
  protected
    procedure InitDataBase;
    procedure InitDocument;
    procedure InitUriService;
  end;

var
  frmRichEditSimpleMailMerge: TfrmRichEditSimpleMailMerge;

implementation

uses
  dxRichEdit.Utils.UriStreamService;

const
  DatabaseName = '..\..\Data\Employees.cds';
  PhotosDatabaseName = '..\..\Data\EmployeesPhotos.cds';
  DocumentName = '..\..\Data\MailMerge.rtf';

{$R *.dfm}

{ TfrmRichEditSimpleMailMerge }

procedure TfrmRichEditSimpleMailMerge.InitDataBase;
begin
  if FileExists(DatabaseName) then
    cdsMail.LoadFromFile(DatabaseName);
  if FileExists(PhotosDatabaseName) then
    cdsPhotos.LoadFromFile(PhotosDatabaseName);
end;

procedure TfrmRichEditSimpleMailMerge.InitDocument;
begin
  if FileExists(DocumentName) then
    RichEditControl.Document.LoadDocument(DocumentName, TdxRichEditDocumentFormat.Undefined);
end;

procedure TfrmRichEditSimpleMailMerge.InitUriService;
var
  AService: IdxUriStreamService;
  AProvider: TdxDBUriStreamProvider;
begin
  AProvider := TdxDBUriStreamProvider.Create(cdsPhotos, 'EmployeeID', 'Photo', 'dbimg://');
  AService := RichEditControl.InnerControl.GetService<IdxUriStreamService>;
  AService.RegisterProvider(AProvider);
end;

procedure TfrmRichEditSimpleMailMerge.RichEditControlMailMergeGetTargetDocument(
  Sender: TObject; const Args: TdxMailMergeGetTargetDocumentEventArgs);
var
  AForm: TfrmRibbonRichEditMain;
begin
  AForm := TfrmRibbonRichEditMain.Create(Self);
  Args.TargetDocument := AForm.RichEditControl.Document;
  AForm.Show;
end;

procedure TfrmRichEditSimpleMailMerge.FormCreate(Sender: TObject);
begin
  inherited;
  TcxFontNameComboBoxProperties(cxBarEditItemFont.Properties).DropDownListStyle := lsEditList;
end;

procedure TfrmRichEditSimpleMailMerge.FormShow(Sender: TObject);
begin
  InitDocument;
  InitDataBase;
  InitUriService;
  tvEmployees.Controller.FocusedRowIndex := 0;
  tvEmployees.Controller.FocusedRow.Expand(True);
end;

end.
