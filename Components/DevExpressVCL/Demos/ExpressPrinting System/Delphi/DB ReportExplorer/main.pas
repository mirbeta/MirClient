unit main;

interface

{$I cxVer.inc}

uses
  SysUtils, Classes, Graphics, Controls, Dialogs, StdCtrls, Forms, DB, 
  Grids, DBGrids, ExtCtrls, ComCtrls, ToolWin, Menus, ImgList, dxPSCore, 
  dxPSDBBasedXplorer, dxPSXplorerTreeView, dxPSGlbl, dxPSUtl, dxPSEngn, dxPrnPg, 
  dxBkgnd, dxWrap, dxPrnDev, dxExtCtrls, dxPSCompsProvider, dxPSFillPatterns, 
  dxPSEdgePatterns, dxPSTVLnk, dxPSBaseGridLnk, dxPSGrLnks, dxPSStdGrLnk,
  dxPSShapes, dxPSContainerLnk, dxPSPDFExportCore, dxPSPDFExport,
  cxDrawTextUtils, dxPSPrVwStd, dxPScxEditorProducers, cxGroupBox, cxControls,
  dxPScxExtEditorProducers, dxPScxPageControlProducer, cxGraphics,
  ActnList, cxLookAndFeels, dxPSPrVwAdv, dxPSPrVwRibbon, dxmdaset,
  dxPScxCommon, dxPScxExtCommon
  {Follow units have to be added if you want to support all types of saved reports,
   i.e. reports that were created from all types of ReportLinks.
   These units contain registration information for all item types used to create them

   You must own appropriate Developer Express Inc. Control Librraies }

{$IFDEF EXPRESSSPREADSHEET2}
  , dxPSdxSpreadSheetLnk                                   { cxSpreadSheet }
{$ENDIF}
{$IFDEF EXPRESSLAYOUTCONTROL}
  , dxPSdxLCLnk                                  { dxLayoutControl }
{$ENDIF}
{$IFDEF EXPRESSGRID}
  , dxPScxGridLnk { cxGrid, cxTreeList, cxVerticalGrid and any others cx-family products }
{$ENDIF}
  ;
                  
type
  TfmMain = class;
  
  TdxFormExplorerChangeNotifier = class(TdxPSExplorerChangeNotifierAdapter)
  private
    FForm: TfmMain;
  protected
    procedure ItemDataLoaded(AnItem: TdxPSExplorerItem); override;
    procedure ItemDataUnloaded(AnItem: TdxPSExplorerItem); override;
  public
    constructor Create(AForm: TfmMain);
    property Form: TfmMain read FForm;
  end;

  TfmMain = class(TForm, IdxPSExplorerTreeContainerHost, IdxPSExplorerContextCommandBuilder)
    Explorer: TdxPSDBBasedExplorer;
    dsFolders: TDataSource;
    dsItems: TDataSource;
    Label1: TLabel;
    ToolBar1: TToolBar;
    tbFileLoad: TToolButton;
    tbFileClose: TToolButton;
    tbFilePageSetup: TToolButton;
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miFileLoad: TMenuItem;
    miFileClose: TMenuItem;
    miLine30: TMenuItem;
    miFilePrint: TMenuItem;
    miFilePageSetup: TMenuItem;
    miLine3: TMenuItem;
    miFileExit: TMenuItem;
    miExplorer: TMenuItem;
    miExplorerCreateNewFolder: TMenuItem;
    miLine31: TMenuItem;
    miExplorerDelete: TMenuItem;
    miExplorerRename: TMenuItem;
    miLine39: TMenuItem;
    miExplorerProperties: TMenuItem;
    miFilePreview: TMenuItem;
    tbFilePreview: TToolButton;
    tbFilePrint: TToolButton;
    tbExplorerFolderCreate: TToolButton;
    ToolButton7: TToolButton;
    tbExplorerDelete: TToolButton;
    tbExplorerProperties: TToolButton;
    ToolButton11: TToolButton;
    pmExplorer: TPopupMenu;
    pmiExplorerLoadData: TMenuItem;
    pmiExplorerUnloadData: TMenuItem;
    miLine33: TMenuItem;
    pmiExplorerCreateFolder: TMenuItem;
    miLine34: TMenuItem;
    pmiExplorerDelete: TMenuItem;
    pmiExplorerRename: TMenuItem;
    miLine40: TMenuItem;
    pmiExplorerProperties: TMenuItem;
    pnlExplorerTreeHost: TcxGroupBox;
    Splitter1: TSplitter;
    TreeChangeTimer: TTimer;
    ComponentPrinter: TdxComponentPrinter;
    Stub: TdxStringGridReportLink;
    ilMain: TcxImageList;
    ActionList1: TActionList;
    actLoad: TAction;
    actUnLoad: TAction;
    actPageSetup: TAction;
    actPrintPreview: TAction;
    actPrint: TAction;
    actNewFolder: TAction;
    actRename: TAction;
    actProperties: TAction;
    actDelete: TAction;
    actExit: TAction;
    cxLookAndFeelController1: TcxLookAndFeelController;
    mdFolders: TdxMemData;
    mdItems: TdxMemData;
    mdFoldersID: TAutoIncField;
    mdFoldersParentID: TIntegerField;
    mdFoldersName: TStringField;
    mdItemsID: TAutoIncField;
    mdItemsParentID: TIntegerField;
    mdItemsName: TStringField;
    mdItemsData: TBlobField;
    procedure Button1Click(Sender: TObject);
    procedure FilePreviewClick(Sender: TObject);
    procedure FilePageSetupClick(Sender: TObject);
    procedure FileExitClick(Sender: TObject);
    procedure FilePrintClick(Sender: TObject);
    procedure ExplorerCreateNewFolderClick(Sender: TObject);
    procedure ExplorerDeleteItemClick(Sender: TObject);
    procedure ExplorerItemShowPropertySheetsClick(Sender: TObject);
    procedure ExplorerLoadItemDataClick(Sender: TObject);
    procedure ExplorerRenameItemClick(Sender: TObject);
    procedure ExplorerUnloadItemDataClick(Sender: TObject);
    procedure pmExplorerPopup(Sender: TObject);
    procedure TreeChangeTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ExplorerItemDataLoadError(Sender: TCustomdxPSExplorer;
      AnItem: TdxPSExplorerItem; var AShowErrorMessage: Boolean;
      var AText: String);
  private
    FExplorerChangeNotifier: TdxFormExplorerChangeNotifier;
    FExplorerContextCommandMenuItems: TList;
    FExplorerContextCommandPopupMenuItems: TList;
    FExplorerContextCommandToolButtons: TList;
    FExplorerTree: TdxPSExplorerTreeViewContainer;
    FLastSelectedItem: TCustomdxPSExplorerItem;
    FPreviewBox: TdxPSImageScrollBox;
    FReportDocument: TdxPSReportDocument;

    function GetExplorerContextCommandMenuItem(Index: Integer): TMenuItem;
    function GetExplorerContextCommandMenuItemCount: Integer;
    function GetExplorerContextCommandPopupMenuItem(Index: Integer): TMenuItem;
    function GetExplorerContextCommandPopupMenuItemCount: Integer;
    function GetExplorerContextCommandToolButton(Index: Integer): TToolButton;
    function GetExplorerContextCommandToolButtonCount: Integer;
    function GetIsReportItemSelected: Boolean;
    function GetIsReportValid: Boolean;
    function GetPreviewGraphic: TGraphic;

    procedure AssignDataSets;
    procedure CreateExplorerTree;
    procedure CreatePreviewBox;

    function IsSelectedItemLoaded: Boolean;
    procedure LoadItemPreview(AnItem: TdxPSExplorerItem);
    procedure UpdateControls; 
    
    procedure DoExplorerTreeChange(Sender: TObject; ANode: TTreeNode);
    procedure DoExplorerTreeDblClick(Sender: TObject);
    procedure DoShowExplorerPopup(Sender: TObject; Button: TMouseButton; 
      Shift: TShiftState; X, Y: Integer);
  protected
    { IdxPSExplorerTreeContainerHost }
    function GetFlat: Boolean;
    function GetReportLink: TBasedxReportLink;
    function GetTreeContainerParent: TcxControl;
    procedure UpdateState;
    
    procedure AddExplorerContextCommand(ACommand: TCustomdxPSExplorerContextCommand); virtual;
    procedure UpdateExplorerContextCommands; virtual; 

    procedure ExplorerContextCommandClick(Sender: TObject);

    property ExplorerContextCommandMenuItemCount: Integer read GetExplorerContextCommandMenuItemCount;
    property ExplorerContextCommandMenuItems[Index: Integer]: TMenuItem read GetExplorerContextCommandMenuItem;
    property ExplorerContextCommandPopupMenuItemCount: Integer read GetExplorerContextCommandPopupMenuItemCount;
    property ExplorerContextCommandPopupMenuItems[Index: Integer]: TMenuItem read GetExplorerContextCommandPopupMenuItem;
    property ExplorerContextCommandToolButtonCount: Integer read GetExplorerContextCommandToolButtonCount;
    property ExplorerContextCommandToolButtons[Index: Integer]: TToolButton read GetExplorerContextCommandToolButton;
    property ExplorerTree: TdxPSExplorerTreeViewContainer read FExplorerTree;
    property LastSelectedItem: TCustomdxPSExplorerItem read FLastSelectedItem write FLastSelectedItem;
    property PreviewBox: TdxPSImageScrollBox read FPreviewBox;
    property PreviewGraphic: TGraphic read GetPreviewGraphic;
    property ReportDocument: TdxPSReportDocument read FReportDocument;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    property IsReportItemSelected: Boolean read GetIsReportItemSelected;
    property IsReportValid: Boolean read GetIsReportValid;
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses 
  Windows, dxPSRes;
  
{ TdxFormExplorerChangeNotifier }  

constructor TdxFormExplorerChangeNotifier.Create(AForm: TfmMain);
begin
  inherited Create(AForm.Explorer);
  FForm := AForm;
end;

procedure TdxFormExplorerChangeNotifier.ItemDataLoaded(AnItem: TdxPSExplorerItem);
begin
  Form.UpdateControls;
end;

procedure TdxFormExplorerChangeNotifier.ItemDataUnloaded(AnItem: TdxPSExplorerItem);
begin
  Form.UpdateControls;
end;
  
constructor TfmMain.Create(AOwner: TComponent);
begin
  FExplorerContextCommandMenuItems := TList.Create;
  FExplorerContextCommandPopupMenuItems := TList.Create;
  FExplorerContextCommandToolButtons := TList.Create;
  inherited;
end;

destructor TfmMain.Destroy;
begin
  FreeAndNil(FReportDocument);
  FreeAndNil(FExplorerChangeNotifier);
  ExplorerUnloadItemDataClick(nil);
  FreeAndNil(FExplorerContextCommandToolButtons);
  FreeAndNil(FExplorerContextCommandPopupMenuItems);
  FreeAndNil(FExplorerContextCommandMenuItems);
  FreeAndNil(FExplorerTree);
  inherited;
end;

procedure TfmMain.Button1Click(Sender: TObject);
begin
end;

function TfmMain.GetFlat: Boolean;
begin
  Result := False;
end;

procedure TfmMain.DoExplorerTreeDblClick(Sender: TObject);
begin
  LastSelectedItem := ExplorerTree.SelectedItem;
  TreeChangeTimerTimer(TreeChangeTimer);
end;

function TfmMain.GetReportLink: TBasedxReportLink;
begin
  Result := ComponentPrinter.CurrentLink;
end;

function TfmMain.GetTreeContainerParent: TcxControl;
begin
  Result := pnlExplorerTreeHost;
end;

procedure TfmMain.UpdateState;
begin
  UpdateControls;
end;

procedure TfmMain.AddExplorerContextCommand(ACommand: TCustomdxPSExplorerContextCommand);

  function IsCommandSeparator(ACommand: TCustomdxPSExplorerContextCommand): Boolean;
  begin
    Result := ACommand is TdxPSExplorerContextCommandSeparator;
  end;
   
  function AddExplorerContextCommandMenuItem(AParent: TMenuItem; ACommand: TCustomdxPSExplorerContextCommand): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    with Result do
    begin
      ImageIndex := ilMain.AddMasked(ACommand.Bitmap, ACommand.Bitmap.TransparentColor);
      Caption := ACommand.Caption;
      Enabled := ACommand.Enabled;
      Hint := ACommand.Hint;
      ShortCut := ACommand.ShortCut;
      Tag := Integer(ACommand);

      OnClick := ExplorerContextCommandClick;
    end;
    AParent.Insert(0, Result);
  end;

var
  MenuItem: TMenuItem;
begin
  MenuItem := AddExplorerContextCommandMenuItem(pmExplorer.Items, ACommand);
  if not IsCommandSeparator(ACommand) and (FExplorerContextCommandPopupMenuItems.IndexOf(MenuItem) = -1) then
    FExplorerContextCommandPopupMenuItems.Add(MenuItem);
    
  MenuItem := AddExplorerContextCommandMenuItem(miExplorer, ACommand);
  if not IsCommandSeparator(ACommand) and (FExplorerContextCommandMenuItems.IndexOf(MenuItem) = -1) then
    FExplorerContextCommandMenuItems.Add(MenuItem);
end;

procedure TfmMain.UpdateExplorerContextCommands;

  procedure UpdateMenuItems;
  var
    I: Integer;
  begin
    for I := 0 to ExplorerContextCommandMenuItemCount - 1 do
      with ExplorerContextCommandMenuItems[I] do 
        Enabled := TCustomdxPSExplorerContextCommand(Tag).Enabled;
  end;

  procedure UpdatePopupMenuItems;
  var
    I: Integer;
  begin
    for I := 0 to ExplorerContextCommandPopupMenuItemCount - 1 do
      with ExplorerContextCommandPopupMenuItems[I] do 
        Enabled := TCustomdxPSExplorerContextCommand(Tag).Enabled;
  end;
    
  procedure UpdateToolButtons;
  var
    I: Integer;
  begin
    for I := 0 to ExplorerContextCommandToolButtonCount - 1 do
      with ExplorerContextCommandToolButtons[I] do 
        Enabled := TCustomdxPSExplorerContextCommand(Tag).Enabled;
  end;
  
begin
  if not (csDestroying in ComponentState) then 
  begin
    UpdateMenuItems;
    UpdatePopupMenuItems;
    UpdateToolButtons;
  end;
end;

procedure TfmMain.ExplorerContextCommandClick(Sender: TObject);
var
  Command: TCustomdxPSExplorerContextCommand;
begin
  Command := TCustomdxPSExplorerContextCommand(TMenuItem(Sender).Tag);
  if Command.Enabled then
    Command.Execute;
end;

function TfmMain.GetExplorerContextCommandMenuItem(Index: Integer): TMenuItem;
begin
  Result := FExplorerContextCommandMenuItems.Items[Index];
end;

function TfmMain.GetExplorerContextCommandMenuItemCount: Integer;
begin
  Result := FExplorerContextCommandMenuItems.Count;
end;

function TfmMain.GetExplorerContextCommandPopupMenuItem(Index: Integer): TMenuItem;
begin
  Result := FExplorerContextCommandPopupMenuItems.Items[Index];
end;

function TfmMain.GetExplorerContextCommandPopupMenuItemCount: Integer;
begin
  Result := FExplorerContextCommandPopupMenuItems.Count;
end;

function TfmMain.GetExplorerContextCommandToolButton(Index: Integer): TToolButton;
begin
  Result := FExplorerContextCommandToolButtons.Items[Index];
end;

function TfmMain.GetExplorerContextCommandToolButtonCount: Integer;
begin
  Result := FExplorerContextCommandToolButtons.Count;
end;

function TfmMain.GetIsReportItemSelected: Boolean;
begin
  Result := ExplorerTree.SelectedItem is TdxPSExplorerItem;
end;

function TfmMain.GetIsReportValid: Boolean;
begin
  Result := PreviewGraphic <> nil;
end;

function TfmMain.GetPreviewGraphic: TGraphic;
begin
  if ReportDocument <> nil then
    Result := ReportDocument.Preview
  else  
    Result := nil;
end;

procedure TfmMain.AssignDataSets;
begin
  mdFolders.Close;
  mdFolders.LoadFromBinaryFile('..\..\Data\Folders(AutoInc).dat');
  mdFolders.Open;

  mdItems.Close;
  mdItems.LoadFromBinaryFile('..\..\Data\Items(AutoInc).dat');
  mdItems.Open;
end;

procedure TfmMain.CreateExplorerTree;
var
  ExplorerTreeContainerHost: IdxPSExplorerTreeContainerHost;
  ExplorerContextCommands: IdxPSExplorerContextCommands;
  ExplorerContextCommandBuilder: IdxPSExplorerContextCommandBuilder;
begin  
  if Self.GetInterface(IdxPSExplorerTreeContainerHost, ExplorerTreeContainerHost) then 
  begin
    FExplorerTree := Explorer.CreateTree(ExplorerTreeContainerHost) as TdxPSExplorerTreeViewContainer; 
    ExplorerTree.TreeView.OnChange := DoExplorerTreeChange;
    ExplorerTree.TreeView.OnDblClick := DoExplorerTreeDblClick;
    ExplorerTree.TreeView.OnMouseUp := DoShowExplorerPopup; 
    
    Explorer.BuildTree(ExplorerTree);
    FExplorerChangeNotifier := TdxFormExplorerChangeNotifier.Create(Self);
  end;
  if Explorer.GetInterface(IdxPSExplorerContextCommands, ExplorerContextCommands) and
    Self.GetInterface(IdxPSExplorerContextCommandBuilder, ExplorerContextCommandBuilder) then
    ExplorerContextCommands.BuildCommandSet(ExplorerContextCommandBuilder);
end;

procedure TfmMain.CreatePreviewBox;
begin
  FPreviewBox := TdxPSImageScrollBox.Create(Self);
  with FPreviewBox do
  begin
    Parent := Self;
    Align := alClient;
    Visible := True;//}False;
  end;
end;

procedure TfmMain.DoExplorerTreeChange(Sender: TObject; ANode: TTreeNode);
begin
  TreeChangeTimer.Enabled := False;
  LastSelectedItem := ExplorerTree.SelectedItem;
  TreeChangeTimer.Enabled := True;
end;

procedure TfmMain.DoShowExplorerPopup(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then 
    pmExplorer.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

function TfmMain.IsSelectedItemLoaded: Boolean;
begin
  ExplorerTree.LoadSelectedItemData;
  Result := ExplorerTree.IsSelectedItemCurrentlyLoaded;
end;

procedure TfmMain.LoadItemPreview(AnItem: TdxPSExplorerItem);
var
  Stream: TStream;
begin
  FreeAndNil(FReportDocument);
  Stream := AnItem.CreateDataStream(smRead);
  if Stream <> nil then
  try
    try
      FReportDocument := TBasedxReportLink.ExtractReportDocument(Stream, False);
    except
      FReportDocument := nil;
    end;
  finally
    Stream.Free;
  end;
  UpdateControls;
end;

procedure TfmMain.UpdateControls;
begin
  if csDestroying in ComponentState then
     Exit;

  actLoad.Enabled := ExplorerTree.CanLoadSelectedItemData;
  actUnLoad.Enabled := ExplorerTree.CanUnloadItemData;
  actPageSetup.Enabled := IsReportItemSelected;
  actPrintPreview.Enabled := IsReportItemSelected;
  actPrint.Enabled := IsReportItemSelected;
  actNewFolder.Enabled := ExplorerTree.CanCreateFolder;
  actDelete.Enabled := ExplorerTree.CanDeleteSelection;
  actRename.Enabled := ExplorerTree.CanRenameSelectedItem;
  actProperties.Enabled := ExplorerTree.CanShowPropertySheetsForSelectedItem;

  PreviewBox.Enabled := IsReportValid;
  PreviewBox.Picture.Assign(PreviewGraphic);
  if IsReportValid then
    PreviewBox.HintText := ''
  else
    PreviewBox.HintText := sdxNone;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  AssignDataSets;
  CreatePreviewBox;
  CreateExplorerTree;
  ActiveControl := ExplorerTree.TreeView;
  UpdateControls;
end;

procedure TfmMain.FilePageSetupClick(Sender: TObject);
begin
  if IsSelectedItemLoaded then 
    ComponentPrinter.PageSetup(nil);
end;

procedure TfmMain.FilePreviewClick(Sender: TObject);
begin
  if IsSelectedItemLoaded then 
    ComponentPrinter.Preview;
end;

procedure TfmMain.FilePrintClick(Sender: TObject);
begin
  if IsSelectedItemLoaded then 
    ComponentPrinter.Print(True, nil, nil);
end;

procedure TfmMain.FileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.ExplorerCreateNewFolderClick(Sender: TObject);
begin
  if ExplorerTree.CanCreateFolder then 
    Explorer.CreateNewFolder(ExplorerTree.CreationParent);
end;

procedure TfmMain.ExplorerDeleteItemClick(Sender: TObject);
begin
  ExplorerTree.DeleteSelection;
end;

procedure TfmMain.ExplorerItemShowPropertySheetsClick(Sender: TObject);
begin
  ExplorerTree.ShowSelectedItemPropertySheets;
end;
                           
procedure TfmMain.ExplorerLoadItemDataClick(Sender: TObject);
begin
  if ExplorerTree.CanLoadSelectedItemData then 
    ExplorerTree.LoadSelectedItemData
end;

procedure TfmMain.ExplorerRenameItemClick(Sender: TObject);
begin
  ExplorerTree.BeginEdit;
end;

procedure TfmMain.ExplorerUnloadItemDataClick(Sender: TObject);
begin
  if ExplorerTree.CanUnloadItemData then 
    ExplorerTree.UnloadItemData
end;

procedure TfmMain.pmExplorerPopup(Sender: TObject);
begin
  UpdateExplorerContextCommands;
  UpdateControls;
end; 

procedure TfmMain.TreeChangeTimerTimer(Sender: TObject);
var
  Item: TCustomdxPSExplorerItem;
begin
  Item := ExplorerTree.SelectedItem;
  if (Item = LastSelectedItem) and (Item is TdxPSExplorerItem) and not TdxPSExplorerItem(Item).IsCurrentlyLoaded then
  begin
    TTimer(Sender).Enabled := False;
    LoadItemPreview(TdxPSExplorerItem(Item));
  end;  
end;

procedure TfmMain.ExplorerItemDataLoadError(Sender: TCustomdxPSExplorer;
  AnItem: TdxPSExplorerItem; var AShowErrorMessage: Boolean;
  var AText: String);
const
  CRLF = #13#10;
  ErrorText: string = 'Cannot Load Item "%s".' + CRLF + 
    'You should uncomment appropriate units in "uses" clause.' + CRLF + 
    CRLF +
    'Please read ReadMe.txt.';
begin
  AShowErrorMessage := True;
  AText := Format(ErrorText, [TdxPSDBBasedExplorerItem(AnItem).Name]);
end;

end.
