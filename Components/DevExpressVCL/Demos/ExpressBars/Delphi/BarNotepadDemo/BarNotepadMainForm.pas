unit BarNotepadMainForm;

{$I cxVer.inc}

interface

uses
{$IFDEF EXPRESSSKINS}
  dxBarSkinnedCustForm, dxSkinsdxBarPainter,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ActnList, NotepadMainForm, cxPC,
  dxTabbedMDI, dxBar, cxLookAndFeels, ImgList, cxGraphics, cxClasses, dxBarExtItems, cxFontNameComboBox, cxBarEditItem,
  cxDropDownEdit, dxColorEdit, dxRibbonGallery, dxSkinChooserGallery, NotepadChildForm, IniFiles;

type

  { TBarRecentDocumentsController }

  TBarRecentDocumentsController = class(TRecentDocumentsController)
  private
    FBarList: TdxBarListItem;
  protected
    procedure DoLoad(AConfig: TCustomIniFile); override;
    procedure DoSave(AConfig: TCustomIniFile); override;
  public
    constructor Create(ABarList: TdxBarListItem);
    procedure Add(const AFileName: string); override;
  end;

  { TfrmBarsNotepadMain }

  TfrmBarsNotepadMain = class(TfrmNotepadMain)
    bbAbout: TdxBarButton;
    bbAlignCenter: TdxBarButton;
    bbAlignLeft: TdxBarButton;
    bbAlignRight: TdxBarButton;
    bbBarsHelp: TdxBarButton;
    bbBold: TdxBarButton;
    bbBullets: TdxBarButton;
    bbCopy: TdxBarButton;
    bbCut: TdxBarButton;
    bbDockingHelp: TdxBarButton;
    bbDXDownloads: TdxBarButton;
    bbDXOnWeb: TdxBarButton;
    bbDXProducts: TdxBarButton;
    bbDXSupport: TdxBarButton;
    bbExit: TdxBarButton;
    bbFind: TdxBarButton;
    bbFont: TdxBarButton;
    bbFontColor: TdxBarButton;
    bbItalic: TdxBarButton;
    bbMyDX: TdxBarButton;
    bbNew: TdxBarButton;
    bbOpen: TdxBarButton;
    bbPaste: TdxBarButton;
    bbPrint: TdxBarButton;
    bbRedo: TdxBarButton;
    bbReplace: TdxBarButton;
    bbSave: TdxBarButton;
    bbSaveAs: TdxBarButton;
    bbSelectAll: TdxBarButton;
    bbTabbedView: TdxBarButton;
    bbUnderline: TdxBarButton;
    bbUndo: TdxBarButton;
    bsHelp: TdxBarSubItem;
    bsLookAndFeel: TdxBarSubItem;
    bsZoom: TdxBarSubItem;
    dxbFile: TdxBar;
    dxbFont: TdxBar;
    dxbFormat: TdxBar;
    dxbMain: TdxBar;
    dxbStatusBar: TdxBar;
    dxbView: TdxBar;
    liRecentDocuments: TdxBarListItem;
    liZoom: TdxBarListItem;
    pmEditor: TdxBarPopupMenu;
    siEdit: TdxBarSubItem;
    siFile: TdxBarSubItem;
    siFormat: TdxBarSubItem;
    siView: TdxBarSubItem;
    procedure bbTabbedViewClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure liZoomClick(Sender: TObject);
    procedure liRecentDocumentsClick(Sender: TObject);
  private
    procedure PopulateZoomFactors;
    procedure UpdateImageIndexes;
  protected
    function CreateRecentDocumentsController: TRecentDocumentsController; override;
    procedure DoUpdateControls(AActiveChild: TfrmNotepadChild); override;
  end;

var
  frmBarsNotepadMain: TfrmBarsNotepadMain;

implementation

{$R *.dfm}

{ TBarRecentDocumentsController }

constructor TBarRecentDocumentsController.Create(ABarList: TdxBarListItem);
begin
  inherited Create;
  FBarList := ABarList;
end;

procedure TBarRecentDocumentsController.Add(const AFileName: string);
var
  AIndex: Integer;
begin
  FBarList.Items.BeginUpdate;
  try
    AIndex := FBarList.Items.IndexOf(AFileName);
    if AIndex >= 0 then
      FBarList.Items.Move(AIndex, 0)
    else
      FBarList.Items.Add(AFileName);

    while FBarList.Items.Count > 10 do
      FBarList.Items.Delete(9);
  finally
    FBarList.Items.EndUpdate;
  end;
end;

procedure TBarRecentDocumentsController.DoLoad(AConfig: TCustomIniFile);
var
  I, ACount: Integer;
begin
  inherited DoLoad(AConfig);
  FBarList.Items.BeginUpdate;
  try
    FBarList.Items.Clear;

    ACount := AConfig.ReadInteger(ClassName, 'Count', 0);
    for I := 0 to ACount - 1 do
      FBarList.Items.Add(AConfig.ReadString(ClassName, IntToStr(I), ''));
  finally
    FBarList.Items.EndUpdate;
  end;
end;

procedure TBarRecentDocumentsController.DoSave(AConfig: TCustomIniFile);
var
  I: Integer;
begin
  inherited DoSave(AConfig);
  AConfig.WriteInteger(ClassName, 'Count', 0);
  for I := 0 to FBarList.Items.Count - 1 do
    AConfig.WriteString(ClassName, IntToStr(I), FBarList.Items[I]);
end;

{ TfrmBarsNotepadMain }

procedure TfrmBarsNotepadMain.PopulateZoomFactors;
const
  ZoomValues: array[0..8] of Integer = (50, 80, 100, 110, 120, 140, 150, 200, 400);
var
  I: Integer;
begin
  for I := Low(ZoomValues) to High(ZoomValues) do
    liZoom.Items.AddObject(IntToStr(ZoomValues[I]) + ' %', TObject(ZoomValues[I]));
end;

procedure TfrmBarsNotepadMain.UpdateImageIndexes;
begin
  bbBarsHelp.ImageIndex := 18;
  bbDockingHelp.ImageIndex := 18;
  bbDXSupport.ImageIndex := 20;
  bbDXDownloads.ImageIndex := 20;
  bbDXOnWeb.ImageIndex := 20;
  bbDXProducts.ImageIndex := 20;
  bbMyDX.ImageIndex := 20;
end;

function TfrmBarsNotepadMain.CreateRecentDocumentsController: TRecentDocumentsController;
begin
  Result := TBarRecentDocumentsController.Create(liRecentDocuments);
end;

procedure TfrmBarsNotepadMain.DoUpdateControls(AActiveChild: TfrmNotepadChild);
var
  AZoomFactor: Integer;
begin
  inherited DoUpdateControls(AActiveChild);
  bbTabbedView.Down := dxTabbedMDIManager1.Active;
  bsZoom.Visible := VisibleTodxBarVisible(AActiveChild <> nil);
  if AActiveChild <> nil then
  begin
    AZoomFactor := Round(AActiveChild.Editor.Properties.ZoomFactor * 100);
    liZoom.ItemIndex := liZoom.Items.IndexOfObject(TObject(AZoomFactor));
    bsZoom.Caption := IntToStr(AZoomFactor) + ' %';
    Editor.PopupMenu := pmEditor;
  end;
end;

procedure TfrmBarsNotepadMain.bbTabbedViewClick(Sender: TObject);
begin
  dxTabbedMDIManager1.Active := bbTabbedView.Down;
end;

procedure TfrmBarsNotepadMain.FormShow(Sender: TObject);
begin
  inherited;
  PopulateZoomFactors;
  UpdateImageIndexes;
end;

procedure TfrmBarsNotepadMain.liRecentDocumentsClick(Sender: TObject);
begin
  OpenFile(liRecentDocuments.Items[liRecentDocuments.ItemIndex]);
end;

procedure TfrmBarsNotepadMain.liZoomClick(Sender: TObject);
begin
  if FUpdatingControls = 0 then
  begin
    Editor.Properties.ZoomFactor := Integer(liZoom.Items.Objects[liZoom.ItemIndex]) / 100;
    UpdateControls;
  end;
end;

end.
