unit StylesMultiDemoMain;

interface

uses
  Windows, Messages, Forms, SysUtils, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxEdit, DB, cxDBData, Dialogs,Classes, ActnList, ImgList, Controls, Menus,
  StdCtrls, ExtCtrls, ShellAPI, cxButtons, cxData, cxClasses, cxControls,
  ComCtrls, cxLookAndFeelPainters, cxContainer, cxRadioGroup, cxListBox, cxTL,
  DemoBasicMain, cxLookAndFeels, cxMaskEdit, cxMemo, cxCurrencyEdit,
  cxCheckBox, cxDBLookupComboBox, cxInplaceContainer, cxDBTL, cxTLData,
  cxTextEdit, cxCalc;

type
  TcxStyleRepositoryType = (shtNone, shtPredefined, shtUserDefined);

  TStylesMultiDemoMainForm = class(TDemoBasicMainForm)
    pnlLeft: TPanel;
    Splitter: TSplitter;
    actSaveToFile: TAction;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    actLoadFromFile: TAction;
    gbUserDefined: TGroupBox;
    gbPredefined: TGroupBox;
    btnLoad: TcxButton;
    btnSave: TcxButton;
    btnEdit: TcxButton;
    actEditStyleSheet: TAction;
    pnlCurrentStyleSheet: TPanel;
    cxDBTreeList: TcxDBTreeList;
    cxDBTreeListID: TcxDBTreeListColumn;
    cxDBTreeListPARENTID: TcxDBTreeListColumn;
    cxDBTreeListNAME: TcxDBTreeListColumn;
    cxDBTreeListPHONE: TcxDBTreeListColumn;
    cxDBTreeListFAX: TcxDBTreeListColumn;
    cxDBTreeListBUDGET: TcxDBTreeListColumn;
    cxDBTreeListVACANCY: TcxDBTreeListColumn;
    cxDBTreeListManager: TcxDBTreeListColumn;
    cxDBTreeListManagerPhone: TcxDBTreeListColumn;
    cxDBTreeListManagerEmail: TcxDBTreeListColumn;
    cxDBTreeListManagerAdress: TcxDBTreeListColumn;
    tlStyleSheets: TcxTreeList;
    clnRadio: TcxTreeListColumn;
    clnGroupName: TcxTreeListColumn;
    procedure FormCreate(Sender: TObject);
    procedure actSaveToFileExecute(Sender: TObject);
    procedure actLoadFromFileExecute(Sender: TObject);
    procedure actEditStyleSheetExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure tlStyleSheetsIsGroupNode(Sender: TcxCustomTreeList;
      ANode: TcxTreeListNode; var IsGroup: Boolean);
    procedure tlStyleSheetsStylesGetContentStyle(Sender: TcxCustomTreeList;
      AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
    procedure tlStyleSheetsStylesGetNodeIndentStyle(Sender: TcxCustomTreeList;
      ANode: TcxTreeListNode; ALevel: Integer; var AStyle: TcxStyle);
    procedure tlStyleSheetsSelectionChanged(Sender: TObject);
    procedure cxDBTreeListInitInsertingRecord(Sender: TcxCustomDBTreeList;
      AFocusedNode: TcxDBTreeListNode; var AHandled: Boolean);
    procedure cxDBTreeListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    function GetCurrentStyleSheet: TcxTreeListStyleSheet;
    procedure CreateStyleSheetsList(AStyleRepositoryType: TcxStyleRepositoryType);
    procedure UpdateGridStyleSheets(const AStyleSheet: TcxTreeListStyleSheet);
    procedure ChangeVisibility(AType: TcxStyleRepositoryType);
    procedure ClearUserDefinedStyleSheets;
    procedure LoadUserDefinedStyleSheets(AFileName: TFileName);
    procedure SaveUserDefinedStyleSheets(AFileName: TFileName);
    procedure SelectFistChild(AStyleRepositoryType: TcxStyleRepositoryType);
  end;

var
  StylesMultiDemoMainForm: TStylesMultiDemoMainForm;

implementation

uses
  StylesMultiDemoData, cxStyleSheetEditor, cxTLStyleSheetPreview;

{$R *.dfm}

procedure TStylesMultiDemoMainForm.FormCreate(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}
  inherited;
  with StylesMultiDemoDataDM do
  begin
    CreateStyleSheetsList(shtUserDefined);
    CreateStyleSheetsList(shtPredefined);
  end;
  SelectFistChild(shtPredefined);
  cxDBTreeList.FullExpand;

//}
end;

procedure TStylesMultiDemoMainForm.UpdateGridStyleSheets(const AStyleSheet: TcxTreeListStyleSheet);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  if GetCurrentStyleSheet = AStyleSheet then Exit;
  cxDBTreeList.Styles.StyleSheet := AStyleSheet;
  if AStyleSheet <> nil then
    pnlCurrentStyleSheet.Caption := AStyleSheet.Caption
  else
    pnlCurrentStyleSheet.Caption := 'None';
  cxDBTreeList.Update;
//}
end;

procedure TStylesMultiDemoMainForm.actSaveToFileExecute(Sender: TObject);
begin
  with SaveDialog do
    if Execute then
      SaveUserDefinedStyleSheets(FileName);
end;

procedure TStylesMultiDemoMainForm.actLoadFromFileExecute(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
    begin
      LoadUserDefinedStyleSheets(FileName);
      SelectFistChild(shtUserDefined);
    end;
end;

procedure TStylesMultiDemoMainForm.ChangeVisibility(AType: TcxStyleRepositoryType);
begin
  btnEdit.Enabled := AType = shtUserDefined;
  btnSave.Enabled := AType = shtUserDefined;
end;

procedure TStylesMultiDemoMainForm.actEditStyleSheetExecute(
  Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  ShowcxStyleSheetEditor(GetCurrentStyleSheet, nil);

//}
end;

procedure TStylesMultiDemoMainForm.FormShow(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code

  ShowMessage('WARNING: tutorial not completed. First, please apply the steps '+
              'shown in the StylesMultiDemo.doc file');

//}
end;

function TStylesMultiDemoMainForm.GetCurrentStyleSheet: TcxTreeListStyleSheet;
begin
  Result := TcxTreeListStyleSheet(cxDBTreeList.Styles.StyleSheet);
end;

procedure TStylesMultiDemoMainForm.LoadUserDefinedStyleSheets(AFileName: TFileName);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  UpdateGridStyleSheets(nil);
  ClearUserDefinedStyleSheets;

  LoadStyleSheetsFromIniFile(AFileName, StylesMultiDemoDataDM.strepUserDefined,
    TcxTreeListStyleSheet);

  CreateStyleSheetsList(shtUserDefined);
//}
end;

procedure TStylesMultiDemoMainForm.SaveUserDefinedStyleSheets(AFileName: TFileName);
var
  AList: TList;
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  AList := TList.Create;
  try
    PopulateStyleSheetsList(AList);
    SaveStyleSheetsToIniFile(AFileName, AList);
   finally
      AList.Free;
    end;

//}
end;

procedure TStylesMultiDemoMainForm.ClearUserDefinedStyleSheets;
begin
  with StylesMultiDemoDataDM.strepUserDefined do
  begin
    Clear;
    ClearStyleSheets;
  end;
end;

procedure TStylesMultiDemoMainForm.FormActivate(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFileDir(Application.ExeName);
  SaveDialog.InitialDir := OpenDialog.InitialDir;
end;

procedure TStylesMultiDemoMainForm.tlStyleSheetsIsGroupNode(
  Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var IsGroup: Boolean);
begin
  if (ANode.Level = 0) and (ANode.Index <> Integer(shtNone)) then
   IsGroup := True;
end;

procedure TStylesMultiDemoMainForm.tlStyleSheetsStylesGetContentStyle(
  Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
begin
  with StylesMultiDemoDataDM do
    if ANode.Level = 0 then
      AStyle := styGroupNode
    else
      AStyle := styOptionalNode;
end;

procedure TStylesMultiDemoMainForm.CreateStyleSheetsList(
  AStyleRepositoryType: TcxStyleRepositoryType);
var
  I: Integer;
  ANode: TcxTreeListNode;
  AStyleRepository: TcxStyleRepository;
begin
  if AStyleRepositoryType = shtNone then
    Exit;
  with StylesMultiDemoDataDM do
    if AStyleRepositoryType = shtUserDefined then
      AStyleRepository := strepUserDefined
    else
      AStyleRepository := strepPredefined;
  with AStyleRepository do
  begin
    ANode := tlStyleSheets.Root.Root[Integer(AStyleRepositoryType)];
    ANode.DeleteChildren;
    for I := 0 to StyleSheetCount - 1 do
      with ANode.AddChild do
      begin
        Values[clnGroupName.ItemIndex] := StyleSheets[I].Caption;
        Data := StyleSheets[I];
      end;
  end;
end;
procedure TStylesMultiDemoMainForm.SelectFistChild(
  AStyleRepositoryType: TcxStyleRepositoryType);
var
  ANode: TcxTreeListNode;
begin
  ANode := tlStyleSheets.Root.Root[Integer(AStyleRepositoryType)];
  if ANode.Count > 0 then
  begin
    ANode[0].Focused := True;
    ANode[0].MakeVisible;
  end;
end;

procedure TStylesMultiDemoMainForm.cxDBTreeListInitInsertingRecord(
  Sender: TcxCustomDBTreeList; AFocusedNode: TcxDBTreeListNode;
  var AHandled: Boolean);
begin
  if AFocusedNode <> nil then
    StylesMultiDemoDataDM.SetParentValue(AFocusedNode.ParentKeyValue);
end;

procedure TStylesMultiDemoMainForm.cxDBTreeListDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited;
  //
end;

procedure TStylesMultiDemoMainForm.tlStyleSheetsSelectionChanged(
  Sender: TObject);
var
  I: Integer;
  ANode: TcxTreeListNode; 
begin
  if tlStyleSheets.SelectionCount > 0 then
  begin
    ANode := tlStyleSheets.Selections[0];
    if not ANode.IsGroupNode then
    begin
      tlStyleSheets.BeginUpdate;
      try
        for I := 0 to tlStyleSheets.AbsoluteCount - 1 do
          tlStyleSheets.AbsoluteItems[I].Values[
            clnRadio.ItemIndex] := I = ANode.AbsoluteIndex;
      finally
        tlStyleSheets.EndUpdate;
      end;
      UpdateGridStyleSheets(TcxTreeListStyleSheet(ANode.Data));
      ChangeVisibility(TcxStyleRepositoryType(ANode.Parent.Index));
    end;
  end;
end;

procedure TStylesMultiDemoMainForm.tlStyleSheetsStylesGetNodeIndentStyle(
  Sender: TcxCustomTreeList; ANode: TcxTreeListNode; ALevel: Integer;
  var AStyle: TcxStyle);
begin
  AStyle := Sender.Styles.Background;
end;

end.
