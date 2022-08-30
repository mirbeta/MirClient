unit IssueListStyles;

interface

uses
  Windows, Messages, SysUtils, Dialogs, StdCtrls, cxButtons, Classes,
  Controls , Graphics, Forms,
  IssueListStyleData, cxStyles, cxGridTableView, cxLookAndFeelPainters, cxListBox;

type
  TIssueListStylesForm = class(TForm)
    lbPredefinedStyleSheets: TcxListBox;
    btnEdit: TcxButton;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    btnClear: TcxButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure cxButton1Click(Sender: TObject);
    procedure cxButton2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnClearClick(Sender: TObject);
    procedure lbPredefinedStyleSheetsClick(Sender: TObject);
  private
    FStyleData: TdmStyles;
    procedure PopulateStylesList;
    procedure LoadStyleSheets(const AFileName: TFileName);
    procedure SaveStyleSheets(const AFileName: TFileName);
    procedure UpdateStyleAndController;
    procedure SetStyleSheet;
    procedure ResetStyleSheet;
    procedure SelectStyleFromList;
    procedure ResetStyleAndController;
  public
    SelectedStyleSheet: TcxGridTableViewStyleSheet;
    property StyleData: TdmStyles read FStyleData;
  end;

implementation

uses
  cxStyleSheetEditor, IssueListData, IssueListMain, IssueListGrid,
  cxGridStyleSheetsPreview;

{$R *.dfm}

procedure TIssueListStylesForm.FormCreate(Sender: TObject);
begin
  FStyleData := TdmSTyles.Create(nil);
  SelectedStyleSheet := nil;
  PopulateStylesList;
  SelectStyleFromList;
end;

procedure TIssueListStylesForm.FormDestroy(Sender: TObject);
begin
  FStyleData.Free;
end;

procedure TIssueListStylesForm.PopulateStylesList;
var
  I:Integer;
begin
  with FStyleData.strepPredefined do
  begin
    lbPredefinedStyleSheets.Clear;
    for I := 0 to StyleSheetCount - 1 do
      lbPredefinedStyleSheets.Items.AddObject(StyleSheets[I].Caption, StyleSheets[I]);
    lbPredefinedStyleSheets.ItemIndex := 0;
  end;
end;


procedure TIssueListStylesForm.btnEditClick(Sender: TObject);
begin
  with lbPredefinedStyleSheets do
    ShowcxStyleSheetEditor(TcxGridTableViewStyleSheet(Items.Objects[ItemIndex]), nil);
end;


procedure TIssueListStylesForm.cxButton1Click(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
    begin
      LoadStyleSheets(FileName);
      SelectStyleFromList;
    end;
end;

procedure TIssueListStylesForm.cxButton2Click(Sender: TObject);
begin
  with SaveDialog do
    if Execute then
      SaveStyleSheets(FileName);
end;

procedure TIssueListStylesForm.LoadStyleSheets(const AFileName: TFileName);
begin
  StyleData.strepPredefined.Clear;
  StyleData.strepPredefined.ClearStyleSheets;

  LoadStyleSheetsFromIniFile(AFileName, StyleData.strepPredefined,
    TcxGridTableViewStyleSheet);
  PopulateStylesList;
end;

procedure TIssueListStylesForm.SaveStyleSheets(const AFileName: TFileName);
var
  AList: TList;

  procedure PopulateStyleSheetsList(const AList: TList);
  var
   I: Integer;
  begin
    if AList <> nil then
    begin
      AList.Clear;
      with StyleData.strepPreDefined do
       for I:= 0 to StyleSheetCount - 1 do
          AList.Add(StyleSheets[I]);
    end;
  end;

begin
  AList := TList.Create;
  try
    PopulateStyleSheetsList(AList);
    SaveStyleSheetsToIniFile(AFileName, AList);
   finally
      AList.Free;
    end;
end;

procedure TIssueListStylesForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TIssueListStylesForm.UpdateStyleAndController;
begin
  with dmMain, SelectedStyleSheet.Styles do
  begin
    if ContentOdd <> nil then
      edstcMain.Style.Color := ContentOdd.Color;
    if (Selection <> nil) and (stSelected <> nil) then
    begin
      stSelected.Color := Selection.Color;
      stSelected.Font := Selection.Font;
    end;
  end;
end;

procedure TIssueListStylesForm.ResetStyleAndController;
begin
  with dmMain do
  begin
    edstcMain.Style.Color := clWindow;
    begin
      stSelected.Color := stBlue.Color;
      stSelected.Font := stBlue.Font;
    end;
  end;
end;

procedure TIssueListStylesForm.SetStyleSheet;
begin
  with IssueListMainForm.GridForm do
  begin
    cxGrid.BeginUpdate;
    try
      dmMain.strepMain.StyleSheets[0].CopyFrom(SelectedStyleSheet);
      tvProjects.DataController.ClearDetails;  // refresh detail level
      tvDepartments.DataController.ClearDetails;
    finally
      cxGrid.EndUpdate;
    end;
  end;
  UpdateStyleAndController;
end;

procedure TIssueListStylesForm.btnClearClick(Sender: TObject);
begin
  ResetStyleSheet;
  ResetStyleAndController;
end;

procedure TIssueListStylesForm.ResetStyleSheet;

   procedure ResetStyleSheetStyles;
   begin
      with dmMain.ssTableStyles.Styles do
      begin
        Background := nil;
        Content := nil;
        ContentEven  := nil;
        ContentOdd := nil;
        FilterBox := nil;
        Footer := nil;
        Group := nil;
        GroupByBox := nil;
        Header := nil;
        Inactive := nil;
        IncSearch := nil;
        Indicator := nil;
        Preview := nil;
        Selection := nil;
      end;
   end;

begin
  with IssueListMainForm.GridForm do
  begin
    cxGrid.BeginUpdate;
    try
     ResetStyleSheetStyles;
     tvProjects.DataController.ClearDetails;  // refresh detail level
     tvDepartments.DataController.ClearDetails;
    finally
      cxGrid.EndUpdate;
    end;
  end;
end;

procedure TIssueListStylesForm.SelectStyleFromList;
begin
  with lbPredefinedStyleSheets do
    SelectedStyleSheet := TcxGridTableViewStyleSheet(Items.Objects[ItemIndex]);
  SetStyleSheet;
  UpdateStyleAndController;
end;

procedure TIssueListStylesForm.lbPredefinedStyleSheetsClick(
  Sender: TObject);
begin
  if TcxListBox(Sender).Items.Count > 0 then
    SelectStyleFromList;
end;

end.
