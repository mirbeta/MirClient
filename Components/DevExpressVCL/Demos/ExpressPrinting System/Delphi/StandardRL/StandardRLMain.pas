unit StandardRLMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, checklst, StdCtrls, Grids, ComCtrls, ShellAPI, ToolWin,
  dxPSChLbxLnk, dxPSGrLnks, dxPSLbxLnk, dxPSStdGrLnk, dxPSCore, dxPSRELnk,
  dxPSTVLnk, ExtCtrls, dxPSGlbl, dxPSUtl, dxPSEngn, dxPrnPg, dxBkgnd,
  dxWrap, dxPrnDev, ImgList, dxPSCompsProvider, dxPSFillPatterns,
  dxPSEdgePatterns, dxPSBaseGridLnk, DemoBasicMain, dxPSPDFExportCore,
  dxPSPDFExport, cxDrawTextUtils, dxPSPrVwStd, dxPScxEditorProducers,
  dxPScxExtEditorProducers, dxPScxPageControlProducer, ActnList,
  dxPSContainerLnk;

type
  TStandardRLMainForm = class(TDemoBasicMainForm)
    ilFlags: TImageList;
    ilFontImages: TImageList;
    PageControl: TPageControl;
    tsStringGrid: TTabSheet;
    StringGrid: TStringGrid;
    tsDrawGrid: TTabSheet;
    DrawGrid: TDrawGrid;
    tsListBox: TTabSheet;
    FontsList: TListBox;
    tsCheckListBox: TTabSheet;
    CountryCodeList: TCheckListBox;
    tsTreeView: TTabSheet;
    Panel1: TPanel;
    Label1: TLabel;
    cbDrives: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    TreeView: TTreeView;
    tsRichEdit: TTabSheet;
    Editor: TRichEdit;
    ilButtons: TImageList;
    dxComponentPrinter1Link1: TdxStringGridReportLink;
    dxComponentPrinter1Link2: TdxDrawGridReportLink;
    dxComponentPrinter1Link3: TdxListBoxReportLink;
    dxComponentPrinter1Link4: TdxCheckListBoxReportLink;
    dxComponentPrinter1Link5: TdxTreeViewReportLink;
    dxComponentPrinter1Link6: TdxRichEditReportLink;
    procedure DrawGridDrawCell(Sender: TObject; Col, Row: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FontsListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure PageControlChange(Sender: TObject);
    procedure dxComponentPrinterLink2CustomDrawCell(
      Sender: TBasedxReportLink; ACol, ARow: Integer; ACanvas: TCanvas;
      ABoundsRect, AClientRect: TRect);
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeViewExpanded(Sender: TObject; Node: TTreeNode);
    procedure TreeViewCollapsed(Sender: TObject; Node: TTreeNode);
    procedure cbDrivesChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    TtfBitmap, DevBitmap, SysBitmap : TBitmap;
    procedure DrawFlag(Row, Col: Integer; Rect: TRect; ACanvas: TCanvas);
    procedure BuildTree(APath: string; AItem: TTreeNode);
  public
    { Public declarations }
  end;

var
  StandardRLMainForm: TStandardRLMainForm;

implementation

{$R *.DFM}

function EnumFontFamProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
begin
  case FontType of
    TRUETYPE_FONTTYPE: TStrings(Data).AddObject(LogFont.lfFaceName, Pointer(StandardRLMainForm.TtfBitmap));
    RASTER_FONTTYPE: TStrings(Data).AddObject(LogFont.lfFaceName, Pointer(StandardRLMainForm.SysBitmap));
    DEVICE_FONTTYPE: TStrings(Data).AddObject(LogFont.lfFaceName, Pointer(StandardRLMainForm.DevBitmap));
  end;
  Result := 1;
end;

procedure TStandardRLMainForm.DrawGridDrawCell(Sender: TObject; Col, Row: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  if gdSelected in State then
    ilFlags.DrawingStyle := dsSelected
  else
    ilFlags.DrawingStyle := dsNormal;
  DrawFlag(Col, Row, Rect, TDrawGrid(Sender).Canvas);
end;

procedure TStandardRLMainForm.FormCreate(Sender: TObject);
  procedure BuildDriverList;
  var
    I: Integer;
    Drive: PChar;
  begin
    for I := 0 to 31 do
    begin
      if Boolean(GetLogicalDrives and (1 SHL I)) then
      begin
        Drive := PChar(CHR(65 + I) + ':\');
        cbDrives.Items.Add(Drive);
        if I = 2 then cbDrives.ItemIndex := cbDrives.Items.Count - 1;
      end;
    end;
  end;

var
  DC: HDC;
  i, j: Integer;
begin
  inherited;
  TtfBitmap := TBitmap.Create;
  ilFontImages.GetBitmap(0,TtfBitmap);
  DevBitmap := TBitmap.Create;
  ilFontImages.GetBitmap(2,DevBitmap);
  SysBitmap := TBitmap.Create;
  ilFontImages.GetBitmap(1,SysBitmap);
  DC := GetDC(0);
  try
    EnumFontFamilies(DC, nil, @EnumFontFamProc, Integer(FontsList.Items));
  finally
    ReleaseDC(0, DC);
  end;
  for i := 1 to 10 do
  begin
    StringGrid.Cells[0,i] := IntToStr(i);
    for j := 1 to 10 do
    begin
      StringGrid.Cells[j,0] := IntToStr(j);
      StringGrid.Cells[j,i] := IntToStr(i*j);
    end;
  end;
  Randomize;
  for i :=0 to CountryCodeList.Items.Count - 1 do
  begin
    CountryCodeList.Checked[i] := Boolean(Random(2));
  end;
  try
    Editor.Lines.LoadFromFile(ExtractFilePath(Application.ExeName)+'overview.rtf');
  except
  end;
  BuildDriverList;
  cbDrivesChange(cbDrives);
  dxComponentPrinter.CurrentLink := dxComponentPrinter.ReportLink[PageControl.ActivePage.PageIndex];
end;

procedure TStandardRLMainForm.FormDestroy(Sender: TObject);
begin
  if TtfBitmap <> nil then
    TtfBitmap.Free;
  if DevBitmap <> nil then
    DevBitmap.Free;
  if SysBitmap <> nil then
    SysBitmap.Free;
end;

procedure TStandardRLMainForm.FontsListDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  Offset : Integer;
  Bitmap : TBitmap;
begin
  with (Control as TListBox).Canvas do
  begin
    FillRect(Rect);
    Offset := 2;
    Bitmap := TBitmap(TListBox(Control).Items.Objects[Index]);
    if Bitmap <> nil then
    begin
      Draw(Rect.Left+2,Rect.Top,Bitmap);
      Offset := Bitmap.width + 6;
    end;
    Font.Name := (Control as TListBox).Items.Strings[Index];
    TextOut(Rect.Left+Offset, Rect.Top, (Control as TListBox).Items.Strings[Index]);
  end;
end;

procedure TStandardRLMainForm.PageControlChange(Sender: TObject);
begin
  dxComponentPrinter.CurrentLink := dxComponentPrinter.ReportLink[TPageControl(Sender).ActivePage.PageIndex];
end;

procedure TStandardRLMainForm.DrawFlag(Row, Col: Integer; Rect: TRect; ACanvas: TCanvas);
var
  Bitmap: TBitmap;
begin
  with ACanvas do
  begin
    Pen.Color := clWhite;
    MoveTo(Rect.Left,Rect.Bottom);
    LineTo(Rect.Right,Rect.Bottom);
    LineTo(Rect.Right,Rect.Top-1);
    Pen.Color := clBtnShadow;
    MoveTo(PenPos.x - 1,PenPos.y + 1);
    LineTo(Rect.Left,Rect.Top);
    LineTo(Rect.Left,Rect.Bottom);
    Pen.Color := clBtnFace;
    MoveTo(Rect.Left + 1,Rect.Bottom - 1);
    LineTo(Rect.Right - 1,Rect.Bottom - 1);
    LineTo(Rect.Right - 1,Rect.Top);
    MoveTo(PenPos.x - 1,PenPos.y + 1);
    Pen.Color := clBlack;
    LineTo(Rect.Left + 1,Rect.Top + 1);
    LineTo(Rect.Left + 1,Rect.Bottom - 1);
    Bitmap := TBitmap.Create;
    ilFlags.GetBitmap(Col+Row*7,Bitmap);
    if Bitmap <> nil then
    begin
      StretchDraw(Bounds(Rect.Left+2, Rect.Top+2, Rect.Right-Rect.Left-4, Rect.Bottom-Rect.Top-4),Bitmap);
      Bitmap.Free;
    end;
  end;
end;

procedure TStandardRLMainForm.dxComponentPrinterLink2CustomDrawCell(
  Sender: TBasedxReportLink; ACol, ARow: Integer; ACanvas: TCanvas;
  ABoundsRect, AClientRect: TRect);
begin
  ilFlags.DrawingStyle := dsNormal;
  DrawFlag(ACol, ARow, AClientRect, ACanvas);
end;

procedure TStandardRLMainForm.BuildTree(APath: string; AItem: TTreeNode);
var
  SearchRec, Dummy: TSearchRec;
  Found: Integer;
  Node: TTreeNode;
begin
  Found := FindFirst(APath + '*.*', faAnyFile, SearchRec);
  try
    while Found = 0 do
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if AItem <> nil then
          Node := TreeView.Items.AddChild(AItem, SearchRec.Name)
        else
          Node := TreeView.Items.Add(nil, SearchRec.Name);
        Node.StateIndex := 3;
        if SearchRec.Attr and faDirectory <> 0 then
        begin
          Node.HasChildren := FindFirst(APath + SearchRec.Name + '\*.*', faAnyFile, Dummy) = 0;
          Node.StateIndex := 1;
        end;
      end;
      Found := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

procedure TStandardRLMainForm.TreeViewExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);

  function GetNodeFullPath(Node: TTreeNode): String;
  begin
    Result := '';
    while Node <> nil do
    begin
      if Node.HasChildren then
        Result := '\' + Result;
      Result := Node.Text + Result;
      Node := Node.Parent;
    end;
    Result := cbDrives.Text + Result;
  end;

begin
  if Node.HasChildren and (Node.Count = 0) then
    BuildTree(GetNodeFullPath(Node), Node);
end;

procedure TStandardRLMainForm.TreeViewExpanded(Sender: TObject; Node: TTreeNode);
begin
  Node.StateIndex := 2;
end;

procedure TStandardRLMainForm.TreeViewCollapsed(Sender: TObject; Node: TTreeNode);
begin
  Node.StateIndex := 1;
end;

procedure TStandardRLMainForm.cbDrivesChange(Sender: TObject);
begin
  with TreeView do
  begin
    Cursor := crHourGlass;
    Items.BeginUpdate;
    Items.Clear;
    BuildTree(TComboBox(Sender).Text, nil);
    Items.EndUpdate;
    Cursor := crDefault;
  end;  
end;

procedure TStandardRLMainForm.Button1Click(Sender: TObject);
begin
  TreeView.Items.BeginUpdate;
  TreeView.FullExpand;
  TreeView.Items.EndUpdate;
end;

procedure TStandardRLMainForm.Button2Click(Sender: TObject);
begin
  TreeView.FullCollapse;
end;

end.
