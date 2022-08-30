unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, Grids, DBGrids, ExtCtrls, dxdborgc, dxorgchr, ComCtrls,
  Menus, StdCtrls, Spin, cxGraphics, cxControls, cxLookAndFeels, ImgList,
  cxLookAndFeelPainters, DBClient, MidasLib;

type
  TMainForm = class(TForm)
    AddChildeNode1: TMenuItem;
    AddNode1: TMenuItem;
    ColorDialog: TColorDialog;
    cxLookAndFeelController: TcxLookAndFeelController;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBTree: TdxDbOrgChart;
    DeleteNode1: TMenuItem;
    Edit1: TMenuItem;
    Exit1: TMenuItem;
    File1: TMenuItem;
    ilTree: TcxImageList;
    It3D: TMenuItem;
    ItAnimated: TMenuItem;
    ItFullCollapse: TMenuItem;
    ItFullExpand: TMenuItem;
    ItRotated: TMenuItem;
    ItZoom: TMenuItem;
    MainMenu: TMainMenu;
    N1: TMenuItem;
    Options1: TMenuItem;
    PC: TPageControl;
    RenameNode1: TMenuItem;
    Splitter1: TSplitter;
    Tree: TdxOrgChart;
    tsDBOrgChart: TTabSheet;
    tsOrgChart: TTabSheet;
    View1: TMenuItem;
    N2: TMenuItem;
    miLooknFeel: TMenuItem;
    miKindOffice11: TMenuItem;
    miKindStandard: TMenuItem;
    miKindFlat: TMenuItem;
    miKindUltraFlat: TMenuItem;
    miKindNative: TMenuItem;
    Panel1: TPanel;
    cxButton1: TButton;
    miAntialiasing: TMenuItem;
    Table1: TClientDataSet;
    Table1ID: TAutoIncField;
    Table1PARENT: TIntegerField;
    Table1NAME: TStringField;
    Table1CDATE: TDateField;
    Table1CBY: TStringField;
    Table1WIDTH: TIntegerField;
    Table1HEIGHT: TIntegerField;
    Table1TYPE: TStringField;
    Table1COLOR: TIntegerField;
    Table1IMAGE: TIntegerField;
    Table1IMAGEALIGN: TStringField;
    Table1ORDER: TIntegerField;
    Table1ALIGN: TStringField;
    Table1Align_num: TIntegerField;
    Table1Type_num: TIntegerField;
    Table1ImageAlign_num: TIntegerField;
    procedure AddChildeNode1Click(Sender: TObject);
    procedure AddNode1Click(Sender: TObject);
    procedure DBGrid1ColEnter(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DBTreeCreateNode(Sender: TObject; Node: TdxOcNode);
    procedure DeleteNode1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure It3DClick(Sender: TObject);
    procedure ItAnimatedClick(Sender: TObject);
    procedure ItFullCollapseClick(Sender: TObject);
    procedure ItFullExpandClick(Sender: TObject);
    procedure ItRotatedClick(Sender: TObject);
    procedure ItZoomClick(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure PCChange(Sender: TObject);
    procedure RenameNode1Click(Sender: TObject);
    procedure TableAfterInsert(DataSet: TDataSet);
    procedure TableALIGNChange(Sender: TField);
    procedure TableIMAGEALIGNChange(Sender: TField);
    procedure TableTYPEChange(Sender: TField);
    procedure TreeCreateNode(Sender: TObject; Node: TdxOcNode);
    procedure miKindNativeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure miAntialiasingClick(Sender: TObject);
    procedure Table1CalcFields(DataSet: TDataSet);
  private
    FAllowChanges: Boolean;
    function AddNode(AChild: Boolean): TdxOcNode;
    procedure InitializeLookAndFeelState;
  public
    function GetActiveOrgChart: TdxCustomOrgChart;
    function GetImageAlign(const AAlignName: string): TdxOcImageAlign;
    function GetNodeAlign(const AAlignName: string): TdxOcNodeAlign;
    function GetShape(const AShapeName: string): TdxOcShape;
    //
    property ActiveOrgChart: TdxCustomOrgChart read GetActiveOrgChart;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Options, dxorgced;

{$R *.DFM}

type
  TdxCustomOrgChartAccess = class(TdxCustomOrgChart);

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  InitializeLookAndFeelState;
  Table1.Open;
  DBTree.WidthFieldName  := 'Width';
  DBTree.HeightFieldName := 'Height';
  DBTree.ChAlignFieldName := 'Align_num';
  DBTree.ImAlignFieldName := 'ImageAlign_num';
  DBTree.ShapeFieldName := 'Type_num';
  DBTree.ColorFieldName := 'Color';
  PCChange(PC);
  FAllowChanges := True;
end;

function TMainForm.AddNode(AChild: Boolean): TdxOcNode;
var
  ATree: TdxCustomOrgChartAccess;
begin
  ATree := TdxCustomOrgChartAccess(ActiveOrgChart);
  if ATree.Selected = nil then
    Result := ATree.Add(nil, nil)
  else
  begin
    if AChild then
      Result := ATree.AddChild(ATree.Selected, nil)
    else
      Result := ATree.Insert(ATree.Selected, nil);
    ATree.Selected.Expanded := True;
  end;
  Result.Text := 'New topic';
  Result.Color := clWhite;
  Result.Shape := shRectangle;
  ATree.Selected := Result;
end;

function TMainForm.GetActiveOrgChart: TdxCustomOrgChart;
begin
  if PC.ActivePage = tsOrgChart then
    Result := Tree
  else
    Result := DBTree;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AddNode1Click(Sender: TObject);
begin
  Table1.DisableControls;
  try
    AddNode(False);
  finally
    Table1.EnableControls;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ShowOrgChartEditor(Tree);
  PCChange(nil);
end;

procedure TMainForm.AddChildeNode1Click(Sender: TObject);
begin
  Table1.DisableControls;
  try
    AddNode(True);
  finally
    Table1.EnableControls;
  end;
end;

procedure TMainForm.RenameNode1Click(Sender: TObject);
begin
  if TdxCustomOrgChartAccess(ActiveOrgChart).Selected <> nil then
    TdxCustomOrgChartAccess(ActiveOrgChart).ShowEditor;
end;

procedure TMainForm.DeleteNode1Click(Sender: TObject);
var
  ANode: TdxOcNode;
begin
  ANode := TdxCustomOrgChartAccess(ActiveOrgChart).Selected;
  if ANode <> nil then
    TdxCustomOrgChartAccess(ActiveOrgChart).Delete(ANode);
end;

procedure TMainForm.PCChange(Sender: TObject);
begin
  with TdxCustomOrgChartAccess(ActiveOrgChart) do
  begin
    miAntialiasing.Checked := Antialiasing;
    ItAnimated.Checked := ocAnimate in Options;
    It3D.Checked := ocRect3D in Options;
    ItRotated.Checked := Rotated;
    ItZoom.Checked := Zoom;
  end;
end;

procedure TMainForm.InitializeLookAndFeelState;
begin
  miKindOffice11.Checked := cxLookAndFeelController.Kind = lfOffice11;
  miKindStandard.Checked := cxLookAndFeelController.Kind = lfStandard;
  miKindUltraFlat.Checked := cxLookAndFeelController.Kind = lfUltraFlat;
  miKindFlat.Checked := cxLookAndFeelController.Kind = lfFlat;
  miKindNative.Checked := cxLookAndFeelController.NativeStyle;
end;

procedure TMainForm.ItZoomClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  TdxCustomOrgChartAccess(ActiveOrgChart).Zoom := TMenuItem(Sender).Checked;
end;

procedure TMainForm.miAntialiasingClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  TdxCustomOrgChartAccess(ActiveOrgChart).Antialiasing := TMenuItem(Sender).Checked;
end;

procedure TMainForm.miKindNativeClick(Sender: TObject);
begin
  cxLookAndFeelController.NativeStyle := TMenuItem(Sender).Tag = 4;
  if TMenuItem(Sender).Tag < 4 then
    cxLookAndFeelController.Kind := TcxLookAndFeelKind(TMenuItem(Sender).Tag);
end;

procedure TMainForm.ItRotatedClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  TdxCustomOrgChartAccess(ActiveOrgChart).Rotated := TMenuItem(Sender).Checked;
end;

procedure TMainForm.ItAnimatedClick(Sender: TObject);
var
  ATree: TdxCustomOrgChartAccess;
begin
  ATree := TdxCustomOrgChartAccess(ActiveOrgChart);
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  if TMenuItem(Sender).Checked then
    ATree.Options := ATree.Options + [ocAnimate]
  else
    ATree.Options := ATree.Options - [ocAnimate];
end;

procedure TMainForm.It3DClick(Sender: TObject);
var
  ATree: TdxCustomOrgChartAccess;
begin
  ATree := TdxCustomOrgChartAccess(ActiveOrgChart);
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  if TMenuItem(Sender).Checked then
    ATree.Options := ATree.Options + [ocRect3D]
  else
    ATree.Options := ATree.Options - [ocRect3D];
end;

procedure TMainForm.ItFullExpandClick(Sender: TObject);
begin
  TdxCustomOrgChartAccess(ActiveOrgChart).FullExpand;
end;

procedure TMainForm.ItFullCollapseClick(Sender: TObject);
begin
  TdxCustomOrgChartAccess(ActiveOrgChart).FullCollapse;
end;

procedure TMainForm.Options1Click(Sender: TObject);
begin
  OptionsForm.ShowModal;
end;

procedure TMainForm.DBGrid1DrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  if Column.FieldName = 'COLOR' then
  begin
    DBGrid1.Canvas.Brush.Color := Table1.FieldByName('Color').AsInteger;
    DBGrid1.Canvas.FillRect(Rect);
  end;
end;

procedure TMainForm.DBTreeCreateNode(Sender: TObject; Node: TdxOcNode);
begin
  with Node, Table1 do
  begin
    if FindField('width').AsInteger > 50 then
      Width := FindField('width').AsInteger;
    if FindField('height').AsInteger > 50 then
      Height := FindField('height').AsInteger;
    Shape := GetShape(FindField('type').AsString);
    Color := FindField('color').AsInteger;
    Node.ChildAlign := GetNodeAlign(FindField('Align').AsString);
    Node.ImageAlign := GetImageAlign(FindField('ImageAlign').AsString);
  end;
end;

procedure TMainForm.TableAfterInsert(DataSet: TDataSet);
begin
  with Table1, DBTree do
  begin
    FindField('Height').AsInteger := DefaultNodeHeight;
    FindField('Width').AsInteger := DefaultNodeWidth;
    FindField('Type').AsString := 'Rectangle';
    FindField('Color').AsInteger := clWhite;
    FindField('Image').AsInteger := -1;
    FindField('ImageAlign').AsString := 'Left-Top';
    FindField('Align').AsString := 'Center';
  end;
end;

procedure TMainForm.DBGrid1DblClick(Sender: TObject);
begin
  if TDBGrid(Sender).SelectedField.FieldName = 'COLOR' then
    if ColorDialog.Execute then
      with TDBGrid(Sender).DataSource.DataSet do
      begin
        Edit;
        FieldByName('COLOR').AsInteger := ColorDialog.Color;
        Post;
        DBTree.Selected.Color := ColorDialog.Color;
      end;
end;

procedure TMainForm.DBGrid1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    DBGrid1DblClick(Sender);
  end;
  if Key = VK_DOWN then
  begin
    Key := 0;
    TDBGrid(Sender).DataSource.DataSet.Next;
  end;
end;

procedure TMainForm.DBGrid1ColEnter(Sender: TObject);
begin
  with TDBGrid(Sender) do
    if SelectedField.FieldName = 'COLOR' then
      Options := Options - [dgEditing]
    else
      Options := Options + [dgEditing];
end;

function TMainForm.GetShape(const AShapeName: String): TdxOcShape;
const
  ShapeMap: array[TdxOcShape] of string = ('Rectange', 'Round Rect', 'Ellipse', 'Diamond');
var
  AIndex: TdxOcShape;
begin
  Result := Low(TdxOcShape);
  for AIndex := Low(TdxOcShape) to High(TdxOcShape) do
    if SameText(ShapeMap[AIndex], AShapeName) then
    begin
      Result := AIndex;
      Break;
    end;
end;

procedure TMainForm.TableTYPEChange(Sender: TField);
begin
  if Table1.State = dsEdit then
    DBTree.Selected.Shape := GetShape(Sender.AsString);
end;

function TMainForm.GetNodeAlign(const AAlignName: string): TdxOcNodeAlign;
const
  AlignMap: array[TdxOcNodeAlign] of string = ('Left', 'Center', 'Right');
var
  AIndex: TdxOcNodeAlign;
begin
  Result := Low(TdxOcNodeAlign);
  for AIndex := Low(TdxOcNodeAlign) to High(TdxOcNodeAlign) do
    if SameText(AlignMap[AIndex], AAlignName) then
    begin
      Result := AIndex;
      Break;
    end;
end;

procedure TMainForm.TableALIGNChange(Sender: TField);
begin
  if Table1.State = dsEdit then
    DBTree.Selected.ChildAlign := GetNodeAlign(Sender.AsString);
end;

function TMainForm.GetImageAlign(const AAlignName: string): TdxOcImageAlign;
const
  AlignMap: array[TdxOcImageAlign] of string = (
   'None',
   'Left-Top', 'Left-Center', 'Left-Bottom',
   'Right-Top', 'Right-Center', 'Right-Bottom',
   'Top-Left', 'Top-Center', 'Top-Right',
   'Bottom-Left', 'Bottom-Center', 'Bottom-Right');
var
  AIndex: TdxOcImageAlign;
begin
  Result := Low(TdxOcImageAlign);
  for AIndex := Low(TdxOcImageAlign) to High(TdxOcImageAlign) do
    if SameText(AlignMap[AIndex], AAlignName) then
    begin
      Result := AIndex;
      Break;
    end;
end;

procedure TMainForm.TableIMAGEALIGNChange(Sender: TField);
begin
  if Table1.State = dsEdit then
    DBTree.Selected.ImageAlign := GetImageAlign(Sender.AsString);
end;

procedure TMainForm.TreeCreateNode(Sender: TObject; Node: TdxOcNode);
begin
  with Node do
  begin
    Shape := shRectangle;
    Color := clWhite;
    Node.ChildAlign := caCenter;
    Node.ImageAlign := iaLT;
  end;
end;

procedure TMainForm.Table1CalcFields(DataSet: TDataSet);
begin
  if DataSet.FieldByName('TYPE').AsString = 'Rectangle' then
    DataSet.FieldByName('Type_num').AsInteger := 0;
  if DataSet.FieldByName('TYPE').AsString = 'Round Rect' then
    DataSet.FieldByName('Type_num').AsInteger := 1;
  if DataSet.FieldByName('TYPE').AsString = 'Ellipse' then
    DataSet.FieldByName('Type_num').AsInteger := 2;
  if DataSet.FieldByName('TYPE').AsString = 'Diamond' then
    DataSet.FieldByName('Type_num').AsInteger := 3;

  if DataSet.FieldByName('ALIGN').AsString = 'Left' then
    DataSet.FieldByName('Align_num').AsInteger := 0;
  if DataSet.FieldByName('ALIGN').AsString = 'Center' then
    DataSet.FieldByName('Align_num').AsInteger := 1;
  if DataSet.FieldByName('ALIGN').AsString = 'Right' then
    DataSet.FieldByName('Align_num').AsInteger := 2;

  if DataSet.FieldByName('IMAGEALIGN').AsString = 'None' then
    DataSet.FieldByName('ImageAlign_num').AsInteger := 0;
  if DataSet.FieldByName('IMAGEALIGN').AsString = 'Left-Top' then
    DataSet.FieldByName('ImageAlign_num').AsInteger := 1;
  if DataSet.FieldByName('IMAGEALIGN').AsString = 'Left-Center' then
    DataSet.FieldByName('ImageAlign_num').AsInteger := 2;
  if DataSet.FieldByName('IMAGEALIGN').AsString = 'Left-Bottom' then
    DataSet.FieldByName('ImageAlign_num').AsInteger := 3;
  if DataSet.FieldByName('IMAGEALIGN').AsString = 'Right-Top' then
    DataSet.FieldByName('ImageAlign_num').AsInteger := 4;
  if DataSet.FieldByName('IMAGEALIGN').AsString = 'Right-Center' then
    DataSet.FieldByName('ImageAlign_num').AsInteger := 5;
  if DataSet.FieldByName('IMAGEALIGN').AsString = 'Right-Bottom' then
    DataSet.FieldByName('ImageAlign_num').AsInteger := 6;
  if DataSet.FieldByName('IMAGEALIGN').AsString = 'Top-Left' then
    DataSet.FieldByName('ImageAlign_num').AsInteger := 7;
  if DataSet.FieldByName('IMAGEALIGN').AsString = 'Top-Center' then
    DataSet.FieldByName('ImageAlign_num').AsInteger := 8;
  if DataSet.FieldByName('IMAGEALIGN').AsString = 'Top-Right' then
    DataSet.FieldByName('ImageAlign_num').AsInteger := 9;
  if DataSet.FieldByName('IMAGEALIGN').AsString = 'Bottom-Left' then
    DataSet.FieldByName('ImageAlign_num').AsInteger := 10;
  if DataSet.FieldByName('IMAGEALIGN').AsString = 'Bottom-Center' then
    DataSet.FieldByName('ImageAlign_num').AsInteger := 11;
  if DataSet.FieldByName('IMAGEALIGN').AsString = 'Bottom-Right' then
    DataSet.FieldByName('ImageAlign_num').AsInteger := 12;
end;

end.
