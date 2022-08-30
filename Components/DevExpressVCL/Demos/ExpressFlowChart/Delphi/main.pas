unit main;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  dxflchrt, ExtCtrls, Db, StdCtrls, Grids, DBGrids,ShellAPI, dxFcEdit,
  Menus, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, ImgList,
  MidasLib,
  dxmdaset,
{$IFDEF EXPRESSBARS}
  dxFlowChartDesigner,
{$ENDIF}
  DBClient;

type
  TMainForm = class(TForm)
    Splitter: TSplitter;
    dxFlowChart: TdxFlowChart;
    DataSource: TDataSource;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    DBGrid: TDBGrid;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    View1: TMenuItem;
    Help1: TMenuItem;
    miOpen: TMenuItem;
    miSaveAs: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    EditChart1: TMenuItem;
    miZoomIn: TMenuItem;
    miZoomOut: TMenuItem;
    N2: TMenuItem;
    miFit: TMenuItem;
    DeveloperExpressontheWeb1: TMenuItem;
    miActualSize: TMenuItem;
    N3: TMenuItem;
    miAntialiasing: TMenuItem;
    ImageList1: TcxImageList;
    Table: TdxMemData;
    TableBkColor: TIntegerField;
    TableChart: TBlobField;
    TableChartName: TStringField;
    procedure dxFlowChartDblClick(Sender: TObject);
    procedure TableAfterScroll(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnOnWebClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure DataSourceDataChange(Sender: TObject; Field: TField);
    procedure miOpenClick(Sender: TObject);
    procedure btnFitClick(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure miActualSizeClick(Sender: TObject);
    procedure TableAfterInsert(DataSet: TDataSet);
    procedure miAntialiasingClick(Sender: TObject);
    procedure TableBeforeInsert(DataSet: TDataSet);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  strict private
    procedure SaveCurrentChart;
    procedure SaveCurrentChartBackgroundColor(AColor: TColor);
  private
    FUpdate : Boolean;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.SaveCurrentChartBackgroundColor(AColor: TColor);
begin
  FUpdate := False;
  Table.Edit;
  Table.FieldByName('BkColor').AsInteger := AColor;
  Table.Post;
  FUpdate := True;
end;

procedure TMainForm.SaveCurrentChart;
var
  AStream : TMemoryStream;
begin
  FUpdate := False;
  Table.Edit;
  AStream := TMemoryStream.Create;
  try
    dxFlowChart.SaveToStream(AStream);
    AStream.Position := 0;
    TableChart.LoadFromStream(AStream);
    TableBkColor.Value := clWindow;
  finally
    AStream.Free;
  end;
  Table.Post;
  SaveCurrentChartBackgroundColor(dxFlowChart.Color);
  FUpdate := True;
end;

procedure TMainForm.dxFlowChartDblClick(Sender: TObject);
begin
  if (Table.RecordCount <> 0) and ShowFlowChartEditor(dxFlowChart, 'ExpressFlowChart Editor') then
    SaveCurrentChart;
end;

procedure TMainForm.TableAfterScroll(DataSet: TDataSet);
var AStream : TMemoryStream;
begin
  if not FUpdate then
    Exit;
  dxFlowChart.BeginUpdate;
  dxFlowChart.Clear;
  if not (TBlobField(Table.FieldByName('Chart')).isNull) then
  begin
    AStream := TMemoryStream.Create;
    TBlobField(Table.FieldByName('Chart')).SaveToStream(AStream);
    AStream.Position := 0;
    dxFlowChart.LoadFromStream(AStream);
    AStream.Free;
  end;
  dxFlowChart.Color := clWindow;
  dxFlowChart.EndUpdate;
end;

procedure TMainForm.TableBeforeInsert(DataSet: TDataSet);
begin
  if (DataSet.FieldByName('ChartName').AsString = '') and TBlobField(DataSet.FieldByName('Chart')).isNull then
    DataSet.Delete;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Table.SaveToBinaryFile('Data.bin');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  miAntialiasing.Checked := True;
  dxFlowChart.Antialiasing := miAntialiasing.Checked;
  FUpdate := True;
  Table.Open;
  Table.LoadFromBinaryFile('Data.bin');
end;

procedure TMainForm.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnOnWebClick(Sender: TObject);
begin
  ShellExecute(Handle, PChar('OPEN'), PChar('https://www.devexpress.com/products/vcl/'), Nil, Nil, SW_SHOWMAXIMIZED);
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    dxFlowChart.SaveToFile(SaveDialog.FileName);
end;

procedure TMainForm.DataSourceDataChange(Sender: TObject; Field: TField);
begin
  miOpen.Enabled := Table.RecordCount > 0;
  miSaveAs.Enabled := miOpen.Enabled;
end;

procedure TMainForm.miOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    dxFlowChart.LoadFromFile(OpenDialog.FileName);
    if Table.RecordCount <> 0 then
      SaveCurrentChart;
  end;
end;

procedure TMainForm.btnFitClick(Sender: TObject);
begin
  miFit.Checked := not miFit.Checked;
  if miFit.Checked then
  begin
    dxFlowChart.Zoom := 0;
    miZoomIn.Enabled := False;
    miZoomOut.Enabled := False;
    miActualSize.Enabled := False;
  end
  else
  begin
    dxFlowChart.Zoom := 100;
    miZoomIn.Enabled := True;
    miZoomOut.Enabled := True;
    miActualSize.Enabled := True;
  end;
end;

procedure TMainForm.btnZoomInClick(Sender: TObject);
begin
  miZoomOut.Enabled := True;
  if dxFlowChart.Zoom < 490 then
    dxFlowChart.Zoom := dxFlowChart.Zoom + 10
  else
    miZoomIn.Enabled := False;
end;

procedure TMainForm.btnZoomOutClick(Sender: TObject);
begin
  miZoomIn.Enabled := True;
  if dxFlowChart.Zoom > 20 then
    dxFlowChart.Zoom := dxFlowChart.Zoom - 10
  else
    miZoomOut.Enabled := False;
end;

procedure TMainForm.miActualSizeClick(Sender: TObject);
begin
  dxFlowChart.Zoom := 100;
end;

procedure TMainForm.miAntialiasingClick(Sender: TObject);
begin
  dxFlowChart.Antialiasing := miAntialiasing.Checked;
end;

procedure TMainForm.TableAfterInsert(DataSet: TDataSet);
begin
  SaveCurrentChartBackgroundColor(clWindow);
end;

end.
