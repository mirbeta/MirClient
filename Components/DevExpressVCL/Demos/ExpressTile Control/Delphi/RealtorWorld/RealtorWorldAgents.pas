unit RealtorWorldAgents;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, DB,
  cxLookAndFeelPainters, dxCustomTileControl, dxTileControl, RealtorWorldDM,
  cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit,
  cxDBData, DBClient, cxGridChartView, cxGridDBChartView, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxClasses,
  cxGridLevel, cxGrid, ExtCtrls, cxTextEdit, cxCurrencyEdit, cxImage,
  cxMemo, cxSplitter, RealtorWorldBaseFrame, MidasLib;

type
  TfrmAgents = class(TfrmBase)
    tcAgents: TdxTileControl;
    pnDetailSite: TPanel;
    cxgDetailChartLevel1: TcxGridLevel;
    cxgDetailChart: TcxGrid;
    cxgDetailInfo: TcxGrid;
    cxGridDBTableView1: TcxGridDBTableView;
    cxGridLevel1: TcxGridLevel;
    cxgDetailChartDBChartView1: TcxGridDBChartView;
    cxGridDBTableView1Address: TcxGridDBColumn;
    cxGridDBTableView1Beds: TcxGridDBColumn;
    cxGridDBTableView1Baths: TcxGridDBColumn;
    cxGridDBTableView1HouseSize: TcxGridDBColumn;
    cxGridDBTableView1Price: TcxGridDBColumn;
    cxGridDBTableView1Photo: TcxGridDBColumn;
    cxgNorthEastSeries: TcxGridDBChartSeries;
    cxgMidWestSeries: TcxGridDBChartSeries;
    cxgSouthSeries: TcxGridDBChartSeries;
    cxgWestSeries: TcxGridDBChartSeries;
    cxSplitter1: TcxSplitter;
    dsChart: TDataSource;
    cdsChart: TClientDataSet;
    cdsChartMidWest: TIntegerField;
    cdsChartNorthEast: TIntegerField;
    cdsChartSouth: TIntegerField;
    cdsChartWest: TIntegerField;
    cdsChartAgentID: TIntegerField;
    cxStyleRepository1: TcxStyleRepository;
    cxStyle1: TcxStyle;
    cdsChartDate: TIntegerField;
    procedure AA(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean);
    procedure cxSplitter2BeforeClose(Sender: TObject;
      var AllowClose: Boolean);
  private
    procedure InitializeFrame;
    procedure InitializeChartDataSet;
    procedure OnItemClick(Sender: TdxTileControlItem);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SelectItem(APhotoID, AAgentID: Integer); override;
  end;

implementation

{$R *.dfm}

uses
  RealtorWorldMain;

constructor TfrmAgents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitializeFrame;
  InitializeChartDataSet;
end;

procedure TfrmAgents.SelectItem(APhotoID, AAgentID: Integer);
var
  I: Integer;
begin
  for I := 0 to tcAgents.Items.Count - 1 do
    if tcAgents.Items[I].Tag = AAgentID then
    begin
      tcAgents.Items[I].MakeVisible;
      tcAgents.Items[I].Click;
      Break;
    end;
end;

procedure TfrmAgents.InitializeFrame;
var
  AItem: TdxTileControlItem;
  dsAgents: TDataSet;
begin
  tcAgents.BeginUpdate;
  try
    dsAgents := DMRealtorWorld.clHomesAndAgents;
    dsAgents.First;
    while not dsAgents.EOF do
    begin
      AItem := tcAgents.CreateItem(True);
      AItem.Glyph.Image.LoadFromFieldValue(dsAgents.FieldByName('Photo').Value);
      AItem.Style := frmRealtorWorld.tlAgents.Style;
      AItem.Glyph.Align := oaMiddleRight;
      AItem.Glyph.Image.Scale(70, 100);
      AItem.Tag := dsAgents.FieldByName('ID').AsInteger;
      AItem.Style.Font.Size := 13;
      AItem.Text1.Value := dsAgents.FieldByName('FirstName').AsString + ' ' + dsAgents.FieldByName('LastName').AsString;
      AItem.Text1.IndentHorz := 10;
      AItem.Text1.IndentVert := 10;
      AItem.Text2.Value := dsAgents.FieldByName('Phone').AsString;
      AItem.Text2.Align := oaTopLeft;
      AItem.Text2.IndentHorz := 10;
      AItem.Text2.IndentVert := 30;
      AItem.OnClick := OnItemClick;
      dsAgents.Next;
    end;
  finally
    tcAgents.EndUpdate;
  end;
end;

procedure TfrmAgents.OnItemClick(Sender: TdxTileControlItem);
begin
  DMRealtorWorld.clHomesDetail.Filter := 'AgentID=' + IntToStr(Sender.Tag);
  cdsChart.Filter := 'AgentID=' + IntToStr(Sender.Tag);
  DMRealtorWorld.clHomesDetail.First;
end;

procedure TfrmAgents.InitializeChartDataSet;
var
  AYear, I: Integer;
begin
  cdsChart.DisableControls;
  try
    for I := 1 to 6 do
      for AYear := 2003 to 2012 do
      begin
       cdsChart.Append;
       cdsChartDate.Value := AYear;
       cdsChartMidWest.Value := Random(20) + 4;
       cdsChartNorthEast.Value := Random(20) + 5;
       cdsChartSouth.Value := Random(20) + 2;
       cdsChartWest.Value := Random(20) + 3;
       cdsChartAgentID.Value := I;
       cdsChart.Post;
      end;
  finally
    cdsChart.EnableControls;
  end;
end;

procedure TfrmAgents.AA(Sender: TObject; var NewWidth, NewHeight: Integer;
  var Resize: Boolean);
begin
    //
end;

procedure TfrmAgents.cxSplitter2BeforeClose(Sender: TObject;
  var AllowClose: Boolean);
begin
  AllowClose := False;
end;

initialization
  RegisterFrame(IDAgents, TfrmAgents);

end.
