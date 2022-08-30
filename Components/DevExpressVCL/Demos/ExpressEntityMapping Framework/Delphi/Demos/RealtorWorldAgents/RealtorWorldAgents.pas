unit RealtorWorldAgents;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Menus,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, DB, StdCtrls, ExtCtrls,
  cxLookAndFeelPainters, dxCustomTileControl, dxTileControl,
  cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit,
  cxDBData, cxGridChartView, cxGridDBChartView, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxClasses,
  cxGridLevel, cxGrid, cxTextEdit, cxCurrencyEdit, cxImage, dxSkinsCore,
  cxMemo, cxSplitter, cxNavigator, cxDataControllerConditionalFormattingRulesManagerDialog,
  RealtorWorldDM;

type
  TfrmAgents = class(TForm)
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
    cxStyleRepository1: TcxStyleRepository;
    cxStyle1: TcxStyle;
    lbDescription: TLabel;
    mmMain: TMainMenu;
    miAbout: TMenuItem;
    procedure cxSplitter2BeforeClose(Sender: TObject;
      var AllowClose: Boolean);
    procedure miAboutClick(Sender: TObject);
  private
    procedure InitializeFrame;
    procedure OnItemClick(Sender: TdxTileControlItem);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SelectItem(AAgentID: Integer);
  end;

var
  frmAgents: TfrmAgents;

implementation

uses
  AboutDemoForm,
  dxEMF.Types,
  RealtorWorld.Entities;

{$R *.dfm}

constructor TfrmAgents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DMRealtorWorld.InitializeCharts;
  InitializeFrame;
end;

procedure TfrmAgents.SelectItem(AAgentID: Integer);
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
  AAgents: IdxEMFCollection<TAgents>;
  AAgent: TAgents;
begin
  tcAgents.BeginUpdate;
  try
    AAgents := DMRealtorWorld.EMFSession.GetObjects<TAgents>;
    for AAgent in AAgents do
    begin
      AItem := tcAgents.CreateItem(True);
      AItem.Glyph.Image.Assign(AAgent.Photo);
      AItem.Glyph.Align := oaMiddleRight;
      AItem.Glyph.Image.Scale(70, 100);
      AItem.Tag := AAgent.ID;
      AItem.Style.Font.Size := 13;
      AItem.Text1.Value := AAgent.FirstName + ' ' + AAgent.LastName;
      AItem.Text1.IndentHorz := 10;
      AItem.Text1.IndentVert := 10;
      AItem.Text2.Value := AAgent.Phone;
      AItem.Text2.Align := oaTopLeft;
      AItem.Text2.IndentHorz := 10;
      AItem.Text2.IndentVert := 30;
      AItem.OnClick := OnItemClick;
    end;

    SelectItem(AAgents.First.Id);
  finally
    tcAgents.EndUpdate;
  end;
end;

procedure TfrmAgents.miAboutClick(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

procedure TfrmAgents.OnItemClick(Sender: TdxTileControlItem);
begin
  DMRealtorWorld.AgentId := Sender.Tag;
end;

procedure TfrmAgents.cxSplitter2BeforeClose(Sender: TObject;
  var AllowClose: Boolean);
begin
  AllowClose := False;
end;

end.
