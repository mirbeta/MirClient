unit RealtorWorldDM;

interface

uses
  SysUtils, Classes, DB, ADODB,
  dxEMF.Types, dxEMF.Core, dxEMF.DataProvider.ADO, dxEMF.DataSet, dxEMF.DB.MSAccess,
  RealtorWorld.Entities;

type
  TDMRealtorWorld = class(TDataModule)
    dsHomesDetail: TDataSource;
    EMFSession: TdxEMFSession;
    EMFADODataProvider: TdxEMFADODataProvider;
    ADOConnection: TADOConnection;
    edsHomesDetail: TdxEMFDataSet;
    edsChart: TdxEMFDataSet;
    edsHomesDetailID: TIntegerField;
    edsHomesDetailAddress: TWideStringField;
    edsHomesDetailBeds: TSmallintField;
    edsHomesDetailBaths: TSmallintField;
    edsHomesDetailHouseSize: TFloatField;
    edsHomesDetailLotSize: TFloatField;
    edsHomesDetailPrice: TFloatField;
    edsHomesDetailFeatures: TWideMemoField;
    edsHomesDetailYearBuilt: TIntegerField;
    edsHomesDetailType: TIntegerField;
    edsHomesDetailStatus: TIntegerField;
    edsHomesDetailPhoto: TBlobField;
    edsChartAgent: TdxEntityField;
    edsChartNorthEast: TIntegerField;
    edsChartMidWest: TIntegerField;
    edsChartSouth: TIntegerField;
    edsChartWest: TIntegerField;
    edsChartDate: TIntegerField;
    edsHomesDetailAgent: TdxEntityField;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    FCharts: IdxEMFCollection<TCharts>;
    FAgentID: Integer;
    procedure SetAgentID(const Value: Integer);
    procedure ClearCharts;
    procedure UpdateCharts;
  public
    procedure InitializeCharts;
    procedure UpdateHomes;
    property AgentID: Integer read FAgentID write SetAgentID;
  end;

var
  DMRealtorWorld: TDMRealtorWorld;

implementation

uses
  DateUtils;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TDMRealtorWorld }

procedure TDMRealtorWorld.ClearCharts;
var
  AChart: TCharts;
begin
  if FCharts = nil then
    Exit;
  while FCharts.Count > 0 do
  begin
    AChart := FCharts[FCharts.Count - 1];
    FCharts.Remove(AChart);
    AChart.Free;
  end;
end;

procedure TDMRealtorWorld.DataModuleCreate(Sender: TObject);
const
  DataPath = '..\..\..\Data\';
  DBFileName = 'RealtorWorld.mdb';
begin
  ADOConnection.Properties['Data Source'].Value := DataPath + DBFileName;
end;

procedure TDMRealtorWorld.DataModuleDestroy(Sender: TObject);
begin
  ClearCharts;
end;

procedure TDMRealtorWorld.InitializeCharts;
var
  AAgent: TAgents;
  AAgents: IdxEMFCollection<TAgents>;
  AYear: Integer;
  AChart: TCharts;
  ACurrentYear: Integer;
begin
  AAgents := EMFSession.GetObjects<TAgents>;

  FCharts := TdxEMFCollections.Create<TCharts>;
  for AAgent in AAgents do
  begin
    ACurrentYear := YearOf(Now);
    for AYear := 2003 to ACurrentYear do
    begin
     AChart := TCharts.Create;
     AChart.Agent := AAgent;
     AChart.Date := AYear;
     AChart.MidWest := Random(20) + 4;
     AChart.NorthEast := Random(20) + 5;
     AChart.South := Random(20) + 2;
     AChart.West := Random(20) + 3;

     FCharts.Add(AChart);
    end;
  end;
end;

procedure TDMRealtorWorld.SetAgentID(const Value: Integer);
begin
  if FAgentID = Value then
    Exit;
  FAgentID := Value;
  edsHomesDetail.Params.ParamByName('AgentID').AsInteger := Value;
  if edsHomesDetail.Active then
    edsHomesDetail.Refresh
  else
    edsHomesDetail.Open;
  UpdateCharts;
end;

procedure TDMRealtorWorld.UpdateCharts;
var
  ACharts: TArray<TCharts>;
begin
  if FCharts = nil then
    Exit;
  edsChart.Close;
  ACharts := FCharts.GetObjects(function (ACurrentChart: TCharts): Boolean
    begin
      Result := ACurrentChart.Agent.ID = AgentID;
    end);
  if Length(ACharts) = 0 then
    Exit;
  edsChart.AssignData<TCharts>(ACharts);
  edsChart.Open;
end;

procedure TDMRealtorWorld.UpdateHomes;
var
  AHomes: THomes;
begin
  EMFSession.BeginTrackingChanges;
  for AHomes in EMFSession.GetObjects<THomes> do
    if AHomes.Agent = nil then
    begin
      AHomes.Agent := EMFSession.Find<TAgents>(AHomes.ID mod 6 + 1);
      EMFSession.Save(AHomes);
    end;
  EMFSession.FlushChanges;
end;

end.
