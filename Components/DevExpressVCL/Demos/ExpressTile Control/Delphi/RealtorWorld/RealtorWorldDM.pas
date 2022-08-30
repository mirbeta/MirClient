unit RealtorWorldDM;

interface

uses
  SysUtils, Classes, DB, ADODB, DBClient, Provider, Controls, Forms, MidasLib;

type
  TDMRealtorWorld = class(TDataModule)
    dsHomePhotos: TDataSource;
    dsHomesAndAgents: TDataSource;
    dsHouseSales: TDataSource;
    dsMortgage: TDataSource;
    dsHomesAndHomes: TDataSource;
    clMortgage: TClientDataSet;
    clMortgageDate1: TDateTimeField;
    clMortgageFRM30: TFloatField;
    clMortgageFRM15: TFloatField;
    clMortgageARM1: TFloatField;
    clHousesSales: TClientDataSet;
    clHomesAndHomes: TClientDataSet;
    clHomesAndAgents: TClientDataSet;
    clHomePhotos: TClientDataSet;
    clHousesSalesID: TIntegerField;
    clHousesSalesDate1: TDateTimeField;
    clHousesSalesCount1: TIntegerField;
    clHousesSalesRegion: TMemoField;
    clHousesSalesSeasonallyAdjusted: TMemoField;
    clHousesSalesType1: TMemoField;
    dspHomesAndHomes: TDataSetProvider;
    dsHomesDetail: TDataSource;
    clHomesDetail: TClientDataSet;
    clHomesDetailID: TIntegerField;
    clHomesDetailBeds: TSmallintField;
    clHomesDetailBaths: TSmallintField;
    clHomesDetailHouseSize: TFloatField;
    clHomesDetailLotSize: TFloatField;
    clHomesDetailPrice: TFloatField;
    clHomesDetailFeatures: TMemoField;
    clHomesDetailYearBuilt: TMemoField;
    clHomesDetailType: TIntegerField;
    clHomesDetailStatus: TIntegerField;
    clHomesDetailPhoto: TBlobField;
    clHomesDetailAgentID: TIntegerField;
    clHomesDetailYearID: TIntegerField;
    dsHouseSalsesChart: TDataSource;
    clHouseSalesChart: TClientDataSet;
    clHouseSalesChartDate: TDateField;
    clHouseSalesChartState: TStringField;
    clHouseSalesChartSeasonallyAdjusted: TStringField;
    clHouseSalesChartMidWest: TIntegerField;
    clHouseSalesChartNorthEast: TIntegerField;
    clHouseSalesChartSouth: TIntegerField;
    clHouseSalesChartWest: TIntegerField;
    clHouseSalesChartAll: TIntegerField;
    dsHouseRating: TDataSource;
    dsHousesSimular: TDataSource;
    clHousesSimular: TClientDataSet;
    IntegerField1: TIntegerField;
    clHousesSimularYear: TIntegerField;
    dsHousePrice: TDataSource;
    clHousePrice: TClientDataSet;
    IntegerField2: TIntegerField;
    clHousePriceDate: TDateField;
    clHousePricePrice: TFloatField;
    clHousesSimularProposals: TIntegerField;
    clHousesSimularSold: TIntegerField;
    clHouseRating: TClientDataSet;
    clHouseRatingHouseID: TIntegerField;
    clHouseRatingRegionName: TStringField;
    clHouseRatingValue: TIntegerField;
    dsResearchChart: TDataSource;
    clResearchChart: TClientDataSet;
    clResearchChartDate: TDateField;
    clResearchChartState: TStringField;
    clResearchChartSeasonallyAdjusted: TStringField;
    clResearchChartMidWest: TIntegerField;
    clResearchChartNorthEast: TIntegerField;
    clResearchChartSouth: TIntegerField;
    clResearchChartWest: TIntegerField;
    clResearchChartAll: TIntegerField;
    clHomesAndHomesID: TIntegerField;
    clHomesAndHomesAddress: TMemoField;
    clHomesAndHomesBeds: TSmallintField;
    clHomesAndHomesBaths: TSmallintField;
    clHomesAndHomesHouseSize: TFloatField;
    clHomesAndHomesLotSize: TFloatField;
    clHomesAndHomesPrice: TFloatField;
    clHomesAndHomesFeatures: TMemoField;
    clHomesAndHomesYearBuilt: TMemoField;
    clHomesAndHomesType: TIntegerField;
    clHomesAndHomesStatus: TIntegerField;
    clHomesAndHomesPhoto: TBlobField;
    clHomesAndHomesAgentID: TIntegerField;
    clHomesAndHomesYearID: TIntegerField;
    clHomesDetailAddress: TMemoField;
    procedure DataModuleCreate(Sender: TObject);
    procedure clHomesAndHomesCalcFields(DataSet: TDataSet);
  private
    FDataPath: string;
    procedure ActualizeDates(ADataSet: TDataSet; const AFieldName: string);
    procedure DoIncrementDates(ADataSet: TDataSet; const AFieldName: string; AMonthCount: Integer);
    function GetMaxDate(ADataSet: TDataSet; const AFieldName: string): TDate;
  public
    property DataPath: string read FDataPath;
  end;

var
  DMRealtorWorld: TDMRealtorWorld;

implementation

uses DateUtils, Math, RealtorWorldDataPath;

{$R *.dfm}

procedure TDMRealtorWorld.DataModuleCreate(Sender: TObject);
begin
  FDataPath := ExtractFilePath(Application.ExeName) + GetDataPath;
  clHomePhotos.LoadFromFile(FDataPath + 'HomePhotos.cds');
  clHomesAndAgents.LoadFromFile(FDataPath + 'HomesAndAgents.cds');
  clHomesAndHomes.LoadFromFile(FDataPath + 'HomesAndHomes.cds');
  clHousesSales.LoadFromFile(FDataPath + 'HousesSales.cds');
  ActualizeDates(clHousesSales, 'Date1');
  clMortgage.LoadFromFile(FDataPath + 'Mortgage.cds');
  ActualizeDates(clMortgage, 'Date1');
  clHomesDetail.Active := True;
  clHouseSalesChart.LoadFromFile(FDataPath + 'HouseSalesChart.cds');
  ActualizeDates(clHouseSalesChart, 'Date');
  clHouseRating.LoadFromFile(FDataPath + 'HouseRating.cds');
  clHousesSimular.LoadFromFile(FDataPath + 'HouseSimular.cds');
  clHousePrice.LoadFromFile(FDataPath + 'HousePrice.cds');
  clResearchChart.LoadFromFile(FDataPath + 'HousesSalesResearch.cds');
  ActualizeDates(clResearchChart, 'Date');
end;

procedure TDMRealtorWorld.clHomesAndHomesCalcFields(DataSet: TDataSet);
begin
  clHomesAndHomesAgentID.Value := clHomesAndHomesID.Value mod 6 + 1;
  clHomesAndHomesYearID.Value := StrToInt(clHomesAndHomesYearBuilt.Value);
end;

procedure TDMRealtorWorld.ActualizeDates(ADataSet: TDataSet; const AFieldName: string);
var
  AMaxDate, AActualMaxDate: TDate;
begin
  ADataSet.DisableControls;
  try
    AMaxDate := GetMaxDate(ADataSet, AFieldName);
    AActualMaxDate := RecodeDate(Date, YearOf(Date), MonthOf(Date), DayOf(AMaxDate) + 1);
    if AActualMaxDate > Date then
      IncMonth(AActualMaxDate, -1);
    DoIncrementDates(ADataSet, AFieldName, MonthsBetween(AActualMaxDate, AMaxDate));
  finally
    ADataSet.EnableControls;
  end;
  ADataSet.First;
end;

procedure TDMRealtorWorld.DoIncrementDates(ADataSet: TDataSet; const AFieldName: string; AMonthCount: Integer);
begin
  with ADataSet do
  begin
    First;
    while not EOF do
    begin
      Edit;
      FieldByName(AFieldName).AsDateTime := IncMonth(FieldByName(AFieldName).AsDateTime, AMonthCount);
      Post;
      Next;
    end;
  end;
end;

function TDMRealtorWorld.GetMaxDate(ADataSet: TDataSet; const AFieldName: string): TDate;
begin
  Result := 0;
  with ADataSet do
  begin
    First;
    while not EOF do
    begin
      Result := Max(Result, FieldByName(AFieldName).AsDateTime);
      Next;
    end;
  end;
end;

end.


