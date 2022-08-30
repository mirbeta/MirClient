unit RealtorWorld.Entities;

interface

uses
  SysUtils,
  dxCoreClasses, dxGDIPlusClasses,
  dxEMF.Types,
  dxEMF.Metadata,
  dxEMF.Core;

type

  { TAgents }

  TAgents = class
  private
    FID: Integer;
    FFirstName: string;
    FLastName: string;
    FPhone: string;
    FEmail: string;
    FPhoto: TdxSmartImage;
  public
    constructor Create;
    destructor Destroy; override;

    property ID: Integer read FID write FID;
    property FirstName: string read FFirstName write FFirstName;
    property LastName: string read FLastName write FLastName;
    property Phone: string read FPhone write FPhone;
    property Email: string read FEmail write FEmail;
    property Photo: TdxSmartImage read FPhoto;
  end;

  { THomes }

  THomes = class
  private
    FID: Integer;
    FAddress: string;
    FBeds: SmallInt;
    FBaths: SmallInt;
    FHouseSize: Double;
    FLotSize: Double;
    FPrice: Double;
    FFeatures: string;
    FYearBuilt: Integer;
    FType: Integer;
    FStatus: Integer;
    FPhoto: TBytes;
    FAgent: TAgents;
    function GetAgentID: Integer;
  public
    property ID: Integer read FID write FID;
    property Address: string read FAddress write FAddress;
    property Beds: SmallInt read FBeds write FBeds;
    property Baths: SmallInt read FBaths write FBaths;
    property HouseSize: Double read FHouseSize write FHouseSize;
    property LotSize: Double read FLotSize write FLotSize;
    property Price: Double read FPrice write FPrice;
    property Features: string read FFeatures write FFeatures;
    property YearBuilt: Integer read FYearBuilt write FYearBuilt;
    property &Type: Integer read FType write FType;
    property Status: Integer read FStatus write FStatus;
    property Photo: TBytes read FPhoto write FPhoto;
    property Agent: TAgents read FAgent write FAgent;
    property AgentID: Integer read GetAgentID;
  end;

  { TCharts }

  TCharts = class
  private
    FDate: Integer;
    FAgent: TAgents;
    FNorthEast: Integer;
    FMidWest: Integer;
    FSouth: Integer;
    FWest: Integer;
  public
    property Agent: TAgents read FAgent write FAgent;
    property MidWest: Integer read FMidWest write FMidWest;
    property NorthEast: Integer read FNorthEast write FNorthEast;
    property South: Integer read FSouth write FSouth;
    property West: Integer read FWest write FWest;
    property Date: Integer read FDate write FDate;
  end;

implementation

procedure RegisterEntities;
begin
  EntityManager.RegisterEntity(TAgents).
    RegisterProperty('ID').Key.
    RegisterProperty('FirstName').Size(50).
    RegisterProperty('LastName').Size(50).
    RegisterProperty('Phone').Size(50).
    RegisterProperty('Email').Size(50).
    RegisterProperty('Photo').Blob;
  EntityManager.RegisterEntity(THomes).
    RegisterProperty('ID').Key.
    RegisterProperty('Address').Size(50).
    RegisterProperty('Beds').
    RegisterProperty('Baths').
    RegisterProperty('HouseSize').
    RegisterProperty('LotSize').
    RegisterProperty('Price').
    RegisterProperty('Features').Blob.
    RegisterProperty('YearBuilt').
    RegisterProperty('Type').
    RegisterProperty('Status').
    RegisterProperty('Photo').Blob.
    RegisterProperty('Agent');
  EntityManager.RegisterEntity(TCharts).
//    RegisterField('FID').Key.Generator(TdxGeneratorType.Identity).
    RegisterProperty('Agent').
    RegisterProperty('NorthEast').
    RegisterProperty('MidWest').
    RegisterProperty('South').
    RegisterProperty('West').
    RegisterProperty('Date');
end;

{ TAgents }

constructor TAgents.Create;
begin
  inherited Create;
  FPhoto := TdxSmartImage.Create;
end;

destructor TAgents.Destroy;
begin
  FreeAndNil(FPhoto);
  inherited Destroy;
end;

{ THomes }

function THomes.GetAgentID: Integer;
begin
  Result := ID mod 6 + 1;
end;

initialization
  RegisterEntities;

finalization
  EntityManager.UnRegisterEntities([
    TAgents,
    THomes,
    TCharts]);

end.
