unit OpenWeatherMapService;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  dxCoreClasses, dxCore, cxGraphics, cxControls, StrUtils, RTLConsts, Types,
  Generics.Collections, dxGdiPlusClasses, dxMapControlTypes, dxMapItem,
  dxMapControlHttpRequest, dxXmlDoc, dxRESTService;

const
  WeatherApiUri = 'http://api.openweathermap.org/data/2.5/weather?APPID=%s';
  WeatherByCityNameUri = WeatherApiUri + '&q=%s&mode=xml&units=metric';
  WeatherByGeoPointUri = WeatherApiUri + '&lat=%s&lon=%s&mode=xml&units=metric';
  WeatherBaseImageUri = 'http://openweathermap.org/img/w/';
  AdditionalLayerUri = 'http://tile.openweathermap.org/map/%s/[z]/[x]/[y].png?appid=%s';
  AdditionalLayerSubUri: array [0..5] of string = ('clouds_new', 'precipitation_new', 'pressure_new', 'wind_new', 'temp_new', 'snow_new');

type
  TWeatherInfo = class
  private
    FCityId: Integer;
    FCityName: string;
    FFullCityName: string;
    FGeoPoint: TdxMapControlGeoPoint;
    FHumidity: string;
    FImageUrl: string;
    FMapItem: TdxMapCustomElement;
    FPressure: string;
    FTemperature: string;
    FWind: string;
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
  public
    procedure UpdateItemImage;
    procedure UpdateItemText(AIsCelsius: Boolean);

    property CityId: Integer read FCityId write FCityId;
    property CityName: string read FCityName write FCityName;
    property FullCityName: string read FFullCityName write FFullCityName;
    property GeoPoint: TdxMapControlGeoPoint read FGeoPoint write FGeoPoint;
    property Humidity: string read FHumidity write FHumidity;
    property ImageUrl: string read FImageUrl write FImageUrl;
    property MapItem: TdxMapCustomElement read FMapItem write FMapItem;
    property Pressure: string read FPressure write FPressure;
    property Temperature: string read FTemperature write FTemperature;
    property Visible: Boolean read GetVisible write SetVisible;
    property Wind: string read FWind write FWind;
  end;

  TOpenWeatherMapRESTServiceResponse = class(TdxRestServiceResponse)
  private
    FWeatherInfo: TWeatherInfo;
  protected
    procedure DoParse(ADoc: TdxXMLDocument); override;
  public
    property WeatherInfo: TWeatherInfo read FWeatherInfo;
  end;

  TOpenWeatherLoadInfoTask = class(TdxRESTServiceTask)
  private
    FCityName: string;
  protected
    procedure Execute; override;
  public
    property CityName: string read FCityName write FCityName;
  end;

  TOpenWeatherMapRESTService = class(TdxRESTService)
  protected
    function CreateResponse: TdxRestServiceResponse; override;
  public
    procedure RequestAsyncWeatherInfoByCityName(const ACityName: string);
  end;

  TOpenWeatherLoadIconTask = class(TdxAsyncTask)
  private
    FStream: TStream;
    FUrl: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const AUrl: string);
    destructor Destroy; override;
    property Stream: TStream read FStream;
    property Url: string read FUrl;
  end;

  TIconLoadedEvent = procedure (ASender: TObject; const AImageUrl: string) of object;

  TWeatherIconCache = class
  private
    FAsyncTaskRunner: TdxAsyncTaskRunner;
    FLockQueryIcon: TRTLCriticalSection;
    FIcons: TObjectDictionary<string, TdxSmartImage>;
    FOnIconLoaded: TIconLoadedEvent;
  protected
    procedure IconLoaded(ASender: TObject);
    procedure QueryIcon(const AUrl: string);
  public
    constructor Create;
    destructor Destroy; override;
    function GetIcon(const AIconHRef: string): TdxSmartImage;
    property OnIconLoaded: TIconLoadedEvent read FOnIconLoaded write FOnIconLoaded;
  end;

  TWeatherInfosItemAddedEvent = procedure (ASender: TObject; AInfo: TWeatherInfo) of object;

  TWeatherInfos = class
  private
    FItems: TObjectList<TWeatherInfo>;
    FOpenWeatherRestService: TOpenWeatherMapRESTService;
    FOnItemAdded: TWeatherInfosItemAddedEvent;
    function CheckResponse(AResponse: TdxRestServiceResponse; out AWeatherInfo: TWeatherInfo): Boolean;
    procedure IconLoaded(ASender: TObject; const AUrl: string);
    procedure OpenWeatherMapRESTServiceResponse(Sender: TObject;
      AResponse: TdxRestServiceResponse; var ADestroyResponse: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddInfo(AWeatherInfo: TWeatherInfo);
    function GetInfo(const AName: string; out AWeatherInfo: TWeatherInfo): Boolean; overload;
    function GetInfo(const AGeoPoint: TdxMapControlGeoPoint; out AWeatherInfo: TWeatherInfo): Boolean; overload;
    function GetInfoByMapItem(AItem: TdxMapCustomElement): TWeatherInfo;
    procedure HideInfo(AInfoName: string);
    function IsInfoExists(const AName: string; out AWeatherInfo: TWeatherInfo): Boolean; overload;
    function IsInfoExists(const ACityId: Integer; out AWeatherInfo: TWeatherInfo): Boolean; overload;
    procedure LoadInfoAsync(const AName: string);
    procedure UpdateItemText(AIsCelsius: Boolean);
    property Items: TObjectList<TWeatherInfo> read FItems;
    property OnItemAdded: TWeatherInfosItemAddedEvent read FOnItemAdded write FOnItemAdded;
  end;

function WeatherIconCache: TWeatherIconCache;
function GetKey: string;

implementation

{$R *.res}

var
  FWeatherIconCache: TWeatherIconCache;

function GetKey: string;
var
  Buffer: array [0..255] of Char;
begin
  SetString(Result, Buffer, LoadString(FindResourceHInstance(HInstance), 101, Buffer, Length(Buffer)));
end;

function WeatherIconCache: TWeatherIconCache;
begin
  if FWeatherIconCache = nil then
    FWeatherIconCache := TWeatherIconCache.Create;
  Result := FWeatherIconCache;
end;

{ TWeatherInfo }

procedure TWeatherInfo.UpdateItemImage;
var
  AImage: TdxSmartImage;
begin
  AImage := WeatherIconCache.GetIcon(ImageUrl);
  MapItem.Image.Assign(AImage);
  MapItem.ScreenTip.Description.Glyph.Assign(AImage);
end;

procedure TWeatherInfo.UpdateItemText(AIsCelsius: Boolean);
const
  FMeasure: array [Boolean] of string = ('°F', '°C');
var
  ATemperature: Integer;
begin
  if AIsCelsius then
    ATemperature := Round(dxStrToFloat(Temperature))
  else
    ATemperature := Round(dxStrToFloat(Temperature) * 9 / 5 + 32);

  FMapItem.Text := CityName + dxCRLF + IfThen(ATemperature > 0, '+') + IntToStr(ATemperature) + FMeasure[AIsCelsius];
end;

function TWeatherInfo.GetVisible: Boolean;
begin
  Result := FMapItem.Visible;
end;

procedure TWeatherInfo.SetVisible(const Value: Boolean);
begin
  FMapItem.Visible := Value;
end;

{ TOpenWeatherMapRESTService }

procedure TOpenWeatherMapRESTService.RequestAsyncWeatherInfoByCityName(
  const ACityName: string);
var
  ATask: TOpenWeatherLoadInfoTask;
begin
  ATask := TOpenWeatherLoadInfoTask.Create(Self, Format(WeatherByCityNameUri, [GetKey, dxParamsEncode(ACityName)]));
  ATask.CityName := ACityName;
  RunAsyncTask(ATask);
end;

function TOpenWeatherMapRESTService.CreateResponse: TdxRestServiceResponse;
begin
  Result := TOpenWeatherMapRESTServiceResponse.Create;
end;

{ TOpenWeatherMapRESTServiceResponse }

procedure TOpenWeatherMapRESTServiceResponse.DoParse(ADoc: TdxXMLDocument);
var
  ACurrentNode, ANode, AWindNode, AHumidityNode: TdxXMLNode;
  ALon, ALat, AAttr: TdxXMLNodeAttribute;
  ACityName: string;
  ACityId: Integer;
begin
  FWeatherInfo := nil;
  if ADoc.Root.FindChild('current', ACurrentNode) then
  begin
    ACityId := -1;
    if ACurrentNode.FindChild('city', ANode) then
    begin
      if ANode.Attributes.Find('name', AAttr) then
        ACityName := AAttr.ValueAsString;
      if ANode.Attributes.Find('id', AAttr) then
        ACityId := AAttr.ValueAsInteger;
    end;
    if ANode.FindChild('coord', ANode) then
    begin
      if ANode.Attributes.Find('lon', ALon) and
        ANode.Attributes.Find('lat', ALat) then
      begin
        FWeatherInfo := TWeatherInfo.Create;
        FWeatherInfo.GeoPoint := dxMapControlGeoPoint(dxStrToFloat(ALat.ValueAsString),
          dxStrToFloat(ALon.ValueAsString));

        FWeatherInfo.FullCityName := ACityName;
        FWeatherInfo.CityName := ACityName;
        FWeatherInfo.CityId := ACityId;

        if ACurrentNode.FindChild('temperature', ANode) then
          if ANode.Attributes.Find('value', AAttr) then
            FWeatherInfo.Temperature := AAttr.ValueAsString;

        if ACurrentNode.FindChild('weather', ANode) then
          if ANode.Attributes.Find('icon', AAttr) then
          begin
            FWeatherInfo.ImageUrl := WeatherBaseImageUri + AAttr.ValueAsString + '.png';
            if WeatherIconCache.GetIcon(FWeatherInfo.ImageUrl) = nil then
              FWeatherInfo.ImageUrl := '';
          end;

        if ACurrentNode.FindChild('wind', AWindNode) then
        begin
          if AWindNode.FindChild('direction', ANode) then
            if ANode.Attributes.Find('name', AAttr) then
              FWeatherInfo.Wind := AAttr.ValueAsString;
          if AWindNode.FindChild('speed', ANode) then
            if ANode.Attributes.Find('value', AAttr) then
              FWeatherInfo.Wind := FWeatherInfo.Wind + ', ' + AAttr.ValueAsString + ' m/s';
        end;

        if ACurrentNode.FindChild('humidity', AHumidityNode) then
        begin
          if AHumidityNode.Attributes.Find('value', AAttr) then
            FWeatherInfo.Humidity := AAttr.ValueAsString;
          if AHumidityNode.Attributes.Find('unit', AAttr) then
            FWeatherInfo.Humidity := FWeatherInfo.Humidity + ' ' + AAttr.ValueAsString;
        end;

        if ACurrentNode.FindChild('pressure', ANode) then
        begin
          if ANode.Attributes.Find('value', AAttr) then
            FWeatherInfo.Pressure := AAttr.ValueAsString;
          if ANode.Attributes.Find('unit', AAttr) then
            FWeatherInfo.Pressure := FWeatherInfo.Pressure + ' ' + AAttr.ValueAsString;
        end;
      end;
    end;
  end;
end;

{ TWeatherIconCache }

constructor TWeatherIconCache.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLockQueryIcon);
  FIcons := TObjectDictionary<string, TdxSmartImage>.Create([doOwnsValues]);
  FAsyncTaskRunner := TdxAsyncTaskRunner.Create;
end;

destructor TWeatherIconCache.Destroy;
begin
  FreeAndNil(FAsyncTaskRunner);
  FreeAndNil(FIcons);
  DeleteCriticalSection(FLockQueryIcon);
  inherited Destroy;
end;

function TWeatherIconCache.GetIcon(const AIconHRef: string): TdxSmartImage;
begin
  Result := nil;
  if AIconHRef = '' then
    Exit;
  EnterCriticalSection(FLockQueryIcon);
  try
    if not FIcons.TryGetValue(AIconHRef, Result) then
    begin
      Result := TdxSmartImage.CreateSize(1, 1);
      FIcons.Add(AIconHRef, Result);
      QueryIcon(AIconHRef);
    end;
  finally
    LeaveCriticalSection(FLockQueryIcon);
  end;
end;

procedure TWeatherIconCache.IconLoaded(ASender: TObject);
var
  AImage: TdxSmartImage;
  ATask: TOpenWeatherLoadIconTask;
begin
  ATask := ASender as TOpenWeatherLoadIconTask;
  ATask.Stream.Position := 0;
  AImage := GetIcon(ATask.Url);
  AImage.LoadFromStream(ATask.Stream);
  if Assigned(FOnIconLoaded) then
    FOnIconLoaded(Self, ATask.Url);
end;

procedure TWeatherIconCache.QueryIcon(const AUrl: string);
var
  ATask: TOpenWeatherLoadIconTask;
begin
  ATask := TOpenWeatherLoadIconTask.Create(AUrl);
  ATask.OnTaskFinished := IconLoaded;
  FAsyncTaskRunner.RunTask(ATask);
end;

{ TOpenWeatherLoadInfoTask }

procedure TOpenWeatherLoadInfoTask.Execute;
var
  AResponse: TOpenWeatherMapRESTServiceResponse;
begin
  inherited Execute;
  if (Response <> nil) then
  begin
    AResponse := Response as TOpenWeatherMapRESTServiceResponse;
    if AResponse.WeatherInfo <> nil then
      AResponse.WeatherInfo.CityName := FCityName;
  end;
end;

{ TOpenWeatherLoadIconTask }

constructor TOpenWeatherLoadIconTask.Create(const AUrl: string);
begin
  inherited Create;
  FUrl := AUrl;
  FStream := TMemoryStream.Create;
end;

destructor TOpenWeatherLoadIconTask.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TOpenWeatherLoadIconTask.Execute;
begin
  GetContent(FUrl, FStream);
end;

{ TWeatherInfos }

constructor TWeatherInfos.Create;
begin
  inherited Create;
  FItems := TObjectList<TWeatherInfo>.Create;
  FOpenWeatherRestService := TOpenWeatherMapRESTService.Create;
  FOpenWeatherRestService.OnResponse := OpenWeatherMapRESTServiceResponse;
  WeatherIconCache.OnIconLoaded := IconLoaded;
end;

destructor TWeatherInfos.Destroy;
begin
  WeatherIconCache.OnIconLoaded := nil;
  FreeAndNil(FOpenWeatherRestService);
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TWeatherInfos.AddInfo(AWeatherInfo: TWeatherInfo);
begin
  FItems.Add(AWeatherInfo);
  FOnItemAdded(Self, AWeatherInfo);
end;

function TWeatherInfos.CheckResponse(AResponse: TdxRestServiceResponse;
  out AWeatherInfo: TWeatherInfo): Boolean;
var
  AOpenWeatherResponse: TOpenWeatherMapRESTServiceResponse;
begin
  Result := False;
  if AResponse <> nil then
  begin
    AOpenWeatherResponse := AResponse as TOpenWeatherMapRESTServiceResponse;
    AWeatherInfo := AOpenWeatherResponse.WeatherInfo;
    Result := AWeatherInfo <> nil;
  end;
end;

function TWeatherInfos.GetInfo(const AName: string;
  out AWeatherInfo: TWeatherInfo): Boolean;
var
  AResponse: TdxRestServiceResponse;
begin
  FOpenWeatherRestService.Request(Format(WeatherByCityNameUri, [GetKey, dxParamsEncode(AName)]), AResponse);
  Result := CheckResponse(AResponse, AWeatherInfo);
  AResponse.Free;
end;

function TWeatherInfos.GetInfo(const AGeoPoint: TdxMapControlGeoPoint;
  out AWeatherInfo: TWeatherInfo): Boolean;
var
  AResponse: TdxRestServiceResponse;
begin
  FOpenWeatherRestService.Request(Format(WeatherByGeoPointUri,
    [GetKey, dxFloatToStr(AGeoPoint.Latitude), dxFloatToStr(AGeoPoint.Longitude)]), AResponse);
  Result := CheckResponse(AResponse, AWeatherInfo);
  AResponse.Free;
end;

function TWeatherInfos.GetInfoByMapItem(
  AItem: TdxMapCustomElement): TWeatherInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
    if FItems[I].MapItem = AItem then
    begin
      Result := FItems[I];
      Break
    end;
end;

procedure TWeatherInfos.HideInfo(AInfoName: string);
var
  AInfo: TWeatherInfo;
begin
  if IsInfoExists(AInfoName, AInfo) then
    AInfo.Visible := False;
end;

procedure TWeatherInfos.IconLoaded(ASender: TObject; const AUrl: string);
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    if SameText(AUrl, FItems[I].ImageUrl) then
      FItems[I].UpdateItemImage;
end;

function TWeatherInfos.IsInfoExists(const ACityId: Integer;
  out AWeatherInfo: TWeatherInfo): Boolean;
var
  I: Integer;
begin
  Result := False;
  AWeatherInfo := nil;
  for I := 0 to FItems.Count - 1 do
    if FItems[I].CityId = ACityId then
    begin
      Result := True;
      AWeatherInfo := FItems[I];
      Break;
    end;
end;

procedure TWeatherInfos.LoadInfoAsync(const AName: string);
begin
  FOpenWeatherRestService.RequestAsyncWeatherInfoByCityName(AName);
end;

function TWeatherInfos.IsInfoExists(const AName: string;
  out AWeatherInfo: TWeatherInfo): Boolean;
var
  I: Integer;
begin
  Result := False;
  AWeatherInfo := nil;
  for I := 0 to FItems.Count - 1 do
    if SameText(FItems[I].FullCityName, AName) then
    begin
      Result := True;
      AWeatherInfo := FItems[I];
      Break;
    end;
end;

procedure TWeatherInfos.OpenWeatherMapRESTServiceResponse(Sender: TObject;
  AResponse: TdxRestServiceResponse; var ADestroyResponse: Boolean);
var
  AWeatherInfo: TWeatherInfo;
begin
  if CheckResponse(AResponse, AWeatherInfo) then
    AddInfo(AWeatherInfo);
end;

procedure TWeatherInfos.UpdateItemText(AIsCelsius: Boolean);
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems[I].UpdateItemText(AIsCelsius);
end;

initialization

finalization
  FreeAndNil(FWeatherIconCache);

end.
